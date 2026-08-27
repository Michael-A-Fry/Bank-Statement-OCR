# jobs.R -- run a conversion in its OWN short-lived R process, and poll for it. R is single-threaded
# and this is ONE process serving the whole team, so an engine call inside the Convert observer froze
# every other analyst's browser for as long as it ran - measured at 65 seconds. The child reports back
# through files in the job's own directory and nothing else, because a file is the only channel that
# survives the child dying; result.rds is written as result.part and RENAMED so it exists only when
# whole, and exit carries the child's own reason, kind then words. A child that dies writing nothing
# is caught by the liveness probe and the hard timeout: a job must end in "done" or "failed".

# Past the core count they do not go faster. Floor of 2 so a single-core VM still overlaps a scan
# with a CSV; ceiling of 6 because beyond that the disk and the OCR engine are the limit.
JOB_CAP_MIN <- 2L
JOB_CAP_MAX <- 6L
# The highest a SITE may set by hand. What it stops is the typo: one child peaks at 108 MB, so a
# cap of 500 is a stray zero and 50 GB.
JOB_CAP_HARD_MAX <- 32L
JOB_TIMEOUT_SECS <- 1800
JOB_START_GRACE_SECS <- 60
# Liveness is asked at most this often per job: on Linux it is a stat(), elsewhere it forks ps.
JOB_PROBE_SECS <- 2

.JOB_REG <- new.env(parent = emptyenv())
.JOB_CTL <- new.env(parent = emptyenv())

.job_next_seq <- function() {
  n <- (.JOB_CTL$seq %||% 0L) + 1L
  .JOB_CTL$seq <- n
  n
}
.job_all <- function() mget(ls(.JOB_REG), envir = .JOB_REG)

# Set once at startup from app.max_concurrent_jobs; NULL works it out from the hardware. Anything not
# a whole number in range is REFUSED OUT LOUD and the worked-out value used instead - a site that
# typed the wrong thing must not discover it as mysterious slowness months later. Shapes reached by
# typing them, all silent before: 500 taken literally, 2.7 truncated to 2, `yes` read as 1, a list.
job_set_max_concurrent <- function(n = NULL) {
  refuse <- function(why) {
    .JOB_CTL$cap <- NULL
    warning(sprintf(paste("app.max_concurrent_jobs is '%s', which %s.",
                          "Statement Studio is using %d instead."),
                    paste(as.character(n), collapse = " "), why, job_max_concurrent()),
            call. = FALSE, immediate. = TRUE)
    invisible(job_max_concurrent())
  }
  if (is.null(n) || (length(n) == 1L && all(is.na(n)))) {
    .JOB_CTL$cap <- NULL
    return(invisible(job_max_concurrent()))
  }
  if (length(n) != 1L) return(refuse("is a list, not one number of jobs"))
  if (is.logical(n)) return(refuse("is a yes/no, not a number of jobs"))
  v <- suppressWarnings(as.numeric(as.character(n))[1])
  if (!is.finite(v)) return(refuse("is not a number of jobs"))
  if (v != round(v)) return(refuse("is not a whole number of jobs"))
  if (v < 1) return(refuse("is fewer than one job"))
  if (v > JOB_CAP_HARD_MAX)
    return(refuse(sprintf("is more than this tool will run at once (%d)", JOB_CAP_HARD_MAX)))
  .JOB_CTL$cap <- as.integer(v)
  invisible(.JOB_CTL$cap)
}

job_max_concurrent <- function() {
  if (!is.null(.JOB_CTL$cap)) return(.JOB_CTL$cap)
  cores <- suppressWarnings(as.integer(parallel::detectCores())[1])
  if (is.na(cores) || cores < 1L) cores <- 2L
  max(JOB_CAP_MIN, min(JOB_CAP_MAX, cores - 1L))
}

# Capping processes is not enough: tesseract is OpenMP-parallel and takes a thread per core, so three
# conversions on a four-core box is twelve threads fighting over four - one scan reads in 36 seconds
# alone and three together had not finished a PAGE after ten minutes. cap x threads never exceeds it.
job_threads_per_job <- function() {
  cores <- suppressWarnings(as.integer(parallel::detectCores())[1])
  if (is.na(cores) || cores < 1L) cores <- 1L
  max(1L, cores %/% max(1L, job_max_concurrent()))
}

.job_rscript <- function() {
  exe <- if (identical(.Platform$OS.type, "windows")) "Rscript.exe" else "Rscript"
  p <- file.path(R.home("bin"), exe)
  if (file.exists(p)) p else exe
}

# Job folders live inside this process's temp dir so R clears them when the app stops. Deliberately
# not the prefixes sweep_temp_dirs() reclaims - that sweep must never delete a running job.
.job_root <- function(jobs_dir = NULL) {
  d <- jobs_dir %||% file.path(tempdir(), "ss_jobs")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

job_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1])
  if (is.na(pid) || pid <= 0L) return(NA)
  # On Linux /proc is authoritative and costs one small read. A ZOMBIE IS NOT ALIVE: /proc/<pid>
  # outlives the process until its exit status is collected, and these children are reparented to init,
  # so "does the folder exist" reported a crashed conversion as still running.
  if (dir.exists("/proc")) {
    st <- safe(suppressWarnings(
      readLines(file.path("/proc", as.character(pid), "stat"), warn = FALSE)[1]), NA_character_)
    if (is.na(st) || !nzchar(st)) return(FALSE)
    return(!identical(strsplit(sub("^.*\\) ", "", st), " ")[[1]][1], "Z"))
  }
  out <- if (identical(.Platform$OS.type, "windows"))
    safe(suppressWarnings(system2("tasklist", c("/FI", shQuote(sprintf("PID eq %d", pid)), "/NH"),
                                  stdout = TRUE, stderr = FALSE)), NULL)
  else
    safe(suppressWarnings(system2("ps", c("-p", as.character(pid)),
                                  stdout = TRUE, stderr = FALSE)), NULL)
  if (is.null(out) || !length(out)) return(NA)
  any(grepl(as.character(pid), out, fixed = TRUE))
}

# Stop a conversion AND everything it started: every scanned page is read by the tesseract binary, so
# killing only the R child left three orphaned tesseract processes burning a core each. The tree is
# collected while the parent is STILL ALIVE, because the instant it dies its children reparent.
job_kill_tree <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1])
  if (is.na(pid) || pid <= 0L) return(invisible(FALSE))
  if (identical(.Platform$OS.type, "windows")) {
    safe(suppressWarnings(system2("taskkill", c("/PID", as.character(pid), "/T", "/F"),
                                  stdout = FALSE, stderr = FALSE)))
    return(invisible(TRUE))
  }
  kids <- .job_descendants(pid)
  safe(tools::pskill(pid))               # the root first, so it starts nothing more
  for (p in kids) safe(tools::pskill(p))
  invisible(TRUE)
}

.job_ppid_map <- function() {
  if (dir.exists("/proc")) {
    ps <- suppressWarnings(as.integer(list.files("/proc", pattern = "^[0-9]+$")))
    ps <- ps[!is.na(ps)]
    pp <- vapply(ps, function(p) {
      # suppressWarnings as well as safe(): a process that exits between the listing and this read
      # makes file() WARN before it errors, and a tidy-up must not print noise to the console.
      st <- safe(suppressWarnings(
        readLines(file.path("/proc", as.character(p), "stat"), warn = FALSE)[1]),
        NA_character_)
      if (is.na(st) || !nzchar(st)) return(NA_integer_)
      # Field 2 is the command name in brackets and MAY CONTAIN SPACES AND BRACKETS, so fields are
      # counted from the LAST ")" - splitting on spaces is how a kill-the-tree kills something else.
      suppressWarnings(as.integer(strsplit(sub("^.*\\) ", "", st), " ")[[1]][2]))
    }, integer(1))
    return(data.frame(pid = ps, ppid = pp))
  }
  out <- safe(suppressWarnings(system2("ps", c("-eo", "pid=,ppid="),
                                       stdout = TRUE, stderr = FALSE)), NULL)
  if (is.null(out) || !length(out)) return(data.frame(pid = integer(0), ppid = integer(0)))
  m <- do.call(rbind, lapply(strsplit(trimws(out), "[[:space:]]+"),
                             function(x) suppressWarnings(as.integer(x[1:2]))))
  data.frame(pid = m[, 1], ppid = m[, 2])
}

.job_descendants <- function(pid) {
  tab <- .job_ppid_map()
  out <- integer(0); frontier <- pid
  for (depth in seq_len(10L)) {
    kids <- tab$pid[!is.na(tab$ppid) & tab$ppid %in% frontier]
    kids <- setdiff(kids, c(out, pid))
    if (!length(kids)) break
    out <- c(out, kids); frontier <- kids
  }
  out
}

# deparse() writes the paths as proper R string literals, so a folder with a space still resolves.
# NOTHING IN HERE MAY PRINT: out.log is the only evidence about a process killed from outside, and
# Rscript auto-prints every visible top-level expression.
.job_child_script <- function(jobdir, root, threads = job_threads_per_job()) {
  c("# Written by job_start() (R/jobs.R) -- one conversion, one process.",
    "# Safe to re-run by hand for a post-mortem:  Rscript child.R",
    "# Everything at top level is invisible() on purpose: this script's stdout is",
    "# the crash evidence for a job that never came back (R/jobs.R).",
    paste0(".jd <- ",   deparse(jobdir)),
    paste0(".root <- ", deparse(root)),
    "# THE SAME CHARACTER LOCALE AS THE APP, first thing and not negotiable.",
    "# app.R forces LC_CTYPE to a UTF-8 locale at startup; the child inherits the",
    "# SERVICE's environment instead, which on this box is C/ASCII. Every string",
    "# the child made then came back marked in a locale the app cannot read, and",
    "# readRDS warned about each one -- printing the transaction lines of a client",
    "# statement onto the server console to do it. Descriptions are kept VERBATIM",
    "# (charter), so a re-encoding across this boundary is not a cosmetic risk.",
    paste0('invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", ',
           deparse(Sys.getlocale("LC_CTYPE")), ")))"),
    "# This conversion's share of the machine (see job_threads_per_job). Set on the",
    "# ENVIRONMENT, because the thing that has to read it is the tesseract binary",
    "# this process shells out to, not this process.",
    paste0('invisible(Sys.setenv(OMP_THREAD_LIMIT = ', deparse(as.character(threads)),
           ', OMP_NUM_THREADS = ', deparse(as.character(threads)), "))"),
    'invisible(writeLines(as.character(Sys.getpid()), file.path(.jd, "pid")))',
    "# One terminal file, renamed into place, so the parent can never read half",
    "# a reason and call it the whole one. Line 1 is the KIND, line 2 the words.",
    "#",
    "# WHY THE CHILD HAS TO SAY WHICH KIND. The parent cannot tell from the outside",
    "# whether the engine looked at the statement and refused it, or whether this",
    "# process broke before it ever reached the statement -- and those two facts get",
    "# OPPOSITE sentences on screen (app.R: FRIENDLY_READ_ERROR vs CONVERT_STOPPED).",
    "# Every error used to arrive as the same 'error', so a server fault was read out",
    "# to the analyst as a fault in her file. MEASURED, twice, by breaking the server",
    "# and not the file: with the app folder unreachable, and again with the job's own",
    "# args.rds truncated, the screen said 'It may be password-protected, an image-only",
    "# scan, or not a statement. Try a PDF or CSV export from your bank.' -- sending",
    "# her back to the bank for a file that was never the problem, while nothing",
    "# anywhere said the server was. Only this process knows which side of the line it",
    "# fell on, so only this process can say.",
    "#   engine -- job_run_task ran and raised. About the STATEMENT.",
    "#   host   -- could not load the app, read its own arguments, or write its",
    "#             answer. About this MACHINE, and never the analyst's file.",
    '.say <- function(kind, txt) {',
    '  p <- file.path(.jd, "exit.part")',
    '  writeLines(c(kind, paste(as.character(txt), collapse = " ")), p)',
    '  invisible(file.rename(p, file.path(.jd, "exit")))',
    '}',
    "# Three phases, because the phase IS the diagnosis. .ready / .answer only come",
    "# into existence when the phase before them finished, so a phase that failed",
    "# stops the run without needing a flag to be reset anywhere.",
    "invisible(tryCatch({",
    "  setwd(.root)",
    '  for (f in list.files(file.path(.root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)',
    '  a <- readRDS(file.path(.jd, "args.rds"))',
    '  .ready <- TRUE',
    '}, error = function(e) .say("host", conditionMessage(e))))',
    'if (exists(".ready")) invisible(tryCatch({',
    "  .answer <- job_run_task(a$task, a$paths, a$args, jobdir = .jd)",
    '}, error = function(e) .say("engine", conditionMessage(e))))',
    'if (exists(".answer")) invisible(tryCatch({',
    "  # The reader's cache BEFORE the answer: the parent seeds itself from this",
    "  # the moment it sees result.rds, so both files must already be on disk.",
    '  saveRDS(job_input_cache(), file.path(.jd, "inputs.part"))',
    '  file.rename(file.path(.jd, "inputs.part"), file.path(.jd, "inputs.rds"))',
    '  saveRDS(.answer, file.path(.jd, "result.part"))',
    '  file.rename(file.path(.jd, "result.part"), file.path(.jd, "result.rds"))',
    '}, error = function(e) .say("host", conditionMessage(e))))',
    "# Belt and braces: leaving with neither file would read to the parent as a",
    "# process that vanished, which is a different (and less useful) diagnosis.",
    'if (!file.exists(file.path(.jd, "result.rds")) && !file.exists(file.path(.jd, "exit")))',
    '  .say("host", "the conversion ended without producing a result")')
}

# Not an optimisation: read_input() memoises by content hash because the GUI reads the same upload four
# or five times per result, and converting in a child leaves the SERVER's cache cold, so the first
# render would re-OCR the scan in the Shiny process. Measured: 36.3s cold, 0.02s cached.
job_input_cache <- function() {
  if (!exists(".INPUT_CACHE", inherits = TRUE)) return(list())
  cache <- get(".INPUT_CACHE", inherits = TRUE)
  if (!is.environment(cache)) return(list())
  mget(ls(cache), envir = cache)
}
job_seed_input_cache <- function(x) {
  if (!length(x) || !exists(".INPUT_CACHE", inherits = TRUE)) return(invisible(0L))
  cache <- get(".INPUT_CACHE", inherits = TRUE)
  if (!is.environment(cache)) return(invisible(0L))
  nm <- names(x)
  nm <- nm[!is.na(nm) & nzchar(nm)]
  for (k in nm) assign(k, x[[k]], envir = cache)
  invisible(length(nm))
}

# Three tasks, because the app has three places that used to block: one statement, a whole case
# folder, and the maintainer's bulk audit. Here rather than in the child script so tests can call it.
job_run_task <- function(task, paths, args, jobdir = NULL) {
  paths <- as.character(paths)
  args <- as.list(args %||% list())
  switch(as.character(task)[1],
    convert = do.call(convert_document, c(list(paths[1]), args)),
    batch = {
      do.call(convert_batch, c(list(paths), args,
                               list(progress = .job_progress_writer(jobdir))))
    },
    audit = {
      # IT AUDITS; IT DOES NOT CONVERT. This was the last place a batch went through
      # convert_statement() rather than convert_document(), so a report in the folder was converted as
      # a failed statement.
      tmpl <- load_template_set(args$templates_dir, args$user_templates_dir)
      list(audit = batch_audit(paths, templates = tmpl), converted = NULL)
    },
    stop(sprintf("no such job task: '%s'", as.character(task)[1])))
}

# Per-file progress the parent can SHOW: a fifty-file case that looks frozen is a case the analyst
# kills half way. One short line per file, so a half-written last line costs one stale label.
.job_progress_writer <- function(jobdir) {
  if (is.null(jobdir)) return(NULL)
  p <- file.path(jobdir, "progress")
  function(i, n, f) safe(cat(sprintf("%d/%d %s\n", i, n, basename(f)),
                             file = p, append = TRUE))
}

# The handle is an ENVIRONMENT on purpose: a job has identity and changing state, and a list would
# give the Shiny observer a snapshot that stops matching the process it names.
job_start <- function(input_path, outdir, ..., task = "convert",
                      root = getwd(), jobs_dir = NULL,
                      timeout_secs = JOB_TIMEOUT_SECS, rscript = NULL) {
  paths <- as.character(input_path)
  if (!length(paths)) stop("job_start needs at least one input path")
  seq <- .job_next_seq()
  id <- sprintf("job%05d", seq)
  jd <- file.path(.job_root(jobs_dir), id)
  dir.create(jd, recursive = TRUE, showWarnings = FALSE)
  # Say what is actually wrong: without this the only thing anybody sees of a full disk is R's
  # "cannot open the connection", which names neither the folder nor the reason.
  if (!dir.exists(jd))
    stop(sprintf(paste("could not create the folder this conversion needs (%s).",
                       "The disk may be full, or this account may not be allowed",
                       "to write there."), jd), call. = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  saveRDS(list(task = task, paths = paths, args = list(outdir = outdir, ...)),
          file.path(jd, "args.rds"))
  writeLines(.job_child_script(normalizePath(jd, winslash = "/", mustWork = FALSE), root),
             file.path(jd, "child.R"))
  j <- new.env(parent = emptyenv())
  j$id <- id
  j$task <- as.character(task)[1]
  j$paths <- paths
  j$outdir <- outdir
  j$dir <- jd
  j$root <- root
  j$state <- "queued"
  j$started <- Sys.time()          # when the ANALYST asked, not when a slot opened
  j$launched <- as.POSIXct(NA)
  j$pid <- NA_integer_
  j$seq <- seq                     # request order, and the queue's only tie-break
  j$timeout <- timeout_secs
  j$rscript <- rscript %||% .job_rscript()
  j$probe_at <- 0
  j$reason <- NA_character_        # what to TELL somebody, in plain words
  j$reason_kind <- NA_character_   # which kind of ending it was
  j$detail <- NA_character_        # the raw text, for the maintainer's log only
  j$result <- NULL
  assign(id, j, envir = .JOB_REG)
  job_pump()
  j
}

.job_launch <- function(j) {
  j$launched <- Sys.time()
  j$state <- "running"
  j$probe_at <- as.numeric(j$launched) + JOB_PROBE_SECS
  log <- file.path(j$dir, "out.log")
  ok <- safe(system2(j$rscript,
                     args = c("--vanilla", shQuote(file.path(j$dir, "child.R"))),
                     stdout = log, stderr = log, wait = FALSE), NULL)
  if (is.null(ok) || (is.numeric(ok) && length(ok) && !is.na(ok[1]) && ok[1] != 0))
    .job_fail(j, "nostart", "the conversion process could not be started")
  invisible(j$state)
}

.job_fail <- function(j, kind, detail) {
  j$state <- "failed"
  j$reason_kind <- kind
  j$detail <- paste(as.character(detail %||% ""), collapse = " ")
  j$reason <- switch(kind,
    error   = "the conversion stopped with an error",
    broken  = "the conversion could not be run on this server",
    stopped = "the conversion process stopped before it finished",
    timeout = sprintf("the conversion was still running after %d minutes",
                      as.integer(round(j$timeout / 60))),
    nostart = "no conversion process started",
    "the conversion did not finish")
  "failed"
}

# Fail the job as the kind the CHILD said it was: "engine" means the engine looked at the statement and
# raised, "host" means this machine let the conversion down first. An unreadable exit file is a HOST
# fault - reading it as an engine error puts "may be password-protected" on screen on no evidence.
.job_exit_reason <- function(j, ef) {
  x <- .job_read(ef)
  kind <- trimws(as.character(x)[1])
  if (is.na(kind) || !kind %in% c("engine", "host"))
    return(.job_fail(j, "broken", paste(x, collapse = " ")))
  .job_fail(j, if (identical(kind, "engine")) "error" else "broken",
            paste(x[-1], collapse = " "))
}

.job_refresh <- function(j, now = Sys.time()) {
  if (!identical(j$state, "running")) return(j$state)
  rf <- file.path(j$dir, "result.rds")
  ef <- file.path(j$dir, "exit")
  if (file.exists(rf)) { j$state <- "done"; return("done") }
  if (file.exists(ef)) return(.job_exit_reason(j, ef))
  if (is.na(j$pid)) {
    p <- safe(suppressWarnings(as.integer(.job_read(file.path(j$dir, "pid"))))[1], NA_integer_)
    if (!is.na(p)) j$pid <- p
  }
  el <- suppressWarnings(as.numeric(difftime(now, j$launched, units = "secs")))
  if (!is.finite(el)) el <- 0
  if (is.na(j$pid)) {
    if (el > JOB_START_GRACE_SECS)
      return(.job_fail(j, "nostart", "no conversion process wrote a process id"))
    return("running")
  }
  if (el > j$timeout) {
    job_kill_tree(j$pid)
    return(.job_fail(j, "timeout", sprintf("still running after %.0f seconds", el)))
  }
  if (as.numeric(now) >= j$probe_at) {
    j$probe_at <- as.numeric(now) + JOB_PROBE_SECS
    if (identical(job_pid_alive(j$pid), FALSE)) {
      # The process can finish between the file tests above and this probe. Look again before
      # calling it dead: a finished conversion reported as a failure throws away work.
      if (file.exists(rf)) { j$state <- "done"; return("done") }
      if (file.exists(ef)) return(.job_exit_reason(j, ef))
      return(.job_fail(j, "stopped", .job_stopped_detail(j, now)))
    }
  }
  "running"
}

.job_read <- function(p) if (file.exists(p)) safe(readLines(p, warn = FALSE), character(0)) else character(0)
# THE EMPTY ANSWER IS LOAD-BEARING: a process killed from outside says nothing at all, and the
# caller has to tell that apart from one that spoke. Blank lines are not words.
.job_tail <- function(p, n = 20L) {
  x <- trimws(.job_read(p))
  x <- x[nzchar(x)]
  if (!length(x)) return("")
  paste(utils::tail(x, n), collapse = " | ")
}

# What a maintainer can act on when a conversion process disappears; this lands in logs/errors.log and
# never on screen. A child killed from OUTSIDE writes nothing, so silence is itself the finding.
.job_stopped_detail <- function(j, now = Sys.time()) {
  el <- suppressWarnings(as.numeric(difftime(now, j$launched, units = "secs")))
  said <- .job_tail(file.path(j$dir, "out.log"))
  sprintf("%s task, process %s, gone after %s. %s",
          as.character(j$task %||% "unknown")[1],
          if (is.na(j$pid)) "(no process id)" else as.character(j$pid),
          if (isTRUE(is.finite(el))) sprintf("%.0fs", el) else "an unknown time",
          if (nzchar(said)) paste("Its last output was:", said)
          else paste("It left no output of its own, so nothing inside the conversion",
                     "asked it to stop - look for an out-of-memory kill, an operator",
                     "kill, or a restart of this machine at that time."))
}

job_pump <- function() {
  if (isTRUE(.JOB_CTL$draining)) return(invisible(0L))
  jobs <- .job_all()
  if (!length(jobs)) return(invisible(0L))
  for (j in jobs) if (identical(j$state, "running")) .job_refresh(j)
  running <- sum(vapply(jobs, function(j) identical(j$state, "running"), logical(1)))
  slots <- job_max_concurrent() - running
  if (slots <= 0L) return(invisible(0L))
  q <- Filter(function(j) identical(j$state, "queued"), jobs)
  if (!length(q)) return(invisible(0L))
  q <- q[order(vapply(q, function(j) j$seq, integer(1)))]
  started <- 0L
  for (j in q[seq_len(min(length(q), slots))]) { .job_launch(j); started <- started + 1L }
  invisible(started)
}

job_poll <- function(j) {
  job_pump()
  if (identical(j$state, "running")) .job_refresh(j)
  j$state
}

job_queue_ahead <- function(j) {
  sum(vapply(.job_all(), function(k)
    !identical(k$id, j$id) && k$state %in% c("queued", "running") && k$seq < j$seq,
    logical(1)))
}

job_progress <- function(j) {
  ln <- .job_read(file.path(j$dir, "progress"))
  ln <- ln[grepl("^[0-9]+/[0-9]+ .", ln)]
  if (!length(ln)) return(NULL)
  last <- ln[length(ln)]
  list(i = as.integer(sub("^([0-9]+)/.*$", "\\1", last)),
       n = as.integer(sub("^[0-9]+/([0-9]+) .*$", "\\1", last)),
       file = sub("^[0-9]+/[0-9]+ ", "", last))
}

job_result <- function(j) {
  if (!identical(j$state, "done")) return(NULL)
  if (!is.null(j$result)) return(j$result)
  r <- safe(readRDS(file.path(j$dir, "result.rds")), NULL)
  if (is.null(r)) {
    .job_fail(j, "error", "the conversion's result file could not be read back")
    return(NULL)
  }
  safe(job_seed_input_cache(safe(readRDS(file.path(j$dir, "inputs.rds")), NULL)))
  j$result <- r
  r
}

job_failure <- function(j) {
  if (!identical(j$state, "failed")) return(NULL)
  list(kind = j$reason_kind %||% "unknown",
       reason = j$reason %||% "the conversion did not finish",
       detail = j$detail %||% NA_character_)
}

# Stop it if it is still going, delete its folder, forget it. Called when a result is taken, when a
# new conversion supersedes this one, and on session end - a tab closed mid-scan must not leave OCR.
job_reap <- function(j) {
  if (is.null(j)) return(invisible(FALSE))
  if (identical(j$state, "running")) {
    deadline <- Sys.time() + 2
    while (is.na(j$pid) && Sys.time() < deadline) {
      p <- safe(suppressWarnings(as.integer(.job_read(file.path(j$dir, "pid"))))[1], NA_integer_)
      if (!is.na(p)) j$pid <- p else Sys.sleep(0.02)
    }
    if (!is.na(j$pid)) job_kill_tree(j$pid)
  }
  if (!is.null(j$dir) && dir.exists(j$dir)) safe(unlink(j$dir, recursive = TRUE))
  if (!is.null(j$id) && exists(j$id, envir = .JOB_REG, inherits = FALSE))
    rm(list = j$id, envir = .JOB_REG)
  j$state <- "reaped"
  job_pump()          # the slot is free now, not at the next poll
  invisible(TRUE)
}

job_reap_all <- function() {
  .JOB_CTL$draining <- TRUE
  on.exit(.JOB_CTL$draining <- FALSE, add = TRUE)
  for (j in .job_all()) safe(job_reap(j))
  invisible(TRUE)
}
