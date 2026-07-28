# jobs.R -- run a conversion in its OWN short-lived R process, and poll for it.
#
# WHY THIS EXISTS. R is single-threaded and Statement Studio is ONE process
# serving the whole team, so the engine call inside the Convert observer froze
# EVERY other analyst's browser for as long as it ran. Measured on this box with
# two browsers open: while one person converted a scanned statement the other
# page answered nothing for 65 seconds -- no spinner, no error, no refusal, just
# a dead screen. That is the silent failure the charter forbids on the screen,
# and it gets worse with every extra user.
#
# WHY A PROCESS PER CONVERSION, rather than a worker pool or an async framework:
#   * NO NEW DEPENDENCIES (charter). system2(wait = FALSE) here and
#     invalidateLater() in app.R are base R and base Shiny. `parallel` and
#     `tools`, used below, ship with R itself.
#   * measured: a bare Rscript starts in 0.20s and sourcing all of R/ takes that
#     to 0.27s. That is ~7% on the CHEAPEST conversion (a 2.9s CSV) and ~2% on a
#     scan, so a persistent pool would buy back almost nothing.
#   * the engine is ALREADY safe to run concurrently and is not what is being
#     fixed: every conversion writes its own run-log file (write_log_record,
#     R/logging.R), its own outdir, and a run id carrying a random suffix
#     (R/convert.R), so two processes converting the same statement in the same
#     second cannot collide. test-jobs.R pins that with real concurrent runs.
#
# HOW THE CHILD REPORTS BACK -- files in the job's own directory, nothing else.
# No pipe, no socket, no shared memory: a file is the only channel that survives
# the child dying, and a child that dies is the case this has to get right. The
# parent never blocks on the child and never reads a half-written file:
#
#   args.rds    written by the PARENT: the task, the paths, the engine arguments.
#   child.R     written by the PARENT: the script the process runs. A real file
#               rather than Rscript -e, because quoting an expression through a
#               shell is the kind of thing that works until a path has a space in
#               it -- and because a maintainer can re-run the exact failing
#               conversion by hand with `Rscript child.R`.
#   pid         written by the CHILD, first thing. system2() does not hand back a
#               process id, and the child knows its own.
#   progress    appended by the CHILD, one line per file of a batch.
#   inputs.rds  the reader's content-keyed cache (see job_input_cache).
#   result.rds  the answer. Written as result.part and RENAMED, because rename
#               within a directory is atomic: the file exists only when it is
#               whole, so "does it exist" is a safe test for "is it finished".
#   exit        the child's own reason for producing no result, same rename trick.
#   out.log     stdout + stderr, for the maintainer. Never shown to a user.
#
# A child that dies without writing ANYTHING -- segfault, or the OOM killer
# taking the biggest process on a 4-core box running six OCR jobs -- writes
# neither result.rds nor exit. That case is the whole reason job_poll also probes
# whether the process is still alive, and why there is a hard timeout behind
# that: a job must always end in "done" or "failed", never in a hang.

# The cap on simultaneous jobs. Ten analysts must not be able to start ten OCR
# processes on a four-core box: past the core count they do not go faster, they
# go slower together, and the last one can take the machine down. Floor of 2 so a
# single-core VM still overlaps a scan with somebody's CSV; ceiling of 6 because
# beyond that the disk and the OCR engine, not the queue, are the limit.
JOB_CAP_MIN <- 2L
JOB_CAP_MAX <- 6L
# How long a child may take before it is declared failed. Generous on purpose: a
# 4.4 MB scan measured over two minutes, and killing a real conversion is worse
# than waiting. It exists so that a job whose process cannot be probed still
# ENDS -- loudly -- rather than spinning forever.
JOB_TIMEOUT_SECS <- 1800
# How long to wait for the child to write its pid before concluding no process
# ever started (Rscript missing, no permission to execute, a full disk).
JOB_START_GRACE_SECS <- 60
# Liveness is asked at most this often per job. On Linux it is a stat() and free;
# elsewhere it forks `ps`/`tasklist`, and six jobs polled twice a second would be
# 12 processes a second spent asking a question that changes slowly.
JOB_PROBE_SECS <- 2

# The registry is per PROCESS, deliberately: it holds the jobs THIS app started,
# which is exactly the set it is responsible for capping, polling and reaping.
.JOB_REG <- new.env(parent = emptyenv())
.JOB_CTL <- new.env(parent = emptyenv())

.job_next_seq <- function() {
  n <- (.JOB_CTL$seq %||% 0L) + 1L
  .JOB_CTL$seq <- n
  n
}
.job_all <- function() mget(ls(.JOB_REG), envir = .JOB_REG)

# job_set_max_concurrent(n) -- the deployment's cap, set once at startup from
# config (app.max_concurrent_jobs). NULL/absent means "work it out from the
# hardware". A value that is not a whole number of jobs is REFUSED out loud
# rather than silently rounded away: a site that typed the wrong thing in
# config.yaml must not discover it as mysterious slowness months later.
job_set_max_concurrent <- function(n = NULL) {
  if (is.null(n) || (length(n) == 1L && is.na(n))) {
    .JOB_CTL$cap <- NULL
    return(invisible(job_max_concurrent()))
  }
  v <- suppressWarnings(as.integer(n)[1])
  if (is.na(v) || v < 1L) {
    warning(sprintf(paste("app.max_concurrent_jobs is '%s', which is not a number of jobs.",
                          "Statement Studio is using %d instead."),
                    paste(as.character(n), collapse = " "), job_max_concurrent()),
            call. = FALSE, immediate. = TRUE)
    .JOB_CTL$cap <- NULL
    return(invisible(job_max_concurrent()))
  }
  .JOB_CTL$cap <- v
  invisible(v)
}

# job_max_concurrent() -- how many conversions may run at once.
job_max_concurrent <- function() {
  if (!is.null(.JOB_CTL$cap)) return(.JOB_CTL$cap)
  # detectCores() can return NA (a container with no cpuinfo); treat that as the
  # smallest machine rather than as "no limit".
  cores <- suppressWarnings(as.integer(parallel::detectCores())[1])
  if (is.na(cores) || cores < 1L) cores <- 2L
  max(JOB_CAP_MIN, min(JOB_CAP_MAX, cores - 1L))
}

# job_threads_per_job() -- how many CPU threads ONE conversion may use.
#
# CAPPING PROCESSES IS NOT ENOUGH, and this is the measurement that says so.
# OCR is done by the `tesseract` binary (R/ocr.R), which is OpenMP-parallel and
# by default takes a thread per core. Three conversions at once on this four-core
# box is therefore twelve threads fighting over four cores, and it does not
# degrade gracefully: one scanned statement reads in 36 seconds alone, and three
# of them together had not finished a single PAGE after ten minutes. Uncapped,
# the concurrency this file exists to allow would be far worse than the queue it
# replaces. (The repo's own test runner has always been invoked with
# OMP_THREAD_LIMIT=1 for the same reason.)
#
# So the cap means THREADS, not just processes: cap x threads never exceeds the
# machine. THE TRADE-OFF IS REAL AND IS THE RIGHT WAY ROUND. One analyst alone on
# a four-core box gets one thread instead of four, so a lone scan is slower than
# it was; the moment a second analyst clicks Convert the pair finish sooner than
# they would have queued, and nobody's page was ever frozen. A site that would
# rather have the old single-conversion speed can set app.max_concurrent_jobs: 1
# and get every core back for it -- still queued, still announced.
job_threads_per_job <- function() {
  cores <- suppressWarnings(as.integer(parallel::detectCores())[1])
  if (is.na(cores) || cores < 1L) cores <- 1L
  max(1L, cores %/% max(1L, job_max_concurrent()))
}

# The R that is running THIS app, not whatever `Rscript` a service account's PATH
# happens to find. A team VM with two R versions installed must not convert with
# one of them and serve with the other.
.job_rscript <- function() {
  exe <- if (identical(.Platform$OS.type, "windows")) "Rscript.exe" else "Rscript"
  p <- file.path(R.home("bin"), exe)
  if (file.exists(p)) p else exe
}

# Where job folders live: inside THIS process's temp dir, so R clears them when
# the app stops. Deliberately not the "cv_"/"ts_" prefixes sweep_temp_dirs()
# reclaims (R/retention.R) -- that sweep must never delete a job that is running.
.job_root <- function(jobs_dir = NULL) {
  d <- jobs_dir %||% file.path(tempdir(), "ss_jobs")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# job_pid_alive(pid) -> TRUE / FALSE / NA. NA means "this host cannot tell", and
# the caller must read that as "assume it is working" -- declaring a running
# conversion dead would throw away real work, which is the worse error of the two.
job_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1])
  if (is.na(pid) || pid <= 0L) return(NA)
  # Linux (the deployment VM): /proc is authoritative and costs one small read, so
  # it can be asked often without forking anything.
  #
  # A ZOMBIE IS NOT ALIVE. /proc/<pid> outlives the process itself until whoever
  # inherited it collects the exit status, and the conversions here are reparented
  # to init the moment they start (system2 launches them through a shell that
  # exits). Measured: a killed child sat in state Z for about a second. Reading
  # only "does the folder exist" therefore reported a finished conversion as still
  # running -- and on a child that CRASHED, that is a second of the screen saying
  # "converting" about a process that is already gone.
  if (dir.exists("/proc")) {
    st <- safe(suppressWarnings(
      readLines(file.path("/proc", as.character(pid), "stat"), warn = FALSE)[1]), NA_character_)
    if (is.na(st) || !nzchar(st)) return(FALSE)
    # Fields counted from the LAST ")": the command name is field 2 and may itself
    # contain spaces and brackets. Field 3 is the state letter.
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

# job_kill_tree(pid) -- stop a conversion AND everything it started.
#
# A conversion is not one process. The engine shells out: every scanned page is
# read by the `tesseract` BINARY through system2 (R/ocr.R), and pages are
# rendered by `pdftoppm`. Killing only the R child leaves those running.
# MEASURED, and it is not a small leak: five sessions closed their tabs and three
# orphaned tesseract processes went on burning a core each for fourteen minutes
# on pages no browser was left to read -- on the same four-core box that was
# supposed to be free for the next analyst.
#
# The tree is collected while the parent is STILL ALIVE. The instant it dies its
# children are reparented to init and there is nothing left linking them to the
# conversion they belong to.
job_kill_tree <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1])
  if (is.na(pid) || pid <= 0L) return(invisible(FALSE))
  if (identical(.Platform$OS.type, "windows")) {
    # /T is the whole tree, /F is "do not ask" -- there is nobody at the console.
    safe(suppressWarnings(system2("taskkill", c("/PID", as.character(pid), "/T", "/F"),
                                  stdout = FALSE, stderr = FALSE)))
    return(invisible(TRUE))
  }
  kids <- .job_descendants(pid)
  safe(tools::pskill(pid))               # the root first, so it starts nothing more
  for (p in kids) safe(tools::pskill(p))
  invisible(TRUE)
}

# Every process and its parent, once. /proc on Linux (the deployment VM) because
# it is free and forks nothing; `ps` anywhere else.
.job_ppid_map <- function() {
  if (dir.exists("/proc")) {
    ps <- suppressWarnings(as.integer(list.files("/proc", pattern = "^[0-9]+$")))
    ps <- ps[!is.na(ps)]
    pp <- vapply(ps, function(p) {
      # suppressWarnings as well as safe(): a process that exits between the
      # listing and this read makes file() WARN before it errors, and a tidy-up
      # must not print a page of noise onto the server console.
      st <- safe(suppressWarnings(
        readLines(file.path("/proc", as.character(p), "stat"), warn = FALSE)[1]),
        NA_character_)
      if (is.na(st) || !nzchar(st)) return(NA_integer_)
      # Field 2 is the command name in brackets and MAY CONTAIN SPACES AND
      # BRACKETS, so the fields are counted from the LAST ")" -- splitting the
      # whole line on spaces reads the wrong number for half the processes on the
      # box, which is how a kill-the-tree ends up killing something else.
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
  # Bounded. A conversion's tree is R -> sh -> tesseract, three deep; a ppid table
  # read one row at a time can disagree with itself while processes come and go,
  # and a walk that trusts it must not be able to loop for ever.
  for (depth in seq_len(10L)) {
    kids <- tab$pid[!is.na(tab$ppid) & tab$ppid %in% frontier]
    kids <- setdiff(kids, c(out, pid))
    if (!length(kids)) break
    out <- c(out, kids); frontier <- kids
  }
  out
}

# The script the child runs. deparse() writes the paths as proper R string
# literals, so a folder containing a space or a backslash still resolves.
#
# NOTHING IN HERE MAY PRINT. out.log is where the child's stdout and stderr go,
# and it is the ONLY evidence a maintainer has about a process that was killed
# from outside -- see .job_stopped_detail. Rscript auto-prints the value of every
# visible top-level expression, so the locale call below put its own return value
# ("C.UTF-8") on line 1 of every child's log, and a job killed before it printed
# anything else was recorded in logs/errors.log as a locale string where the
# reason should be. Every top-level statement here is therefore invisible(), and
# a new one must be too: what reaches out.log is then the child's real voice --
# an R warning, a tesseract complaint, a kernel message - or nothing at all,
# which is itself a finding.
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
    "# a reason and call it the whole one.",
    '.say <- function(txt) {',
    '  p <- file.path(.jd, "exit.part")',
    '  writeLines(paste(as.character(txt), collapse = " "), p)',
    '  invisible(file.rename(p, file.path(.jd, "exit")))',
    '}',
    "invisible(tryCatch({",
    "  setwd(.root)",
    '  for (f in list.files(file.path(.root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)',
    '  a <- readRDS(file.path(.jd, "args.rds"))',
    "  res <- job_run_task(a$task, a$paths, a$args, jobdir = .jd)",
    "  # The reader's cache BEFORE the answer: the parent seeds itself from this",
    "  # the moment it sees result.rds, so both files must already be on disk.",
    '  saveRDS(job_input_cache(), file.path(.jd, "inputs.part"))',
    '  file.rename(file.path(.jd, "inputs.part"), file.path(.jd, "inputs.rds"))',
    '  saveRDS(res, file.path(.jd, "result.part"))',
    '  file.rename(file.path(.jd, "result.part"), file.path(.jd, "result.rds"))',
    "}, error = function(e) .say(conditionMessage(e))))",
    "# Belt and braces: leaving with neither file would read to the parent as a",
    "# process that vanished, which is a different (and less useful) diagnosis.",
    'if (!file.exists(file.path(.jd, "result.rds")) && !file.exists(file.path(.jd, "exit")))',
    '  .say("the conversion ended without producing a result")')
}

# job_input_cache() / job_seed_input_cache(x) -- hand the reader's work across the
# process boundary.
#
# THIS IS NOT AN OPTIMISATION, it is what keeps the fix from undoing itself.
# read_input() memoises by content hash (R/read_input.R) because the GUI reads the
# same upload four or five times per result -- the X-ray, the page count, the
# coverage diagnostic. Converting in a child leaves the SERVER's cache cold, so
# the first of those renders would re-OCR the scan in the Shiny process and freeze
# every browser again, one render later than before. Measured on the ASB scan:
# 36.3s to read it cold, 0.02s from the cache, and 20 KB to carry the cache over.
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

# job_run_task(task, paths, args) -- WHAT A CHILD DOES. Three tasks, because the
# app has three places that used to block: one statement, a whole case folder, and
# the maintainer's bulk audit. It lives here rather than in the child script so it
# is ordinary, testable R that the suite can call directly.
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
      # The maintainer's bulk audit: a picture of a folder, then (optionally) the
      # heavier pass that really converts and logs each file so the runs feed
      # Insights. Both moved here whole from app.R; nothing about what they do
      # changed, only which process does it.
      tmpl <- load_template_set(args$templates_dir, args$user_templates_dir)
      prog <- .job_progress_writer(jobdir)
      b <- batch_audit(paths, templates = tmpl)
      conv <- NULL
      if (isTRUE(args$convert)) {
        nms <- as.character(args$names %||% basename(paths))
        rows <- vector("list", length(paths))
        for (i in seq_along(paths)) {
          if (is.function(prog)) prog(i, length(paths), nms[i])
          r <- tryCatch(convert_statement(paths[i], outdir = args$outdir,
                 templates_dir = args$templates_dir,
                 user_templates_dir = args$user_templates_dir,
                 logdir = args$logdir, requested_by = "batch"),
                 error = function(e) NULL)
          csv <- if (!is.null(r)) r$outputs[grepl("\\.csv$", r$outputs)] else character(0)
          nrw <- if (length(csv) && file.exists(csv[1]))
            tryCatch(nrow(utils::read.csv(csv[1], check.names = FALSE)),
                     error = function(e) NA_integer_) else NA_integer_
          rows[[i]] <- data.frame(file = nms[i], status = r$status %||% "failed",
            template = r$template_id %||% NA_character_,
            trust = r$trust$level %||% NA_character_,
            n_rows = nrw, stringsAsFactors = FALSE)
        }
        conv <- do.call(rbind, rows)
      }
      list(audit = b, converted = conv)
    },
    stop(sprintf("no such job task: '%s'", as.character(task)[1])))
}

# Per-file progress the parent can SHOW. A fifty-file case that looks frozen is a
# case the analyst kills half way, and the parent cannot see inside this process.
# APPENDED, one short line per file: an append cannot damage the lines already
# written, so the worst a half-written last line can cost is one stale label --
# and job_progress() drops any line that does not match the shape.
.job_progress_writer <- function(jobdir) {
  if (is.null(jobdir)) return(NULL)
  p <- file.path(jobdir, "progress")
  function(i, n, f) safe(cat(sprintf("%d/%d %s\n", i, n, basename(f)),
                             file = p, append = TRUE))
}

# job_start(input_path, outdir, ...) -> a job handle.
#
# The handle is an ENVIRONMENT on purpose. A job has identity and changing state,
# and every caller must see the same job: passing a list around would give the
# Shiny observer a snapshot that silently stops matching the process it names.
# Fields: id, task, paths, outdir, dir, state, started, launched, pid.
#
# The job is registered QUEUED and job_pump() decides whether a process starts
# now. With a free slot that happens on this very call, so the ordinary
# single-user case never sees a queue at all.
job_start <- function(input_path, outdir, ..., task = "convert",
                      root = getwd(), jobs_dir = NULL,
                      timeout_secs = JOB_TIMEOUT_SECS, rscript = NULL) {
  paths <- as.character(input_path)
  if (!length(paths)) stop("job_start needs at least one input path")
  seq <- .job_next_seq()
  id <- sprintf("job%05d", seq)
  jd <- file.path(.job_root(jobs_dir), id)
  dir.create(jd, recursive = TRUE, showWarnings = FALSE)
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
  # system2 answers about the LAUNCH, not the conversion. A non-zero answer here
  # means no process was started at all, and that must not sit as "running" until
  # the grace period runs out when we already know.
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
    stopped = "the conversion process stopped before it finished",
    timeout = sprintf("the conversion was still running after %d minutes",
                      as.integer(round(j$timeout / 60))),
    nostart = "no conversion process started",
    "the conversion did not finish")
  "failed"
}

# .job_refresh(j) -- is this job still occupying a slot? Cheap file tests first,
# then a throttled liveness probe, then the timeout. Never reads the result: that
# is the owning session's business and can wait for its own poll.
.job_refresh <- function(j, now = Sys.time()) {
  if (!identical(j$state, "running")) return(j$state)
  rf <- file.path(j$dir, "result.rds")
  ef <- file.path(j$dir, "exit")
  if (file.exists(rf)) { j$state <- "done"; return("done") }
  if (file.exists(ef)) return(.job_fail(j, "error", .job_read(ef)))
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
      # The process can finish in the instant between the file tests above and
      # this probe. Look again before calling it dead: a finished conversion
      # reported as a failure would throw away real work an analyst is waiting on.
      if (file.exists(rf)) { j$state <- "done"; return("done") }
      if (file.exists(ef)) return(.job_fail(j, "error", .job_read(ef)))
      return(.job_fail(j, "stopped", .job_stopped_detail(j, now)))
    }
  }
  "running"
}

.job_read <- function(p) if (file.exists(p)) safe(readLines(p, warn = FALSE), character(0)) else character(0)
# .job_tail(p, n) -- the child's own last words, or "" when it left none. THE
# EMPTY ANSWER IS LOAD-BEARING: a process killed from outside says nothing at
# all, and the caller has to be able to tell that apart from one that spoke.
# Blank lines are not words, so they do not count as having spoken.
.job_tail <- function(p, n = 20L) {
  x <- trimws(.job_read(p))
  x <- x[nzchar(x)]
  if (!length(x)) return("")
  paste(utils::tail(x, n), collapse = " | ")
}

# .job_stopped_detail(j, now) -- WHAT A MAINTAINER CAN ACT ON when a conversion
# process disappears. This is the text that lands in logs/errors.log, and it is
# maintainer-only: the screen gets CONVERT_STOPPED and never a word of this.
#
# A child that R itself stopped writes the error into `exit` and is reported as
# "error". A child killed from OUTSIDE -- the OOM killer, an operator kill, a
# machine restart -- writes nothing, and this line is the only trace it leaves.
# It used to be the tail of out.log, whose line 1 was the auto-printed value of
# the child's own Sys.setlocale() call, so a job killed before printing anything
# else was recorded as
#     [1] "C.UTF-8"
# a locale string where the reason should be. The child is silent now (see
# .job_child_script), so state what THIS process actually knows: which task,
# which pid, how long it had been running, and whether the child said anything.
# Silence is itself the finding -- nothing inside the conversion asked it to
# stop -- so it points at where the cause must be instead of trailing off.
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

# job_pump() -- keep exactly as many processes running as the cap allows, oldest
# request first. Every job_poll() calls it, so ANY session polling moves the
# queue on: a queued session does not depend on the busy ones to still be there.
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

# job_poll(handle) -> "queued" | "running" | "done" | "failed".
#
# "queued" is a fourth answer on purpose. A job that has not started is not
# running, and saying it is would be the exact silence this whole change exists
# to remove -- the screen can only tell somebody they are third in line if the
# registry admits there is a line.
job_poll <- function(j) {
  job_pump()
  if (identical(j$state, "running")) .job_refresh(j)
  j$state
}

# job_queue_ahead(handle) -- how many conversions are in front of this one, so the
# screen can say it. Counts jobs asked for EARLIER that have not finished, running
# and queued alike: a running job is genuinely ahead of a queued one, and the
# number falls by one each time any of them lands.
job_queue_ahead <- function(j) {
  sum(vapply(.job_all(), function(k)
    !identical(k$id, j$id) && k$state %in% c("queued", "running") && k$seq < j$seq,
    logical(1)))
}

# job_progress(handle) -> list(i, n, file) for a batch, NULL when there is nothing
# to say yet. A torn final line is dropped rather than shown half-read.
job_progress <- function(j) {
  ln <- .job_read(file.path(j$dir, "progress"))
  ln <- ln[grepl("^[0-9]+/[0-9]+ .", ln)]
  if (!length(ln)) return(NULL)
  last <- ln[length(ln)]
  list(i = as.integer(sub("^([0-9]+)/.*$", "\\1", last)),
       n = as.integer(sub("^[0-9]+/([0-9]+) .*$", "\\1", last)),
       file = sub("^[0-9]+/[0-9]+ ", "", last))
}

# job_result(handle) -- the object the child produced, or NULL if it did not get
# that far. Reading it also seeds this process's reader cache (see
# job_input_cache), because the screen is about to ask for the same file again.
job_result <- function(j) {
  if (!identical(j$state, "done")) return(NULL)
  if (!is.null(j$result)) return(j$result)
  r <- safe(readRDS(file.path(j$dir, "result.rds")), NULL)
  if (is.null(r)) {
    # The one file that proves the work happened is unreadable. That is a failure,
    # and it has to be reported as one: handing back an empty result would put a
    # blank answer on screen as though it were the statement's.
    .job_fail(j, "error", "the conversion's result file could not be read back")
    return(NULL)
  }
  safe(job_seed_input_cache(safe(readRDS(file.path(j$dir, "inputs.rds")), NULL)))
  j$result <- r
  r
}

# job_failure(handle) -> list(kind, reason, detail). `reason` is a plain sentence
# fit for a screen; `detail` is the child's own words and belongs in the
# maintainer's error log, never in front of an analyst.
job_failure <- function(j) {
  if (!identical(j$state, "failed")) return(NULL)
  list(kind = j$reason_kind %||% "unknown",
       reason = j$reason %||% "the conversion did not finish",
       detail = j$detail %||% NA_character_)
}

# job_reap(handle) -- stop it if it is still going, delete its folder, forget it.
# Called when a result has been taken, when a new conversion supersedes this one,
# and on session end: a browser tab closed mid-scan must not leave an OCR process
# running for two minutes on work nobody will ever read.
job_reap <- function(j) {
  if (is.null(j)) return(invisible(FALSE))
  if (identical(j$state, "running")) {
    # The child writes its pid a fraction of a second after launch, and reaping
    # inside that window -- a tab closed the instant after Convert -- would leave
    # a process nobody owns. So wait the short moment it takes to appear. Bounded,
    # because "no pid ever" means no process started and there is nothing to kill.
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

# job_reap_all() -- for shutdown and for tests. Leaving a child running after the
# app has stopped means an OCR process nobody owns.
job_reap_all <- function() {
  # Draining, so job_reap's pump cannot start a queued job at the very moment
  # everything is being stopped -- a process launched into a closing app is the
  # orphan this function exists to prevent.
  .JOB_CTL$draining <- TRUE
  on.exit(.JOB_CTL$draining <- FALSE, add = TRUE)
  for (j in .job_all()) safe(job_reap(j))
  invisible(TRUE)
}
