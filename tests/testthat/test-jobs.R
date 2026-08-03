# test-jobs.R -- the job registry (R/jobs.R): one conversion, one process.
#
# These are BEHAVIOUR tests, not source scans: R/jobs.R was put in R/ precisely so
# the queue, the cap, the failure paths and the concurrency guarantee can be
# driven without a browser. Several really do launch R processes -- that is the
# point, because the thing being pinned is what happens between processes.

.jobs_reset <- function() {
  job_reap_all()
  job_set_max_concurrent(NULL)
  invisible(TRUE)
}

# .await(handles, secs) -- poll until every job is terminal, or give up. Returns
# the states. Polls exactly the way app.R does, so what the suite exercises is the
# loop the screen runs.
.await <- function(hs, secs = 240) {
  t0 <- Sys.time()
  repeat {
    st <- vapply(hs, job_poll, character(1))
    if (all(st %in% c("done", "failed"))) return(st)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > secs) return(st)
    Sys.sleep(0.15)
  }
}

.csv_fixture <- function() fixture("samples/raw/anz/anz_transaction_export_01.csv")

# A throwaway run-log folder per test. The suite must never write into the real
# logs/runs/: those records are the audit trail a conversion is defended from, and
# a test run sitting in Admin -> Insights beside real casework is a lie about what
# this machine has done.
.tlog <- function() { d <- tempfile("tjoblog_"); dir.create(d, recursive = TRUE); d }

# ---------------------------------------------------------------------------
# The cap. A four-core box must not run ten OCR processes because ten people
# clicked Convert.
test_that("the cap leaves a core for the app, and is floored and ceilinged", {
  on.exit(.jobs_reset())
  job_set_max_concurrent(NULL)
  cores <- suppressWarnings(as.integer(parallel::detectCores())[1])
  if (is.na(cores) || cores < 1L) cores <- 2L
  expect_identical(job_max_concurrent(), max(2L, min(6L, cores - 1L)))
  expect_gte(job_max_concurrent(), 2L)     # a single-core VM still overlaps two
  expect_lte(job_max_concurrent(), 6L)     # a 64-core one does not run 63 OCRs
})

test_that("a site can set the cap, and a setting that is not a number is refused out loud", {
  on.exit(.jobs_reset())
  expect_identical(job_set_max_concurrent(4L), 4L)
  expect_identical(job_max_concurrent(), 4L)
  # absent / blank means "work it out from the hardware", not "no limit"
  job_set_max_concurrent(NULL)
  expect_identical(job_max_concurrent(), max(2L, min(6L, max(1L,
    suppressWarnings(as.integer(parallel::detectCores())[1]) %||% 2L) - 1L)))
  # a typo in config.yaml must never read as "unlimited", and must never be silent
  expect_warning(job_set_max_concurrent("lots"), "max_concurrent_jobs")
  expect_identical(job_max_concurrent(), job_max_concurrent())
  expect_gte(job_max_concurrent(), 2L)
  expect_warning(job_set_max_concurrent(0), "max_concurrent_jobs")
  expect_gte(job_max_concurrent(), 2L)
})

# THE SHAPES AN OPERATOR ACTUALLY TYPES. Every one of these was accepted in
# silence, and the first two are the ones that take a server down: the cap is a
# count of whole R PROCESSES, each of which then shells out to tesseract.
test_that("a cap an operator could not have meant is refused, and the tool keeps working", {
  on.exit(.jobs_reset())
  .jobs_reset()
  auto <- job_max_concurrent()                     # what the hardware says
  bad <- function(v) {
    expect_warning(job_set_max_concurrent(v), "max_concurrent_jobs",
                   info = paste("accepted in silence:", paste(format(v), collapse = " ")))
    # refused means REFUSED -- back to the worked-out value, never the typed one
    expect_identical(job_max_concurrent(), auto,
                     info = paste("not refused:", paste(format(v), collapse = " ")))
  }
  bad(500)          # a stray zero: 500 R processes, ~50 GB of them
  bad(1e9)
  bad(JOB_CAP_HARD_MAX + 1L)
  bad(2.7)          # silently truncated to 2, which the file claimed it refused
  bad(TRUE)         # YAML reads "max_concurrent_jobs: yes" as TRUE -> a silent 1
  bad(FALSE)
  bad(c(3, 4))      # a YAML list: the rest used to be dropped without a word
  bad(0); bad(-1); bad("three")

  # ...and a number a site could have meant is still honoured, typed either way
  expect_identical(job_set_max_concurrent(1L), 1L)
  expect_identical(job_set_max_concurrent("6"), 6L)
  expect_identical(job_set_max_concurrent(JOB_CAP_HARD_MAX), JOB_CAP_HARD_MAX)

  # THE FIGURE IN THE REFUSAL IS THE FIGURE IT THEN USES. This line is what
  # docs/operational/go-live-checklist.md has the operator read off the console;
  # it used to name the cap that was in force BEFORE the bad value, which the app
  # then did not use.
  job_set_max_concurrent(5L)
  msg <- tryCatch(job_set_max_concurrent("three"), warning = conditionMessage)
  expect_match(msg, sprintf("using %d instead", auto), fixed = TRUE)
  expect_false(grepl("using 5 instead", msg, fixed = TRUE))
  expect_identical(job_max_concurrent(), auto)
})

# ---------------------------------------------------------------------------
# THE QUEUE IS HONEST. This is the whole reason job_poll has a fourth answer: a
# job that has not started is not running, and the screen can only say "2 ahead of
# you" if the registry admits there is a line.
test_that("over the cap, jobs QUEUE and say how many are in front", {
  skip_if_not(file.exists(.csv_fixture()))
  on.exit(.jobs_reset())
  .jobs_reset()
  job_set_max_concurrent(2L)
  hs <- lapply(1:5, function(i) {
    od <- tempfile("tjob_"); dir.create(od)
    job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
              templates_dir = file.path(engine_root(), "templates"),
              logdir = .tlog(), requested_by = "TSTJOB")
  })
  states <- vapply(hs, function(h) h$state, character(1))
  expect_identical(states, c("running", "running", "queued", "queued", "queued"))
  # ...and each one can say its own place in the line, oldest first.
  expect_identical(vapply(hs, job_queue_ahead, integer(1)), 0:4)
  # a queued job has NOT been launched -- no process, no pid, nothing running
  expect_true(all(is.na(vapply(hs[3:5], function(h) h$pid, integer(1)))))
  expect_true(all(vapply(hs[3:5], function(h) !file.exists(file.path(h$dir, "pid")), logical(1))))

  st <- .await(hs)
  expect_identical(st, rep("done", 5L))
  # the line empties: nothing is ahead of anything once it is all finished
  expect_identical(sum(vapply(hs, job_queue_ahead, integer(1))), 0L)
  for (h in hs) job_reap(h)
})

test_that("polling ANY job moves the queue on, not just the busy ones", {
  # A queued session must not depend on the sessions ahead of it still being
  # there: the analyst who is waiting is the one who polls most.
  skip_if_not(file.exists(.csv_fixture()))
  on.exit(.jobs_reset())
  .jobs_reset()
  job_set_max_concurrent(1L)
  mk <- function() { od <- tempfile("tjob_"); dir.create(od)
    job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
              templates_dir = file.path(engine_root(), "templates"),
              logdir = .tlog(), requested_by = "TSTJOB") }
  a <- mk(); b <- mk()
  expect_identical(c(a$state, b$state), c("running", "queued"))
  t0 <- Sys.time()
  repeat {                                  # poll ONLY the waiting job
    if (identical(job_poll(b), "done")) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 240) break
    Sys.sleep(0.15)
  }
  expect_identical(b$state, "done")
  expect_identical(a$state, "done")         # ...which also collected the one ahead
  job_reap(a); job_reap(b)
})

# ---------------------------------------------------------------------------
# THE FAILURE PATHS. A job must always end. "Never a hang" is the promise; a
# child that dies without saying anything is the case that tests it.
test_that("a child that dies saying nothing comes back FAILED, with a readable reason", {
  on.exit(.jobs_reset())
  .jobs_reset()
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
                 templates_dir = file.path(engine_root(), "templates"),
                 logdir = .tlog(), requested_by = "TSTJOB")
  # wait for a real process, then kill it the way the OOM killer would
  t0 <- Sys.time()
  while (is.na(h$pid) && as.numeric(difftime(Sys.time(), t0, units = "secs")) < 60) {
    job_poll(h); Sys.sleep(0.05)
  }
  skip_if(is.na(h$pid), "the child never reported a pid on this host")
  tools::pskill(h$pid)
  t0 <- Sys.time()
  repeat {
    st <- job_poll(h)
    if (st %in% c("done", "failed")) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 90) break
    Sys.sleep(0.15)
  }
  # It may have finished before the signal landed (a CSV is fast); what must never
  # happen is that it sits "running" for ever with no process behind it.
  expect_true(h$state %in% c("done", "failed"))
  if (identical(h$state, "failed")) {
    f <- job_failure(h)
    expect_true(f$kind %in% c("stopped", "error"))
    expect_true(nzchar(f$reason))
    # the sentence a person would be shown says what happened, not how
    expect_false(grepl("pid|SIG|[Ee]rror in ", f$reason))
    expect_null(job_result(h))
    # THE MAINTAINER'S HALF. app.R writes f$detail, and nothing else, into
    # logs/errors.log. For a job killed before it printed anything the whole line
    # used to read
    #     [2026-07-28 06:59:05] convert job job00023 (stopped): [1] "C.UTF-8"
    # -- the auto-printed value of the child's own Sys.setlocale() call, a locale
    # string where the reason should be. It must now name what ran and point at
    # where the cause has to be.
    if (identical(f$kind, "stopped")) {
      expect_false(grepl("^\\[1\\]", trimws(f$detail)))
      expect_match(f$detail, "convert task")
      expect_match(f$detail, "gone after")
      expect_match(f$detail, "out-of-memory|last output was")
    }
  }
  job_reap(h)
})

# NOTHING IN THE CHILD MAY PRINT. Its stdout/stderr is out.log, which is the only
# evidence about a process that was killed from outside, and Rscript auto-prints
# the value of every visible top-level expression -- which is how a locale string
# came to be line 1 of every child's log and the recorded reason for every job
# that died early.
test_that("a child that runs cleanly leaves no output of its own", {
  on.exit(.jobs_reset())
  .jobs_reset()
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
                 templates_dir = file.path(engine_root(), "templates"),
                 logdir = .tlog(), requested_by = "TSTJOB")
  st <- .await(list(h), 240)
  expect_identical(unname(st), "done")            # a clean CSV converts
  said <- trimws(readLines(file.path(h$dir, "out.log"), warn = FALSE))
  expect_identical(said[nzchar(said)], character(0))
  # ...and the script itself says why, so the next line added to it keeps the rule
  s <- .job_child_script(h$dir, engine_root())
  expect_true(any(grepl("invisible(suppressWarnings(Sys.setlocale", s, fixed = TRUE)))
  expect_true(any(grepl("invisible(tryCatch({", s, fixed = TRUE)))
  job_reap(h)
})

# The line itself, without needing a process to die on cue. This is what a
# maintainer opening logs/errors.log after a 3am crash actually reads.
test_that("a stopped job records what ran, for how long, and where the cause must be", {
  jd <- tempfile("tjd_"); dir.create(jd)
  j <- new.env(parent = emptyenv())
  j$dir <- jd; j$task <- "convert"; j$pid <- 4242L; j$launched <- Sys.time() - 12

  # A child killed from OUTSIDE says nothing at all, and the silence is itself
  # the finding: nothing inside the conversion asked it to stop.
  d <- .job_stopped_detail(j)
  expect_match(d, "^convert task, process 4242, gone after [0-9]+s\\. ")
  expect_match(d, "left no output of its own")
  expect_match(d, "out-of-memory kill")
  expect_false(grepl("^\\[1\\]", d))            # never an auto-printed R value
  expect_false(grepl("UTF-8", d, fixed = TRUE))

  # ...and when the child DID leave words, they are what a maintainer wants most.
  writeLines(c("", "Error: cannot allocate vector of size 2.1 Gb"), file.path(jd, "out.log"))
  d2 <- .job_stopped_detail(j)
  expect_match(d2, "Its last output was: Error: cannot allocate vector", fixed = TRUE)
  expect_false(grepl("left no output of its own", d2, fixed = TRUE))

  # A log holding nothing but blank lines is silence, not words -- otherwise the
  # "it said nothing" finding would be reported as an empty quotation.
  writeLines(c("", "   ", ""), file.path(jd, "out.log"))
  expect_match(.job_stopped_detail(j), "left no output of its own")

  # A job that never reported a pid must still produce a sentence, not "NA".
  j$pid <- NA_integer_; unlink(file.path(jd, "out.log"))
  expect_match(.job_stopped_detail(j), "process \\(no process id\\)")
})

test_that("an engine error inside the child is reported as an error, not as a death", {
  # The two are different facts and the screen says different things about them
  # (app.R: FRIENDLY_READ_ERROR vs CONVERT_STOPPED), so the registry must not
  # collapse them into one.
  on.exit(.jobs_reset())
  .jobs_reset()
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start("no-such-file-anywhere.csv", od, task = "nonsense", root = engine_root())
  st <- .await(list(h), 120)
  expect_identical(st, "failed")
  f <- job_failure(h)
  expect_identical(f$kind, "error")
  expect_match(f$detail, "no such job task")      # the child's own words, for the log
  expect_false(grepl("no such job task", f$reason))  # ...and NOT in the screen's sentence
  job_reap(h)
})

test_that("a job whose process never starts fails instead of waiting for ever", {
  on.exit(.jobs_reset())
  .jobs_reset()
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
                 rscript = file.path(tempdir(), "definitely-not-an-r-at-all"))
  st <- .await(list(h), 120)
  expect_identical(st, "failed")
  expect_identical(job_failure(h)$kind, "nostart")
  job_reap(h)
})

# ---------------------------------------------------------------------------
# A CONVERSION IS A TREE, NOT A PROCESS.
#
# Found by running it: five sessions closed their tabs and three orphaned
# `tesseract` processes went on burning a core each for FOURTEEN MINUTES on pages
# no browser was left to read. The engine shells out (R/ocr.R), so killing the R
# child alone leaves the OCR binary behind -- it only notices its reader is gone
# when it next writes, which on a page it is still working through is never.
test_that("stopping a conversion stops the OCR binary it started", {
  scan <- fixture("samples/raw/tutorial/sample_everyday_scanned.pdf")
  skip_if_not(ocr_available())
  skip_if_not(file.exists(scan))
  skip_if_not(dir.exists("/proc"), "needs a process table this test can read")
  on.exit(.jobs_reset())
  .jobs_reset()
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start(scan, od, task = "convert", root = engine_root(),
                 templates_dir = file.path(engine_root(), "templates"),
                 logdir = .tlog(), requested_by = "TSTJOB")
  comm <- function(p) safe(suppressWarnings(
    readLines(file.path("/proc", as.character(p), "comm"), warn = FALSE)[1]), NA_character_)
  t0 <- Sys.time()
  tree <- integer(0)
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 120) {
    job_poll(h)
    if (!is.na(h$pid)) {
      d <- .job_descendants(h$pid)
      if (any(vapply(d, comm, character(1)) %in% c("tesseract", "pdftoppm"))) { tree <- d; break }
    }
    if (h$state %in% c("done", "failed")) break
    Sys.sleep(0.05)
  }
  skip_if(!length(tree), "the OCR binary was never caught running")
  expect_true(any(vapply(tree, comm, character(1)) %in% c("tesseract", "pdftoppm")))
  job_reap(h)
  # Bounded wait, not an instant assertion: a signal is delivered at once but a
  # process still has to unwind and be collected. What must never happen is that
  # it is STILL THERE a moment later, which is what fourteen minutes of orphaned
  # OCR looked like.
  t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < 15 &&
         any(vapply(c(h$pid, tree), function(p) isTRUE(job_pid_alive(p)), logical(1))))
    Sys.sleep(0.1)
  expect_false(isTRUE(job_pid_alive(h$pid)))                       # the R child
  for (p in tree) expect_false(isTRUE(job_pid_alive(p)),           # ...and its tree
                               info = paste("orphan left behind:", p, comm(p)))
})

test_that("the cap is a cap on THREADS, not just on processes", {
  # Three conversions at once on a four-core box is twelve OpenMP threads if
  # nothing says otherwise, and it does not degrade gracefully: measured, one
  # scanned statement reads in 36s alone and three together had not finished a
  # single page after ten minutes. cap x threads must not exceed the machine.
  on.exit(.jobs_reset())
  .jobs_reset()
  cores <- suppressWarnings(as.integer(parallel::detectCores())[1])
  if (is.na(cores) || cores < 1L) cores <- 1L
  expect_gte(job_threads_per_job(), 1L)
  expect_lte(job_threads_per_job() * job_max_concurrent(), max(cores, job_max_concurrent()))
  # one at a time gets the whole machine back -- the escape hatch for a site that
  # would rather have the old single-conversion speed
  job_set_max_concurrent(1L)
  expect_identical(job_threads_per_job(), cores)
  # ...and the child really is told, on the ENVIRONMENT, because the thing that
  # reads it is the tesseract binary the child shells out to.
  job_set_max_concurrent(3L)
  sc <- paste(.job_child_script("/tmp/jd", "/tmp/root"), collapse = "\n")
  expect_match(sc, sprintf('OMP_THREAD_LIMIT = "%d"', job_threads_per_job()))
  expect_match(sc, "Sys.setenv", fixed = TRUE)
})

test_that("reaping a job stops its process and forgets it", {
  skip_if_not(file.exists(.csv_fixture()))
  on.exit(.jobs_reset())
  .jobs_reset()
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
                 templates_dir = file.path(engine_root(), "templates"),
                 logdir = .tlog(), requested_by = "TSTJOB")
  d <- h$dir
  expect_true(dir.exists(d))
  job_reap(h)
  expect_false(dir.exists(d))                    # the folder goes with it
  expect_identical(h$state, "reaped")
  expect_length(ls(.JOB_REG), 0L)                # ...and so does the registry entry
})

# ---------------------------------------------------------------------------
# THE READER'S CACHE CROSSES THE PROCESS BOUNDARY.
#
# read_input() memoises by content hash because the GUI reads the same upload
# four or five times per result. Converting in a child leaves the SERVER's cache
# cold, so without this the first render after a result would re-OCR the statement
# in the Shiny process and freeze every browser again -- one render later than
# before, which is not a fix.
test_that("the child hands its reader cache back, so the app does not re-read the file", {
  skip_if_not(file.exists(.csv_fixture()))
  on.exit(.jobs_reset())
  .jobs_reset()
  clear_input_cache()
  expect_length(ls(.INPUT_CACHE), 0L)
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
                 templates_dir = file.path(engine_root(), "templates"),
                 logdir = .tlog(), requested_by = "TSTJOB")
  expect_identical(.await(list(h), 240), "done")
  expect_length(ls(.INPUT_CACHE), 0L)            # nothing yet -- the child has it
  res <- job_result(h)
  expect_identical(res$status, "ok")
  expect_gt(length(ls(.INPUT_CACHE)), 0L)        # collecting the result warms it
  # ...and it is the RIGHT file, keyed the way read_input keys it
  expect_true(file_sha256(.csv_fixture()) %in% ls(.INPUT_CACHE))
  job_reap(h)
})

# ---------------------------------------------------------------------------
# THE ENGINE IS DETERMINISTIC UNDER CONCURRENCY.
#
# The whole design rests on it: N analysts converting at once must get exactly
# what each would have got alone. Proved at the CLI by hand; pinned here so it
# stays true. Nothing about run ids, output bytes or the run log may depend on
# who else happened to be converting.
test_that("N simultaneous jobs on one statement produce identical output files", {
  skip_if_not(file.exists(.csv_fixture()))
  on.exit(.jobs_reset())
  .jobs_reset()
  job_set_max_concurrent(4L)                    # really at the same time, not queued
  n <- 4L
  ld <- .tlog()                                 # ONE log folder, so the four records meet
  hs <- lapply(seq_len(n), function(i) {
    od <- tempfile("tjob_"); dir.create(od)
    job_start(.csv_fixture(), od, task = "convert", root = engine_root(),
              templates_dir = file.path(engine_root(), "templates"),
              logdir = ld, requested_by = "TSTJOB")
  })
  expect_true(all(vapply(hs, function(h) identical(h$state, "running"), logical(1))))
  expect_identical(.await(hs), rep("done", n))
  rs <- lapply(hs, job_result)

  # 1. every one of them converted, and read the same statement the same way
  expect_identical(vapply(rs, function(r) r$status, character(1)), rep("ok", n))
  expect_length(unique(vapply(rs, function(r) r$template_id, character(1))), 1L)

  # 2. the OUTPUT BYTES are identical -- the charter's determinism promise, held
  #    across processes running at the same second on the same file
  md5 <- vapply(rs, function(r) unname(tools::md5sum(r$outputs[["csv"]])), character(1))
  expect_length(unique(md5), 1L)
  x <- vapply(rs, function(r) unname(tools::md5sum(r$outputs[["json"]])), character(1))
  expect_length(unique(x), 1L)

  # 3. ...and each run is its OWN audit record. Same statement, same second: the
  #    run id's random suffix is what stops the second run overwriting the first's
  #    record, and a forensic tool must not lose a record of work it did.
  ids <- vapply(rs, function(r) r$run_id %||% NA_character_, character(1))
  expect_length(unique(ids), n)
  expect_true(all(file.exists(file.path(ld, "runs", paste0(ids, ".json")))))
  for (h in hs) job_reap(h)
})

# ---------------------------------------------------------------------------
# VERBATIM ACROSS THE PROCESS BOUNDARY.
#
# Found by running it: app.R forces LC_CTYPE to a UTF-8 locale at startup, the
# child inherited the SERVICE's environment instead (C/ASCII on the deployment
# box), and the CSV it delivered held "Caf<U+00E9> Kr<U+00F6>ne" where the
# statement says "Cafe Kroene" with its accents. Not a warning, not a flag: a
# description silently no longer verbatim, in the file the analyst downloads, and
# a different answer from the same statement converted before this change. It is
# pinned here at the level that matters -- the OUTPUT BYTES, against the same
# conversion run in this process.
test_that("a non-ASCII description survives the process boundary, byte for byte", {
  skip_if_not(file.exists(.csv_fixture()))
  on.exit(.jobs_reset())
  .jobs_reset()
  # The app runs in a UTF-8 locale (app.R does this at startup); on a host that
  # has none there is nothing for the two processes to disagree about.
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)
  got <- FALSE
  for (loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) { got <- TRUE; break }
  skip_if_not(got, "no UTF-8 locale on this host")

  d <- tempfile("tjobenc_"); dir.create(d)
  p <- file.path(d, "accents.csv")
  con <- file(p, open = "w", encoding = "UTF-8")
  writeLines(c(
    "Type,Details,Particulars,Code,Reference,Amount,Date,ForeignCurrencyAmount,ConversionCharge",
    "Payment,Caf\u00e9 Kr\u00f6ne,M\u00fcller GmbH,note,1671190,-23.40,19/06/2014,,",
    "Salary,Payroll Ltd,Warner Bros,,Payroll,2000.00,19/06/2014,,"), con)
  close(con)

  h <- job_start(p, file.path(d, "viajob"), task = "convert", root = engine_root(),
                 templates_dir = file.path(engine_root(), "templates"),
                 logdir = .tlog(), requested_by = "TSTJOB")
  expect_identical(.await(list(h), 240), "done")
  # Collecting the result must not even WARN: the warning readRDS raised printed
  # the transaction lines of the statement onto the server console to do it.
  expect_silent(res <- job_result(h))
  here <- convert_document(p, outdir = file.path(d, "inprocess"),
                           templates_dir = file.path(engine_root(), "templates"),
                           logdir = .tlog(),
                           requested_by = "TSTJOB")
  expect_identical(res$status, "ok")
  expect_identical(res$feed_rows$description, here$feed_rows$description)
  expect_identical(res$feed_rows$description[1], "Caf\u00e9 Kr\u00f6ne")
  # THE DELIVERED FILE, not just the object: the CSV is what the analyst
  # downloads and what the dashboards are fed from.
  expect_identical(unname(tools::md5sum(res$outputs[["csv"]])),
                   unname(tools::md5sum(here$outputs[["csv"]])))
  expect_false(any(grepl("<U+", readLines(res$outputs[["csv"]], warn = FALSE), fixed = TRUE)))
  job_reap(h)
})

# ---------------------------------------------------------------------------
# A CASE FOLDER IS ONE JOB, and it still says which file it is on.
test_that("a batch job reports per-file progress the screen can read", {
  skip_if_not(file.exists(.csv_fixture()))
  on.exit(.jobs_reset())
  .jobs_reset()
  sess <- tempfile("tjobb_"); dir.create(sess)
  paths <- file.path(sess, sprintf("case_%d.csv", 1:3))
  for (p in paths) file.copy(.csv_fixture(), p)
  h <- job_start(paths, sess, task = "batch", root = engine_root(),
                 templates_dir = file.path(engine_root(), "templates"),
                 logdir = .tlog(), requested_by = "TSTJOB")
  seen <- list()
  t0 <- Sys.time()
  repeat {
    st <- job_poll(h)
    p <- job_progress(h)
    if (!is.null(p)) seen[[paste(p$i, p$file)]] <- p
    if (st %in% c("done", "failed")) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 240) break
    Sys.sleep(0.05)
  }
  expect_identical(h$state, "done")
  b <- job_result(h)
  expect_true(is.data.frame(b))
  expect_identical(nrow(b), 3L)
  expect_identical(b$status, rep("ok", 3L))
  # the progress the screen shows names the FILE, not just a bar: "is it stuck?"
  # and "how much longer?" are the same question on a fifty-file case.
  expect_gt(length(seen), 0L)
  last <- seen[[length(seen)]]
  expect_identical(last$n, 3L)
  expect_true(grepl("^case_[0-9]+\\.csv$", last$file))
  job_reap(h)
})

# ---------------------------------------------------------------------------
# The child protocol itself: the parent must never read a half-written answer.
test_that("the child writes its answer under a temporary name and renames it", {
  src <- readLines(file.path(engine_root(), "R", "jobs.R"), warn = FALSE)
  joined <- paste(src, collapse = "\n")
  # result.rds EXISTING is the parent's test for "finished", so it may only ever
  # exist whole. Same for the reason a failed child leaves behind.
  expect_match(joined, 'file.rename\\(file.path\\(.jd, "result.part"\\), file.path\\(.jd, "result.rds"\\)\\)')
  expect_match(joined, 'file.rename\\(p, file.path\\(.jd, "exit"\\)\\)')
  # ...and the cache is on disk BEFORE the result, because the result is the
  # signal the parent acts on.
  expect_lt(grep("saveRDS\\(job_input_cache", src)[1],
            grep('saveRDS\\(.answer, file.path\\(.jd, "result.part"', src)[1])
  # a child that leaves neither must not read as a hang
  expect_match(joined, "ended without producing a result", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# WHOSE FAULT WAS IT -- THE STATEMENT, OR THIS SERVER?
#
# app.R spends this one field and nothing else: `error` gets FRIENDLY_READ_ERROR
# ("it may be password-protected, an image-only scan, or not a statement"), every
# other kind gets CONVERT_STOPPED. Every failure inside the child used to arrive
# as `error`, so a broken SERVER was read out to the analyst as a broken FILE --
# sending her back to her bank to re-download a statement that was never the
# problem, while nothing anywhere said the server was in trouble.
test_that("a server fault is never reported as a fault in the analyst's file", {
  on.exit(.jobs_reset())
  .jobs_reset()
  # (a) the app folder is unreachable -- a share that blipped, a half-copied
  #     update. The child cannot even load the engine.
  bad <- tempfile("badroot_"); dir.create(file.path(bad, "R"), recursive = TRUE)
  writeLines('stop("cannot open URL: app folder unavailable")', file.path(bad, "R", "zz.R"))
  od <- tempfile("tjob_"); dir.create(od)
  h <- job_start(.csv_fixture(), od, task = "convert", root = bad)
  expect_identical(.await(list(h), 120), "failed")
  f <- job_failure(h)
  expect_identical(f$kind, "broken")           # NOT "error": nothing read the file
  expect_false(identical(f$kind, "error"))
  expect_match(f$detail, "app folder unavailable")   # the cause, for the maintainer
  expect_false(grepl("password|scan|bank", f$reason))
  job_reap(h)

  # (b) the job's OWN arguments file is damaged -- a torn write on a full disk.
  od2 <- tempfile("tjob_"); dir.create(od2)
  h2 <- job_start(.csv_fixture(), od2, task = "convert", root = engine_root(),
                  templates_dir = file.path(engine_root(), "templates"), logdir = .tlog())
  writeLines("not an rds at all", file.path(h2$dir, "args.rds"))
  expect_identical(.await(list(h2), 120), "failed")
  expect_identical(job_failure(h2)$kind, "broken")
  job_reap(h2)
})

test_that("an exit file this parent cannot read blames the server, not the file", {
  # Fail-safe direction: guessing "engine" on no evidence would put "your file may
  # be password-protected" on screen about a file nothing ever looked at.
  jd <- tempfile("tjd_"); dir.create(jd)
  j <- new.env(parent = emptyenv()); j$dir <- jd; j$timeout <- JOB_TIMEOUT_SECS
  ef <- file.path(jd, "exit")
  writeLines(c("something else entirely", "a message"), ef)
  .job_exit_reason(j, ef)
  expect_identical(j$reason_kind, "broken")
  expect_match(j$detail, "a message")          # nothing is lost from the log
  writeLines(character(0), ef)
  .job_exit_reason(j, ef)
  expect_identical(j$reason_kind, "broken")
  # ...and the two kinds the child really writes are read as themselves
  writeLines(c("engine", "could not parse page 3"), ef)
  .job_exit_reason(j, ef)
  expect_identical(j$reason_kind, "error")
  expect_identical(j$detail, "could not parse page 3")
  writeLines(c("host", "no space left on device"), ef)
  .job_exit_reason(j, ef)
  expect_identical(j$reason_kind, "broken")
  expect_identical(j$detail, "no space left on device")
  expect_false(grepl("password|bank", j$reason))
})

test_that("a job whose folder cannot be created says so, in words a maintainer can act on", {
  # A full disk, or a TEMP the service account may not write. R's own answer is
  # "cannot open the connection", which names neither the folder nor the reason --
  # and app.R puts THIS text in logs/errors.log.
  on.exit(.jobs_reset())
  blocker <- tempfile("notadir_"); writeLines("x", blocker)   # a FILE where a folder must go
  od <- tempfile("tjob_"); dir.create(od)
  err <- tryCatch({ job_start(.csv_fixture(), od, jobs_dir = file.path(blocker, "jobs")); NULL },
                  error = conditionMessage)
  expect_false(is.null(err))
  expect_match(err, "could not create the folder this conversion needs")
  expect_match(err, "disk may be full")
  expect_false(grepl("cannot open the connection", err, fixed = TRUE))
})

test_that("liveness cannot mistake 'this host cannot tell' for 'it died'", {
  # Declaring a running conversion dead throws away real work an analyst is
  # waiting on, so an unanswerable probe is NA and the caller reads NA as alive.
  expect_true(is.na(job_pid_alive(NA)))
  expect_true(is.na(job_pid_alive(0)))
  expect_true(is.na(job_pid_alive(-1)))
  expect_true(isTRUE(job_pid_alive(Sys.getpid())))
  # a pid that cannot exist is FALSE, not NA -- otherwise nothing would ever be
  # noticed as dead
  expect_false(isTRUE(job_pid_alive(2147483000L)))
})
