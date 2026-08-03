# test-deployment.R -- the SEAM between RUN-ME.bat and the bundle builder.
#
# The offline bundle is the actual deliverable, and its two halves never meet
# until the folder is on an air-gapped Windows server with a non-technical analyst
# in front of it. RUN-ME.bat looks for files by name; scripts/bundle-offline.R puts
# files there under a name; nothing has ever checked the two agree. That is how a
# bundle could be built with no installer in it: the builder copied
# scripts/install-offline.R to offline/install-on-pc.R only `if (file.exists(...))`,
# so renaming or moving the source produced a bundle that was missing the one file
# RUN-ME.bat runs, said nothing, exited 0, and failed days later where nobody could
# fix it.
#
# So this file does two things a .bat file cannot do for itself:
#   1. reads both halves as text and asserts they name the same files, so renaming
#      one half fails the suite;
#   2. actually RUNS the builder's plain-R sections against a SYNTHETIC repo in a
#      temp folder and inspects the bundle that comes out. No internet, no Windows,
#      and never a real statement -- the fake repo is built from scratch here.
#
# test-deployment-docs.R covers the decisions recorded in these files; this covers
# whether the files still fit together.

.dep2_root  <- function() engine_root()
.dep2_lines <- function(rel) readLines(file.path(.dep2_root(), rel), warn = FALSE)
.dep2_text  <- function(rel) paste(.dep2_lines(rel), collapse = "\n")

# ---------------------------------------------------------------------------
# Running the builder here.
#
# bundle-offline.R is one script with numbered "## n." sections. Sections 2 and 3
# need CRAN and GitHub; 1, 4 and 5 (copy the app / copy the installer / write the
# manifest) are plain R and are exactly the ones that decide what ends up in the
# folder. Slicing by section HEADER rather than by line number means editing
# inside a section does not break these tests, while deleting a section does.
# ---------------------------------------------------------------------------

.bo_src <- function() readLines(file.path(.dep2_root(), "scripts", "bundle-offline.R"), warn = FALSE)

# .bo_part(src, n) -- section n as text; n = 0 is everything above section 1.
.bo_part <- function(src, n) {
  hdr <- grep("^## [0-9]+\\.", src)
  testthat::expect_gt(length(hdr), 0L)
  if (identical(n, 0L)) return(paste(src[seq_len(hdr[1] - 1L)], collapse = "\n"))
  i <- which(sub("^## ([0-9]+)\\..*$", "\\1", src[hdr]) == as.character(n))
  testthat::expect_length(i, 1L)          # a renamed section must not pass silently
  b <- if (i == length(hdr)) length(src) else hdr[i + 1L] - 1L
  paste(src[hdr[i]:b], collapse = "\n")
}

# .bo_fake_repo(private) -- a throwaway repo with the real layout and no real data.
# `private` names files to drop into samples/_private_staging, standing in for the
# genuine bank statements that live there on a maintainer's machine.
.bo_fake_repo <- function(private = character(0)) {
  root <- tempfile("fakerepo"); dir.create(root)
  for (d in c("R", "templates", "templates_user", "templates_seed", "fields_templates",
              "config", "scripts", "www", "tests/testthat/logs", "docs",
              "samples/raw/anz", "samples/_private_staging", "dictionaries"))
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
  w <- function(rel, txt = "x") writeLines(txt, file.path(root, rel))
  for (f in c("app.R", "ui_content.R", "ui_labels.R", "CHANGELOG.md", "run.R",
              "README.md", "RUN-ME.bat")) w(f)
  w("VERSION", "9.9.9")
  w("R/util.R"); w("templates/t.yaml"); w("www/app.css")
  w("config/config.example.yaml"); w("config/config.yaml", "admin_password: secret")
  w("dictionaries/labels.yaml"); w("dictionaries/lexicon.yaml")
  w("scripts/run_app.R", "# the app launcher RUN-ME.bat starts")
  w("samples/raw/anz/specimen.pdf", "public specimen")
  w("samples/_private_staging/README.md", "only this README travels")
  w("tests/testthat/logs/run.log", "a run log naming a real file")
  for (p in private) w(file.path("samples/_private_staging", p), "PRETEND REAL STATEMENT")
  file.copy(file.path(.dep2_root(), "scripts", "install-offline.R"),
            file.path(root, "scripts", "install-offline.R"))
  root
}

# .bo_preamble() -- just the definitions, evaluated somewhere harmless. The
# preamble deletes and recreates the dist folder RELATIVE TO THE WORKING
# DIRECTORY, so it is never evaluated where the suite is running.
.bo_preamble <- function() {
  wd <- tempfile("bundlewd"); dir.create(wd)
  old <- setwd(wd); on.exit(setwd(old), add = TRUE)
  e <- new.env(parent = globalenv())
  # suppressWarnings: the preamble locates itself from commandArgs("--file="),
  # which under the suite is a path relative to a working directory we have just
  # left. Harmless here, and re-pointed at the fake repo below.
  suppressWarnings(utils::capture.output(eval(parse(text = .bo_part(.bo_src(), 0L)), envir = e)))
  e
}

# .bo_build(fake, parts) -- run those sections against the fake repo, in a temp
# working directory (the builder writes its dist folder relative to the cwd).
# Returns the dist path, the console output and the evaluation environment.
.bo_build <- function(fake, parts = c(1L, 4L, 5L), poppler = TRUE, tesseract = TRUE,
                      r_installer = TRUE) {
  src <- .bo_src()
  wd  <- tempfile("bundlewd"); dir.create(wd)
  old <- setwd(wd); on.exit(setwd(old), add = TRUE)
  e <- new.env(parent = globalenv())
  out <- suppressWarnings(utils::capture.output(eval(parse(text = .bo_part(src, 0L)), envir = e)))
  # The preamble works out the repo from the SCRIPT's own path. Under the suite
  # that is the real repo -- which holds real statements -- so re-point it at the
  # synthetic one before section 1 copies anything. Asserted, not assumed.
  e$app_root  <- normalizePath(fake, winslash = "/")
  e$.self_dir <- function() file.path(normalizePath(fake, winslash = "/"), "scripts")
  testthat::expect_false(identical(e$app_root, normalizePath(.dep2_root(), winslash = "/")))
  # Stand-ins for what sections 2 and 3 would have produced.
  e$got_poppler <- poppler; e$got_tesseract <- tesseract; e$rfull <- "4.4.1"
  e$r_dest <- file.path(e$prereq, "R-4.4.1-win.exe")
  if (r_installer) writeLines("stub installer", e$r_dest)
  for (n in parts) out <- c(out, utils::capture.output(eval(parse(text = .bo_part(src, n)), envir = e)))
  list(dist = file.path(wd, "StatementStudio-offline"), out = paste(out, collapse = "\n"), env = e)
}

# ------------------------------------------------- B1: the bundle's own installer

test_that("the builder puts its own installer in the bundle, under the name RUN-ME.bat runs", {
  b <- .bo_build(.bo_fake_repo())
  expect_true(file.exists(file.path(b$dist, "offline", "install-on-pc.R")))
  # ...and it is the real installer, not an empty placeholder.
  expect_identical(readLines(file.path(b$dist, "offline", "install-on-pc.R"), warn = FALSE),
                   readLines(file.path(.dep2_root(), "scripts", "install-offline.R"), warn = FALSE))
})

test_that("a bundle that would have no installer is not built at all", {
  # The whole B1 failure: the source renamed, the copy skipped, the build reporting
  # success. RUN-ME.bat runs install-on-pc.R unconditionally on the first launch, so
  # a bundle without it installs NOTHING -- unlike a missing Poppler, which costs
  # only scanned PDFs. Same class as a missing R installer, so the same treatment:
  # fail on the internet PC, where it can still be fixed.
  fake <- .bo_fake_repo()
  file.rename(file.path(fake, "scripts", "install-offline.R"),
              file.path(fake, "scripts", "install_offline.R"))
  expect_error(.bo_build(fake), "installer is missing")
})

# ------------------------------------------------ B2: nothing drops out in silence

test_that("the manifest states every prerequisite, present or absent", {
  # updating.md tells the operator to check the manifest for MISSING before
  # carrying the USB stick to the server. The manifest is the only thing that
  # travels with it, so a prerequisite it does not mention cannot be checked at
  # all -- it used to name two of the five.
  b <- .bo_build(.bo_fake_repo(), poppler = FALSE, tesseract = TRUE)
  man <- readLines(file.path(b$dist, "offline", "manifest.txt"), warn = FALSE)
  for (key in c("r_installer:", "installer:", "packages:", "poppler:", "tesseract:",
                "app_payload:", "dictionaries:", "private_data:"))
    expect_true(any(grepl(key, man, fixed = TRUE)), info = paste("manifest omits", key))
  # an absent prerequisite is named as MISSING and says what it costs
  expect_match(grep("^  poppler:", man, value = TRUE), "MISSING")
  expect_match(paste(man, collapse = "\n"), "scanned PDFs will not be readable", fixed = TRUE)
  # and the present ones are not reported as missing
  for (key in c("^  r_installer:", "^  installer:", "^  tesseract:", "^  app_payload:"))
    expect_false(grepl("MISSING", grep(key, man, value = TRUE)), info = key)
})

test_that("a bundle missing part of the app is a build failure, not a printed count", {
  # file.copy() reports failure by return value and the copy loop discards it, so
  # "copied N items" counts attempts. Without the app itself there is nothing to
  # start, and the person carrying it has no way to know.
  fake <- .bo_fake_repo()
  unlink(file.path(fake, "app.R"))
  expect_error(.bo_build(fake), "never arrived")
})

test_that("a live config.yaml that survived the prune stops the build", {
  # unlink() reports failure by return value, and that value was discarded. A
  # config.yaml still in the bundle carries the ADMIN PASSWORD out of the building,
  # and a bundle copied over an existing install would overwrite the server's real
  # settings -- feed folder included -- with the build PC's.
  expect_match(.bo_part(.bo_src(), 1L),
               'if (file.exists(file.path(dist, "config", "config.yaml")))', fixed = TRUE)
  b <- .bo_build(.bo_fake_repo())
  expect_false(file.exists(file.path(b$dist, "config", "config.yaml")))
  expect_true(file.exists(file.path(b$dist, "config", "config.example.yaml")))
})

test_that("a package that never downloaded fails the build instead of the server", {
  # makeRepo warns and carries on. The only proof the air-gapped box can install is
  # a .zip per package sitting in repo/ -- and there is no CRAN there to fix a gap.
  e <- .bo_preamble()
  repo <- tempfile("repo"); dir.create(file.path(repo, "bin/windows/contrib/4.3"), recursive = TRUE)
  file.create(file.path(repo, "bin/windows/contrib/4.3", c("shiny_1.8.0.zip", "yaml_2.3.8.zip")))
  expect_identical(e$.missing_from_repo(c("shiny", "yaml"), repo), character(0))
  expect_identical(e$.missing_from_repo(c("shiny", "pdftools", "yaml"), repo), "pdftools")
  # the guard that uses it must still be wired into the package section
  expect_match(.bo_part(.bo_src(), 2L), ".missing_from_repo(", fixed = TRUE)
  expect_match(.bo_part(.bo_src(), 2L), "did not download", fixed = TRUE)
})

# ------------------------------------------- real statements never leave the box

test_that("no real statement can travel inside the bundle", {
  # samples/_private_staging holds REAL bank statements. Its own README promises
  # the folder "stays local ... never included in any package", and .gitignore
  # refuses to commit it -- but it sits inside samples/, which the bundle ships, and
  # file.copy(recursive = TRUE) takes a directory whole. So every one of them was
  # copied onto the USB stick that leaves this unit. Only the README travels.
  b <- .bo_build(.bo_fake_repo(private = c("anz_0382004_jun.pdf", "victim_statement.pdf")))
  staged <- file.path(b$dist, "samples", "_private_staging")
  expect_identical(list.files(staged, recursive = TRUE, all.files = TRUE), "README.md")
  # run logs name the statements that were processed; they do not travel either
  expect_false(dir.exists(file.path(b$dist, "tests", "testthat", "logs")))
  # ...while the public specimen corpus the golden tests need still does
  expect_true(file.exists(file.path(b$dist, "samples", "raw", "anz", "specimen.pdf")))
  # and the build says so, so it is visible on the console as well as in the manifest
  expect_match(b$out, "private staging excluded", fixed = TRUE)
})

test_that("the pruning is proved, not just attempted", {
  # If a future edit drops a never-ship path back into the payload, the build must
  # stop rather than trust that the prune covered it.
  src <- .bo_part(.bo_src(), 1L)
  expect_match(src, "never_ship", fixed = TRUE)
  expect_match(src, "private data would have shipped", fixed = TRUE)
  e <- .bo_preamble()
  expect_true("samples/_private_staging" %in% e$never_ship)
  expect_identical(e$never_ship_keep, "samples/_private_staging/README.md")
})

# ------------------------------ B4: the two halves must keep naming the same files

test_that("every script RUN-ME.bat runs exists, in the repo or in the bundle", {
  # The seam that let B1 through. Renaming either half now fails here.
  txt <- .dep2_lines("RUN-ME.bat")
  # only the lines that actually INVOKE Rscript, not the ones testing for it
  calls <- trimws(sub('^\\s*"%RSCRIPT%"\\s*', "", grep('^\\s*"%RSCRIPT%"\\s', txt, value = TRUE)))
  # An unrecognised invocation is a failure too: a new one has to be accounted for
  # here, not discovered on the server.
  expect_setequal(calls, c('"%APP%\\scripts\\run_app.R"', "install-on-pc.R"))

  # the repo-side one is a real file, in a folder the bundle ships
  expect_true(file.exists(file.path(.dep2_root(), "scripts", "run_app.R")))
  b <- .bo_build(.bo_fake_repo())
  expect_true(file.exists(file.path(b$dist, "scripts", "run_app.R")))

  # the bundle-side one is produced by the builder, into the folder RUN-ME.bat
  # looks in, under exactly the name it looks for
  expect_true(file.exists(file.path(.dep2_root(), "scripts", "install-offline.R")))
  expect_true(file.exists(file.path(b$dist, "offline", "install-on-pc.R")))
})

test_that("RUN-ME.bat and the builder agree on where the bundle folder is", {
  bat <- .dep2_text("RUN-ME.bat")
  expect_match(bat, 'set "BUNDLE=%APP%\\offline"', fixed = TRUE)
  e <- .bo_preamble()
  expect_identical(basename(e$bundle), "offline")
  expect_identical(basename(e$prereq), "prereqs")
  # the R installer: RUN-ME.bat globs it, the builder names it
  expect_match(bat, '"%BUNDLE%\\prereqs\\R-*-win.exe"', fixed = TRUE)
  b <- .bo_build(.bo_fake_repo())
  found <- Sys.glob(file.path(b$dist, "offline", "prereqs", "R-*-win.exe"))
  expect_length(found, 1L)
})

test_that("the dictionary and config names RUN-ME.bat seeds from are the ones shipped", {
  bat <- .dep2_text("RUN-ME.bat")
  # :dictOne is called with a live filename and seeds from "<name>.example<.ext>";
  # the builder renames each shipped dictionary with the same transform. Compute
  # both and demand they match, rather than eyeballing two spellings.
  called <- trimws(sub("^\\s*call :dictOne\\s+", "",
                       grep("^\\s*call :dictOne\\s", .dep2_lines("RUN-ME.bat"), value = TRUE)))
  expect_gt(length(called), 0L)
  for (f in called) {
    expect_true(file.exists(file.path(.dep2_root(), "dictionaries", f)),
                info = paste("RUN-ME.bat seeds a dictionary the repo does not have:", f))
    wants  <- paste0(tools::file_path_sans_ext(f), ".example.", tools::file_ext(f))  # %~n1.example%~x1
    builds <- sub("\\.(ya?ml)$", ".example.\\1", f)                                  # the builder's rename
    expect_identical(builds, wants)
  }
  # and the bundle really contains those files under those names
  b <- .bo_build(.bo_fake_repo())
  for (f in called)
    expect_true(file.exists(file.path(b$dist, "dictionaries",
                                      paste0(tools::file_path_sans_ext(f), ".example.", tools::file_ext(f)))),
                info = paste("the bundle has no seed for", f))
  # config is seeded the same way, from a file that must exist
  expect_match(bat, "config\\config.example.yaml", fixed = TRUE)
  expect_true(file.exists(file.path(.dep2_root(), "config", "config.example.yaml")))
  expect_true(file.exists(file.path(b$dist, "config", "config.example.yaml")))
  # ...and the live config never ships, or a rebuild would overwrite the server's
  expect_false(file.exists(file.path(b$dist, "config", "config.yaml")))
})

# ------------------------------------------------- B3: RUN-ME.bat as the entry point

test_that("a failed start is reported to Task Scheduler as a failure", {
  # The app's exit code used to be dropped: the script ended on `goto :eof` after a
  # pause, so every run looked successful and "restart on failure" never fired. An
  # app that dies at startup then stays dead, silently. :rfail already exits
  # non-zero for exactly this reason.
  txt <- .dep2_lines("RUN-ME.bat")
  run <- grep('"%RSCRIPT%" "%APP%\\scripts\\run_app.R"', txt, fixed = TRUE)
  expect_length(run, 1L)
  nxt <- grep("^:", txt); nxt <- nxt[nxt > run][1]      # up to the next label only
  blk <- txt[seq(run + 1L, nxt - 1L)]
  expect_match(paste(blk, collapse = "\n"), 'set "RC=%ERRORLEVEL%"', fixed = TRUE)
  # endlocal and exit MUST be one line: on two, cmd has already discarded RC by the
  # time it expands %RC%, and the script silently exits 0 again.
  expect_match(paste(blk, collapse = "\n"), "endlocal & exit /b %RC%", fixed = TRUE)
  expect_length(grep("^\\s*endlocal\\s*$", blk), 0L)
})

test_that("a missing installer on the server is diagnosed as itself", {
  # Before pushd, not after: pushd into a folder that does not exist leaves the
  # next command running in the WRONG directory. And the generic setup-failed
  # message blames R packages, which sends the operator to rebuild for a reason
  # that was never true.
  txt <- .dep2_lines("RUN-ME.bat")
  chk  <- grep('if not exist "%BUNDLE%\\install-on-pc.R"', txt, fixed = TRUE)
  push <- grep('pushd "%BUNDLE%"', txt, fixed = TRUE)
  expect_length(chk, 1L)
  expect_length(push, 1L)
  expect_lt(chk, push)
  bat <- paste(txt, collapse = "\n")
  expect_match(bat, "The offline installer is missing", fixed = TRUE)
  expect_match(bat, "manifest.txt", fixed = TRUE)   # points at what would have said so
})

test_that("nothing in RUN-ME.bat leaves the app running from the wrong folder", {
  # A service or Task Scheduler entry starts the batch file with an arbitrary
  # working directory. Everything therefore has to be anchored to %~dp0, and the
  # app script has to re-anchor itself as well.
  bat <- .dep2_text("RUN-ME.bat")
  expect_match(bat, 'set "APP=%~dp0"', fixed = TRUE)
  expect_match(bat, '"%RSCRIPT%" "%APP%\\scripts\\run_app.R"', fixed = TRUE)
  # the only cwd change is a pushd, which is undone
  expect_length(grep("^\\s*cd\\s", .dep2_lines("RUN-ME.bat")), 0L)
  expect_length(grep("^\\s*pushd ", .dep2_lines("RUN-ME.bat")), 1L)
  expect_length(grep("^\\s*popd\\s*$", .dep2_lines("RUN-ME.bat")), 1L)
  # run_app.R does not trust the inherited working directory either
  app <- .dep2_text("scripts/run_app.R")
  expect_match(app, "setwd(app_dir)", fixed = TRUE)
  expect_match(app, "--file=", fixed = TRUE)
})

test_that("the private R stays private and the machine's own R is left alone", {
  # The stated design: whatever R or RStudio is already installed is ignored and
  # untouched. That rests entirely on these flags and on never resolving Rscript
  # from PATH.
  bat <- .dep2_text("RUN-ME.bat")
  expect_match(bat, "!recordversion", fixed = TRUE)   # does not become the machine's R
  expect_match(bat, "!associate", fixed = TRUE)       # does not grab .RData
  expect_match(bat, '/DIR="%RUNTIME%"', fixed = TRUE) # installs inside the app folder
  expect_match(bat, 'set "R_LIBS_USER=%RLIB%"', fixed = TRUE)
  expect_match(bat, 'set "R_LIBS_SITE="', fixed = TRUE)
  # never `where Rscript` / a bare Rscript: that would pick up the server's R,
  # which has none of the packages. (make-bundle.bat runs on the BUILD pc, where
  # finding the local R is the whole point, so it is exempt.)
  expect_length(grep("where Rscript", .dep2_lines("RUN-ME.bat"), fixed = TRUE), 0L)
  expect_length(grep("^\\s*Rscript", .dep2_lines("RUN-ME.bat")), 0L)
  # both R layouts are handled: bin\x64\ and bin\ (32-bit-era installs put it in
  # neither, and that path fails loudly at :rfail rather than starting wrong)
  expect_match(bat, '%RUNTIME%\\bin\\x64\\Rscript.exe', fixed = TRUE)
  expect_match(bat, '%RUNTIME%\\bin\\Rscript.exe', fixed = TRUE)
})

# ------------------------------------- install-offline.R: the user's PATH is theirs

# .io_env() -- the helper block of install-offline.R (everything above section 1),
# so the PATH logic can be exercised without a registry.
.io_env <- function() {
  src <- .dep2_lines("scripts/install-offline.R")
  cut <- grep("^## 1\\.", src)[1]
  testthat::expect_false(is.na(cut))
  e <- new.env(parent = globalenv())
  # see .bo_preamble: the script's own self-location warns when evaluated from a
  # different working directory, and nothing under test depends on it.
  suppressWarnings(eval(parse(text = paste(src[seq_len(cut - 1L)], collapse = "\n")), envir = e))
  e
}

test_that("a PATH that could not be read is never used to build a new one", {
  # This is the bug, in one line: the old code did
  #   cur <- sub(..., grep("Path", q, value = TRUE)[1])
  # and a failed query makes that NA. nzchar(NA) is TRUE, so NA was treated as a
  # real PATH and the machine ended up with a user PATH of literally
  # "NA;C:\...\poppler\bin" -- everything that was there, gone, silently.
  e <- .io_env()
  seen <- NULL
  e$system2 <- function(command, args, ...) { seen <<- args; 0L }
  e$.reg    <- function(args) list(ok = FALSE, out = character(0))
  expect_true(is.na(e$.user_path()))
  out <- utils::capture.output(res <- e$.add_to_user_path("C:/app/poppler/bin", "Poppler"))
  expect_false(res)
  expect_null(seen)                                   # setx was never called
  expect_match(paste(out, collapse = " "), "NOT changed", fixed = TRUE)
  expect_match(paste(out, collapse = " "), "C:/app/poppler/bin", fixed = TRUE)
})

test_that("an account with no PATH of its own still gets one, with no NA in it", {
  # "the key reads, but this user has no Path value" is a fresh service account,
  # not a failure -- creating the PATH is right there. Telling it apart from an
  # unreadable registry is the whole reason .user_path() exists.
  e <- .io_env()
  seen <- NULL
  e$system2 <- function(command, args, ...) { seen <<- args; 0L }
  e$.reg    <- function(args) list(ok = length(args) == 2L, out = character(0))
  expect_identical(e$.user_path(), "")
  out <- utils::capture.output(res <- e$.add_to_user_path("C:/app/poppler/bin", "Poppler"))
  expect_true(res)
  expect_identical(seen[[1]], "PATH")
  expect_false(grepl("NA;", seen[[2]], fixed = TRUE))
  expect_match(seen[[2]], "C:/app/poppler/bin", fixed = TRUE)
})

test_that("an existing PATH is appended to, not replaced", {
  e <- .io_env()
  seen <- NULL
  e$system2 <- function(command, args, ...) { seen <<- args; 0L }
  e$.reg <- function(args) if (length(args) == 2L) list(ok = TRUE, out = character(0)) else
    list(ok = TRUE, out = c("", "HKEY_CURRENT_USER\\Environment",
                            "    Path    REG_EXPAND_SZ    %USERPROFILE%\\bin;C:\\tools"))
  # unexpanded, so a %USERPROFILE% in the stored value survives the round trip
  expect_identical(e$.user_path(), "%USERPROFILE%\\bin;C:\\tools")
  utils::capture.output(e$.add_to_user_path("C:/app/poppler/bin", "Poppler"))
  expect_match(seen[[2]], "%USERPROFILE%", fixed = TRUE)
  expect_match(seen[[2]], "C:\\tools", fixed = TRUE)
  expect_match(seen[[2]], "C:/app/poppler/bin", fixed = TRUE)
  # already there -> left alone
  seen <- NULL
  e$.reg <- function(args) if (length(args) == 2L) list(ok = TRUE, out = character(0)) else
    list(ok = TRUE, out = "    Path    REG_SZ    C:\\tools;C:/app/poppler/bin")
  utils::capture.output(e$.add_to_user_path("C:/app/poppler/bin", "Poppler"))
  expect_null(seen)
})

test_that("setx is not allowed to truncate the PATH in silence", {
  # setx cuts the value at 1024 characters and only whispers about it. A truncated
  # PATH is a broken machine that nothing announces, so refuse and say what to do.
  e <- .io_env()
  seen <- NULL
  e$system2   <- function(command, args, ...) { seen <<- args; 0L }
  e$.user_path <- function() paste(rep("C:\\padding\\directory", 60), collapse = ";")
  out <- utils::capture.output(res <- e$.add_to_user_path("C:/app/poppler/bin", "Poppler"))
  expect_false(res)
  expect_null(seen)
  expect_match(paste(out, collapse = " "), "1024", fixed = TRUE)
})

test_that("both OCR tools go through the same careful PATH append", {
  # Tesseract used to repeat the whole read/append inline, so it re-ran the same
  # bug AND overwrote the Poppler entry that had just been added -- leaving a
  # server that shipped Poppler unable to read a single scanned page.
  lines <- .dep2_lines("scripts/install-offline.R")
  # exactly ONE place in the whole script writes the PATH...
  expect_length(grep('system2("setx"', lines, fixed = TRUE), 1L)
  # ...and both OCR tools reach it through the same guarded helper
  expect_length(grep("^\\s*\\.add_to_user_path\\(", lines), 2L)
  expect_length(grep("^\\.add_to_user_path <- function", lines), 1L)
  # The key it asks for is the KEY. It used to be written "HKCU\\Environment" in R
  # source -- a DOUBLED backslash, so the string handed to reg.exe was not the key
  # name at all. Asserted on the value passed, not on the source spelling, because
  # backslashes in R source are exactly what went wrong here.
  e <- .io_env(); asked <- list()
  e$.reg <- function(args) { asked[[length(asked) + 1L]] <<- args; list(ok = FALSE, out = character(0)) }
  e$.user_path()
  expect_identical(asked[[1]][2], "HKCU\\Environment")
  expect_identical(nchar(asked[[1]][2]), 16L)          # one backslash, not two
})

# ------------------------------------------------------------------- ASCII only

test_that("no non-ASCII byte survives in any deployment script", {
  # These files are read by cmd.exe and by R on a Windows server whose code page is
  # not UTF-8. A byte above 127 is mojibake on the console at best, and a parse
  # error at worst -- on the one machine where nobody can edit the file. The engine
  # is already held to this; the deployment half was not covered by anything.
  files <- c("RUN-ME.bat", "make-bundle.bat",
             file.path("scripts", c("bundle-offline.R", "install-offline.R", "run_app.R",
                                    "audit-statement.R", "bulk-audit.R")))
  offenders <- character(0)
  for (f in files) {
    raw <- readBin(file.path(.dep2_root(), f), "raw", file.info(file.path(.dep2_root(), f))$size)
    bad <- which(as.integer(raw) > 127L)
    if (length(bad)) offenders <- c(offenders, sprintf("%s byte %d", f, bad[1]))
  }
  expect_identical(offenders, character(0),
                   info = paste("non-ASCII:", paste(offenders, collapse = " | ")))
})
