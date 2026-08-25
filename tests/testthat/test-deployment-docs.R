# test-deployment-docs.R -- the deployment package and the operational docs ARE
# part of the product, and they fail in exactly the way the charter forbids:
# silently. A bundle that ships live state over a server's Admin-taught words, an
# installer whose failure is recorded as success, a doc that tells the maintainer
# to run a command that cannot work -- none of these break a test, break a build,
# or say anything at all. They surface days later on an air-gapped box, and the
# person in front of them is a non-technical analyst with no way to diagnose it.
#
# So the deployment contract is asserted here, the same way the engine contract is
# asserted elsewhere. These tests read the shipped scripts and docs as text: they
# cannot run a Windows .bat or download from CRAN, but they CAN prove the decisions
# that matter are still in the file, and they fail loudly when one is undone.

.dep_root <- function() engine_root()
.dep_read <- function(rel) {
  p <- file.path(.dep_root(), rel)
  # A missing file is a FAILURE, not an error: an error aborts the rest of the
  # test, so the report would name one problem and hide the others.
  testthat::expect_true(file.exists(p), info = paste("missing file:", rel))
  if (!file.exists(p)) return("")
  paste(readLines(p, warn = FALSE), collapse = "\n")
}
.dep_lines <- function(rel) readLines(file.path(.dep_root(), rel), warn = FALSE)

# .dep_eval_assign(rel, name) -- parse an R script and evaluate ONE top-level
# assignment from it, so a test can inspect a real value (not a regex guess)
# without running the whole script (which downloads from the internet).
.dep_eval_assign <- function(rel, name) {
  exprs <- parse(file.path(.dep_root(), rel))
  for (e in exprs) {
    if (is.call(e) && length(e) == 3 &&
        as.character(e[[1]]) %in% c("<-", "=") &&
        identical(as.character(e[[2]]), name))
      return(eval(e[[3]], envir = new.env(parent = baseenv())))
  }
  stop(sprintf("no top-level assignment to '%s' in %s", name, rel))
}

# ---------------------------------------------------------------- the bundle ---

test_that("the offline bundle never ships the live dictionaries over a server's own", {
  # dictionaries/labels.yaml + lexicon.yaml are Admin-edited LIVE STATE on the
  # server: every wording and recognition marker the team has taught the tool.
  # Shipping them inside the app payload means the documented update ("replace the
  # files in the destination") silently reverts all of it, and statements that
  # reconciled last week quietly stop reconciling -- the cardinal failure, arriving
  # through a routine maintenance step.
  items <- .dep_eval_assign("scripts/bundle-offline.R", "app_items")
  expect_false("dictionaries" %in% items)
  # ...while still shipping everything that IS product.
  # "www" holds app.css. Omitting it breaks NOTHING that a test or a launch would
  # notice - the app runs, and it is entirely unstyled. That is why it is pinned.
  expect_true(all(c("R", "templates", "config", "www",
                    "app.R", "ui_content.R", "ui_labels.R", "RUN-ME.bat") %in% items))
  expect_true(file.exists(file.path(.dep_root(), "www", "app.css")))

  src <- .dep_read("scripts/bundle-offline.R")
  # It must still carry STARTING vocabularies, as examples, or a brand-new install
  # has none: same shape as config.example.yaml.
  expect_match(src, "\\.example\\.", fixed = FALSE)
  expect_match(src, 'file.path(dist, "dictionaries")', fixed = TRUE)
})

test_that("a bundle with no R installer is a hard build error, not a printed 'skipped'", {
  # grab() returns FALSE on a failed download and the call sites used to discard it:
  # the script printed DONE, exited 0, make-bundle.bat reported success, and the
  # failure surfaced days later on the air-gapped server as "Could not set up the
  # private R" -- with the internet PC no longer in the room.
  src <- .dep_read("scripts/bundle-offline.R")
  expect_match(src, "if \\(!file\\.exists\\(r_dest\\)\\) stop\\(")
})

test_that("missing OCR tooling is reported loudly, naming the consequence", {
  # Poppler/Tesseract missing is not a footnote: text PDFs, CSV and Excel keep
  # working and then a scanned statement cannot be read at all. Say so while the
  # internet is still available to fix it.
  src <- .dep_read("scripts/bundle-offline.R")
  expect_match(src, "got_poppler", fixed = TRUE)     # the result is CAPTURED...
  expect_match(src, "got_tesseract", fixed = TRUE)
  expect_match(src, "SCANNED PDFs WILL NOT BE READABLE", fixed = TRUE)  # ...and used
})

test_that("the CRAN mirror is set explicitly, never left as the @CRAN@ placeholder", {
  # Under a stock Rscript (how make-bundle.bat calls this) getOption("repos") is
  # "@CRAN@", which miniCRAN cannot resolve with nobody there to answer a prompt.
  src <- .dep_read("scripts/bundle-offline.R")
  expect_match(src, "@CRAN@", fixed = TRUE)
  expect_match(src, "options(repos = c(CRAN = cran))", fixed = TRUE)
})

test_that("the bundle records what it contains, so an air-gapped box can be identified", {
  src <- .dep_read("scripts/bundle-offline.R")
  expect_match(src, "manifest.txt", fixed = TRUE)
  expect_match(src, "r_version:", fixed = TRUE)     # which R the server will run
  expect_match(src, "app_version:", fixed = TRUE)
  expect_match(src, "packages (name version)", fixed = TRUE)
})

# ------------------------------------------------------------------ RUN-ME.bat --

test_that("the .installed marker is written ONLY when the installer succeeded", {
  # The marker means "setup is done, never do it again". Written unconditionally, a
  # FAILED install was frozen in place: every later launch skipped setup, so the
  # documented remedy ("run RUN-ME.bat again") could never re-run it and a broken
  # install stayed broken through every restart.
  txt <- .dep_lines("RUN-ME.bat")
  marker <- grep('type nul > "%BUNDLE%\\.installed"', txt, fixed = TRUE)
  expect_length(marker, 1L)
  before <- paste(txt[seq_len(marker - 1L)], collapse = "\n")
  expect_match(before, 'set "RC=%ERRORLEVEL%"', fixed = TRUE)
  expect_match(before, 'if not "%RC%"=="0"', fixed = TRUE)
  # and the failure has to be visible + retryable, not silent
  expect_match(paste(txt, collapse = "\n"), "Offline setup did NOT complete", fixed = TRUE)
})

test_that("RUN-ME.bat restores-or-seeds the dictionaries, like it already does for config", {
  txt <- paste(.dep_lines("RUN-ME.bat"), collapse = "\n")
  expect_match(txt, "call :dictSync", fixed = TRUE)
  expect_match(txt, ":dictOne", fixed = TRUE)
  expect_match(txt, "labels.yaml", fixed = TRUE)
  expect_match(txt, "lexicon.yaml", fixed = TRUE)
  # seed from the shipped example only when the server has nothing of its own
  expect_match(txt, "%~n1.example%~x1", fixed = TRUE)
  # and keep an off-folder backup so replacing the whole folder can't lose them
  expect_match(txt, "%LOCALAPPDATA%\\StatementStudio\\dictionaries", fixed = TRUE)
})

test_that("the scheduled-task path never ends on a pause", {
  # A trailing `pause` keeps the Task Scheduler entry at "Running" long after R has
  # exited, which defeats restart-on-failure -- the app is dead and Windows thinks
  # it is fine. /service suppresses every pause; a human double-click keeps them so
  # an error message stays on screen.
  txt <- .dep_lines("RUN-ME.bat")
  expect_match(paste(txt, collapse = "\n"), '"%~1"=="/service"', fixed = TRUE)
  # no unguarded `pause` anywhere: every one goes through :maybePause, which is
  # a no-op under /service.
  expect_length(grep("^\\s*pause\\s*$", txt), 0L)
  expect_length(grep("if not defined NOPAUSE pause", txt, fixed = TRUE), 1L)
  expect_true(length(grep("call :maybePause", txt, fixed = TRUE)) >= 2L)
})

# ------------------------------------------------------------ install-offline --

test_that("a package that failed to install makes setup FAIL, not quietly pass", {
  # RUN-ME.bat gates its marker on this exit status, so reporting success over a
  # broken install is what froze it in place.
  src <- .dep_read("scripts/install-offline.R")
  expect_match(src, "if (length(miss)) {", fixed = TRUE)
  expect_match(src, "quit(status = 1)", fixed = TRUE)
  expect_match(src, "SETUP INCOMPLETE", fixed = TRUE)
  # the packages it checks are the ones the bundle ships
  expect_setequal(.dep_eval_assign("scripts/install-offline.R", "pkgs"),
                  .dep_eval_assign("scripts/bundle-offline.R", "pkgs"))
})

# -------------------------------------------------------------- the doc truths --

test_that("updating.md tells the truth about what a folder-replace keeps", {
  txt <- .dep_read("docs/operational/updating.md")
  expect_match(txt, "dictionaries", fixed = TRUE)          # the omission that lost taught words
  expect_match(txt, "labels.yaml.bak", fixed = TRUE)       # the recovery app.R already writes
  expect_match(txt, "lexicon.yaml.bak", fixed = TRUE)
  expect_match(txt, "backup-and-restore.md", fixed = TRUE)
  expect_match(txt, "params.R", fixed = TRUE)              # overwritten -- must be re-applied
})

test_that("a backup-and-restore procedure exists and names every irreplaceable path", {
  # templates_user/, fields_templates_user/, doc_templates_user/, dictionaries/
  # and logs/metadata/ are the accumulated value of the tool and exist nowhere
  # else; the only documented backup used to be config.yaml, to the SAME machine.
  # doc_templates_user/ arrived with mode: document and was missed here once --
  # a folder that is not in this list is a folder nobody is told to copy.
  txt <- .dep_read("docs/operational/backup-and-restore.md")
  for (p in c("templates_user", "fields_templates_user", "doc_templates_user",
              "dictionaries", "logs\\metadata", "config.yaml"))
    expect_match(txt, p, fixed = TRUE)
  expect_match(txt, "Restoring", fixed = TRUE)
  expect_match(.dep_read("docs/operational/README.md"), "backup-and-restore.md", fixed = TRUE)
})

test_that("the maintainer runbook gives a test command that actually works on the server", {
  # Three places told the maintainer to run "Rscript tests/run_tests.R". On the
  # server that cannot work: RUN-ME.bat installs R with !recordversion, so the
  # private R is not on PATH, and the packages live in the app's own R-lib.
  txt <- .dep_read("docs/operational/maintaining-the-engine.md")
  expect_match(txt, "R-runtime\\bin\\x64\\Rscript.exe", fixed = TRUE)
  expect_match(txt, "R_LIBS_USER", fixed = TRUE)
  expect_match(txt, "tests\\run_tests.R", fixed = TRUE)
  expect_match(txt, "skipped", fixed = TRUE)     # a skip is not a pass
  # ...and the other two things an inheriting analyst needs
  expect_match(txt, "params.R", fixed = TRUE)
  expect_match(txt, "HOWTO-add-template-test.md", fixed = TRUE)
  # linked from the operational index, and the charter from both indexes
  expect_match(.dep_read("docs/operational/README.md"), "maintaining-the-engine.md", fixed = TRUE)
  expect_match(.dep_read("docs/operational/README.md"), "charter.md", fixed = TRUE)
  expect_match(.dep_read("docs/context/README.md"), "charter.md", fixed = TRUE)
})

test_that("the docs open the firewall port and name the symptom of not doing it", {
  # The app binds 0.0.0.0 and the docs promise "anyone on the network opens port
  # 8100", but Windows Server blocks unsolicited inbound TCP: on day 1 it works
  # locally and times out for every remote user.
  run <- .dep_read("docs/operational/running-and-keeping-it-up.md")
  expect_match(run, "netsh advfirewall firewall add rule", fixed = TRUE)
  expect_match(run, "localport=8100", fixed = TRUE)
  expect_match(run, "times out", fixed = TRUE)
  expect_match(.dep_read("docs/operational/first-time-setup.md"), "netsh advfirewall", fixed = TRUE)
})

test_that("the Task Scheduler recipe covers the settings that silently kill the app", {
  run <- .dep_read("docs/operational/running-and-keeping-it-up.md")
  expect_match(run, "3 days", fixed = TRUE)              # ticked by default -> app dies mid-week
  expect_match(run, "restart", ignore.case = TRUE)       # restart-on-failure
  expect_match(run, "/service", fixed = TRUE)            # no trailing pause on the service path
  expect_match(run, "Is it running?", fixed = TRUE)      # and how to check
})

test_that("build-contract.md's module map matches the modules that exist", {
  # The map named a NON-EXISTENT R/read_excel.R and covered 17 of 45 modules. A doc
  # that invents a file is worse than no doc: the maintainer goes looking for it.
  # Compared BOTH ways, so the map cannot silently rot again in either direction.
  txt <- .dep_read("docs/context/architecture/build-contract.md")
  block <- sub("(?s).*## 1\\. Directory layout", "", txt, perl = TRUE)
  block <- sub("(?s)## 2\\..*", "", block, perl = TRUE)
  listed <- unique(regmatches(block, gregexpr("(?m)^  [a-z_0-9]+\\.R\\b", block, perl = TRUE))[[1]])
  listed <- sort(trimws(listed))
  actual <- sort(list.files(file.path(.dep_root(), "R"), pattern = "\\.R$"))
  expect_identical(setdiff(listed, actual), character(0))   # no invented modules
  expect_identical(setdiff(actual, listed), character(0))   # no undocumented ones
})

test_that("build-contract.md describes the engine that exists", {
  txt <- .dep_read("docs/context/architecture/build-contract.md")
  expect_match(txt, "unsigned", fixed = TRUE)                  # the 5th amount style
  # the flags vocabulary: the real emitted set, not the old guess
  expect_false(grepl("redacted,ocr,fx,reversal,malformed", txt, fixed = TRUE))
  for (f in c("ocr_low_conf", "row_text_merged", "date_year_inferred"))
    expect_match(txt, f, fixed = TRUE)
  # logging: one file per run, and honest about the filename + period dates it holds
  expect_match(txt, "logs/runs/<run_id>.json", fixed = TRUE)
  expect_match(txt, "source_file", fixed = TRUE)
})

test_that("edge-cases.md no longer marks a shipped capability as not built", {
  rows <- grep("Wrapped multi", .dep_lines("docs/context/edge-cases.md"),
               value = TRUE, useBytes = TRUE)
  expect_length(rows, 1L)
  # not "⛔ needs real data" any more (byte-compared: the file is UTF-8 and the
  # test box's locale must not decide whether this passes)
  expect_false(grepl("⛔", rows, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("row_text_merged", rows, fixed = TRUE, useBytes = TRUE))
})

test_that("the published test-suite figures are dated, and the stale ones are gone", {
  # Deliberately NOT pinned to a number: every test added moves the totals, so a
  # hard-coded assertion here would make the docs wrong on the next commit and
  # train the maintainer to ignore this test. What must hold is that a published
  # figure is (a) not the long-dead one and (b) marked as a measurement, so nobody
  # reads it as a promise.
  # launch-audit.md was the third file checked here. It was a point-in-time
  # go/no-go written before launch, and it was deleted rather than maintained -- a
  # readiness verdict that is never re-taken decays into a claim about today that
  # nobody re-measured. The two files that DO carry live figures stay pinned.
  for (d in c("README.md", "docs/context/roadmap.md")) {
    txt <- .dep_read(d)
    expect_false(grepl("343 tests", txt, fixed = TRUE), info = d)
    expect_false(grepl("1,430", txt, fixed = TRUE), info = d)
    expect_true(grepl("measured|last full run", txt), info = d)
  }
  # ...and the maintainer's runbook states the real pass condition, which does not
  # move: nothing failed, nothing errored, nothing was skipped.
  run <- .dep_read("docs/operational/maintaining-the-engine.md")
  expect_match(run, "The pass condition is", fixed = TRUE)
  expect_match(run, "particular total", fixed = TRUE)
})

test_that("the roadmap's local-ML item is gone and the audit records why", {
  # Its two headline uses ship deterministically in R/analytics.R and are on screen
  # in Admin; the item also contradicts the charter's not-machine-learning clause.
  road <- .dep_read("docs/context/roadmap.md")
  expect_false(grepl("| 4 | **Local-ML learning loop**", road, fixed = TRUE))  # not in the backlog
  expect_match(road, "Killed - do not re-propose", fixed = TRUE)               # recorded, with the reason
  expect_false(grepl("local-ML loop (only once", road, fixed = TRUE))          # nor in the sequence
  expect_match(road, "template_drift", fixed = TRUE)
  expect_match(road, "unsupported_clusters", fixed = TRUE)

  audit <- .dep_read("docs/context/engine-audit.md")
  expect_match(audit, "SHIPPED (`template_drift()`", fixed = TRUE)
  expect_match(audit, "SHIPPED (`unsupported_clusters()`", fixed = TRUE)
  expect_match(audit, "Parked", fixed = TRUE)
  expect_match(audit, "Template synthesis from two examples", fixed = TRUE)
  expect_match(audit, "second reader", fixed = TRUE)

  # ...and the functions the docs now cite really are there, deterministically.
  expect_true(is.function(template_drift))
  expect_true(is.function(unsupported_clusters))
})

test_that("the golden-test HOWTO covers the PDF path it claims to cover", {
  txt <- .dep_read("tests/HOWTO-add-template-test.md")
  expect_match(txt, "make_pdf_fixtures.R", fixed = TRUE)
  expect_match(txt, "PDF_GOLDENS", fixed = TRUE)
  expect_match(txt, "maintaining-the-engine.md", fixed = TRUE)  # the command that works on the server
  # the generator and the goldens test it points at must exist
  expect_true(file.exists(file.path(.dep_root(), "tests/testthat/fixtures/make_pdf_fixtures.R")))
  expect_true(file.exists(file.path(.dep_root(), "tests/testthat/test-pdf_template_goldens.R")))
})

# ---------------------------------------------------------------------------
# The changelog is the first thing whoever inherits this reads, and a version
# stamp that disagrees with it is worse than none: engine_version() writes VERSION
# into every run log, every JSON output and the feed manifest, so a stale stamp
# makes "which build produced this figure?" unanswerable after the fact.
test_that("VERSION matches the newest release in the changelog", {
  v <- trimws(.dep_lines("VERSION")[1])
  expect_match(v, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  heads <- grep("^## ", .dep_lines("CHANGELOG.md"), value = TRUE)
  expect_gt(length(heads), 0)
  expect_identical(trimws(sub("^## ", "", heads[1])), v)
})

test_that("the changelog points at documents that exist", {
  txt <- .dep_read("CHANGELOG.md")
  links <- unlist(regmatches(txt, gregexpr("\\]\\([^)]+\\)", txt)))
  links <- gsub("^\\]\\(|\\)$", "", links)
  links <- links[!grepl("^https?://|^#", links)]
  missing <- links[!file.exists(file.path(.dep_root(), sub("#.*$", "", links)))]
  expect_identical(missing, character(0),
                   info = paste("changelog links to a missing file:",
                                paste(missing, collapse = ", ")))
})

# ------------------------------------------------------------ the doc graph ---
# The rules above pin the CONTENT of individual pages. These two pin the SHAPE of
# the set, which is the failure mode a doc cull introduces: a page is deleted or
# renamed and the six pages that pointed at it now send the reader to a 404, or a
# page survives with nothing linking to it and is quietly never read again.
# Neither breaks a test, a build, or the app -- which is exactly why they are
# asserted here.

.dep_md_files <- function() {
  root <- .dep_root()
  c("README.md", "CHANGELOG.md",
    sub(paste0("^", root, "/"), "",
        list.files(file.path(root, "docs"), pattern = "\\.md$",
                   recursive = TRUE, full.names = TRUE)))
}

# .dep_ref_files() -- every file that can NAME a page of this tree, not just the
# ones written in markdown. A pointer rots the same way whether it is a markdown
# link, a backticked path in a README, an R comment, or an href: the file moves
# and the reader is sent nowhere. Scanning only docs/ left three whole classes
# unguarded -- R/ engine comments, the two template-folder READMEs, and the test
# HOWTOs -- and templates_user/README.md sat pointing at a deleted page for the
# one decision it exists to explain (how a user template becomes a curated one).
.dep_ref_files <- function() {
  root <- .dep_root()
  rel <- function(dir, pat, recursive = FALSE)
    sub(paste0("^", root, "/"), "",
        list.files(file.path(root, dir), pattern = pat,
                   recursive = recursive, full.names = TRUE))
  out <- c(.dep_md_files(),
           "app.R", "ui_content.R", "ui_labels.R", "run.R",
           "templates_user/README.md", "templates_seed/README.md",
           rel("R", "\\.R$"), rel("scripts", "\\.R$"),
           rel("tests", "\\.(R|md)$", recursive = TRUE),
           rel("www", "\\.(html|css|js)$"))
  # samples/_private_staging/ is gitignored (real client statements are staged
  # there), so anything in it exists on one box and not the next -- a guard that
  # reads it would pass or fail by accident.
  unique(out[file.exists(file.path(root, out))])
}

test_that("every internal link in the docs resolves to a file that exists", {
  root <- .dep_root()
  broken <- character(0)
  for (rel in .dep_md_files()) {
    txt <- .dep_read(rel)
    links <- unlist(regmatches(txt, gregexpr("\\]\\([^)]+\\)", txt)))
    links <- gsub("^\\]\\(|\\)$", "", links)
    links <- links[!grepl("^https?://|^mailto:|^#", links)]
    links <- sub("#.*$", "", links)                 # drop the anchor
    links <- trimws(links); links <- links[nzchar(links)]
    for (l in links) {
      # relative to the linking file's own directory, the way a reader follows it
      target <- normalizePath(file.path(root, dirname(rel), l), mustWork = FALSE)
      if (!file.exists(target)) broken <- c(broken, paste0(rel, " -> ", l))
    }
  }
  expect_identical(broken, character(0),
                   info = paste("dangling doc link(s):", paste(broken, collapse = "; ")))
})

test_that("every path into this tree named OUTSIDE the docs resolves too", {
  # Same failure, wider net: a path that names a file of this tree must resolve
  # wherever it is written. Only tokens with a "/" in them are followed, so a
  # generated OUTPUT name (`report.md`, `statement.audit.md`) is never mistaken
  # for a pointer. Either reading is accepted -- relative to the naming file's own
  # folder, or from the repo root -- because both are how these are written and
  # both are how a reader follows one; what is forbidden is a path that resolves
  # NEITHER way.
  root <- .dep_root()
  broken <- character(0)
  n <- 0L
  for (rel in .dep_ref_files()) {
    txt <- paste(readLines(file.path(root, rel), warn = FALSE), collapse = "\n")
    toks <- unique(unlist(regmatches(
      txt, gregexpr("[.A-Za-z0-9_-]+(?:/[.A-Za-z0-9_-]+)+\\.md", txt, perl = TRUE))))
    for (t in toks) {
      n <- n + 1L
      if (!file.exists(file.path(root, dirname(rel), t)) &&
          !file.exists(file.path(root, t)))
        broken <- c(broken, paste0(rel, " -> ", t))
    }
  }
  expect_gt(n, 40L)                  # the scan itself must not go quiet
  expect_identical(broken, character(0),
                   info = paste("dangling page reference(s):",
                                paste(broken, collapse = "; ")))
})

test_that("no docs page is orphaned from its index", {
  # A how-to nobody can find is a how-to that does not exist. Each index must name
  # every page in its own tree (charter.md and the two indexes themselves excepted
  # -- the indexes are reached from README.md, and charter.md is named by both).
  #
  # docs/ ROOT used to be exempt BY CONSTRUCTION: this walked one directory down,
  # so overview.md, design.md and story.md -- nine hundred lines, one of them
  # opening "for a developer inheriting this cold" -- had no referrer anywhere in
  # the tree and nothing said so. Their index is README.md, the page a newcomer
  # actually opens.
  root <- .dep_root()
  named_in <- function(idx, pages, where) {
    missing <- pages[!vapply(pages, function(p) grepl(basename(p), idx, fixed = TRUE),
                             logical(1))]
    expect_identical(unname(missing), character(0),
                     info = paste0(where, " does not link: ",
                                   paste(missing, collapse = ", ")))
  }
  for (dir in c("operational", "context")) {
    pages <- list.files(file.path(root, "docs", dir), pattern = "\\.md$",
                        recursive = TRUE)
    named_in(.dep_read(file.path("docs", dir, "README.md")),
             setdiff(pages, "README.md"), paste0("docs/", dir, "/README.md"))
  }
  named_in(.dep_read("README.md"),
           list.files(file.path(root, "docs"), pattern = "\\.md$"), "README.md")
})
