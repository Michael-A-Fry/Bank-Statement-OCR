# test-docs-truth.R -- the two ways a document lies without breaking anything.
#
# The written pages are part of the product: on an air-gapped box they are the
# only thing standing between a non-technical analyst and a setting she cannot
# guess. They fail silently -- a page can print a working admin password, or send
# a reader to a folder that does not exist, and nothing builds, nothing crashes,
# no test goes red. It surfaces days later, in front of the person least able to
# diagnose it.
#
# Both rules below are asserted against the REAL engine, never a restatement of
# it: the password blocks go through load_config()/admin_password_is_default()
# from R/config.R, so if the placeholder or the refusal ever changes shape, this
# follows it instead of drifting away from it.
#
# Related, deliberately not duplicated here: test-deployment-docs.R already pins
# markdown link targets and "<dir>/<page>.md" tokens. This file covers the two
# things that scan cannot see -- what a fenced block MEANS when it is loaded, and
# a reference to a DIRECTORY, which has no ".md" on the end.

.dt_root <- function() engine_root()

# Every shipped document. README.md and CHANGELOG.md ship beside docs/ and are
# read the same way, so they are held to the same rule.
.dt_doc_files <- function() {
  root <- .dt_root()
  rel <- c("README.md", "CHANGELOG.md",
           sub(paste0("^", root, "/"), "",
               list.files(file.path(root, "docs"), pattern = "\\.md$",
                          recursive = TRUE, full.names = TRUE)))
  rel[file.exists(file.path(root, rel))]
}

# .dt_fenced(lines) -- the body of every ``` fenced block, one string per block.
# The fence language tag is ignored: what matters is whether the block LOADS as
# settings, not what it was labelled.
.dt_fenced <- function(lines) {
  marks <- grep("^\\s*```", lines)
  if (length(marks) < 2L) return(character(0))
  opens <- marks[seq(1L, length(marks) - 1L, by = 2L)]
  closes <- marks[seq(2L, length(marks), by = 2L)]
  out <- character(0)
  for (i in seq_along(opens)) {
    if (closes[i] - opens[i] < 2L) next            # an empty block holds nothing
    out <- c(out, paste(lines[(opens[i] + 1L):(closes[i] - 1L)], collapse = "\n"))
  }
  out
}

# ------------------------------------------------------- the admin password ---

test_that("no document prints an admin password that would UNLOCK the Admin tab", {
  # Admin serves template deletion, the shared dictionary and the analytics-feed
  # settings. A settings block in a page is there to be COPIED, so whatever it
  # prints becomes a real deployment's password -- and one that is printed in a
  # document is a password everybody has. The shipped placeholder is safe
  # precisely because the app treats it as "nobody has set one yet" and refuses
  # to open Admin at all; anything else in that field opens the tab.
  #
  # So: load every fenced block that carries an admin_password through the REAL
  # loader and require the tab to stay shut. A near-miss is the whole risk --
  # "change-me" is not the placeholder "changeme", so it is a working password,
  # and it reads like a placeholder to everyone who copies it.
  #
  # BSO_ADMIN_PASSWORD wins over the file in load_config(), so a value in the
  # test box's environment would decide this test. Clear it for the duration.
  env_was <- Sys.getenv("BSO_ADMIN_PASSWORD", unset = NA_character_)
  Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (!is.na(env_was)) Sys.setenv(BSO_ADMIN_PASSWORD = env_was), add = TRUE)

  checked <- 0L
  for (rel in .dt_doc_files()) {
    blocks <- .dt_fenced(readLines(file.path(.dt_root(), rel), warn = FALSE))
    for (b in blocks) {
      if (!grepl("admin_password", b, fixed = TRUE)) next
      y <- tryCatch(yaml::yaml.load(b), error = function(e) e)
      # A settings block that does not parse is its own defect: the reader is
      # told to copy it into config.yaml, and a broken config.yaml reverts the
      # whole file to built-in defaults.
      expect_false(inherits(y, "error"),
                   info = paste0(rel, ": a fenced block naming admin_password does not parse as YAML"))
      if (inherits(y, "error") || !is.list(y)) next
      if (is.null(y$app) || is.null(y$app$admin_password)) next
      p <- tempfile(fileext = ".yaml")
      on.exit(unlink(p), add = TRUE)
      yaml::write_yaml(y, p)
      cfg <- load_config(p, refresh = TRUE)
      checked <- checked + 1L
      expect_true(admin_password_is_default(cfg),
                  info = paste0(rel, ": this settings block sets a WORKING admin password (",
                                y$app$admin_password,
                                ") - copying it opens the Admin tab for whoever read the page"))
    }
  }
  # The scan itself must not go quiet: the docs DO print settings blocks, and a
  # regex that stopped matching them would turn this test into a green no-op.
  expect_gt(checked, 0L)
})

test_that("the placeholder this rests on is still the one that keeps Admin shut", {
  # The test above proves the documented value keeps the tab closed. This proves
  # the CONVERSE, so the pair cannot both pass by the refusal having gone away:
  # a value that is not the placeholder really does open it.
  env_was <- Sys.getenv("BSO_ADMIN_PASSWORD", unset = NA_character_)
  Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (!is.na(env_was)) Sys.setenv(BSO_ADMIN_PASSWORD = env_was), add = TRUE)

  shut <- tempfile(fileext = ".yaml")
  open <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(shut, open)), add = TRUE)
  yaml::write_yaml(list(app = list(admin_password = .DEFAULT_ADMIN_PASSWORD)), shut)
  yaml::write_yaml(list(app = list(admin_password = "change-me")), open)
  expect_true(admin_password_is_default(load_config(shut, refresh = TRUE)))
  expect_false(admin_password_is_default(load_config(open, refresh = TRUE)))
})

# ------------------------------------------------------------ the doc paths ---

test_that("every docs/ path named in prose is a file or folder that exists", {
  # The failure this exists for: an accountant was handed "docs/for-analysts/"
  # and there was no such folder. Nothing caught it, because every scan in this
  # suite looks for a page NAME ending in .md -- and a directory has no
  # extension, so a pointer to a whole folder was the one kind of reference
  # nobody was checking.
  #
  # Fenced blocks are skipped: they hold commands and settings, where a path is
  # an example to type ("D:/StatementStudio/feed"), not a pointer to follow.
  #
  # A token is only followed when it is unambiguously a pointer -- it ends in a
  # "/" (a folder) or carries a file extension. That deliberately ignores a
  # fragment such as a path cut off mid-word by a truncated table cell, which is
  # evidence of nothing and cannot be followed by anyone.
  root <- .dt_root()
  broken <- character(0)
  seen <- 0L
  for (rel in .dt_doc_files()) {
    lines <- readLines(file.path(root, rel), warn = FALSE)
    fence <- FALSE
    for (i in seq_along(lines)) {
      if (grepl("^\\s*```", lines[i])) { fence <- !fence; next }
      if (fence) next
      toks <- regmatches(lines[i], gregexpr("docs/[A-Za-z0-9._/-]*", lines[i]))[[1]]
      for (t in toks) {
        t <- sub("[.,;:)`'\"]+$", "", t)                  # trailing punctuation
        pointer <- grepl("/$", t) || grepl("\\.[A-Za-z0-9]+$", t)
        if (!pointer) next
        seen <- seen + 1L
        if (!file.exists(file.path(root, t)))
          broken <- c(broken, sprintf("%s:%d -> %s", rel, i, t))
      }
    }
  }
  expect_gt(seen, 20L)                 # the scan itself must not go quiet
  expect_identical(broken, character(0),
                   info = paste("docs/ path named in prose that does not exist:",
                                paste(unique(broken), collapse = "; ")))
})

test_that("the analyst's own pages are reachable from the folder she is handed", {
  # Whichever folder her pages physically live in, the name she is given has to
  # land somewhere that routes her onward. This asserts the landing, not the
  # layout: the front door exists, and it names each of her four pages.
  idx <- file.path(.dt_root(), "docs", "for-analysts", "README.md")
  # A missing index is a FAILURE, not an error: an error aborts the rest of the
  # test, so the report would name the absent folder and hide which pages the
  # index had stopped naming.
  expect_true(file.exists(idx))
  txt <- if (file.exists(idx)) paste(readLines(idx, warn = FALSE), collapse = "\n") else ""
  for (page in c("converting-statements.md", "adding-a-bank-template.md",
                 "when-something-goes-wrong.md", "survey-a-statement-with-ai.md"))
    expect_true(grepl(page, txt, fixed = TRUE),
                info = paste("the analyst's index does not name", page))
})

# ---------------------------------------------------- where a control really is ---
#
# A page that says "open X, then Y" is a claim about the SCREEN, and the screen
# moves. Twice now, work that re-homed a panel left the sentence that routes an
# analyst to it untouched -- and the wrong one was the first navigation
# instruction she is given, on the page her own index tells her to start with.
# Nothing catches that by reading the prose. app.R is the authority, so the
# assertions below read app.R.

.dt_app <- function() readLines(file.path(.dt_root(), "app.R"), warn = FALSE)

# The lines rendered INSIDE the panel that "Show me how it read this" opens.
# Found by matching parentheses forward from the conditionalPanel that gates it,
# so nothing here depends on a line number in the file that moves most.
.dt_toggle_region <- function(src = .dt_app()) {
  i <- grep('conditionalPanel("output.cv_detail_open == true"', src, fixed = TRUE)
  if (!length(i)) return(NULL)
  depth <- 0L; started <- FALSE
  for (k in i[1]:length(src)) {
    ln <- sub("#.*$", "", gsub('"[^"]*"', "", src[k]))   # strings and comments out
    depth <- depth + nchar(gsub("[^(]", "", ln)) - nchar(gsub("[^)]", "", ln))
    if (!started && depth > 0L) started <- TRUE
    if (started && depth <= 0L) return(src[i[1]:k])
  }
  NULL
}

.dt_outputs <- function(lines) {
  ids <- unlist(regmatches(lines,
    gregexpr('(uiOutput|DTOutput|plotOutput|tableOutput)\\("[A-Za-z0-9_]+"', lines)))
  unique(sub('"$', "", sub('^.*\\("', "", ids)))
}

# The label on the disclosure that holds Checks / Diagnostics / Field coverage.
# Read out of app.R for the same reason: it is the name a page has to use.
.dt_checks_panel <- function(src = .dt_app()) {
  i <- grep("output\\$cv_detail <- renderUI", src)
  if (!length(i)) return(NA_character_)
  blk <- src[i[1]:min(length(src), i[1] + 40L)]
  s <- grep("tags\\$summary\\(", blk)
  if (!length(s)) return(NA_character_)
  seg <- paste(blk[s[1]:min(length(blk), s[1] + 2L)], collapse = " ")
  lit <- gsub('^"|"$', "", unlist(regmatches(seg, gregexpr('"[^"]*"', seg))))
  lit <- lit[!grepl("cursor|font-|color|var\\(|:", lit)]
  if (!length(lit)) NA_character_ else lit[1]
}

test_that("no page sends her behind the evidence toggle for something on the page", {
  region <- .dt_toggle_region()
  # A guard that cannot find the control it guards must FAIL, not pass quietly --
  # renaming the conditionalPanel is exactly the change that would move a surface.
  expect_false(is.null(region))
  behind <- if (is.null(region)) character(0) else .dt_outputs(region)
  expect_true("ix_plot" %in% behind)          # the page picture IS behind it

  # What a page can name, and the output app.R draws it with. Every value is
  # checked against app.R below, so a renamed output fails here rather than
  # turning this test into a green no-op.
  surface <- c("See it on the page" = "ix_plot",
               "the charts"         = "cv_trend",
               "the analysis"       = "cv_trend",
               "Checks"             = "cv_kpis",
               "Diagnostics"        = "cv_diag",
               "Field coverage"     = "cv_coverage",
               "field coverage"     = "cv_coverage",
               "dashboard decision" = "cv_feed",
               "dashboard line"     = "cv_feed")
  expect_identical(setdiff(unname(surface), .dt_outputs(.dt_app())), character(0))

  toggle <- "Show me how it read this"
  # The CONTAINER is not a surface: "Checks & detail" is the disclosure the
  # checks live in, and naming it beside the toggle is how a page says where the
  # toggle sits. Strip it before looking for "Checks".
  panel <- sub("\\s*\\(.*$", "", .dt_checks_panel() %||% "Checks & detail")
  pages <- c(list.files(file.path(.dt_root(), "docs", "operational"), "\\.md$", full.names = TRUE),
             list.files(file.path(.dt_root(), "docs", "for-analysts"), "\\.md$", full.names = TRUE))
  expect_gt(length(pages), 4L)
  bad <- character(0)
  for (p in pages) {
    txt <- gsub(panel, " ", paste(readLines(p, warn = FALSE), collapse = " "), fixed = TRUE)
    # One claim per sentence; a table cell is its own claim, so a "|" ends one too.
    for (s in unlist(strsplit(txt, "(?<=[.!?])\\s+|\\|", perl = TRUE))) {
      if (!grepl(toggle, s, fixed = TRUE)) next
      for (ph in names(surface)) {
        if (!grepl(ph, s, fixed = TRUE)) next
        if (!(surface[[ph]] %in% behind))
          bad <- c(bad, sprintf("%s: '%s' is not behind '%s' - app.R renders %s outside it",
                                basename(p), ph, toggle, surface[[ph]]))
      }
    }
  }
  expect_identical(unique(bad), character(0), info = paste(unique(bad), collapse = " | "))
})

test_that("a page that routes her to Checks or Diagnostics names the panel they sit in", {
  # The other half, and the one that catches a sentence naming no surface at all
  # ("everything below lives there"): app.R draws all three tables inside ONE
  # disclosure, so a page that walks her to them and never names it has left her
  # looking for a heading that is not where it says.
  panel <- .dt_checks_panel()
  expect_false(is.na(panel))
  short <- sub("\\s*\\(.*$", "", panel)
  idx <- file.path(.dt_root(), "docs", "for-analysts", "README.md")
  expect_true(file.exists(idx))
  pages <- unique(unlist(regmatches(readLines(idx, warn = FALSE),
                                    gregexpr("[a-z0-9-]+\\.md", readLines(idx, warn = FALSE)))))
  checked <- 0L
  for (pg in pages) {
    f <- file.path(.dt_root(), "docs", "operational", pg)
    if (!file.exists(f)) next
    txt <- paste(readLines(f, warn = FALSE), collapse = " ")
    if (!grepl("Diagnostics", txt, fixed = TRUE)) next
    checked <- checked + 1L
    expect_true(grepl(short, txt, fixed = TRUE),
                info = paste0(pg, ": sends her to Diagnostics but never names \"", short, "\""))
  }
  expect_gt(checked, 0L)
})

# ------------------------------------------------------ what design.md claims ---

test_that("design.md states the ASCII rule the suite really enforces", {
  # The rule widened: test-app-adoption.R scans the three UI files and
  # test-labels.R scans R/, run.R and the test runner, both for a non-ASCII BYTE
  # anywhere -- string literals and comments included -- because an en dash in a
  # character class was eating a byte off customer values under LC_ALL=C.
  # design.md said a non-ASCII string literal was fine and that a string VALUE
  # "still passes". Proved here rather than restated: the same byte test, fed the
  # exact thing the page called fine.
  p <- tempfile(fileext = ".R"); on.exit(unlink(p), add = TRUE)
  pound <- rawToChar(as.raw(c(0xc2, 0xa3)))          # so this file stays ASCII too
  writeBin(charToRaw(paste0('x <- "', pound, '"\n')), p)
  expect_gt(length(grep("[^ -~\t]", readLines(p, warn = FALSE), useBytes = TRUE)), 0L)

  d <- paste(readLines(file.path(.dt_root(), "docs", "design.md"), warn = FALSE), collapse = "\n")
  expect_false(grepl("inside a string literal is fine", d, fixed = TRUE))
  expect_false(grepl("still passes", d, fixed = TRUE))
  expect_true(grepl("\\uXXXX", d, fixed = TRUE))     # and what to write instead
})

# Which file really defines a symbol, by reading the tree rather than the page.
.dt_defines <- function(sym) {
  rel <- c("app.R", "ui_labels.R", "ui_content.R",
           file.path("R", list.files(file.path(.dt_root(), "R"), "\\.R$")))
  pat <- paste0("^", gsub("([.$])", "\\\\\\1", sym), "[ ]*<-")
  hit <- Filter(function(f) any(grepl(pat, readLines(file.path(.dt_root(), f), warn = FALSE))), rel)
  if (length(hit)) hit[1] else NA_character_
}

test_that("design.md's add-a-check recipe never calls two files one file", {
  # Step 4 is the step the page itself flags as the one that catches people out,
  # and it said the .KPI_DIAGNOSIS entry goes in "the same file" as DIAG_PLAIN.
  # It does not: DIAG_PLAIN is wording (ui_labels.R) and .KPI_DIAGNOSIS is engine
  # (R/diagnose.R). Following the page leaves a declared category that nothing
  # raises, which the suite then fails as a dead row -- from the wrong file.
  d <- readLines(file.path(.dt_root(), "docs", "design.md"), warn = FALSE)
  s <- grep("^### To add a check", d)
  expect_gt(length(s), 0L)
  nxt <- grep("^#", d); nxt <- nxt[nxt > s[1]]
  sec <- paste(d[s[1]:(if (length(nxt)) nxt[1] - 1L else length(d))], collapse = " ")

  syms <- c("CHECK_PLAIN", ".DIAG_FIX_OWNER", "DIAG_PLAIN", ".KPI_DIAGNOSIS",
            "INFORMATIONAL_CHECKS")
  home <- vapply(syms, .dt_defines, character(1))
  expect_identical(unname(syms[is.na(home)]), character(0))   # they all still exist

  # Every symbol's real file has to be named somewhere in the recipe...
  for (s2 in syms)
    expect_true(grepl(home[[s2]], sec, fixed = TRUE),
                info = paste("the recipe names", s2, "but never", home[[s2]]))

  # ...and no two symbols from DIFFERENT files may be joined by "the same file".
  occ <- do.call(rbind, lapply(syms, function(x) {
    g <- gregexpr(x, sec, fixed = TRUE)[[1]]
    if (g[1] == -1L) return(NULL)
    data.frame(sym = x, at = as.integer(g), len = nchar(x), stringsAsFactors = FALSE)
  }))
  expect_gt(nrow(occ), 4L)
  occ <- occ[order(occ$at), , drop = FALSE]
  bad <- character(0)
  for (i in seq_len(nrow(occ) - 1L)) {
    a <- occ[i, ]; b <- occ[i + 1L, ]
    if (identical(home[[a$sym]], home[[b$sym]])) next
    if (grepl("same file", substr(sec, a$at + a$len, b$at - 1L), fixed = TRUE))
      bad <- c(bad, sprintf("%s (%s) and %s (%s) are called the same file",
                            a$sym, home[[a$sym]], b$sym, home[[b$sym]]))
  }
  expect_identical(bad, character(0), info = paste(bad, collapse = "; "))
})

# ------------------------------------------- what the analyst pages count -----

test_that("the info rows the page counts are the ones the engine can raise", {
  # "Six kinds ... see the five kinds above" -- six was right, the back-reference
  # was not, and a reader who trusts the smaller number goes looking for a bullet
  # that is not missing. Counted from R/diagnose.R, not from the page: the
  # severity is the third argument to add(), and the trailing comma is what keeps
  # the severity-ordering vector c("high", "medium", "info") out of the count.
  eng <- readLines(file.path(.dt_root(), "R", "diagnose.R"), warn = FALSE)
  cats <- unlist(regmatches(eng, gregexpr('"[a-z_]+",\\s*"info",', eng)))
  cats <- unique(sub('".*$', "", sub('^"', "", cats)))
  n <- length(cats)
  expect_gt(n, 3L)
  word <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
            "nine", "ten")[n]
  expect_false(is.na(word))

  f <- file.path(.dt_root(), "docs", "operational", "when-something-goes-wrong.md")
  ln <- readLines(f, warn = FALSE)
  txt <- paste(ln, collapse = " ")
  expect_true(grepl(paste(word, "kinds of row are"), txt, ignore.case = TRUE),
              info = paste("the page does not say there are", word, "info kinds"))
  expect_true(grepl(paste("see the", word, "kinds"), txt, ignore.case = TRUE),
              info = paste("the back-reference does not say", word))
  # ...and it really lists that many, so the words and the bullets cannot drift.
  a <- grep("kinds of row are \\*info\\* severity", ln)
  b <- grep("^So read the sentence", ln)
  expect_gt(length(a), 0L); expect_gt(length(b), 0L)
  b <- b[b > a[1]][1]
  expect_identical(sum(grepl("^- \\*", ln[a[1]:b])), n)
})

test_that("the published suite baseline is one this tree could have produced", {
  # A floor to compare against decays in silence: the runbook told a maintainer
  # that a LOWER total than the recorded one means something did not run, while
  # recording 855 tests against a tree that runs more than that.
  #
  # Only ONE of the three figures can be checked from inside the suite -- the
  # file count -- because tests and assertions are known only by running, and a
  # suite cannot run itself. Static counts are no substitute: 885 test_that
  # blocks are written down and 864 execute. So this pins what can be pinned,
  # requires the date that makes the rest readable as a measurement, and refuses
  # the retired numbers by name so they cannot come back.
  f <- file.path(.dt_root(), "docs", "operational", "maintaining-the-engine.md")
  txt <- paste(readLines(f, warn = FALSE), collapse = " ")
  m <- regmatches(txt, regexpr("[0-9,]+ files, [0-9,]+ tests, [0-9,]+ passing", txt))
  expect_length(m, 1L)
  # "[0-9][0-9,]*", not "[0-9,]+": the latter matches the SEPARATOR commas too and
  # turns "71 files, 855 tests" into 71, NA, 855.
  nums <- as.integer(gsub(",", "", unlist(regmatches(m, gregexpr("[0-9][0-9,]*", m)))))
  expect_length(nums, 3L)
  files <- length(list.files(file.path(.dt_root(), "tests", "testthat"),
                             "^test-.*\\.R$"))
  expect_identical(nums[1], files)          # exact, and the one that grows quietly
  expect_gte(nums[2], nums[1])              # at least one test per file
  expect_gte(nums[3], nums[2])              # at least one assertion per test
  # Dated, and not dated in the future.
  dt <- regmatches(txt, gregexpr("20[0-9]{2}-[0-9]{2}-[0-9]{2}", txt))[[1]]
  expect_gt(length(dt), 0L)
  expect_true(all(as.Date(dt) <= Sys.Date()))
  # Retired figures, by name (the idiom test-deployment-docs.R already uses).
  for (stale in c("855 tests", "4,213", "725 tests"))
    expect_false(grepl(stale, txt, fixed = TRUE), info = paste("stale figure:", stale))
})

test_that("the incident procedure says which release made engine_version a build id", {
  # Its headline test is "same engine_version and same template_sha256, so any
  # difference is a finding". True from 1.4.0 on, and NOT true of the records
  # already on the box: the stamp only moved at release while behaviour changed
  # inside one, so pairs exist with one version, one template hash and opposite
  # verdicts. A maintainer applying the rule to those hunts a defect that is
  # really a fix. The page has to name the release the rule holds from, and that
  # release has to be one the changelog actually has.
  f <- file.path(.dt_root(), "docs", "operational", "investigating-a-wrong-conversion.md")
  ln <- readLines(f, warn = FALSE)
  a <- grep("^## 4\\.", ln); b <- grep("^## 5\\.", ln)
  expect_gt(length(a), 0L); expect_gt(length(b), 0L)
  sec <- paste(ln[a[1]:(b[1] - 1L)], collapse = " ")
  expect_true(grepl("any difference is a finding", sec, fixed = TRUE))
  vs <- unique(unlist(regmatches(sec, gregexpr("[0-9]+\\.[0-9]+\\.[0-9]+", sec))))
  expect_gt(length(vs), 0L)
  heads <- trimws(sub("^## ", "",
                      grep("^## ", readLines(file.path(.dt_root(), "CHANGELOG.md"),
                                             warn = FALSE), value = TRUE)))
  expect_identical(setdiff(vs, heads), character(0),
                   info = paste("names a version the changelog does not have:",
                                paste(setdiff(vs, heads), collapse = ", ")))
})
