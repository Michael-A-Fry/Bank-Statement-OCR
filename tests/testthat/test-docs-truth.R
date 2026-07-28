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
