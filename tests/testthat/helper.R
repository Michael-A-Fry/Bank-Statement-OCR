# helper.R -- shared test helpers.
#
# Statement templates are retired from the product (auto-reading-spec section 10):
# the automatic reader is the only way a statement is read. The shipped templates
# live on as TEST MATERIAL under fixtures/templates/, because a fixed set of boxes
# is still the sharpest way to test the table reader (parse_pdf_table,
# parse_statement, reconcile) that the automatic reader assembles its rows with.
#
# Two kinds of golden test, one golden CSV each (tests/testthat/expected/):
#   expect_statement_ok(fixture, expected, template_id)  the table reader, with the
#     fixture template, reproduces the golden byte for byte;
#   expect_auto_read_golden(fixture, expected, outcomes)  the automatic reader, given
#     no template, reads the same statement to the same figures.

engine_root <- function() {
  r <- Sys.getenv("ENGINE_ROOT", "")
  if (nzchar(r)) return(r)
  # Fallback: two levels up from this helper (tests/testthat -> repo root).
  normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), "..", ".."))
}

fixture <- function(rel) file.path(engine_root(), rel)

# WHERE THE FIXTURE TEMPLATES ARE, in one place, so the next move touches one line.
fixture_templates_dir <- function() file.path(engine_root(), "tests", "testthat", "fixtures", "templates")

# fixture_templates() -> every fixture template, named by id. The engine no longer
# loads templates, so this is the whole loader: the YAML as written.
fixture_templates <- function() {
  fs <- sort(list.files(fixture_templates_dir(), pattern = "\\.ya?ml$", full.names = TRUE))
  tp <- lapply(fs, yaml::read_yaml)
  stats::setNames(tp, vapply(tp, function(t) t$id, ""))
}

fixture_template <- function(id) {
  t <- fixture_templates()[[id]]
  testthat::expect_false(is.null(t), info = paste("no fixture template:", id))
  t
}

# read_core_csv -- read a golden/core CSV back with the exact core column types
# so comparisons are type-stable.
read_core_csv <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        na.strings = "", check.names = FALSE)
  coerce_core(df)
}

# parse_fixture(fixture_rel, template_id) -- parse a fixture with a fixture template.
# The fixture is parsed ONCE and the same parsed object is reconciled: parsing is
# deterministic, so a second parse only cost time -- and it meant `recon` was
# computed from a different object than the one the test then asserts on.
parse_fixture <- function(fixture_rel, template_id) {
  template <- fixture_template(template_id)
  input <- read_input(fixture(fixture_rel))
  parsed <- parse_statement(input, template)
  list(template = template, input = input, parsed = parsed,
       recon = reconcile(parsed, template))
}

# expect_statement_ok -- the table reader, with the fixture template, against the
# golden CSV snapshot.
expect_statement_ok <- function(fixture_path, expected_csv_path, template_id) {
  res <- parse_fixture(fixture_path, template_id)
  got <- coerce_core(res$parsed$transactions)
  exp <- read_core_csv(fixture(expected_csv_path))
  testthat::expect_equal(got, exp)
  invisible(res)
}

# expect_auto_read_golden(fixture_rel, expected_rel, outcomes, fields) -- the
# automatic reader, given no template and nothing learned, reads the statement to
# the golden's figures. `outcomes` is what the reader may decide: a statement
# nothing on it proves goes to a person ("check"), and that is right, but its
# figures must still be the statement's. Whatever `outcomes` allows, an automatic
# outcome (proven / layout_match) with any figure off the golden fails: that is
# the silently wrong answer the product must never give.
AUTO_OUTCOMES <- c("proven", "layout_match")
expect_auto_read_golden <- function(fixture_rel, expected_rel, outcomes,
                                    fields = c("date", "amount", "direction", "balance", "description"),
                                    layouts = list(), bank = NULL) {
  rd <- auto_read(read_input(fixture(fixture_rel)), layouts = layouts, bank = bank)
  testthat::expect_true(rd$outcome %in% outcomes,
    info = sprintf("%s read as %s: %s", fixture_rel, rd$outcome, rd$why))
  exp <- read_core_csv(fixture(expected_rel))
  got <- coerce_core(rd$transactions)
  testthat::expect_equal(nrow(got), nrow(exp), info = fixture_rel)
  if (nrow(got) == nrow(exp)) {
    for (f in fields) testthat::expect_equal(got[[f]], exp[[f]], info = paste(fixture_rel, f))
  }
  invisible(rd)
}

# .src_block(src, pat, n) -- the block of app.R that starts at the first line
# matching `pat`, as one string.
#
# THE WINDOW USED TO BE A LINE COUNT, and a line count is a promise the code
# keeps breaking. Add a paragraph of comment inside an observer and the window
# slides off the end of what it was checking: the lucky half of those tests go
# red for no reason, and the unlucky half go GREEN FOR THE WRONG REASON -- the
# pattern they were pinning is simply no longer inside the text they read. That
# is the worse failure, because nothing announces it.
#
# So it counts braces instead, and ends where the block ends. `n` survives as a
# FLOOR, not a ceiling, for the call sites whose pattern is a plain line rather
# than the head of a block: the result is at least the n lines it always was, so
# no assertion that passes today can start failing because the window shrank.
#
# Strings and comments are stripped before counting, or a brace inside a message
# would close a block early.
.src_block <- function(src, pat, n = 40L) {
  i <- grep(pat, src)
  testthat::expect_true(length(i) >= 1, info = paste("not found:", pat))
  i <- i[1]
  bare <- gsub("#.*$", "", gsub('"[^"]*"', "", gsub("'[^']*'", "", src)))
  depth <- 0L; end <- NA_integer_
  for (k in seq(i, length(src))) {
    depth <- depth +
      nchar(gsub("[^{(]", "", bare[k])) - nchar(gsub("[^})]", "", bare[k]))
    if (k > i && depth <= 0L) { end <- k; break }
  }
  if (is.na(end)) end <- length(src)
  paste(src[i:min(length(src), max(end, i + n - 1L))], collapse = " ")
}
