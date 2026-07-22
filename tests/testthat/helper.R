# helper.R -- shared test helpers.
#
# expect_statement_ok(fixture_path, expected_csv_path, template_id, bank)
# parses a fixture through the engine and compares the core Transactions table
# to a golden CSV snapshot stored under tests/testthat/expected/.
#
# Pattern for adding a new template test (see tests/HOWTO-add-template-test.md):
#   1. Add templates/<id>.yaml and a fixture under samples/raw/<bank>/.
#   2. Generate the golden CSV from the engine's own parse, eyeball it.
#   3. Save it to tests/testthat/expected/<id>.csv.
#   4. test-<id>.R: expect_statement_ok("<fixture>", "<expected>", "<id>", "<BANK>").

engine_root <- function() {
  r <- Sys.getenv("ENGINE_ROOT", "")
  if (nzchar(r)) return(r)
  # Fallback: two levels up from this helper (tests/testthat -> repo root).
  normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), "..", ".."))
}

fixture <- function(rel) file.path(engine_root(), rel)

templates_dir <- function() file.path(engine_root(), "templates")

# read_core_csv -- read a golden/core CSV back with the exact core column types
# so comparisons are type-stable.
read_core_csv <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        na.strings = "", check.names = FALSE)
  coerce_core(df)
}

# parse_fixture -- detect + parse a fixture, returning the parsed object.
parse_fixture <- function(fixture_rel, bank = NULL, statement_type = NULL) {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture(fixture_rel))
  det <- detect_statement(input, templates, hint_bank = bank,
                          hint_type = statement_type)
  testthat::expect_true(det$matched,
    info = sprintf("detection failed for %s: %s", fixture_rel, det$detail))
  template <- templates[[det$template_id]]
  list(detection = det, template = template,
       parsed = parse_statement(input, template),
       recon = reconcile(parse_statement(input, template), template))
}

# expect_statement_ok -- core comparison against a golden CSV snapshot.
expect_statement_ok <- function(fixture_path, expected_csv_path,
                                template_id = NULL, bank = NULL,
                                statement_type = NULL) {
  res <- parse_fixture(fixture_path, bank = bank, statement_type = statement_type)
  if (!is.null(template_id)) {
    testthat::expect_identical(res$detection$template_id, template_id)
  }
  got <- coerce_core(res$parsed$transactions)
  exp <- read_core_csv(fixture(expected_csv_path))
  testthat::expect_equal(got, exp)
  invisible(res)
}
