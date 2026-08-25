# helper.R -- shared test helpers.
#
# expect_statement_ok(fixture_path, expected_csv_path, template_id, bank)
# parses a fixture through the engine and compares the core Transactions table
# to a golden CSV snapshot stored under tests/testthat/expected/.
#
# Pattern for adding a new template test (see tests/HOWTO-add-template-test.md):
#   1. Add templates/statements/<id>.yaml and a fixture under samples/raw/<bank>/.
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

# WHERE THE TEMPLATES ARE, in one place. Every template lives under templates/,
# one folder per kind (see templates/README.md). Tests ask these functions rather
# than spelling the path out, so the next time the layout moves it moves here and
# nowhere else -- which is the whole reason the last move touched forty files.
.templates_root <- function() file.path(engine_root(), "templates")
templates_dir      <- function() file.path(.templates_root(), "statements")
user_templates_dir <- function() file.path(.templates_root(), "statements_user")
seed_templates_dir <- function() file.path(.templates_root(), "statements_seed")
fields_templates_dir   <- function() file.path(.templates_root(), "fields")
document_templates_dir <- function() file.path(.templates_root(), "documents")

# read_core_csv -- read a golden/core CSV back with the exact core column types
# so comparisons are type-stable.
read_core_csv <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        na.strings = "", check.names = FALSE)
  coerce_core(df)
}

# parse_fixture -- detect + parse a fixture, returning the parsed object.
# The fixture is parsed ONCE and the same parsed object is reconciled: parsing is
# deterministic, so a second parse only cost time -- and it meant `recon` was
# computed from a different object than the one the test then asserts on.
parse_fixture <- function(fixture_rel, bank = NULL, statement_type = NULL) {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture(fixture_rel))
  det <- detect_statement(input, templates, hint_bank = bank,
                          hint_type = statement_type)
  testthat::expect_true(det$matched,
    info = sprintf("detection failed for %s: %s", fixture_rel, det$detail))
  template <- templates[[det$template_id]]
  parsed <- parse_statement(input, template)
  list(detection = det, template = template,
       parsed = parsed,
       recon = reconcile(parsed, template))
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
