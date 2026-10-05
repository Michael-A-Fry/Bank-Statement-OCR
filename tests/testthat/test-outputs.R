# Output reproducibility (build-contract 11.4). Same input + reading must yield
# byte-identical artifacts across runs -- including the xlsx, whose core
# properties otherwise embed a wall-clock timestamp.

# The table reader's own template for the BNZ export (test material only).
.out_tmpl <- function() yaml::read_yaml(fixture("tests/testthat/fixtures/templates/bnz_everyday_csv.yaml"))

test_that("xlsx / csv / json are byte-reproducible across runs", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  tmpl <- .out_tmpl()
  parsed <- parse_statement(input, tmpl)
  recon <- reconcile(parsed, tmpl)

  d1 <- file.path(tempdir(), "rep1"); d2 <- file.path(tempdir(), "rep2")
  unlink(c(d1, d2), recursive = TRUE)
  write_outputs(parsed, recon, d1, "bnz")
  Sys.sleep(1.1)  # force a different wall clock between runs
  write_outputs(parsed, recon, d2, "bnz")

  for (ext in c("xlsx", "csv", "json")) {
    m1 <- tools::md5sum(file.path(d1, paste0("bnz.", ext)))
    m2 <- tools::md5sum(file.path(d2, paste0("bnz.", ext)))
    expect_equivalent(m1, m2, info = sprintf("%s not reproducible", ext))
  }
})

# ---------------------------------------------------------------------------
# F1-45: determinism has to be FALSIFIABLE. Nothing recorded which build produced
# a figure, so "same input + same learned state = same answer" could never be
# checked after the fact.
# ---------------------------------------------------------------------------

test_that("the JSON output carries the stamp it is given", {
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  tmpl <- .out_tmpl()
  parsed <- parse_statement(input, tmpl)
  recon <- reconcile(parsed, tmpl)

  d <- tempfile("bstamp"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- write_outputs(parsed, recon, d, "bnz", formats = "json",
    build = list(engine_version = engine_version(), layouts_state = "0123456789ab",
                 layout = "bnz_1@2", outcome = "proven", proof_kind = "chain"))
  js <- jsonlite::fromJSON(paste(readLines(p[["json"]]), collapse = "\n"))
  expect_identical(js$build$engine_version, engine_version())
  expect_identical(js$build$layout, "bnz_1@2")
  expect_identical(js$build$layouts_state, "0123456789ab")
})

test_that("a JSON written with no build block still names the engine version", {
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  tmpl <- .out_tmpl()
  parsed <- parse_statement(input, tmpl); recon <- reconcile(parsed, tmpl)
  d <- tempfile("bstamp2"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- write_outputs(parsed, recon, d, "bnz", formats = "json")
  js <- jsonlite::fromJSON(paste(readLines(p[["json"]]), collapse = "\n"))
  expect_identical(js$build$engine_version, engine_version())
})

test_that("engine_version reads the repo VERSION file (and never errors without one)", {
  expect_true(file.exists(file.path(engine_root(), "VERSION")))
  expect_true(nzchar(engine_version()))
  expect_false(is.na(engine_version()))
  # the run record carries it too, so a figure traces back to a build.
  fx <- fixture("samples/raw/bnz/bnz_transaction_export_01.csv")
  skip_if_not(file.exists(fx))
  od <- tempfile("vo_"); ld <- tempfile("vl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  res <- convert_statement(fx, outdir = od, logdir = ld, layouts_dir = tempfile("vly_"), tracking_dir = NA)
  rec <- jsonlite::fromJSON(paste(readLines(file.path(ld, "runs",
    paste0(res$run_id, ".json"))), collapse = "\n"))
  expect_identical(rec$engine_version, engine_version())
  expect_identical(rec$layouts_state, "empty")
})

test_that("every row of the files carries its own statement's account number and name", {
  tx <- data.frame(row_id = 1:3, date = "2026-02-03", description = c("A", "B", "C"), amount = c(-1, 2, -3),
                   statement_index = c(1L, 1L, 2L), stringsAsFactors = FALSE)
  parsed <- list(transactions = tx, header = list(account_number = NA_character_),
                 statements = list(list(account_number = "01-2345-0678901-00", account_name = "A HOLDER"),
                                   list(account_number = "12-3456-1234567-00", account_name = NA_character_)))
  out <- .with_accounts(tx, parsed)
  expect_identical(names(out)[1:3], c("row_id", "account_number", "account_name"))
  expect_identical(out$account_number, c("01-2345-0678901-00", "01-2345-0678901-00", "12-3456-1234567-00"))
  expect_identical(out$account_name, c("A HOLDER", "A HOLDER", NA))
  # one statement: the header's
  one <- .with_accounts(tx[, 1:4], list(transactions = tx[, 1:4], header = list(account_number = "03-4567-0890123-00")))
  expect_identical(unique(one$account_number), "03-4567-0890123-00")
})
