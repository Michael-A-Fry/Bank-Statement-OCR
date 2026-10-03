# Local metadata capture (R/metadata_capture.R): the on-box "ML goldmine".
# It must be rich, level-gated, PII-safe (no raw content), and LOCAL ONLY -- never
# in the Qlik feed. One file per run under logs/metadata/, kept forever.

# .mc_ctx() -- what convert_statement hands the capture: the automatic reader's
# reading of the file (its template, parse and reconciliation).
.mc_ctx <- function(status = "ok") {
  inp <- read_input(fixture("samples/raw/anz/anz_creditcard_01.csv"))
  rd <- auto_read(inp)
  list(run_id = "run-1", ts = "2026-01-01T00:00:00Z", requested_by = "u",
       sha = "deadbeef", input = inp, parsed = rd$parsed, recon = rd$recon,
       meta = extract_metadata(inp), template = rd$template, status = status,
       elapsed_ms = 12)
}

test_that("full capture is rich and structured", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  rec <- capture_metadata(.mc_ctx(), .config_defaults())
  expect_identical(rec$level, "full")
  expect_true(!is.null(rec$layout$signature))
  expect_true(!is.null(rec$parse_quality$field_fill))         # full-only detail
  expect_true(!is.null(rec$reconciliation$kpis))
  expect_equal(rec$parse_quality$row_count, nrow(.mc_ctx()$parsed$transactions))
})

test_that("levels gate the depth; off captures nothing", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  ctx <- .mc_ctx()
  off <- .config_defaults(); off$metadata$level <- "off"
  expect_null(capture_metadata(ctx, off))
  std <- .config_defaults(); std$metadata$level <- "standard"
  r <- capture_metadata(ctx, std)
  expect_null(r$parse_quality$field_fill)                     # detail is full-only
  expect_false(is.null(r$reconciliation$kpi_fail_count))      # standard summarises
  expect_null(r$reconciliation$kpis)
})

test_that("full capture records multi-statement counts and the novelty gaps", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  ctx <- .mc_ctx()
  # a bank that writes cow/horse for its D/C indicator; the reading only knows D/C.
  ctx$parsed$transactions$type[1:2] <- "cow"
  ctx$parsed$transactions$type[3]   <- "horse"
  rec <- capture_metadata(ctx, .config_defaults())
  # multi-statement / periods / accounts are present.
  expect_false(is.null(rec$multi_statement))
  expect_true(!is.null(rec$multi_statement$n_periods))
  expect_true(!is.null(rec$multi_statement$n_accounts))
  # the "we missed it" signals: an unmapped source column and unrecognised tokens.
  expect_true("ConversionCharge" %in% unlist(rec$novelty$source_headers))
  expect_true("ConversionCharge" %in% unlist(rec$novelty$unmapped_columns))
  expect_setequal(toupper(unlist(rec$novelty$unrecognised_type_values)), c("COW", "HORSE"))
  # value SHAPES (not values) captured.
  expect_true(!is.null(rec$parse_quality$amount_buckets))
  expect_true(!is.null(rec$parse_quality$desc_len))
  expect_true(!is.null(rec$parse_quality$unparsed_dates))
})

test_that("a switched-off category is dropped", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  cfg <- .config_defaults(); cfg$metadata$capture$parse_quality <- FALSE
  rec <- capture_metadata(.mc_ctx(), cfg)
  expect_null(rec$parse_quality)
  expect_false(is.null(rec$reconciliation))                   # only that one goes
})

test_that("capture is PII-safe: no raw content, account number only hashed", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  # a record whose account number is present must store a HASH, never the number.
  ctx <- .mc_ctx()
  ctx$parsed$header$account_number <- "12-3456-7890123-00"
  rec <- capture_metadata(ctx, .config_defaults())
  blob <- jsonlite::toJSON(rec, auto_unbox = TRUE, na = "null")
  expect_false(grepl("12-3456-7890123-00", blob))             # raw account never present
  expect_true(nzchar(rec$account_hash) && !is.na(rec$account_hash))
  # no verbatim description/payee text leaks into the record.
  descs <- ctx$parsed$transactions$description
  descs <- descs[!is.na(descs) & nzchar(descs)]
  for (d in utils::head(descs, 5)) expect_false(grepl(d, blob, fixed = TRUE))
})

test_that("convert_statement writes a metadata file, and never into the feed", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  cv <- convert_sandbox()
  res <- cv(fixture("samples/raw/anz/anz_creditcard_01.csv"))
  ld <- file.path(sandbox_dir(cv), "logs")
  mf <- list.files(file.path(ld, "metadata"), full.names = TRUE)
  expect_length(mf, 1)                                        # one file per run
  rec <- jsonlite::fromJSON(paste(readLines(mf[1]), collapse = "\n"))
  expect_identical(rec$run_id, res$run_id)
  expect_true(!is.null(rec$layout))
  # the transient capture field must NOT leak onto the returned result.
  expect_null(res$metadata_capture)
})

test_that("save_metadata_config round-trips only the metadata block", {
  p <- tempfile(fileext = ".yaml")
  writeLines(c("app:", "  title: Keep Me"), p)                # pre-existing content
  ok <- save_metadata_config("standard",
    list(layout = TRUE, parse_quality = FALSE, reconciliation = TRUE,
         multi_statement = TRUE, novelty = TRUE, ocr = TRUE), p)
  expect_true(ok)
  y <- yaml::read_yaml(p)
  expect_identical(y$app$title, "Keep Me")                   # other config untouched
  expect_identical(y$metadata$level, "standard")
  expect_false(isTRUE(y$metadata$capture$parse_quality))
  expect_true(isTRUE(y$metadata$retain_forever))
})

# ---------------------------------------------------------------------------
# L7. A FORM OR A REPORT WAS FILED HERE AS AN UNSUPPORTED STATEMENT.
#
# convert_document() tries the statement pipeline first and only then the form
# and report ones -- and this record is written by the statement pass, before
# either of the others has run. So a report that converted perfectly left a
# record describing the abandoned attempt: status "unsupported", template_id
# null, no kind at all. Measured against the seven-page document fixture, where
# the run log for the SAME run correctly said kind tables, status ok.
#
# This folder is kept FOREVER, is exempt from the rollup, and is exactly what
# R/suggestions.R mines for "build these next" -- so the corpus could not tell a
# genuinely unsupported layout, worth a template, from a report that read a
# hundred rows, and would recommend building what already exists.
# ---------------------------------------------------------------------------

test_that("every record says which route it describes", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  rec <- capture_metadata(.mc_ctx(), .config_defaults())
  expect_identical(rec$kind, "statement")          # the default, and true here
  ctx <- .mc_ctx(); ctx$kind <- "tables"
  expect_identical(capture_metadata(ctx, .config_defaults())$kind, "tables")
})

test_that("no name printed on a page ever reaches a record kept forever", {
  # G9. layout_signature()'s fallback used to be the twelve commonest long words
  # on the page, and on a document with no transaction header -- exactly the class
  # that reaches the form and report routes -- those are the people named on it.
  # Rule 2 at the top of this module says names are never stored, and that
  # promise is kept here whatever another module's fallback does.
  expect_identical(.layout_hint_safe("ambrose | whitcombe", "pdf"), "")
  expect_identical(.layout_hint_safe("amount | balance | date", "pdf"),
                   "amount | balance | date")
  expect_identical(.layout_hint_safe("*date | payee | memo", "delimited"),
                   "*date | payee | memo")

  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  ctx <- .mc_ctx()
  ctx$input$kind <- "pdf"     # the branch with the frequency fallback behind it
  ctx$layout_sig <- list(signature = "abc123", hint = "ambrose | whitcombe | amount")
  rec <- capture_metadata(ctx, .config_defaults())
  expect_identical(rec$layout$signature, "abc123")   # clustering is untouched
  expect_false(grepl("ambrose", rec$layout$hint %||% "", fixed = TRUE))
})
