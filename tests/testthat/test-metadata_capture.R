# Local metadata capture (R/metadata_capture.R): the on-box "ML goldmine".
# It must be rich, level-gated, PII-safe (no raw content), and LOCAL ONLY -- never
# in the Qlik feed. One file per run under logs/metadata/, kept forever.

.mc_ctx <- function(status = "ok") {
  inp <- read_input(fixture("samples/raw/anz/anz_creditcard_01.csv"))
  tp  <- load_templates(templates_dir())
  tmpl <- tp[["anz_creditcard_csv"]]
  parsed <- parse_statement(inp, tmpl)
  recon  <- reconcile(parsed, tmpl)
  det    <- detect_statement(inp, tp)
  list(run_id = "run-1", ts = "2026-01-01T00:00:00Z", requested_by = "u",
       sha = "deadbeef", input = inp, parsed = parsed, recon = recon, det = det,
       meta = extract_metadata(inp), template = tmpl, status = status,
       elapsed_ms = 12)
}

test_that("full capture is rich and structured", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  rec <- capture_metadata(.mc_ctx(), .config_defaults())
  expect_identical(rec$level, "full")
  expect_true(!is.null(rec$layout$signature))
  expect_true(!is.null(rec$detection$candidate_scores))       # full-only detail
  expect_true(!is.null(rec$parse_quality$field_fill))
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
  # a bank that writes cow/horse for its D/C indicator, template only knows D/C.
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
  cfg <- .config_defaults(); cfg$metadata$capture$detection <- FALSE
  expect_null(capture_metadata(.mc_ctx(), cfg)$detection)
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
  ld <- tempfile("logs_"); out <- tempfile("out_")
  res <- convert_statement(fixture("samples/raw/anz/anz_creditcard_01.csv"),
    outdir = out, templates_dir = templates_dir(),
    user_templates_dir = tempfile("u_"), logdir = ld)
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
    list(layout = TRUE, parse_quality = FALSE, detection = TRUE,
         reconciliation = TRUE, ocr = TRUE, redaction = TRUE), p)
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

test_that("route_metadata carries each route's own numbers, and no content", {
  tr <- list(kind = "tables", status = "ok", template_id = "northwind_position",
             template_version = 3, template_sha256 = "abc", n_tables = 6L,
             n_rows = 102L, unclaimed_words = 0L,
             empty_tables = character(0), thin_tables = character(0),
             weak_tables = "fees_charged", absent_tables = character(0),
             parse_fail_tables = character(0))
  m <- route_metadata(tr)
  expect_identical(m$kind, "tables")
  expect_identical(m$status, "ok")
  expect_identical(m$template_id, "northwind_position")
  expect_identical(m$template_origin, "document")
  expect_equal(m$report$n_tables, 6L)
  expect_equal(m$report$n_table_rows, 102L)
  expect_equal(m$report$weak_tables, 1L)           # a COUNT, never the name
  expect_false(grepl("fees_charged", paste(unlist(m), collapse = " "), fixed = TRUE))

  fm <- route_metadata(list(kind = "form", status = "needs_review",
                            template_id = "anz_kiwisaver_fields", n_fields = 9L,
                            n_values = 7L, n_conflicts = 3L, required_missing = 0L))
  expect_identical(fm$template_origin, "fields")
  expect_equal(fm$form$n_conflicts, 3L)
  # a statement result has nothing to correct: the record already describes it
  expect_equal(length(route_metadata(list(kind = "statement", status = "ok"))), 0L)
})

test_that("amending corrects this run's record in place, and keeps the attempt", {
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  ld <- tempfile("mc_"); dir.create(file.path(ld, "metadata"), recursive = TRUE)
  on.exit(unlink(ld, recursive = TRUE), add = TRUE)
  ctx <- .mc_ctx("unsupported")
  ctx$template <- NULL                             # the statement pass matched nothing
  write_metadata_record(ld, "run-1", capture_metadata(ctx, .config_defaults()))
  expect_equal(length(list.files(file.path(ld, "metadata"))), 1L)

  res <- list(run_id = "run-1", kind = "tables", status = "ok",
              template_id = "northwind_position", template_version = 2,
              template_sha256 = "abc", n_tables = 6L, n_rows = 102L,
              unclaimed_words = 0L)
  amend_metadata_record(ld, res)

  # ONE record per run, still. A second file would leave the forever corpus
  # holding two answers for one conversion -- the fault, doubled.
  files <- list.files(file.path(ld, "metadata"), full.names = TRUE)
  expect_equal(length(files), 1L)
  rec <- jsonlite::fromJSON(paste(readLines(files[1]), collapse = "\n"))
  expect_identical(rec$kind, "tables")
  expect_identical(rec$status, "ok")
  expect_identical(rec$template_id, "northwind_position")
  expect_equal(rec$report$n_table_rows, 102L)
  expect_identical(rec$run_id, "run-1")            # same run, same record
  # THE STATEMENT ATTEMPT IS NOT DELETED, it is labelled: the pipeline really did
  # find no match and that is a signal worth keeping -- what it may not do is
  # masquerade as the verdict on the run.
  expect_identical(rec$statement_attempt$status, "unsupported")
  expect_true(is.null(rec$statement_attempt$template_id))
})

test_that("amending is a no-op when there is nothing to correct", {
  ld <- tempfile("mc_"); dir.create(file.path(ld, "metadata"), recursive = TRUE)
  on.exit(unlink(ld, recursive = TRUE), add = TRUE)
  # a statement run: the record already describes it correctly
  expect_null(amend_metadata_record(ld, list(run_id = "r", kind = "statement")))
  # capture level `off`, so there is no record to amend and no error either
  expect_null(amend_metadata_record(ld, list(run_id = "nope", kind = "tables",
                                             status = "ok", n_tables = 1L)))
  expect_equal(length(list.files(file.path(ld, "metadata"))), 0L)
})

test_that("a clashing run id amends the record that run wrote, not the earlier one", {
  # write_log_record NEVER overwrites: two conversions inside one second share a
  # run id and the second is filed as "<id>~2.json". Correcting one run's record
  # with another run's answer would be worse than the fault being fixed.
  skip_if_not(file.exists(fixture("samples/raw/anz/anz_creditcard_01.csv")))
  ld <- tempfile("mc_"); dir.create(file.path(ld, "metadata"), recursive = TRUE)
  on.exit(unlink(ld, recursive = TRUE), add = TRUE)
  first <- capture_metadata(.mc_ctx("ok"), .config_defaults())
  write_metadata_record(ld, "run-1", first)
  Sys.setFileTime(file.path(ld, "metadata", "run-1.json"), Sys.time() - 60)
  write_metadata_record(ld, "run-1", capture_metadata(.mc_ctx("unsupported"),
                                                      .config_defaults()))
  files <- sort(list.files(file.path(ld, "metadata")))
  expect_identical(files, c("run-1.json", "run-1~2.json"))

  amend_metadata_record(ld, list(run_id = "run-1", kind = "tables", status = "ok",
                                 template_id = "d1", n_tables = 2L, n_rows = 9L))
  a <- jsonlite::fromJSON(paste(readLines(file.path(ld, "metadata", "run-1.json")),
                                collapse = "\n"))
  b <- jsonlite::fromJSON(paste(readLines(file.path(ld, "metadata", "run-1~2.json")),
                                collapse = "\n"))
  expect_identical(a$kind, "statement")            # the earlier run, untouched
  expect_identical(b$kind, "tables")               # the one that just wrote
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
