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
