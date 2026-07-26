# Output reproducibility (build-contract 11.4). Same input + template must yield
# byte-identical artifacts across runs -- including the xlsx, whose core
# properties otherwise embed a wall-clock timestamp.

test_that("xlsx / csv / json are byte-reproducible across runs", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  tmpl <- templates[["bnz_everyday_csv"]]
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
# a figure, so "same input + same template = same answer" could never be checked
# after the fact.
# ---------------------------------------------------------------------------

test_that("the JSON output stamps the engine build and the template content hash", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  tmpl <- templates[["bnz_everyday_csv"]]
  parsed <- parse_statement(input, tmpl)
  recon <- reconcile(parsed, tmpl)

  d <- tempfile("bstamp"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- write_outputs(parsed, recon, d, "bnz", formats = "json",
    build = list(engine_version = engine_version(), template_id = tmpl$id,
                 template_version = as.character(tmpl$version),
                 template_sha256 = template_sha256(tmpl)))
  js <- jsonlite::fromJSON(paste(readLines(p[["json"]]), collapse = "\n"))
  expect_identical(js$build$engine_version, engine_version())
  expect_identical(js$build$template_id, "bnz_everyday_csv")
  expect_identical(js$build$template_sha256, template_sha256(tmpl))
})

test_that("a JSON written with no build block still names the engine version", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  tmpl <- templates[["bnz_everyday_csv"]]
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
  res <- convert_statement(fx, outdir = od, templates_dir = templates_dir(), logdir = ld)
  rec <- jsonlite::fromJSON(paste(readLines(file.path(ld, "runs",
    paste0(res$run_id, ".json"))), collapse = "\n"))
  expect_identical(rec$engine_version, engine_version())
  expect_identical(rec$template_sha256,
                   template_sha256(load_templates(templates_dir())[["bnz_everyday_csv"]]))
})
