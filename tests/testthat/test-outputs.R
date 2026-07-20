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
