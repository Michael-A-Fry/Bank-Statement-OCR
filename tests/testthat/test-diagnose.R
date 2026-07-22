# Tests for fail-loud diagnostics (R/diagnose.R) + its wiring into convert/outputs.

.mk_tx <- function(n = 1L, ...) {
  base <- data.frame(
    row_id = seq_len(n), date = rep("2025-01-01", n), date_raw = rep("1/1/25", n),
    description = rep("a", n), amount = rep(-5, n), amount_raw = rep("-5", n),
    direction = rep("debit", n), balance = rep(NA_real_, n),
    balance_raw = rep(NA_character_, n), particulars = rep(NA_character_, n),
    code = rep(NA_character_, n), reference = rep(NA_character_, n),
    other_party = rep(NA_character_, n), type = rep(NA_character_, n),
    currency = rep("NZD", n), flags = rep("", n), stringsAsFactors = FALSE)
  ov <- list(...)
  for (nm in names(ov)) base[[nm]] <- ov[[nm]]
  base
}

test_that("a statement dated outside the template's effective range is noted (P2-9)", {
  parsed <- list(transactions = .mk_tx(1),
                 header = list(period_start = "01/06/2025", period_end = "30/06/2025"))
  tmpl <- list(id = "t", effective_from = "2018-01-01", effective_to = "2020-12-31")
  d <- build_diagnostics("ok", parsed = parsed, metadata = list(template = tmpl))
  expect_true(any(d$category == "date_out_of_range"))     # soft caution raised
  # in-range -> no such note (a normal statement is unaffected).
  tmpl2 <- list(id = "t", effective_from = "2018-01-01", effective_to = NULL)
  d2 <- build_diagnostics("ok", parsed = parsed, metadata = list(template = tmpl2))
  expect_false(any(d2$category == "date_out_of_range"))
})

test_that("failing KPIs map to actionable fixes, most severe first", {
  parsed <- list(transactions = .mk_tx(2, date = c("2025-01-01", "2025-01-02"),
                                       amount = c(-5, -5), flags = c("", "")))
  recon <- list(kpis = data.frame(
    name = c("balance_reconciliation", "running_balance_continuity"),
    status = c("fail", "fail"), expected = c("100", "0"), actual = c("90", "2"),
    discrepancy = c("-10", "2"), detail = c("off by 10", "2 discontinuities"),
    stringsAsFactors = FALSE),
    trust = list(level = "low", score = 0, reasons = "fails"))
  d <- build_diagnostics("needs_review", parsed = parsed, recon = recon)
  expect_true(any(d$category == "reconciliation_mismatch"))
  expect_true(any(d$category == "balance_break"))
  expect_true(all(nzchar(d$how_to_fix)))
  expect_equal(d$severity[1], "high")
})

test_that("unsupported and failed produce actionable diagnostics", {
  du <- build_diagnostics("unsupported", det = list(detail = "closest bnz score 2/3"))
  expect_equal(du$category, "unknown_format")
  expect_match(du$how_to_fix, "toolkit", ignore.case = TRUE)
  df <- build_diagnostics("failed", messages = "cannot read file")
  expect_equal(df$category, "unreadable")
})

test_that("malformed / unparsed rows are diagnosed", {
  parsed <- list(transactions = .mk_tx(2,
    date = c("2025-01-01", NA), date_raw = c("1/1/25", "99/99/99"),
    amount = c(-5, NA), flags = c("", "malformed")))
  d <- build_diagnostics("needs_review", parsed = parsed, recon = list(kpis = NULL))
  expect_true(any(d$category == "row_parse"))
  expect_true(any(d$category == "date_parse"))
  expect_true(any(d$category == "amount_parse"))
})

test_that("clean statement yields a single 'none' diagnostic", {
  parsed <- list(transactions = .mk_tx())
  recon <- list(kpis = data.frame(name = "transaction_count", status = "pass",
    expected = ">0", actual = "1", discrepancy = NA, detail = "ok",
    stringsAsFactors = FALSE), trust = list(level = "high", score = 100, reasons = "ok"))
  d <- build_diagnostics("ok", parsed = parsed, recon = recon)
  expect_equal(nrow(d), 1L)
  expect_equal(d$category, "none")
})

test_that("convert_statement attaches diagnostics and writes a Diagnostics sheet", {
  out <- tempfile("diag_out_")
  res <- convert_statement(fixture("samples/raw/kiwibank/kiwibank_transaction_01.csv"),
                           bank = "Kiwibank", outdir = out,
                           templates_dir = templates_dir(), logdir = tempfile("log_"))
  expect_true(is.data.frame(res$diagnostics))
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  wb <- openxlsx::loadWorkbook(res$outputs[["xlsx"]])
  expect_true("Diagnostics" %in% names(wb))
})

test_that("diagnostics carry a 'who fixes this' owner and it classifies sensibly", {
  d <- build_diagnostics("unsupported", det = list(detail = "no match"))
  expect_true("fix_owner" %in% names(d))
  expect_equal(d$fix_owner[d$category == "unknown_format"], "template")
  # a multi-statement bundle is an input fix
  d2 <- build_diagnostics("ok", parsed = list(transactions =
      data.frame(row_id=1L, date="2025-01-01", date_raw="x", description="a", amount=-1,
        amount_raw="-1", direction="debit", balance=NA_real_, balance_raw=NA_character_,
        particulars=NA_character_, code=NA_character_, reference=NA_character_,
        other_party=NA_character_, type=NA_character_, currency="NZD", flags="",
        stringsAsFactors=FALSE)),
    recon = list(kpis=NULL),
    metadata = list(multi = list(likely_multiple = TRUE, reasons = "2 periods")))
  expect_equal(d2$fix_owner[d2$category == "multiple_statements"], "input")
  expect_match(diag_fix_owner_label("template"), "toolkit")
  expect_match(diag_fix_owner_label("escalate"), "Developer")
})
