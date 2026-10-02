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

# The Diagnostics table is customer-facing, and the unknown_format row printed the
# detection LOG line into it: "closest anz_everyday_pdf score 2/3 (missing
# 'Transaction type and details')". A template id and a fraction are exactly what
# the operational guide tells the analyst to report as a bug when they appear on a
# screen. Same evidence, in words; the id and the score stay in the run log.
test_that("the unknown-layout diagnostic carries no template id and no score", {
  out <- tempfile("diagplain_"); dir.create(out)
  res <- convert_statement(fixture("samples/raw/anz/anz_card_summary_sample.pdf"),
                           outdir = out, templates_dir = templates_dir(), logdir = out)
  expect_identical(res$status, "unsupported")
  d <- res$diagnostics
  detail <- d$detail[d$category == "unknown_format"]
  expect_length(detail, 1L)
  expect_false(grepl("anz_creditcard_csv", detail, fixed = TRUE))   # no id
  expect_false(grepl("score", detail, ignore.case = TRUE))          # no metric
  expect_match(detail, "ANZ creditcard statement", fixed = TRUE)    # the human name
  expect_match(detail, "TransactionDate", fixed = TRUE)             # the missing wording
  # ...and the id + score are NOT lost -- they are in the run log, where they belong
  expect_match(res$run_log$detect_detail, "anz_creditcard_csv", fixed = TRUE)
  expect_match(res$run_log$detect_detail, "score", fixed = TRUE)
})

test_that("a caller that builds a det by hand still gets its detail through", {
  # build_diagnostics has callers that assemble a det themselves (the batch audit,
  # the tests above); they carry no detail_plain and must not lose their message.
  d <- build_diagnostics("unsupported", det = list(detail = "closest bnz score 2/3"))
  expect_equal(d$detail, "closest bnz score 2/3")
})

# A tie now converts, so the reviewer is holding figures from ONE of the tied
# templates. The tie list alone does not say which -- and that is the first thing
# needed to check the figures against the right template.
test_that("the ambiguous-template diagnostic names the template that was used", {
  d <- build_diagnostics("needs_review",
    metadata = list(tied = c("a_csv", "b_csv"), tied_used = "a_csv"))
  row <- d[d$category == "ambiguous_template", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_match(row$detail, "it was read with a_csv", fixed = TRUE)
  expect_match(row$detail, "a_csv, b_csv", fixed = TRUE)
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

# "the amounts were read fine but the DIRECTION may be inverted" is not "amounts
# couldn't be read". It shared amount_parse's category, so the screen rendered the
# opposite of what happened, on the card a forensic accountant acts on.
test_that("an inverted-direction warning is its own category, not an unread-amount one", {
  recon <- list(kpis = data.frame(
    name = "amount_direction", status = "fail", expected = "mixed",
    actual = "all money-in", discrepancy = NA,
    detail = "every amount shares one sign", stringsAsFactors = FALSE))
  d <- build_diagnostics("needs_review", parsed = list(transactions = .mk_tx()),
                         recon = recon)
  expect_true("amount_direction" %in% d$category)
  expect_false("amount_parse" %in% d$category)
  expect_equal(unname(.DIAG_FIX_OWNER[["amount_direction"]]), "template")
  # ...and a genuinely unreadable amount still raises amount_parse
  d2 <- build_diagnostics("needs_review",
    parsed = list(transactions = .mk_tx(2, amount = c(-5, NA), flags = c("", ""))),
    recon = list(kpis = NULL))
  expect_true("amount_parse" %in% d2$category)
  expect_false("amount_direction" %in% d2$category)
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

# .diag_fix_owner used to be a switch() with a default of "escalate". Four live
# categories had quietly fallen off the end of it and were being triaged as
# "Developer - engine gap", the most expensive possible answer -- including
# date_format_mismatch, whose own how-to-fix text tells the analyst to change the
# template's date format in the toolkit. These pin the owners so the routing
# cannot drift back.
test_that("categories that a person can fix are NOT triaged as an engine gap", {
  expect_equal(.diag_fix_owner("date_format_mismatch"), "template")  # fix it in the toolkit
  expect_equal(.diag_fix_owner("scanned_no_ocr"), "input")           # rescan / install OCR
  expect_equal(.diag_fix_owner("ocr_confidence_unknown"), "input")   # rescan / check the image
  # ...and the one that genuinely IS a maintainer's problem still says so: the
  # page could not be rasterised, so redactions cannot be proved to have held.
  expect_equal(.diag_fix_owner("redaction_unverified"), "escalate")
  # an unrecognised category still fails safe (surface it, never hide it)
  expect_equal(.diag_fix_owner("something_new"), "escalate")
  expect_equal(.diag_fix_owner(character(0)), character(0))          # type-stable
})

# A failing KPI with no entry in .KPI_DIAGNOSIS gets the deliberately vague
# fallback -- which was what the redaction-scan failure, the gravest verdict the
# engine can reach, was being rendered with.
test_that("a failed redaction scan is diagnosed as a redaction problem, not a template one", {
  tx <- data.frame(row_id = 1L, date = "2025-01-01", date_raw = "x", description = "a",
    amount = -1, amount_raw = "-1", direction = "debit", balance = NA_real_,
    balance_raw = NA_character_, particulars = NA_character_, code = NA_character_,
    reference = NA_character_, other_party = NA_character_, type = NA_character_,
    currency = "NZD", flags = "", stringsAsFactors = FALSE)
  kpis <- data.frame(name = "redaction_scan", status = "fail", expected = "0",
    actual = "2", discrepancy = "2",
    detail = "the redaction scan could not complete on 2 page(s)",
    stringsAsFactors = FALSE)
  d <- build_diagnostics("needs_review",
    parsed = list(transactions = tx, header = list(redaction_scan_incomplete = 0L)),
    recon = list(kpis = kpis, trust = list(completeness_verified = TRUE)))
  row <- d[d$where == "redactions", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$category, "redaction_unverified")
  expect_equal(row$severity, "high")
  expect_equal(row$fix_owner, "escalate")          # not "template"
  expect_match(row$how_to_fix, "Do NOT release")
  expect_false(grepl("Review this check against the source", row$how_to_fix))
})

# The guard that makes the drift above impossible to repeat: every category the
# file can actually raise must have a declared owner, and the table must not carry
# entries for categories nothing raises (a dead row reads as coverage it isn't).
test_that("every diagnostic category has a declared owner, and none is dead", {
  src <- paste(readLines(file.path(engine_root(), "R", "diagnose.R"), warn = FALSE),
               collapse = "\n")
  # Two shapes raise a diagnostic: add(where, category, severity, ...) puts the
  # category immediately before the severity, and the failing-KPI table names it.
  pos <- regmatches(src, gregexpr("\"[a-z0-9_]+\",[ \n]*\"(high|medium|info)\"", src))[[1]]
  pos <- sub("^\"([a-z0-9_]+)\".*$", "\\1", pos)
  named <- regmatches(src, gregexpr("category = \"[a-z0-9_]+\"", src))[[1]]
  named <- sub("^category = \"([a-z0-9_]+)\"$", "\\1", named)
  raised <- setdiff(unique(c(pos, named)), c("high", "medium", "info"))
  expect_gt(length(raised), 20)          # the scan itself must not go quiet
  expect_equal(sort(setdiff(raised, names(.DIAG_FIX_OWNER))), character(0))
  expect_equal(sort(setdiff(names(.DIAG_FIX_OWNER), raised)), character(0))
  # every owner is one of the five the label map can render
  expect_true(all(.DIAG_FIX_OWNER %in%
    c("template", "input", "review", "none", "escalate")))
  expect_false(any(is.na(diag_fix_owner_label(unname(.DIAG_FIX_OWNER)))))
})

# A scan we could not machine-read must never be reported as an unknown layout.
# With no text and no word boxes every template scores 0, so the generic
# "no template matched -- closest X, missing 'TransactionDate'" message used to
# fire and send the analyst to build a template for a page with no readable text.
test_that("a scan with no OCR tooling says so, and names the admin fix (#54)", {
  det <- list(matched = FALSE, template_id = NA_character_,
              detail = "closest anz_creditcard_csv score 0/4 (missing 'TransactionDate')")
  d <- build_diagnostics("unsupported", det = det,
         metadata = list(scanned_no_ocr = 3L, ocr_tools = FALSE))
  expect_true("scanned_no_ocr" %in% d$category)
  expect_false("unknown_format" %in% d$category)          # the misleading line is suppressed
  row <- d[d$category == "scanned_no_ocr", , drop = FALSE]
  expect_match(row$detail[1], "scan")
  expect_match(row$how_to_fix[1], "Tesseract")            # names the actual cause
  expect_match(row$how_to_fix[1], "NOT help")             # and stops the wild goose chase
})

test_that("a scan WITH OCR tooling blames quality, not the install (#54)", {
  d <- build_diagnostics("unsupported", det = list(matched = FALSE, detail = "x"),
         metadata = list(scanned_no_ocr = 1L, ocr_tools = TRUE))
  row <- d[d$category == "scanned_no_ocr", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_match(row$how_to_fix[1], "300 dpi")
  expect_false(grepl("Tesseract", row$how_to_fix[1]))
})

# ---- PDF document provenance (#46) ----------------------------------------
# "Was this produced by the bank, or by someone with a PDF editor?" is answered by
# the file's own header. We STATE what it says and stop there: info severity, no
# action owner, and no effect on figures, status or trust.

.mk_doc <- function(...) utils::modifyList(
  list(producer = "xPression 20.3 Patch 1 /  / JPDF", creator = NA_character_,
       created = "2025-09-14 00:00:53", modified = "2025-09-14 00:00:53",
       encrypted = FALSE, pdf_version = "1.4"), list(...))

test_that("a PDF modified after it was created is reported as a fact (#46)", {
  d <- build_diagnostics("ok", parsed = list(transactions = .mk_tx(), header = list()),
         recon = list(kpis = NULL),
         metadata = list(pdf_doc = .mk_doc(modified = "2026-02-01 09:00:00")))
  row <- d[d$category == "document_provenance", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$severity, "info")            # never escalates a run
  expect_equal(row$fix_owner, "none")           # nobody has to do anything
  expect_match(row$detail, "2026-02-01 09:00:00", fixed = TRUE)
  expect_match(row$detail, "last-modified time is not the same")
  # states the facts, draws no conclusion
  expect_false(grepl("tamper|forg|falsif|suspic", row$detail, ignore.case = TRUE))
  expect_match(row$how_to_fix, "No action", fixed = TRUE)
})

test_that("a general-purpose PDF tool is named even when the timestamps match (#46)", {
  d <- build_diagnostics("ok", parsed = list(transactions = .mk_tx(), header = list()),
         recon = list(kpis = NULL),
         metadata = list(pdf_doc = .mk_doc(producer = "Adobe Acrobat 19.10 Image Conversion Plug-in",
                                           creator = "Adobe Acrobat 19.10")))
  row <- d[d$category == "document_provenance", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_match(row$detail, "Adobe Acrobat", fixed = TRUE)
  expect_match(row$detail, "general-purpose software")
})

test_that("a bank's own statement engine raises nothing (#46 noise guard)", {
  # xPression is a statement-composition system and the timestamps agree: there is
  # nothing to say, so nothing is said. A note on every PDF would be noise Beth
  # learns to ignore -- which is how a real signal gets missed.
  d <- build_diagnostics("ok", parsed = list(transactions = .mk_tx(), header = list()),
         recon = list(kpis = NULL), metadata = list(pdf_doc = .mk_doc()))
  expect_false("document_provenance" %in% d$category)
  # and a run with no PDF metadata at all (CSV / Excel) is untouched
  d2 <- build_diagnostics("ok", parsed = list(transactions = .mk_tx()),
                          recon = list(kpis = NULL))
  expect_equal(nrow(d2), 1L)
  expect_equal(d2$category, "none")
})

test_that("provenance is reported even when no template matched (#46)", {
  # An unrecognised file is exactly when "what wrote this?" is worth knowing.
  d <- build_diagnostics("unsupported", det = list(matched = FALSE, detail = "no match"),
         metadata = list(pdf_doc = .mk_doc(producer = "Ghostscript 9.55",
                                           created = "2020-01-01 00:00:00",
                                           modified = "2020-01-01 00:00:00")))
  expect_true("document_provenance" %in% d$category)
  expect_true("unknown_format" %in% d$category)   # the real problem still leads
  expect_equal(d$severity[1], "high")             # info never outranks it
})

test_that("an encrypted file and unstated fields are described honestly (#46)", {
  d <- build_diagnostics("ok", parsed = list(transactions = .mk_tx(), header = list()),
         recon = list(kpis = NULL),
         metadata = list(pdf_doc = .mk_doc(producer = "iTextSharp 4.1.6 by 1T3XT",
                                           creator = NA_character_, encrypted = TRUE)))
  row <- d[d$category == "document_provenance", , drop = FALSE]
  expect_match(row$detail, "creator not stated", fixed = TRUE)
  expect_match(row$detail, "the file is encrypted", fixed = TRUE)
})

test_that(".pdf_tool_hit matches product names literally and case-insensitively", {
  expect_identical(.pdf_tool_hit("Adobe Acrobat 19.10"), "Adobe Acrobat")
  expect_identical(.pdf_tool_hit("https://legacy.imagemagick.org"), "ImageMagick")
  expect_true(is.na(.pdf_tool_hit("xPression 23.4 Patch 13 /  / JPDF")))
  expect_true(is.na(.pdf_tool_hit(NA_character_)))
  expect_true(is.na(.pdf_tool_hit("")))
  expect_true(is.na(.pdf_tool_hit(NULL)))
})

test_that("provenance also reaches diagnostics from the parsed header (#46)", {
  # convert_statement builds the metadata list; the parse builds the header. The
  # diagnostic reads either, so whichever route carries it, the fact is reported.
  d <- build_diagnostics("ok",
         parsed = list(transactions = .mk_tx(),
                       header = list(pdf_doc = .mk_doc(producer = "Foxit PhantomPDF"))),
         recon = list(kpis = NULL))
  expect_true("document_provenance" %in% d$category)
})

test_that("an ordinary unsupported layout still gets the template advice (#54 guard)", {
  d <- build_diagnostics("unsupported", det = list(matched = FALSE, detail = "no match"),
         metadata = list(scanned_no_ocr = 0L, ocr_tools = TRUE))
  expect_true("unknown_format" %in% d$category)
  expect_false("scanned_no_ocr" %in% d$category)
})
