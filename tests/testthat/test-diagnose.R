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

test_that("unread, unproven and failed produce actionable diagnostics", {
  du <- build_diagnostics("unsupported", reading = list(why = "No line on any page prints a date with a figure beside it."))
  expect_equal(du$category, "not_read")
  expect_match(du$detail, "No line on any page", fixed = TRUE)
  expect_match(du$how_to_fix, "Please check", fixed = TRUE)
  dn <- build_diagnostics("needs_review", reading = list(why = "2 different readings of the columns all fit the arithmetic."))
  expect_equal(dn$category, "not_proven")
  expect_equal(dn$fix_owner, "reading")
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

test_that("a derived amount, a refused fix and a disputed bank are each said", {
  d <- build_diagnostics("needs_review", reading = list(why = "x", derived = 2L, fix_error = "no such column",
                                                        bank_why = "You picked ANZ, but...", bank_blocked = TRUE))
  expect_true(all(c("derived_amounts", "fix_not_applied", "bank_check") %in% d$category))
  expect_match(d$detail[d$category == "derived_amounts"], "2 amount", fixed = TRUE)
  expect_equal(d$severity[d$category == "bank_check"], "medium")
  d2 <- build_diagnostics("ok", reading = list(bank_why = "Using ANZ, as picked."))
  expect_equal(d2$severity[d2$category == "bank_check"], "info")
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
                           bank = "Kiwibank", outdir = out, logdir = tempfile("log_"),
                           layouts_dir = tempfile("ly_"), tracking_dir = NA)
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
  expect_equal(unname(.DIAG_FIX_OWNER[["amount_direction"]]), "reading")
  # ...and a genuinely unreadable amount still raises amount_parse
  d2 <- build_diagnostics("needs_review",
    parsed = list(transactions = .mk_tx(2, amount = c(-5, NA), flags = c("", ""))),
    recon = list(kpis = NULL))
  expect_true("amount_parse" %in% d2$category)
  expect_false("amount_direction" %in% d2$category)
})

test_that("diagnostics carry a 'who fixes this' owner and it classifies sensibly", {
  d <- build_diagnostics("unsupported", reading = list(why = "no table"))
  expect_true("fix_owner" %in% names(d))
  expect_equal(d$fix_owner[d$category == "not_read"], "reading")
  # a multi-statement bundle is an input fix
  d2 <- build_diagnostics("ok", parsed = list(transactions =
      data.frame(row_id=1L, date="2025-01-01", date_raw="x", description="a", amount=-1,
        amount_raw="-1", direction="debit", balance=NA_real_, balance_raw=NA_character_,
        particulars=NA_character_, code=NA_character_, reference=NA_character_,
        other_party=NA_character_, type=NA_character_, currency="NZD", flags="",
        stringsAsFactors=FALSE)),
    recon = list(kpis=NULL),
    metadata = list(bundle_unsplit = TRUE, multi = list(likely_multiple = TRUE, reasons = "2 periods")))
  expect_equal(d2$fix_owner[d2$category == "multiple_statements"], "input")
  expect_match(diag_fix_owner_label("reading"), "Please check")
  expect_match(diag_fix_owner_label("escalate"), "Developer")
})

# .diag_fix_owner used to be a switch() with a default of "escalate". Four live
# categories had quietly fallen off the end of it and were being triaged as
# "Developer - engine gap", the most expensive possible answer -- including
# date_format_mismatch, whose own how-to-fix text sends the analyst to check the
# dates. These pin the owners so the routing cannot drift back.
test_that("categories that a person can fix are NOT triaged as an engine gap", {
  expect_equal(.diag_fix_owner("date_format_mismatch"), "reading")   # check it on Please check
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
    c("reading", "input", "review", "none", "escalate")))
  expect_false(any(is.na(diag_fix_owner_label(unname(.DIAG_FIX_OWNER)))))
})

# A scan we could not machine-read must never be reported as a reading to check:
# with no text there is nothing to set roles on.
test_that("a scan with no OCR tooling says so, and names the admin fix (#54)", {
  d <- build_diagnostics("unsupported", reading = list(why = "The PDF has no readable text on any page."),
         metadata = list(scanned_no_ocr = 3L, ocr_tools = FALSE))
  expect_true("scanned_no_ocr" %in% d$category)
  expect_false("not_read" %in% d$category)                # the misleading line is suppressed
  row <- d[d$category == "scanned_no_ocr", , drop = FALSE]
  expect_match(row$detail[1], "scan")
  expect_match(row$how_to_fix[1], "Tesseract")            # names the actual cause
})

test_that("a scan WITH OCR tooling blames quality, not the install (#54)", {
  d <- build_diagnostics("unsupported", reading = list(why = "x"),
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

test_that("provenance is reported even when nothing was read (#46)", {
  # An unreadable statement is exactly when "what wrote this?" is worth knowing.
  d <- build_diagnostics("unsupported", reading = list(why = "no table"),
         metadata = list(pdf_doc = .mk_doc(producer = "Ghostscript 9.55",
                                           created = "2020-01-01 00:00:00",
                                           modified = "2020-01-01 00:00:00")))
  expect_true("document_provenance" %in% d$category)
  expect_true("not_read" %in% d$category)         # the real problem still leads
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

test_that("an ordinary unread statement still gets the Please check advice (#54 guard)", {
  d <- build_diagnostics("unsupported", reading = list(why = "no table"),
         metadata = list(scanned_no_ocr = 0L, ocr_tools = TRUE))
  expect_true("not_read" %in% d$category)
  expect_false("scanned_no_ocr" %in% d$category)
})
