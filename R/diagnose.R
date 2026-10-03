# diagnose.R -- fail-loud diagnostics. For any run that isn't perfectly clean,
# produce a structured explanation: WHERE it happened, WHY (category), HOW BAD
# (severity), the detail, and HOW TO FIX it. The goal is never a silent wrong
# answer: if the engine can't be fully confident, it says exactly why and what
# to do about it.
#
# build_diagnostics(status, messages, reading, parsed, recon, metadata) ->
# data.frame with columns: where, category, severity, detail, how_to_fix,
# fix_owner (most severe first).

.diag_row <- function(where, category, severity, detail, how_to_fix) {
  data.frame(where = where, category = category, severity = severity,
             detail = detail, how_to_fix = how_to_fix, stringsAsFactors = FALSE)
}

# WHO fixes this, so a lone analyst never wonders whether to click or phone a
# developer:
#   reading  = the analyst, on Please check (confirm the reading, or set a column's role)
#   input    = the person who supplied the file (split a bundle, re-export, rescan)
#   review   = just eyeball the data (expected situation, not an error)
#   none     = informational, no action
#   escalate = a genuine engine gap -> send it to a developer (rare)
#
# ONE entry per category the engine can raise. A plain lookup table rather than a
# switch(): adding a diagnostic means adding its line HERE too. The test "every
# diagnostic category has a declared owner" (test-diagnose.R) scans this file for
# the categories actually raised and fails if one is missing here.

# Raised from two places -- a failing reconciliation KPI and a direct check -- so
# the cure is written once and used from both.
.FIX_ROW_PARSE <- paste(
  "Some lines of the file did not read as a whole row. Check those rows against the",
  "statement on Please check; if the file itself is broken (a stray quote, a",
  "footer read as a row), ask for a fresh export.")

.FIX_CHECK_READING <- paste(
  "Open Please check: the columns found are drawn on the page. If the reading is",
  "right, confirm it. If a column is wrong, set its role (money out, money in,",
  "balance) and re-read - a fix that then adds up is learned for this bank.")

.DIAG_FIX_OWNER <- c(
  # the analyst, on Please check
  not_proven              = "reading",
  not_read                = "reading",
  fix_not_applied         = "reading",
  derived_amounts         = "reading",
  bank_check              = "reading",
  reconciliation_mismatch = "reading",
  balance_break           = "reading",
  row_count               = "reading",
  row_parse               = "reading",
  date_parse              = "reading",
  date_format_mismatch    = "reading",
  amount_parse            = "reading",
  amount_direction        = "reading",
  date_out_of_range       = "reading",
  account_number_shape    = "input",
  sign_scan_unavailable   = "escalate",
  unreadable              = "input",
  scanned_no_ocr          = "input",
  multiple_statements     = "input",
  oversized               = "input",
  oversized_page          = "input",
  sign_from_ink           = "review",
  low_ocr_confidence      = "input",
  ocr_confidence_unknown  = "input",
  completeness_unverified = "input",
  combined_statement      = "review",
  mixed_currency          = "review",
  ocr                     = "none",
  document_provenance     = "none",
  none                    = "none")

# .diag_fix_owner(category) -- the owner for each category; anything not in the
# table is a developer's (an engine gap), never silently the analyst's.
.diag_fix_owner <- function(category) {
  owner <- unname(.DIAG_FIX_OWNER[as.character(category)])
  owner[is.na(owner)] <- "escalate"
  owner
}

# ---------------------------------------------------------------------------
# PDF document provenance (informational).
#
# .PDF_GENERAL_TOOLS -- Producer / Creator strings that name a GENERAL-PURPOSE
# document tool (an editor, a converter, an office suite, a raster re-writer)
# rather than a bank's own statement-composition system. Matched case-insensitively
# as a substring of the self-declared Producer or Creator.
#
# WHY name them at all: the single most common question asked of a statement PDF is
# "did this come from the bank, or has it been through someone's PDF editor?" --
# and the answer is sitting in the file's own header. Naming the tool is a FACT
# about the file, not an accusation: plenty of innocent workflows (printing to PDF,
# combining pages, re-saving an email attachment) leave exactly these strings, and
# the strings themselves can be edited or removed by anyone. The diagnostic
# therefore says what the file claims and stops there -- it is severity "info",
# fix_owner "none", and it changes no figure, no status and no trust score.
#
# Plain vector on purpose: extending it is a one-line edit an analyst can make.
.PDF_GENERAL_TOOLS <- c(
  "Adobe Acrobat", "Acrobat Distiller", "Adobe Photoshop", "Adobe Illustrator",
  "Foxit", "Nitro", "PDF-XChange", "PDFsam", "Sejda", "Smallpdf", "iLovePDF",
  "PDFtk", "Ghostscript", "qpdf", "PyPDF", "pypdf", "iText", "ImageMagick",
  "LibreOffice", "OpenOffice", "Microsoft Word", "Microsoft Excel",
  "Microsoft PowerPoint", "Word for", "Excel for", "Google Docs Renderer",
  "Skia/PDF", "Quartz PDFContext", "Preview", "Scribus", "Canva", "Chromium")

# .pdf_tool_hit(x) -- the first general-purpose tool named in `x`, or NA. Matched
# as a LITERAL substring (fixed = TRUE): the entries are product names, so a stray
# regex metacharacter in one must never change what the others match.
.pdf_tool_hit <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1]) || !nzchar(x[1])) return(NA_character_)
  s <- tolower(x[1])
  hit <- .PDF_GENERAL_TOOLS[vapply(tolower(.PDF_GENERAL_TOOLS),
                                   function(t) grepl(t, s, fixed = TRUE), logical(1))]
  if (length(hit)) hit[1] else NA_character_
}

# ---------------------------------------------------------------------------
# What a FAILING reconciliation check means, and what to do about it: one entry
# per KPI that can fail (R/reconcile.R). Named fields rather than the positional
# c(where, category, severity, fix) vector this used to be -- the positions were
# read back as info[1]..info[4] two screens away, which is exactly how a wrong
# severity or a fix text in the category slot would slip through unnoticed.
#
# ADDING A KPI: add its entry here. Without one it still gets a diagnostic, but a
# deliberately vague one (.KPI_DIAGNOSIS_FALLBACK) that names no cause and no cure.
.KPI_DIAGNOSIS <- list(
  balance_reconciliation = list(
    where = "balance check", category = "reconciliation_mismatch", severity = "high",
    how_to_fix = "Statement doesn't reconcile: a transaction may be mis-signed, missing, or the opening/closing balance is wrong. Compare the total against the source."),
  running_balance_continuity = list(
    where = "running balance", category = "balance_break", severity = "high",
    how_to_fix = "Running balance jumps: a row's amount or sign is likely wrong, or a transaction is missing. Check the rows around the break."),
  transaction_count = list(
    where = "parse", category = "row_count", severity = "high",
    how_to_fix = "The statement states how many transactions it holds and a different number was read. Compare the rows on Please check with the statement: a row is missing or a summary line was read as a row."),
  dates_within_period = list(
    where = "dates", category = "date_out_of_range", severity = "medium",
    how_to_fix = "Dates fall outside the statement period: the date-format mapping may be wrong (day/month vs month/day)."),
  dates_readable = list(
    where = "dates", category = "date_parse", severity = "high",
    how_to_fix = "Some row dates could not be read. Check the date column on Please check against the statement: the column may be in the wrong place, or the dates printed in an unusual style."),
  no_unparsed_rows = list(
    where = "rows", category = "row_parse", severity = "high",
    how_to_fix = .FIX_ROW_PARSE),
  # NOT a column fault, which is why it gets its own category. Every other failing
  # check here points at the mapping; this one points at the IMAGE. The account number
  # is the only piece of metadata that can be checked against its own shape, and it is
  # the field that says whose statement this is -- so a misread digit here is a
  # statement attributed to the wrong account, not a column in the wrong place.
  account_number = list(
    where = "account number", category = "account_number_shape", severity = "medium",
    how_to_fix = paste("Read the account number off the statement image and compare it",
                       "character by character. On a scan this is almost always a misread",
                       "digit (0/8, 1/7, 5/6) or a lost one. If the STATEMENT prints it in",
                       "some form other than the New Zealand 2-4-7-2, nothing is wrong with",
                       "the file and the shape rule should be reported to whoever looks",
                       "after the tool.")),
  # Its own category, not amount_parse. The amounts here were read PERFECTLY -- it
  # is their DIRECTION that may be inverted -- and amount_parse is worded on screen
  # as "amounts couldn't be read", the opposite of what happened, on a card a
  # forensic accountant acts on. Same code, same file, two different defects.
  amount_direction = list(
    where = "amount direction", category = "amount_direction", severity = "high",
    how_to_fix = "Every amount has the same sign and there's no running balance to confirm direction. If this export lists amounts WITHOUT a +/- sign, money-in and money-out are inverted -- check the direction against the statement on Please check before relying on it."))

.KPI_DIAGNOSIS_FALLBACK <- list(
  where = "check", category = "reconciliation_mismatch", severity = "medium",
  how_to_fix = "Review this check against the source statement.")

# Compact a set of row indices for display.
.rng <- function(idx) {
  if (!length(idx)) return("")
  if (length(idx) <= 8) return(paste(idx, collapse = ","))
  paste0(paste(utils::head(idx, 8), collapse = ","), ",... (", length(idx), " total)")
}

build_diagnostics <- function(status, messages = character(0), reading = NULL,
                              parsed = NULL, recon = NULL, metadata = NULL) {
  rows <- list()
  raised <- character(0)          # categories already reported, so nothing is said twice
  add <- function(where, category, severity, detail, how_to_fix) {
    raised <<- c(raised, category)
    rows[[length(rows) + 1L]] <<- .diag_row(where, category, severity, detail, how_to_fix)
  }

  # A scan we could not machine-read looks identical to a statement the reader
  # could not make sense of unless it is said.
  no_ocr <- suppressWarnings(as.integer(metadata$scanned_no_ocr %||% 0L))
  if (identical(status, "unsupported") && !is.na(no_ocr) && no_ocr > 0) {
    tools_missing <- !isTRUE(metadata$ocr_tools %||% TRUE)
    add("file", "scanned_no_ocr", "high",
        sprintf("%d page(s) are a scan (an image, with no text layer), so there was nothing to read.", no_ocr),
        if (tools_missing)
          paste("This machine has no OCR software installed, so scanned statements cannot be read at all.",
                "Ask whoever set the tool up to install Tesseract and Poppler (they ship in the offline",
                "bundle under offline/prereqs).")
        else
          paste("The scan quality was too low to read. Try a cleaner copy - scan at 300 dpi or higher,",
                "straight, in good contrast - or ask the bank for a digital (text) PDF."))
  } else if (identical(status, "unsupported")) {
    add("reading", "not_read", "high", reading$why %||% "Nothing on the file could be read as a statement.",
        paste(.FIX_CHECK_READING, "If the file is not a bank statement at all, set it aside."))
  } else if (identical(status, "needs_review") && !is.null(reading$why)) {
    add("reading", "not_proven", "medium", reading$why, .FIX_CHECK_READING)
  } else if (identical(status, "failed")) {
    add("file", "unreadable", "high",
        paste(messages, collapse = " "),
        paste("Check the file opens, is the expected type (CSV / PDF / Excel),",
              "and is not password-protected or corrupt."))
  }
  if (length(reading$fix_error))
    add("Please check", "fix_not_applied", "high", reading$fix_error[1],
        "The fix was not applied, so this is the reading without it. Set the roles again on Please check.")
  nd <- suppressWarnings(as.integer(reading$derived %||% 0L))
  if (!is.na(nd) && nd > 0L)
    add("amounts", "derived_amounts", "medium",
        sprintf("%d amount(s) could not be read and were filled in from the running balance (flag amount_from_balance)", nd),
        "Check each marked amount against the statement before relying on it: it is arithmetic on two printed balances, not a figure read from the page.")
  # Two calls, two literal severities: a category whose severity is computed
  # cannot be audited by reading this file (test-diagnose.R scans for the shape).
  if (!is.null(reading$bank_why) && isTRUE(reading$bank_blocked))
    add("bank", "bank_check", "medium", reading$bank_why,
        "Nothing is learned from this statement until you confirm which bank issued it. Pick the right bank and convert again.")
  else if (!is.null(reading$bank_why))
    add("bank", "bank_check", "info", reading$bank_why,
        "Check the bank picked for this file is the one that issued it.")

  if (!is.null(metadata)) {
    # A file that looks like several statements but could not be split with
    # confidence is read whole, and never taken without a person.
    if (isTRUE(metadata$bundle_unsplit))
      add("upload", "multiple_statements", "high",
          paste(metadata$multi$reasons, collapse = "; "),
          paste("This file looks like more than one statement, and where one ends and the next",
                "begins could not be confirmed, so it was read whole. Split it into one",
                "statement per file and convert each."))
    else if (isTRUE(metadata$multi$combined_accounts))
      add("upload", "combined_statement", "info",
          sprintf("%d account numbers appear in one statement period", metadata$multi$n_accounts %||% 0L),
          "Looks like a combined statement (several accounts/products, or transfer counterparties named in transactions). If transactions from more than one account are mixed, running balances won't be continuous across them - review per account.")
    p <- suppressWarnings(as.integer(metadata$pages %||% NA))
    if (!is.na(p) && p > PARAM_MAX_PAGES) {
      ocr_p <- suppressWarnings(as.integer(metadata$ocr_pages %||% 0L))
      if (is.na(ocr_p)) ocr_p <- 0L
      mins <- max(1, round(p * (if (ocr_p > 0) PARAM_SECS_PER_SCAN_PAGE
                                else PARAM_SECS_PER_PAGE) / 60))
      add("upload", "oversized", "info",
          sprintf("%d pages in one file%s", p,
                  if (ocr_p > 0) sprintf(", %d of them read as scans", ocr_p) else ""),
          sprintf(paste("No action. A long statement is read the same way a short one",
                        "is - 400 pages and 12,000 rows convert in about 70 seconds,",
                        "with no limit to hit. This one worked out at roughly %d",
                        "minute(s)%s.",
                        "Do NOT split it into smaller files to speed it up: the",
                        "opening-plus-transactions-equals-closing check only works",
                        "across the whole statement, so splitting removes the proof",
                        "that the figures are right."),
                  mins,
                  if (ocr_p > 0)
                    ", most of it spent reading scanned pages as pictures"
                  else ""))
    }
    mp <- suppressWarnings(as.numeric(metadata$max_page_pt %||% NA))
    if (!is.na(mp) && mp > PARAM_MAX_PAGE_PT)
      add("upload", "oversized_page", "medium",
          sprintf("largest page is %.0f pt (> 2880 pt / 40 in)", mp),
          "Pages larger than 40 inches (2880 pt) can break rendering/OCR. Re-export at a standard page size.")
    # THE SIGNS ON THIS PAGE WERE NOT WHERE A SIGN NORMALLY IS. Either a minus was
    # drawn as vector ink and is absent from the text layer, or one was printed in
    # the background colour and is in the text layer but not on the page. The reader
    # now handles both (.apply_ink_signs, R/read_pdf.R) and the figures are right --
    # but a statement that prints its signs this way is a fact about the document,
    # and a reviewer comparing the workbook against the page should know why a
    # figure is negative when the page shows no minus they can see. Info severity,
    # fix_owner "review": nothing is wrong and nothing needs correcting.
    .nink <- suppressWarnings(as.integer(metadata$ink_minus_signs %||% 0L))
    .nfaint <- suppressWarnings(as.integer(metadata$faint_minus_signs %||% 0L))
    if (!is.na(.nink) && !is.na(.nfaint) && (.nink + .nfaint) > 0L)
      add("upload", "sign_from_ink", "info",
          paste(c(if (.nink > 0L) sprintf("%d minus sign(s) are drawn as a line, not printed as text", .nink),
                  if (.nfaint > 0L) sprintf("%d minus sign(s) are in the text but printed in the background colour, so they are not on the page", .nfaint)),
                collapse = "; "),
          paste("Nothing to fix - the signs have been read from the page itself.",
                "If you are checking a figure against the statement by eye, this is",
                "why a minus may be hard to see."))
  }

  # PDF document provenance. Raised for EVERY status (a file that could not be read
  # is exactly when "what wrote this?" is worth knowing), and only when
  # there is something to say: the modified time differs from the creation time, or
  # the Producer/Creator names a general-purpose PDF tool. Facts only -- severity
  # info, fix_owner none, no effect on figures, status or trust. Read from either
  # place the reader's metadata can arrive so it works whichever caller supplies it.
  doc <- metadata$pdf_doc %||% parsed$header$pdf_doc
  if (is.list(doc)) {
    fv <- function(v) if (is.null(v) || !length(v) || is.na(v[1]) || !nzchar(as.character(v[1])))
      "not stated" else as.character(v[1])
    created <- fv(doc$created); modified <- fv(doc$modified)
    tool <- .pdf_tool_hit(doc$producer)
    if (is.na(tool)) tool <- .pdf_tool_hit(doc$creator)
    edited_time <- !identical(created, "not stated") && !identical(modified, "not stated") &&
                   !identical(created, modified)
    if (edited_time || !is.na(tool)) {
      why <- character(0)
      if (edited_time) why <- c(why, "the file's last-modified time is not the same as its creation time")
      if (!is.na(tool)) why <- c(why, sprintf("the tool it names (%s) is general-purpose software, not a bank statement system", tool))
      add("document", "document_provenance", "info",
        sprintf("the PDF says it was written by %s (creator %s), created %s, last modified %s%s - noted because %s",
                fv(doc$producer), fv(doc$creator), created, modified,
                if (isTRUE(doc$encrypted)) ", and the file is encrypted" else "",
                paste(why, collapse = ", and ")),
        paste("No action - this is recorded, not a problem. These details are what the file says",
              "about ITSELF: any tool can write, change or remove them, and ordinary handling",
              "(printing to PDF, combining pages, re-saving an attachment) produces exactly the",
              "same marks. Nothing about the transactions is affected. If the origin of the",
              "document matters to this case, compare these details against the copy the bank issued."))
    }
  }

  # THE SIGN CHECK DID NOT RUN, which is worth more than it looks. Some banks draw
  # the minus as a stroke of ink, and some print it in the background colour on
  # POSITIVE amounts to keep a column right-aligned. Both are invisible to the text
  # layer, in opposite directions, and on a statement with no running balance NOTHING
  # ELSE CATCHES EITHER -- the figures come out inverted and every check passes.
  # pdftocairo is what reads them, and it ships in the same poppler bundle as the
  # tools the rest of the reader already needs, so its absence is an install fault.
  if (isTRUE(!is.null(metadata$ink_scan_ran)) && !isTRUE(metadata$ink_scan_ran)) {
    add("signs", "sign_scan_unavailable", "high",
        paste("the check that reads a minus sign from the page itself did not run, so",
              "if this bank draws its minus as a line, or prints one in the background",
              "colour, the money-in / money-out direction may be inverted on every row"),
        paste("Ask whoever set the tool up to install Poppler, including pdftocairo",
              "(it is in the offline bundle under offline/prereqs, alongside the",
              "pdftotext this reader already uses). Until then, check the +/- direction",
              "of these rows against the statement by eye before using them - and",
              "treat a statement with NO running balance column as unverified,",
              "because the balance checks are what would otherwise have caught it."))
  }

  if (!is.null(parsed) && !is.null(parsed$transactions)) {
    tx <- parsed$transactions

    if (!is.null(recon) && !is.null(recon$kpis)) {
      k <- recon$kpis
      fails <- k[k$status == "fail", , drop = FALSE]
      for (i in seq_len(nrow(fails))) {
        # An auto-split run tags each KPI name "<name> [statement N]"; match the
        # BASE name so the correct severity + fix text (not the generic fallback)
        # are used, while the failing statement stays visible in the detail below.
        nm <- sub("[[:space:]]*\\[statement [0-9]+\\]$", "", fails$name[i])
        stmt_tag <- regmatches(fails$name[i], regexpr("\\[statement [0-9]+\\]$", fails$name[i]))
        info <- .KPI_DIAGNOSIS[[nm]] %||% .KPI_DIAGNOSIS_FALLBACK
        where <- if (length(stmt_tag)) paste(info$where, stmt_tag) else info$where
        add(where, info$category, info$severity, fails$detail[i], info$how_to_fix)
      }
    }

    # 1b. Completeness cannot be verified (no balance / stated count).
    if (!is.null(recon) && isFALSE(recon$trust$completeness_verified) && nrow(tx) > 0)
      add("completeness", "completeness_unverified", "medium",
        "no balance or stated count to reconcile against",
        "The engine can't confirm every transaction was captured (nothing to reconcile the total against). Count the rows against the statement, or prefer a CSV/Excel export or a statement that shows a running balance.")

    # 2. Row-level parse problems (independent of KPI wiring).
    # Guarded against the KPI raising it first: the KPI and the row-level scan both
    # find the same malformed rows and now share one cure, so without this the
    # reader gets the same instruction twice and reads it as two findings.
    mal <- which(grepl("malformed", tx$flags %||% ""))
    if (length(mal) && !("row_parse" %in% raised))
      add(sprintf("rows %s", .rng(mal)), "row_parse", "high",
      sprintf("%d row(s) had the wrong number of fields", length(mal)),
      .FIX_ROW_PARSE)

    dalt <- which(grepl("date_alt_format", tx$flags %||% ""))
    if (length(dalt)) add(sprintf("rows %s (date)", .rng(dalt)), "date_format_mismatch", "medium",
      sprintf("%d date(s) were written in a different style from the rest of the column", length(dalt)),
      "Rows like '17 Sep' were read with the year taken from the statement period. Check those dates against the statement.")

    dyi <- which(grepl("date_year_inferred", tx$flags %||% ""))
    if (length(dyi)) add(sprintf("rows %s (date)", .rng(dyi)), "date_out_of_range", "medium",
      sprintf("%d date(s) took their YEAR from a number in the page text, not a statement period", length(dyi)),
      "The statement showed day+month only and no readable period, so the year was inferred from a single 4-digit number on the page (which could be a footer/copyright year). Confirm the year is right.")

    dbad <- which(is.na(tx$date) & !is.na(tx$date_raw) & nzchar(tx$date_raw %||% ""))
    if (length(dbad)) add(sprintf("rows %s (date)", .rng(dbad)), "date_parse", "medium",
      sprintf("%d date(s) could not be read", length(dbad)),
      "These dates are printed in a style that did not read. Check them against the statement on Please check.")

    abad <- which(is.na(tx$amount))
    if (length(abad)) add(sprintf("rows %s (amount)", .rng(abad)), "amount_parse", "high",
      sprintf("%d amount(s) could not be read", length(abad)),
      "Check these rows on Please check: the amount column may be in the wrong place, or the figures printed in an unusual style.")

    # 3. Informational context.
    cur <- unique(tx$currency[!is.na(tx$currency)])
    if (length(cur) > 1) add("currency", "mixed_currency", "info",
      sprintf("multiple currencies present: %s", paste(cur, collapse = ", ")),
      "Foreign-currency lines are present. Confirm downstream handling of non-base currencies.")

    ocrp <- suppressWarnings(as.integer(parsed$header$ocr_pages %||% NA))
    if (!is.na(ocrp) && ocrp > 0) add("pages (OCR)", "ocr", "info",
      sprintf("%d page(s) were machine-read via OCR", ocrp),
      "OCR pages can contain recognition errors. Spot-check machine-read values against the image.")
    ocrc <- suppressWarnings(as.numeric(parsed$header$ocr_min_confidence %||% NA))
    if (!is.na(ocrc) && ocrc < PARAM_OCR_PAGE_MIN_CONF) add("OCR text", "low_ocr_confidence", "high",
      sprintf("lowest page-mean OCR confidence was %.0f%%", ocrc),
      "OCR is unsure of some characters. Re-scan at higher DPI/contrast, or verify the flagged pages against the image; reconciliation still guards the totals. Rows with a doubtful cell carry an 'ocr_low_conf' flag.")
    # OCR ran but confidence could not be measured (the TSV pass failed): a
    # distinct caveat, since the generic info note above understates it.
    if (!is.na(ocrp) && ocrp > 0 && is.na(ocrc)) add("OCR text", "ocr_confidence_unknown", "high",
      "OCR ran but its confidence could not be measured",
      "Treat the machine-read values as unverified and check them against the image.")
  }

  if (!length(rows)) {
    clean <- .diag_row("-", "none", "info",
                       "No issues detected; all applicable checks passed.", "-")
    clean$fix_owner <- "none"
    return(clean)
  }

  out <- do.call(rbind, rows)
  out$fix_owner <- .diag_fix_owner(out$category)
  out[order(match(out$severity, c("high", "medium", "info"))), , drop = FALSE]
}

# diag_fix_owner_label(owner) -- plain-language "who fixes this" for display.
diag_fix_owner_label <- function(owner) {
  unname(c(
    reading  = "You - check the reading (Please check)",
    input    = "You - fix the file (split / re-export / rescan)",
    review   = "You - review the data (expected, not an error)",
    none     = "No action",
    escalate = "Developer - engine gap (escalate)"
  )[owner])
}
