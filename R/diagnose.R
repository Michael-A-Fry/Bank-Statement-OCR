# diagnose.R -- fail-loud diagnostics. For any run that isn't perfectly clean,
# produce a structured explanation: WHERE it happened, WHY (category), HOW BAD
# (severity), the detail, and HOW TO FIX it. The goal is never a silent wrong
# answer: if the engine can't be fully confident, it says exactly why and what
# to do about it.
#
# build_diagnostics(status, messages, det, parsed, recon) -> data.frame with
# columns: where, category, severity, detail, how_to_fix (most severe first).

.diag_row <- function(where, category, severity, detail, how_to_fix) {
  data.frame(where = where, category = category, severity = severity,
             detail = detail, how_to_fix = how_to_fix, stringsAsFactors = FALSE)
}

# WHO fixes this, so a lone analyst never wonders whether to draw a box or phone a
# developer:
#   template = the analyst, in the wizard (a column/box/date/amount setting)
#   input    = the person who supplied the file (split a bundle, re-export, rescan)
#   review   = just eyeball the data (expected situation, not an error)
#   none     = informational, no action
#   escalate = a genuine engine gap -> send it to a developer (rare)
#
# ONE entry per category the engine can raise. A plain lookup table rather than a
# switch(): adding a diagnostic means adding its line HERE too, and it is obvious at
# a glance which categories are covered. That matters -- when this was a switch,
# four live categories had quietly fallen off the end and were being reported as
# "Developer - engine gap" (see below), including date_format_mismatch, whose own
# how-to-fix text tells the analyst to change the template. The test
# "every diagnostic category has a declared owner" (test-diagnose.R) scans this
# file for the categories actually raised and fails if one is missing here.
.DIAG_FIX_OWNER <- c(
  # the analyst fixes it in the template toolkit
  unknown_format          = "template",
  ambiguous_template      = "template",
  matched_but_empty       = "template",
  read_by_another_template = "template",
  reconciliation_mismatch = "template",
  balance_break           = "template",
  row_count               = "template",
  row_parse               = "template",
  date_parse              = "template",
  date_format_mismatch    = "template",
  amount_parse            = "template",
  date_out_of_range       = "template",
  # whoever supplied the file fixes it (split / re-export / rescan / install OCR)
  unreadable              = "input",
  scanned_no_ocr          = "input",
  multiple_statements     = "input",
  oversized               = "input",
  oversized_page          = "input",
  low_ocr_confidence      = "input",
  ocr_confidence_unknown  = "input",
  completeness_unverified = "input",
  # nothing is wrong; just look at the data
  combined_statement      = "review",
  mixed_currency          = "review",
  # stated for the record; nobody has to do anything
  redaction               = "none",
  ocr                     = "none",
  document_provenance     = "none",
  none                    = "none",
  # a genuine engine/install gap: the page could not be rasterised, so the tool
  # cannot prove nothing is hidden under a box. Only a maintainer can fix that.
  redaction_unverified    = "escalate")

# .diag_fix_owner(category) -- look the owner up. An unknown category defaults to
# escalate (fail safe: surface it, don't hide it).
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

# Verbatim period bounds / effective dates are parsed with the shared
# .tolerant_date (see R/params.R) -- NA on anything unrecognised so the
# effective-range check simply skips rather than guessing.

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
    how_to_fix = "Parsed count doesn't match: the template rows/columns may not fit this file. Re-map it in the template toolkit."),
  dates_within_period = list(
    where = "dates", category = "date_out_of_range", severity = "medium",
    how_to_fix = "Dates fall outside the statement period: the date-format mapping may be wrong (day/month vs month/day)."),
  dates_readable = list(
    where = "dates", category = "date_parse", severity = "high",
    how_to_fix = "No row dates could be read: the template's date column wasn't found in this file (renamed header?) or the date format is wrong. Fix the Date column / format in the template toolkit."),
  no_unparsed_rows = list(
    where = "rows", category = "row_parse", severity = "high",
    how_to_fix = "Some source lines didn't parse (malformed or lost): usually a delimiter/quoting issue, or a preamble/footer line read as data. Check those rows."),
  amount_direction = list(
    where = "amount direction", category = "amount_parse", severity = "high",
    how_to_fix = "Every amount has the same sign and there's no running balance to confirm direction. If this export lists amounts WITHOUT a +/- sign, money-in and money-out are inverted -- set the correct amount style (e.g. debit/credit columns, or unsigned) in the template toolkit."),
  # Had no entry, so the gravest verdict the engine can reach -- the redaction scan
  # did not finish, so blacked-out text may have been read and emitted -- was
  # rendered with the fallback below: category reconciliation_mismatch, owner
  # "template", advice "review this check against the source statement". It told the
  # analyst to adjust their column mapping about a possible PII leak.
  redaction_scan = list(
    where = "redactions", category = "redaction_unverified", severity = "high",
    how_to_fix = "The tool could not confirm that text hidden under a redaction box stayed hidden on every page. Do NOT release this output: check those pages against the source PDF first. A working image rasteriser (see the offline prereqs) removes this warning."))

.KPI_DIAGNOSIS_FALLBACK <- list(
  where = "check", category = "reconciliation_mismatch", severity = "medium",
  how_to_fix = "Review this check against the source statement.")

# Compact a set of row indices for display.
.rng <- function(idx) {
  if (!length(idx)) return("")
  if (length(idx) <= 8) return(paste(idx, collapse = ","))
  paste0(paste(utils::head(idx, 8), collapse = ","), ",... (", length(idx), " total)")
}

build_diagnostics <- function(status, messages = character(0), det = NULL,
                              parsed = NULL, recon = NULL, metadata = NULL) {
  rows <- list()
  raised <- character(0)          # categories already reported, so nothing is said twice
  add <- function(where, category, severity, detail, how_to_fix) {
    raised <<- c(raised, category)
    rows[[length(rows) + 1L]] <<- .diag_row(where, category, severity, detail, how_to_fix)
  }

  # A SCAN WE COULD NOT MACHINE-READ is not an unknown layout, and must never be
  # reported as one. With no text and no word boxes every template scores 0, so the
  # generic "no template matched -- closest X, missing 'TransactionDate'" message is
  # actively misleading: it sends the analyst to build a template for a page that has
  # no readable text, which can never work. Say what actually happened, and name the
  # cause -- missing OCR tooling is an ADMIN fix, poor scan quality is not.
  no_ocr <- suppressWarnings(as.integer(metadata$scanned_no_ocr %||% 0L))
  if (identical(status, "unsupported") && !is.na(no_ocr) && no_ocr > 0) {
    tools_missing <- !isTRUE(metadata$ocr_tools %||% TRUE)
    add("file", "scanned_no_ocr", "high",
        sprintf("%d page(s) are a scan (an image, with no text layer), so there was nothing to read.", no_ocr),
        if (tools_missing)
          paste("This machine has no OCR software installed, so scanned statements cannot be read at all.",
                "Ask whoever set the tool up to install Tesseract and Poppler (they ship in the offline",
                "bundle under offline/prereqs). Building a template will NOT help until that is done.")
        else
          paste("The scan quality was too low to read. Try a cleaner copy - scan at 300 dpi or higher,",
                "straight, in good contrast - or ask the bank for a digital (text) PDF."))
  } else if (identical(status, "unsupported") && !is.null(metadata$matched_empty)) {
    # The wording matched, the layout did not. "Add a template" is not the fix --
    # there IS one, its columns just sit in the wrong place. Send the analyst to
    # the template that failed, not to a blank form.
    add("template", "matched_but_empty", "high",
        sprintf("%s matches the wording on this statement but read no transactions from it",
                metadata$matched_empty),
        paste("Open this statement in the template toolkit with that template and check where the",
              "columns sit - the usual cause is a column band drawn in the wrong place, or over",
              "the wrong part of the page. The X-ray view shows what the tool saw."))
  } else if (identical(status, "unsupported")) {
    # A TIE is not an unknown layout, and "go and add a template" is the wrong
    # instruction for it: templates that already fit scored the same, so a new one
    # would simply tie as well and the next conversion would be no better. Same
    # status, opposite fix -- so say the opposite thing.
    if (detect_ambiguous(det)) {
      tied <- detect_tied_ids(det)
      add("detection", "ambiguous_template", "high",
          sprintf("%s templates fit this statement equally well: %s",
                  length(tied), paste(tied, collapse = ", ")),
          paste("Pick one on the Convert page and it converts straight away.",
                "If these are near-duplicates of each other, ask whoever looks after the tool",
                "to merge or retire one, and the question stops being asked."))
    } else {
      add("detection", "unknown_format", "high",
          det$detail %||% "no template matched this file",
          paste("Add a template for this layout in the template toolkit (Add a template tab:",
                "upload a sample and confirm what it detects). The closest match and the missing columns are in the detail."))
    }
  } else if (identical(status, "failed")) {
    add("file", "unreadable", "high",
        paste(messages, collapse = " "),
        paste("Check the file opens, is the expected type (CSV / PDF / Excel),",
              "and is not password-protected or corrupt."))
  }

  if (!is.null(metadata)) {
    # A different template had to step in because the first one read nothing. Say
    # so, or the broken template stays broken: nobody knows it failed, because the
    # conversion appeared to work.
    if (!is.null(metadata$rescued_from) && !is.na(metadata$rescued_from))
      add("template", "read_by_another_template", "medium",
          sprintf("%s matched the wording first but read no transactions, so %s was used instead",
                  metadata$rescued_from, metadata$template$id %||% "another template"),
          paste("Check the figures are right. Then get the template that read nothing looked at -",
                "its columns are in the wrong place, and it will keep winning the match on",
                "statements like this one."))
    if (isTRUE(metadata$multi$likely_multiple))
      add("upload", "multiple_statements", "high",
          paste(metadata$multi$reasons, collapse = "; "),
          "This upload looks like more than one statement bundled together, which corrupts a single parse. Split it into one statement per file and re-run.")
    else if (isTRUE(metadata$multi$combined_accounts))
      add("upload", "combined_statement", "info",
          sprintf("%d account numbers appear in one statement period", metadata$multi$n_accounts %||% 0L),
          "Looks like a combined statement (several accounts/products, or transfer counterparties named in transactions). If transactions from more than one account are mixed, running balances won't be continuous across them - review per account.")
    p <- suppressWarnings(as.integer(metadata$pages %||% NA))
    if (!is.na(p) && p > PARAM_MAX_PAGES)
      add("upload", "oversized", "medium",
          sprintf("%d pages in one file", p),
          "Very long PDFs (>100 pages) may hit tool limits; split into smaller files if extraction stalls.")
    mp <- suppressWarnings(as.numeric(metadata$max_page_pt %||% NA))
    if (!is.na(mp) && mp > PARAM_MAX_PAGE_PT)
      add("upload", "oversized_page", "medium",
          sprintf("largest page is %.0f pt (> 2880 pt / 40 in)", mp),
          "Pages larger than 40 inches (2880 pt) can break rendering/OCR. Re-export at a standard page size.")
  }

  # PDF document provenance. Raised for EVERY status (a file that didn't even match
  # a template is exactly when "what wrote this?" is worth knowing), and only when
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

  if (!is.null(parsed) && !is.null(parsed$transactions)) {
    tx <- parsed$transactions

    # 0. effective_from / effective_to (SOFT signal, P2-9): if the matched template
    # declares a validity window and this statement's period falls outside it, the
    # bank may have changed the format. Never a hard filter -- the statement still
    # parses; this is only a caution so an outdated-format template that matches
    # newer statements is visible, not silently trusted.
    tmpl <- metadata$template
    if (!is.null(tmpl)) {
      eff_from <- .tolerant_date(tmpl$effective_from); eff_to <- .tolerant_date(tmpl$effective_to)
      pd <- c(.tolerant_date(parsed$header$period_start), .tolerant_date(parsed$header$period_end))
      pd <- pd[!is.na(pd)]
      if (length(pd) && (!is.na(eff_from) || !is.na(eff_to)) &&
          ((!is.na(eff_from) && any(pd < eff_from)) || (!is.na(eff_to) && any(pd > eff_to))))
        add("template period", "date_out_of_range", "medium",
          sprintf("the statement's dates fall outside this template's declared valid range (%s to %s)",
                  tmpl$effective_from %||% "any", tmpl$effective_to %||% "any"),
          "The bank may have changed this statement's format since this template was made. Check the columns still line up; if the layout differs, build an updated template.")
    }

    # 1. Failing reconciliation KPIs -> a fix per check.
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
    mal <- which(grepl("malformed", tx$flags %||% ""))
    if (length(mal)) add(sprintf("rows %s", .rng(mal)), "row_parse", "high",
      sprintf("%d row(s) had the wrong number of fields", length(mal)),
      "Wrong field count: check the delimiter/quoting, or a preamble/footer line was read as data.")

    dalt <- which(grepl("date_alt_format", tx$flags %||% ""))
    if (length(dalt)) add(sprintf("rows %s (date)", .rng(dalt)), "date_format_mismatch", "medium",
      sprintf("%d date(s) were written in a different style than the template declares", length(dalt)),
      "Rows like '17 Sep' were read with the year taken from the statement period. Update the template's date format in the toolkit to make this explicit.")

    dyi <- which(grepl("date_year_inferred", tx$flags %||% ""))
    if (length(dyi)) add(sprintf("rows %s (date)", .rng(dyi)), "date_out_of_range", "medium",
      sprintf("%d date(s) took their YEAR from a number in the page text, not a statement period", length(dyi)),
      "The statement showed day+month only and no readable period, so the year was inferred from a single 4-digit number on the page (which could be a footer/copyright year). Confirm the year is right, or add the statement period to the template.")

    dbad <- which(is.na(tx$date) & !is.na(tx$date_raw) & nzchar(tx$date_raw %||% ""))
    if (length(dbad)) add(sprintf("rows %s (date)", .rng(dbad)), "date_parse", "medium",
      sprintf("%d date(s) could not be read", length(dbad)),
      "The date-format mapping is likely wrong for these rows. Set the correct format in the template toolkit (e.g. day/month/year).")

    abad <- which(is.na(tx$amount) & !grepl("redacted", tx$flags %||% ""))
    if (length(abad)) add(sprintf("rows %s (amount)", .rng(abad)), "amount_parse", "high",
      sprintf("%d amount(s) could not be read", length(abad)),
      "The amount style/format is wrong: check the amount style (signed vs D/C vs debit/credit columns) and the thousands/decimal separators.")

    # 3. Informational context.
    cur <- unique(tx$currency[!is.na(tx$currency)])
    if (length(cur) > 1) add("currency", "mixed_currency", "info",
      sprintf("multiple currencies present: %s", paste(cur, collapse = ", ")),
      "Foreign-currency lines are present. Confirm downstream handling of non-base currencies.")

    red <- which(grepl("redacted", tx$flags %||% ""))
    if (length(red)) add(sprintf("rows %s", .rng(red)), "redaction", "info",
      sprintf("%d redacted value(s), kept as shown", length(red)),
      "Redactions are intentional; values are left as [REDACTED]. No action needed.")

    # The header signal, for callers that pass no `recon` (build_diagnostics is
    # called without one in places). When the redaction_scan KPI ran it has
    # ALREADY said this, in more detail, from the same header field -- and two
    # rows both headed "do NOT release this output" read as two separate leaks.
    # One fact, reported once, by whichever source got here first.
    rsi <- suppressWarnings(as.integer(parsed$header$redaction_scan_incomplete %||% NA))
    if (!is.na(rsi) && rsi > 0 && !("redaction_unverified" %in% raised))
      add("redactions", "redaction_unverified", "high",
          sprintf("%d page(s) could not be checked for hidden (drawn-over) text", rsi),
          "The tool could not rasterise these pages to confirm nothing is hidden under a drawn box. Treat their visible text with caution and check the pages against the original; a working image rasteriser removes this warning.")

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
    template = "You - adjust the template (toolkit)",
    input    = "You - fix the file (split / re-export / rescan)",
    review   = "You - review the data (expected, not an error)",
    none     = "No action",
    escalate = "Developer - engine gap (escalate)"
  )[owner])
}
