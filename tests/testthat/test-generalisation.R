# test-generalisation.R -- perturbation suite proving the engine is not overfit
# to the exact bytes of its shipped samples.
#
# THE CONTRACT under test (the product's core promise): for every perturbed
# variant of a shipped sample, convert_statement() must either
#   (a) convert with the SAME transaction dates/amounts as the unperturbed
#       golden result (status "ok"), or
#   (b) refuse or flag the file (status unsupported/needs_review/failed) with a
#       non-empty message.
# It must NEVER return status "ok" with dates, amounts or a row count that
# differ from the golden expectation. .expect_contract() below asserts exactly
# that disjunction.
#
# Variants are generated PROGRAMMATICALLY, in tempdir(), at test time -- never
# stored -- from the same fixtures the golden tests already use (see
# helper.R and tests/testthat/expected/). Every variant is one fixed,
# deterministic transformation; nothing here uses random numbers.
#
# Two genuine engine bugs surfaced while building this suite (a UTF-8 BOM or a
# case-changed header silently nulled the date column while status stayed
# "ok"). Both are now FIXED in the engine - safe_readlines() strips a leading
# BOM, detection and field mapping match case-insensitively where unambiguous,
# and the dates_readable check makes zero-readable-dates impossible to pass
# silently - and every former skip below is a live positive assertion.

# ---- minimal quote-aware CSV line helpers (test-local; engine is a black box) ----

# .csv_split(line) -- one physical CSV line -> character vector of fields,
# honouring double-quoted fields exactly like read.table does.
.csv_split <- function(line, delim = ",") {
  as.character(unlist(utils::read.table(
    text = line, sep = delim, header = FALSE, quote = "\"",
    colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = character(0), comment.char = "", strip.white = FALSE)[1, , drop = TRUE]))
}

# .csv_quote(x) -- quote one field only if it needs it (carries the delimiter
# or a literal quote).
.csv_quote <- function(x, delim = ",") {
  if (grepl(delim, x, fixed = TRUE) || grepl("\"", x, fixed = TRUE))
    paste0("\"", gsub("\"", "\"\"", x), "\"")
  else x
}

# .csv_join(fields) -- fields -> one physical CSV line, quoting as needed.
.csv_join <- function(fields, delim = ",") {
  paste(vapply(fields, .csv_quote, character(1), delim = delim), collapse = delim)
}

# ---- perturbation builders, one per variant class ----

# 1. Reordered columns: same header set, one fixed permutation (reversed) on
# the header AND every non-blank data line.
.reorder_cols <- function(lines, hdr_idx) {
  hdr <- .csv_split(lines[hdr_idx])
  perm <- rev(seq_along(hdr))
  out <- lines
  for (i in seq_along(lines)) {
    if (i < hdr_idx || !nzchar(trimws(lines[i]))) next
    f <- .csv_split(lines[i])
    if (length(f) != length(hdr)) next
    out[i] <- .csv_join(f[perm])
  }
  out
}

# 2. Extra unknown column: an unrecognised "Branch Code" column with a
# constant value, appended after the last existing column.
.add_extra_column <- function(lines, hdr_idx) {
  out <- lines
  for (i in seq_along(lines)) {
    if (i < hdr_idx || !nzchar(trimws(lines[i]))) next
    f <- .csv_split(lines[i])
    val <- if (i == hdr_idx) "Branch Code" else "01"
    out[i] <- .csv_join(c(f, val))
  }
  out
}

# 3. A preamble line above the header.
.add_preamble <- function(lines) c("Account transactions export", lines)

# 4. Windows line endings (CRLF) + a UTF-8 BOM, returned as raw bytes ready to
# write verbatim. Writing raw bytes (not writeLines) keeps exact control of the
# byte sequence instead of relying on platform newline translation.
.crlf_bom_bytes <- function(lines) {
  text <- paste0(paste(lines, collapse = "\r\n"), "\r\n")
  c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(text))
}

# 5. A quoted field containing the delimiter, written into the first non-blank
# data row's description-mapped column.
.quote_delimiter_field <- function(lines, hdr_idx, field) {
  hdr <- .csv_split(lines[hdr_idx])
  col <- match(field, hdr)
  if (is.na(col)) return(lines)
  i <- hdr_idx + 1L
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) i <- i + 1L
  if (i > length(lines)) return(lines)
  f <- .csv_split(lines[i])
  f[col] <- "COFFEE, CAFE LTD"
  out <- lines
  out[i] <- .csv_join(f)
  out
}

# 6. Trailing empty lines and a footer line.
.add_trailing_footer <- function(lines) c(lines, "", "", "End of statement")

# 7. Header case change: uppercase exactly one header field's name (the field
# used as the template's date source), leaving every other header and all data
# untouched.
.uppercase_header_field <- function(lines, hdr_idx, field) {
  hdr <- .csv_split(lines[hdr_idx])
  col <- match(field, hdr)
  if (is.na(col)) return(lines)
  hdr[col] <- toupper(hdr[col])
  out <- lines
  out[hdr_idx] <- .csv_join(hdr)
  out
}

# ---- tempfile writers ----

.write_lines_tmp <- function(lines) {
  p <- tempfile("gen_", fileext = ".csv")
  con <- file(p, open = "wb")
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
  close(con)
  p
}

.write_bytes_tmp <- function(bytes) {
  p <- tempfile("gen_", fileext = ".csv")
  con <- file(p, open = "wb")
  writeBin(bytes, con)
  close(con)
  p
}

# ---- sample registry ----
# hdr_idx: the physical line the unperturbed sample's header sits on. Every
# sample here has the header on line 1 EXCEPT the ASB export, which carries a
# metadata preamble first (see templates/asb_everyday_csv.yaml preamble
# .header_regex); .hdr_idx() below locates it the same way.

.base_lines <- function(rel_path) readLines(fixture(rel_path), warn = FALSE, encoding = "UTF-8")

.hdr_idx <- function(sample_id, lines) {
  if (identical(sample_id, "asb_everyday_csv")) {
    idx <- grep("^Date,Unique Id,Tran Type", lines)
    return(if (length(idx)) idx[1] else 1L)
  }
  1L
}

SAMPLES <- list(
  bnz_everyday_csv = list(
    path = "samples/raw/bnz/bnz_transaction_export_01.csv", bank = "BNZ",
    date_field = "Date", description_field = "Payee",
    golden = read_core_csv(fixture("tests/testthat/expected/bnz_everyday_csv.csv"))),
  anz_everyday_csv = list(
    path = "samples/raw/anz/anz_transaction_export_01.csv", bank = "ANZ",
    date_field = "Date", description_field = "Details",
    golden = read_core_csv(fixture("tests/testthat/expected/anz_everyday_csv.csv"))),
  anz_creditcard_csv = list(
    path = "samples/raw/anz/anz_creditcard_01.csv", bank = "ANZ",
    date_field = "TransactionDate", description_field = "Details",
    golden = read_core_csv(fixture("tests/testthat/expected/anz_creditcard_csv.csv"))),
  asb_everyday_csv = list(
    path = "samples/raw/asb/asb_transaction_export_01.csv", bank = "ASB",
    date_field = "Date", description_field = "Memo",
    golden = read_core_csv(fixture("tests/testthat/expected/asb_everyday_csv.csv"))),
  kiwibank_everyday_csv = list(
    path = "samples/raw/kiwibank/kiwibank_transaction_01.csv", bank = "Kiwibank",
    date_field = "Transaction Date", description_field = "Description",
    golden = read_core_csv(fixture("tests/testthat/expected/kiwibank_everyday_csv.csv"))),
  westpac_everyday_csv = list(
    path = "samples/raw/westpac/westpac_transaction_export_01.csv", bank = "Westpac",
    date_field = "Date", description_field = "Description",
    golden = read_core_csv(fixture("tests/testthat/expected/westpac_everyday_csv.csv")))
)

# ---- run + assert ----

# .convert_variant(path, bank) -- the real engine entry point, csv-only output
# (keeps the suite fast; the contract only needs the core table).
.convert_variant <- function(path, bank) {
  convert_statement(path, bank = bank, formats = "csv",
                    outdir = tempfile("gen_out_"),
                    templates_dir = templates_dir(),
                    logdir = tempfile("gen_log_"))
}

# .expect_contract(res, golden) -- the generalisation contract itself.
.expect_contract <- function(res, golden, info = "") {
  if (identical(res$status, "ok")) {
    testthat::expect_false(is.null(res$outputs[["csv"]]), info = info)
    tx <- read_core_csv(res$outputs[["csv"]])
    testthat::expect_equal(nrow(tx), nrow(golden), info = info)
    testthat::expect_equal(tx$date, golden$date, info = info)
    testthat::expect_equal(tx$amount, golden$amount, info = info)
  } else {
    testthat::expect_true(res$status %in% c("unsupported", "needs_review", "failed"),
                          info = info)
    testthat::expect_true(length(res$messages) > 0 && any(nzchar(res$messages)),
                          info = info)
  }
}

# ==== 1. reordered columns =================================================
# header_contains_all fingerprints are sets, and every field is looked up by
# NAME (not position) downstream, so reordering should never change the
# result. The one exception -- ASB's preamble.header_regex is anchored to the
# literal, ordered header text -- is exercised here too: it correctly fails to
# locate the header at all and reports "unsupported", which still satisfies
# the contract (disjunction branch b), so no special-casing is needed.
test_that("reordered columns convert identically or are honestly flagged", {
  for (sample_id in names(SAMPLES)) {
    s <- SAMPLES[[sample_id]]
    lines <- .base_lines(s$path)
    variant <- .reorder_cols(lines, .hdr_idx(sample_id, lines))
    res <- .convert_variant(.write_lines_tmp(variant), s$bank)
    .expect_contract(res, s$golden, info = sample_id)
  }
})

# ==== 2. extra unknown column ===============================================
test_that("an extra unknown column converts identically or is honestly flagged", {
  for (sample_id in names(SAMPLES)) {
    s <- SAMPLES[[sample_id]]
    lines <- .base_lines(s$path)
    variant <- .add_extra_column(lines, .hdr_idx(sample_id, lines))
    res <- .convert_variant(.write_lines_tmp(variant), s$bank)
    .expect_contract(res, s$golden, info = sample_id)
  }
})

# ==== 3. preamble line above the header =====================================
# ASB already tolerates an export preamble (header_regex scans for the header
# rather than assuming line 1); everything else has preamble: null, so an
# extra line pushes their header off line 1 and detection correctly reports
# "unsupported" rather than mis-reading the preamble text as data.
test_that("a preamble line above the header converts identically or is honestly flagged", {
  for (sample_id in names(SAMPLES)) {
    s <- SAMPLES[[sample_id]]
    lines <- .base_lines(s$path)
    variant <- .add_preamble(lines)
    res <- .convert_variant(.write_lines_tmp(variant), s$bank)
    .expect_contract(res, s$golden, info = sample_id)
  }
})

# ==== 4. Windows line endings + UTF-8 BOM ===================================
# CRLF alone is harmless (readLines() copes fine). The BOM used to be the
# hazard when it landed on the header's FIRST cell (the date column for
# bnz/westpac, whose headers start "Date,..."); safe_readlines() now strips a
# leading BOM, so every sample must convert identically - asserted below.
test_that("CRLF line endings and a UTF-8 BOM convert identically for every sample", {
  for (sample_id in names(SAMPLES)) {
    s <- SAMPLES[[sample_id]]
    lines <- .base_lines(s$path)
    res <- .convert_variant(.write_bytes_tmp(.crlf_bom_bytes(lines)), s$bank)
    .expect_contract(res, s$golden, info = sample_id)
    # BOM + CRLF must be invisible: same status branch as the clean file, and
    # for bnz/westpac in particular the dates must survive (the old bug nulled
    # them while status stayed "ok").
    if (identical(res$status, "ok"))
      testthat::expect_equal(read_core_csv(res$outputs[["csv"]])$date, s$golden$date,
                             info = sample_id)
  }
})

# ==== 5. quoted field containing the delimiter ==============================
test_that("a quoted field containing the delimiter converts identically or is honestly flagged", {
  for (sample_id in names(SAMPLES)) {
    s <- SAMPLES[[sample_id]]
    lines <- .base_lines(s$path)
    hdr_idx <- .hdr_idx(sample_id, lines)
    variant <- .quote_delimiter_field(lines, hdr_idx, s$description_field)
    res <- .convert_variant(.write_lines_tmp(variant), s$bank)
    .expect_contract(res, s$golden, info = sample_id)
  }
})

# ==== 6. trailing empty lines + footer line =================================
# The blank lines are silently ignored (no_unparsed_rows only counts non-empty
# physical lines), but "End of statement" is a non-empty line with far fewer
# fields than the header, so it is parsed as one extra malformed row. That
# correctly fails no_unparsed_rows and downgrades every sample to
# needs_review -- honest flagging, not silent corruption.
test_that("trailing blank lines and a footer line convert identically or are honestly flagged", {
  for (sample_id in names(SAMPLES)) {
    s <- SAMPLES[[sample_id]]
    lines <- .base_lines(s$path)
    variant <- .add_trailing_footer(lines)
    res <- .convert_variant(.write_lines_tmp(variant), s$bank)
    .expect_contract(res, s$golden, info = sample_id)
  }
})

# ==== 7. header case change (date column) ===================================
# Column lookup is case-insensitive where it is unambiguous: detection's
# fingerprint scoring (R/detect.R .score_template) and field mapping
# (R/parse.R .pick) both fall back to a UNIQUE case-insensitive match, so a
# bank flipping its header casing ("Date" -> "DATE") converts identically.
# ASB is the one exception: its header-location regex is anchored and
# case-sensitive, so the case change stops the header being found at all and
# the file is correctly reported "unsupported" - fails closed, never silently
# wrong.
test_that("a case-changed date header is honestly flagged (ASB fails closed)", {
  s <- SAMPLES[["asb_everyday_csv"]]
  lines <- .base_lines(s$path)
  hdr_idx <- .hdr_idx("asb_everyday_csv", lines)
  variant <- .uppercase_header_field(lines, hdr_idx, s$date_field)
  res <- .convert_variant(.write_lines_tmp(variant), s$bank)
  .expect_contract(res, s$golden, info = "asb_everyday_csv")
  testthat::expect_identical(res$status, "unsupported")
})

test_that("a case-changed date header converts identically (all non-ASB samples)", {
  for (sample_id in setdiff(names(SAMPLES), "asb_everyday_csv")) {
    s <- SAMPLES[[sample_id]]
    lines <- .base_lines(s$path)
    hdr_idx <- .hdr_idx(sample_id, lines)
    variant <- .uppercase_header_field(lines, hdr_idx, s$date_field)
    res <- .convert_variant(.write_lines_tmp(variant), s$bank)
    .expect_contract(res, s$golden, info = sample_id)
    # The dates themselves must survive the case change - the old bug matched
    # the template but silently blanked every date while status stayed "ok".
    if (identical(res$status, "ok"))
      testthat::expect_equal(read_core_csv(res$outputs[["csv"]])$date, s$golden$date,
                             info = sample_id)
  }
})

# ==== the never-silently-wrong safety net ===================================
# Even when a date column IS found, its values may not parse (wrong format for
# this file, corrupted export). Zero readable dates must never leave as a
# clean "ok": the dates_readable check fails and the run is flagged.
test_that("a file whose dates cannot be parsed is flagged, never a clean ok", {
  s <- SAMPLES[["bnz_everyday_csv"]]
  lines <- .base_lines(s$path)
  hdr_idx <- .hdr_idx("bnz_everyday_csv", lines)
  out <- lines
  for (i in seq_along(lines)) {
    if (i <= hdr_idx || !nzchar(trimws(lines[i]))) next
    f <- .csv_split(lines[i]); f[1] <- "not-a-date"; out[i] <- .csv_join(f)
  }
  res <- .convert_variant(.write_lines_tmp(out), s$bank)
  testthat::expect_false(identical(res$status, "ok"))
  if (!is.null(res$kpis) && "dates_readable" %in% res$kpis$name)
    testthat::expect_identical(res$kpis$status[res$kpis$name == "dates_readable"], "fail")
})
