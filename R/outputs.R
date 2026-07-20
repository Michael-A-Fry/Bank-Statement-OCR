# outputs.R -- write xlsx (multi-sheet), csv (core table), json (full object).

# .header_df(header) -- header named list -> two-column field/value frame.
.header_df <- function(header) {
  data.frame(
    field = names(header),
    value = vapply(header, function(v)
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1],
      character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# .checks_df(recon) -- KPI rows plus trust summary rows.
.checks_df <- function(recon) {
  kpis <- recon$kpis
  trust <- recon$trust
  trust_rows <- data.frame(
    name = c("trust_level", "trust_score"),
    status = c("info", "info"),
    expected = c(NA, NA),
    actual = c(trust$level, as.character(trust$score)),
    discrepancy = c(NA, NA),
    detail = c(paste(trust$reasons, collapse = "; "), ""),
    stringsAsFactors = FALSE
  )
  rbind(kpis, trust_rows)
}

# Fixed epoch stamped into the workbook so the xlsx is byte-reproducible.
.XLSX_FIXED_TIMESTAMP <- "2001-01-01T00:00:00Z"

# .deterministic_core(core) -- rewrite the dcterms:created timestamp in the
# workbook core-properties XML to a constant. Robust to the tag being empty.
.deterministic_core <- function(core) {
  core <- as.character(core)[1]
  if (is.na(core) || !nzchar(core)) return(core)
  repl <- sprintf("<dcterms:created xsi:type=\"dcterms:W3CDTF\">%s</dcterms:created>",
                  .XLSX_FIXED_TIMESTAMP)
  gsub("<dcterms:created[^>]*>[^<]*</dcterms:created>", repl, core, perl = TRUE)
}

# .normalize_zip_timestamps(path) -- an xlsx is a ZIP; the container stamps each
# entry's DOS modification time (2-second granularity) with the wall clock, so
# identical content still yields differing bytes run-to-run. Walk the central
# directory + each local file header and pin every mod-time/mod-date field to a
# constant (1980-01-01 00:00), making the archive byte-reproducible. Pure base R
# (readBin/writeBin); no external zip tooling. No-op if the structure is not the
# expected single-segment ZIP (never throws).
.normalize_zip_timestamps <- function(path) {
  n <- file.info(path)$size
  if (is.na(n) || n < 22) return(invisible(FALSE))
  raw <- readBin(path, "raw", n = n)

  rd16 <- function(o) as.integer(raw[o + 1L]) + 256L * as.integer(raw[o + 2L])
  rd32 <- function(o) rd16(o) + 65536 * rd16(o + 2L)
  # Fixed DOS date = 1980-01-01 (year 0, month 1, day 1) -> 0x0021; time 0.
  set_dt <- function(o) {
    raw[o + 1L] <<- as.raw(0x00); raw[o + 2L] <<- as.raw(0x00)  # time
    raw[o + 3L] <<- as.raw(0x21); raw[o + 4L] <<- as.raw(0x00)  # date
  }
  sig <- function(o, b0, b1, b2, b3)
    identical(raw[o + 1:4], as.raw(c(b0, b1, b2, b3)))

  # Locate the End Of Central Directory record (PK\5\6), scanning from the end.
  eocd <- -1L
  lo <- max(0L, n - 22L - 65535L)
  for (o in seq.int(n - 22L, lo)) {
    if (sig(o, 0x50, 0x4b, 0x05, 0x06)) { eocd <- o; break }
  }
  if (eocd < 0L) return(invisible(FALSE))

  n_entries <- rd16(eocd + 10L)
  cd_off <- rd32(eocd + 16L)
  if (cd_off < 0 || cd_off >= n) return(invisible(FALSE))

  p <- cd_off
  for (i in seq_len(n_entries)) {
    if (p + 46L > n || !sig(p, 0x50, 0x4b, 0x01, 0x02)) return(invisible(FALSE))
    set_dt(p + 12L)                       # central-dir entry time/date
    fn_len <- rd16(p + 28L); ex_len <- rd16(p + 30L); cm_len <- rd16(p + 32L)
    lho <- rd32(p + 42L)                  # local header offset
    if (lho >= 0 && lho + 12L <= n && sig(lho, 0x50, 0x4b, 0x03, 0x04)) {
      set_dt(lho + 10L)                   # local file header time/date
    }
    p <- p + 46L + fn_len + ex_len + cm_len
  }
  writeBin(raw, path)
  invisible(TRUE)
}

# .neutralize_formula(v) -- defend the SPREADSHEET outputs (xlsx/csv) against
# formula injection: a merchant/description beginning with = + @ (or a leading
# tab/CR) executes as a formula when opened in Excel. Prefix those with a single
# quote so Excel shows the literal text. Characters are preserved (nothing is
# stripped); only a display-safety marker is added, and ONLY for spreadsheets --
# the JSON output stays byte-for-byte verbatim (it is never executed). Applied to
# free-text columns only, so numeric-looking raw fields are untouched.
.SS_TEXT_COLS <- c("description", "particulars", "code", "reference",
                   "other_party", "type", "raw")
.neutralize_formula <- function(v) {
  v <- as.character(v)
  hit <- !is.na(v) & grepl("^[=+@\t\r]", v)
  v[hit] <- paste0("'", v[hit])
  v
}
.spreadsheet_safe <- function(df) {
  for (col in intersect(.SS_TEXT_COLS, names(df)))
    df[[col]] <- .neutralize_formula(df[[col]])
  df
}

# write_outputs(parsed, recon, outdir, basename, formats) -> named path vector.
write_outputs <- function(parsed, recon, outdir, basename,
                          formats = c("xlsx", "csv", "json"),
                          diagnostics = NULL, metadata = NULL) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)

  if ("xlsx" %in% formats) {
    xlsx_path <- file.path(outdir, paste0(basename, ".xlsx"))
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Transactions")
    openxlsx::writeData(wb, "Transactions", .spreadsheet_safe(parsed$transactions))
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", .header_df(parsed$header))
    openxlsx::addWorksheet(wb, "Checks")
    openxlsx::writeData(wb, "Checks", .checks_df(recon))
    openxlsx::addWorksheet(wb, "Provenance")
    openxlsx::writeData(wb, "Provenance", .spreadsheet_safe(parsed$provenance))
    if (!is.null(diagnostics)) {
      openxlsx::addWorksheet(wb, "Diagnostics")
      openxlsx::writeData(wb, "Diagnostics", diagnostics)
    }
    if (!is.null(metadata)) {
      openxlsx::addWorksheet(wb, "Metadata")
      openxlsx::writeData(wb, "Metadata", metadata_df(metadata))
    }
    # Byte-reproducibility (guarantee 11.4): openxlsx stamps docProps/core.xml
    # with the wall-clock time, so identical input+template would otherwise yield
    # differing xlsx bytes. Pin the created timestamp to a fixed constant before
    # saving so the same input always produces the same file.
    wb$core <- .deterministic_core(wb$core)
    openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    safe(.normalize_zip_timestamps(xlsx_path))
    paths["xlsx"] <- xlsx_path
  }

  if ("csv" %in% formats) {
    csv_path <- file.path(outdir, paste0(basename, ".csv"))
    utils::write.csv(.spreadsheet_safe(parsed$transactions), csv_path, row.names = FALSE, na = "")
    paths["csv"] <- csv_path
  }

  if ("json" %in% formats) {
    json_path <- file.path(outdir, paste0(basename, ".json"))
    full <- list(
      header = parsed$header,
      transactions = parsed$transactions,
      extras = parsed$extras,
      provenance = parsed$provenance,
      kpis = recon$kpis,
      trust = recon$trust,
      diagnostics = diagnostics,
      metadata = metadata
    )
    writeLines(jsonlite::toJSON(full, dataframe = "rows", auto_unbox = TRUE,
                                na = "null", pretty = TRUE), json_path)
    paths["json"] <- json_path
  }

  paths
}
