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

# write_outputs(parsed, recon, outdir, basename, formats) -> named path vector.
write_outputs <- function(parsed, recon, outdir, basename,
                          formats = c("xlsx", "csv", "json")) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)

  if ("xlsx" %in% formats) {
    xlsx_path <- file.path(outdir, paste0(basename, ".xlsx"))
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Transactions")
    openxlsx::writeData(wb, "Transactions", parsed$transactions)
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::writeData(wb, "Summary", .header_df(parsed$header))
    openxlsx::addWorksheet(wb, "Checks")
    openxlsx::writeData(wb, "Checks", .checks_df(recon))
    openxlsx::addWorksheet(wb, "Provenance")
    openxlsx::writeData(wb, "Provenance", parsed$provenance)
    openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    paths["xlsx"] <- xlsx_path
  }

  if ("csv" %in% formats) {
    csv_path <- file.path(outdir, paste0(basename, ".csv"))
    utils::write.csv(parsed$transactions, csv_path, row.names = FALSE, na = "")
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
      trust = recon$trust
    )
    writeLines(jsonlite::toJSON(full, dataframe = "rows", auto_unbox = TRUE,
                                na = "null", pretty = TRUE), json_path)
    paths["json"] <- json_path
  }

  paths
}
