# Sheet-aware Excel drafting: read_excel_input picks the right sheet, skips
# preamble rows, fixes serial dates; .draft_excel maps the columns. Fixtures are
# generated deterministically at test time (openxlsx), never stored.

skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not installed")
skip_if_not(requireNamespace("readxl", quietly = TRUE), "readxl not installed")

.mk <- function(name) file.path(tempdir(), name)

# (a) clean single sheet, header row 1, text dates
.clean_xlsx <- function() {
  p <- .mk("gx_clean.xlsx")
  df <- data.frame(Date = c("05/01/2026", "06/01/2026", "07/01/2026"),
                   Description = c("EFTPOS COFFEE", "SALARY", "POWER CO"),
                   Amount = c("-4.50", "1500.00", "-180.20"),
                   Balance = c("995.50", "2495.50", "2315.30"),
                   stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, p, overwrite = TRUE)
  p
}

# (b) preamble junk + serial dates + a subtotal row + an empty spacer row
.messy_xlsx <- function() {
  p <- .mk("gx_messy.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Export")
  openxlsx::writeData(wb, "Export", "Kowhai Bank NZ", startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Export", "Account 12-3456-7890123-00", startRow = 2, colNames = FALSE)
  openxlsx::writeData(wb, "Export",
    data.frame(Date = c(46027, 46028, NA, 46029),         # 2026-01-05..07 as serials
               Description = c("EFTPOS COFFEE", "SALARY", "Subtotal", "POWER CO"),
               Amount = c(-4.5, 1500, NA, -180.2),
               stringsAsFactors = FALSE),
    startRow = 4)
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  p
}

# (c) multi-sheet: sheet 1 is notes, sheet 2 holds the table
.multi_xlsx <- function() {
  p <- .mk("gx_multi.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Notes")
  openxlsx::writeData(wb, "Notes", "Prepared for the file. See next sheet.", colNames = FALSE)
  openxlsx::addWorksheet(wb, "Transactions")
  openxlsx::writeData(wb, "Transactions",
    data.frame(Date = c("05/01/2026", "06/01/2026"),
               Details = c("EFTPOS COFFEE", "SALARY"),
               Amount = c("-4.50", "1500.00"), stringsAsFactors = FALSE))
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
  p
}

# (d) no transaction table at all
.notable_xlsx <- function() {
  p <- .mk("gx_none.xlsx")
  openxlsx::write.xlsx(data.frame(Note = c("hello", "world"), stringsAsFactors = FALSE),
                       p, overwrite = TRUE)
  p
}

test_that("clean single-sheet workbook reads with header row 1 (old behaviour)", {
  inp <- read_input(.clean_xlsx())
  expect_identical(inp$kind, "excel")
  expect_setequal(names(inp$table), c("Date", "Description", "Amount", "Balance"))
  expect_equal(nrow(inp$table), 3)
})

test_that("preamble rows are skipped, serial dates become ISO, spacers dropped", {
  inp <- read_input(.messy_xlsx())
  t <- inp$table
  expect_setequal(names(t), c("Date", "Description", "Amount"))
  expect_equal(t$Date[1], "2026-01-05")
  expect_equal(t$Date[2], "2026-01-06")
  expect_true("Subtotal" %in% t$Description)   # kept in the table; the DATE GATE drops it at parse
})

test_that("the sheet holding the transaction table is chosen, not sheet 1", {
  inp <- read_input(.multi_xlsx())
  expect_setequal(names(inp$table), c("Date", "Details", "Amount"))
})

test_that("excel drafting round-trips: draft -> validate -> preview, subtotal dropped", {
  p <- .messy_xlsx()
  tmpl <- draft_template(p, bank = "Kowhai")
  expect_false(is.null(tmpl))
  expect_identical(tmpl$format, "excel")
  expect_identical(tmpl$columns$date$source, "Date")
  expect_length(validate_template(tmpl), 0)
  tx <- draft_preview(p, tmpl)
  expect_false(is.null(tx))
  # The dateless subtotal row is kept-and-flagged (date NA, no amount), never a
  # silent number: the engine's keep + flag philosophy, same as delimited.
  expect_equal(nrow(tx), 4)
  expect_equal(sum(!is.na(tx$date)), 3)
  sub <- tx[grepl("Subtotal", tx$description %||% ""), , drop = FALSE]
  expect_true(all(is.na(sub$date)))
  expect_equal(sort(tx$date[!is.na(tx$date)]),
               c("2026-01-05", "2026-01-06", "2026-01-07"))
})

test_that("a workbook with no transaction table drafts to NULL, honestly", {
  expect_null(draft_template(.notable_xlsx(), bank = "X"))
})
