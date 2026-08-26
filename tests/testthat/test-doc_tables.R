# test-doc_tables.R -- the multi-table DOCUMENT extractor (R/tables.R,
# R/tables_detect.R, R/doc_extract.R).
#
# The document under test is built as word geometry, not rendered as a PDF -- see
# helper-doc.R for why. Every assertion below is about a shape the real reports
# have, and each one is named after the mistake it exists to catch.

context_input <- function() doc_fixture_input()

# ---------------------------------------------------------------------------
# Row grouping: the line pitch is LEARNED, not assumed
# ---------------------------------------------------------------------------

test_that("the row tolerance is learned from the page's own line pitch", {
  expect_equal(.doc_row_tol(c(100, 118, 136, 154)), 18 * 0.6)
  # A widely-leaded title block must not produce a tolerance that swallows the
  # tighter pair inside it: capped at 14 (see .doc_row_tol).
  expect_equal(.doc_row_tol(c(60, 100, 140, 180, 220)), 14)
  # Too little to measure -> the proven statement tolerance, never a guess.
  expect_equal(.doc_row_tol(c(100, 118)), as.numeric(PARAM_PDF_ROW_TOL))
  # A declared tolerance always wins.
  expect_equal(.doc_row_tol(c(100, 118, 136), declared = 5), 5)
})

test_that("a label and the value printed 22pt under it stay two lines", {
  inp <- context_input()
  lines <- .doc_lines(.doc_page_words(inp, 1))
  expect_equal(length(lines), 8L)
  txt <- vapply(lines, .doc_line_text, character(1))
  expect_true(any(grepl("^Client reference$", txt)))
  expect_true(any(grepl("^NW-99-9999-9999$", txt)))
})

# ---------------------------------------------------------------------------
# Locating a table: text first, position second
# ---------------------------------------------------------------------------

test_that("a table is found by its heading, and says so", {
  inp <- context_input(); tm <- doc_fixture_template()
  loc <- doc_locate_table(inp, tm$tables$account_summary, tm)
  expect_identical(loc$anchor, "header")
  expect_equal(loc$confidence, 1)
  expect_identical(loc$pages, 2L)
  expect_equal(loc$windows[[1]]$y_min, 100)
  expect_equal(loc$windows[[1]]$skip, 1L)
})

test_that("a table that MOVED down the page is still found by its heading", {
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$start$page <- 7L; tab$end$page <- 7L     # same y as page 2: 98 -- and stale
  loc <- doc_locate_table(inp, tab, tm)
  expect_identical(loc$anchor, "header")
  expect_equal(loc$windows[[1]]$y_min, 140)    # not the declared 98
  r <- doc_table_rows(inp, tab, tm, loc)
  expect_equal(r$n_rows, 2L)
  expect_identical(r$rows$Type, c("Transaction", "Savings"))
})

test_that("with nothing it was told to look for on the page, it says the table is not here", {
  # WAS: this asserted the position fall-through, and its own comment described
  # what that produced -- the title line and the header row quietly read as data,
  # three rows where the table has two. That is the cardinal failure, and the
  # register (H1) asks for the fifth outcome instead: a table found ONLY by
  # position, when the template DID say what to look for, is refused.
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$start$page <- 7L; tab$end$page <- 7L
  tab$anchor$header_text <- list("Nothing", "Like", "This", "Wording")
  tab$anchor$first_column <- list("NoSuchLabel")
  tab$doc_pages <- 7L
  loc <- doc_locate_table(inp, tab, tm)
  expect_identical(loc$anchor, "not_found")
  expect_equal(loc$confidence, 0)
  expect_length(loc$windows, 0L)
  expect_match(loc$detail, "not found on this document", fixed = TRUE)
  r <- doc_table_rows(inp, tab, tm, loc)
  expect_equal(r$n_rows, 0L)
})

test_that("the first-column labels find a table whose heading changed", {
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$anchor$header_text <- list("Wording", "That", "Is", "Gone")
  loc <- doc_locate_table(inp, tab, tm)
  expect_identical(loc$anchor, "first_column")
  expect_equal(loc$confidence, 0.7)
})

# ---------------------------------------------------------------------------
# Reading the rows
# ---------------------------------------------------------------------------

test_that("a one-page table reads every row and no header", {
  inp <- context_input(); tm <- doc_fixture_template()
  r <- doc_table_rows(inp, tm$tables$account_summary, tm)
  expect_equal(r$n_rows, 4L)
  expect_identical(names(r$rows)[1:2], c("page", "row"))
  expect_true(all(c("Account", "Type", "Opening", "Closing") %in% names(r$rows)))
  expect_identical(r$rows$Account[1], "Everyday 99-9999-9999999-99")
  expect_identical(r$rows$Opening[2], "15,000.00")
  expect_identical(r$rows$Closing[4], "-410.05")
  expect_false("Account" %in% r$rows$Account)      # the header is not a row
  expect_equal(nrow(r$spilled), 0L)                # nothing fell outside a column
})

test_that("a declared column type adds a parsed value WITHOUT replacing the text", {
  inp <- context_input(); tm <- doc_fixture_template()
  r <- doc_table_rows(inp, tm$tables$account_summary, tm)
  expect_true("Opening__value" %in% names(r$rows))
  expect_identical(r$rows$Opening[1], "1,240.55")       # what the page says
  # WAS: expect_identical(r$rows$Opening__value[1], "1,240.55") -- the companion
  # was the matched SUBSTRING, not a value, so "(1,234.56)" stayed "(1,234.56)"
  # and no sign was ever resolved (register I5). It is a signed number now.
  expect_identical(r$rows$Opening__value[1], 1240.55)
  expect_identical(r$rows$Closing__value[4], -410.05)   # printed "-410.05"
  # An "auto" column has no companion: nothing is inferred from a column whose
  # kind nobody declared.
  tab <- tm$tables$account_summary
  tab$columns[[3]]$type <- "auto"
  r2 <- doc_table_rows(inp, tab, tm)
  expect_false("Opening__value" %in% names(r2$rows))
  expect_identical(r2$rows$Opening[1], "1,240.55")
})

test_that("a table spanning three pages reads them all and drops every repeat of its header", {
  inp <- context_input(); tm <- doc_fixture_template()
  r <- doc_table_rows(inp, tm$tables$transactions, tm)
  expect_equal(r$n_rows, 83L)                      # 28 + 44 + 11
  expect_identical(sort(unique(r$rows$page)), 3:5)
  expect_equal(sum(r$rows$page == 3L), 28L)
  expect_equal(sum(r$rows$page == 4L), 44L)
  expect_equal(sum(r$rows$page == 5L), 11L)
  expect_false(any(r$rows$Description %in% "Description"))
  expect_equal(r$n_header, 3L)                     # one per page
  # The narrative above the table on page 3 is NOT in it: the table starts where
  # it starts, and the words above that boundary are simply not read.
  expect_false(any(grepl("Narrative", r$rows$Description)))
  # ...and neither is the "Fees charged" table that follows it on page 5.
  expect_false(any(r$rows$Description %in% "per month"))
})

test_that("a total row with blanks in the middle is kept, and blank columns are not flagged", {
  inp <- context_input(); tm <- doc_fixture_template()
  r <- doc_table_rows(inp, tm$tables$position, tm)
  expect_equal(r$n_rows, 6L)                       # 5 detail rows + the closing row
  last <- r$rows[r$rows$Item == "ClosingBalance", , drop = FALSE]
  expect_equal(nrow(last), 1L)
  expect_identical(last$Total, "1,810.20")
  expect_true(is.na(last$M1) || !nzchar(last$M1))  # empty ON PURPOSE
  # may_be_blank exempts a column from the fill threshold: 5 of 6 is 0.83, above
  # the 0.5 default anyway, so make the point on a column that really is thin.
  expect_false(any(r$report$low_fill))
  expect_equal(r$report$fill_rate[r$report$column == "Total"], 1)
  expect_equal(r$report$fill_rate[r$report$column == "M1"], round(5 / 6, 3))
})

test_that("a column band drawn too narrow shows up as unclaimed words, not as a clean read", {
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$columns[[4]]$x_min <- 505      # the Closing band now misses every figure
  tab$columns[[4]]$x_max <- 520
  r <- doc_table_rows(inp, tab, tm)
  expect_equal(r$n_rows, 4L)
  expect_true(all(is.na(r$rows$Closing)))
  expect_equal(nrow(r$spilled), 4L)               # the four figures it lost
  expect_true(all(grepl("[0-9]", r$spilled$text)))
  cl <- r$report[r$report$column == "Closing", , drop = FALSE]
  expect_equal(cl$fill_rate, 0)
  expect_true(cl$low_fill)                        # and it is FLAGGED
})

test_that("two tables on one page do not read each other's rows", {
  inp <- context_input(); tm <- doc_fixture_template()
  fees <- list(name = "Fees charged",
    start = list(page = 5, y = 328), end = list(page = 5, y = 400),
    header_rows = 1L, follow = FALSE,
    anchor = list(header_text = list("Fee", "Basis", "Amount")),
    columns = list(list(name = "Fee", x_min = 30, x_max = 199),
                   list(name = "Basis", x_min = 199, x_max = 380),
                   list(name = "Amount", x_min = 380, x_max = 520)))
  r <- doc_table_rows(inp, fees, tm)
  expect_equal(r$n_rows, 3L)
  expect_identical(r$rows$Fee, c("AccountKeeping", "Transaction", "Overdraft"))
  expect_false(any(grepl("PAYMENT", r$rows$Basis)))     # not the table above it
  expect_false(any(r$rows$Fee %in% "Telephone"))        # not the table below it
})

test_that("a table longer on THIS document than on the example is followed, and it is recorded", {
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$transactions
  # Drawn as if it ended at the bottom of page 3 -- which is what a template built
  # on a shorter copy of the document would say.
  tab$end <- list(page = 3, y = 841.89)
  tab$follow <- TRUE
  loc <- doc_locate_table(inp, tab, tm)
  expect_equal(loc$extended_to, 5L)
  expect_identical(sort(unique(loc$pages)), 3:5)
  expect_true(grepl("carries on to page 5", loc$detail))
  r <- doc_table_rows(inp, tab, tm, loc)
  expect_equal(r$n_rows, 83L)
  # ...AND IT STOPS. Following to the bottom of page 5 swallowed the fee table and
  # the contact table: 93 rows, ten of them with an amount in the wrong column and
  # a date that was not a date, and nothing about that looks wrong afterwards.
  expect_equal(nrow(r$stops), 1L)
  expect_equal(r$stops$page, 5L)
  expect_true(grepl("stops on page 5", r$detail))
  expect_false(any(grepl("per month|Telephone", unlist(r$rows))))
  # follow: false is the opposite promise, and it is kept.
  tab$follow <- FALSE
  expect_equal(doc_locate_table(inp, tab, tm)$pages, 3L)
})

test_that("a big gap INSIDE a table does not end it when the row still fits", {
  # The stop rule needs both halves. A schedule that leaves whitespace before its
  # own total row is a shape these documents genuinely use, and cutting there
  # would drop the one row somebody is looking for.
  pages <- doc_fixture_pages()
  extra <- doc_page(doc_L(40, 700, "31/12/2025"), doc_L(110, 700, "TOTAL"),
                    doc_R(400, 700, "1,234.56"), doc_R(500, 700, "9,876.54"))
  pages[[5]] <- rbind(pages[[5]], extra)      # 700 is 460pt below the last row
  inp <- doc_input(pages)
  tm <- doc_fixture_template()
  tab <- tm$tables$transactions
  tab$end <- list(page = 5, y = 841.89)       # open-ended: the stop rule applies
  loc <- doc_locate_table(inp, tab, tm)
  r <- doc_table_rows(inp, tab, tm, loc)
  # It stops at "Fees charged" (one filled column, after a big gap) and NOT at the
  # total row, which fills all four despite the much bigger gap before it.
  expect_equal(nrow(r$stops), 1L)
  expect_false(any(r$rows$Description %in% "TOTAL"))   # stopped before reaching it
  # ...and with the fee/contact tables out of the way it IS kept.
  p5 <- pages[[5]]
  p5 <- p5[p5$y <= 240 | p5$y == 700, , drop = FALSE]
  pages[[5]] <- p5
  r2 <- doc_table_rows(doc_input(pages), tab, tm)
  expect_true(any(r2$rows$Description %in% "TOTAL"))
  expect_equal(nrow(r2$stops), 0L)
})

# ---------------------------------------------------------------------------
# Label / value pairs -- two boxes, an offset, and the wording as the anchor
# ---------------------------------------------------------------------------

test_that("all four arrangements of a label and its value are read", {
  inp <- context_input(); tm <- doc_fixture_template()
  p <- doc_pairs(inp, tm)
  expect_equal(nrow(p), 4L)
  expect_true(all(p$matched))
  expect_true(all(p$found_by == "its wording"))
  val <- function(k) p$value[p$pair == k]
  expect_identical(val("prepared_for"), "Ambrose Family Trust")   # value to the right
  expect_identical(val("client_reference"), "NW-99-9999-9999")    # value underneath
  expect_identical(val("generated"), "07/04/2025")                # a typed date
  expect_identical(val("total_contributions"), "1,240.55")        # value BEFORE its label
})

test_that("a pair follows its label when the label moves, and falls back to the box when the wording is gone", {
  inp <- context_input(); tm <- doc_fixture_template()
  # Move the whole front matter down 30pt: the drawn boxes are now all wrong.
  pages <- doc_fixture_pages()
  pages[[1]]$y <- pages[[1]]$y + 30
  moved <- doc_input(pages)
  p <- doc_pairs(moved, tm)
  expect_identical(p$value[p$pair == "prepared_for"], "Ambrose Family Trust")
  expect_identical(p$found_by[p$pair == "prepared_for"], "its wording")

  # Now take the wording away. The drawn box is all that is left, and the result
  # says so rather than pretending it found the label.
  tm2 <- tm
  tm2$pairs$prepared_for$label_text <- "A phrase that is not on this document"
  p2 <- doc_pairs(inp, tm2)
  expect_identical(p2$found_by[p2$pair == "prepared_for"], "its place on the page")
  expect_identical(p2$value[p2$pair == "prepared_for"], "Ambrose Family Trust")
})

# ---------------------------------------------------------------------------
# Auto-detection
# ---------------------------------------------------------------------------

test_that("the tables on the document are proposed, with a continuation joined into one", {
  inp <- context_input()
  props <- propose_tables(inp)
  expect_equal(length(props), 6L)
  nms <- vapply(props, function(p) p$name, character(1))
  expect_true("Account summary" %in% nms)
  expect_true("Fees charged" %in% nms)
  expect_true("Contact details" %in% nms)
  expect_true("Statement of position by period" %in% nms)
  expect_true("Account summary 1" %in% nms)        # the page-7 copy, made unique

  tx <- props[[which(vapply(props, function(p) p$start$page == 3L, logical(1)))]]
  expect_equal(tx$start$page, 3L)
  expect_equal(tx$end$page, 5L)                    # pages 3, 4 and 5 as ONE table
  expect_equal(tx$n_rows, 83L)
  expect_equal(length(tx$columns), 4L)
  expect_identical(vapply(tx$columns, function(cc) cc$name, character(1)),
                   c("Date", "Description", "Amount", "Balance"))
})

test_that("proposed column bands sit between the columns, not on top of them", {
  inp <- context_input()
  props <- propose_tables(inp, pages = 2L)
  expect_equal(length(props), 1L)
  cols <- props[[1]]$columns
  expect_equal(length(cols), 4L)
  x0 <- vapply(cols, function(cc) cc$x_min, numeric(1))
  x1 <- vapply(cols, function(cc) cc$x_max, numeric(1))
  expect_true(all(x1 > x0))
  expect_true(all(diff(x0) > 0))
  expect_equal(x1[-length(x1)], x0[-1])            # bands meet, never overlap
  # ...and reading the table through them gives back what is on the page.
  tab <- props[[1]]
  tab$anchor$header_text <- list("Account", "Type", "Opening", "Closing")
  r <- doc_table_rows(inp, tab, NULL)
  expect_equal(r$n_rows, 4L)
  expect_equal(nrow(r$spilled), 0L)
})

test_that("a sparse total row is reached for, not left behind", {
  inp <- context_input()
  props <- propose_tables(inp, pages = 6L)
  expect_equal(length(props), 1L)
  # The closing row has two cells where the rows above have seven, so it breaks
  # the run of tabular lines -- and it is the row somebody came for. The proposal
  # reaches down past the break for a line that still lands in the table's own
  # bands, so all six come out. (This used to stop at five and say so.)
  expect_equal(props[[1]]$n_rows, 6L)
  expect_equal(length(props[[1]]$columns), 7L)
  r <- doc_table_rows(inp, props[[1]], NULL)
  expect_equal(r$n_rows, 6L)
  expect_true("ClosingBalance" %in% r$rows$Item)   # the first COLUMN, not page/row
})

test_that("prose is not proposed as a table", {
  inp <- context_input()
  expect_equal(length(propose_tables(inp, pages = 1L)), 0L)
})

test_that("a table with no figures anywhere keeps its heading line as a row, visibly", {
  # There is no font information to go on, so "the first line has no figures and
  # the ones below do" is the only header signal there is -- and a table of names
  # and addresses satisfies neither half of it. The rule fails toward keeping the
  # row rather than deleting one, and the row is visible in the preview. "Header
  # lines" on the builder is what fixes it.
  inp <- context_input()
  props <- propose_tables(inp, pages = 5L)
  contacts <- Filter(function(p) identical(p$name, "Contact details"), props)
  expect_equal(length(contacts), 1L)
  expect_equal(contacts[[1]]$header_rows, 0L)
  expect_equal(contacts[[1]]$n_rows, 4L)          # the heading line is one of them
  # ...and setting header_rows to 1 takes it out again, with nothing else moved.
  tab <- contacts[[1]]; tab$header_rows <- 1L
  r <- doc_table_rows(inp, tab, NULL)
  expect_equal(r$n_rows, 3L)
  expect_false(any(vapply(r$rows, function(v) any(v %in% "Channel"), logical(1))))
})

test_that("label/value pairs are proposed, including the value-before-its-label one", {
  inp <- context_input()
  pr <- propose_pairs(inp, page = 1L)
  lab <- vapply(pr, function(p) p$label_text, character(1))
  expect_true("Prepared for" %in% lab)
  expect_true("Client reference" %in% lab)
  expect_true("Generated" %in% lab)
  expect_true("Total contributions" %in% lab)
  got <- pr[[which(lab == "Total contributions")]]
  expect_identical(got$read, "1,240.55")
  expect_identical(got$type, "money")
  # the value box is to the LEFT of the label box, which is the whole point
  expect_lt(got$value$x_min, got$label$x_min)
  expect_identical(pr[[which(lab == "Generated")]]$type, "date")
})

# ---------------------------------------------------------------------------
# The template: validation
# ---------------------------------------------------------------------------

test_that("the fixture template is valid", {
  expect_equal(validate_document_template(doc_fixture_template()), character(0))
})

test_that("overlapping column bands are rejected, because an overlap DELETES a column", {
  tm <- doc_fixture_template()
  tm$tables$account_summary$columns[[2]]$x_min <- 150   # now overlaps column 1
  p <- validate_document_template(tm)
  expect_true(any(grepl("overlap", p)))
})

test_that("a template that finds nothing, or ends before it starts, is refused", {
  tm <- doc_fixture_template()
  tm$tables <- list(); tm$pairs <- list()
  expect_true(any(grepl("finds nothing", validate_document_template(tm))))

  tm2 <- doc_fixture_template()
  tm2$tables$transactions$end$page <- 1L
  expect_true(any(grepl("before it starts", validate_document_template(tm2))))

  tm3 <- doc_fixture_template()
  tm3$tables$account_summary$columns <- list()
  expect_true(any(grepl("no columns", validate_document_template(tm3))))
})

test_that("a generic fingerprint is refused here exactly as it is everywhere else", {
  tm <- doc_fixture_template()
  tm$fingerprint$page_contains_all <- list("Balance", "Total")
  expect_true(length(validate_document_template(tm)) > 0L)
  tm$fingerprint$page_contains_all <- list()
  expect_true(any(grepl("required", validate_document_template(tm))))
})

test_that("two tables named the same thing are refused before they collide on one sheet", {
  tm <- doc_fixture_template()
  tm$tables$transactions$name <- "Account Summary"
  expect_true(any(grepl("called the same thing", validate_document_template(tm))))
})

test_that("a column with an unknown kind is refused", {
  tm <- doc_fixture_template()
  tm$tables$account_summary$columns[[1]]$type <- "currency"
  expect_true(any(grepl("not one of", validate_document_template(tm))))
})

# ---------------------------------------------------------------------------
# End to end
# ---------------------------------------------------------------------------

test_that("extracting the whole document gives every table, the pairs, and one honest summary", {
  inp <- context_input(); tm <- doc_fixture_template()
  ext <- extract_document(inp, tm)
  expect_identical(names(ext$tables), c("account_summary", "transactions", "position"))
  expect_equal(nrow(ext$pairs), 4L)

  s <- ext$summary
  expect_equal(nrow(s), 3L)
  expect_true(all(s$found_by == "its heading"))
  expect_equal(s$rows[s$table == "transactions"], 83L)
  expect_equal(s$unclaimed_words[s$table == "account_summary"], 0L)
  expect_identical(s$pages[s$table == "transactions"], "3, 4, 5")

  # the long form: one row per cell, tagged with its table and page
  lg <- ext$long
  expect_identical(names(lg),
                   c("table", "table_name", "page", "row", "column", "value"))
  expect_equal(nrow(lg), (4L + 83L + 6L) * 4L + 6L * 3L)   # 7-col table adds 3 more
  expect_true(all(c("account_summary", "transactions", "position") %in% lg$table))
  expect_identical(unique(lg$table_name[lg$table == "position"]),
                   "Statement of position")
  # the parsed "__value" companions are NOT duplicated into the long file
  expect_false(any(grepl("__value$", lg$column)))
})

test_that("a document template is detected by its identifying phrases, and a tie is not a match", {
  inp <- context_input(); tm <- doc_fixture_template()
  d <- detect_document_template(inp, list(northwind_position = tm))
  expect_true(d$matched)
  expect_identical(d$template_id, "northwind_position")

  tm2 <- tm; tm2$id <- "other_report"
  d2 <- detect_document_template(inp, list(a = tm, b = tm2))
  expect_false(d2$matched)
  expect_true(grepl("equally specific", d2$detail))

  tm3 <- tm; tm3$fingerprint$page_contains_all <- list("Not on this document at all")
  d3 <- detect_document_template(inp, list(a = tm3))
  expect_false(d3$matched)
})

test_that("a document template round-trips through save and load", {
  dir <- file.path(tempdir(), paste0("doctpl-", as.integer(Sys.time())))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  tm <- doc_fixture_template()
  p <- save_document_template(tm, dir)
  expect_true(file.exists(p))
  back <- load_document_templates(dir)
  expect_identical(names(back), "northwind_position")
  expect_equal(length(back$northwind_position$tables), 3L)
  expect_identical(document_template_ids(dir), "northwind_position")
  # and reading THROUGH the reloaded template gives the same rows
  r <- doc_table_rows(context_input(), back$northwind_position$tables$account_summary,
                      back$northwind_position)
  expect_equal(r$n_rows, 4L)
  expect_true(delete_document_template("northwind_position", dir))
  expect_identical(document_template_ids(dir), character(0))
})

test_that("an invalid document template is never written to disk", {
  dir <- file.path(tempdir(), paste0("doctpl-bad-", as.integer(Sys.time())))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  tm <- doc_fixture_template()
  tm$tables$account_summary$columns[[2]]$x_min <- 150
  expect_error(save_document_template(tm, dir), "not valid")
  expect_false(dir.exists(dir) && length(list.files(dir)) > 0L)
})

test_that("the outputs written are the readable form AND the loadable one", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not installed")
  dir <- file.path(tempdir(), paste0("docout-", as.integer(Sys.time())))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  ext <- extract_document(context_input(), doc_fixture_template())
  paths <- write_document_outputs(ext, dir, "sample")
  expect_true(any(grepl("\\.tables\\.xlsx$", paths)))
  expect_true(any(grepl("\\.tables-long\\.csv$", paths)))
  expect_true(any(grepl("\\.report\\.csv$", paths)))
  expect_true(all(file.exists(paths)))
  long <- utils::read.csv(paths[grepl("tables-long", paths)][1],
                          stringsAsFactors = FALSE, colClasses = "character")
  expect_true(all(c("table", "table_name", "page", "row", "column", "value") %in%
                    names(long)))
  sheets <- openxlsx::getSheetNames(paths[grepl("\\.xlsx$", paths)][1])
  expect_true("Account summary" %in% sheets)
  expect_true("Report" %in% sheets)
})

test_that("worksheet names are made legal and unique before the workbook is built", {
  tables <- list(
    a = list(name = "Fees: charged / accrued [2025]"),
    b = list(name = paste(rep("Long", 12), collapse = " ")),
    c = list(name = paste(rep("Long", 12), collapse = " ")))
  s <- .doc_sheet_names(tables)
  expect_false(any(grepl("[\\\\/?*:\\[\\]]", s)))
  expect_true(all(nchar(s) <= 31L))
  expect_equal(length(unique(s)), 3L)
})

# ---------------------------------------------------------------------------
# The one rule this whole mode hangs on
# ---------------------------------------------------------------------------

test_that("a tables result can never reach the Qlik feed", {
  cfg <- list(feed = list(enabled = TRUE, feed_dir = file.path(tempdir(), "nofeed")))
  res <- list(kind = "tables", status = "ok", run_id = "r1",
              header = list(source_sha256 = strrep("a", 64)))
  expect_null(write_feed(res, cfg))
  expect_false(dir.exists(cfg$feed$feed_dir))
})

# ---------------------------------------------------------------------------
# Columns as EDGES -- what the builder manipulates
# ---------------------------------------------------------------------------

test_that("a table's columns are its edges, and the edges rebuild the columns", {
  tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  e <- doc_column_edges(tab)
  expect_equal(e, c(30, 199, 325, 430, 510))
  cols <- doc_columns_from_edges(e, old = .doc_columns(tab))
  expect_equal(length(cols), 4L)
  # a column whose two edges did not move keeps everything it was given: its
  # name, its kind, and whether it may be blank
  expect_identical(vapply(cols, function(cc) cc$name, character(1)),
                   c("Account", "Type", "Opening", "Closing"))
  expect_identical(cols[[3]]$type, "money")
  # ...and the round trip is exact
  expect_equal(doc_column_edges(list(columns = cols)), e)
})

test_that("one click on the page means exactly one thing, four ways", {
  e <- c(30, 199, 325, 430, 510)
  # between two columns -> split
  expect_equal(doc_edge_click(e, 250), c(30, 199, 250, 325, 430, 510))
  # on a divider -> remove it, so the two columns become one
  expect_equal(doc_edge_click(e, 199), c(30, 325, 430, 510))
  expect_equal(doc_edge_click(e, 327), c(30, 199, 430, 510))   # within tolerance
  # outside the table -> the nearer outer edge comes out to meet the click
  expect_equal(doc_edge_click(e, 12), c(12, 199, 325, 430, 510))
  expect_equal(doc_edge_click(e, 560), c(30, 199, 325, 430, 560))
  # ON an outer edge -> move it, rather than making a three-point column beside it
  expect_equal(doc_edge_click(e, 33), c(33, 199, 325, 430, 510))
  expect_equal(doc_edge_click(e, 508), c(30, 199, 325, 430, 508))
  # nothing sensible to do -> nothing happens
  expect_equal(doc_edge_click(e, NA), e)
  expect_equal(doc_edge_click(c(40), 100), c(40))
})

test_that("splitting a column names both halves from the document's own header", {
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  expect_identical(doc_header_names(inp, tab, tm),
                   c("Account", "Type", "Opening", "Closing"))
  # merge Type into Account, and the surviving column is named from BOTH header
  # cells that now sit in it -- which is what the page says it is
  e <- doc_edge_click(doc_column_edges(tab), 199)
  expect_identical(doc_header_names(inp, tab, tm, e)[1], "Account Type")
})

test_that("the columns can be worked out from whatever is inside the table now", {
  inp <- context_input(); tm <- doc_fixture_template()
  bare <- tm$tables$account_summary
  bare$columns <- list()                       # no columns at all
  fit <- doc_fit_columns(inp, bare, tm)
  expect_equal(length(fit), 4L)
  expect_identical(vapply(fit, function(cc) cc$name, character(1)),
                   c("Account", "Type", "Opening", "Closing"))
  # the bands meet exactly, so a gap or an overlap is not expressible
  x0 <- vapply(fit, function(cc) cc$x_min, numeric(1))
  x1 <- vapply(fit, function(cc) cc$x_max, numeric(1))
  expect_equal(x1[-length(x1)], x0[-1])
  # and reading through them gives back what is on the page
  tab <- bare; tab$columns <- fit
  r <- doc_table_rows(inp, tab, tm)
  expect_equal(r$n_rows, 4L)
  expect_equal(nrow(r$spilled), 0L)
  expect_identical(r$rows$Closing[4], "-410.05")
  # a printed rule spans the whole width and would merge every band into one
  expect_equal(length(doc_fit_columns(hard_ruled(), propose_tables(hard_ruled())[[1]], NULL)), 3L)
})

test_that("a value with wording and no box at all is read, and does not crash the rest", {
  # `sp$label` PARTIAL-MATCHES `label_text` in R, so a pair described only by its
  # wording returned a character vector where a box was expected and took the
  # whole extraction down with "$ operator is invalid for atomic vectors". Every
  # typed value is exactly that shape.
  inp <- context_input(); tm <- doc_fixture_template()
  tm$pairs <- list(prepared_for = list(label_text = "Prepared for", type = "text"),
                   generated = list(label_text = "Generated", type = "date"))
  p <- doc_pairs(inp, tm)
  expect_equal(nrow(p), 2L)
  expect_identical(p$value[p$pair == "prepared_for"], "Ambrose Family Trust")
  expect_identical(p$value[p$pair == "generated"], "07/04/2025")
  expect_true(all(p$found_by == "its wording"))
  expect_equal(validate_document_template(tm), character(0))
  # ...but a value with NEITHER a box nor wording can never be found, and saying
  # so at save time is the only chance to say it.
  tm$pairs$nothing <- list(type = "money")
  expect_true(any(grepl("neither a box nor any wording",
                        validate_document_template(tm))))
})

# ---------------------------------------------------------------------------
# A TABLE THE DOCUMENT DOES NOT CONTAIN (register H1, H2, H3)
# ---------------------------------------------------------------------------

# one_table_doc() -- a one-page document carrying ONE table, and it is not the
# Account summary. Everything below asks the Account summary's template entry to
# read it, which is what a family template of many tables does on every copy.
one_table_doc <- function() doc_input(list(doc_page(
  doc_L(40, 60, "Fees charged"),
  doc_L(40, 90, "Fee"), doc_L(240, 90, "Basis"), doc_R(500, 90, "Amount"),
  doc_L(40, 110, "AccountKeeping"), doc_L(240, 110, "per month"), doc_R(500, 110, "5.00"),
  doc_L(40, 128, "Transaction"), doc_L(240, 128, "per item"), doc_R(500, 128, "0.30"))))

test_that("a table the document does not contain comes back absent, not fabricated", {
  # MEASURED BEFORE THE FIX: anchor position_same_page, confidence 0.40, and one
  # row -- "Transaction | per item | NA | 0.30" -- which is the FEE table's ink
  # read through the Account summary's bands and delivered under its name. A
  # thirty-five table template did that thirty-four times on one document.
  inp <- one_table_doc(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$start$page <- 1L; tab$end$page <- 1L
  loc <- doc_locate_table(inp, tab, tm)
  expect_identical(loc$anchor, "not_found")
  expect_equal(loc$confidence, 0)
  expect_length(loc$windows, 0L)
  r <- doc_table_rows(inp, tab, tm, loc)
  expect_equal(r$n_rows, 0L)
  expect_equal(nrow(r$rows), 0L)
  # and it is SAID -- the summary reports it as not found, not as an empty table
  # that happened to have nothing in it.
  s <- document_summary(list(account_summary = c(r, list(name = "Account summary"))))
  expect_identical(s$found_by, "not found")
  expect_equal(s$rows, 0L)
})

test_that("a table whose declared page is past the end of the document is refused", {
  # The clamp put page 2 of a 12-page template onto the LAST page of a 2-page
  # copy and read whatever ink was there -- for every table at once.
  inp <- one_table_doc(); tm <- doc_fixture_template()
  loc <- doc_locate_table(inp, tm$tables$account_summary, tm)   # declared start: page 2
  expect_identical(loc$anchor, "not_found")
  expect_length(loc$windows, 0L)
  expect_match(loc$detail, "page 2", fixed = TRUE)
  expect_match(loc$detail, "only has 1 page", fixed = TRUE)
  expect_equal(doc_table_rows(inp, tm$tables$account_summary, tm, loc)$n_rows, 0L)
})

test_that("a table that IS on the document is untouched by either refusal", {
  inp <- context_input(); tm <- doc_fixture_template()
  loc <- doc_locate_table(inp, tm$tables$account_summary, tm)
  expect_identical(loc$anchor, "header")
  expect_equal(doc_table_rows(inp, tm$tables$account_summary, tm, loc)$n_rows, 4L)
  ext <- extract_document(inp, tm)
  expect_identical(unname(vapply(ext$tables, function(r) r$anchor, character(1))),
                   c("header", "header", "header"))
})

test_that("a declared END past the last page still just stops at the last page", {
  # Only the START is refused. A table declared to finish on page 7 of a shorter
  # copy is a truncation the row run already handles, not a fabrication.
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$transactions
  tab$end$page <- 99L
  loc <- doc_locate_table(inp, tab, tm)
  expect_identical(loc$anchor, "header")
  expect_true(max(loc$pages) <= 7L)
})

test_that("the different-length grade reads the page count off the TEMPLATE, so 0.2 is reachable", {
  # doc_pages is written at the template ROOT (document_template_from_proposal),
  # and this graded itself on tab$doc_pages -- a key nothing writes -- so the
  # comparison was always TRUE and position_other_page could never happen.
  tm <- doc_fixture_template()                       # root doc_pages = 7
  blank <- doc_page(doc_L(40, 60, "prose only"), doc_L(40, 90, "more prose"),
                    doc_L(40, 120, "and more"))
  tab <- tm$tables$position
  tab$anchor <- NULL                                 # nothing declared to look for
  tab$start$page <- 1L; tab$end$page <- 1L
  short <- doc_locate_table(doc_input(list(blank)), tab, tm)
  expect_identical(short$anchor, "position_other_page")
  expect_equal(short$confidence, 0.2)
  same <- doc_locate_table(doc_input(rep(list(blank), 7)), tab, tm)
  expect_identical(same$anchor, "position_same_page")
  expect_equal(same$confidence, 0.4)
  # ...and a per-table override still wins where a template sets one.
  tab$doc_pages <- 1L
  own <- doc_locate_table(doc_input(list(blank)), tab, tm)
  expect_identical(own$anchor, "position_same_page")
})

test_that("a table with nothing declared to look for still reads from where it was drawn", {
  # The refusal is of a GUESS made against evidence that failed, never of a table
  # whose template never said what to look for -- that one has only its position.
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$anchor <- NULL
  tab$start$page <- 7L; tab$end$page <- 7L
  loc <- doc_locate_table(inp, tab, tm)
  expect_true(loc$anchor %in% c("header", "position_same_page", "position_other_page"))
  expect_gt(length(loc$windows), 0L)
})

# ---------------------------------------------------------------------------
# WHAT LANDED IN THE COLUMN, NOT JUST WHETHER ANYTHING DID (register I3)
# ---------------------------------------------------------------------------

# fee_doc(amount_lo, amount_hi) -- a three-column fee schedule, with the Amount
# band drawn wherever the test wants it.
fee_doc <- function() doc_input(list(doc_page(
  doc_L(40, 60, "Fee"), doc_L(200, 60, "Basis"), doc_R(500, 60, "Amount"),
  doc_L(40, 80, "Legal"),   doc_L(310, 80, "fees"),          doc_R(500, 80, "1,200.00"),
  doc_L(40, 98, "Court"),   doc_L(310, 98, "filing"),        doc_R(500, 98, "330.00"),
  doc_L(40, 116, "Postage"), doc_L(310, 116, "disbursements"), doc_R(500, 116, "18.50"))))

fee_tab <- function(amount = c(440, 520)) list(
  name = "Fees", start = list(page = 1, y = 58), end = list(page = 1, y = 140),
  header_rows = 1L, follow = FALSE,
  anchor = list(header_text = list("Fee", "Basis", "Amount")),
  columns = list(
    list(name = "Fee",    x_min = 30,  x_max = 190, type = "text"),
    list(name = "Basis",  x_min = 190, x_max = 300, type = "text"),
    list(name = "Amount", x_min = amount[1], x_max = amount[2], type = "money")))

fee_tmpl <- function(tab) list(mode = "document", ref_width = DOC_W,
                               ref_height = DOC_H, tables = list(fees = tab))

test_that("a money column full of words is reported as a band failure, not as a full column", {
  # MEASURED BEFORE THE FIX: Amount reported filled 3, rows 3, fill_rate 1.00,
  # low_fill FALSE -- a perfectly full, perfectly clean column holding "fees",
  # "filing" and "disbursements", while Amount__value was NA in all three rows.
  # The engine had already computed the answer and thrown it away.
  tab <- fee_tab(c(300, 380))            # the band sits over the Basis words
  r <- doc_table_rows(fee_doc(), tab, fee_tmpl(tab))
  amt <- r$report[r$report$column == "Amount", , drop = FALSE]
  expect_equal(amt$filled, 3L)
  expect_equal(amt$fill_rate, 1)         # unchanged: it IS full
  expect_false(amt$low_fill)             # ...and it is NOT a thin column
  expect_equal(amt$parsed, 0L)           # the new fact
  expect_equal(amt$parse_rate, 0)
  expect_true(amt$wrong_kind)
  # NOTHING SILENT: it says so where the screen already prints a line.
  expect_match(r$detail, "nothing in Amount reads as", fixed = TRUE)
})

test_that("a column reading what it was told to hold is not flagged", {
  tab <- fee_tab()
  r <- doc_table_rows(fee_doc(), tab, fee_tmpl(tab))
  amt <- r$report[r$report$column == "Amount", , drop = FALSE]
  expect_equal(amt$parsed, 3L)
  expect_equal(amt$parse_rate, 1)
  expect_false(amt$wrong_kind)
  expect_false(grepl("reads as", r$detail, fixed = TRUE))
})

test_that("a cell holding two things is counted, and a column that declared nothing is not judged", {
  tab <- fee_tab()
  r <- doc_table_rows(fee_doc(), tab, fee_tmpl(tab))
  # "and courier" style cells: Basis holds one token per row here, Fee holds one.
  expect_equal(r$report$multi_token[r$report$column == "Fee"], 0L)
  tab2 <- tab
  tab2$columns[[3]]$type <- "auto"
  r2 <- doc_table_rows(fee_doc(), tab2, fee_tmpl(tab2))
  amt <- r2$report[r2$report$column == "Amount", , drop = FALSE]
  expect_true(is.na(amt$parsed))         # nothing was declared, so nothing failed
  expect_true(is.na(amt$multi_token))
  expect_false(amt$wrong_kind)
})

test_that("a swallowed neighbour shows up as two things in one cell", {
  # The A3 shape from the reader's side: one band covering two columns' ink.
  tab <- fee_tab()
  tab$columns <- list(list(name = "Fee", x_min = 30, x_max = 440, type = "text"),
                      list(name = "Amount", x_min = 440, x_max = 520, type = "money"))
  r <- doc_table_rows(fee_doc(), tab, fee_tmpl(tab))
  fee <- r$report[r$report$column == "Fee", , drop = FALSE]
  expect_equal(fee$filled, 3L)
  expect_equal(fee$fill_rate, 1)
  expect_false(fee$low_fill)             # nothing in the old report said a word
  expect_equal(fee$multi_token, 3L)      # ...and every cell holds two
})

# ---------------------------------------------------------------------------
# THE COMPANION IS A VALUE, NOT A SUBSTRING (register I5, I6)
# ---------------------------------------------------------------------------

test_that("every way a report prints a negative comes back negative", {
  # MEASURED BEFORE THE FIX, all four verbatim: "(1,234.56)", "987.65-",
  # "55.00 CR", "12.34 DR" -- no sign resolved anywhere, so a bracketed negative
  # was indistinguishable from a positive to anything that read the column.
  expect_identical(.doc_cell_value("(1,234.56)", "money"), -1234.56)
  expect_identical(.doc_cell_value("987.65-", "money"), -987.65)
  expect_identical(.doc_cell_value("55.00 CR", "money"), 55)
  expect_identical(.doc_cell_value("12.34 DR", "money"), -12.34)
  expect_identical(.doc_cell_value("1,240.55", "money"), 1240.55)
  # A cell that is not a figure is still not a figure: the shape test is intact.
  expect_true(is.na(.doc_cell_value("Legal fees", "money")))
  expect_true(is.na(.doc_cell_value("", "money")))
  # and a plain number in brackets is negative too
  expect_identical(.doc_cell_value("(1,234)", "number"), -1234)
  expect_identical(.doc_cell_value("42", "number"), 42)
})

test_that("a date can start at a four-digit year, and an impossible one is refused", {
  # MEASURED BEFORE THE FIX: "2025-04-07" came back "25-04-07", which opens as
  # 2007 in one reader and 1925 in another. Nothing was ever validated.
  expect_identical(.doc_cell_value("2025-04-07", "date"), "2025-04-07")
  expect_identical(.doc_cell_value("07/04/2025", "date"), "2025-04-07")
  expect_identical(.doc_cell_value("1 Apr 2025", "date"), "2025-04-01")
  # the statement route's own guards, now used here: 31 February is not a date,
  # and a year outside the trusted window is not one either.
  expect_true(is.na(.doc_cell_value("31/02/2025", "date")))
  expect_true(is.na(.doc_cell_value("07/04/1066", "date")))
  # a declared format is the only one tried, so an American column reads American
  expect_identical(.doc_cell_value("07/04/2025", "date", "%m/%d/%Y"), "2025-07-04")
  expect_identical(.doc_cell_value("07/04/2025", "date", "%d/%m/%Y"), "2025-04-07")
})

test_that("the reader emits the parsed value and keeps the page's own words beside it", {
  inp <- context_input(); tm <- doc_fixture_template()
  r <- doc_table_rows(inp, tm$tables$transactions, tm)
  expect_identical(r$rows$Date[1], "01/04/2025")            # verbatim
  expect_identical(r$rows$Date__value[1], "2025-04-01")     # and parsed
  expect_true(is.numeric(r$rows$Amount__value))
  expect_identical(r$rows$Amount[1], "-382.50")
  expect_identical(r$rows$Amount__value[1], -382.5)
  # the empty prototype has to agree with the full frame or the two cannot rbind
  tab <- tm$tables$transactions; tab$start$page <- 1L; tab$end$page <- 1L
  tab$anchor <- list(header_text = list("NothingLikeThis"))
  e <- doc_table_rows(inp, tab, tm)
  expect_equal(nrow(e$rows), 0L)
  expect_true(is.numeric(e$rows$Amount__value))
  expect_true(is.character(e$rows$Date__value))
})

test_that("a money column carrying two currencies says so instead of collapsing them", {
  # MEASURED BEFORE THE FIX: A$2,000.00 and US$4,000.00 both came back "$..." --
  # the marker was stripped and which money it was could not be recovered.
  p <- doc_page(doc_L(40, 60, "Fund"), doc_R(500, 60, "Value"),
                doc_L(40, 80, "Alpha"), doc_R(500, 80, "A$2,000.00"),
                doc_L(40, 98, "Beta"),  doc_R(500, 98, "US$4,000.00"),
                doc_L(40, 116, "Gamma"), doc_R(500, 116, "A$1,500.00"))
  tab <- list(name = "Funds", start = list(page = 1, y = 58),
              end = list(page = 1, y = 140), header_rows = 1L, follow = FALSE,
              anchor = list(header_text = list("Fund", "Value")),
              columns = list(list(name = "Fund", x_min = 30, x_max = 380, type = "text"),
                             list(name = "Value", x_min = 380, x_max = 520, type = "money")))
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(f = tab))
  r <- doc_table_rows(doc_input(list(p)), tab, tm)
  expect_identical(r$rows$Value__value, c(2000, 4000, 1500))
  expect_identical(r$report$currency[r$report$column == "Value"], "A$, US$")
  expect_match(r$detail, "more than one currency", fixed = TRUE)
  # one currency throughout is recorded and said nothing about
  p1 <- doc_page(doc_L(40, 60, "Fund"), doc_R(500, 60, "Value"),
                 doc_L(40, 80, "Alpha"), doc_R(500, 80, "A$2,000.00"),
                 doc_L(40, 98, "Beta"),  doc_R(500, 98, "A$4,000.00"))
  r1 <- doc_table_rows(doc_input(list(p1)), tab, tm)
  expect_identical(r1$report$currency[r1$report$column == "Value"], "A$")
  expect_false(grepl("more than one currency", r1$detail, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# THE FILE THAT LEAVES THE BUILDING
#
# Everything below is about what survives into the download and the record. A
# screen gets closed; a court date is two years later.
# ---------------------------------------------------------------------------

test_that("a table called Report does not silently destroy the whole workbook", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not installed")
  # H8. write_document_outputs adds its own Report/Values sheets AFTER the table
  # sheets, so a table called either -- or two differing only in case -- made
  # openxlsx refuse, the tryCatch returned FALSE and the workbook was never
  # mentioned again. Measured before the fix: two roll-up CSVs, no xlsx, no error.
  inp <- context_input(); tm <- doc_fixture_template()
  tm$tables$position$name <- "Report"
  tm$tables$account_summary$name <- "report"       # ...and the same name in another case
  ext <- extract_document(inp, tm)
  dir <- file.path(tempdir(), paste0("h8-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- write_document_outputs(ext, dir, "doc")
  x <- paths[grepl("\\.tables\\.xlsx$", paths)]
  expect_length(x, 1L)
  expect_true(file.exists(x))
  sheets <- openxlsx::getSheetNames(x)
  # the reader's own sheets are reserved, and every sheet is unique IGNORING CASE
  expect_true(all(c("Report", "Values") %in% sheets))
  expect_equal(length(unique(tolower(sheets))), length(sheets))
  expect_equal(length(sheets), length(ext$tables) + 2L)
})

test_that("a workbook that cannot be written says so, and every table is written anyway", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not installed")
  # The other half of H8: the per-table CSV fallback was gated on "openxlsx is not
  # installed", so a workbook openxlsx REFUSED left the tables written nowhere at
  # all -- and nothing said a word about it.
  inp <- context_input()
  ext <- extract_document(inp, doc_fixture_template())
  dir <- file.path(tempdir(), paste0("h8b-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir.create(dir, recursive = TRUE)
  dir.create(file.path(dir, "doc.tables.xlsx"))     # the workbook's path is taken
  paths <- suppressWarnings(write_document_outputs(ext, dir, "doc"))
  expect_false(any(grepl("\\.tables\\.xlsx$", paths)))
  expect_true(any(grepl("account_summary\\.csv$", paths)))
  expect_true(any(grepl("transactions\\.csv$", paths)))
  expect_match(attr(paths, "notes"), "workbook could not be written")
})

test_that("an other output names what produced it, and is the same bytes every run", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not installed")
  # G1 + G4. The workbook used to carry no template id, no version, no engine
  # version, no source file and no hash -- and it changed bytes on every run,
  # because openxlsx stamps the wall clock and the ZIP stamps each entry, while
  # the statement writer pins both.
  inp <- context_input(); tm <- doc_fixture_template()
  ext <- extract_document(inp, tm)
  build <- list(engine_version = "9.9.9", template_id = tm$id,
                template_version = "1", template_sha256 = template_sha256(tm),
                source_file = "sample.pdf", source_sha256 = "0123456789",
                page_count = "7")
  d1 <- file.path(tempdir(), paste0("g1a-", as.integer(runif(1, 1, 1e6))))
  d2 <- file.path(tempdir(), paste0("g1b-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(c(d1, d2), recursive = TRUE), add = TRUE)
  p1 <- write_document_outputs(ext, d1, "doc", build = build)
  p2 <- write_document_outputs(ext, d2, "doc", build = build)
  x1 <- p1[grepl("\\.xlsx$", p1)][1]; x2 <- p2[grepl("\\.xlsx$", p2)][1]
  expect_true("Build" %in% openxlsx::getSheetNames(x1))
  bs <- openxlsx::read.xlsx(x1, sheet = "Build")
  expect_true(all(c("engine_version", "template_id", "template_sha256",
                    "source_file", "source_sha256") %in% bs$field))
  expect_identical(bs$value[bs$field == "template_id"], tm$id)
  # ...and the same input written twice is the same file, byte for byte
  expect_identical(readBin(x1, "raw", file.info(x1)$size),
                   readBin(x2, "raw", file.info(x2)$size))
  # the CSV that can be read without opening the workbook carries it too
  rp <- paste(readLines(p1[grepl("\\.report\\.csv$", p1)][1]), collapse = "\n")
  expect_match(rp, "What produced this file", fixed = TRUE)
  expect_match(rp, tm$id, fixed = TRUE)
})

test_that("the per-table summary is in the report CSV, not only in the workbook", {
  # G3. found_by / confidence / unclaimed_words / pages reached ONE sheet of one
  # optional file: on a box with no openxlsx the summary was written nowhere at
  # all, while the comment here claimed nothing was dropped for want of a package.
  inp <- context_input()
  ext <- extract_document(inp, doc_fixture_template())
  dir <- file.path(tempdir(), paste0("g3r-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- write_document_outputs(ext, dir, "doc", formats = "csv")
  rp <- paste(readLines(paths[grepl("\\.report\\.csv$", paths)][1]), collapse = "\n")
  expect_match(rp, "Every table on this document", fixed = TRUE)
  expect_match(rp, "its heading", fixed = TRUE)          # how each table was found
  expect_match(rp, "unclaimed_words", fixed = TRUE)
  expect_match(rp, "Every column of every table", fixed = TRUE)
})

test_that("the words no column claimed are written out, not just counted", {
  # G3. `spilled` is the single best piece of evidence this route produces -- the
  # actual words and where they were printed -- and the only reader anywhere was
  # an nrow() for the count. Narrow a money band and the figures it loses must
  # come out in a file, not vanish.
  inp <- context_input(); tm <- doc_fixture_template()
  tm$tables$account_summary$columns[[4]]$x_min <- 470
  tm$tables$account_summary$columns[[4]]$x_max <- 480
  ext <- extract_document(inp, tm)
  expect_gt(nrow(ext$unclaimed), 0L)
  expect_identical(names(ext$unclaimed), c("table", "page", "x", "y", "text"))
  dir <- file.path(tempdir(), paste0("g3u-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- write_document_outputs(ext, dir, "doc", formats = "csv")
  u <- paths[grepl("\\.unclaimed\\.csv$", paths)]
  expect_length(u, 1L)
  back <- utils::read.csv(u, stringsAsFactors = FALSE, colClasses = "character")
  expect_true("1,810.20" %in% back$text)      # a real closing balance, recovered
  # a clean read writes no such file: its existence is the signal
  clean <- extract_document(inp, doc_fixture_template())
  d2 <- file.path(tempdir(), paste0("g3v-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  expect_length(Filter(function(q) grepl("\\.unclaimed\\.csv$", q),
                       write_document_outputs(clean, d2, "doc", formats = "csv")), 0L)
})

test_that("a cell that reads as a formula reaches the CSV as text", {
  # G6. "=1+1" reached doc.tables-long.csv verbatim and Excel evaluated it, so the
  # reviewer saw 2 where the document printed =1+1 -- a wrong figure that looks
  # right, delivered by the tool's own output. A report has no fixed schema, so
  # every character column is guarded.
  p <- doc_page(doc_L(40, 60, "Item"), doc_L(240, 60, "Note"),
                doc_L(40, 80, "=1+1"), doc_L(240, 80, "=HYPERLINK(0)"))
  tab <- list(name = "Funds", start = list(page = 1, y = 58),
              end = list(page = 1, y = 140), header_rows = 1L, follow = FALSE,
              anchor = list(header_text = list("Item", "Note")),
              columns = list(list(name = "Item", x_min = 30, x_max = 200, type = "text"),
                             list(name = "Note", x_min = 200, x_max = 400, type = "text")))
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(f = tab))
  ext <- extract_document(doc_input(list(p)), tm)
  dir <- file.path(tempdir(), paste0("g6-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- write_document_outputs(ext, dir, "doc", formats = "csv")
  back <- utils::read.csv(paths[grepl("tables-long", paths)][1],
                          stringsAsFactors = FALSE, colClasses = "character")
  expect_true(all(grepl("^'=", back$value)))          # neutralised, not stripped
  expect_true(any(back$value == "'=1+1"))
})

# ---------------------------------------------------------------------------
# WHICH TEMPLATE READS THIS
# ---------------------------------------------------------------------------

test_that("an identifying phrase matches however it was typed", {
  # A2c, measured: a page printing QUARTERLY PORTFOLIO REPORT was NOT matched by
  # the phrase typed as "Quarterly Portfolio Report", and nothing on screen said
  # why. This is very likely "I put a phrase printed on it, bang smack on front
  # page, still used another template".
  inp <- doc_input(list(doc_page(doc_L(40, 60, "QUARTERLY PORTFOLIO REPORT"),
                                 doc_L(40, 90, "Prepared for: A Client"))))
  mk <- function(...) list(q = list(id = "q", mode = "document", format = "pdf",
    fingerprint = list(page_contains_all = list(...)),
    tables = list(x = list(name = "x"))))
  for (ph in c("QUARTERLY PORTFOLIO REPORT", "Quarterly Portfolio Report",
               "Quarterly  Portfolio Report", "quarterly portfolio report."))
    expect_true(detect_document_template(inp, mk(ph))$matched, info = ph)
  # punctuation between the words stops mattering too -- but a phrase that is not
  # printed here still does not match, which is the half that must not loosen
  expect_true(detect_document_template(inp, mk("Prepared for A Client"))$matched)
  expect_false(detect_document_template(inp, mk("Annual holdings statement"))$matched)
  # ...and a phrase is still not allowed to match ACROSS a line break
  expect_false(detect_document_template(inp, mk("REPORT Prepared"))$matched)
})

test_that("two templates that both fit are reported, not discarded", {
  # H10 / L2. A tie returned matched = FALSE and named neither template, so a
  # document the library reads perfectly came back "unsupported". The statement
  # route has never done that: same return shape, same resolution.
  inp <- context_input(); tm <- doc_fixture_template()
  tm2 <- tm; tm2$id <- "other_report"
  d <- detect_document_template(inp, list(a = tm, b = tm2))
  expect_false(d$matched)
  expect_identical(sort(d$tied), c("a", "b"))
  expect_identical(d$template_id, "a")            # the best by a stated order
  expect_match(d$detail, "equally specific")
  expect_match(d$detail_plain, "equally well")
  expect_false(grepl("[0-9a-f]{16}", d$detail_plain))   # no ids, no hashes
  # the same fields the statement detector returns
  expect_true(all(c("candidates", "eligible_ids", "tied", "margin", "runner_up",
                    "detail", "detail_plain") %in% names(d)))
  # a MORE specific template beats a less specific one outright: no tie at all
  tm3 <- tm; tm3$id <- "sharper"
  tm3$fingerprint$page_contains_all <- list("Consolidated position report",
                                            "NORTHWIND TRUSTEE SERVICES")
  d2 <- detect_document_template(inp, list(a = tm, c = tm3))
  expect_true(d2$matched)
  expect_identical(d2$template_id, "c")
  expect_identical(d2$tied, character(0))
})

test_that("a template that almost matched says which wording was missing", {
  # L3. The statement route says "closest anz_everyday_pdf score 1/3 (missing
  # 'Deposits')". This route said "no document template's identifying phrases were
  # all found" -- no candidate, no wording, nothing to act on.
  inp <- context_input(); tm <- doc_fixture_template()
  tm$fingerprint$page_contains_all <- list("Consolidated position report",
                                           "Schedule of derivatives")
  d <- detect_document_template(inp, list(northwind_position = tm))
  expect_false(d$matched)
  expect_identical(d$tied, character(0))
  expect_match(d$detail, "closest northwind_position score 1/2")
  expect_match(d$detail, "Schedule of derivatives", fixed = TRUE)
  # ...and the same fact in words, with no id and no fraction in it
  expect_match(d$detail_plain, "Schedule of derivatives", fixed = TRUE)
  expect_false(grepl("northwind_position", d$detail_plain, fixed = TRUE))
  expect_equal(nrow(d$candidates), 1L)
  # having no templates at all is still a different answer from none matching
  expect_match(detect_document_template(inp, list())$detail, "no document templates")
})

# ---------------------------------------------------------------------------
# ABSENT IS NOT EMPTY
# ---------------------------------------------------------------------------

test_that("a table that is not on this document is absent, and is said to be", {
  # H1. A table the document does not carry came back full of whatever sat at its
  # old coordinates. It now has no entry, no sheet, no long rows and no column
  # report -- and one row in the summary saying so.
  inp <- context_input(); tm <- doc_fixture_template()
  tm$tables$position$start$page <- 2L; tm$tables$position$end$page <- 2L
  ext <- extract_document(inp, tm)
  expect_identical(ext$misses, "position")
  expect_false("position" %in% names(ext$tables))
  expect_equal(sum(ext$long$table == "position"), 0L)
  expect_equal(sum(ext$report$table == "position"), 0L)
  # STATED, not silent: one row in the summary, and it is the last one
  s <- ext$summary
  expect_true("position" %in% s$table)
  expect_identical(s$found_by[s$table == "position"], "not on this document")
  expect_equal(s$rows[s$table == "position"], 0L)
  expect_true(is.na(s$confidence[s$table == "position"]))
  expect_match(s$note[s$table == "position"], "not on this copy")
  # and the summary keeps ONE shape: the same columns whether or not any is missing
  expect_identical(names(s), names(extract_document(inp, doc_fixture_template())$summary))
})

test_that("a table set to be read wherever it appears may be absent without fault", {
  # An absence is only a fault for a table PINNED here. Three-this-month and
  # seven-next means absence is the ordinary case, and a permanent needs_review is
  # a warning nobody reads.
  tm <- doc_fixture_template()
  expect_false(.doc_table_optional(tm$tables$position))
  expect_true(.doc_table_optional(utils::modifyList(tm$tables$position,
                                                    list(optional = TRUE))))
  expect_true(.doc_table_optional(utils::modifyList(tm$tables$position,
                                                    list(occurrence = "all"))))
  expect_false(.doc_table_optional(utils::modifyList(tm$tables$position,
                                                     list(occurrence = "once"))))
})

test_that("a typed column that parsed nothing is what decides the verdict", {
  # G3 / I3. A money column whose band landed on words reports itself completely
  # full -- fill asks only whether a cell has any text -- while the engine already
  # knows not one cell of it parsed as money and threw that away.
  rep <- data.frame(table = c("a", "a", "b"), column = c("Amount", "Name", "Date"),
                    type = c("money", "text", "date"), filled = c(3L, 3L, 3L),
                    rows = c(3L, 3L, 3L), parse_rate = c(0, 1, 1),
                    wrong_kind = c(TRUE, FALSE, FALSE), stringsAsFactors = FALSE)
  expect_identical(.doc_parse_failures(rep), "a")
  # ...and it degrades to doing nothing on a report frame that has neither column
  expect_identical(.doc_parse_failures(rep[, c("table", "column", "type")]),
                   character(0))
  expect_identical(.doc_parse_failures(NULL), character(0))
})

test_that("a per-table doc_pages is accepted and a nonsense one is refused", {
  # H3. doc_pages is written at the template ROOT and read with a per-table
  # override, so the override has to be a page count or be refused at save time.
  tm <- doc_fixture_template()
  tm$tables$account_summary$doc_pages <- 12L
  expect_identical(validate_document_template(tm), character(0))
  tm$tables$account_summary$doc_pages <- 0L
  expect_match(paste(validate_document_template(tm), collapse = " "),
               "number of pages the example had")
})

# ---------------------------------------------------------------------------
# A COLUMN WHOSE BAND WAS MERGED INTO ITS NEIGHBOUR (register A3)
# ---------------------------------------------------------------------------

a3_doc <- function() doc_input(list(doc_page(
  doc_L(40, 60, "Item"), doc_L(300, 60, "Note"), doc_R(560, 60, "Amount"),
  # the first data row runs on past the Note gutter, and Note is BLANK on it
  doc_L(40, 80, "A very long description indeed that runs on and on past the gutter"),
  doc_R(560, 80, "1,000.00"),
  doc_L(40, 98, "Beta"), doc_L(300, 98, "seen"), doc_R(560, 98, "2,000.00"))))

test_that("a band any one line breaks in two is two columns", {
  # MEASURED BEFORE THE FIX: two columns, the first called "Item Note" running
  # 37-421 -- so every Note value on every other row was read into the Item cell,
  # silently, because a tiled band claims every word and nothing is unclaimed.
  box <- list(page = 1, x_min = 30, x_max = 580, y_min = 55, y_max = 88)
  cols <- doc_columns_from_box(a3_doc(), box)
  expect_equal(length(cols), 3L)
  expect_identical(vapply(cols, function(cc) cc$name, character(1)),
                   c("Item", "Note", "Amount"))
  # the divisions still tile, so nothing can fall between two columns
  x0 <- vapply(cols, function(cc) cc$x_min, numeric(1))
  x1 <- vapply(cols, function(cc) cc$x_max, numeric(1))
  expect_equal(x1[-length(x1)], x0[-1])
})

test_that("a box nobody disagrees about is derived exactly as it was", {
  inp <- context_input()
  box <- list(page = 2, x_min = 30, x_max = 520, y_min = 95, y_max = 125)
  cols <- doc_columns_from_box(inp, box)
  expect_equal(length(cols), 4L)
  expect_identical(vapply(cols, function(cc) cc$name, character(1)),
                   c("Account", "Type", "Opening", "Closing"))
})

test_that("fitting the columns never takes a drawn column away", {
  # MEASURED BEFORE THE FIX: two columns came back. `Note` -- drawn, named, typed
  # -- was deleted because this month's copy prints nothing in it, and its
  # territory went to the neighbours that tile beside it.
  tab <- list(name = "T", start = list(page = 1, y = 58), end = list(page = 1, y = 120),
              header_rows = 1L, follow = FALSE,
              anchor = list(header_text = list("Item", "Amount")),
              columns = list(
                list(name = "Item",   x_min = 30,  x_max = 230, type = "text"),
                list(name = "Note",   x_min = 230, x_max = 380, type = "text"),
                list(name = "Amount", x_min = 380, x_max = 580, type = "money")))
  inp <- doc_input(list(doc_page(
    doc_L(40, 60, "Item"), doc_R(560, 60, "Amount"),
    doc_L(40, 80, "Alpha"), doc_R(560, 80, "1,000.00"),
    doc_L(40, 98, "Beta"),  doc_R(560, 98, "2,000.00"))))
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(t = tab))
  fit <- doc_fit_columns(inp, tab, tm)
  expect_equal(length(fit), 3L)
  expect_identical(vapply(fit, function(cc) as.character(cc$name), character(1))[2], "Note")
  # ...and it is kept EXACTLY as drawn, in one piece, with its type
  expect_equal(fit[[2]]$x_min, 230)
  expect_equal(fit[[2]]$x_max, 380)
  expect_identical(fit[[2]]$type, "text")
})

test_that("fitting the columns can never return fewer than the table already has", {
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  fit <- doc_fit_columns(inp, tab, tm)
  expect_gte(length(fit), length(tab$columns))
  # a table it cannot even locate hands back exactly what was drawn
  gone <- tab
  gone$start$page <- 99L; gone$end$page <- 99L
  expect_equal(length(doc_fit_columns(inp, gone, tm)), length(tab$columns))
})

test_that("the words no column claimed come back as a frame anybody can write out", {
  # Its only reader anywhere was nrow(). On the measured wrong-column read it
  # held the four real closing balances -- the figures the table lost.
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$columns[[4]]$x_min <- 505              # the Closing band misses every figure
  r <- doc_table_rows(inp, tab, tm)
  expect_identical(names(r$spilled), c("page", "y", "x", "text"))
  expect_identical(vapply(r$spilled, class, character(1)),
                   c(page = "integer", y = "numeric", x = "numeric", text = "character"))
  expect_equal(nrow(r$spilled), 4L)
  expect_identical(r$spilled$text,
                   c("1,810.20", "15,420.75", "50,000.00", "-410.05"))
  expect_identical(rownames(r$spilled), as.character(seq_len(4)))
  expect_false(is.unsorted(r$spilled$y))     # reading order
  # and a clean read hands back the same shape with no rows in it
  clean <- doc_table_rows(inp, tm$tables$account_summary, tm)
  expect_equal(nrow(clean$spilled), 0L)
  expect_identical(names(clean$spilled), c("page", "y", "x", "text"))
})

# ---------------------------------------------------------------------------
# A3 (the proposer's half): the divisions come from all the ink in the table
# ---------------------------------------------------------------------------

test_that("a column blank on the table's first page is still a column", {
  # A3. propose_tables() kept only the FIRST page's bands when it joined a
  # continuation, so a column that prints nothing until page 2 was drawn off its
  # heading word alone. Measured before this: the Ref band came out 415.0-446.5,
  # all ten reference values on page 2 fell outside it and were UNCLAIMED, and
  # every Ref cell in the file read blank -- a column of nothing, with the rows
  # around it looking perfect.
  hdr <- function(y) list(doc_L(40, y, "Date"), doc_L(120, y, "Description"),
                          doc_R(400, y, "Amount"), doc_L(430, y, "Ref"))
  row1 <- function(y, i) list(doc_L(40, y, sprintf("%02d/04/2025", i)),
                              doc_L(120, y, sprintf("PAYMENT-%03d", i)),
                              doc_R(400, y, .doc_money(100 + i)))
  row2 <- function(y, i) c(row1(y, i), list(doc_L(430, y, sprintf("REFERENCE-%03d", i))))
  p1 <- do.call(doc_page, c(list(doc_L(40, 60, "Payments schedule")), hdr(100),
          unlist(lapply(1:20, function(i) row1(100 + 18 * i, i)), recursive = FALSE)))
  p2 <- do.call(doc_page, c(hdr(60),
          unlist(lapply(1:10, function(i) row2(60 + 18 * i, i + 20)), recursive = FALSE)))
  inp <- doc_input(list(p1, p2))

  props <- propose_tables(inp)
  expect_equal(length(props), 1L)
  ref <- Filter(function(cc) identical(cc$name, "Ref"), props[[1]]$columns)
  expect_length(ref, 1L)                       # it kept the name the heading gave
  r <- doc_table_rows(inp, props[[1]], NULL)
  expect_equal(nrow(r$spilled), 0L)            # nothing left outside a column
  got <- r$rows[["Ref"]][r$rows$page == 2L]
  expect_true(all(nzchar(got)))                # ...and every value is IN it
  expect_true(any(grepl("^REFERENCE-", got)))
})

test_that("a heading names the band it is printed over, not the band with its number", {
  # A3. The name map was by INDEX, so the moment the band count and the header
  # cell count differed every name moved one column left. Measured on a schedule
  # whose rows print a code the heading row has nothing over: the code band was
  # called "Basis", the basis band "Amount", and the money band "column_4".
  p <- do.call(doc_page, c(
    list(doc_L(40, 60, "Fee schedule"),
         doc_L(40, 100, "Item"), doc_L(240, 100, "Basis"), doc_R(500, 100, "Amount")),
    unlist(lapply(1:6, function(i) list(
      doc_L(40, 100 + 18 * i, sprintf("ITEM%02d", i)),
      doc_L(150, 100 + 18 * i, sprintf("AB%02d", i)),
      doc_L(240, 100 + 18 * i, "per month"),
      doc_R(500, 100 + 18 * i, .doc_money(5 * i)))), recursive = FALSE)))
  props <- propose_tables(doc_input(list(p)))
  expect_equal(length(props), 1L)
  cols <- props[[1]]$columns
  band_of <- function(nm) {
    hit <- Filter(function(cc) identical(cc$name, nm), cols)
    if (!length(hit)) NULL else hit[[1]]
  }
  inside <- function(nm, x) {
    b <- band_of(nm); !is.null(b) && x >= b$x_min && x <= b$x_max
  }
  expect_true(inside("Item", 50))              # ITEM01
  expect_true(inside("Basis", 260))            # "per month"
  expect_true(inside("Amount", 490))           # the figure
  expect_false(inside("Basis", 160))           # ...and NOT the unheaded code band
  r <- doc_table_rows(doc_input(list(p)), props[[1]], NULL)
  expect_true(all(grepl("^per month", r$rows[["Basis"]])))
  expect_true(all(grepl("^[0-9]", r$rows[["Amount"]])))
})

test_that("a table is never proposed under a heading that names a person", {
  # G9b. The proposed title is stored in the template verbatim and A2b makes it
  # the search key, so "Prepared for Mr John Smith" would put a customer's name
  # into a file that gets copied off the box. The drafter already refuses one as
  # a fingerprint phrase; the search simply carries on up the page.
  p <- do.call(doc_page, c(
    list(doc_L(40, 40, "Holdings at period end"),
         doc_L(40, 70, "Prepared for Mr John Smith"),
         doc_L(40, 100, "Code"), doc_L(240, 100, "Units"), doc_R(500, 100, "Value")),
    unlist(lapply(1:5, function(i) list(
      doc_L(40, 100 + 18 * i, sprintf("SEC%02d", i)),
      doc_L(240, 100 + 18 * i, sprintf("%d", 10 * i)),
      doc_R(500, 100 + 18 * i, .doc_money(25 * i)))), recursive = FALSE)))
  props <- propose_tables(doc_input(list(p)))
  expect_equal(length(props), 1L)
  expect_false(grepl("John Smith", props[[1]]$name, fixed = TRUE))
  expect_identical(props[[1]]$name, "Holdings at period end")
})
