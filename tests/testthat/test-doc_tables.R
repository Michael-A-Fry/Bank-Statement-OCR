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

test_that("with no heading to go on it falls back to position, and says THAT", {
  inp <- context_input(); tm <- doc_fixture_template()
  tab <- tm$tables$account_summary
  tab$start$page <- 7L; tab$end$page <- 7L
  tab$anchor$header_text <- list("Nothing", "Like", "This", "Wording")
  tab$anchor$first_column <- list("NoSuchLabel")
  tab$doc_pages <- 7L
  loc <- doc_locate_table(inp, tab, tm)
  expect_identical(loc$anchor, "position_same_page")
  expect_lt(loc$confidence, 0.5)
  # THE POINT OF THE TEST. Reading from a stale position on a page where the
  # table has moved does not fail loudly -- it quietly picks up the title line and
  # the header row as if they were data. That is why the confidence above is low
  # and why the status rule in convert_tables() sends this to review.
  r <- doc_table_rows(inp, tab, tm, loc)
  expect_equal(r$n_rows, 3L)
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
  expect_identical(r$rows$Opening__value[1], "1,240.55")
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
