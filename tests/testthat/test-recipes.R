# test-recipes.R -- reading a known design with its recipe (R/recipes.R): the
# loader, the recogniser, columns that hang under their headings, the proof gate,
# and the way auto_read() and convert_statement() use a recipe. The rule under
# test throughout: a recipe proposes, the statement's own arithmetic decides, so a
# recipe reading is automatic only when it is proven, and a recipe that does not
# fit leaves the file to the automatic reader exactly as before.
#
# Statements are written as lines of text, as in test-auto-read.R: one word box
# per word, 5pt a character, 12pt a line.

rc_pdf <- function(..., width = 600, shift = NULL) {
  pages <- list(...)
  words <- lapply(seq_along(pages), function(p) {
    lines <- pages[[p]]
    rows <- lapply(seq_along(lines), function(i) {
      m <- gregexpr("\\S+", lines[i])[[1]]
      if (m[1] < 0) return(NULL)
      tx <- regmatches(lines[i], list(m))[[1]]
      data.frame(width = nchar(tx) * 5, height = 8, x = 20 + (as.numeric(m) - 1) * 5,
                 y = 20 + i * 12, space = TRUE, text = tx, stringsAsFactors = FALSE)
    })
    w <- do.call(rbind, rows)
    if (!is.null(shift) && length(shift) >= p) w$x <- w$x + shift[p]
    w
  })
  list(kind = "pdf", path = "", sha256 = NA_character_,
       pages = vapply(pages, paste, "", collapse = "\n"), words = words,
       page_width = rep(width, length(pages)), page_height = rep(800, length(pages)),
       page_ocr = rep(FALSE, length(pages)), meta = list())
}

# One table line: the date at column 1, the details at 10, money out ending at 62,
# money in at 76, the balance at 90 (a trailing OD or CR prints after it).
rc_line <- function(date = "", desc = "", dr = "", cr = "", bal = "", mark = "") {
  s <- rep(" ", 110)
  put <- function(s, txt, at) { ch <- strsplit(txt, "")[[1]]; if (length(ch)) s[at:(at + length(ch) - 1L)] <- ch; s }
  s <- put(s, date, 1); s <- put(s, desc, 10)
  if (nzchar(dr)) s <- put(s, dr, 63 - nchar(dr))
  if (nzchar(cr)) s <- put(s, cr, 77 - nchar(cr))
  if (nzchar(bal)) s <- put(s, bal, 91 - nchar(bal))
  if (nzchar(mark)) s <- put(s, mark, 92)
  sub("\\s+$", "", paste(s, collapse = ""))
}

rc_box <- function(period = "01 Feb 2026 to 28 Feb 2026", open = "1,000.00", close = "3,344.91",
                   glance = "Account at a glance") c(
  "ANZ                                                         Account statement", "",
  paste0(glance, "                         Statement date 28 Feb 2026"),
  "Account name       SAMPLE TRADING LIMITED",
  "Account number     01-0021-2348037-00",
  paste("Statement period  ", period),
  paste("Opening balance            ", open),
  paste("Closing balance            ", close), "")
rc_head <- rc_line("Date", "Transaction type and details", "Withdrawals", "Deposits", "Balance")
rc_rows <- list(
  rc_line("03 Feb", "EP    RIVERSIDE DAIRY", dr = "12.40", bal = "987.60"),
  rc_line("05 Feb", "DC    SALARY MATAI HOLDINGS", cr = "3,120.00", bal = "4,107.60"),
  rc_line("09 Feb", "DD    CITY COUNCIL RATES", dr = "268.15", bal = "3,839.45"),
  rc_line("14 Feb", "AP    TRANSFER TO SAVINGS", dr = "400.00", bal = "3,439.45"),
  rc_line("21 Feb", "VT    HARBOUR FUEL", dr = "96.72", bal = "3,342.73"),
  rc_line("26 Feb", "      CREDIT INTEREST PAID", cr = "2.18", bal = "3,344.91"))
rc_want <- c(-12.40, 3120.00, -268.15, -400.00, -96.72, 2.18)
rc_dates <- c("2026-02-03", "2026-02-05", "2026-02-09", "2026-02-14", "2026-02-21", "2026-02-26")
rc_total <- function(dr, cr, what = "period") rc_line("", paste("      Totals at end of", what), dr = dr, cr = cr)

# The one-page statement, and the same statement over two pages, its heading
# printed again on the second, a page total and a balance brought forward.
rc_one <- function(rows = rc_rows, box = rc_box(), extra = character(0))
  c(box, rc_head, unlist(rows), rc_total("777.27", "3,122.18"), extra, "", "Page 1 of 1")
rc_two_pages <- function() list(
  c(rc_box(), rc_head, unlist(rc_rows[1:3]), rc_total("280.55", "3,120.00", "page"), "", "Page 1 of 2"),
  c("ANZ", "", rc_head, rc_line("", "      Balance brought forward from previous page", bal = "3,839.45"),
    unlist(rc_rows[4:6]), rc_total("777.27", "3,122.18"), "", "Page 2 of 2"))

rc_shipped <- function() recipes_load(fixture("recipes"))
rc_anz <- function() Filter(function(r) identical(r$id, "anz_everyday_pdf"), rc_shipped())[[1]]

rc_write <- function(dir, name, lines) writeLines(lines, file.path(dir, name))
rc_tmpdir <- function() { d <- tempfile("recipes_"); dir.create(d); d }
rc_valid_yaml <- function(id = "kauri_test", version = 1, status = "proven", extra = character(0)) c(
  sprintf("recipe: %s", id), "format: 1", sprintf("version: %s", version), "bank: anz", "kind: pdf",
  sprintf("status: %s", status),
  "recognise: {all: [\"Account at a glance\", \"Transaction type and details\"]}",
  "statement_starts: \"Account at a glance\"",
  "period: {label: \"Statement period\"}",
  "table:",
  "  header: [\"Date\", \"Transaction type and details\", \"Withdrawals\", \"Deposits\", \"Balance\"]",
  "  columns:",
  "    date: {under: \"Date\"}",
  "    description: {under: \"Transaction type and details\"}",
  "    debit: {under: \"Withdrawals\"}",
  "    credit: {under: \"Deposits\"}",
  "    balance: {under: \"Balance\"}",
  "  ends_at: [\"Totals at end of page\", \"Totals at end of period\"]",
  "  no_rows: [\"No transactions for this period\"]",
  "dates: {format: \"%d %b\", year: period}",
  "money: {style: debit_credit_cols, negative: [\"OD\"]}", extra)

# ---- the loader ------------------------------------------------------------------------

test_that("the shipped recipes load whole, each with its id and version", {
  rcs <- rc_shipped()
  expect_identical(attr(rcs, "problems"), character(0))
  refs <- vapply(rcs, `[[`, "", "ref")
  expect_true(all(c("anz_everyday_pdf@1", "anz_loan_pdf@1") %in% refs))
  anz <- rc_anz()
  expect_identical(anz$cols$field, c("date", "description", "debit", "credit", "balance"))
  expect_true(anz$anchored)
  expect_identical(anz$status, "proven")
})

test_that("a file that is not a valid recipe is named and left out, never half-used", {
  d <- rc_tmpdir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rc_write(d, "ok.yaml", rc_valid_yaml("ok_one"))
  rc_write(d, "broken.yaml", c("recipe: [unclosed"))
  rc_write(d, "noformat.yaml", sub("^format: 1$", "", rc_valid_yaml("no_format")))
  rc_write(d, "future.yaml", sub("^format: 1$", "format: 2", rc_valid_yaml("future_one")))
  rc_write(d, "upper.yaml", rc_valid_yaml("Bad-Id"))
  rc_write(d, "orphan.yaml", sub("under: \"Deposits\"", "under: \"Money in\"", rc_valid_yaml("orphan_head"), fixed = TRUE))
  rc_write(d, "mixed.yaml", sub("{under: \"Balance\"}", "{x_min: 400, x_max: 450}", rc_valid_yaml("mixed_cols"), fixed = TRUE))
  rc_write(d, "marker.yaml", sub("negative: [\"OD\"]", "negative: [\"ZZ\"]", rc_valid_yaml("odd_marker"), fixed = TRUE))
  rc_write(d, "year.yaml", sub("year: period", "year: printed", rc_valid_yaml("year_rule"), fixed = TRUE))
  rcs <- recipes_load(d)
  expect_identical(vapply(rcs, `[[`, "", "ref"), "ok_one@1")
  p <- attr(rcs, "problems")
  expect_length(p, 8L)
  for (f in c("broken.yaml", "noformat.yaml", "future.yaml", "upper.yaml", "orphan.yaml", "mixed.yaml",
              "marker.yaml", "year.yaml"))
    expect_true(any(startsWith(p, f)), info = f)
  expect_true(any(grepl("format 2; this reader reads format 1", p, fixed = TRUE)))
  expect_true(any(grepl("\"ZZ\"", p, fixed = TRUE)))
})

test_that("a recipe is never edited: its highest version is used, and a retired version retires it", {
  d <- rc_tmpdir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rc_write(d, "a.yaml", rc_valid_yaml("kauri_test", 1))
  rc_write(d, "a@2.yaml", rc_valid_yaml("kauri_test", 2))
  rc_write(d, "b.yaml", rc_valid_yaml("gone_test", 1))
  rc_write(d, "b@2.yaml", rc_valid_yaml("gone_test", 2, status = "retired"))
  rc_write(d, "c.yaml", rc_valid_yaml("kauri_test", 2))   # the same id@version twice
  rcs <- recipes_load(d)
  expect_identical(vapply(rcs, `[[`, "", "ref"), "kauri_test@2")
  expect_true(any(grepl("already defined", attr(rcs, "problems"), fixed = TRUE)))
})

# ---- the recogniser ----------------------------------------------------------------------

test_that("a recipe recognises its own design, and only when it is clearly the best", {
  rcs <- rc_shipped()
  inp <- rc_pdf(rc_one())
  expect_identical(recipe_recognise(inp, rcs)$recipe$ref, "anz_everyday_pdf@1")
  # Not the design: its own words are missing, or a word it must not see is printed.
  expect_null(recipe_recognise(rc_pdf(rc_one(box = rc_box(glance = "Your account"))), rcs)$recipe)
  expect_null(recipe_recognise(rc_pdf(rc_one(extra = "The following is a summary of your loan")), rcs)$recipe)
  # Its words, but not its table heading.
  noh <- rc_one(); noh[noh == rc_head] <- rc_line("Date", "Details", "Debits", "Credits", "Balance")
  expect_null(recipe_recognise(rc_pdf(noh), rcs)$recipe)
  # The bank picked is another bank's: the recipe is still found (a wrong pick never
  # hides the right recipe), and the mismatch is said in plain words.
  wrong <- recipe_recognise(inp, rcs, bank = "ASB")
  expect_identical(wrong$recipe$ref, "anz_everyday_pdf@1")
  expect_match(wrong$bank_note, "but ASB was chosen", fixed = TRUE)
  expect_identical(recipe_recognise(inp, rcs, bank = "ANZ")$recipe$ref, "anz_everyday_pdf@1")
  expect_null(recipe_recognise(inp, rcs, bank = "ANZ")$bank_note)
  # Two recipes that both fit: both are handed on, for each to read the statement.
  twin <- rc_anz(); twin$id <- "anz_twin"; twin$ref <- "anz_twin@1"
  rg <- recipe_recognise(inp, list(rc_anz(), twin))
  expect_length(rg$fits, 2L)
  # A draft is never used to read automatically, and a scan is not a text PDF.
  draft <- rc_anz(); draft$status <- "draft"
  expect_null(recipe_recognise(inp, list(draft))$recipe)
  scan <- inp; scan$page_ocr <- TRUE
  expect_null(recipe_recognise(scan, rcs)$recipe)
})

# ---- reading: columns under their headings ----------------------------------------------------

test_that("a recipe reads its design and proves it, the bands measured under the headings", {
  rd <- recipe_read(rc_pdf(rc_one()), rc_anz())
  expect_identical(rd$outcome, "proven")
  expect_identical(rd$matched_recipe, "anz_everyday_pdf@1")
  expect_equal(round(rd$transactions$amount, 2), rc_want)
  expect_identical(rd$transactions$date, rc_dates)
  expect_true(all(rd$checks$ok %in% c(TRUE, NA)))
  expect_identical(rd$proof$kind, "chain")
  expect_match(rd$why, "Read with recipe anz_everyday_pdf@1", fixed = TRUE)
  # Each money band holds its heading: the heading word's own span is inside it.
  b <- rd$columns
  hx <- function(word) { w <- rc_pdf(rc_one())$words[[1]]; w <- w[w$text == word, ][1, ]; c(w$x, w$x + w$width) }
  for (f in c("debit", "credit", "balance")) {
    h <- hx(c(debit = "Withdrawals", credit = "Deposits", balance = "Balance")[[f]])
    expect_true(b$x_min[b$field == f] <= h[1] + 1 && b$x_max[b$field == f] >= h[2] - 1, info = f)
  }
})

test_that("shifting every word on a page a few points reads exactly the same", {
  base <- recipe_read(do.call(rc_pdf, rc_two_pages()), rc_anz())
  expect_identical(base$outcome, "proven")
  expect_equal(round(base$transactions$amount, 2), rc_want)
  for (sh in list(c(0, 7), c(-4, 0), c(5, -6), c(11, 11))) {
    rd <- recipe_read(do.call(rc_pdf, c(rc_two_pages(), list(shift = sh))), rc_anz())
    expect_identical(rd$outcome, "proven", info = paste(sh, collapse = ","))
    expect_identical(rd$transactions$date, base$transactions$date)
    expect_equal(rd$transactions$amount, base$transactions$amount)
    expect_identical(rd$transactions$description, base$transactions$description)
    # The bands moved with the print.
    b0 <- base$columns; b1 <- rd$columns
    expect_equal(b1$x_min[b1$page == 2] - b0$x_min[b0$page == 2], rep(sh[2], 5))
  }
})

test_that("money columns printed further apart on one page are read under their own headings", {
  pg <- rc_two_pages()
  # Page 2 prints its money columns 15 points further right, headings and figures
  # together: the first page's bands would cut through them.
  p2 <- rc_pdf(pg[[2]])$words[[1]]
  move <- p2$x >= 20 + 50 * 5
  inp <- do.call(rc_pdf, pg)
  inp$words[[2]]$x[move] <- inp$words[[2]]$x[move] + 15
  rd <- recipe_read(inp, rc_anz())
  expect_identical(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), rc_want)
})

test_that("fixed x-bands are the fallback for a design with no heading row", {
  anz <- rc_anz()
  fixed <- anz
  x <- function(col) 20 + (col - 1) * 5
  fixed$anchored <- FALSE
  fixed$cols$under <- NA_character_
  fixed$cols$x_min <- c(x(1) - 5, x(9), x(45), x(64), x(78))
  fixed$cols$x_max <- c(x(9), x(45), x(64), x(78), x(96))
  rd <- recipe_read(rc_pdf(rc_one()), fixed)
  expect_identical(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), rc_want)
})

# ---- the proof gate --------------------------------------------------------------------------

test_that("a statement whose figures do not add up is never proven by its recipe", {
  rows <- rc_rows
  rows[[3]] <- rc_line("09 Feb", "DD    CITY COUNCIL RATES", dr = "268.15", bal = "3,893.45")
  rd <- recipe_read(rc_pdf(rc_one(rows)), rc_anz())
  expect_identical(rd$outcome, "check")
  expect_false(isTRUE(rd$checks$ok[rd$checks$check == "balance_chain"]))
  # The printed closing balance disagrees with the rows: not proven either.
  rd2 <- recipe_read(rc_pdf(rc_one(box = rc_box(close = "3,345.91"))), rc_anz())
  expect_identical(rd2$outcome, "check")
  # And auto_read never takes such a reading from the recipe: it reads the file
  # without it, and says the recipe did not prove.
  ar <- auto_read(rc_pdf(rc_one(rows)), opts = list(recipes = rc_shipped()))
  expect_null(ar$matched_recipe)
  expect_false(identical(ar$outcome, "proven"))
  expect_true(any(grepl("recipe anz_everyday_pdf@1", ar$notes, fixed = TRUE)))
  expect_identical(ar$recipe_tried$recipe, "anz_everyday_pdf@1")
})

test_that("a recipe whose columns the arithmetic reads the other way is not proven", {
  swapped <- rc_anz()
  swapped$cols$field[3:4] <- c("credit", "debit")
  rd <- recipe_read(rc_pdf(rc_one()), swapped)
  expect_identical(rd$outcome, "check")
  expect_false(isTRUE(rd$checks$ok[rd$checks$check == "unique"]))
  expect_match(rd$why, "reads its figure columns as", fixed = TRUE)
})

test_that("a line the recipe cannot account for holds the reading back", {
  # A row redacted but for its date: its amount is gone, and so is the proof.
  rows <- append(rc_rows, list(rc_line("10 Feb")), after = 3)
  rd <- recipe_read(rc_pdf(rc_one(rows)), rc_anz())
  expect_identical(rd$outcome, "check")
  expect_false(isTRUE(rd$checks$ok[rd$checks$check == "dated_lines_used"]))
  # A dated line with a figure printed outside the table (below its end).
  rd2 <- recipe_read(rc_pdf(rc_one(extra = c("", rc_line("27 Feb", "      LATE FEE", dr = "5.00", bal = "3,339.91")))), rc_anz())
  expect_identical(rd2$outcome, "check")
  expect_false(isTRUE(rd2$checks$ok[rd2$checks$check == "lines_accounted"]))
  # A line shaped like a transaction in another table (a summary box) is allowed
  # only while the printed opening and closing balances add up over the rows.
  owing <- "Interest owing as at 28 Feb 2026        12.34"
  box <- append(rc_box(), owing, after = 8)
  rd3 <- recipe_read(rc_pdf(rc_one(box = box)), rc_anz())
  expect_identical(rd3$outcome, "proven")
  expect_true(isTRUE(rd3$checks$ok[rd3$checks$check == "other_tables"]))
  box4 <- box[!grepl("^Closing balance", box)]
  rd4 <- recipe_read(rc_pdf(rc_one(box = box4)), rc_anz())
  expect_identical(rd4$outcome, "check")
  expect_false(isTRUE(rd4$checks$ok[rd4$checks$check == "other_tables"]))
})

test_that("the year is never guessed, and a sign the design never prints is not read", {
  # The period is not printed where the recipe says: a year-less date's year is
  # then not the statement's to give.
  nop <- rc_one(box = rc_box()[!grepl("Statement period", rc_box())])
  rd <- recipe_read(rc_pdf(nop), rc_anz())
  expect_identical(rd$outcome, "check")
  expect_false(isTRUE(rd$checks$ok[rd$checks$check == "dates_in_period"]))
  # "CR" on an everyday account's balance: this design never prints it.
  rows <- rc_rows
  rows[[6]] <- rc_line("26 Feb", "      CREDIT INTEREST PAID", cr = "2.18", bal = "3,344.91", mark = "CR")
  rd2 <- recipe_read(rc_pdf(rc_one(rows)), rc_anz())
  expect_identical(rd2$outcome, "check")
  expect_false(isTRUE(rd2$checks$ok[rd2$checks$check == "signs_settled"]))
})

test_that("a new account's first statement takes each row's year up to its period end", {
  rows <- list(
    rc_line("20 Dec", "      DEPOSIT", cr = "500.00", bal = "500.00"),
    rc_line("02 Jan", "EP    RIVERSIDE DAIRY", dr = "12.40", bal = "487.60"),
    rc_line("28 Feb", "      CREDIT INTEREST PAID", cr = "0.40", bal = "488.00"))
  st <- c(rc_box(period = "START - 28 Feb 2026", open = "0.00", close = "488.00"), rc_head, unlist(rows),
          rc_total("12.40", "500.40"), "", "Page 1 of 1")
  rd <- recipe_read(rc_pdf(st), rc_anz())
  expect_identical(rd$outcome, "proven")
  expect_identical(rd$transactions$date, c("2025-12-20", "2026-01-02", "2026-02-28"))
  expect_true(is.na(rd$parsed$header$period_start))      # START prints no start date
  expect_identical(rd$parsed$header$period_end, "28 Feb 2026")
})

# ---- files of several statements, and statements with no rows ----------------------------------

# A second month: two rows, so the running balance itself settles which way each
# figure goes (with one row, a reading and its exact negation both add up, and the
# automatic reader's search rightly calls that undecided).
rc_second <- function(open = "3,344.91") {
  o <- .num(open); a <- o - 10; b <- a + 25
  f <- function(v) formatC(v, format = "f", digits = 2, big.mark = ",")
  c(rc_box(period = "01 Mar 2026 to 31 Mar 2026", open = open, close = f(b)), rc_head,
    rc_line("05 Mar", "DD    CITY COUNCIL RATES", dr = "10.00", bal = f(a)),
    rc_line("09 Mar", "DC    REFUND", cr = "25.00", bal = f(b)), rc_total("10.00", "25.00"), "", "Page 1 of 1")
}

test_that("a file of several statements is read one statement at a time, and proven only when they follow on", {
  rd <- recipe_read(rc_pdf(rc_one(), rc_second()), rc_anz())
  expect_identical(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(rc_want, -10.00, 25.00))
  expect_identical(rd$transactions$statement_index, c(rep(1L, 6), 2L, 2L))
  expect_length(rd$statements, 2L)
  expect_true(isTRUE(rd$checks$ok[rd$checks$check == "statements_join"]))
  # The second statement does not open where the first closed: a statement may be
  # missing between them, so a person looks.
  gap <- recipe_read(rc_pdf(rc_one(), rc_second(open = "3,000.00")), rc_anz())
  expect_identical(gap$outcome, "check")
  expect_false(isTRUE(gap$checks$ok[gap$checks$check == "statements_join"]))
})

test_that("a statement with no rows is proven only when it says so and its balances agree", {
  empty <- function(open = "3,344.91", close = "3,344.91", say = TRUE) c(
    rc_box(open = open, close = close), rc_head,
    if (say) rc_line("", "      No transactions for this period"), "", "Page 1 of 1")
  rd <- recipe_read(rc_pdf(empty()), rc_anz())
  expect_identical(rd$outcome, "proven")
  expect_identical(nrow(rd$transactions), 0L)
  expect_true(isTRUE(rd$proof$empty))
  expect_identical(recipe_read(rc_pdf(empty(close = "3,000.00")), rc_anz())$outcome, "check")
  expect_identical(recipe_read(rc_pdf(empty(say = FALSE)), rc_anz())$outcome, "check")
})

# ---- integration ------------------------------------------------------------------------------

test_that("auto_read reads a recognised design with its recipe first, and the same file without one as before", {
  inp <- rc_pdf(rc_one())
  ar <- auto_read(inp, opts = list(recipes = rc_shipped()))
  expect_identical(ar$outcome, "proven")
  expect_identical(ar$matched_recipe, "anz_everyday_pdf@1")
  expect_match(ar$why, "Read with recipe", fixed = TRUE)
  expect_null(ar$matched_layout)
  plain <- auto_read(inp, opts = list(recipes = FALSE))
  expect_null(plain$matched_recipe)
  expect_null(plain$recipe_tried)
  # A file no recipe recognises is read exactly as with recipes switched off.
  other <- rc_pdf(rc_one(box = rc_box(glance = "Your account")))
  a1 <- auto_read(other, opts = list(recipes = rc_shipped())); a2 <- auto_read(other, opts = list(recipes = FALSE))
  a1$secs <- a2$secs <- NULL
  expect_identical(a1, a2)
  # A person's roles are a fix to the automatic reading: no recipe stands in.
  fx <- auto_read(inp, opts = list(recipes = rc_shipped(), roles = c("debit", "credit", "balance")))
  expect_null(fx$matched_recipe)
})

test_that("convert_statement carries the recipe to the result and the run log, and learns no layout from it", {
  # A recipe kept in the server's own folder (config paths$recipes), for the
  # design of a shipped fixture.
  d <- rc_tmpdir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rc_write(d, "kauri_fixture_pdf.yaml", c(
    "recipe: kauri_fixture_pdf", "format: 1", "version: 1", "bank: anz", "kind: pdf", "status: proven",
    "recognise: {all: [\"Statement of Accounts\", \"Transaction type and details\"]}",
    "period: {label: \"Statement period\"}",
    "table:",
    "  header: [\"Date\", \"Transaction type and details\", \"Withdrawals\", \"Deposits\", \"Balance\"]",
    "  columns:",
    "    date: {under: \"Date\"}",
    "    description: {under: \"Transaction type and details\"}",
    "    debit: {under: \"Withdrawals\"}",
    "    credit: {under: \"Deposits\"}",
    "    balance: {under: \"Balance\"}",
    "dates: {format: \"%d %b\", year: period}",
    "money: {style: debit_credit_cols, negative: [\"OD\"]}"))
  cfg <- file.path(d, "config.yaml")
  writeLines(c("paths:", sprintf("  recipes: \"%s\"", d)), cfg)
  before <- Sys.getenv("BSO_CONFIG", unset = NA_character_)
  Sys.setenv(BSO_CONFIG = cfg)
  on.exit(if (is.na(before)) Sys.unsetenv("BSO_CONFIG") else Sys.setenv(BSO_CONFIG = before), add = TRUE)
  cv <- convert_sandbox(); sd <- sandbox_dir(cv)
  r <- cv(fixture("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf"), bank = "ANZ")
  expect_identical(r$status, "ok")
  expect_identical(r$outcome, "proven")
  expect_identical(r$matched_recipe, "kauri_fixture_pdf@1")
  expect_identical(r$reading[[1]]$matched_recipe, "kauri_fixture_pdf@1")
  expect_identical(r$stamp$recipe, "kauri_fixture_pdf@1")
  expect_identical(r$learn[[1]]$action, "none")
  expect_match(r$learn[[1]]$why, "recipe kauri_fixture_pdf@1", fixed = TRUE)
  rec <- jsonlite::fromJSON(file.path(sd, "logs", "runs", paste0(r$run_id, ".json")))
  expect_identical(rec$recipe, "kauri_fixture_pdf@1")
  expect_identical(nrow(r$transactions %||% r$reading[[1]]$transactions), 6L)
})

test_that("statements printing no period start are put in order by their period end", {
  mk <- function(start, end, open, close) list(outcome = "proven",
    checks = data.frame(check = "ends_printed", ok = TRUE, why = "", stringsAsFactors = FALSE),
    parsed = list(header = list(period_start = start, period_end = end, opening_balance = open, closing_balance = close)))
  first <- mk(NA_character_, "30 Sep 2019", 0, 100)
  second <- mk("01 Oct 2019", "31 Oct 2019", 100, 100)
  third <- mk("01 Nov 2019", "30 Nov 2019", 100, 250)
  expect_true(.bundle_joins(list(third, first, second)))
  expect_false(.bundle_joins(list(first, mk("01 Oct 2019", "31 Oct 2019", 90, 100))))
})


# Two recipes that both read a statement and add up: the same figures -> the newest
# recipe; different figures -> a person decides (owner's rule, 5 Oct).
test_that("two recipes that both prove: same figures take the newer, different figures go to a person", {
  inp <- rc_pdf(rc_one())
  old <- rc_anz(); new <- rc_anz(); new$id <- "anz_new"; new$ref <- "anz_new@2"; new$version <- 2L
  r <- recipe_first(inp, opts = list(recipes = list(old, new)))
  expect_identical(r$outcome, "proven")
  expect_identical(r$matched_recipe, "anz_new@2")
})
