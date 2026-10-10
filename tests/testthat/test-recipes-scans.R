# test-recipes-scans.R -- recipes read OCR'd pages, and a PDF whose text layer is
# garbage is OCR'd instead of being read.
#
# A scan (or a page read by OCR because its text cannot be believed) is read with
# the recipe of its design, with the slack OCR needs in the WORDS that recognise
# the design and its table heading (one letter wrong in a long word, 0 for o, 1
# for l). Its figures are read as printed, and the statement's arithmetic decides
# exactly as for a text PDF: a recipe never makes a scan automatic that does not
# add up.
#
# The garbage text layer: a font with no Unicode map extracts glyph codes that
# come out as symbols and digits. Such a page reads as a picture would, by OCR.

sc_yaml <- function(id = "kauri_scan", extra = character(0)) c(
  sprintf("recipe: %s", id), "format: 1", "version: 1", "bank: anz", "kind: pdf", "status: proven",
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
  "  ends_at: [\"Totals at end of period\"]",
  "dates: {format: \"%d %b\", year: period}",
  "money: {style: debit_credit_cols}", extra)
sc_recipes <- function(lines = sc_yaml()) {
  d <- tempfile("recipes_"); dir.create(d)
  writeLines(lines, file.path(d, "r.yaml"))
  rc <- recipes_load(d); unlink(d, recursive = TRUE)
  rc
}

# The statement as cells: text, x (points), left or right aligned.
sc_cells <- list(
  list(c("ANZ", 40, "l"), c("Account statement", 400, "l")),
  list(c("Account at a glance", 40, "l"), c("Statement date 28 Feb 2026", 400, "l")),
  list(c("Statement period 01 Feb 2026 to 28 Feb 2026", 40, "l")),
  list(c("Opening balance", 40, "l"), c("1,000.00", 250, "r")),
  list(c("Closing balance", 40, "l"), c("3,344.91", 250, "r")),
  list(),
  list(c("Date", 40, "l"), c("Transaction type and details", 95, "l"), c("Withdrawals", 380, "r"),
       c("Deposits", 460, "r"), c("Balance", 540, "r")),
  list(c("03 Feb", 40, "l"), c("EP RIVERSIDE DAIRY", 95, "l"), c("12.40", 380, "r"), c("987.60", 540, "r")),
  list(c("05 Feb", 40, "l"), c("DC SALARY MATAI HOLDINGS", 95, "l"), c("3,120.00", 460, "r"), c("4,107.60", 540, "r")),
  list(c("09 Feb", 40, "l"), c("DD CITY COUNCIL RATES", 95, "l"), c("268.15", 380, "r"), c("3,839.45", 540, "r")),
  list(c("14 Feb", 40, "l"), c("AP TRANSFER TO SAVINGS", 95, "l"), c("400.00", 380, "r"), c("3,439.45", 540, "r")),
  list(c("21 Feb", 40, "l"), c("VT HARBOUR FUEL", 95, "l"), c("96.72", 380, "r"), c("3,342.73", 540, "r")),
  list(c("26 Feb", 40, "l"), c("CREDIT INTEREST PAID", 95, "l"), c("2.18", 460, "r"), c("3,344.91", 540, "r")),
  list(c("Totals at end of period", 95, "l"), c("777.27", 380, "r"), c("3,122.18", 460, "r")),
  list(), list(c("Page 1 of 1", 40, "l")))
sc_want <- c(-12.40, 3120.00, -268.15, -400.00, -96.72, 2.18)

# sc_draw(cells, scramble) -- the cells drawn on the current grid page; with
# scramble, every letter and digit is drawn as a symbol instead (the text a font
# with no Unicode map extracts).
sc_draw <- function(cells, scramble = FALSE) {
  garble <- function(s) {
    cp <- utf8ToInt(s); al <- grepl("[[:alnum:]]", strsplit(s, "")[[1]])
    cp[al] <- utf8ToInt("!#%&()*+;<=>?@[]^_{|}~")[1L + cp[al] %% 22L]
    intToUtf8(cp)
  }
  for (i in seq_along(cells)) for (cl in cells[[i]])
    grid::grid.text(if (scramble) garble(cl[1]) else cl[1], x = grid::unit(as.numeric(cl[2]), "points"),
                    y = grid::unit(800 - i * 16, "points"), just = if (cl[3] == "l") "left" else "right",
                    gp = grid::gpar(fontsize = 10))
}
sc_text_pdf <- function(cells = sc_cells) {
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p, width = 595 / 72, height = 842 / 72)
  grid::grid.newpage(); sc_draw(cells)
  grDevices::dev.off()
  p
}
# The statement as a picture only: no text layer at all (a non-selectable PDF).
sc_scan_pdf <- function(cells = sc_cells) {
  img <- magick::image_read_pdf(sc_text_pdf(cells), density = 200)
  p <- tempfile(fileext = ".pdf")
  magick::image_write(img, p, format = "pdf", density = "200x200")
  p
}
# The statement as a picture with a garbage text layer over it.
sc_garbage_pdf <- function(cells = sc_cells) {
  img <- magick::image_read_pdf(sc_text_pdf(cells), density = 300)
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p, width = 595 / 72, height = 842 / 72)
  grid::grid.newpage()
  # The garbage text first, the picture over it: the text layer is there to be
  # extracted, and the page shows only the picture.
  sc_draw(cells, scramble = TRUE)
  grid::grid.raster(as.raster(img), width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = FALSE)
  grDevices::dev.off()
  p
}

# OCR'd words without OCR: a text statement whose words are given as OCR read them.
sc_words <- function(edit = NULL, ocr = TRUE) {
  inp <- read_input(sc_text_pdf())
  for (k in names(edit)) {
    inp$words[[1]]$text[inp$words[[1]]$text == k] <- edit[[k]]
    inp$pages[1] <- gsub(sprintf("(?<![[:alnum:].,])%s(?![[:alnum:]])", gsub(".", "[.]", k, fixed = TRUE)), edit[[k]], inp$pages[1], perl = TRUE)
  }
  inp$page_ocr <- rep(ocr, length(inp$words))
  inp
}

test_that("an OCR'd page is recognised and read with its recipe, its wording allowed OCR's slack", {
  rc <- sc_recipes()
  # "Withdrawals" read as "Withdrawa1s", "glance" as "glancc": the heading and the
  # recognising phrase still stand.
  inp <- sc_words(list(Withdrawals = "Withdrawa1s", glance = "glancc"))
  rg <- recipe_recognise(inp, rc)
  expect_false(is.null(rg$recipe))
  rd <- recipe_first(inp, opts = list(recipes = rc))
  expect_identical(rd$outcome, "proven")
  expect_equal(rd$transactions$amount, sc_want)
  expect_identical(rd$matched_recipe, "kauri_scan@1")
})

test_that("the slack is for OCR'd pages only, and never for a short word", {
  rc <- sc_recipes()
  # The same misreadings on a text PDF are what the statement prints: not this design.
  expect_null(recipe_recognise(sc_words(list(glance = "glancc"), ocr = FALSE), rc)$recipe)
  # One letter wrong, missing or extra in a word of four letters or more; a word
  # of three or fewer must be exact ("and" in the heading never becomes "end").
  expect_identical(.rc_tok_match(c("date", "data", "dat", "datee", "dta"), "date"), c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(.rc_tok_match(c("and", "end", "an"), "and"), c(TRUE, FALSE, FALSE))
  expect_identical(.rc_tok_match(c("o1d", "0ld"), "old"), c(TRUE, TRUE))
  # Two letters wrong is another word.
  expect_null(recipe_recognise(sc_words(list(glance = "gxancc")), rc)$recipe)
})

test_that("a must-not-appear phrase misread by OCR still rules the recipe out", {
  y <- sc_yaml(); y[grepl("^recognise:", y)] <-
    "recognise: {all: [\"Account at a glance\"], none: [\"Totals at end of period\"]}"
  rc <- sc_recipes(y)
  expect_null(recipe_recognise(sc_words(), rc)$recipe)
  expect_null(recipe_recognise(sc_words(list(period = "perlod")), rc)$recipe)
})

test_that("an OCR'd statement that does not add up is never proven by its recipe", {
  rc <- sc_recipes()
  # OCR reads 268.15 as 263.15: the running balance breaks, the arithmetic says no.
  rd <- recipe_first(sc_words(list("268.15" = "263.15")), opts = list(recipes = rc))
  expect_false(identical(rd$outcome, "proven"))
})

test_that("a non-selectable (picture-only) statement is OCR'd and proven with its recipe", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  skip_if_not(requireNamespace("magick", quietly = TRUE))
  p <- sc_scan_pdf(); on.exit(unlink(p), add = TRUE)
  expect_equal(sum(nchar(pdftools::pdf_text(p))), 0L)
  inp <- read_input(p)
  expect_true(isTRUE(inp$page_ocr[1]))
  rd <- recipe_first(inp, opts = list(recipes = sc_recipes()))
  expect_identical(rd$outcome, "proven")
  expect_equal(rd$transactions$amount, sc_want)
})

# ---- a garbage text layer ---------------------------------------------------------

test_that("text that does not read as words is a garbage text layer; real statements are not", {
  garble <- function(s) { cp <- utf8ToInt(s); al <- grepl("[[:alnum:]]", strsplit(s, "")[[1]])
    cp[al] <- utf8ToInt("!#%&()*+;<=>?@[]^_{|}~")[1L + cp[al] %% 22L]; intToUtf8(cp) }
  good <- paste(rep(c("03 Feb EP RIVERSIDE DAIRY 12.40 987.60", "Opening balance 1,000.00",
                      "Statement period 01 Feb 2026 to 28 Feb 2026", "Account number 01-0021-2348037-00",
                      "NZD PS AT DR CR 4.00- REF1001 Page 1 of 3"), 4), collapse = "\n")
  expect_gt(.text_word_share(good), 0.9)
  expect_lt(.text_word_share(garble(good)), 0.4)
  wb <- function(n) data.frame(text = rep("x", n), stringsAsFactors = FALSE)
  expect_false(page_needs_ocr(good, wb(80)))
  # Word boxes present do not save it: the words are not the page.
  expect_true(page_needs_ocr(garble(good), wb(80)))
  # Too few words to tell: left to the other tests (a short page is not judged).
  expect_true(is.na(.text_word_share("#$% &'( )*+")))
  # Another script reads as words (a bilingual statement's Chinese labels).
  zh <- paste(rep(c("币种 NZD", "上期余额 0.33", "Balance 0.36"), 12), collapse = " ")
  expect_gt(.text_word_share(zh), 0.9)
})

test_that("a PDF whose text layer is garbage is OCR'd, never read as text, and proven with its recipe", {
  skip_if_not(requireNamespace("magick", quietly = TRUE))
  p <- sc_garbage_pdf(); on.exit(unlink(p), add = TRUE)
  layer <- pdftools::pdf_text(p)[1]
  expect_gt(nchar(layer), 200L)                         # there IS a text layer...
  expect_true(page_needs_ocr(layer, pdftools::pdf_data(p)[[1]]))   # ...and it is not believed
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  inp <- read_input(p)
  expect_true(isTRUE(inp$page_ocr[1]))
  expect_false(grepl("#$", inp$pages[1], fixed = TRUE))
  expect_match(inp$pages[1], "RIVERSIDE", fixed = TRUE)
  rd <- recipe_first(inp, opts = list(recipes = sc_recipes()))
  expect_identical(rd$outcome, "proven")
  expect_equal(rd$transactions$amount, sc_want)
})
