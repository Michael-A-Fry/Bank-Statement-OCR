# Tests for the declarative label dictionary + matcher (R/labels.R).
# This is the generic answer to "hundreds of different wordings" for labelled
# scalar values. All pattern/config-driven -- no bank-specific logic.

test_that("synonyms: any_of matches whichever wording a statement uses", {
  spec <- list(any_of = c("opening balance", "balance brought forward",
                          "starting balance"), value = "money")
  expect_equal(match_label(spec, "Opening balance            $1,000.00")$value, "$1,000.00")
  expect_equal(match_label(spec, "Balance brought forward     $2,500.50")$value, "$2,500.50")
  expect_equal(match_label(spec, "STARTING BALANCE: $3,000.00")$value, "$3,000.00")
  # absent -> NA, not an error, and matched is FALSE
  r <- match_label(spec, "no balance line here")
  expect_true(is.na(r$value)); expect_false(r$matched)
})

test_that("value sits on the label line, else the next line", {
  spec <- list(any_of = "closing balance", value = "money")
  # same line
  expect_equal(match_label(spec, "Closing balance  $9.99")$value, "$9.99")
  # value on the following line (label is a heading)
  pages <- "Closing balance\n$42.00"
  expect_equal(match_label(spec, pages)$value, "$42.00")
})

test_that("occurrence controls which of several matches is returned", {
  pages <- c("Fee $1.00", "Fee $2.00", "Fee $3.00")
  base <- list(any_of = "Fee", value = "money")
  expect_equal(match_label(c(base, list(occurrence = "first")), pages)$value, "$1.00")
  expect_equal(match_label(c(base, list(occurrence = "last")),  pages)$value, "$3.00")
  expect_equal(match_label(c(base, list(occurrence = "all")),   pages)$value, "$1.00; $2.00; $3.00")
})

test_that("conflicting repeats are flagged (never silently guessed)", {
  pages <- c("Total $10.00", "Total $99.00")
  r <- match_label(list(any_of = "Total", value = "money"), pages)   # default on_conflict: flag
  expect_true(r$conflict)
  expect_equal(r$value, "$10.00")                    # keeps first, but conflict=TRUE
  r2 <- match_label(list(any_of = "Total", value = "money", on_conflict = "last"), pages)
  expect_equal(r2$value, "$99.00")
})

test_that("where scopes the search to a page", {
  pages <- c("Statement date 01/02/2025 page one",
             "Statement date 09/09/2099 back page")
  expect_equal(match_label(list(any_of = "Statement date", value = "date",
                                where = "page1"), pages)$value, "01/02/2025")
  expect_equal(match_label(list(any_of = "Statement date", value = "date",
                                where = "last_page"), pages)$value, "09/09/2099")
})

test_that("value types: text keeps the verbatim remainder; regex/date extract", {
  expect_equal(match_label(list(any_of = "Account name", value = "text"),
                           "Account name: O'Connor & Sons")$value, "O'Connor & Sons")
  expect_equal(match_label(list(any_of = "IRD", pattern = "[0-9]{2,3}-[0-9]{3}-[0-9]{3}"),
                           "IRD number 123-456-789")$value, "123-456-789")
  expect_equal(match_label(list(any_of = "Period", value = "date_range"),
                           "Period 1 April 2025 to 31 March 2026")$value,
               "1 April 2025 | 31 March 2026")
})

test_that("a bare string or {label:} spec still works (back-compat)", {
  expect_equal(match_label("opening balance", "Opening balance $5.00")$value, "$5.00")
  expect_equal(match_label(list(label = "opening balance"),
                           "Opening balance $6.00")$value, "$6.00")
})

test_that("the shipped base dictionary loads and carries synonyms", {
  d <- default_label_dict()
  skip_if(length(d) == 0)   # only meaningful when the yaml resolves
  expect_true("opening_balance" %in% names(d))
  expect_true(any(grepl("brought forward", .spec_terms(d$opening_balance))))
})

test_that("extract_fields flags a required-but-missing field", {
  tmpl <- list(fields = list(
    total = list(any_of = "Grand total", value = "money", required = TRUE)))
  f <- extract_fields(list(pages = "nothing relevant here"), tmpl, dict = list())
  expect_true(f$flagged[f$field == "total"])
  expect_false(f$matched[f$field == "total"])
})

test_that("extract_fields inherits dictionary synonyms by field name", {
  dict <- list(opening_balance = list(
    any_of = c("opening balance", "balance brought forward"), value = "money"))
  tmpl <- list(fields = list(opening_balance = list()))   # no wording of its own
  f <- extract_fields(list(pages = "Balance brought forward   $77.00"), tmpl, dict = dict)
  expect_equal(f$value[f$field == "opening_balance"], "$77.00")
})

# ---------------------------------------------------------------------------
# N1xx: THE ENGINE MUST GIVE THE SAME ANSWER IN EVERY LOCALE IT RUNS IN.
#
# R/labels.R:109 held an EN DASH inside a regex character class. In a C (ASCII)
# locale -- the container's default, and the deployment locale docs/design.md
# names -- a multibyte character in a bracket expression is read one byte at a
# time, so `sub("^[ :<en-dash>-]+", "", rest)` ATE the leading byte of any
# non-ASCII value that followed a label: "Account name: <euro>1,234.00" came back
# as an invalid-UTF-8 string that was silently no longer verbatim, and a
# UTF-8-marked line threw "unable to translate ... to a wide string" outright.
#
# Writing the glyph as an escape does NOT fix it -- measured, the byte is still
# eaten -- so these tests are about BEHAVIOUR IN BOTH LOCALES, not about spelling.
# The spelling is held separately below, because an ASCII source file is what
# survives the trip to an air-gapped Windows box.
# ---------------------------------------------------------------------------

# .in_locale(loc, expr_text) -- run a snippet in a CHILD R process pinned to one
# locale, and return what it printed. A child, because Sys.setlocale in-process
# leaks into every later test in the file; and the engine is re-sourced there so
# the snippet exercises the real functions, not a copy.
.in_locale <- function(loc, expr_text) {
  scr <- tempfile(fileext = ".R")
  writeLines(c(sprintf('root <- "%s"', engine_root()),
               'for (f in list.files(file.path(root, "R"), pattern = "\\\\.R$", full.names = TRUE)) source(f)',
               expr_text), scr)
  out <- suppressWarnings(system2("Rscript", scr, stdout = TRUE, stderr = TRUE,
                                  env = paste0("LC_ALL=", loc)))
  paste(out, collapse = "\n")
}

.LOCALES <- c("C", "C.utf8")

test_that("a non-ASCII value after a label survives byte for byte in EVERY locale", {
  skip_if_not(nzchar(Sys.which("Rscript")))
  # the euro sign, built from bytes so this test file stays ASCII too
  snippet <- paste(
    'euro <- rawToChar(as.raw(c(0xe2, 0x82, 0xac)))',
    'v <- .text_after(paste0("Account name: ", euro, "1,234.00"), "Account name")',
    'cat(paste(sprintf("%02x", as.integer(charToRaw(v))), collapse = ""), validUTF8(v), "\\n")',
    sep = "\n")
  got <- vapply(.LOCALES, function(l) trimws(.in_locale(l, snippet)), character(1))
  # the euro's three bytes are all still there, in every locale, and the result is
  # still valid UTF-8 -- this is the verbatim-content guarantee, in bytes
  for (l in .LOCALES)
    expect_identical(got[[l]], "e282ac312c3233342e3030 TRUE", info = l)
  # ...and the two locales agree, which is the property that actually matters
  expect_identical(length(unique(got)), 1L)
})

test_that("a UTF-8-marked label line does not THROW in a C locale", {
  skip_if_not(nzchar(Sys.which("Rscript")))
  snippet <- paste(
    'l <- paste0("Account name: ", intToUtf8(0x20AC), "1,234.00")   # UTF-8 marked',
    'r <- tryCatch(.text_after(l, "Account name"),',
    '              error = function(e) paste("THREW:", conditionMessage(e)))',
    'cat(paste(sprintf("%02x", as.integer(charToRaw(r))), collapse = ""), "\\n")',
    sep = "\n")
  for (l in .LOCALES)
    expect_identical(trimws(.in_locale(l, snippet)), "e282ac312c3233342e3030", info = l)
})

test_that("a printed balance is read the same whatever currency symbol it carries", {
  skip_if_not(nzchar(Sys.which("Rscript")))
  # .MINUS had the same fault, one line up: a bracket expression holding the two
  # non-ASCII minus signs. Under LC_ALL=C a GBP line came back NA -- the balance
  # the statement prints simply went missing, with only a console warning.
  snippet <- paste(
    'gbp <- rawToChar(as.raw(c(0xc2, 0xa3)))',
    'cat(.value_from_line(paste0("Opening Balance ", gbp, "1,234.56"), "money"), "\\n")',
    sep = "\n")
  for (l in .LOCALES)
    expect_identical(trimws(.in_locale(l, snippet)), "1,234.56", info = l)
})

test_that("the ordinary ASCII label line is untouched by all of this", {
  expect_identical(.text_after("Account name: O'Connor & Sons", "Account name"),
                   "O'Connor & Sons")
  expect_identical(.text_after("Account name - Smith Ltd", "Account name"), "Smith Ltd")
  # the en dash is still stripped, which is what the character class was for
  expect_identical(.text_after(paste0("Account name ", intToUtf8(0x2013), " Smith Ltd"),
                               "Account name"), "Smith Ltd")
})

# ---------------------------------------------------------------------------
# N1xx: NO NON-ASCII BYTE ANYWHERE IN THE ENGINE, literals and comments alike.
#
# test-app-adoption.R has held app.R / ui_labels.R / ui_content.R to this for a
# while. It never covered R/ -- which is where the engine that produces the
# FIGURES lives, and where an en dash in a character class was quietly eating
# bytes off customer values. The scan lives here because R/labels.R is the module
# that was broken by one, and because a rule that is only enforced on the screen
# files is a rule about decoration rather than about correctness.
# ---------------------------------------------------------------------------

.engine_r_files <- function() {
  c(list.files(file.path(engine_root(), "R"), pattern = "\\.R$", full.names = TRUE),
    file.path(engine_root(), c("run.R", "tests/run_tests.R")))
}

.nonascii_in <- function(path) {
  ln <- readLines(path, warn = FALSE)
  bad <- grep("[^ -~\t]", ln, useBytes = TRUE)
  if (!length(bad)) return(character(0))
  sprintf("%s:%d %s", basename(path), bad, substr(ln[bad], 1, 90))
}

test_that("no non-ASCII byte survives anywhere in R/, run.R or the test runner", {
  files <- .engine_r_files()
  files <- files[file.exists(files)]
  expect_gt(length(files), 40L)                    # the scan must not go quiet
  offenders <- unlist(lapply(files, .nonascii_in))
  expect_identical(offenders, character(0),
                   info = paste("non-ASCII byte:", paste(offenders, collapse = " | ")))
})

test_that("the scan really does catch a byte, so a clean board means something", {
  p <- tempfile(fileext = ".R")
  writeBin(charToRaw(paste0("x <- \"", rawToChar(as.raw(c(0xe2, 0x80, 0x93))), "\"\n")), p)
  expect_gt(length(.nonascii_in(p)), 0L)
})

test_that("the glyphs that were escaped are still THERE, as escapes", {
  # A file that passed the scan by DELETING the character it needed would be worse
  # than the one it replaced. R/labels.R must still strip an en dash and a minus,
  # and R/read_pdf.R must still know its block glyphs.
  lab <- paste(readLines(file.path(engine_root(), "R/labels.R"), warn = FALSE), collapse = "\n")
  for (esc in c("\\\\u2013", "\\\\u2212")) expect_match(lab, esc)
  rp <- paste(readLines(file.path(engine_root(), "R/read_pdf.R"), warn = FALSE), collapse = "\n")
  for (esc in c("\\\\u2588", "\\\\u25a0")) expect_match(rp, esc)
})

# ---------------------------------------------------------------------------
# N1xx: THE THREE ENTRY POINTS MUST AGREE ABOUT THE LOCALE.
#
# app.R forced a UTF-8 locale; run.R and tests/run_tests.R did not. So the CLI the
# incident procedure tells a maintainer to reproduce a disputed conversion with
# ran WITHOUT the guard the app has -- the re-run could differ from the run it was
# meant to reproduce, and the procedure would call that difference a finding. And
# the suite proved the engine worked in the one locale the app never uses.
# ---------------------------------------------------------------------------
test_that("app.R, run.R and the test runner all force a UTF-8 locale", {
  for (f in c("app.R", "run.R", "tests/run_tests.R")) {
    p <- file.path(engine_root(), f)
    skip_if_not(file.exists(p))
    src <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_match(src, "Sys.setlocale\\(\"LC_CTYPE\"", info = f)
    expect_match(src, "C.UTF-8", fixed = TRUE, info = f)
  }
})
