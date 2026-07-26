# Delimited reader resilience + completeness (build-contract sections 11.3/11.5).
# Ragged rows must be ISOLATED and flagged 'malformed' without corrupting their
# clean neighbours; merged/lost records must be caught by no_unparsed_rows;
# multi-line quoted records must keep per-row provenance; malformed quoting must
# never crash the whole statement.

.bnz_tmpl <- function() {
  load_templates(templates_dir())[["bnz_everyday_csv"]]
}

test_that("safe_readlines transcodes non-UTF-8 input, keeping descriptions verbatim (P2-5)", {
  # Windows-1252 export: 0xE9 = é, 0xA3 = £. readLines(encoding="UTF-8") would tag
  # these as UTF-8 without transcoding -> invalid/mojibake. Must come back as the
  # real characters, valid UTF-8.
  tf <- tempfile(fileext = ".csv")
  writeBin(as.raw(c(0x44,0x61,0x74,0x65,0x2C,0x50,0x61,0x79,0x65,0x65,0x0A,          # Date,Payee
                    0x30,0x31,0x2C,0x43,0x61,0x66,0xE9,0x20,0xA3,0x35,0x0A)), tf)     # 01,Café £5
  ln <- safe_readlines(tf)
  expect_length(ln, 2)
  expect_true(validUTF8(ln[2]))
  expect_true(all(c(233L, 163L) %in% utf8ToInt(ln[2])))   # é (233) and £ (163) intact

  # a UTF-8 BOM is still stripped from the first header (unchanged behaviour).
  tf2 <- tempfile(fileext = ".csv")
  writeBin(as.raw(c(0xEF,0xBB,0xBF,0x44,0x61,0x74,0x65,0x0A,0x31,0x0A)), tf2)
  expect_identical(safe_readlines(tf2)[1], "Date")

  # plain ASCII / valid UTF-8 is unchanged.
  tf3 <- tempfile(fileext = ".csv"); writeLines(c("Date,Amount", "01/01/2025,5.00"), tf3)
  expect_identical(safe_readlines(tf3), c("Date,Amount", "01/01/2025,5.00"))

  # a UTF-16LE BOM file is decoded, not garbled.
  tf4 <- tempfile(fileext = ".csv")
  writeBin(c(as.raw(c(0xFF,0xFE)),
             as.raw(as.vector(rbind(utf8ToInt("Date\n1\n"), 0L)))), tf4)
  expect_identical(safe_readlines(tf4), c("Date", "1"))
})

test_that("a too-LONG row is flagged malformed and neighbours stay intact", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_ragged_long.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  tx <- p$transactions
  expect_equal(nrow(tx), 3L)
  # offending row 1 flagged; clean rows carry no flags
  expect_true(grepl("malformed", tx$flags[1]))
  expect_identical(tx$flags[2:3], c("", ""))
  # neighbours parsed byte-for-byte correctly (no column shift)
  expect_equal(tx$amount[2:3], c(-100, 11.5))
  expect_identical(tx$date[2:3], c("2026-01-02", "2026-01-03"))
  expect_identical(tx$description[2:3], c("Savings", "Friend Share"))
  # completeness KPI fails (a row could not be accounted for cleanly)
  k <- reconcile(p, .bnz_tmpl())$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "fail")
})

test_that("a too-SHORT row is flagged malformed and neighbours stay intact", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_ragged_short.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  tx <- p$transactions
  expect_equal(nrow(tx), 2L)
  expect_true(grepl("malformed", tx$flags[1]))
  expect_identical(tx$flags[2], "")
  expect_equal(tx$amount[2], -100)
  expect_identical(tx$description[2], "Savings")
  k <- reconcile(p, .bnz_tmpl())$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "fail")
})

test_that("an embedded-newline quoted record stays one row with provenance", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_embedded_newline.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  tx <- p$transactions
  expect_equal(nrow(tx), 2L)
  # description preserved verbatim, including the embedded newline
  expect_identical(tx$description[1], "OConnor & Sons\nLTD")
  expect_equal(tx$amount, c(-25, 11.5))
  # provenance is populated per row (NOT blanked); the multi-line span recorded
  expect_equal(nrow(p$provenance), 2L)
  expect_false(any(is.na(p$provenance$source_ref)))
  expect_false(any(is.na(p$provenance$raw)))
  expect_identical(p$provenance$source_ref[1], "csv:line=2-3")
})

test_that("a cross-line stray quote that merges records is caught, not silent", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_merged_quote.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  k <- reconcile(p, .bnz_tmpl())$kpis
  row <- k[k$name == "no_unparsed_rows", ]
  # 3 non-empty source data lines but fewer good rows -> completeness FAILS
  expect_equal(row$status, "fail")
  expect_equal(as.integer(row$expected), 3L)
  expect_gt(3L - as.integer(row$actual), 0L)
})

test_that("an unbalanced quote never fails the whole statement", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_merged_quote.csv"))
  # convert_statement must never throw; it degrades to needs_review, not failed.
  expect_error(parse_statement(input, .bnz_tmpl()), NA)
})

# ---------------------------------------------------------------------------
# Spreadsheet PADDING lines (bare separators, no content).
# A CSV saved out of a spreadsheet is routinely padded to a fixed block, so a
# two-transaction statement arrives with two dozen ",,,,,," lines. Those used to
# become empty transactions, each flagged 'malformed' -- which is worse than
# useless: it buries a genuinely malformed row and puts invented rows in the
# workbook. They are now ignored exactly like an empty line, COUNTED, and excluded
# from the completeness proof so it still compares like with like.
# ---------------------------------------------------------------------------

test_that("bare-separator lines are counted as padding, not parsed as rows", {
  tf <- tempfile(fileext = ".csv")
  writeLines(c("Date,Payee,Particulars,Code,Reference,Amount",
               "01/01/2026,Shop,P,C,R,-25.00",
               ",,,,,",
               "  ,, , ,,  ",
               "02/01/2026,Bank,P,C,R,11.50",
               ",,,,,"), tf)
  r <- read_delimited(read_input(tf), .bnz_tmpl())
  expect_equal(nrow(r$table), 2L)
  expect_equal(r$n_data_lines, 2L)          # the proof counts only real records
  expect_equal(r$n_padding_lines, 3L)       # ...and the omission is reported
  expect_identical(r$table$Amount, c("-25.00", "11.50"))

  p <- parse_statement(read_input(tf), .bnz_tmpl())
  expect_equal(nrow(p$transactions), 2L)
  expect_true(all(p$transactions$flags == ""))
  expect_equal(p$padding_line_count, 3L)
  k <- reconcile(p, .bnz_tmpl())$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
})

test_that("a line with ANY content is still kept and flagged, never called padding", {
  # The guard against over-reach. Only a line with nothing but separators and
  # whitespace is padding; a line carrying so much as one character is a record,
  # is kept, and is flagged if it does not parse -- the fail-closed path is intact.
  tf <- tempfile(fileext = ".csv")
  writeLines(c("Date,Payee,Particulars,Code,Reference,Amount",
               "01/01/2026,Shop,P,C,R,-25.00",
               ",,,,,x",                      # one stray character
               ",,,,,\"\""), tf)              # an explicitly EMPTY quoted field
  r <- read_delimited(read_input(tf), .bnz_tmpl())
  expect_equal(nrow(r$table), 3L)
  expect_equal(r$n_padding_lines, 0L)
  p <- parse_statement(read_input(tf), .bnz_tmpl())
  expect_equal(nrow(p$transactions), 3L)
  expect_true(all(grepl("malformed", p$transactions$flags[2:3])))
})

test_that("padding never masks a genuinely LOST record (completeness still fails)", {
  # A stray cross-line quote merges two records into one. Even with padding lines
  # in the same file, the completeness KPI must still catch the loss.
  tf <- tempfile(fileext = ".csv")
  writeLines(c("Date,Payee,Particulars,Code,Reference,Amount",
               "01/01/2026,\"Shop,P,C,R,-25.00",
               "02/01/2026,Bank,P,C,R,11.50\"",
               "03/01/2026,Cafe,P,C,R,-3.00",
               ",,,,,"), tf)
  p <- parse_statement(read_input(tf), .bnz_tmpl())
  expect_equal(p$padding_line_count, 1L)
  k <- reconcile(p, .bnz_tmpl())$kpis
  row <- k[k$name == "no_unparsed_rows", ]
  expect_equal(row$status, "fail")
  expect_equal(as.integer(row$expected), 3L)   # padding excluded from the count
})
