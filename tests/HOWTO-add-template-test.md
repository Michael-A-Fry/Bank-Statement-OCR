# How to add a statement and its golden test

Since 2.0.0 there is nothing to add to the engine for a new bank or a new layout:
the automatic reader reads a statement from its content and proves it with the
statement's own arithmetic, and a server learns each bank's layouts by itself. What
you add here is a **regression test**: a synthetic statement in that layout, and
the table it must read to. If a later change to the reader breaks the layout, the
suite fails before any server sees it.

(The file keeps its old name because other pages link to it.)

Two paths, same shape. Steps 2, 3 and 4 are shared; step 1 (the fixture) is where
delimited and PDF differ.

| | Delimited (CSV/TSV) / Excel | PDF |
|---|---|---|
| Fixture | a small **synthetic** export under `samples/raw/<bank>/` | a synthetic PDF built by `tests/testthat/fixtures/make_pdf_fixtures.R` |
| Golden | `tests/testthat/expected/<name>.csv` | same |
| Test | a new `tests/testthat/test-<name>.R` | one entry in `PDF_GOLDENS` in `tests/testthat/test-pdf_template_goldens.R` |

**Never use a real customer statement as a fixture.** Fixtures are committed and
copied to every server.

## 1a. Make the fixture: delimited / Excel
Write a short export under `samples/raw/<bank>/` with the bank's real column
headings and order, and invented rows. Keep a running balance in it if the bank's
export has one: that is what lets the reader prove it.

## 1b. Make the fixture: PDF
Add a generator function to `tests/testthat/fixtures/make_pdf_fixtures.R`, alongside
`make_anz()` / `make_asb()` / `make_westpac()`, and run it:

```
Rscript tests/testthat/fixtures/make_pdf_fixtures.R
```

It writes `tests/testthat/fixtures/<name>_sample.pdf`. Follow the existing functions:

- **Copy the bank's real geometry.** `open_a4()` sets up an A4 page in points;
  place each column where the bank prints it, so the test exercises the layout the
  reader will really meet.
- **Invent everything**: a fictional bank, people, accounts and figures.
- **Make it add up.** Use `running_balance()` and print the opening and closing
  balances, so the reader can prove the reading and the test checks the proof, not
  just the column split.
- **Add the noise the real statement has**: a summary box, an "Upcoming payments"
  table, a notes page. Those are what break readers.

Commit the generated `.pdf` as well as the generator: the generator documents how
the fixture was made, the PDF is what the test reads.

## 2. Generate and EYEBALL the golden table
Read the fixture with the automatic reader, given nothing learned, and write the
table it produces:

```r
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
rd <- auto_read(read_input("samples/raw/<bank>/<fixture>"))   # or the .pdf from step 1b
rd$outcome; rd$why
dir.create("tests/testthat/expected", showWarnings = FALSE, recursive = TRUE)
write.csv(coerce_core(rd$transactions),
          "tests/testthat/expected/<name>.csv", row.names = FALSE, na = "")
```

- `rd$outcome` should be **`proven`** for a statement with a running balance or
  printed totals. If it is `check`, read `rd$why` before going on: either the
  fixture does not add up (fix the fixture), or the reader has a real gap (that is
  a finding, not a golden).
- A statement with no balance and no totals cannot prove itself; `check` is the
  right answer for it, and its golden still pins the figures a person is shown.

Then open `tests/testthat/expected/<name>.csv` and check it **by eye** against the
fixture. **This is the only step where a person decides what "correct" means**;
everything afterwards only proves the reader keeps agreeing with what you signed
off. Check that dates are ISO, money out is negative and money in positive, no
description is cut short or merged with its neighbour, and the `flags` column says
what you expect (for example `date_carried` where a date is printed once per day).

## 3. Write the test

**Delimited / Excel**: create `tests/testthat/test-<name>.R` with the shared helper
from `tests/testthat/helper.R`:

```r
FIXTURE  <- "samples/raw/<bank>/<fixture>"
EXPECTED <- "tests/testthat/expected/<name>.csv"

test_that("the automatic reader proves the export, with the golden figures", {
  expect_auto_read_golden(FIXTURE, EXPECTED, outcomes = "proven")
})
```

`expect_auto_read_golden(fixture, expected, outcomes, fields)` reads the fixture with
the automatic reader and nothing learned, requires one of `outcomes`, and compares
the `fields` (date, amount, direction, balance, description by default) to the
golden. Whatever `outcomes` allows, an automatic outcome with any figure off the
golden fails: that is the silently wrong answer the product must never give. For a
statement that cannot prove itself, use `outcomes = "check"`.

**PDF**: add one entry to `PDF_GOLDENS` at the top of
`tests/testthat/test-pdf_template_goldens.R`; the loop below it builds the test:

```r
  list(id = "<name>",
       fx  = "tests/testthat/fixtures/<name>_sample.pdf",
       exp = "tests/testthat/expected/<name>.csv")
```

The loop requires the automatic reader to prove the fixture to the golden. (The
three entries that came from 1.x also check the old table reader with their fixture
template; a new entry needs no template.)

## 4. Prove it
Run the single shared runner and confirm everything still passes:

```
Rscript tests/run_tests.R
```

**On the server**, `Rscript` is not on PATH: the app's private R is deliberately
unregistered. Use the exact command in
[`docs/operational/maintaining-the-engine.md`](../docs/operational/maintaining-the-engine.md).

A new golden is not "done" until it passes, every other test still passes, and the
board says `skipped: 0`, because a skipped test proved nothing.
