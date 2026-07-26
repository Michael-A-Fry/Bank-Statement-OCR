# How to add a template and its golden test

Every bank template in `templates/` ships with a golden-file test so parsing stays
reproducible — and that is enforced: the guard test in
`tests/testthat/test-pdf_template_goldens.R` fails if any template in `templates/`
has no snapshot under `tests/testthat/expected/`. A template without a test cannot
join the **proven** set, and only the proven set feeds Qlik.

Two paths, same shape. Steps 1, 3, 4 and 5 are shared; step 2 (the fixture) is where
delimited and PDF differ.

| | Delimited (CSV/TSV) / Excel | PDF |
|---|---|---|
| Fixture | a small export under `samples/raw/<bank>/` | a synthetic one-page PDF built by `tests/testthat/fixtures/make_pdf_fixtures.R` |
| Golden | `tests/testthat/expected/<id>.csv` | same |
| Test file | a new `tests/testthat/test-<id>.R` | one entry in `PDF_GOLDENS` in `tests/testthat/test-pdf_template_goldens.R` |

## 1. Add the template
Create `templates/<id>.yaml` following the spec in
`docs/context/architecture/build-contract.md` (section 5). Set a unique `id`, the
`fingerprint.header_contains_all` tokens, the `delimiter`, the canonical
`columns` map, and the `amount_sign` handler
(`signed` | `debit_credit_cols` | `dr_cr_suffix` | `type_dc` | `unsigned`).

For a PDF template (`format: pdf`), the same information lives under `table:` — the
x-bands (`x_min`/`x_max` in PDF points) that map each column, plus the same
`amount_sign` handler. The visual band editor in the app writes this for you.

## 2a. Add a fixture — delimited / Excel
Place a representative export under `samples/raw/<bank>/`. Use synthetic /
specimen data only (no real PII).

## 2b. Add a fixture — PDF
Real statements are personal financial data and are never committed. Instead add a
generator function to `tests/testthat/fixtures/make_pdf_fixtures.R`, alongside
`make_anz()` / `make_asb()` / `make_westpac()`, and run it:

```
Rscript tests/testthat/fixtures/make_pdf_fixtures.R
```

It writes `tests/testthat/fixtures/<template_id>_sample.pdf`. Follow the existing
functions exactly:

- **Draw at the coordinates the template actually declares.** `open_a4()` sets up an
  A4 page in **points**, so the `x` values you pass to `L()` (left-aligned) and `R()`
  (right-aligned) *are* the x-band coordinates in the YAML. That is the whole point:
  the fixture exercises the template's real geometry, so a nudged band is caught.
- **Invent everything** — fictional bank, people, accounts and figures.
- **Make it reconcile.** Use `running_balance()` and print the opening and closing
  balances in the header, so the fixture exercises the reconciliation checks and not
  just the column split.
- Include whatever the template fingerprints on (a header phrase, a `$` suffix on a
  column label, an `OPENING BALANCE` line…), or detection will not match.

Commit the generated `.pdf` as well as the generator: the generator documents how
the fixture was made, the PDF is what the test reads.

## 3. Generate and EYEBALL the golden snapshot
Produce the expected core table from the engine's own parse, then read it to
confirm dates are ISO, amounts are signed, and descriptions are verbatim:

```r
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
templates <- load_templates("templates")
input  <- read_input("samples/raw/<bank>/<fixture>")   # or the .pdf from step 2b
det    <- detect_statement(input, templates, hint_bank = "<BANK>")
parsed <- parse_statement(input, templates[[det$template_id]])
dir.create("tests/testthat/expected", showWarnings = FALSE, recursive = TRUE)
write.csv(coerce_core(parsed$transactions),
          "tests/testthat/expected/<id>.csv", row.names = FALSE, na = "")
```

Open `tests/testthat/expected/<id>.csv` and verify it by eye before you rely on it.
**This is the only step where a human decides what "correct" means** — everything
afterwards just proves the engine keeps agreeing with what you signed off. For a PDF,
check especially that no description was truncated at a band edge, that the
debit/credit split matches the printed columns, and that the `flags` column says what
you expect (e.g. `date_year_inferred` when the row shows no year).

## 4. Write the test

**Delimited / Excel** — create `tests/testthat/test-<id>.R` using the shared helper:

```r
FIXTURE  <- "samples/raw/<bank>/<fixture>"
EXPECTED <- "tests/testthat/expected/<id>.csv"

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "<id>", bank = "<BANK>")
})
```

**PDF** — add one entry to `PDF_GOLDENS` at the top of
`tests/testthat/test-pdf_template_goldens.R`; the loop below it builds the test:

```r
  list(id = "<id>",
       fx  = "tests/testthat/fixtures/<id>_sample.pdf",
       exp = "tests/testthat/expected/<id>.csv")
```

`expect_statement_ok(fixture_path, expected_csv_path, template_id, bank)` runs
detection + parse and compares the core `Transactions` table to the snapshot.

## 5. Prove it
Run the single shared runner and confirm every bank still passes:

```
Rscript tests/run_tests.R
```

**On the server**, `Rscript` is not on PATH — the app's private R is deliberately
unregistered. Use the exact command in
[`docs/operational/maintaining-the-engine.md`](../docs/operational/maintaining-the-engine.md).

A template is not "done" until its golden test passes and all other banks
still pass — and `skipped: 0`, because a skipped test proved nothing.
