# How to add a template and its golden test

Every bank template ships with a golden-file test so parsing stays reproducible.
Follow the same four steps used for `bnz_everyday_csv`.

## 1. Add the template
Create `templates/<id>.yaml` following the spec in
`docs/architecture/build-contract.md` (section 5). Set a unique `id`, the
`fingerprint.header_contains_all` tokens, the `delimiter`, the canonical
`columns` map, and the `amount_sign` handler
(`signed` | `debit_credit_cols` | `dr_cr_suffix` | `type_dc`).

## 2. Add a fixture
Place a representative export under `samples/raw/<bank>/`. Use synthetic /
specimen data only (no real PII).

## 3. Generate and EYEBALL the golden snapshot
Produce the expected core table from the engine's own parse, then read it to
confirm dates are ISO, amounts are signed, and descriptions are verbatim:

```r
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
templates <- load_templates("templates")
input  <- read_input("samples/raw/<bank>/<fixture>")
det    <- detect_statement(input, templates, hint_bank = "<BANK>")
parsed <- parse_statement(input, templates[[det$template_id]])
dir.create("tests/testthat/expected", showWarnings = FALSE, recursive = TRUE)
write.csv(coerce_core(parsed$transactions),
          "tests/testthat/expected/<id>.csv", row.names = FALSE, na = "")
```

Open `tests/testthat/expected/<id>.csv` and verify it by eye before committing.

## 4. Write the test
Create `tests/testthat/test-<id>.R` using the shared helper:

```r
FIXTURE  <- "samples/raw/<bank>/<fixture>"
EXPECTED <- "tests/testthat/expected/<id>.csv"

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "<id>", bank = "<BANK>")
})
```

`expect_statement_ok(fixture_path, expected_csv_path, template_id, bank)` runs
detection + parse and compares the core `Transactions` table to the snapshot.

## 5. Prove it
Run the single shared runner and confirm every bank still passes:

```
Rscript tests/run_tests.R
```

A template is not "done" until its golden test passes and all other banks
still pass.
