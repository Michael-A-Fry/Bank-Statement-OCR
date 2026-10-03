# Paused build: automatic reading (stage 1), 3 Oct 2026 10:35 UTC

Paused because the account's weekly usage flag turned to "allowed_warning"
(the pause rule agreed with the product owner). Nothing here is loaded by the
app: the app only sources R/*.R.

What is here, exactly as the build agents left it when stopped (unfinished,
not reviewed, may not even parse):

| File | Was going to | State when stopped |
|---|---|---|
| R/auto_read.R, auto_read_pdf.R, auto_read_prove.R, auto_read_tabular.R | R/ | reader + prover, mid-build (tabular part barely started) |
| parse_pdf_table.patch | applied to R/parse_pdf_table.R | per-page columns support, mid-build |
| R/bank_identity.R, tests/test-bank-identity.R, dictionaries/nz_bank_branches.csv, nz_banks.yaml | R/, tests/testthat/, dictionaries/ | bank identity, built, review not started |

The layout store and tracking had not been written yet.

To resume: move the files back, `git apply wip/auto-reading/parse_pdf_table.patch`,
and restart stage 1 (the workflow "auto-reading-foundations") from this state.
