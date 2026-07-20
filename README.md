# Bank Statement OCR — statement conversion engine

A pure-**R** engine that turns a bank/credit-card statement into clean,
structured, downloadable data (Excel + CSV + JSON) with reconciliation checks
and a trust score. Built for forensic-accounting use: audit-grade fidelity,
verbatim descriptions, honoured redactions, and no silent data loss.

**No Python, no reticulate, no machine learning.** Deterministic behaviour and
declarative per-bank templates — a data analyst adds a new statement by editing
a YAML file, not by writing code.

---

## What works today (v1)

- **Delimited path (CSV/TSV/TDV): end-to-end for six banks** — ANZ everyday,
  ANZ credit card, ASB, BNZ, Kiwibank, Westpac — each with a passing golden-file
  test. `read_input → detect → parse → reconcile → outputs`.
- **OCR**: image-only / scanned PDF pages are read via the system Tesseract
  engine (driven from R, no binding required); each OCR'd page is flagged.
- **PDF reader + forensic redaction guard**: text under a redaction overlay is
  never emitted; OCR only reads visible pixels so redactions stay honoured.
- **Reconciliation KPIs + trust score**: balance reconciliation, running-balance
  continuity, transaction count, dates-in-period, completeness, redaction
  summary — surfaced as single-line checks with a high/medium/low trust level.
- **Never crashes**: any failure returns a `failed`/`unsupported`/`needs_review`
  status with an *actionable* message; one JSON line is logged per run.
- **Test suite**: `16 files / 76 tests / 292 assertions, 0 failures`.

## Not done yet (needs real data, not code)

- **Per-bank PDF transaction-table templates** — the committed PDF samples are
  specimen/"how-to" documents with placeholder fields, not real transaction
  tables. Real PDF statements are needed to derive column geometry.
- **Excel (.xlsx) templates** — the reader exists; no `.xlsx` fixtures/templates
  yet. Auto-categorisation is intentionally out of v1 scope.

---

## Quick start

```r
# From the repo root, source the engine and convert a statement:
for (f in list.files("R", full.names = TRUE)) source(f)
res <- convert_statement("samples/raw/bnz/bnz_transaction_export_01.csv",
                         bank = "BNZ", outdir = "outputs")
res$status      # "ok"
res$trust       # list(level, score, reasons)
res$outputs     # paths to the .xlsx / .csv / .json
```

Or from the shell:

```sh
Rscript run.R samples/raw/bnz/bnz_transaction_export_01.csv BNZ outputs
```

Outputs per statement:
- **`.xlsx`** — sheets `Transactions` (16-col core schema), `Summary` (header),
  `Checks` (KPIs + trust), `Provenance` (row → source).
- **`.csv`** — the core `Transactions` table (tool-agnostic).
- **`.json`** — full object (header, transactions, extras, provenance, KPIs, trust).

---

## Deployment (your server, no CRAN needed)

The engine is a plain folder of R files. On a Debian/Ubuntu host:

```sh
# R + the packages the engine uses (all via apt, no CRAN required)
apt-get install -y r-base-core \
  r-cran-yaml r-cran-jsonlite r-cran-openxlsx r-cran-pdftools

# OCR (optional but recommended — enables scanned-PDF reading)
apt-get install -y tesseract-ocr poppler-utils

# tests only:
apt-get install -y r-cran-testthat
```

Copy the folder across, drop templates in `templates/`, and call
`convert_statement()` from whatever analytics tool runs R. OCR auto-enables when
`tesseract` + `pdftoppm` are on the PATH and no-ops safely when they are not.

---

## Adding a new bank (no code)

1. Copy an existing `templates/<bank>.yaml` and edit the column map, date format,
   `amount_sign`, and `fingerprint` for the new layout.
2. Put a sample export under `samples/raw/<bank>/`.
3. Generate the golden snapshot and add a one-line test — see
   [`tests/HOWTO-add-template-test.md`](tests/HOWTO-add-template-test.md).
4. `Rscript tests/run_tests.R` — your bank must pass and no other bank may break.

Template format and the full data contract: [`docs/architecture/build-contract.md`](docs/architecture/build-contract.md).
Requirements & decisions history: [`docs/discovery/discovery-log.md`](docs/discovery/discovery-log.md).

---

## Layout

```
R/            engine (schema, readers, detect, normalise, parse, reconcile,
              outputs, logging, ocr, convert)
templates/    declarative per-bank YAML templates
samples/      specimen corpus (public samples + guides)
tests/        golden-file + unit tests (run: Rscript tests/run_tests.R)
run.R         CLI entrypoint
docs/         build contract + discovery log
```

## Forensic guarantees (all covered by tests)

1. Descriptions preserved **verbatim** (special characters intact).
2. Redactions honoured as-shown, **never derived**; text under a redaction is
   never emitted or OCR'd out.
3. **No silent drops** — completeness is proven by a KPI.
4. **Reproducible** — same input + template ⇒ identical output; no manual edits.
5. **Never crashes** — every error becomes a status with an actionable reason.
