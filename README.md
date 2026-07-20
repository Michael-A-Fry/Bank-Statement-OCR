# Bank Statement OCR — statement conversion engine

A pure-**R** engine that turns a bank/credit-card statement into clean,
structured, downloadable data (Excel + CSV + JSON) with reconciliation checks
and a trust score. Built for forensic-accounting use: audit-grade fidelity,
verbatim descriptions, honoured redactions, and no silent data loss.

**No Python, no reticulate, no machine learning.** Deterministic behaviour and
declarative per-bank templates — a data analyst adds a new statement by using a
point-and-click wizard (or editing a YAML file), not by writing code.

---

## What works today (v1)

- **Delimited path (CSV/TSV/TDV): end-to-end for six banks** — ANZ everyday,
  ANZ credit card, ASB, BNZ, Kiwibank, Westpac — each with a passing golden-file
  test — plus a cross-bank **Xero-standard** import template.
- **PDF statements** convert end-to-end via declarative `format: pdf` templates
  (x-band columns; headings/notes/gaps ignored by the date-parse row filter),
  and **Excel (.xlsx)** is supported too.
- **Visual PDF wizard** — draw boxes over the columns, preview live, generate the
  `format: pdf` template. No YAML by hand.
- **Key-value (`mode: fields`)** extraction foundation for IRD/form documents. `read_input → detect → parse → reconcile → outputs`.
- **Interactive GUI + template wizard (Shiny)** — upload → convert → review the
  checks → download; plus a wizard that builds a *new bank template for you*
  from a sample (map columns by dropdown, preview, save).
- **OCR**: image-only / scanned PDF pages are read via the system Tesseract
  engine (driven from R, no binding required); each OCR'd page is flagged.
- **PDF reader + forensic redaction guard**: text under a redaction overlay is
  never emitted; OCR only reads visible pixels so redactions stay honoured.
- **Reconciliation KPIs + trust score**: balance reconciliation, running-balance
  continuity, transaction count, dates-in-period, completeness, redaction
  summary — surfaced as single-line checks with a high/medium/low trust level.
- **Fail-loud diagnostics**: any non-clean run reports **where / why / how-bad /
  how-to-fix** — in the result, the workbook's `Diagnostics` sheet, the JSON, and
  the app. The engine never returns a silent wrong answer.
- **OCR pre-processing**: greyscale → deskew → normalise → upscale (ImageMagick)
  before Tesseract, to lift accuracy on scanned pages.
- **Never crashes**: any failure returns a `failed`/`unsupported`/`needs_review`
  status with an *actionable* message; one JSON line is logged per run.
- **Test suite**: `16 files / 76 tests / 292 assertions, 0 failures`.

## Not done yet (data-gated, not code)

- **More per-bank PDF templates** — the PDF table parser + visual wizard are
  built and tested; adding a bank's PDF is a wizard session against a real
  statement (a YAML, not engine work).
- **Real native SBS/TSB/Co-op/Heartland exports** and a **real IRD document** to
  wire key-value mode into the full output. Auto-categorisation stays out of scope.

---

## The interactive app (GUI + wizard)

```sh
# from the repo root:
R -e 'shiny::runApp(".", launch.browser = TRUE)'
```

Three tabs, all point-and-click — no coding:
- **Convert** — upload a statement, (optionally force a bank), click Convert.
  See the detected template, trust score, the reconciliation checks, and a
  transaction preview; download the Excel / CSV / JSON.
- **Template wizard** — upload a *sample* export, map each field (date, amount,
  description, …) to a source column with dropdowns, set the date format and
  amount style, tick the fingerprint columns, **Preview** the parse live, then
  **Save** — it writes `templates/<id>.yaml` for you. That is how a sole analyst
  adds a new bank.
- **Help** — the exact test commands and the add-a-bank steps.

## Quick start (no GUI)

```r
for (f in list.files("R", full.names = TRUE)) source(f)
res <- convert_statement("samples/raw/bnz/bnz_transaction_export_01.csv",
                         bank = "BNZ", outdir = "outputs")
res$status; res$trust; res$outputs
```

```sh
Rscript run.R samples/raw/bnz/bnz_transaction_export_01.csv BNZ outputs
Rscript tests/run_tests.R      # the whole test suite
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

# GUI + wizard:
apt-get install -y r-cran-shiny r-cran-dt

# OCR (optional but recommended — enables scanned-PDF reading):
apt-get install -y tesseract-ocr poppler-utils

# tests only:
apt-get install -y r-cran-testthat
```

Copy the folder across, drop templates in `templates/`, and call
`convert_statement()` from whatever analytics tool runs R, or run the Shiny app
for your team. OCR auto-enables when `tesseract` + `pdftoppm` are on the PATH and
no-ops safely when they are not.

---

## Adding a new bank (no code)

1. **Easiest:** open the app's **Template wizard**, upload a sample, map the
   columns, Preview, Save.
2. Or copy an existing `templates/*.yaml` and edit the `columns:` map.
3. Add a golden test — see [`tests/HOWTO-add-template-test.md`](tests/HOWTO-add-template-test.md).
4. `Rscript tests/run_tests.R` — your bank must pass and no other bank may break.

**New here? Start with [`docs/ONBOARDING.md`](docs/ONBOARDING.md)** — a worked,
click-through guide to converting statements and adding banks.
Real-world edge cases + honest status: [`docs/edge-cases.md`](docs/edge-cases.md).
Visual-wizard design + A/B/C roadmap: [`docs/wizard-vision-and-roadmap.md`](docs/wizard-vision-and-roadmap.md).
OCR pre-processing research: [`docs/research/ocr-preprocessing.md`](docs/research/ocr-preprocessing.md).
Template format and the full data contract: [`docs/architecture/build-contract.md`](docs/architecture/build-contract.md).
Requirements & decisions history: [`docs/discovery/discovery-log.md`](docs/discovery/discovery-log.md).

---

## Layout

```
app.R         Shiny GUI + template wizard
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
