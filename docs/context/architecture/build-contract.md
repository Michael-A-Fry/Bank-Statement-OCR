# Build contract - statement conversion engine (v1)

The single source of truth every module and agent codes against. Pure **R only**
(no reticulate, no Python). Base R + minimal packages (`yaml`, `jsonlite`,
`openxlsx`, `testthat`; `pdftools`/`tesseract` optional for the PDF path).
Deterministic - **no machine learning**. Never crash - return structured status.

## 1. Directory layout
`R/` holds **47 single-concern modules**. The core conversion path is the first
group; the rest support it. (Every module's own header comment is the authority on
what it does — this table is the map, not a duplicate spec.)

```
R/   -- the conversion path (input -> parsed -> checked -> written)
  schema.R              canonical core schema, constructors, coercion
  util.R                small shared helpers (safe IO, hashing, status objects)
  params.R              every numeric tuning threshold, in one visible place
  config.R              deployment settings (config.yaml) + defaults
  read_input.R          dispatch by extension -> reader; also the .xlsx reader (readxl)
  read_delimited.R      CSV/TSV/TDV/TXT reader (base R, quoting + preamble)
  read_pdf.R            PDF text + word-box reader (pdftools) + redaction guard
  parse_pdf_table.R     PDF word boxes -> transaction table (x-bands / anchors)
  templates.R           load + validate declarative per-bank YAML templates
  detect.R              deterministic fingerprint scoring -> template match
  normalise.R           parse_date / parse_amount / clean_description (verbatim)
  labels.R              label dictionary + matcher (single labelled values)
  lexicon.R             externalised recognition vocabularies (admin-editable)
  parse.R               parse_statement(input, template) -> parsed object
  extract_metadata.R    generic statement metadata + multi-statement detection
  extract_fields.R      key-value ("mode: fields") extraction (IRD/forms)
  forms.R               orchestrator for the fields paradigm
  tables.R              a REPORT's tables: locate by wording, read, measure the fill
  tables_detect.R       propose the tables + label/value pairs on a report
  doc_extract.R         orchestrator for the document paradigm (many tables)
  split.R               opt-in deterministic auto-split of a bundled upload
  reconcile.R           reconciliation KPIs + deterministic trust mapping
  diagnose.R            fail-loud diagnostics (where / why / how bad / who fixes)
  outputs.R             write xlsx (multi-sheet) / csv (core) / json (full)
  feed.R                the governed analytics feed Qlik loads
  convert.R             convert_statement(...) orchestrator -- NEVER throws
  batch.R               a whole case folder through the same front door, one row per file
  jobs.R                runs a conversion in its own child R process, and polls it
R/   -- OCR and image handling
  ocr.R                 system Tesseract, driven from R
  ocr_preprocess.R      pre-OCR image conditioning (magick)
  detect_redaction.R    rasterised (solid black box) redaction detection
  inspect.R             "See it on the page" geometry
R/   -- templates without code (the wizard + drafting)
  draft.R               draft a template from a single file
  column_profile.R      everything needed to draft, captured from a sample
  wizard_auto.R         wizard pre-fill helpers
  wizard_detect.R       delimiter / date / amount auto-detection
  layout.R              stable, PII-light layout fingerprint
  suggestions.R         the deterministic half of the learning loop
R/   -- operations, evidence and governance
  logging.R             run + feedback records, ONE FILE PER EVENT
  feedback.R            per-conversion human verdict capture
  metadata_capture.R    local-only per-run metadata corpus (retain-forever)
  analytics.R           run/feedback logs -> Insights, drift, layout clusters
  audit.R               safe-to-share single-statement structural audit
  batch_audit.R         the same over a whole folder, clustered by gap
  coverage.R            "have I set this up right?" self-check
  row_coverage.R        PII-safe explanation of PDF rows that did not survive
  uploads.R             upload capture + lifecycle
  inbox.R               read-only view of the folder-drop intake
  requests.R            the "none of these fits -- tell our team" escape hatch
  retention.R           what is left on disk, and when it goes away

templates/              EVERY template, one folder per kind (templates/README.md)
  statements/           proven per-bank YAML templates (feed the Qlik gate)
  statements_user/      analyst-made templates (Shiny only, never Qlik)
  statements_seed/      starting points offered by the wizard (never loaded)
  fields/  fields_user/         key-value templates (fields mode)
  documents/  documents_user/   many-table reports (document mode)
dictionaries/labels.yaml   synonym dictionary for labelled values (Admin-edited)
dictionaries/lexicon.yaml  recognition vocabularies (Admin-edited)
config/                 config.example.yaml (config.yaml is per-deployment state)
samples/                specimen corpus (already present)
tests/testthat/         golden-file + unit tests
app.R  ui_content.R  ui_labels.R   the Shiny app
run.R                   thin CLI entrypoint
scripts/                bundle / install / audit command-line entry points
tools/corpus/           survey the document engine against real third-party PDFs
                        (fetch-corpus.py collects them; run-corpus.R measures.
                        The PDFs themselves are never committed - other people's
                        files under other people's licences)
tools/webr/             run the suite under WebR when no system R is available
docs/                   operational how-tos + context (charter, this contract, …)
```

## 2. Canonical core schema (per transaction) - STABLE, identical across banks
`transactions` is a `data.frame` with exactly these columns, in this order:

| column        | type    | notes |
|---------------|---------|-------|
| `row_id`      | integer | 1-based within the statement |
| `date`        | character (ISO `YYYY-MM-DD`) | normalised; `NA` if unparseable |
| `date_raw`    | character | exactly as shown |
| `description` | character | **verbatim** - never strip special chars (`O'Connor & Sons`), only trim outer whitespace |
| `amount`      | numeric  | signed; debit negative, credit positive; `NA` if redacted/unparsed |
| `amount_raw`  | character | exactly as shown (incl. `[REDACTED]`) |
| `direction`   | character | `"debit"` / `"credit"` / `NA` |
| `balance`     | numeric  | running balance if present, else `NA` |
| `balance_raw` | character | as shown or `NA` |
| `particulars` | character | NZ field, verbatim, `NA` if none |
| `code`        | character | NZ field, verbatim, `NA` |
| `reference`   | character | NZ field, verbatim, `NA` |
| `other_party` | character | verbatim, `NA` |
| `type`        | character | bank's transaction-type text, verbatim, `NA` |
| `currency`    | character | ISO 4217, default `"NZD"` |
| `flags`       | character | comma-separated, from the vocabulary in §2a; `""` if none |

### 2a. `flags` vocabulary (the complete emitted set)
A flag is how a row says "this is true of me, and it was never guessed". Only these
tokens are ever written; anything else is a bug. Every path can emit the first
three; the rest are PDF-only, because they describe things only a page can do.

| flag | meaning | emitted by |
|---|---|---|
| `redacted` | the value arrived hidden; `amount` is `NA` and `amount_raw` keeps `[REDACTED]` — never derived | all paths |
| `malformed` | the amount did not come out as a number (unreadable, or absent), so this row's money cannot be totalled | all paths |
| `fx` | the row carries a foreign-currency amount (a mapped `fx_amount` extra) | all paths |
| `date_unresolved` | a date was present but could not be resolved to a real date | PDF |
| `date_year_inferred` | the row printed no year; it was taken from the statement's own period text. Caps trust — an inferred year is not a proven one | PDF |
| `no_date` | a shared-date row deliberately kept with a blank date rather than inheriting one | PDF |
| `date_alt_format` | read through the year-less fallback format | PDF |
| `forced` | a row a human added by hand from *See it on the page* | PDF |
| `row_stitched` | two half-rows the reader re-joined | PDF |
| `row_text_merged` | a wrapped line whose words strayed outside the descriptive bands was folded in: the description is complete, but it was **assembled** | PDF |
| `ocr_low_conf` | an OCR'd date/amount/balance cell held a word below the per-cell confidence floor (`PARAM_OCR_CELL_MIN_CONF`) — a likely misread digit the page-mean confidence would mask | PDF (OCR pages) |

There is **no** `reversal` flag and no `ocr` row flag. A reversal is an accounting
judgement, not something the page states, so the engine does not assert it (see
`edge-cases.md` → "Reversals / duplicates"). Whether OCR was used is a **page**
property, reported as a diagnostic and in provenance, not stamped on every row.

Statement-type **extras** (card `fx_amount`/`posted_date`, KiwiSaver
`units`/`unit_price`, …) go in a SEPARATE `extras` data.frame keyed by
`row_id` - never added to the core schema.

## 3. Statement header (metadata) - `parsed$header` named list
`bank, statement_type, template_id, template_version, account_number,
account_name, period_start, period_end, opening_balance, closing_balance,
currency, source_file, source_sha256, page_count, row_count`.
Unknown fields are `NA`. Account numbers stored **as shown** (specimen data).

## 4. Provenance - `parsed$provenance` data.frame
`row_id, source_ref` (e.g. `"csv:line=5"`, `"pdf:p2:y=412"`), `raw` (raw source
line/cell text). Lives in the metadata sheet only - kept OUT of core data.

## 5. Template YAML spec (delimited path, v1)
```yaml
id: bnz_everyday_csv          # unique
bank: BNZ
statement_type: everyday
format: delimited              # delimited | excel | pdf
version: 1
effective_from: 2018-01-01
effective_to: null
min_score: 3                   # matched only if fingerprint score >= this
fingerprint:
  header_contains_all: [Date, Amount, Payee, "Tran Type", "This Party Account"]
  filename_regex: null
delimiter: ","
preamble:                      # optional (e.g. ASB) - lines before header
  header_regex: "^Date,Unique Id,Tran Type"
columns:                       # canonical field -> source column name
  date:        {source: Date, format: "%d/%m/%y"}
  amount:      {source: Amount}
  description: {source: Payee}
  particulars: {source: Particulars}
  code:        {source: Code}
  reference:   {source: Reference}
  type:        {source: "Tran Type"}
  other_party: {source: "Other Party Account"}
  balance:     null            # null when the export has no balance column
amount_sign: signed            # signed | debit_credit_cols | dr_cr_suffix | type_dc | unsigned
currency: NZD
```
`amount_sign` handlers — the complete set (`R/templates.R` rejects any other value):
`signed` (one signed column); `debit_credit_cols` (separate debit/credit columns →
`columns.debit`/`columns.credit`); `dr_cr_suffix` (`123.45 DR`); `type_dc` (a type
column where `type_debit_value: "D"` means debit — the indicator is passed to
`parse_amount`, so the sign comes from the statement, not from a guess); `unsigned`
(credit-card style: one column of unsigned magnitudes where the sign is a printing
convention, not printed. The template **declares** the convention with
`unsigned_default: debit|credit`, and the payment marker — `CR` by default, taken
from the lexicon — flips it. Nothing is inferred from the numbers themselves).

### 5a. Two ways variability is absorbed - read this before "add a synonym"
The engine faces two *different* variability problems and solves them two ways:

1. **Transaction tables (the core).** Rows have no per-row labels - they live in
   columns. Wording never matters: `columns:` maps a source column (delimited /
   excel) or an x-band (pdf) to a canonical field, once. "Different names /
   places / repeat / absent" are all handled by column mapping + the
   *keep-only-date-parseable-rows* filter + `null` for absent fields. **No
   synonym list is involved, and none should be added.**

2. **Single labelled values** (opening/closing balance, statement period,
   account name, IRD form fields). *Here* wording varies wildly - "opening
   balance" vs "balance brought forward" vs "starting balance" vs "opening:".
   These are matched through the **label dictionary** (§5b), never hardcoded in R.

### 5b. Label dictionary + matcher (single labelled values)
`dictionaries/labels.yaml` is the one place a maintainer teaches the engine new
wording. Matching is case-insensitive substring. A **matcher spec** (used in the
dictionary and in `fields`-mode templates) is:
```yaml
opening_balance:
  any_of:                        # SYNONYMS - list as many as real statements need
    - "opening balance"
    - "balance brought forward"
    - "starting balance"
  value: money                   # money(default) | date | date_range | text | regex:<pat>
  occurrence: first              # first(default) | last | all      -> REPEATS
  where: page1                   # any(default) | page1 | last_page | <int>  -> PLACES
  required: false                # default false                    -> EXIST/NOT
  on_conflict: flag              # flag(default) | first | last      -> disagreeing matches
```
Rules: a bare string or `{label: "..."}` is accepted (back-compat); a value is
read from the label line, or the next line if the label is a heading; a field in
a `fields`-mode template auto-inherits the base-dictionary entry whose key
matches its name (or one named by `dict:`); disagreeing repeats are **flagged,
never silently guessed**. `extract_metadata()` reads opening/closing balance
through this dictionary - no label words live in R. Adding a bank's odd wording
is one line in `dictionaries/labels.yaml`, not a code change.

### 5c. Opt-in auto-split of statement bundles (PDF)
By default a file that holds **more than one statement** is *flagged and refused*
(`detect_multiple_statements` → `needs_review`), because a wrongly-placed boundary
is itself a silently-wrong outcome. A PDF template may **opt in** to automatic
splitting with a `split:` block:
```yaml
split:
  on: page1_marker         # boundary signal: page1_marker | opening_label
  min_statements: 2         # only split when at least this many segments are found
```
When present *and* the upload is detected as a bundle, the engine segments it at
the declared deterministic marker (each `Page 1 of N` reset, or each repeated
opening-balance header), parses and **reconciles every segment independently**,
and rolls trust up to the **weakest** segment. It commits the split **only** when
the number of statements is **independently confirmed** — a *different* structural
count (distinct periods, page-1 resets, or repeated opening/closing blocks) agrees
on the same count. Per-segment reconciliation is reported as added confidence but
is **not** a substitute: a running-balance column is continuous across any cut, so
a wrongly-placed boundary would still reconcile within each piece — only an
independent count can confirm the boundaries. Otherwise it falls back to the safe
flag-and-refuse default. Output rows carry a `statement_index` column; per-statement
periods / balances / trust are in `result$metadata$split$statements`, and the
combined header's per-statement identity fields (account, balances) are left blank
so the feed never stamps one statement's account onto another's rows. `split:` is
PDF-only (a delimited / Excel export is almost always a single account/period); it
is rejected by `validate_template` on any other format or with an unknown `on` value.

## 6. Function interfaces (exact signatures)
- `load_templates(dir) -> list<template>` (parsed + validated; invalid template = hard error at load, listed by id).
- `read_input(path) -> input` : `list(kind, path, sha256, lines=NULL, table=NULL, pages=NULL, meta)`. Dispatch by extension.
- `detect_statement(input, templates, hint_bank=NULL, hint_type=NULL) -> list(template_id, score, matched(logical), candidates=data.frame(id,score))`. Deterministic fingerprint scoring. `matched` TRUE only if best `score >= template$min_score` AND unambiguous (strictly greater than 2nd best). Hints filter candidates.
- `parse_statement(input, template) -> parsed` : `list(transactions, extras, header, provenance)` per schema above.
- `parse_date(x, fmt) -> list(iso, raw)`; `parse_amount(x, style, ...) -> list(value, direction, raw)`; `clean_description(x) -> character` (verbatim-preserving: only `trimws`).
- `reconcile(parsed, template) -> list(kpis=data.frame, trust=list(level, score, reasons))`. KPI rows: `name, status(pass|fail|na), expected, actual, discrepancy, detail`.
- `write_outputs(parsed, recon, outdir, basename, formats=c("xlsx","csv","json")) -> character[]` (paths written).
- `write_log_record(logdir, subdir, id, record) -> invisible(path)`. **One JSON file
  per event, never an append** (§10). It never overwrites: a clashing id gets a
  `~2` suffix. There is deliberately no `log_event()` / JSONL appender — a shared
  append is the one write that can interleave over SMB.
- `convert_statement(path, bank=NULL, statement_type=NULL, outdir="out", templates_dir="templates/statements", user_templates_dir="templates/statements_user", requested_by=NULL, formats=c("xlsx","csv","json"), logdir="logs", redaction_rects=NULL, force_template=NULL, force_rows=NULL, log=TRUE) -> result`. **Never throws.** Returns `list(status, template_id, trust, kpis, header, outputs, messages, ...)`.
- `convert_document(path, ...)` is the **front door**: it runs `convert_statement()`
  and falls back to the form/labelled-value pipeline (`R/forms.R`) only when the
  statement path returns `unsupported` and no template was forced.

## 7. Status model (`result$status`)
`ok` (matched + parsed + reconciled) · `needs_review` (parsed but a KPI failed
or low trust) · `unsupported` (no confident template match) · `failed`
(unreadable/error). Every non-`ok` status carries an **actionable** message:
*why* + *what it needs* (e.g. `"unsupported: no template matched; closest
bnz_everyday_csv score 2/3 (missing 'Tran Type' header)"`).

## 8. Reconciliation KPIs (compute where data allows, else `na` with reason)
The authoritative list is the one in `reconcile()` at the bottom of
`R/reconcile.R`, in report order. Every one of them must have a `CHECK_PLAIN`
entry in `ui_labels.R`; `test-seams.R` fails the suite if one does not.

| KPI | Proves | `na` when |
|---|---|---|
| `balance_reconciliation` | `opening + sum(amount) == closing`, on a rounded-cent tolerance | no opening/closing, and none derivable from a running-balance column |
| `running_balance_continuity` | `balance[i] == balance[i-1] + amount[i]` | no balance column |
| `amount_direction` | money in/out is the right way round for the declared style | the style makes it undecidable |
| `transaction_count` | parsed count `== stated count`; **degrades to `n > 0`** when the statement prints no count, which is most statements | never — but read the detail |
| `dates_within_period` | every date falls inside `period_start..period_end` | no period was found |
| `dates_readable` | **at least one** row date parsed — the safety net for a mis-mapped date column, not a completeness proof | zero rows |
| `no_unparsed_rows` | no source data line failed to become a transaction | **PDF and Excel always**: there is no independent physical-line count, so it cannot be proved (it can still `fail`, when more rows looked like transactions and could not be read than were read) |
| `redaction_summary` | informational count of redacted rows | always — it is a count, not a check |
| `ocr_confidence` | the page-mean OCR confidence | the statement was not OCR'd |
| `redaction_scan` | nothing under a redaction overlay was read | the scan could not run |

`trust.level` ∈ `high | medium | low`, deterministic: `high` = every applicable
KPI passed; `medium` = one or more could not run; `low` = any failed. Two
adjustments: a `low` caused **only** by secondary checks when
`balance_reconciliation` passed is lifted to `medium`; and a run where **neither**
balance check ran and there is no stated count can never be `high`, because
nothing independently proves a row was not dropped.

**Consequence worth stating plainly: a PDF or Excel statement can never reach
`high`**, because `no_unparsed_rows` is always `na` for those formats. `medium`
is the healthy ceiling there.

## 9. Output artifacts
- **xlsx** (primary), six sheets: `Transactions` (core schema), `Summary` (header
  block), `Checks` (KPIs + trust), `Provenance` (row_id → source_ref → raw),
  `Diagnostics`, `Metadata`.
- **csv**: the core `Transactions` table only (tool-agnostic).
- **json**: the full object — build stamp, `header`, `transactions`, `extras`,
  `kpis`, `trust`, `diagnostics`, `provenance`, `metadata`.

## 10. Logging (`logs/runs/<run_id>.json`, one FILE per run)
`ts, requested_by, source_file, source_sha256, bank_hint, detected_template,
template_version, status, trust_level, row_count, kpi_fail_count, message`
(plus the layout/detection fields Insights reads). One file per event, never a
shared append, so concurrent conversions can never interleave or need a lock, and a
record is never overwritten — a clashing `run_id` gets a `~2` suffix rather than
erasing the earlier audit record.

**What is and isn't in a log record — stated precisely.** No transaction rows, no
descriptions, no amounts, no balances and no account numbers are logged: *statement
CONTENT* never appears. But a run record is not content-free — it deliberately
carries **`source_file` (the uploaded file's name)** and, where the engine read them,
the **statement period dates**, because "which file was this, and what period did it
cover" is exactly what makes the run history an audit trail. A filename can itself be
identifying (`Smith J - ANZ - Mar 2026.pdf`), so treat `logs/` as case-related
material: it stays on the box, is covered by the retention rules in `R/retention.R`,
and is backed up with the same care as evidence
(`docs/operational/backup-and-restore.md`).

The separate local metadata corpus (`logs/metadata/`) is documented in
`metadata-capture.md`, including which fields are hashed rather than stored.

## 11. Forensic guarantees (non-negotiable, must be tested)
1. **Descriptions verbatim** - special characters preserved byte-for-byte.
2. **Redactions honoured** - a redacted value is `[REDACTED]`+`redacted` flag,
   never derived; for PDFs, text under a redaction overlay is NOT extracted.
3. **No silent drops** - `no_unparsed_rows` proves completeness.
4. **Reproducible** - same input + template ⇒ identical output (no manual edits).
5. **Never crashes** - all errors become a `failed` status with a reason.

## 12. Testing (golden-file + unit)
Each fixture has an expected core-table snapshot under
`tests/testthat/expected/`. Tests assert: detection picks the right template;
parsed table equals expected; reconciliation KPIs match; verbatim + redaction
guarantees hold. A template is not "done" until its golden test passes and all
other banks still pass.
