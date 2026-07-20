# Build contract — statement conversion engine (v1)

The single source of truth every module and agent codes against. Pure **R only**
(no reticulate, no Python). Base R + minimal packages (`yaml`, `jsonlite`,
`openxlsx`, `testthat`; `pdftools`/`tesseract` optional for the PDF path).
Deterministic — **no machine learning**. Never crash — return structured status.

## 1. Directory layout
```
R/                      engine functions (one concern per file)
  schema.R              canonical schema + constructors
  util.R                small shared helpers (safe IO, status objects)
  read_input.R          dispatch by extension -> reader
  read_delimited.R      CSV/TSV/TDV reader (base R, quoting + preamble)
  read_excel.R          .xlsx reader (openxlsx)
  read_pdf.R            PDF text + word-box reader (pdftools) + redaction guard
  templates.R           load + validate YAML templates
  detect.R              deterministic bank/statement detection + trust
  normalise.R           parse_date / parse_amount / clean_description (verbatim)
  parse.R               parse_statement(input, template) -> parsed object
  reconcile.R           reconciliation KPIs + trust score
  outputs.R             write xlsx (multi-sheet) / csv / json
  logging.R             append-only run log
  convert.R             convert_statement(...) orchestrator (never throws)
templates/              declarative per-bank YAML templates (runtime-loadable)
samples/                specimen corpus (already present)
tests/testthat/         golden-file + unit tests
run.R                   thin CLI entrypoint
docs/                   architecture + maintainer guide
```

## 2. Canonical core schema (per transaction) — STABLE, identical across banks
`transactions` is a `data.frame` with exactly these columns, in this order:

| column        | type    | notes |
|---------------|---------|-------|
| `row_id`      | integer | 1-based within the statement |
| `date`        | character (ISO `YYYY-MM-DD`) | normalised; `NA` if unpar04able |
| `date_raw`    | character | exactly as shown |
| `description` | character | **verbatim** — never strip special chars (`O'Connor & Sons`), only trim outer whitespace |
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
| `flags`       | character | comma-separated subset of `redacted,ocr,fx,reversal,malformed`; `""` if none |

Statement-type **extras** (card `fx_amount`/`posted_date`, KiwiSaver
`units`/`unit_price`, …) go in a SEPARATE `extras` data.frame keyed by
`row_id` — never added to the core schema.

## 3. Statement header (metadata) — `parsed$header` named list
`bank, statement_type, template_id, template_version, account_number,
account_name, period_start, period_end, opening_balance, closing_balance,
currency, source_file, source_sha256, page_count, row_count`.
Unknown fields are `NA`. Account numbers stored **as shown** (specimen data).

## 4. Provenance — `parsed$provenance` data.frame
`row_id, source_ref` (e.g. `"csv:line=5"`, `"pdf:p2:y=412"`), `raw` (raw source
line/cell text). Lives in the metadata sheet only — kept OUT of core data.

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
preamble:                      # optional (e.g. ASB) — lines before header
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
amount_sign: signed            # signed | debit_credit_cols | dr_cr_suffix | type_dc
currency: NZD
```
`amount_sign` handlers: `signed` (one signed column); `debit_credit_cols`
(separate debit/credit columns → `columns.debit`/`columns.credit`); `dr_cr_suffix`
(`123.45 DR`); `type_dc` (a type column where `type_debit_value: "D"` means debit).

## 6. Function interfaces (exact signatures)
- `load_templates(dir) -> list<template>` (parsed + validated; invalid template = hard error at load, listed by id).
- `read_input(path) -> input` : `list(kind, path, sha256, lines=NULL, table=NULL, pages=NULL, meta)`. Dispatch by extension.
- `detect_statement(input, templates, hint_bank=NULL, hint_type=NULL) -> list(template_id, score, matched(logical), candidates=data.frame(id,score))`. Deterministic fingerprint scoring. `matched` TRUE only if best `score >= template$min_score` AND unambiguous (strictly greater than 2nd best). Hints filter candidates.
- `parse_statement(input, template) -> parsed` : `list(transactions, extras, header, provenance)` per schema above.
- `parse_date(x, fmt) -> list(iso, raw)`; `parse_amount(x, style, ...) -> list(value, direction, raw)`; `clean_description(x) -> character` (verbatim-preserving: only `trimws`).
- `reconcile(parsed, template) -> list(kpis=data.frame, trust=list(level, score, reasons))`. KPI rows: `name, status(pass|fail|na), expected, actual, discrepancy, detail`.
- `write_outputs(parsed, recon, outdir, basename, formats=c("xlsx","csv","json")) -> character[]` (paths written).
- `log_event(logdir, record) -> invisible` (append JSONL).
- `convert_statement(path, bank=NULL, statement_type=NULL, outdir, templates_dir="templates", requested_by=NULL, formats=c("xlsx","csv","json")) -> result`. **Never throws.** Returns `list(status, template_id, trust, kpis, header, outputs, messages)`.

## 7. Status model (`result$status`)
`ok` (matched + parsed + reconciled) · `needs_review` (parsed but a KPI failed
or low trust) · `unsupported` (no confident template match) · `failed`
(unreadable/error). Every non-`ok` status carries an **actionable** message:
*why* + *what it needs* (e.g. `"unsupported: no template matched; closest
bnz_everyday_csv score 2/3 (missing 'Tran Type' header)"`).

## 8. Reconciliation KPIs (compute where data allows, else `na` with reason)
- `balance_reconciliation`: `opening + sum(amount) == closing` (needs opening+closing).
- `running_balance_continuity`: `balance[i] == balance[i-1] + amount[i]` (needs balance column).
- `transaction_count`: parsed count > 0 and == stated count if present.
- `dates_within_period`: all `date` within `period_start..period_end` (if known).
- `no_unparsed_rows`: every non-empty data row became a transaction (completeness).
- `redaction_summary`: count of redacted rows/fields (informational).
`trust.level` ∈ `high|medium|low`: `high` = all applicable KPIs pass;
`medium` = only informational/NA gaps; `low` = any `fail`. Deterministic.

## 9. Output artifacts
- **xlsx** (primary): `Transactions` (core schema), `Summary` (header block),
  `Checks` (KPIs + trust), `Provenance` (row_id → source_ref → raw).
- **csv**: the core `Transactions` table only (tool-agnostic).
- **json**: full object (`header`, `transactions`, `extras`, `kpis`, `trust`).

## 10. Logging (`logs/runs.jsonl`, one line per run)
`ts, requested_by, source_file, source_sha256, bank_hint, detected_template,
template_version, status, trust_level, row_count, kpi_fail_count, message`.
No raw statement content in logs.

## 11. Forensic guarantees (non-negotiable, must be tested)
1. **Descriptions verbatim** — special characters preserved byte-for-byte.
2. **Redactions honoured** — a redacted value is `[REDACTED]`+`redacted` flag,
   never derived; for PDFs, text under a redaction overlay is NOT extracted.
3. **No silent drops** — `no_unparsed_rows` proves completeness.
4. **Reproducible** — same input + template ⇒ identical output (no manual edits).
5. **Never crashes** — all errors become a `failed` status with a reason.

## 12. Testing (golden-file + unit)
Each committed fixture has an expected core-table snapshot under
`tests/testthat/expected/`. Tests assert: detection picks the right template;
parsed table equals expected; reconciliation KPIs match; verbatim + redaction
guarantees hold. A template is not "done" until its golden test passes and all
other banks still pass.
