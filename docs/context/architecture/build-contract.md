# Build contract - statement conversion engine (v2: automatic reading)

The single source of truth every module and agent codes against. Pure **R only**
(no reticulate, no Python). Base R + minimal packages (`yaml`, `jsonlite`,
`openxlsx`, `testthat`; `pdftools`/`tesseract` optional for the PDF path).
Deterministic - **no machine learning**: the tool "learns" a bank's layouts only by
keeping versioned records of readings its own arithmetic proved, never by fitting a
model. Never crash - return structured status.

## 1. Directory layout
`R/` holds **47 single-concern modules**. The core conversion path is the first
group; the rest support it. (Every module's own header comment is the authority on
what it does - this table is the map, not a duplicate spec.)

```
R/   -- the conversion path (input -> read -> proven -> written)
  schema.R              canonical core schema, constructors, coercion
  util.R                small shared helpers (safe IO, hashing, status objects)
  params.R              every numeric tuning threshold, in one visible place
  config.R              deployment settings (config.yaml) + defaults
  read_input.R          dispatch by extension -> reader; also the .xlsx reader (readxl)
  read_delimited.R      CSV/TSV/TDV/TXT reader (base R, quoting + preamble)
  read_pdf.R            PDF text + word-box reader (pdftools) + drawn-sign scan
  identify.R            the Convert table's row per file: its kind, pages and BANK (pre-filled)
  bank_identity.R       which bank issued a statement, from the holder's account number and its wording
  auto_read.R           automatic reading: the reading pipeline, candidates, outcome (proven / check / unread)
  auto_read_pdf.R       automatic reading: tokens, cells and the column model found on each PDF page
  auto_read_prove.R     automatic reading: column roles by arithmetic and the all-or-nothing checks
  auto_read_tabular.R   automatic reading: CSV and Excel exports, columns by content and headings
  layouts.R             each bank's learned layouts: versioned store, matching and learning rules
  fixes.R               a person's unproven fix, held for an admin (never learned on its own)
  tracking.R            no-personal-data record of what automatic reading did, and its summary
  parse_pdf_table.R     PDF word boxes -> transaction table, through the columns the reader found
  parse.R               parse_statement(input, template) -> parsed object (CSV / Excel)
  normalise.R           parse_date / parse_amount / clean_description (verbatim), date and delimiter resolution
  labels.R              label dictionary + matcher (single labelled values)
  lexicon.R             externalised recognition vocabularies (admin-editable)
  extract_metadata.R    generic statement metadata + multi-statement detection
  split.R               deterministic split of a bundled upload, each statement read on its own
  reconcile.R           reconciliation KPIs + deterministic trust mapping
  diagnose.R            fail-loud diagnostics (where / why / how bad / who fixes)
  outputs.R             write xlsx (multi-sheet) / csv (core) / json (full)
  feed.R                the governed analytics feed Qlik loads
  convert.R             convert_statement(...) orchestrator -- NEVER throws
  batch.R               a whole case folder through the same front door, one row per file
  jobs.R                runs a conversion in its own child R process, and polls it
R/   -- OCR and image handling
  ocr.R                 system Tesseract, driven from R (one run per page, time-limited)
  ocr_preprocess.R      pre-OCR image conditioning (magick), with a safe contrast stretch
  inspect.R             page geometry of a template's bands (kept for its tests; no screen calls it since 2.0.0)
R/   -- layout evidence
  layout.R              stable, PII-light layout fingerprint (clusters what could not be read)
  suggestions.R         the deterministic half of the learning loop for WORDS (Admin -> Words)
R/   -- operations, evidence and governance
  logging.R             run + feedback records, ONE FILE PER EVENT
  feedback.R            per-conversion human verdict capture
  metadata_capture.R    local-only per-run metadata corpus (retain-forever)
  analytics.R           run/feedback logs -> Admin Health: layout usage, layout drift, clusters
  audit.R               safe-to-share single-statement structural audit
  batch_audit.R         the same over a whole folder, clustered by gap
  coverage.R            "have I set this up right?" self-check
  row_coverage.R        PII-safe explanation of PDF rows that did not survive (no screen calls it since 2.0.0)
  uploads.R             upload capture + lifecycle
  inbox.R               read-only view of the folder-drop intake
  requests.R            format requests raised by the team (Admin -> Health)
  retention.R           what is left on disk, and when it goes away

templates/              templates/README.md: what lives here now
  layouts/              LIVE: each bank's learned layouts, <bank>/<id>@v<n>.yaml
                        (created on the server; never shipped, never hand-edited)
  layouts/.pending/     LIVE: a person's unproven fix, held for an admin
dictionaries/labels.yaml   synonym dictionary for labelled values (Admin-edited)
dictionaries/lexicon.yaml  recognition vocabularies (Admin-edited)
dictionaries/nz_bank_branches.csv  Payments NZ bank branch register (shipped reference data)
dictionaries/nz_banks.yaml         each NZ bank's names, domains, phones, SWIFT, brand words
config/                 config.example.yaml (config.yaml is per-deployment state)
logs/tracking/          LIVE: automatic-reading tracking, one JSON line per event
samples/                specimen corpus (already present)
tests/testthat/         golden-file + unit tests; fixtures/templates/ holds the
                        retired shipped templates as test material for the table reader
app.R  ui_content.R  ui_labels.R   the Shiny app
run.R                   thin CLI entrypoint
scripts/                bundle / install / audit command-line entry points
tools/synth/            the MEASURED test sets: synthetic statements, each with the
                        answer key it was drawn from, and the scorers
                        (score_auto.R, score_convert.R) that count AUTO_WRONG
                        (must be 0). Dev-time only - the server runs R alone.
tools/webr/             run the suite under WebR when no system R is available
docs/                   operational how-tos + context (charter, this contract, ...)
```

Retired at 2.0.0, and gone from `R/`: `templates.R`, `detect.R`, `learned.R`,
`draft.R`, `column_profile.R`, `column_fit.R`, `wizard_auto.R`, `wizard_detect.R`
(template loading, fingerprint detection, remembered template choices, the
drafter and the guided setup). Their few helpers the reader still needed moved
into `normalise.R`, `util.R` and the `auto_read*.R` files.

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
three; the automatic reader adds the next three; the rest are PDF-only, because
they describe things only a page can do.

| flag | meaning | emitted by |
|---|---|---|
| `redacted` | the value arrived hidden, and `amount_raw` keeps `[REDACTED]`. Where the two printed balances either side prove what the amount must be, it is filled in and the row also carries `amount_from_balance` (which sends the statement to Please check); otherwise `amount` is `NA` | all paths |
| `malformed` | the amount did not come out as a number (unreadable, or absent), so this row's money cannot be totalled | all paths |
| `fx` | the row carries a foreign-currency amount (a mapped `fx_amount` extra) | all paths |
| `amount_from_balance` | the amount could not be read (or was removed) and was worked out from two printed running balances. **Always sends the statement to Please check** (spec section 2, "Derived amounts"); never a figure read from the page | the automatic reader |
| `sign_from_balance` | which way the money went (in or out) was decided by the running balance, not by a printed sign or column | the automatic reader |
| `date_carried` | the statement prints a date once per day; this row took the date from the row above | the automatic reader |
| `date_unresolved` | a date was present but could not be resolved to a real date | PDF |
| `date_year_inferred` | the row printed no year; it was taken from the statement's own period text. Caps trust — an inferred year is not a proven one | PDF |
| `no_date` | a shared-date row deliberately kept with a blank date rather than inheriting one | PDF |
| `date_alt_format` | read through the year-less fallback format | PDF |
| `forced` | kept as a row although no amount could be read on it (the reader keeps it rather than drop a dated line) | PDF |
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
Unknown fields are `NA`. Account numbers stored **as shown** in the analyst's own
outputs (they are the statement's content). `template_id` keeps its name for the
outputs' and the feed's sake; it now names the reading's candidate (`auto_<hash>`)
or the learned layout. The account number never leaves the outputs: not into the
run log, tracking, a layout file or the metadata corpus.

## 4. Provenance - `parsed$provenance` data.frame
`row_id, source_ref` (e.g. `"csv:line=5"`, `"pdf:p2:y=412"`), `raw` (raw source
line/cell text). Lives in the metadata sheet only - kept OUT of core data.

## 5. A layout: the in-memory template list, and the file it is kept in
Nobody writes templates any more. The automatic reader finds the columns from the
statement's content and proves them with its arithmetic (spec section 4), and the
**candidate** it produces is a template list in the schema below, so the table
reader (`parse_statement`, `parse_pdf_table`), reconciliation and the outputs work
on it unchanged. When a reading is proven, that template list plus a `layout:`
block is kept as one of the bank's **learned layouts** (`R/layouts.R`).

A learned layout on disk, `templates/layouts/anz/anz_1@v1.yaml` (abridged):
```yaml
id: anz_1
bank: anz
format: pdf                    # pdf | delimited | excel
version: 1
currency: NZD
table:                         # PDF: what the reader found on the statement that created it
  row_tol: 3
  date_format: "%d %b"
  amount_sign: debit_credit_cols  # signed | debit_credit_cols | dr_cr_suffix | type_dc | unsigned
  ref_width: 594.96            # the band frame of those positions
  ref_height: 841.92
  columns:
    date:        {x_min: 34.0,  x_max: 85.0}
    description: {x_min: 85.0,  x_max: 323.5}
    debit:       {x_min: 323.5, x_max: 413.5}
    credit:      {x_min: 413.5, x_max: 494.5}
    balance:     {x_min: 494.5, x_max: 559.0}
auto:                          # how the arithmetic proved it: roles, sign convention, order
  roles: [debit, credit, balance]
  dir: old                     # oldest first
layout:
  id: anz_1
  bank: anz
  status: provisional          # provisional | proven | retired
  version: 1
  created: "2026-10-03T22:35:46Z"
  proved_by: [<sha256 of each statement that proved it>]
  origin: auto                 # auto | confirmed | corrected
  signature:                   # what "the same layout" means -- NO absolute positions
    kind: pdf                  # pdf | scan | delimited | excel
    roles: [date, description, debit, credit, balance]
    date_format: "%d %b"
    money_style: debit_credit_cols
    balance_freq: every
    newest_first: no
    heading_tokens: [balance, date, deposits, details, withdrawals, ...]
    producer: "Skia/PDF m141"
    rel_x: [0.07, 0.48, 0.69, 0.84, 1.0]
```
A CSV or Excel layout carries `columns:` mapping each canonical field to a source
column name instead of `table:` boxes (`date: {source: Date, format: "%d/%m/%Y"}`).

The `amount_sign` handlers are unchanged and complete: `signed` (one signed
column); `debit_credit_cols` (separate debit/credit columns); `dr_cr_suffix`
(`123.45 DR`); `type_dc` (a type column where `type_debit_value` means debit);
`unsigned` (credit-card style magnitudes whose sign is a printing convention,
`unsigned_default: debit|credit`, flipped by the payment marker from the lexicon).
Which handler applies is now **proved by the arithmetic**, never declared by a
person: every assignment of roles, every sign convention and both orders are tried,
and a reading is proven only when exactly one of them makes every balance step hold.

**Retired keys.** `fingerprint`, `min_score`, `filename_regex`, `hidden`,
`sample`, `draft`, `refines` and `split:` belonged to template detection and the
template library; nothing reads them. The 13 templates that shipped in 1.x are
kept, unchanged, in `tests/testthat/fixtures/templates/` as test material for the
table reader.

**The store's rules** (`R/layouts.R` header; spec section 6):
- A file is **never edited**. Every change - new evidence, a promotion, a confirm,
  a correction, a rename, a retirement - writes the next version beside it, so
  `layouts_state_id()` (a hash over every layout file) identifies exactly what was
  learned when an output was made.
- **Only a proven reading teaches** (`layout_learn`). A new design starts
  `provisional`; it becomes `proven` after `LAYOUT_PROVEN_AFTER` (3) statements
  prove it, or when an admin confirms it (`layout_confirm`). One file counts once
  per layout, so a bundle cannot promote a layout on its own.
- A **person's role fix that then proves** is learned at once, as a corrected
  layout (`layout_correct`). A fix that does not prove, or a plain confirm, applies
  to that file only and is held in `templates/layouts/.pending/` for an admin
  (`R/fixes.R`: `fixes_pending`, `fix_accept`, `fix_discard`). Columns a person
  drew by hand are never learned.
- Nothing is learned while the bank is in question: a pick the statement disagrees
  with blocks learning until a person confirms the bank (`bank_pick()`'s
  `block_learning`), and a statement in a bundle that points at another bank never
  teaches the picked bank's layouts.
- `layout_match(signature, layouts)` scores date format, heading words and
  relative column places (0.70 to match). A layout is a **hypothesis, re-checked
  every time**: it supplies what a page cannot prove (which way a card's signs run)
  and is the first candidate next time, but the arithmetic still decides.

### 5a. Two ways variability is absorbed - read this before "add a synonym"
The engine faces two *different* variability problems and solves them two ways:

1. **Transaction tables (the core).** Rows have no per-row labels - they live in
   columns, and the reader finds the columns from what is printed in them (dates,
   money, words) and proves which is which by the arithmetic. Heading words are a
   vote, never proof. **No synonym list is involved, and none should be added.**

2. **Single labelled values** (opening/closing balance, statement period,
   account name). *Here* wording varies wildly - "opening balance" vs "balance
   brought forward" vs "starting balance" vs "opening:". These are matched through
   the **label dictionary** (section 5b), never hardcoded in R.

### 5b. Label dictionary + matcher (single labelled values)
`dictionaries/labels.yaml` is the one place a maintainer teaches the engine new
wording. Matching is case-insensitive substring. A **matcher spec** is:
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
read from the label line, or the next line if the label is a heading; disagreeing
repeats are **flagged, never silently guessed**. `extract_metadata()` reads
opening/closing balance through this dictionary - no label words live in R.

### 5c. Statement bundles (PDF)
A file that holds **more than one statement** is split only when it is safe
(`R/split.R`, `bundle_segments`): the boundaries come from a deterministic page
marker (each "Page 1 of N" reset), and the number of statements is
**independently confirmed** by a *different* structural count (distinct periods,
or repeated opening/closing blocks) - checked before any reading. Each statement
is then identified (its bank) and read on its own pages, proven by its own
arithmetic, and the outcome rolls up to the **weakest** statement
(`bundle_combine`). Output rows carry a `statement_index` column; per-statement
results are in `result$reading`.

Per-statement proof is added confidence, not a substitute for the count: a running
balance is continuous across any cut, so a wrongly-placed boundary would still
add up within each piece. When the count is not confirmed the file is read whole.
A whole-file reading of something that looks like several statements converts
only when the arithmetic proves every row (consecutive statements of one account
chain cleanly; statements of different accounts break the chain); otherwise it
goes to a person with the *several statements in one file* diagnostic.

## 6. Function interfaces (exact signatures)
- `read_input(path) -> input` : `list(kind, path, sha256, lines=NULL, table=NULL, pages=NULL, words=NULL, meta)`. Dispatch by extension. A scan's pages are OCR'd here; `meta$ocr_timed_out` names any page whose OCR ran out of time (read as blank).
- `bank_identify(input) -> list(institution, bank_code, confidence = high|medium|low|unknown, why, evidence)`. Never returns or stores an account number. `bank_pick(identified, chosen = NULL, confirmed = FALSE)` -> the bank to use, whether to ask, and `block_learning`.
- `auto_read(input, layouts = list(), bank = NULL, opts = list()) -> reading` : `outcome` (`proven` / `layout_match` / `check` / `unread`), `why`, `template`, `parsed`, `recon`, `transactions`, `proof`, `checks` (data.frame `check, ok, why`), `candidates`, `columns`, `matched_layout`. Spec Appendix A1 is the field-by-field contract. `opts$roles` carries a person's role fix from Please check.
- `parse_statement(input, template) -> parsed` : `list(transactions, extras, header, provenance)` per schema above (CSV / Excel); `parse_pdf_table()` for a PDF, which takes `template$table$columns_by_page[[p]]` for page `p` when present.
- `parse_date(x, fmt) -> list(iso, raw)`; `parse_amount(x, style, ...) -> list(value, direction, raw)`; `clean_description(x) -> character` (verbatim-preserving: only `trimws`).
- `reconcile(parsed, template) -> list(kpis=data.frame, trust=list(level, score, reasons))`. KPI rows: `name, status(pass|fail|na), expected, actual, discrepancy, detail`.
- `write_outputs(parsed, recon, outdir, basename, formats=c("xlsx","csv","json")) -> character[]` (paths written).
- `write_log_record(logdir, subdir, id, record) -> invisible(path)`. **One JSON file
  per event, never an append** (section 10). It never overwrites: a clashing id gets a
  `~2` suffix.
- `convert_statement(path, bank=NULL, outdir="out", logdir="logs", formats=c("xlsx","csv","json"), log=TRUE, layouts_dir=NULL, tracking_dir=NULL, overrides=NULL, confirm=FALSE, bank_confirmed=FALSE, requested_by=NULL) -> result`. **Never throws.**
  - `bank`: only when the person changed it; left NULL, the bank is taken from the statement. A "bank" holding a long digit run (an account number typed in the box) is not used, and a message says so.
  - `layouts_dir` / `tracking_dir`: default to `paths$layouts` / `paths$tracking`; `tracking_dir = NA` turns tracking off.
  - `overrides$roles`: a named vector from a found column's field (`result$columns$field`, e.g. `debit`, `other1`) to `debit` / `credit` / `amount` / `balance` / `other`. `overrides$columns`: drawn boxes, `data.frame(field, x_min, x_max[, page])`, PDF only, pages of the FILE. `overrides$statement`: which statement of a bundle the fix is for.
  - `confirm = TRUE` on a `needs_review` reading -> `ok` with `feed_basis = "person"`; refused, with a message, when a balance, opening/closing or printed-totals check contradicts the reading, or when a fix sent with it did not apply.
  - `bank_confirmed = TRUE`: the person kept their pick against the statement's disagreement, which unblocks learning (but never teaches the picked bank from a statement in a bundle that names another).
  - Returns `list(status, outcome, reason, feed_basis, bank, reading, columns, learn, fix_held, person, derived, spot_check, stamp, kpis, trust, header, transactions, outputs, diagnostics, coverage, messages, run_id, run_log, ...)`.
- `spot_check_record(result, verdict = "right"|"wrong"|"cant_tell", tracking_dir)`: the person's answer to a spot check.
- `convert_batch(paths, ..., banks = NULL, overrides = NULL, progress = NULL, done = NULL) -> data.frame(file, status, outcome, bank, chosen, layout, rows, trust, failing_check, message, result)` - a loop over `convert_statement()`; `failing_check` may be `reading:<check>` or `diag:<category>`.
- `identify_file(path, name) -> list(ext, format, kind, pages, state, bank, bank_display, bank_code, confidence, ask, detail)` - the Convert table's row: the bank the statement names, without OCR. `state` is `ready | scanned | scanned_no_ocr | unreadable | unsupported_type`. `identify_scan(path, name)` does the same for a scan's first pages in a background job. `bank_choices(layouts_dir)` feeds the bank dropdown.
- Layouts: `layouts_load(dir, bank, include_retired)`, `layouts_banks(dir)`, `layouts_state_id(dir)`, `layout_match(signature, layouts)`, `layout_learn(reading, bank, file_sha, dir)` -> action `created | evidence_added | promoted | none`, `layout_confirm(id, dir, by)`, `layout_correct(id, template, dir, by, bank)`, `layout_retire(id, dir, by)`, `layout_rename(id, name, dir)`, `layout_display_name(layout, bank_display)`.
- Held fixes: `fix_hold(template, bank, kind, by, dir)`, `fixes_pending(dir)`, `fix_accept(id, dir, by)`, `fix_discard(id, dir)`.
- Tracking: `track_record(fields, path)` (an allowlist of named, typed fields; no free text), `track_summary(path, since)`, `track_export(path, out, since)` (counts only).

## 7. Status model (`result$status`)
The reader's outcome decides it:

| Reader outcome | `status` | `feed_basis` |
|---|---|---|
| `proven` | `ok` | `proven` |
| `layout_match` | `ok` | `layout_match` |
| `check` | `needs_review`, with the reader's reason | `none` |
| `check`, then confirmed by a person | `ok` | `person` |
| `unread` on every statement in the file | `unsupported` | `none` |
| the file itself could not be read | `failed` | `none` |

Three things hold a reading back that would otherwise convert: a scan proven on
poor OCR confidence still goes to a person; a statement with no balance and no
totals is not converted on a layout while its bank is in question; and an amount
filled in from the balance always sends the statement to Please check. Every
non-`ok` status carries an **actionable** message: *why* (the reader's sentence,
naming the failing row and page) + *what it needs*.

## 8. Two sets of checks: the reader's, and the reconciliation KPIs
**The reader's hard checks decide the outcome.** `auto_read()` returns them in
`reading$checks` (`check, ok, why`), all or nothing: `rows_read`,
`rows_match_columns`, `pages_with_rows`, `words_used_once`, `lines_accounted`,
`dates_settled`, `dated_lines_used`, `pages_complete`, `balance_chain`,
`chain_across_pages`, `opening_closing`, `printed_totals`, `dates_readable`,
`dates_in_order`, `dates_in_period`, `signs_settled`, `no_derived_amounts`,
`amounts_read`, `unique`, `rows_proven`, `reader_agrees`, `dates_carried`,
`other_tables`, `ocr_complete`. Each has wording in `READING_CHECK_PLAIN`
(`ui_labels.R`) and a place in tracking's allowlist (`TRACK_CHECKS`,
`R/tracking.R`). A proven reading is one where every check holds and **no other
reading of the columns does**.

**The reconciliation KPIs below still run on every reading**, fill the Checks
table and the trust level, and are what the workbook's `Checks` sheet holds. They
no longer decide the status on their own.

The authoritative KPI list is the one in `reconcile()` at the bottom of
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
| `account_number` | the account number printed is in the four-part NZ shape (2-4-7-2), digits only | not captured, or not in the four-part form |
| `ocr_confidence` | the page-mean OCR confidence (informational) | the statement was not OCR'd |

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
  `kpis`, `trust`, `diagnostics`, `provenance`, `metadata`. The build stamp
  (`build`) is `engine_version`, `reader_version`, `layouts_state`, `layout`,
  `outcome`, `proof_kind`, `institution`, `bank_code`, `bank_confidence`, `kind`:
  what produced this answer, so it can be re-run against the same build and the
  same learned state.

## 10. Logging (`logs/runs/<run_id>.json`, one FILE per run)
`ts, run_id, kind, requested_by, source_file, source_sha256, bank_hint,
institution, bank_code, bank_confidence, file_kind, layout, outcome, proof_kind,
feed_basis, learn_action, person_fix, spot_check, reason, layout_signature,
layout_hint, engine_version, reader_version, layouts_state, status, trust_level,
row_count, derived_amounts, kpi_fail_count, pages, statements, period_start,
period_end, n_accounts, multiple_statements, message`. The bank is its
institution id and two-digit code only. `reason`, `message` and `layout_hint` are
scrubbed of quoted statement text and long digit runs before they are written. One file per event, never a
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
Automatic-reading **tracking** (`logs/tracking/tracking-YYYY-MM.jsonl`,
`R/tracking.R`) is stricter still: an allowlist of named, typed fields (enums, ids,
counts, measurements) with no free text at all, so it carries no file name and
no period dates either.

## 11. Forensic guarantees (non-negotiable, must be tested)
1. **Descriptions verbatim** - special characters preserved byte-for-byte.
2. **Redactions honoured** - a redacted value is `[REDACTED]` + the `redacted`
   flag, never read from under the box. Where the running balance proves what a
   hidden or unreadable amount must be, it is filled in, flagged
   `amount_from_balance`, and the statement goes to Please check (spec section 2,
   "Derived amounts"); it is never presented as read.
3. **No silent drops** - every page with transaction-shaped lines must give rows,
   and every dated line must be used, before a reading can be proven.
4. **Reproducible** - same input + same build + same learned state
   (`layouts_state`) ⇒ identical output (no manual edits). A learned layout is
   never edited in place, so a past state is never overwritten.
5. **Never silently wrong** - an automatic result (`ok` with basis `proven` or
   `layout_match`) is one the statement's own arithmetic proved, uniquely; a
   figure worked out from the balance is marked and always goes to a person.
6. **Never crashes** - all errors become a `failed` status with a reason.

## 12. Testing (golden-file + unit)
Each fixture has an expected core-table snapshot under
`tests/testthat/expected/`. Tests assert: the automatic reader, with nothing
learned, reads the golden figures (`expect_auto_read_golden` in
`tests/testthat/helper.R` - a proven or layout-match reading with any figure off
the golden always fails); the fixture template still reproduces the golden through
the table reader; reconciliation KPIs match; verbatim + redaction guarantees hold.

How accurate it is - as opposed to "has it changed" - is measured outside the
suite, on synthetic statements with an answer key: `tools/synth/score_auto.R`
(the reader) and `tools/synth/score_convert.R` (conversion end to end, figures read
back from the CSV written). Each statement lands in exactly one of `auto_right`,
`AUTO_WRONG` (must be zero), `check_right`, `check_wrong`, `unread`.
