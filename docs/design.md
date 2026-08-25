# Statement Studio — technical design

For a developer inheriting this cold. Read it in one sitting, then read
`docs/context/charter.md` (the rules a change is measured against) and
`docs/context/architecture/build-contract.md` (the exhaustive schema: every
column, every flag token, every KPI, every function signature). This page is the
map, the reasoning, and the traps. It does not restate the contract.

It ends with the three things nobody wrote down before: how to run it (§8), how to
ship a change to it and put it back (§9), and what to do when somebody says a
figure is wrong (§10). The step-by-step versions of those live in
`docs/operational/` — `maintaining-the-engine.md` (the suite on the server,
promoting a template), `updating.md`, `backup-and-restore.md` and
`investigating-a-wrong-conversion.md` — and this page points at them rather than
repeating them.

Everything below was checked against the running code on 2026-07-27, at `VERSION`
1.3.0. Four parts were re-checked on **2026-07-28 at `VERSION` 1.4.0**: §1's
locale rule, invariants **4** and **17** in §5, and §8's *add a check* recipe.
Line numbers are deliberately absent for `app.R` — it moves. Functions and files
are named instead, and every measured figure carries the date it was taken.

---

## 1. Ground rules

- **Pure R.** Base R plus `yaml`, `jsonlite`, `openxlsx`, `readxl`, `pdftools`;
  `tesseract` + `magick` only on the scanned-PDF path; `testthat` for the suite.
  No Python, no `reticulate`, no ML, no network.
- **Air-gapped Windows target, C (non-UTF-8) locale — so every R source the app or
  the engine loads is pure ASCII, byte for byte, literals and comments included.**
  Not a style rule. `R/labels.R` held an en dash inside a regex character class,
  and under `LC_ALL=C` — the stated deployment locale, the locale the suite runs
  in, and the one `run.R` used — it ate the leading byte off any amount printed
  with a currency symbol: a customer value silently no longer verbatim, and
  `validUTF8()` false. Write every glyph as a `\uXXXX` escape — `"\u2713"` for
  the tick, `"\u00a3"` for the pound — seven ASCII characters meaning the same
  thing in every locale and through every editor, mail client and zip on the way
  to that box. A non-ASCII character in an **R name** is worse again: `c("✓ Correct" =
  "correct")` becomes a symbol at parse time, and a C-locale host refuses the file
  outright — the app does not start. The fix already in the code is
  `choiceNames = list(...)` / `choiceValues = list(...)` — literals, never names.
  Guarded by three scans; see invariant 17 for what each one covers.
- **Deterministic.** Same input + same template ⇒ the same bytes out. The xlsx
  writer pins `docProps/core.xml` and normalises zip timestamps for exactly this
  reason (`.deterministic_core`, `.normalize_zip_timestamps` in `R/outputs.R`).
- **Never throws at the front door.** `convert_statement()` wraps its whole body
  in `tryCatch`; any error becomes `status = "failed"` with an actionable message.
  *Whole* body is the load-bearing word. The `tryCatch` used to open below a short
  preamble, so `basename(path)` sat outside it and `convert_document(1L)` threw
  before a single guard ran. Everything that touches `path` is now inside; what is
  left above it cannot throw for any input. If you add a line to that preamble,
  prove it — `convert_document(1L)` must come back `failed`, not error.
- Two helpers are everywhere and are defined in `R/util.R`: `%||%` (null/empty
  coalesce) and `safe(expr, default)` (swallow an error, return a default).

---

## 2. The shape

```
   ui_content.R          ui_labels.R
   (About page,          (STATUS_PLAIN, CHECK_PLAIN, DIAG_PLAIN,
    2-minute guide)       COVERAGE_PLAIN, RESULT_PLAIN, FEED_PLAIN)
          \                   /
           \                 /            copy only - no logic
            v               v
  browser <---- Shiny ---- app.R ------------------------> R/    (the engine)
                             |                              |
                             |                              |
   reads / writes            |                              | reads
   -------------             |                              --------------
   uploads/<id>/  <----------+                              templates/         (shipped, tested)
   logs/runs/     <----------+                              templates_user/    (built in the app)
   logs/feedback/ <----------+                              templates_seed/    (unfinished starts)
   logs/metadata/ <----------+                              fields_templates/  (mode: fields)
   requests/      <----------+                              doc_templates/     (mode: document)
   logs/startup.log <--------+                              doc_templates_user/
                             |                              dictionaries/      (labels, lexicon)
                             |                              config/config.yaml
                             v
                     feed/transactions/   ---> Qlik folder connection
                     feed/review/         ---> the held-back table
                     feed/runs/           ---> one manifest row per conversion
```

**The arrow only points one way.** `R/` never reads `app.R`, `ui_labels.R` or
`ui_content.R`. Verified:
`grep -rn "CHECK_PLAIN\|STATUS_PLAIN\|DIAG_PLAIN\|COVERAGE_PLAIN\|FEED_PLAIN\|RESULT_PLAIN\|ui_labels\|ui_content" R/`
returns comment lines only. The engine emits **codes**
(`needs_review`, `balance_reconciliation`, `withheld:not_proven`); the screen owns
**sentences**. `tests/run_tests.R` sources only `R/`, so a UI symbol is simply not
in scope for the suite: an engine call into one errors the moment a test walks
that line. (`test-seams.R` deliberately `sys.source`s `ui_labels.R` into a private
environment of its own, precisely so that reading both halves stays a special
case.)

`run.R` is a thin CLI over the same engine and loads only `R/`. That is the
second proof that the engine does not need Shiny — and it is also the tool you
reach for when somebody says a figure is wrong (§10).

```
Rscript run.R <file> [bank] [outdir]
```

Sizes, for orientation only — these move every week, so re-measure rather than
quote: `R/` is 46 single-concern modules totalling a bit over twice the length of
`app.R`; `app.R` is a few thousand lines; `ui_labels.R` and `ui_content.R` are a
few hundred between them.

Where the **words** are matters more than where the lines are, and that ratio is
worth a real measurement. An AST walk over every string literal in `app.R`, the
two `ui_` files and `R/`, counting whitespace-separated purely-alphabetic tokens
(so hex colours, CSS lengths and snake_case codes drop out):

| Measured 2026-07-27 | Share of user-facing words |
|---|---|
| `app.R` | ~45% |
| `ui_content.R` + `ui_labels.R` | ~10% |
| **`R/`** | **~45%** |

Nearly half the words a user reads are written by the engine — KPI details in
`R/reconcile.R`, how-to-fix advice in `R/diagnose.R` — because only the engine
has the figures. **A copy audit that reads `app.R` and the two `ui_` files misses
about half of what a user reads.** That is the durable fact here; the percentages
drift with every release.

---

## 3. The path a statement takes

Front door: **`convert_document(path, ...)`** in `R/forms.R`. It tries three
pipelines **in this order**, and only moves on when the one before returns
`unsupported` and no template was forced:

1. the **statement** pipeline (`convert_statement()`),
2. the **form** pipeline (labelled values by wording, `convert_form()`),
3. the **document** pipeline (many tables, `convert_tables()` in `R/doc_extract.R`).

The order is the point: the most checkable case is tried first, and the least
checkable last. It writes the run log exactly once, after the final outcome is
known, and stamps `kind` (`statement` / `form` / `tables`) — which is what
`write_feed()` reads to refuse the last two.

The statement pipeline is **`convert_statement()`** in `R/convert.R`, top to
bottom:

| # | Call | File | What it produces |
|---|---|---|---|
| 1 | `load_template_set(templates_dir, user_templates_dir)` | `R/templates.R` | the detection set. Shipped load first and win any id clash; user templates fill in the rest. A user template marked `hidden:` is left out, anything marked `draft:` is refused at load, and anything marked `sample:` is dropped from the detection set |
| 2 | `read_input(path, redaction_rects)` | `R/read_input.R` → `read_delimited` / `read_excel_input` / `read_pdf` (+ `ocr`, `ocr_preprocess`, `detect_redaction`) | one `input` shape whatever the format: `kind, path, sha256, lines, table, pages, words, meta` |
| 3 | `extract_metadata(input)` | `R/extract_metadata.R` | period, opening/closing balances, account, page counts — read through `dictionaries/labels.yaml`, never hardcoded wording |
| 4 | `detect_multiple_statements(input, meta)` | `R/extract_metadata.R` | is this one file holding several statements? |
| 5 | `layout_signature(input)` | `R/layout.R` | a stable, PII-light shape fingerprint, used by Admin to cluster unseen layouts |
| 6 | `detect_statement(input, templates, hint_bank, hint_type)` | `R/detect.R` | `template_id, score, matched, margin, runner_up, candidates, eligible_ids, tied, detail` |
| 7 | `split_bundle(input, tpl, meta)` **or** `parse_statement(input, tpl, ...)` | `R/split.R`, `R/parse.R` / `R/parse_pdf_table.R` (+ `R/normalise.R`) | `parsed`: `transactions, extras, header, provenance` |
| 8 | `reconcile(parsed, template)` | `R/reconcile.R` | `kpis` (10 checks) + `trust` (`level`, `score`, `reasons`) |
| 9 | `build_diagnostics(status, ...)` | `R/diagnose.R` | where / why / how bad / **who fixes it** |
| 10 | `write_outputs(parsed, recon, outdir, base, formats, ...)` | `R/outputs.R` | 6-sheet xlsx, csv, json — all stamped with `engine_version()` and `template_sha256()` |
| 11 | `field_coverage(parsed, template)` | `R/coverage.R` | populated / partial / empty / unmapped per field |
| 12 | `capture_metadata(...)` + `write_metadata_record()` | `R/metadata_capture.R` | local-only corpus, `logs/metadata/<run_id>.json`, never fed |
| 13 | `log_run(logdir, result)` | `R/convert.R` → `write_log_record` (`R/logging.R`) | one JSON file per run |

`R/feed.R`'s `write_feed()` is **not** in that list. It is called by the Convert
button in `app.R` only — the one place a person has looked at the verdict. Admin's
bulk re-audit and the CLI scripts deliberately do not publish.

`convert_batch()` (`R/batch.R`) is a loop over `convert_document()`, nothing more,
so a batch answer and a single-file answer for the same statement cannot disagree.

### Status, decided in `convert.R`

```
not matched, no tie                      -> unsupported
matched (or tie) but 0 rows parsed       -> unsupported   (with matched_empty naming the template)
any KPI failed, or trust low,
  or an unresolved bundle, or a thin
  detection margin, or a tie             -> needs_review
otherwise                                -> ok
```

### Trust, decided in `.reconcile_trust()`

`high` = every applicable check passed · `medium` = one or more could not run ·
`low` = any failed. Then three caps that can only lower a `high`: completeness
unverified, an unresolved or inferred year, and OCR on any page. Plus one lift: a
`low` caused **only** by secondary checks when `balance_reconciliation` passed
becomes `medium`.

**"Applicable" excludes the informational ones.** `.reconcile_trust()` starts by
dropping every KPI flagged `informational`, so a redaction count or an OCR
confidence figure can never move the level or the score by being a count. OCR
still caps trust — but through the caps below, from the header, not by being read
as a failed check.

**Consequence a newcomer must know: a PDF or Excel statement can never reach
`high`.** `no_unparsed_rows` needs an independent physical source-line count,
which only the delimited reader has (`R/parse.R` and `R/parse_pdf_table.R` both
set `source_line_count = NA_integer_`), so it returns `na`, so `n_na > 0`, so the
ceiling is `medium`. Measured 2026-07-27 on
`samples/_private_staging/anz_single.pdf` — a clean 11-page ANZ PDF, 311 rows,
balance reconciling — it comes out `medium (92)`. That is the healthy ceiling for
the format the charter calls dominant. Do not "fix" it by making the check pass.

---

## 4. The template schema — the extension point

The charter rule is **"a new bank is a YAML template, never new code."** If a
change would make adding a bank require an edit in `R/`, the change is wrong.

Templates live in three places: `templates/` (shipped, `origin = "default"`,
each with a golden test), `templates_user/` (built in the app, `origin = "user"`),
`templates_seed/` (unfinished starting points, marked `draft: true`).
`fields_templates/` is a separate world for labelled-value documents — it does not
take part in transaction detection at all.

Today: **13 template files ship, 12 take part in detection** (`tutorial_everyday_pdf`
carries `sample: true` and is excluded).

### Delimited / Excel

```yaml
id: anz_everyday_csv          # unique; also the filename slug
bank: ANZ
statement_type: everyday
format: delimited             # delimited | excel | pdf
version: 1
min_score: 6                  # matched only when the fingerprint score reaches this
fingerprint:
  header_contains_all: [Type, Details, Particulars, Code, Reference, Amount, Date]
  filename_regex: null        # TIE-BREAKER ONLY, never part of the score
delimiter: ","                # may be a LIST; resolve_delimiter() picks one
preamble: null                # optional {header_regex: ...} for exports with a banner
columns:                      # canonical field -> source column name
  date:        {source: Date, format: "%d/%m/%Y"}   # format may be a LIST
  amount:      {source: Amount}
  description: {source: Details}
  balance:     null           # null = this export has no such column
amount_sign: signed           # signed | debit_credit_cols | dr_cr_suffix | type_dc | unsigned
extras:                       # kept, named, out of the core schema
  fx_amount: {source: ForeignCurrencyAmount}
currency: NZD
```

### `min_score`, and why `header_contains_all` does not mean ALL

One number decides both of this engine's detection failure modes, so it gets a
paragraph of its own.

**A score is a count of fingerprint phrases found, one point each.** For a
delimited or Excel template, a phrase scores if it equals a whole column name
(case-insensitively); for a PDF, if it appears anywhere in the page text as a
fixed substring. `filename_regex` scores **nothing** — it is a tie-breaker only,
so a template can never win on the file's name with no content evidence, and the
same bytes under a different name always detect the same way. `R/detect.R`,
`.score_template()`.

A template **matches** only when it clears **its own** `min_score` *and* strictly
beats the runner-up among the candidates that cleared theirs. A tie on content is
broken by shipped-over-hand-built, then by the filename hint; if neither separates
them the run converts on the best candidate and is held for review (§6), with the
ordering still settled by id so the outcome stays deterministic. Winning by only
one point is a *thin margin* and also routes to review — a near-duplicate template
nearly matched too, and that is worth a second pair of eyes.

So the example above — `min_score: 6` over seven `header_contains_all` phrases —
means **"any six of these seven"**. The key is named `_all` and that is not what
it does. The one number is a dial between two opposite ways of being wrong:

- **Too low** and the fingerprint claims other banks' statements. Not
  theoretical: `asb_everyday_pdf` once carried `min_score: 2` over
  `["Debit/Withdrawal", "Deposit", "Balance"]`, and a foreign bank's PDF scored 2
  on the bare words "Deposit" and "Balance", parsed as ASB, came out `ok`, and
  reached the Qlik feed. `.fp_fingerprint_problems()` exists to refuse that
  shape now.
- **Equal to the phrase count** and the template dies the day the bank renames
  one column or moves one line of the masthead — the layout is unchanged, the
  score drops by one, nothing matches, and the analyst is told to build a
  template for a bank the tool already has.

The `detect_detail` field in every run log records what happened, e.g.
`matched anz_everyday_csv (score 7/6)` — score over threshold. That is the field
to read when a detection surprises you.

### PDF

Same header keys; the table half moves under `table:` and the fingerprint matches
page **text**, not column names.

```yaml
format: pdf
min_score: 2
fingerprint:
  page_contains_all: ["Your transactions", "OPENING BALANCE"]
table:
  row_tol: 3                  # points; how far apart two words can be and still be one row
  date_format: "%d %b"        # ONE format here - a list would be recycled per row
  amount_sign: debit_credit_cols
  ref_width: 595.28           # THE BAND FRAME (see §5). Absent -> A4 assumed.
  ref_height: 841.89
  columns:
    date:        {x_min: 40,  x_max: 78}
    description: {x_min: 90,  x_max: 355}
    debit:       {x_min: 355, x_max: 400}
    credit:      {x_min: 400, x_max: 470}
    balance:     {x_min: 470, x_max: 535}
  metadata_regions: {}        # optional: pin a header value to a drawn box
  extras: {}
currency: NZD
```

Optional on any template: `split:` (§6), `hidden: true`, `sample: true`,
`draft: true`, `refines: <id>`, `effective_from` / `effective_to`,
`decimal_mark: auto|dot|comma`, `type_debit_value` (required by `type_dc`),
`unsigned_default: debit|credit` (required in spirit by `unsigned`).

### `validate_template()` is the gate, and it is opinionated

Read it before adding a key. Three rules matter more than the rest:

1. **Distinctiveness.** `.fp_fingerprint_problems()` refuses a fingerprint that
   could match another bank. Stricter for PDF (phrases are substring-matched
   anywhere on the page, so `min_score` must not be reachable by generic phrases
   alone) than for delimited (phrases must equal a whole column name). This is not
   theoretical: the ASB PDF template once scored its `min_score` of 2 on the bare
   words "Deposit" and "Balance" and parsed a foreign bank's PDF as ASB, `ok`,
   fed to Qlik.
2. **No PII.** A phrase that names a person identifies a customer, not a layout.
   Refused at load, on the hand-edited Advanced-YAML path too.
3. **A date format list is allowed only where the reader resolves it.**
   `resolve_date_format()` picks the one candidate that reads the **whole**
   column, or `NA`. A list anywhere else is rejected loudly, because base
   `as.Date()` recycles a format vector element-wise: row 1 as `%Y/%m/%d`, row 2
   as `%d/%m/%Y`, a whole column of plausible wrong dates.

### `mode: document` — the third template kind

Everything above is a **statement**: one table of transactions, columns running
the full height of every page, judged against a running balance. There are two
other modes, and they exist because all three of those facts are false for other
documents.

| mode | Engine | What it reads | Checked against | Where it goes |
|---|---|---|---|---|
| *(absent)* / statement | `R/parse_pdf_table.R` | one transaction table | a running balance | Excel/CSV/JSON **and the Qlik feed** |
| `fields` | `R/forms.R`, `R/extract_fields.R` | labelled values, found by **wording only**, no coordinates | nothing | download only |
| `document` | `R/tables.R`, `R/tables_detect.R`, `R/doc_extract.R` | **many tables of different shapes**, plus label/value pairs | nothing | download only |

**Why `document` is a separate engine and not a wider statement parser.**
Widening the statement parser would have put the least checkable case inside the
code path every real conversion runs through. The statement path is not
destabilised by report work; that is the whole reason for the split.

**Download only, enforced rather than assumed.** `write_feed()` refuses
`kind = "tables"` outright — one line, with a test. There is no reconciliation
behind a report and nothing that could tell a wrong figure from a right one, so
publishing one as if there were would be the worst thing this tool could do.

The schema, in the shape the builder saves it:

```yaml
id: acme_valuation_report
mode: document                 # the discriminator; is_document_template() reads it
format: pdf
ref_width: 595.28              # the page size the boxes were drawn in
ref_height: 841.89
fingerprint:
  page_contains_all:           # same gate as the other two modes, same reasons
    - Consolidated position report
tables:
  schedule_of_transactions:
    name: Schedule of transactions
    start: {page: 3, y: 148.0}   # a POSITION on a page, not a whole page
    end:   {page: 5, y: 700.9}
    header_rows: 2               # how many lines the heading takes; NOT a row count
    follow: true                 # keep going if the next page repeats the header
    min_fill: 0.5
    anchor:
      header_text: [Date, Description, Amount, Balance]
      first_column: []
    columns:                     # bands that TILE the width: they meet, never overlap
      - {name: Date,        x_min: 40,  x_max: 110, type: auto}
      - {name: Description, x_min: 110, x_max: 360, type: auto}
pairs:
  prepared_for:
    label_text: Prepared for     # found by WORDING on the next document
    label: {page: 1, x_min: 39, x_max: 100, y_min: 99, y_max: 110}
    value: {page: 1, x_min: 149, x_max: 214, y_min: 99, y_max: 110}
    where: {where: right, gap: 49, cross: 0, width: 65, height: 11}
    type: text
```

Four things in there are load-bearing and easy to undo by accident:

1. **A table is a `(page, y)` START and a `(page, y)` END**, not a set of pages.
   Reports put two tables on one page and run a third over three; whole-page
   boundaries cannot express either.
2. **Columns are EDGES, not boxes.** `doc_column_edges()` / `doc_columns_from_edges()`
   turn N columns into N+1 shared dividers, so a **gap** between bands (which
   loses a column of figures) and an **overlap** (which deletes one) are not
   expressible. `doc_set_column_band()` moves one column and the neighbours give
   way, preserving the tiling. `.doc_table_problems()` still refuses both, because
   a template can be hand-edited.
3. **A pair stores the SIDE, not an offset.** `where.where` is
   `right`/`left`/`above`/`below`; on the next document the label is found by its
   wording and the search runs in that direction (`.doc_pair_window()`,
   `.doc_pair_pick()`). A fixed offset misses as soon as the figure is a digit
   longer, which on a re-print it usually is. A template with two boxes and no
   `where` still reads — the relation is worked out from the boxes.
4. **`header_rows` is how tall the heading is, never a row count.** The number of
   data rows is read from the page, between the start and the end.

**The reader tells you where it is unsure, and never silently drops anything.**
Two numbers travel with every table: **unclaimed words** (inside the boundary,
claimed by no column — a band drawn a few points too narrow loses a column of
figures and every remaining row still looks perfect) and **thin columns** (mostly
empty). A one-column table can have neither, so it is called out separately.

**Nothing in these three files is specific to any document.** No fixture wording,
bank name or column name appears in `R/tables.R`, `R/tables_detect.R` or
`R/doc_extract.R`; the default column kind is `auto`, which means the page's own
words stand uncoerced; and a document template carries no `currency`, because
nothing in this mode reads one. `tools/corpus/` exists to keep that honest — it
surveys the engine against a folder of real PDFs nobody here wrote.

### The two ways variability is absorbed — read before adding a synonym

- **Transaction tables**: rows have no per-row labels, they live in columns.
  Wording never matters; `columns:` maps a source column or an x-band to a
  canonical field. **No synonym list is involved and none should be added.**
- **Single labelled values** (opening/closing balance, period, account name, IRD
  form fields): here wording varies wildly, and it is matched through
  `dictionaries/labels.yaml` via `R/labels.R`. Adding a bank's odd wording is one
  line of YAML, never R.

`dictionaries/lexicon.yaml` (`R/lexicon.R`) is the third vocabulary: marker words
the engine looks for (e.g. the `CR` payment marker). Both dictionaries are edited
in Admin and are **live server state** — `scripts/bundle-offline.R` deliberately
does not ship them, and a test pins that.

---

## 5. Invariants that must never break

| # | Invariant | Guarded? | What enforces it |
|---|---|---|---|
| 1 | The engine never reads a front-end file | structural | `tests/run_tests.R` sources only `R/`, so a UI symbol is simply not in scope: an engine call into one errors the moment a test walks that line. `run.R` runs the whole pipeline with no Shiny at all. There is no assertion that greps `R/` for a UI symbol — the structure is the guard |
| 2 | Every code the engine can emit has wording, and there is no dead wording | **yes** | `tests/testthat/test-seams.R` — `expect_setequal` **both ways** for `CHECK_PLAIN` vs `reconcile()`'s builder list, `DIAG_PLAIN` vs **`.DIAG_FIX_OWNER`**, and **`INFORMATIONAL_CHECKS`** vs the `.kpi_*()` builders that pass `informational = TRUE`, plus `STATUS_PLAIN`, `RESULT_PLAIN`, `COVERAGE_PLAIN`. Those three symbol names are the ones a new check has to be added to; see §8 |
| 3 | A check label claims only what its check proves | **yes** | `test-seams.R`, "no check label claims more than its KPI proves" — it forbids "every row" on `no_unparsed_rows`, "matches" on `transaction_count`, "honoured" on `redaction_summary` |
| 4 | "this check could not be proved" and "this template has no column for that field" must read differently | **yes** | `test-seams.R` — `RESULT_PLAIN[["na"]]` must not equal or contain `COVERAGE_PLAIN[["unmapped"]]`. They render inside the same disclosure, and both once read *not on this statement* — a claim about the FILE that neither value is in any position to make |
| 5 | One run, one audit record, never overwritten | **yes** | `write_log_record()` (`R/logging.R`) suffixes a clashing id with `~2`; `test-run-log-identity.R`, "two runs with the same run_id leave TWO audit records" |
| 6 | Nothing under a redaction is read, and a scan that could not finish fails the run | **yes** | `R/detect_redaction.R` + the `read_pdf` guard + `.kpi_redaction_scan()` returning `fail`; `test-detect_redaction.R`, `test-redaction_delimited.R` |
| 7 | Descriptions verbatim | **yes** | `clean_description()` is `trimws` and nothing else; the golden tests carry apostrophes and ampersands |
| 8 | Byte-reproducible outputs | **yes** | `test-outputs.R`, "xlsx / csv / json are byte-reproducible across runs" |
| 9 | Every stored PDF band lives in ONE coordinate space | **yes** | `pdf_band_frame()` / `pdf_band_frame_scale()` (`R/parse_pdf_table.R`) are the only converters; `test-row_coverage.R`, "row_coverage.R keeps no private copy of the band frame" |
| 10 | The reader, *See it on the page* and the coverage report share ONE keep rule | **yes** | `pdf_keep_row()` is called by `parse_pdf_table()` and by `R/inspect.R` (which draws that screen); `test-inspect.R` asserts the counts cannot diverge |
| 11 | The feed gate is machine-only, and nothing but a clean run from an allowed origin reaches `feed/transactions/` | **yes** | `.feed_gate()` (`R/feed.R`) takes no human input; `test-feed.R` covers accept, withhold, re-convert flip, atomic write, NA trust, content-hash keying |
| 12 | A feed write failure is never recorded as a clean accept | **yes** | `.atomic_write_csv()` captures warnings as well as errors (a read-only share warns and returns normally); `test-feed.R`, "a failed feed write is reported" |
| 13 | The `R/` module map in the build contract matches `R/` in both directions | **yes** | `test-deployment-docs.R`, "build-contract.md's module map matches the modules that exist" |
| 14 | Every path into this tree that a document or a comment names, resolves | **yes** | `test-deployment-docs.R` — markdown links across `README.md`, `CHANGELOG.md` and every `.md` under `docs/`, plus a second pass over `R/`, `app.R`, the `ui_` files, `scripts/`, `tests/`, `www/` and the two template-folder READMEs for any path-shaped `.md` reference however it is written |
| 15 | No documentation page is orphaned from its index | **yes** | `test-deployment-docs.R`, "no docs page is orphaned from its index" — `docs/operational/` and `docs/context/` against their own `README.md`, and `docs/*.md` against the repo `README.md` |
| 16 | A skipped test is a failure | **yes** | `tests/run_tests.R` exits 1 on any skip unless `BSO_ALLOW_SKIPS=1`. Counted 2026-07-27: **168** skip guards, which between them can dark the whole PDF/OCR/Excel surface while the board stays green. This is why the exit code, not the summary line, is the pass condition |
| 17 | No non-ASCII **byte** in any R source the app or the engine loads — string literals and comments included, not only names | **yes** | Three scans, and each has a companion test that feeds it a known-bad file so no guard can go quiet. The two byte scans are what the rule now is: `test-labels.R`, "no non-ASCII byte survives anywhere in `R/`, `run.R` or the test runner", and `test-app-adoption.R`, "no non-ASCII byte survives in the three UI files, literals included" — between them `app.R`, both `ui_` files, all of `R/`, `run.R` and `tests/run_tests.R`, **52 files, byte by byte** (2026-07-28). Both also assert the escaped glyphs are still *there*, so a file cannot pass by deleting the character it needed. `test-app-ui.R`, "no R name anywhere carries a non-ASCII character (invariant 17)", reads the *parse tokens* over the same files and stays, because a bad NAME is the worse failure: a C-locale host will not parse the file at all. **`tests/testthat/` is deliberately outside all three** — ten of those files carry a non-ASCII byte on purpose, as the input that proves a scan works |
| 18 | The screen's colours come from the design system, not from the file they are typed in | **no** | **Unguarded.** The only test that looks at this forbids CSS style *tags*, and passes while the inline style *attributes* it does not read carry a private palette. Open as `N46`; see §7 |

Rows marked **structural** need no assertion because the code cannot express the
violation. The one marked **no** is the point of the column: a reader scanning
this table for "what will the suite catch me on" must not get a false negative
from an invariant that is filed somewhere else.

---

## 6. Deliberate decisions a newcomer would otherwise undo

**Detection prefers a shipped template over a hand-built one.**
`detect.R` orders by content score, then `defaults` (shipped beats user), then the
filename tie-breaker, then id. It used to fall through to the alphabetical id
tie-break, so a hand-built `aaa_bank` beat a tested `westpac_everyday_pdf` on
nothing but its name — a template's *filename* deciding which figures reach a
dashboard. A shipped template has a golden test proving it reads that bank
correctly; a user template has not.

**...with exactly one exception, and it is not a bug.** A template saved from the
toolkit after opening a shipped one to fix it carries `refines: <id>`. It ranks
one step above **the single template it names**, and nothing else. Without that,
the analyst's correction ties with the thing it is correcting, loses on
"shipped wins", and nothing on screen explains why. Governance is untouched: it is
still `origin = "user"`, so it still cannot reach the dashboards.

**A tie converts. It does not stop and ask.** `convert.R` takes the best
candidate, converts, and holds the run at `needs_review` with the tied ids named.
The charter's interface rule is why: an accountant cannot know which internal
variant a bank used to print her statement, so asking her is asking a question the
person in front of you cannot answer — and it left her with no spreadsheet over a
problem that was never hers. Every figure still goes through full reconciliation,
so the wrong variant surfaces as a failing check, and `needs_review` never reaches
the dashboards.

**Splitting a bundle is gated on an INDEPENDENT count, not on reconciliation.**
`split_bundle()` refuses unless `.count_agrees()` finds a *different* structural
count (distinct periods, page-1 resets, or repeated opening/closing blocks) that
agrees on the same number of statements — checked **before** any parsing work.
The temptation is to drop that and rely on "every segment reconciled". Do not: a
running-balance column is continuous across **any** cut, so a wrongly-placed
boundary still reconciles inside each piece. Only an independent count can confirm
a boundary. Measured working (2026-07-27): `anz_multiple.pdf` splits into 6
statements, 1,253 rows, each reconciled on its own anchors, trust rolled up to the weakest segment.

**Auto-split is on by default; `split: false` opts out.** It used to be opt-in and
exactly 1 of 13 shipped templates opted in, so a bundle read by any of the other
twelve fell through to the merged parse and produced the failing balance +
continuity pair the feature exists to remove. The opt-in was never what made
splitting safe — the commit gate above is.

**There are two mechanisms for "one file, several periods", and both are needed.**
They take structurally different inputs. `R/split.R` handles several separately
issued statement **documents** concatenated: each prints its own "Page 1 of N" and
its own opening and closing, so each can be reconciled on its **own** anchors, and
merging them would replace several independent proofs with one weaker one.
`.period_span()` in `R/extract_metadata.R` handles **one** document with several
period sections: there are no document boundaries to cut on, so splitting is not
available, and the only way to make the checks work is to read it as one long
span — and only when the periods are proven contiguous, because the reader takes
its year context from `period_start`/`period_end` and a widened span silently
moved every year-less date by two years once already. Delete either and the other
cannot cover its cases.

**The engine never reads a UI file.** Beyond the seam being tested, this is what
lets `run.R`, `scripts/bulk-audit.R` and the whole test suite run with no Shiny
at all — and it is what keeps `ui_labels.R` genuinely rewordable. Words in `R/`
are the KPI *details* and the how-to-fix advice, which only the engine can write
because only it has the figures.

**`.unique_names()` in `app.R` cannot be replaced by per-file subfolders.**
The obvious-looking cure is wrong. The clash is in the **output** name, not the
input path: `convert_document()` writes to `outdir` under
`file_path_sans_ext(basename(path))`, and `convert_batch()` takes ONE `outdir` for
the whole case. Two inputs at `sess/1/statement.pdf` and `sess/2/statement.pdf`
therefore still both write `sess/statement.xlsx`, and both rows' download buttons
still point at the same workbook. This function is the only thing standing between
a 30-file case and one statement's figures downloading as another's. The "(2)"
suffix is visible in the table and in the filename, so the clash is stated.

**A hidden Shiny output keeps `.recalculating` for ever, and it is not a bug.**
Shiny stamps `.recalculating` on an output when it asks the server for a value
and clears it when one arrives. An output inside a `conditionalPanel` that is
false is **suspended**: no value is ever sent, so the class never clears. Half a
dozen Convert-tab outputs sit in exactly that state whenever Add a template is
open. No person ever sees it — the elements are hidden. But it means **"wait
until the page is quiet" is not a usable readiness check**, and anything driving
this UI has to count only *visible* busy elements:

```js
Array.from(document.querySelectorAll('.recalculating, .shiny-busy'))
  .filter(e => e.offsetParent !== null).length
```

Measured, on a browser harness that got this wrong: five minutes a document
instead of thirty seconds, every step burning its whole timeout.

**The feed reads `result$feed_rows`, not the output CSV.** It used to re-read the
CSV, and `utils::read.csv`'s type inference silently mangled it: a leading-zero
code `000001` became `1`. `feed_rows` is the same table the workbook shows, taken
**before** `.spreadsheet_safe()` adds the apostrophe that stops Excel executing a
description beginning `=`.

**`min_trust` ships as `medium`, not `high`.** With `high`, a clean statement with
no running-balance column — and every PDF and Excel statement, per §3 — would be
withheld, emptying the feed for whole banks. Policy lives in `config/config.yaml`,
not in the gate.

**`sample: true` and `draft: true` keep templates out of detection.** A worked
example's generic wording could tie with a real statement and mis-parse it with a
demo's hardcoded bands. A half-drawn seed would join detection with placeholder
coordinates. YAML drops comments, so `# TODO draw` cannot be seen by the loader —
the key can.

---

## 7. Where the bodies are buried

**Most of `app.R` is one `server <- function(input, output, session)` scope**, and
that — not its length — is what makes it hard. Something over a hundred `output$`
assignments, and dozens each of `observeEvent`, `reactive()` and `reactiveVal`,
all in one lexical environment where every name is visible to every other. Counts
are deliberately not pinned here; they move weekly, and any of them is one grep
away. Moving the CSS out to `www/app.css` made the file a few hundred lines
shorter and removed **zero** reactive-graph complexity — CSS was the cheapest
lines in it to lose.

**The blocker on splitting it is the test suite, and it has a two-line fix.**
`tests/testthat/test-app-ui.R` reads `app.R` as **text** — the suite never sources
it, because it needs a live Shiny session. Three helpers do the reading, and each
breaks differently if the file is split:

- `.ui_src()` reads the one file, from most `test_that` blocks in that file;
- `.ui_fun()` lifts a function out by regex on `^\s*name <- function` and parses
  forward until it parses;
- `.ui_block()` takes a **fixed n-line window** after a pattern match, hand-tuned
  per test — this is the one that fails silently, by matching a window that no
  longer holds what the test is asserting about.

`test-app-adoption.R` and `test-admin_auth.R` read `app.R` the same way, and
`test-seams.R`, `test-detect.R`, `test-batch.R` and `test-run-log-identity.R`
read it too. Change `.ui_src()` to concatenate `app.R` plus `server/*.R` **first,
in its own commit**, then split with `source(..., local = TRUE)` inside `server()`
— not Shiny modules, which would mean rewriting every input id. The full plan,
with the four proposed files and their line counts, is in
`docs/context/findings-register.md` under "The app.R split".

**`R/parse_pdf_table.R` is by some way the longest module in `R/`** and the one
that is genuinely hard. Read its band-frame header comment before touching
anything with an `x_min` in it. The rest of `R/` is single-concern and small.

**`app.R` still carries a private palette inlined in `style=` attributes.**
Measured 2026-07-27 by an AST walk over its string literals and named arguments:
**49 distinct hex colours across 96 uses, and 141 `style=` attributes**. Several
of the colours are near-misses for the design token they should use — `#b00020`,
the most-used of them, against `--bad: #b3261e`. Nothing *looks* wrong on screen,
which is why this will not self-correct: the failure is that the stylesheet stops
being the place a colour changes. The test that appears to guard it forbids style
*tags* (`tags$style(`) and never reads a style *attribute*, so it passes and
proves nothing — it is invariant 18, the unguarded row of §5. Open as `N46`.

**`STATUS_PLAIN_AMBIGUOUS` and the `cv_tie_pick` panel are near-dead.** A tie now
converts (§6), so the ambiguous headline and the picker only apply on the narrow
path where a tie's winner then read zero rows. Read the comments in `ui_labels.R`
before reviving either.

**Two findings are open on evidence, not effort** (`N15`, `N16` in the register):
the form fingerprint gate still admits a two-word ubiquitous phrase, and scanned
OCR digit accuracy has never been measured against a hand-keyed golden. Both fail
closed and loudly today.

---

## 8. Working on it safely

### Run it

```
Rscript scripts/run_app.R                # the app, on config's port, host 0.0.0.0
Rscript run.R <file> [bank] [outdir]     # the whole engine, no Shiny
Rscript tests/run_tests.R                # the suite; from the repo root, several minutes
```

`scripts/run_app.R` — not a bare `runApp()` — is the way in: it self-locates (a
boot service can start it anywhere), reads the port and the upload ceiling from
`config/config.yaml`, and says on the console if the settings file did not parse
or the admin password is still the shipped placeholder. `run.R` and
`scripts/run_app.R` are the two entry points; everything else is called by them.

On the deployment box none of those is the command you type. **`RUN-ME.bat` is
what the Windows server double-clicks**, and it runs `scripts/run_app.R` through
the app's **own private R** under `R-runtime\`, installed with `!recordversion` so
it never becomes the machine's R and is never on `PATH`. The suite has to be run
the same way, naming that executable and the app's own `R-lib\`. Both exact
commands are in `docs/operational/maintaining-the-engine.md` §1 — use them, not a
bare `Rscript`.

The suite sources every file in `R/`, sets `ENGINE_ROOT`, runs
`testthat::test_dir`, and **exits 1 on any skip**. The last measured baseline
lives in one place, `docs/operational/maintaining-the-engine.md` — not in this
file, so a stale number here cannot masquerade as a regression signal.

### To add a bank

Write a YAML template (in the app, or by hand), add a fixture and a golden test.
`tests/HOWTO-add-template-test.md` is the recipe. No R change. If you find
yourself editing `R/`, stop and re-read §4.

### To add a check

The recipe is five places, not three, and the two most often missed are two the
suite fails you on. In this order:

1. **`R/reconcile.R`** — a `.kpi_<name>()` builder. Return `NULL` when the check
   does not apply to this statement; `reconcile()` filters those out, and that is
   how a check stays genuinely absent instead of pretending to be an `na`.
2. **`R/reconcile.R`** — one line in the builder list at the bottom of
   `reconcile()`. `test-seams.R` reads **that list** (not the builders) for the set
   of checks that exist.
3. **`ui_labels.R`** — a `CHECK_PLAIN` entry. `expect_setequal` **both ways**
   against the list in step 2: a check with no wording fails the suite, and so does
   wording for a check that no longer exists. The label must claim only what the
   check proves — invariant 3 forbids specific overclaims by name.
4. **If it can `fail`:** three edits, not two, and the third is the one that
   catches people out — **because it is back in the file you have just left**. In
   **`R/diagnose.R`**, a `.DIAG_FIX_OWNER` entry. In **`ui_labels.R`**, a matching
   `DIAG_PLAIN` entry. Then back in **`R/diagnose.R`**, a few dozen lines below the
   first edit, a **`.KPI_DIAGNOSIS` entry keyed by your check's name**, which is
   what actually
   *raises* the category when the check fails and carries its where / severity /
   how-to-fix sentence. `test-seams.R` compares the first two sets both ways, and
   `test-diagnose.R` re-derives the set the file can really raise by reading it,
   so declaring a category that nothing raises fails the suite as a **dead row**
   (`sort(setdiff(names(.DIAG_FIX_OWNER), raised)) not equal to character(0)`).
   Adding the `.KPI_DIAGNOSIS` entry is what clears it; without one, a failing
   check falls back to a generic "review this check against the source
   statement", under the wrong category and the wrong owner.
5. **If it is a count rather than a verdict:** pass `informational = TRUE` in the
   builder *and* add the name to `INFORMATIONAL_CHECKS` in **`ui_labels.R`**.
   `test-seams.R` re-derives the engine's list by reading `R/reconcile.R` for
   builders that set the flag, and pins the screen's copy against it. Skip it and
   your count renders as *could not be checked* — precisely the silent lie the
   fourth result word exists to prevent.

#### Adding a check breaks about thirty tests. Read this before you start.

Not three. **Measured on 2026-07-28**: adding one ordinary check (*was the
statement period read?*), with all five steps above followed completely, took the
suite from green to **27 failed + 1 error, in 17 tests across 10 files**. That is
the expected outcome of a correct change, not a sign you did it wrong.

Two things make it worse than the number:

- **The runner shows you ten of them.** `tests/run_tests.R` uses testthat's
  `SummaryReporter`, which stops after ten and prints *"Maximum number of 10
  failures reached, some test results may be missing."* Two thirds of the
  evidence is off-screen, and the ten you get are alphabetical, not important.
- **Almost none of them name your check.** Fifteen of the twenty-seven are one
  fact repeated: a failing check turns the run's status from `ok` to
  `needs_review`, so every test asserting a clean conversion fails with
  `r$status not identical to "ok"` — and the feed gate then refuses to write, so
  a test that reads the feed CSV afterwards **errors** on a file that was never
  created.

**So, in this order:**

1. **Record the baseline first.** Run `Rscript tests/run_tests.R` before you
   touch anything and keep the numbers. Everything below is a subtraction.
2. **See all of it.** Raise the reporter's cap. Same run as `run_tests.R` —
   sources `R/`, sets `ENGINE_ROOT`, same directory — with nothing hidden. One
   line, from the repo root:

   ```
   Rscript -e 'suppressMessages(library(testthat)); for (f in list.files("R", pattern="[.]R$", full.names=TRUE)) source(f); Sys.setenv(ENGINE_ROOT = normalizePath(".")); test_dir("tests/testthat", reporter = SummaryReporter$new(max_reports = 999L), stop_on_failure = FALSE)'
   ```

   It prints every failure instead of the first ten. It does **not** replace
   `run_tests.R`: only the runner enforces the skip rule and the exit status, so
   use this to read the damage and `run_tests.R` to decide whether you are done.

3. **Read the per-bank golden tests first — they are the only ones that are
   evidence about your check.** Twelve of the twenty-seven were the six shipped
   bank fixtures (`test-anz_everyday_csv.R`, `test-anz_creditcard_csv.R`,
   `test-asb_everyday_csv.R`, `test-bnz_everyday_csv.R`,
   `test-kiwibank_everyday_csv.R`, `test-westpac_everyday_csv.R`) reporting
   `any(k$status == "fail") is not FALSE` and a trust level that is no longer
   high or medium. Those are **real bank statements**. A check that fires on six
   of six is a check that is wrong, or one that should have returned `NULL`
   because it does not apply — not a fixture that is too thin.
4. **Then the seam tests**, which are the ones that actually name what you
   missed: `test-diagnose.R`'s dead-category failure means step 4 above is only
   half done, and `test-seams.R` fails you for missing wording.
5. **The status cascade last** (`test-convert.R`, `test-batch.R`,
   `test-jobs.R`, `test-feed.R`). Fixing 3 fixes all of these at once; there is
   nothing to read in them individually.

**Where this does *not* land, despite what you would expect:**
`test-reconcile.R` did not fail at all. It has exactly one test that asserts the
whole KPI set is clean (`expect_false(any(r$kpis$status == "fail"))`, inside
*date_year_inferred caps trust*), and its `.parsed()` helper fills in a period,
balances and a stated count — so it is often the *least* thin fixture in the
suite, not the most. If it does fail, read the fixture before you read your
check and decide honestly which is at fault; but do not start there.

### To change a threshold

`R/params.R`, which is the only place a tuning number is allowed to live.
Catalogue in `docs/context/engine-parameters.md`.

### To change what reaches Qlik

`config/config.yaml` → `feed:`. Never the gate.

### Before you delete something that looks redundant

Check `docs/context/findings-register.md`. Several things in this codebase look
like duplication and are not — the two period mechanisms, the two dictionaries,
the separate `feed/review` folder — and the register records the measurement that
decided each one.

---

## 9. Shipping a change, and putting it back

The target is an air-gapped Windows box with no internet, no database, no version
control and no admin rights to spare. That shapes the whole delivery end: there is
no deploy pipeline, there is a folder you carry across.

### The install is one folder, and half of it is live state

```
StatementStudio-offline\
  R\  app.R  ui_labels.R  ui_content.R  run.R  scripts\  www\  RUN-ME.bat
  templates\  templates_seed\  fields_templates\  doc_templates\             <- product
  tests\  samples\  docs\                                                    <- product
  config\config.yaml                    <- LIVE: port, admin password, feed policy
  dictionaries\labels.yaml  lexicon.yaml <- LIVE: every wording Admin has taught it
  templates_user\<id>.yaml              <- LIVE: every template built in the app
  fields_templates_user\                <- LIVE: form templates built in the app
  doc_templates_user\                   <- LIVE: report pullers built in the app
  logs\runs\  logs\feedback\  logs\metadata\  <- LIVE: the audit trail, kept forever
  uploads\<id>\                         <- LIVE: real client statements
  feed\                                 <- LIVE: what Qlik reads
  R-runtime\  R-lib\                    <- the app's own private R, installed on the box
```

**Everything in the second group is server state that exists nowhere else**, and
an update must not touch any of it. Some of it is irreplaceable in the strict
sense — `templates_user\`, `fields_templates_user\`, `doc_templates_user\` and
`dictionaries\` are the accumulated work of the team and cannot be rebuilt from
the repo — which is why
`docs/operational/backup-and-restore.md` exists and why it is the first thing in
the update procedure, not the last.

### Build

On a PC **with** internet, double-click `make-bundle.bat` (it runs
`scripts/bundle-offline.R`). It assembles `StatementStudio-offline\`: the app
files, plus `offline\` holding the R installer for *this PC's* R, every CRAN
package, the Poppler and Tesseract binaries, and `offline\manifest.txt` recording
`app_version`, `r_version` and every package version. Check its last lines for
`MISSING` before you carry it anywhere — a failed download is a hard build error
by design, because the alternative was a bundle that reported success and failed
days later on a box with no internet in the room.

Two omissions from the bundle are deliberate and are pinned by tests:
`config/config.yaml` and `dictionaries/*.yaml` ship only as `.example` files, so
copying a fresh bundle over a live server cannot revert its settings or its taught
words. `RUN-ME.bat` seeds them on a first install and restores them from
`%LOCALAPPDATA%\StatementStudio` if the folder's copies have gone.

### Copy, and what the copy replaces

Copy the folder over the existing one and choose **Replace the files in the
destination**. That replaces every file the bundle carries and leaves everything
else alone — which is why the state list above matters: it is safe **because** the
bundle does not carry those files, not because the copy is clever.

The one code file this catches out is **`R\params.R`**: it lives in `R\`, so it is
replaced, and it is the one code file a maintainer is expected to edit. Keep a copy
beside your backups, then merge your values into the *new* file by hand — never
copy the old file over the new one, because a release may have added parameters the
rest of the engine now expects. Full procedure:
`docs/operational/updating.md` and `docs/operational/maintaining-the-engine.md` §2.

### Restart, and prove it

Double-click `RUN-ME.bat`. It does not re-install R or the packages, so it is
quick. Then, in order:

1. `offline\manifest.txt` — does `app_version` say what you just shipped?
2. Run the suite the way §8 describes. `failed: 0`, `errors: 0`, `skipped: 0`.
3. Convert **one statement you know reconciles**, and confirm it still does.
4. Open that conversion's `.json` and check `build.engine_version`. It is read
   from the `VERSION` file at run time and stamped into every run log, every JSON
   and the feed manifest, so this is the one check that proves "which build
   produced this figure?" is answerable on *this box*. If it says `unknown`, the
   `VERSION` file did not travel — fix that before anyone converts anything real.

`VERSION` and the newest `## ` heading in `CHANGELOG.md` must agree; a test pins
it. Bump both in the same change.

### Roll back

Keep the previous **bundle** — `StatementStudio-offline` as it came off the
internet PC, not a copy of the live server folder; the two look alike and only one
of them is safe to put back. Copy it over the app folder the same way an update
goes on, **Replace the files in the destination**. `config\`, `dictionaries\`,
`templates_user\`, `logs\`, `uploads\` and `feed\` are untouched in either
direction, because none of them is in the bundle. No internet is needed, and
nothing has to be uninstalled — the private R lives inside whichever folder you
are running.

Restoring a *snapshot of the live folder* instead reverts all of that live state
to the day the snapshot was taken, which throws away every template built and
every word taught since — and `RUN-ME.bat` then refreshes its
`%LOCALAPPDATA%\StatementStudio` copy from the stale files, taking the fallback
with it.

The one thing a rollback cannot undo is a **template** edited or promoted since
the update, because `templates\` *is* in the bundle. If that is what you are
rolling back, take the template out of `templates_user\` first, or you will
restore the old one over it.

**And it does not undo what already reached Qlik.** `feed\` is live state: the
rows the bad build published are still in `feed\transactions\`, and once Qlik has
reloaded they are on the dashboards. Feed files are keyed by the statement's
content hash, so re-converting a statement overwrites its row in place — that is
the fix. Anything that cannot be re-converted has to be withdrawn, by rating the
run *Wrong* (which calls `retract_feed()` in `R/feed.R`) or by hand on the two
files that hold it. The whole procedure, including what to tell the analysts:
[`docs/operational/rolling-back.md`](operational/rolling-back.md) §4.

---

## 10. When somebody says a figure is wrong

That is the day this document earns its keep, and it has its own procedure rather
than a paragraph here:
[`docs/operational/investigating-a-wrong-conversion.md`](operational/investigating-a-wrong-conversion.md)
— getting the run id out of Admin, finding the byte-identical original under
`uploads/`, reading `logs/runs/<run_id>.json`, reproducing the figure with
`run.R`, and telling a template bug from a bad scan.

Two things to know before you open it. The engine is deterministic, so a re-run of
the same file against the same template **on the same build** reproduces the
figure. Three things break that, and the run log names all three: the template was
edited (`template_sha256`), the app was updated (`engine_version`), or the figure
really did change. Check the first two before concluding the third — and note that
`engine_version` is stamped inside every output, so a re-run on a newer build can
never be *byte*-identical to the old one however right it is. Compare the figures.
And no figure is ever edited: the only fix is a template correction and a re-run,
which is what keeps the output reproducible and the provenance intact.
