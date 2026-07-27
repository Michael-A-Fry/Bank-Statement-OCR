# Statement Studio — technical design

For a developer inheriting this cold. Read it in one sitting, then read
`docs/context/charter.md` (the rules a change is measured against) and
`docs/context/architecture/build-contract.md` (the exhaustive schema: every
column, every flag token, every KPI, every function signature). This page is the
map, the reasoning, and the traps. It does not restate the contract.

Everything below was checked against the code as it stands at `VERSION` 1.2.0.
Line numbers are deliberately absent for `app.R` — it moves. Functions and files
are named instead.

---

## 1. Ground rules

- **Pure R.** Base R plus `yaml`, `jsonlite`, `openxlsx`, `readxl`, `pdftools`;
  `tesseract` + `magick` only on the scanned-PDF path; `testthat` for the suite.
  No Python, no `reticulate`, no ML, no network.
- **Air-gapped Windows target, C (non-UTF-8) locale.** A non-ASCII character
  inside a string literal is fine. A non-ASCII character in an **R name** is not:
  `c("✓ Correct" = "correct")` becomes a symbol at parse time and mangles to
  `'<U+2713>'` on a C-locale host. The fix already in the code is
  `choiceNames = list(...)` / `choiceValues = list(...)` — literals, never names.
- **Deterministic.** Same input + same template ⇒ the same bytes out. The xlsx
  writer pins `docProps/core.xml` and normalises zip timestamps for exactly this
  reason (`.deterministic_core`, `.normalize_zip_timestamps` in `R/outputs.R`).
- **Never throws at the front door.** `convert_statement()` wraps its whole body
  in `tryCatch`; any error becomes `status = "failed"` with an actionable message.
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
   requests/      <----------+                              dictionaries/      (labels, lexicon)
                             |                              config/config.yaml
                             v
                     feed/transactions/   ---> Qlik folder connection
                     feed/review/         ---> the held-back table
                     feed/runs/           ---> one manifest row per conversion
```

**The arrow only points one way.** `R/` never reads `app.R`, `ui_labels.R` or
`ui_content.R`. Verified:
`grep -rn "CHECK_PLAIN\|STATUS_PLAIN\|DIAG_PLAIN\|COVERAGE_PLAIN\|FEED_PLAIN\|RESULT_PLAIN\|ui_labels\|ui_content" R/`
returns five hits, all inside comments. The engine emits **codes**
(`needs_review`, `balance_reconciliation`, `withheld:not_proven`); the screen owns
**sentences**. `tests/run_tests.R` sources only `R/`, so a UI symbol is simply not
in scope for the suite: an engine call into one errors the moment a test walks
that line. (`test-seams.R` deliberately `sys.source`s `ui_labels.R` into a private
environment of its own, precisely so that reading both halves stays a special
case.)

`run.R` is a thin CLI over the same engine and loads only `R/`. That is the
second proof that the engine does not need Shiny.

Sizes, for orientation: `R/` is 46 modules / ~11,400 lines. `app.R` is ~4,900
lines. `ui_labels.R` is 257 lines and `ui_content.R` 156.

Where the **words** are matters more than where the lines are. Measured by an AST
walk over every string literal (2026-07-27): ~11,300 user-facing words, of which
`app.R` holds ~49%, `ui_content.R` + `ui_labels.R` ~16%, and **`R/` ~36%** —
mostly KPI details in `R/reconcile.R` and how-to-fix advice in `R/diagnose.R`,
which the engine writes because only it has the figures. A copy audit that reads
`app.R` and the two `ui_` files still misses more than a third of what a user
reads.

---

## 3. The path a statement takes

Front door: **`convert_document(path, ...)`** in `R/forms.R`. It runs the
statement pipeline, and only if that returns `unsupported` **and** no template
was forced does it try the labelled-value (form) pipeline. It writes the run log
exactly once, after the final outcome is known.

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

**Consequence a newcomer must know: a PDF or Excel statement can never reach
`high`.** `no_unparsed_rows` needs an independent physical source-line count,
which only the delimited reader has (`R/parse.R` and `R/parse_pdf_table.R` both
set `source_line_count = NA_integer_`), so it returns `na`, so `n_na > 0`, so the
ceiling is `medium`. Measured: a clean 11-page ANZ PDF, 311 rows, balance
reconciling, comes out `medium (92)`. That is the healthy ceiling for the format
the charter calls dominant. Do not "fix" it by making the check pass.

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

| # | Invariant | What enforces it |
|---|---|---|
| 1 | The engine never reads a front-end file | `tests/run_tests.R` sources only `R/`, so a UI symbol is out of scope for every engine test; `run.R` runs the whole pipeline with no Shiny at all. Today the belt-and-braces guard is a grep, not an assertion — worth adding one |
| 2 | Every code the engine can emit has wording, and there is no dead wording | `tests/testthat/test-seams.R` — `expect_setequal` **both ways** for `CHECK_PLAIN` vs `reconcile()`'s builder list, `DIAG_PLAIN` vs `.DIAG_FIX_OWNER`, plus `STATUS_PLAIN`, `RESULT_PLAIN`, `COVERAGE_PLAIN` |
| 3 | A check label claims only what its check proves | `test-seams.R`, "no check label claims more than its KPI proves" — it forbids "every row" on `no_unparsed_rows`, "matches" on `transaction_count`, "honoured" on `redaction_summary` |
| 4 | "could not be proved" and "not on this statement" must read differently | `test-seams.R` — `RESULT_PLAIN[["na"]]` must not equal or contain `COVERAGE_PLAIN[["unmapped"]]`. They render inside the same disclosure |
| 5 | One run, one audit record, never overwritten | `write_log_record()` (`R/logging.R`) suffixes a clashing id with `~2`; `test-run-log-identity.R`, "two runs with the same run_id leave TWO audit records" |
| 6 | Nothing under a redaction is read, and a scan that could not finish fails the run | `R/detect_redaction.R` + the `read_pdf` guard + `.kpi_redaction_scan()` returning `fail`; `test-detect_redaction.R`, `test-redaction_delimited.R` |
| 7 | Descriptions verbatim | `clean_description()` is `trimws` and nothing else; the golden tests carry apostrophes and ampersands |
| 8 | Byte-reproducible outputs | `test-outputs.R`, "xlsx / csv / json are byte-reproducible across runs" |
| 9 | Every stored PDF band lives in ONE coordinate space | `pdf_band_frame()` / `pdf_band_frame_scale()` (`R/parse_pdf_table.R`) are the only converters; `test-row_coverage.R`, "row_coverage.R keeps no private copy of the band frame" |
| 10 | The reader, the X-ray and the coverage report share ONE keep rule | `pdf_keep_row()` is called by `parse_pdf_table()` and by `R/inspect.R`; `test-inspect.R` asserts the counts cannot diverge |
| 11 | The feed gate is machine-only, and nothing but a clean run from an allowed origin reaches `feed/transactions/` | `.feed_gate()` (`R/feed.R`) takes no human input; `test-feed.R` (20 blocks) covers accept, withhold, re-convert flip, atomic write, NA trust, content-hash keying |
| 12 | A feed write failure is never recorded as a clean accept | `.atomic_write_csv()` captures warnings as well as errors (a read-only share warns and returns normally); `test-feed.R`, "a failed feed write is reported" |
| 13 | The `R/` module map in the build contract matches `R/` in both directions | `test-deployment-docs.R`, "build-contract.md's module map matches the modules that exist" |
| 14 | Every internal doc link resolves | `test-deployment-docs.R`, "every internal link in the docs resolves to a file that exists" — it walks `README.md`, `CHANGELOG.md` and every `.md` under `docs/` |
| 15 | No page under `docs/operational/` or `docs/context/` is orphaned from its index | `test-deployment-docs.R`, "no operational or context page is orphaned" |
| 16 | A skipped test is a failure | `tests/run_tests.R` exits 1 on any skip unless `BSO_ALLOW_SKIPS=1`. ~100 skip guards can dark the whole PDF/OCR/Excel surface while the board stays green |
| 17 | No non-ASCII character in an R **name** | **Nothing automated.** One comment in `app.R` and this page. `app.R` carries raw UTF-8 bytes on ~46 lines, all inside string values or literal lists; `ui_labels.R` and `ui_content.R` carry none. Treat this as an unguarded invariant |

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
a boundary. Measured working: `anz_multiple.pdf` splits into 6 statements, 1,253
rows, each reconciled on its own anchors, trust rolled up to the weakest segment.

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

**`app.R` is ~4,900 lines and ~4,000 of them are one `server <- function(input,
output, session)` scope.** Counted on 2026-07-27 (these drift by the day): 134
`output$` assignments, 70 `observeEvent`, 33 `reactive()`, 31 `reactiveVal` and
132 distinct `input$` ids, all in one lexical environment where every name is
visible to every other. Moving the CSS out to `www/app.css` made the file 250
lines shorter and removed **zero** reactive-graph complexity — CSS was the
cheapest 250 lines in it to lose.

**The blocker on splitting it is the test suite, and it has a two-line fix.**
`tests/testthat/test-app-ui.R` reads `app.R` as **text** — the suite never sources
it, because it needs a live Shiny session. `.ui_src()` reads the one file (63
call sites across 70 `test_that` blocks); `.ui_fun()` lifts a function by regex on
`^\s*name <- function` and parses forward until it parses (10 lifts); `.ui_block()`
takes a **fixed n-line window** after a pattern match (39 uses, hand-tuned per
test). `test-app-adoption.R` (11 blocks) and `test-admin_auth.R` (10 blocks) read
it the same way, and `test-seams.R` (5 places), `test-detect.R` and `test-batch.R`
read it too. Change `.ui_src()` to concatenate `app.R` plus `server/*.R` **first, in
its own commit**, then split with `source(..., local = TRUE)` inside `server()` —
not Shiny modules, which would mean rewriting every input id. The full plan, with
the four proposed files and their line counts, is in
`docs/context/findings-register.md` under "The app.R split".

**`R/parse_pdf_table.R` is 1,173 lines** and is the one module that is genuinely
hard. Read its band-frame header comment before touching anything with an `x_min`
in it. The rest of `R/` is single-concern and small.

**`app.R` still carries 37 distinct hex colours across 87 uses and 125 inline
`style=` attributes**, several of them near-misses for the token they should use
(`#b00020` x19 against `--bad:#b3261e`). Nothing looks wrong, which is why it will
not self-correct. The test that appears to guard this forbids style *tags* while
125 style *attributes* sit untouched — it passes and proves nothing. Open as N46.

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

```
Rscript tests/run_tests.R          # from the repo root; several minutes
```

It sources every file in `R/`, sets `ENGINE_ROOT`, runs `testthat::test_dir`, and
**exits 1 on any skip**. The last measured baseline lives in one place,
`docs/operational/maintaining-the-engine.md` — not in this file, so a stale number
here cannot masquerade as a regression signal.

**To add a bank:** write a YAML template (in the app, or by hand), add a fixture
and a golden test. `tests/HOWTO-add-template-test.md` is the recipe. No R change.
If you find yourself editing `R/`, stop and re-read §4.

**To add a check:** write a `.kpi_<name>()` builder in `R/reconcile.R`, add one
line to the list in `reconcile()`, add a `CHECK_PLAIN` entry in `ui_labels.R`, and
— if it can `fail` — a how-to-fix entry in `build_diagnostics()`. Miss the third
and `test-seams.R` fails the suite rather than showing a reviewer a raw code.

**To change a threshold:** `R/params.R`, which is the only place a tuning number
is allowed to live. Catalogue in `docs/context/engine-parameters.md`.

**To change what reaches Qlik:** `config/config.yaml` → `feed:`. Never the gate.

**Before you delete something that looks redundant:** check
`docs/context/findings-register.md`. Several things in this codebase look like
duplication and are not — the two period mechanisms, the two dictionaries, the
separate `feed/review` folder — and the register records the measurement that
decided each one.
