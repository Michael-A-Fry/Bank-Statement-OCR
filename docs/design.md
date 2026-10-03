# Statement Studio — technical design

For a developer inheriting this cold. Read it in one sitting, then read
`docs/context/charter.md` (the rules a change is measured against),
`docs/context/auto-reading-spec.md` (the product owner's decisions for automatic
reading, and the contracts its parts were built to) and
`docs/context/architecture/build-contract.md` (the exhaustive schema: every
column, every flag token, every check, every function signature). This page is the
map, the reasoning, and the traps. It does not restate the contract.

It ends with three things: how to run it (§8), how to ship a change to it and put
it back (§9), and what to do when somebody says a figure is wrong (§10). The
step-by-step versions of those live in `docs/operational/`:
`maintaining-the-engine.md` (the suite on the server), `updating.md`,
`backup-and-restore.md` and `investigating-a-wrong-conversion.md`. This page
points at them rather than repeating them.

**Rewritten for 2.0.0, on 2026-10-03**, when templates were retired and every
statement started being read from its content. The ground rules (§1), the
invariants that survived (§5) and the add-a-check recipe (§8) were carried over
from the 1.x page, which was checked against the code at 1.3.0 and 1.4.0. At
2.0.0 `app.R` is about 5,400 lines and `R/` is 47 modules, about 23,000 lines
between them. Those counts move, so re-measure rather than quote them.
`docs/context/outstanding-work.md` is the live register of what is still open.
Line numbers are deliberately absent. Functions and files are named instead, and
every measured figure carries the date it was taken.

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
- **Deterministic.** Same input + same build + same learned state ⇒ the same bytes out. The xlsx
  writer pins `docProps/core.xml` and normalises zip timestamps for exactly this
  reason (`.deterministic_core`, `.normalize_zip_timestamps` in `R/outputs.R`).
- **Never throws at the front door.** `convert_statement()` wraps its whole body
  in `tryCatch`; any error becomes `status = "failed"` with an actionable message.
  *Whole* body is the load-bearing word. The `tryCatch` used to open below a short
  preamble, so `basename(path)` sat outside it and `convert_statement(1L)` threw
  before a single guard ran. Everything that touches `path` is now inside; what is
  left above it cannot throw for any input. If you add a line to that preamble,
  prove it — `convert_statement(1L)` must come back `failed`, not error.
- Two helpers are everywhere and are defined in `R/util.R`: `%||%` (null/empty
  coalesce) and `safe(expr, default)` (swallow an error, return a default).

---


## 2. The shape

```
   ui_content.R          ui_labels.R
   (About page)          (STATUS_PLAIN, OUTCOME_PLAIN, CHECK_PLAIN,
          \               READING_CHECK_PLAIN, DIAG_PLAIN, FLAG_PLAIN, ...)
           \                 /            copy only - no logic
            v               v
  browser <---- Shiny ---- app.R ------------------------> R/    (the engine)
                             |                              |
   reads / writes            |                              | reads / writes
   -------------             |                              --------------
   uploads/<id>/  <----------+                              templates/layouts/<bank>/<id>@v<n>.yaml
   logs/runs/     <----------+                                 (learned layouts, never edited)
   logs/feedback/ <----------+                              templates/layouts/.pending/
   logs/metadata/ <----------+                                 (a person's fix, held for an admin)
   logs/tracking/ <----------+  (counts only)               dictionaries/labels.yaml, lexicon.yaml
   requests/      <----------+                              dictionaries/nz_banks.yaml,
   logs/startup.log <--------+                                 nz_bank_branches.csv (bank identity)
                             |                              config/config.yaml
                             v
                     feed/transactions/   ---> Qlik folder connection
                     feed/review/         ---> the held-back table
                     feed/runs/           ---> one manifest row per conversion
```

**The arrow only points one way.** `R/` never reads `app.R`, `ui_labels.R` or
`ui_content.R`. The engine emits **codes** (`needs_review`, `balance_chain`,
`withheld:not_proven`), and the screen owns the **sentences**.
`tests/run_tests.R` sources only `R/`, so a UI symbol is simply not in scope for
the suite: an engine call into one errors as soon as a test reaches that line.
(`test-seams.R` deliberately `sys.source`s `ui_labels.R` into a private
environment of its own, so that reading both halves stays a special case.)

`run.R` is a thin CLI over the same engine and loads only `R/`. That is the
second proof that the engine does not need Shiny. It is also the tool you reach
for when somebody says a figure is wrong (§10).

```
Rscript run.R <file> [bank] [outdir]
```

Where the **words** are matters more than where the lines are. Measured in
2026-07 (1.x), about 45% of the words a user reads were written by the engine:
check details in `R/reconcile.R`, how-to-fix advice in `R/diagnose.R`, and now
the reader's reasons in `R/auto_read*.R` ("The balance does not add up at row 4
(page 1)"). Only the engine has the figures. **A copy audit that reads `app.R`
and the two `ui_` files misses about half of what a user reads.** The screens
review at 2.0.0 found several engine sentences that reach the screen raw, and
`app.R` rewords them (`docs/context/outstanding-work.md`).

---

## 3. The path a statement takes

Front door: **`convert_statement()`** in `R/convert.R`. It writes the run log
exactly once, after the outcome is known. Top to bottom:

| # | Call | File | What it produces |
|---|---|---|---|
| 1 | `read_input(path)` | `R/read_input.R` → `read_delimited` / the Excel reader / `read_pdf` (+ `ocr`, `ocr_preprocess`) | one `input` shape whatever the format: `kind, path, sha256, lines, table, pages, words, meta`. A scan is OCR'd here, one Tesseract run per page, time-limited |
| 2 | `extract_metadata(input)` | `R/extract_metadata.R` | period, opening/closing balances, account, page counts, read through `dictionaries/labels.yaml` with no wording hardcoded; is this one file holding several statements? |
| 3 | `bundle_segments(input, meta)` | `R/split.R` | the page ranges of each statement in a bundle, or NULL: one statement, or a count nothing independent confirms |
| 4 | `bank_identify(input)` then `bank_pick(identified, chosen, confirmed)` | `R/bank_identity.R` | which bank, how sure, and whether learning must wait for a person. Per statement, in a bundle |
| 5 | `layouts_load(dir, bank)` + `layouts_state_id(dir)` | `R/layouts.R` | that bank's learned layouts, and the hash of the learned state stamped on the output |
| 6 | `auto_read(input, layouts, bank, opts)` | `R/auto_read.R` (+ `_pdf`, `_prove`, `_tabular`) | the **reading**: `outcome`, `why`, `template`, `parsed`, `recon`, `checks`, `proof`, `columns`, `matched_layout` (§4) |
| 7 | `bundle_combine(readings, ranges, npages)` | `R/split.R` | for a bundle: one `parsed` with `statement_index`, outcome rolled up to the weakest statement |
| 8 | the status, the feed basis, the spot-check pick | `R/convert.R` | `status`, `outcome`, `reason`, `feed_basis`, `spot_check` (§4, "Status") |
| 9 | `build_diagnostics(status, ..., reading = )` | `R/diagnose.R` | where / why / how bad / **who fixes it** |
| 10 | `write_outputs(parsed, recon, outdir, base, formats, ...)` | `R/outputs.R` | 6-sheet xlsx, csv, json. The JSON's `build` block carries the stamp |
| 11 | `layout_learn` / `layout_correct` / `fix_hold` | `R/layouts.R`, `R/fixes.R` | what was learned, if anything (§4, "Learning") |
| 12 | `track_record(...)` | `R/tracking.R` | one line in `logs/tracking/`, codes and counts only |
| 13 | `field_coverage`, `capture_metadata` + `write_metadata_record()` | `R/coverage.R`, `R/metadata_capture.R` | populated / empty per field; the local-only corpus, never fed |
| 14 | `log_run(logdir, result)` | `R/convert.R` → `write_log_record` (`R/logging.R`) | one JSON file per run, scrubbed of quoted statement text and long numbers |

`R/feed.R`'s `write_feed()` is **not** in that list. It is called by the Convert
button in `app.R` only, the one place a person has seen the outcome. Training a
bank, Admin's bulk audit and the CLI scripts deliberately do not publish.

`convert_batch()` (`R/batch.R`) is a loop over `convert_statement()`, nothing
more, so a batch answer and a single-file answer for the same statement cannot
disagree. Each conversion runs in its own child R process (`R/jobs.R`), so a long
scan never freezes anyone else's page.

**The Convert table** (`R/identify.R`, `cv_plan` in `app.R`) shows every uploaded
file with its **bank**, pre-filled by `identify_file()` from the PDF's text layer
(or the export itself) through the same `bank_identify()`. A scan's first two
pages are OCR'd for it in a background job (`identify_scan`). **A bank the person
left alone is not passed to the conversion**: the conversion pre-fills it again
from the whole statement, so the table's quick guess can never override the full
reading. A bank the person changed is passed. The dropdowns are plain `<select>`s,
one event per change, recorded per row on the server, so a redraw can never lose
or revive a choice. **Convert again** re-reads only the rows whose bank changed.

---

## 4. Automatic reading: geometry proposes, arithmetic decides

The 1.x extension point was the template: a YAML file per bank layout, matched by
fingerprint phrases. It is retired. On the realistic dev set the shipped
templates read 0 of 128 PDFs, and a template drafted for each file read 32, three
of them "ok" but wrong (spec section 9.1). The cause was structural: the column
finder was all or nothing, it measured one page, and it took which column was
money out from heading words or position, never from the arithmetic.

### The reader (`R/auto_read*.R`)

Spec section 4 has the full pipeline. The shape of it:

1. **Type every token** (`auto_read_pdf.R`): date (with its possible formats),
   money (sign style, CR/DR, decimal point or comma), a number that is not money,
   a marker, or text. Formats are decided by voting over the whole document. A
   bare number is never a date.
2. **Lines and cells in the page's own type size**, then **one column model for
   the whole document**. Figures are grouped by their right edge across every
   page, and text and dates by their left edge. A column needs at least three
   aligned members, so one stray figure in a description cannot become a column.
   Boundaries go in white space that runs down every body row, so a boundary
   never cuts a word. No page is set aside.
3. **Roles by arithmetic** (`auto_read_prove.R`). Every assignment of roles
   (money out, money in, signed amount, balance, other), every sign convention
   and both orders (oldest or newest first) is tried. A reading is **proven** only
   when every balance step holds to the cent **and no other assignment does**.
   Without a running balance: opening + movements = closing, plus any printed
   totals. Heading words and description words (SALARY, EFTPOS) are votes, used
   only where nothing proves the roles, never as proof.
4. **The engine's own table reader assembles the rows.** The winning candidate is
   a template list in the 1.x schema (`table$columns`, and for a PDF
   `table$columns_by_page`), so `parse_pdf_table()` / `parse_statement()` handle
   continuation lines, split rows, summary lines, year-less dates and amounts
   filled from the balance exactly as before. Then **every figure is checked
   again on what the table reader actually produced** (`reader_agrees`).
5. **Hard checks, all or nothing** (the `checks` frame): every balance step holds
   and the chain continues across pages, opening and closing agree, every page
   with transaction-shaped lines gave rows, every dated line is used, dates are
   readable, in order and in the period, no sign is left ambiguous, no amount was
   derived, and the reading is unique.
6. **Bounded repair** in a fixed order (wider or narrower cells, no page shift,
   the bank's other layouts, re-OCR of failing rows on a scan). A repair is
   accepted only if it passes every check and is the only candidate that does.

The same prover reads CSV and Excel (`auto_read_tabular.R`), on a grid of cells
instead of word boxes: headings vote, and the arithmetic decides.

**Outcomes:** `proven` · `layout_match` (no running balance and no totals, but the
reading is a **proven** layout of this bank and nothing contradicts it) · `check`
(read, not proven, with the reason in a sentence) · `unread`. A figure worked out
from the balance always forces `check`. A heading that says one thing while the
arithmetic proves another (card and loan accounts run backwards) loses to the
arithmetic, and a note is recorded.

### Banks (`R/bank_identity.R`)

Evidence in order of trust: **the account holder's own account number**, looked
up in the Payments NZ branch register (bank + branch, not the bank code alone,
because 02 is BNZ and the Co-operative Bank) and checked with IRD's check digits;
then the legal name, website, 0800 number and SWIFT code; then masthead brand
words. Payee account numbers and bank names inside the transactions are ignored,
because ANZ, ASB, Westpac and TSB all appear as payees. **The account number is
parsed, looked up and dropped inside that file**: nothing returned, logged or
stored carries it, its branch, or a hash of it (a branch has about 10^7 bodies, so
a hash would be reversible).

`bank_pick(identified, chosen, confirmed)` combines that with the person's pick.
When the statement names a different bank from the pick with medium or high
confidence, **learning is blocked** until a person confirms which is right
(`bank_confirmed`). A statement with no balance and no totals is not converted on a
layout while its bank is in question.

### Learned layouts (`R/layouts.R`)

A layout is the tool's memory of one bank design. Its **signature** is kind,
roles in order, date format, money style, how often the balance is printed,
heading words, PDF producer and **relative** column positions. Absolute positions
are deliberately not part of its identity, so a shifted copy is the same layout.
`layout_match()` scores date format, heading words and relative positions; 0.70
matches. A layout is a **hypothesis, re-checked every time**. It supplies what a
page cannot prove (which way a card's signs run) and is the first candidate next
time, but the arithmetic still decides.

### Learning: what teaches, and what never does

| Event | What happens |
|---|---|
| A reading is **proven** | `layout_learn`: a new design becomes a **provisional** layout; a known one gains evidence. Proven after 3 statements (`LAYOUT_PROVEN_AFTER`) or an admin's Confirm. One file counts once per layout, so a bundle cannot promote a layout by itself. |
| A person's **role fix then proves** | `layout_correct`: learned at once, as a proven, corrected layout. |
| A role fix that does **not** prove, or a plain **confirm** | Applies to that file only. Held in `templates/layouts/.pending/` for an admin (`fixes_pending`, `fix_accept`, `fix_discard`). |
| A role fix that reads **different figures** from an already-proven reading | Not treated as proven. |
| Columns **drawn by hand** | Never learned: a layout does not remember positions as its identity, so boxes drawn for one file are that file's alone. |
| The **bank is in question** | Nothing is learned. A statement in a bundle that names another bank never teaches the picked bank's layouts, even after `bank_confirmed`. |
| An admin **retires, renames or confirms** | A new version, never an edit. |

### Stamping, so any output can be reproduced

A layout file is **never edited**: every change writes `<id>@v<n+1>.yaml` beside
it, and retiring keeps the file. `layouts_state_id()` hashes every layout file in
the store, and that hash goes on every output alongside the build:

`build = {engine_version, reader_version, layouts_state, layout, outcome, proof_kind, institution, bank_code, bank_confidence, kind}`

It is in the JSON download, in `logs/runs/<run_id>.json` and in the feed
manifest. **Same file + same `engine_version` + same `layouts_state` gives the
same bytes.** A reading proven by its own arithmetic does not depend on the
learned state at all. A layout match does, and the stamp says which layout and
version supplied it.

### Status, decided in `convert.R`

```
proven, layout_match                     -> ok            (feed basis: proven / layout_match)
check                                    -> needs_review  (with the reader's reason)
check, confirmed by a person             -> ok            (feed basis: person)
unread on every statement in the file    -> unsupported
the file itself could not be read        -> failed
```

Three holds sit on top: a proven scan with poor OCR confidence still goes to a
person; a statement with no balance and no totals is not converted on a layout
while its bank is in question; and a confirm is **refused** when a balance,
opening/closing or printed-totals check contradicts the reading, or when a fix sent
with it did not apply.

### Trust, still computed in `.reconcile_trust()`

The reconciliation KPIs (`R/reconcile.R`) still run on every reading. They fill
the Checks table, the `trust` level and the workbook's `Checks` sheet. **They no
longer decide the status**: the reader's checks do. `high` = every applicable KPI
passed · `medium` = one or more could not run · `low` = any failed, with the 1.x
caps. A PDF or Excel statement still can never reach `high`, because
`no_unparsed_rows` has no independent physical-line count to check against. That
is why the screen now leads with the outcome and keeps trust as a detail.

---

## 5. Invariants that must never break

| # | Invariant | Guarded? | What enforces it |
|---|---|---|---|
| 1 | The engine never reads a front-end file | structural | `tests/run_tests.R` sources only `R/`, so a UI symbol is simply not in scope. `run.R` runs the whole pipeline with no Shiny at all |
| 2 | Every code the engine can emit has wording, and there is no dead wording | **yes** | `test-seams.R`: `expect_setequal` **both ways** for `CHECK_PLAIN` vs `reconcile()`'s builder list, `DIAG_PLAIN` vs **`.DIAG_FIX_OWNER`**, **`INFORMATIONAL_CHECKS`** vs the informational builders, plus `STATUS_PLAIN`, `RESULT_PLAIN`, `COVERAGE_PLAIN` and the row flags the reader and the table reader emit. `test-batch.R` holds `READING_CHECK_PLAIN` to the reader's checks and to tracking's allowlist |
| 3 | A check label claims only what its check proves | **yes** | `test-seams.R`, "no check label claims more than its KPI proves" |
| 4 | Nothing is automatic unless the arithmetic proved it, uniquely | **yes**, and **measured** | `test-auto-read.R` (unit and metamorphic: a shifted column, a stray figure, a dropped page or a deleted row gives the same correct output or a flag), `test-convert.R`; and outside the suite, `tools/synth/score_auto.R` / `score_convert.R`, where **AUTO_WRONG must be 0** on every set |
| 5 | One run, one audit record, never overwritten | **yes** | `write_log_record()` (`R/logging.R`) suffixes a clashing id with `~2`; `test-run-log-identity.R` |
| 6 | No account number reaches the run log, tracking, a layout file or the metadata corpus | **yes** | `test-run-log-identity.R`, `test-tracking.R` (an allowlist of typed fields: an id refuses five or more digits in a row), `test-layouts.R`, `test-metadata_capture.R`, `test-bank-identity.R` |
| 7 | Descriptions verbatim | **yes** | `clean_description()` is `trimws` and nothing else; the golden tests carry apostrophes and ampersands |
| 8 | Byte-reproducible outputs | **yes** | `test-outputs.R`, "xlsx / csv / json are byte-reproducible across runs" |
| 9 | A learned layout file is never edited; every change is a new version | **yes** | `test-layouts.R`; five conversions racing to learn one layout give exactly one layout (measured 2026-10-03) |
| 10 | Only a proven reading teaches; a person's unproven word is held, never learned | **yes** | `test-convert.R`, `test-layouts.R` |
| 11 | The feed gate is machine-only, and only `ok` with basis `proven`, `layout_match` or `person` reaches `feed/transactions/` | **yes** | `.feed_gate()` (`R/feed.R`); `test-feed.R` covers accept, withhold, re-convert flip, atomic write, content-hash keying |
| 12 | A feed write failure is never recorded as a clean accept | **yes** | `.atomic_write_csv()` captures warnings as well as errors; `test-feed.R` |
| 13 | The `R/` module map in the build contract matches `R/` in both directions | **yes** | `test-deployment-docs.R` |
| 14 | Every path into this tree that a document or a comment names, resolves | **yes** | `test-deployment-docs.R` and `test-docs-truth.R` |
| 15 | No documentation page is orphaned from its index | **yes** | `test-deployment-docs.R`, "no docs page is orphaned from its index" |
| 16 | A skipped test is a failure | **yes** | `tests/run_tests.R` exits 1 on any skip unless `BSO_ALLOW_SKIPS=1`. This is why the exit code, not the summary line, is the pass condition |
| 17 | No non-ASCII **byte** in any R source the app or the engine loads — string literals and comments included, not only names | **yes** | Two byte scans, each with a companion test that feeds it a known-bad file: `test-labels.R` (all of `R/`, `run.R`, `tests/run_tests.R`) and `test-app-adoption.R` (the three UI files). `test-app-ui.R` reads the *parse tokens* for non-ASCII names, the worse failure. **`tests/testthat/` is deliberately outside them**: some test files carry a non-ASCII byte on purpose, as the input that proves a scan works |
| 18 | The screen's colours come from the design system, not from the file they are typed in | **no** | **Unguarded.** The only test that looks at this forbids CSS style *tags*, and inline style *attributes* still carry a private palette. Open as `N46` |

Rows marked **structural** need no assertion because the code cannot express the
violation. The one marked **no** is the point of the column: a reader scanning
this table for "what will the suite catch me on" must not get a false negative
from an invariant that is filed somewhere else.

---

## 6. Deliberate decisions a newcomer would otherwise undo

**A person's confirm never teaches.** *This is right* converts that file, on the
person's word (`feed_basis = "person"`), and the reading is held for an admin. It
is not learned. The spec says "an unprovable fix applies to that file only until
an admin confirms it", and a confirm is the least provable fix there is. A single
mistaken click would otherwise convert every later statement of that design
without anyone looking.

**A confirm the arithmetic contradicts is refused.** A reading whose balance chain,
opening/closing or printed totals fail is not "unproven", it is wrong, and no click
can make it right. The screen says why and offers Please check's dropdowns.

**An unsplittable bundle converts only if every row proves.** When a file looks
like several statements but the count cannot be confirmed, it is read whole. That
reading is accepted only when the arithmetic proves every row. Consecutive
statements of one account chain cleanly, and statements of different accounts
break the chain, so the proof itself separates the safe case from the unsafe one.
Checked by forcing whole-file reads (2026-10-03). Otherwise the file goes to a
person with the *several statements in one file* diagnostic.

**Splitting is gated on an INDEPENDENT count, not on proof.** `bundle_segments()`
refuses unless `.count_agrees()` finds a *different* structural count (distinct
periods, or repeated opening/closing blocks) that agrees. Each statement's own
proof is not a substitute: a running balance is continuous across **any** cut, so
a wrongly placed boundary still adds up inside each piece.

**A fix in a bundle goes to one statement.** `overrides$statement` names it.
Without that, a fix goes only to the statements that did not convert on their
own. Before this rule, fixing statement 2 broke statements 1 and 3, which had been
proven.

**The account number never leaves `R/bank_identity.R`.** Not even a hash. The
run log keeps `source_file` as the audit trail (a file named after an account
keeps it there), and the analyst's own JSON keeps `metadata$accounts`, because
that is the statement's content, delivered to the person who asked for it.

**A "bank" that holds a long digit run is not used.** It would name the layout
folder, the layout ids and the run log. The bank is taken from the statement
instead, and a message says so. The app's *Another bank* box refuses one too.

**Spot checks are picked from the file's own hash.** The same statement is always
picked or never picked (`.spot_pick`), so re-converting cannot dodge one. A
statement matched to a layout with no arithmetic of its own is picked at twice the
rate: those are the readings the arithmetic cannot reach. Off by default, by the
product owner's decision.

**`.unique_names()` in `app.R` cannot be replaced by per-file subfolders.** The
clash is in the **output** name, not the input path: `convert_statement()` writes
to `outdir` under the file's base name, and `convert_batch()` takes ONE `outdir`
for the whole case. Since 2.0.0 the inputs themselves live in an `in/` subfolder,
because a CSV input and its CSV output share a name, and the output used to
overwrite the input. Please check would then have re-read its own output.

**A hidden Shiny output keeps `.recalculating` for ever, and it is not a bug.** An
output inside a `conditionalPanel` that is false is suspended, so the class never
clears. **"Wait until the page is quiet" is not a usable readiness check.**
Anything driving this UI has to count only *visible* busy elements:

```js
Array.from(document.querySelectorAll('.recalculating, .shiny-busy'))
  .filter(e => e.offsetParent !== null).length
```

**The feed reads `result$feed_rows`, not the output CSV.** `utils::read.csv`'s type
inference silently mangled a leading-zero code (`000001` became `1`). For the same
reason the reader treats a leading-zero code column (`007`) as text, not money.

**The feed's `template_id` / `template_origin` columns kept their names.** A
renamed column is a new field to Qlik and splits the table. They carry the layout
and the gate's basis now.

**A re-read's settings are looked up with `[[...]]`, never `$`.** R's `$`
partially matches names on a list, so `src$bank` found `src$bank_confirmed` on a
re-read with no bank, and a layout was learned under a bank called `FALSE`.

---

## 7. Where the bodies are buried

**Most of `app.R` is one `server <- function(input, output, session)` scope**,
and that, not its length, is what makes it hard. Every name is visible to every
other. The 2.0.0 rewrite took it from 7,789 lines to about 5,300, and removed no
reactive-graph complexity in doing so.

**The tests read `app.R` as text.** `test-app-ui.R`, `test-app-adoption.R`,
`test-admin_auth.R`, `test-seams.R`, `test-batch.R`, `test-run-log-identity.R` and
`test-docs-truth.R` all do. Before splitting the file, change the readers to
concatenate `app.R` plus any `server/*.R` **first, in its own commit**. The plan is
in `docs/context/findings-register.md` under "The app.R split".
`tools/ui/check.mjs` drives the real screens in a browser and is the only proof
they work.

**`R/auto_read.R`, `R/auto_read_pdf.R` and `R/parse_pdf_table.R` are the long,
hard modules.** Read each header comment before touching anything with an `x_min`
in it. `parse_pdf_table()` still holds the band frame: every stored PDF position
lives in one coordinate space (`pdf_band_frame()`), and a layout file stores its
`ref_width` / `ref_height` for exactly that reason.

**`R/inspect.R` and `R/row_coverage.R` have no caller in the app since 2.0.0.**
They drew the old "See it on the page" view and are kept only by their tests.
Either give them a screen again or retire them with their tests; do not grow them.

**`app.R` still carries a private palette inlined in `style=` attributes**
(invariant 18, open as `N46`).

**The reader's sentences reach the screen.** Several engine strings ("sum(amount)",
"1 discontinuity(ies)", "(medium confidence)") are reworded in `app.R` /
`ui_labels.R` rather than at the source. Fixing them at the source would let those
overrides go; the list is in `docs/context/outstanding-work.md`.

---

## 8. Working on it safely

### Run it

```
Rscript scripts/run_app.R                # the app, on config's port, host 0.0.0.0
Rscript run.R <file> [bank] [outdir]     # the whole engine, no Shiny
Rscript tests/run_tests.R                # the suite; from the repo root, several minutes
```

`scripts/run_app.R`, not a bare `runApp()`, is the way in. It self-locates, reads
the port and the upload ceiling from `config/config.yaml`, and says on the console
if the settings file did not parse or the admin password is still the shipped
placeholder.

On the deployment box none of those is the command you type. **`RUN-ME.bat` is
what the Windows server double-clicks**, and it runs `scripts/run_app.R` through
the app's **own private R** under `R-runtime\`. The suite has to be run the same
way. Both exact commands are in `docs/operational/maintaining-the-engine.md` §1.

The suite sources every file in `R/`, sets `ENGINE_ROOT`, runs
`testthat::test_dir`, and **exits 1 on any skip**. The last measured baseline
lives in one place, `docs/operational/maintaining-the-engine.md`.

### To add a bank

Nothing. Convert its statements, or train it on Admin -> Banks. If a bank's
statements do not prove, the reader is missing something general: find it on the
dev set (`tools/synth/`), fix it in `R/auto_read*.R`, and measure on every set
before and after. **AUTO_WRONG must stay 0.** If you find yourself writing
per-bank R code, stop. Bank *identity* is data: a new bank's names, website,
phone and brand words go in `dictionaries/nz_banks.yaml`, and its branches come
from the register in `dictionaries/nz_bank_branches.csv`.

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

### To add a reader check

The reader's hard checks (`reading$checks`) decide the outcome, so a new one is a
bigger change than a KPI. Do it in this order:

1. **`R/auto_read*.R`**: the check itself, added to the reading's `checks` with
   `ok` and a one-sentence `why` that names the row and page.
2. **`ui_labels.R`**: a `READING_CHECK_PLAIN` entry, worded for what the check
   establishes when it holds. `test-batch.R` fails a check with no wording.
3. **`R/tracking.R`**: the name in `TRACK_CHECKS`. Until it is there, tracking
   drops the check with a warning and the Admin counts never see it.
   `test-tracking.R` holds the list to the reader's source.
4. **Measure** on every test set before and after (§8, *To measure*). A check
   that moves AUTO_WRONG off zero, or turns a dozen `auto_right` into `check`
   without finding anything wrong, is not ready.

### To change a threshold

`R/params.R` is the place for tuning numbers that the table reader and OCR use;
its catalogue is `docs/context/engine-parameters.md`. The learned-layout
thresholds are constants at the top of `R/layouts.R` (`LAYOUT_PROVEN_AFTER`,
`LAYOUT_MATCH_MIN`, `LAYOUT_X_SCALE`), each with the measurement that set it.

### To measure

The suite answers "has it changed?". It cannot answer "how often is it right?",
because a golden file is the reader's own output. The answer key has to come from
somewhere else: the synthetic test sets in `tools/synth/`, each statement drawn
with the rows it was drawn from (`tools/synth/README.md`).

```
Rscript tools/synth/score_auto.R    <dir> --mode cold|trained    # the reader alone
Rscript tools/synth/score_convert.R <dir> --mode cold|trained    # convert end to end, figures read back from the CSV
```

Every statement lands in exactly one cell: `auto_right`, **`AUTO_WRONG` (must be
0)**, `check_right`, `check_wrong`, `unread`. The definitions were fixed **before
the reader was built**. Do not change what counts as right to make a number move;
change the reader. Two sets are **held back** (the realistic holdout and the
green-flag set) and are scored once, independently, at the end. Never tune
against them.

### To change what reaches Qlik

You do not. The gate (`R/feed.R`) takes `ok` with basis `proven`, `layout_match`
or `person`, and it has no settings. `config/config.yaml` → `feed:` holds where
it writes and whether withheld runs go to `review/`.

### Before you delete something that looks redundant

Check `docs/context/findings-register.md`. Several things in this codebase look
like duplication and are not: the two period mechanisms, the two dictionaries,
the separate `feed/review` folder, the reconciliation KPIs beside the reader's
checks. The register records the measurement that decided each one.

---

## 9. Shipping a change, and putting it back

The target is an air-gapped Windows box with no internet, no database, no version
control and no admin rights to spare. There is no deploy pipeline. There is a
folder you carry across.

### The install is one folder, and half of it is live state

```
StatementStudio-offline\
  R\  app.R  ui_labels.R  ui_content.R  run.R  scripts\  www\  RUN-ME.bat  VERSION
  tests\  samples\  docs\  templates\README.md                               <- product
  dictionaries\nz_banks.yaml  nz_bank_branches.csv                          <- product (reference data)
  config\config.yaml                    <- LIVE: port, admin password, paths, spot-check rate
  dictionaries\labels.yaml  lexicon.yaml <- LIVE: every wording Admin has taught it
  templates\layouts\                    <- LIVE: every layout learned on this server
  logs\runs\  logs\feedback\  logs\metadata\  logs\tracking\  <- LIVE: the audit trail and the counts
  uploads\<id>\                         <- LIVE: real client statements
  requests\                             <- LIVE: format requests
  feed\                                 <- LIVE: what Qlik reads
  R-runtime\  R-lib\                    <- the app's own private R, installed on the box
```

**Everything marked LIVE is server state that exists nowhere else**, and an update
must not touch any of it. `templates\layouts\`, `dictionaries\` (the two taught
files), `logs\metadata\` and `logs\tracking\` cannot be rebuilt from the repo,
which is why `docs/operational/backup-and-restore.md` is the first step of the
update procedure, not the last.

### Build

On a PC **with** internet, double-click `make-bundle.bat`, which runs
`scripts/bundle-offline.R`. It assembles `StatementStudio-offline\`: the app files,
plus `offline\` holding the R installer, every CRAN package, the Poppler and
Tesseract binaries, and `offline\manifest.txt`. Check its last lines for
`MISSING` before you carry it anywhere.

Two omissions are deliberate and pinned by tests: `config/config.yaml` and the
taught dictionaries ship only as `.example` files, so copying a fresh bundle over
a live server cannot revert its settings or its taught words. **At 2.0.0 the
script is behind**: it renames `nz_banks.yaml` to an example file and leaves
`nz_bank_branches.csv` out (both are reference data and must ship as they are),
and it would carry a build PC's own `templates/layouts/`. Until it is fixed, carry
those two files by hand and check the package has no `templates\layouts\`
(`docs/operational/release-2.0.0-hand-carry.md`).

### Copy, and what the copy replaces

Copy the folder over the existing one and choose **Replace the files in the
destination**. That replaces every file the bundle carries and leaves everything
else alone. It is safe **because** the bundle does not carry the live files.

**It never deletes a file.** The app loads every `R/*.R` it finds, so an engine
file a release removed stays loaded, after the new ones, until someone deletes
it. 2.0.0 is the first release that removed engine files, and its hand-carry list
names them.

**`R\params.R`** lives in `R\`, so it is replaced, and it is the one code file a
maintainer is expected to edit. Keep a copy beside your backups and merge your
values into the *new* file by hand (`docs/operational/maintaining-the-engine.md`
§2).

### Restart, and prove it

Double-click `RUN-ME.bat`. Then, in order:

1. `offline\manifest.txt`: does `app_version` say what you just shipped?
2. Run the suite the way §8 describes: `failed: 0`, `errors: 0`, `skipped: 0`.
3. Convert `samples\raw\tutorial\sample_everyday_statement.pdf`. It must be
   **Proven**, 12 transactions.
4. Open that conversion's `.json` and check `build.engine_version`. If it says
   `unknown`, the `VERSION` file did not travel. Fix that before anyone converts
   anything real.

`VERSION` and the newest `## ` heading in `CHANGELOG.md` must agree; a test pins
it. Bump both in the same change.

### Roll back

Keep the previous **bundle** (`StatementStudio-offline` as it came off the internet
PC, not a copy of the live server folder). Copy it over the app folder the same
way. `config\`, `dictionaries\`, `templates\layouts\`, `logs\`, `uploads\` and
`feed\` are untouched in either direction. If the newer version **added** engine
files, delete them, for the same reason as above. Rolling 2.0.0 back to 1.x also
needs the pre-2.0.0 backup of `templates\statements_user\`
(`docs/operational/rolling-back.md`).

**A rollback does not undo what already reached Qlik.** Re-converting a statement
overwrites its feed row in place, and that is the fix. Anything that cannot be
re-converted has to be withdrawn, by rating the run *Wrong* (which calls
`retract_feed()`) or by hand. The whole procedure:
[`docs/operational/rolling-back.md`](operational/rolling-back.md) §4.

---

## 10. When somebody says a figure is wrong

That is the day this document earns its keep, and it has its own procedure:
[`docs/operational/investigating-a-wrong-conversion.md`](operational/investigating-a-wrong-conversion.md).
It covers getting the run id out of Admin, finding the byte-identical original
under `uploads/`, reading `logs/runs/<run_id>.json`, reproducing the figure with
`run.R`, and telling a learned layout from a reading fault from a bad scan.

Two things to know before you open it. The engine is deterministic, so a re-run
of the same file on the same build against the same learned state reproduces the
figure. The run log names what can break that: the build (`engine_version`), and
the learned state (`layouts_state`, plus the `layout` it matched). A reading
**proven** by its own arithmetic does not depend on the learned state. A **layout
match** does, and retiring a bad layout is the fix for it. A figure that was
**proven and is wrong** is the most serious finding this tool can have: keep
everything, and record it in `docs/context/findings-register.md` the same day.
No figure is ever edited. The only fix is a corrected reading and a re-run, which
keeps the output reproducible and the provenance intact.
