# Changelog

The version record. Newest release first.

`VERSION` is stamped into every run log, every JSON output and the feed manifest,
so the heading below has to match it: that is what makes "which build produced
this figure?" answerable after the fact.

This file is deliberately terse. Each entry says what changed and, where a wrong
figure was possible, what stopped it. The evidence for every line is in
[`docs/context/findings-register.md`](docs/context/findings-register.md), keyed by
finding id.

---

## 1.18.0

**One table: the files, their templates, and — once converted — their results.**

Asked: "Should it output the results in the same table? And allow you to click
through to the individual results and/or download all?" It already did both, in a
*second* table listing the same files again under the first. Now there is one.

- After Convert the same rows carry the **Result** (verdict, rows, confidence) and
  **What to check**, worst first. **Click a row** to open that file's full result
  underneath; the open row is highlighted. **Download everything** (one zip) sits
  above the table with the one-line tally.
- **Convert again converts only what would come out differently.** Change a row and
  it is marked *Changed*; the button reads *Convert 1 changed file*, converts that
  one, and merges its result into its own row — the rest keep theirs. A row also
  counts as changed when a newly added template now recognises it or its template
  was edited (each row's reading is its template plus that template's content
  hash). Nothing changed: *Convert all N again*. This replaces ticking rows.
- **A template saved, hidden or deleted re-checks the files**, so a row never shows
  a guess detection no longer makes. (Fixed: the 1.17.0 table kept its first
  guesses until the files were chosen again.)
- **Choices are kept on the server**, per row. The dropdowns are plain selects, so a
  redraw — a row opened, a re-check — can never lose a choice or bring back one
  from the previous upload.
- **New files replace what the page is about**: choosing them clears the last case's
  results and the result open under them.
- Removed: the DataTables results table, its fold, its tick-boxes, search and paging
  (fifty files at most, worst first). The single-file result's link now asks
  "Something in the wrong column?" — the wrong *bank* is the table's job.
- Dropdown labels drop the word "statement" ("ANZ everyday"), so they fit.

## 1.17.0

**Every upload gets a table: each file, what it is, and the template it will be read
with — already filled in, and changeable before anything converts.**

Asked for in these words: "we NEED a backup to be able to specify that isn't a tiny
little click 'did it do it wrong' … pre fill a table with the upload, its type, and
its guessed template with easy dropdown to change it. Same thing for single
statement."

- **One row per file**, the moment the files are chosen: name, kind (*PDF · 12
  pages*, *Scanned PDF*, *CSV*, *Excel*), a plain dropdown set to the template the
  tool will use, and a chip — *Recognised*, *Close call*, *Two fit*, *Not
  recognised*, *Scanned*, *Can't be read*, or *Your choice* once changed. The
  dropdown offers only templates that can read that kind of file, grouped by bank.
- **The guess is the conversion's own answer, not an approximation.** Same
  detector, same page text, same template set, same refusals. Measured: 0
  differences between the table and the template the conversion used, across 54
  sample files and 43 corpus cases.
- **A row left alone is detected; a row changed is forced — per file.** Leaving a
  row keeps every check detection carries (a whisker-thin win is still held for
  review). A case folder from four banks can have four different templates.
- **Never holds the server.** One file is identified per tick, then the other
  analysts are served; a 400-page PDF identifies in under a second. Scans are said
  to be scans, not OCR'd to fill a table. Convert waits (greyed, and says why) until
  the table is filled — a second or two.
- **After a run the table stays** — "Wrong template? Change it and press Convert
  again." On a case folder it folds away above the results.
- **Removed:** the *Bank* picker and the *It picked the wrong template?* disclosure.
  Each gave one answer for every file in the upload.
- **A chosen template that is not there is refused, not ignored.** It used to fall
  through to auto-detect without a word — reading the file with exactly the pick the
  analyst had overruled. Now that file fails with `template_unavailable` and a cure
  about the template ("choose another"), not about the file.
- Phone width: the table stacks into one card per file, and the result page's wide
  tables scroll inside their own box instead of pushing the page 142px sideways.

## 1.16.0

**A template built here can win auto-detect.**

Reported from production: three templates built in the toolkit, and auto-pick right
about a third of the time. Reproduced: a template drafted from one "Kowhai Bank of
Aotearoa" statement matched four sibling statements perfectly (3 of 3 phrases) — and
auto-detect picked `anz_everyday_pdf` on all four, confidently. The shipped ANZ
template also scored 3, on column headings half the country's banks print
("Withdrawals", "Deposits"), and the next tie-break was "shipped before hand-built".
So a hand-built template could never win a tie, and the one template that named the
bank lost to one whose bank appears nowhere on the page.

- **New tie-breaker, ahead of shipped-first: is the template's own bank printed in
  the statement's header or footer?** Never part of the score, so a template still
  has to earn eligibility on content. Generic words ("Bank", "New Zealand",
  "Limited") are not evidence on their own.
- **Header and footer are found by content, not a line count**: above the first line
  carrying a money figure, below the last. A transaction reading "TRANSFER TO ANZ
  400.00" is never evidence the statement is ANZ's, however short the page.
- The protection shipped-first was written for is kept and strengthened: a hand-built
  `anz_v2` still cannot beat the tested Westpac template on a Westpac statement — now
  because "Westpac" is on the page and "ANZ" is not. With no bank evidence either way,
  shipped-first still decides.

## 1.15.0

**A multi-file upload now has a file-count limit, because it had none.**

`max_upload_mb` is enforced per **request**, so a folder of hundreds of small
statements passed the size check and then converted one after another inside a single
job — with no way to stop it, and the first file's result invisible until the last one
finished. `app.max_batch_files` (default **50**, which is `R/batch.R`'s own "a case
folder is 10–50 statements" with room) refuses the upload **before any work starts**
and says the number. The upload control states the limit next to the size limit.

Refusing at the door costs the user one re-drag. Refusing halfway costs them the run.

## 1.14.0

**No wrong figure reaches a download unmarked.**

Measured on a **ten-statement** bundle whose segments were composed from 45pt left to
30pt right of the template. All ten split correctly and the run was held
(`needs_review`, trust `low`) — but eleven figures came back wrong, and every one of
them looked perfect on its own line. An analyst who downloads the workbook despite
the warning had nothing on the row to tell them which rows to distrust.

- **`columns_misaligned`** is now a row flag, derived from that segment's *own*
  `column_fit` — the only check that can tell statement 3 from statement 7 of one
  file. It does **not** null the figure: the figure is what the reader read, and a
  reader that blanks what it read is no use to somebody holding the statement.
  Refusing to *guess* and refusing to *show* are different things.
- **A zero the balances contradict is a misread, not a transaction.** One cell read
  as exactly `0` where the statement said `-24.16`; the balances either side were
  read perfectly and their difference *is* `-24.16`, but the balance derivation only
  fired on `NA`, so a fabricated `0` went out while the recovery that would have
  fixed it stood by. `0` is the most plausible-looking wrong figure there is — it
  sorts, sums and prints like a real one. A genuine `0.00` transaction is untouched,
  because then the balance does not move either and there is no contradiction.

Result on that bundle: **11 wrong figures → 10, and all 10 flagged**, in statements
the diagnostic names. Zero unflagged wrong figures.

**Two measurements worth keeping**, both corrections of my own earlier counts:

- the first count of that bundle said *27* fabricated. It was wrong: pairing rows
  positionally across the whole file, when statements 7 and 9 each dropped 3 rows,
  shifted every later comparison. Paired **per segment** the real figure was 11. A
  harness that can accuse the engine has to be as checkable as the engine — the
  third time that lesson has been paid for here.
- **mixed formats in one PDF** (three different banks concatenated) are detected and
  refused loudly: `multiple_statements` at high severity naming the remedy, plus
  reconciliation, balance-break, column-band and date diagnostics, trust `low`.

Suite 71 files / 1,001 tests / 5,372 passing / 0 failed.

## 1.13.0

**A bundle is now checked statement by statement.**

Ten statements in one PDF are ten documents, and a bank that re-ran its composition
between two of them moved the columns for the later ones only. Asked of the whole
file at once, the drift check returns **one** page-wide answer: it averages the
drifted statement away, or reports an offset that is wrong for every statement in
the file.

Measured on a two-statement bundle whose **second** statement sits 60pt to the left:
**7 fabricated figures** — the running balance read as the transaction amount. The
arithmetic caught the run (trust `low`, nothing published) and the whole-file check
did name the columns, but it could not say *which statement* had moved, which is the
one thing an analyst needs in order to fix it. It now says:

> statement 2: debit is empty on every row; credit holds something that is not an
> amount on 5 of the 12 rows; balance is empty on every row; 7 of the 12 rows carry
> an amount that no amount column covers; every amount column reads correctly with
> the bands 40pt to the left (anything from 10 to 69pt works)

**40pt did not break it**, which is the other half worth knowing: the bands absorb a
drift of half a column, so a bundle of slightly-varying statements is the *ordinary*
case and is read correctly and silently. Only the statement that really moved is
named.

The severity rule is now one function (`.column_fit_severity`) shared by the
whole-file and per-statement paths, so they cannot drift apart about what counts as
a fault — with a test that there is exactly one copy of it.

Suite 71 files / 1,000 tests / 5,369 passing / 0 failed.

## 1.12.0

**The app crashed its session on every single page load, and nobody could see it.**

One orphaned line sat at the top level of `server()` — `length(rb$tables) > 0L ||
length(rb$pairs) > 0L`, debris from a deleted function; `rb$tables` and `rb$pairs`
appear nowhere else in the file. Reading a `reactiveValues` field outside a reactive
consumer is **fatal**, so the server body aborted there, `bank_choice()` (defined
further down) never came into existence, and an observer above it failed too. The
websocket closed ~300 ms after load, Shiny greyed the page out behind its
disconnected overlay, and the "Working…" pill latched on because the last event was
`shiny:busy` with no `shiny:idle` ever following.

**Users saw a greyed-out screen that said "Working…" forever.** That is the whole UI.

**Why 996 tests missed it.** Every app test reads `app.R` as *text*. Booting and
checking for HTTP 200 does not catch it either — the HTTP response is the static
page, and the server function does not run until a **websocket** opens.
`shiny::testServer()` does not catch it (tried: MockShinySession evaluates the body
inside a reactive context, so the error never fires). It took driving a real browser
at the app with Playwright.

Two guards now, and both were verified by re-introducing the bug:

- **no statement at the top level of `server()` may discard its value.** A statement
  whose head is an operator can only produce a value, and at statement level that
  value goes nowhere. Harmless code never looks like that; orphaned code does.
- the UI sweep itself is a documented dev procedure, because the static guard cannot
  see everything a browser can.

**Also found by the sweep, and fixed:**

- **money was not formatted in the transactions table** — straight out of the CSV a
  figure rendered as R printed it, so the same screen showed `-12.4`, `3120` and
  `2,398.15`. The one column an analyst checks against the paper statement was the
  one column that did not look like it. Now always to the cent, display only (the
  CSV and XLSX keep the raw numeric, because a thousands separator in a
  machine-readable export is how a figure stops being a number on the way into Qlik).
- **the tagline still said "Statements and documents in"** — the tool has read bank
  statements and nothing else since 1.9.0.

**Confirmed working by driving it, not by reading it:** the click-and-drag column
box persists after mouse-up (78×281px, stable past 3.5s) and labels itself "this
whole column"; assigning a column updates the preview; conversion runs with a
centred progress overlay and lands on the result card with downloads, KPI tiles and
plain-English checks. Zero JavaScript errors on every route.

Suite 71 files / 997 tests / 5,361 passing / 0 failed.

## 1.11.0

**The tool no longer withholds readable text, and it is twice as fast.**

**1,376 lines deleted.** Everything that found text under a black box and replaced it
with `[REDACTED]` is gone: the marker-glyph and overlay-rectangle detectors, the
per-page rasterised occlusion scan, the token injector, two KPIs
(`redaction_summary`, `redaction_scan`), two diagnostics (`redaction`,
`redaction_unverified`), the X-ray's redaction layer, `R/detect_redaction.R` entirely,
and the `PARAM_REDACT_*` constants.

The reasoning is the operator's, and it stands: this tool reads a document it has been
given, and whether the sender redacted it competently is not its problem. A figure an
analyst can read in a PDF viewer but not in the spreadsheet is a **worse copy of their
own evidence**, which they will then transcribe by hand. `docs/context/charter.md`
carried the opposite promise — "honour redactions absolutely" — and now records the
reversal and why, rather than contradicting the code.

**The half that mattered got better, not worse.** A value genuinely *gone* from a file
still leaves an empty cell, and that cell is recovered from the running balance:
`balance[i] - balance[i-1]` **is** the amount. The old guard **blocked** that path — it
excluded any row it had marked redacted from the derivation — so deleting it switched
the recovery on.

**And it was most of the remaining runtime.** The occlusion scan rasterised pages
through an external process:

| pages | 1.9.0 | 1.10.0 | **1.11.0** |
|---|---|---|---|
| 100 | 49.8 s | 17.0 s | **10.2 s** |
| 400 | — | 69.5 s | **37.6 s** |

0.09 s/page, flat, 0 wrong figures at every size. Five times faster than where this
session started.

**Four real breaks the cull caused, all found by the suite, all worth recording**
because each was silent:

- `read_pdf_input()` kept a stale `markers = markers` argument, and `safe()` turned
  the missing name into an **empty read** — every PDF returned zero pages and
  detection fell through to a CSV template. Second time this session `safe()` has
  hidden a missing name.
- a regex that deleted the lexicon's redaction entries also ate
  `period_connectives` on the same line, so `lex()` threw and `extract_metadata()`
  failed — *also* behind a `safe()`.
- two `list(..., )` trailing commas (in `parse_pdf_table.R` and `config.R`) left an
  empty argument, which parses and then misbehaves.
- `.plausible_period_date()` had a latent length-0 bug that turned the above into
  "missing value where TRUE/FALSE needed" from three frames below the real cause.
  Guarded now.

A blunt first pass at the tests also destroyed all 129 blocks of `test-app-ui.R` by
matching the word "redacted" anywhere in a file; it was reverted and redone by
subject. The suite is the only reason any of this was visible.

Suite 71 files / 996 tests / 5,356 passing / 0 failed. Corpus 43 cases, 41 clean, 0
fabricated.

## 1.10.0

**A wrong figure that was live on every multi-page statement, and the tool is now
three times faster on a big one.**

**The sign read from the page was only ever read from page 1.** `.pdf_ink` split the
renderer's output on a marker poppler 24.02 does not emit, so a multi-page statement
collapsed into one ink entry holding every page's ink. Pages 2 onward got **no sign
correction at all**, and page 1 got **false positives** from strokes elsewhere in the
document. Measured on two new 3-page corpus cases: **48 fabricated figures**, every
one a sign inversion (`731.87` where the statement said `-731.87`), all from row 31 —
page 2 — and **trust stayed `medium`, so they would have published**. Both faults are
invisible to the text layer and neither is caught by arithmetic on a statement with no
running balance, which is exactly the shape of the `anz_investmentfunds_pdf` template
we ship. Every ink case in the suite was a single page, so nothing failed.

- `.pdf_ink` now returns one entry per page, and **refuses to apply ink at all** if
  the count does not match the document rather than guessing the alignment.
- A statement read **without** that scan now says so — `sign_scan_unavailable`, high
  severity, owner `escalate`, because a missing `pdftocairo` is an install fault and
  the consequence is inverted money-in/money-out with every check passing.
- `draw_signed` in the corpus generator paginates, which is what made the regression
  testable: this layout has no balance column, so the ink is the only evidence of the
  sign.

**It scales now, and the figures are documented.** Nothing had ever been run above
nine pages. A 100-page statement took 49.8 s; it takes **17.0 s**, and 400 pages /
12,000 rows takes 69.5 s at a flat 0.17 s/page with 0 wrong figures. Two stages were
the cost:

- the **drift check** was 46% of a 100-page conversion. Capped to eight pages spread
  across the document — the information is not linear in pages even though the cost
  was. 23.0 s → 1.7 s, and flat.
- the **vector-redaction scan** rasterised every page of every PDF. The render pass
  already done for the sign check says which pages draw a filled shape or an image,
  and a page that draws neither cannot hide text, so it is not rasterised. Reading
  100 pages: 14.6 s → 3.8 s. The gate only ever says "there is nothing here" — with
  no usable ink list every page is scanned as before, because a page wrongly skipped
  would leak blacked-out text.

**No container deployment.** `deploy/Dockerfile`, the ShinyProxy config, the
operational page for it and the app's `SHINYPROXY_USERNAME` identity branch are
**deleted**. It cannot run on an air-gapped Windows box without a Linux guest and a
container runtime, Docker Desktop is licensed for government entities
unconditionally, and a deployment option in the tree that cannot run in the only
environment this tool is installed in is a thing the next maintainer has to cost and
reject again. The reasoning, and the triggers for revisiting it, are in
[`docs/context/architecture/locked-decisions.md`](docs/context/architecture/locked-decisions.md).

**A long statement is no longer told to split itself.** The `oversized` advice above
100 pages said "may hit tool limits; split into smaller files". Both halves were
wrong: 400 pages convert in 70 seconds with no limit to hit, and splitting a statement
**destroys the opening-plus-transactions-equals-closing check**, because each piece
then has an opening balance nothing printed. The tool was telling analysts to degrade
their own evidence to fix a problem it does not have. Now `info`, with the measured
figure and an explicit "do NOT split".

**A scan is 55x slower, and the wait is stated up front.** 9.3 s a page against 0.17,
so a 120-page scan is nineteen minutes behind the same "Converting statement...".
`conversion_estimate()` probes the file in 0.05 s and says how long, and says it is
not stuck. Five concurrent 100-page jobs against a cap of 3 finish in 43.8 s (one
alone: 17.0 s), all five complete, and `job_queue_ahead()` lets the screen say where
someone is in the queue.

**The offline bundle was missing a package our own code names.** `openssl` is called
by name in `R/util.R`, `R/convert.R` and `R/layout.R`, and it was in neither the
bundle's list nor any listed package's dependency tree. The hashes still came out
right, because `digest` computes the same SHA-256 and ships as a dependency of shiny
— but a required package present by accident of another package's dependencies is not
a requirement that has been met. `openssl`, `digest`, `zip` and `htmltools` are now
named explicitly, in both `bundle-offline.R` and `install-offline.R` (the suite
already enforced that those two agree).

**And `file_sha256()` could return an MD5.** The last-resort branch was
`tools::md5sum`, so with neither openssl nor digest available a function named
`file_sha256` returned an MD5 — into a field labelled sha256, in an evidence record,
with nothing saying the algorithm had changed. "Is this the same statement the analyst
converted in March?" would have compared two different algorithms and confidently
answered no. It returns `NA` now, which is what a missing file already returns, so
every caller handles it.

**The bundle manifest understated what a missing Poppler costs.** It said "scanned
PDFs will not be readable", which is true and is the smaller half: without
`pdftocairo` a statement that draws its minus as ink converts with money-in/money-out
**inverted**. The manifest, the build warning and the go-live checklist now say so.

**Also:** a template that stops fitting now names the column (`column_bands`); the
account number's shape is checked (`account_number`); no two files may define the same
function name (a guard, after `.col_kind` was silently defined twice); and the
architecture is written down as closed decisions with the evidence that closed each.

## 1.9.0

**This tool reads bank statements. It does nothing else.**

The two other routes — forms (`mode: fields`) and reports (`mode: document`) —
are **removed**, engine and screens together. A statement is the only kind of
document the tool accepts, the only kind it has a template for, and the only kind
any screen asks about.

### Why

A report engine and a statement engine do not cost the same to be right about.
A statement reconciles: the opening balance, the running balance and the closing
balance check each other, so a wrong read has somewhere to show up. A report has
nothing of the kind — no arithmetic anywhere can tell a figure read out of the
wrong column from the right one — and it was download-only for exactly that
reason. It carried roughly a third of the codebase, every screen had to ask
"which kind is this?" before it could say anything, and the answer was never
checkable. The statement path now gets all of the attention.

### What went

| Removed | Was |
|---|---|
| `R/tables.R`, `R/tables_detect.R`, `R/doc_extract.R` | the report engine (many tables of different shapes) |
| `R/forms.R`, `R/extract_fields.R` | the form engine (labelled values found by wording) and the three-door front door |
| `templates/fields\`, `fields_user\`, `documents\`, `documents_user\` | the two other template libraries |
| `tools/corpus/` | a survey harness for the report engine |
| 7 test files, 2 helpers, 1 fixture builder | the tests behind all of it |
| `docs\operational\pulling-tables-out-of-a-report.md` | the analyst page for the report route |

**9,893 lines of executable code and 44 MB of specimen PDFs.** `R/` + `app.R` +
the two `ui_` files: **31,368 lines → 21,741**.

### What that simplified, rather than merely deleted

- **One front door.** `convert_statement()` in `R/convert.R`. There is no
  `convert_document()` trying three pipelines in order and no `kind` to stamp.
- **`.is_txn_result()` and `.statement_route()` are gone from `app.R`** — ten
  call sites that asked "is this actually a statement?" before every card, every
  table and every trust headline could speak. The answer is now yes, always.
- **`diag_for_route()` / `plain_diag()` lost their `statement` argument** and
  `DIAG_PLAIN_OTHER` / `DIAG_FIX_PLAIN_OTHER` went with it: two whole dictionaries
  that existed to re-word a statement diagnostic for a document that had no
  running balance.
- **`template_kind()`, `.TEMPLATE_KIND_LABEL`, `.TEMPLATE_KIND_NOUN` and
  `template_library_name()` are gone.** One kind needs no discriminator.
  `.template_shape()` lost two of its three branches; `library_overview()` lost
  its `kind` column and the row grouping that existed to split by it.
- **`.detect_by_fingerprint()` is gone** — a detector parameterised over noun,
  normaliser and namer so two routes could share it, with zero callers left.
- **Four config paths removed** (`fields`, `user_fields`, `docs`, `user_docs`),
  with their legacy-name rewrites and their folder-migration entries.
- **Two dead parameters removed** from `R/params.R` (`PARAM_DOC_MIN_COL_PT`,
  `PARAM_DOC_SAME_PAGE_PT`).

### The one real bug this found

**A pinned header value stopped being read, and said nothing.** A statement
template may pin a header figure to a drawn box (`table$metadata_regions`) for
layouts whose wording the label dictionary cannot find. That reader called
`.field_from_region()`, which lived in `R/extract_fields.R` — deleted with the
form engine. The call site wraps it in `safe()`, so the missing function became
`NA`: every pinned closing balance, opening balance, account number and statement
period silently came back empty instead of erroring. The function is restored to
`R/labels.R`, beside `match_label()` whose counterpart it is, and the two tests
that cover it pass again.

A sweep for every other name the deleted files provided found no second case:
the remaining references were all comments, now corrected.

### A measured corpus, and the three faults it found

`tools/synth/` is new and is the first thing in this repository that can say how
accurate the reader IS, rather than only whether it has changed. A Python generator
draws adversarial synthetic statements - 31 cases: page offsets, odd-page offsets,
a table starting on page 2 and page 3, column bands nudged 3pt and 10pt, left-
aligned amounts, wrapped descriptions both indented and blank-stubbed, DR/CR
suffixes, bracketed negatives, comma decimals, a minus drawn as vector ink, an
unheaded total row, a footer shaped like a transaction, missing column headings,
9pt row pitch, no opening balance, no balance column, an overdraft printed two
ways, a period crossing 31 December, landscape, and /Rotate 90 - and writes beside
each PDF the exact rows it was drawn from. `score.R` then reports how many figures
the reader **fabricated**, counted separately from how many it honestly **refused**.

A golden file cannot do this: a golden file is the reader's own output, so if the
reader is wrong the golden is wrong with it and agrees with itself for ever.

**The corpus found three faults, each now fixed with a suite test of its own.**

**An amount cell holding words became a number.** A column band collects every word
whose centre falls in it, so a long description that overflows its own band drops
words into the amount band beside it - and `.num_one()` removed everything that was
not a digit and GLUED THE REST TOGETHER. The cell `ASSESSMENT 2291104A 7.44` - a
reference number beside a $7.44 credit - came back as `22911047.44`. Not a crash
and not a blank: a plausible figure four orders of magnitude wrong, in a row whose
date and description were both right, which is the one error a reviewer reading the
screen cannot catch. 13 of 20 rows on the measured case. Such a cell is now `NA`
and the row carries the `malformed` flag that already says "the amount could not be
read as a number". **Never a guess**: "take the rightmost money-looking token" would
be right most of the time and silently wrong the rest, which is worse than a gap.
Every real way a statement prints money still reads - `$1,234.56`, `NZD 1,234.56`,
`1,234.56 CR`, `196.16 OD`, `(123.45)`, `1 234,56` - and a site's own configured
sign marker counts as declared vocabulary rather than noise.

**A landscape page was squashed into a portrait frame.** The band-frame rescale
exists for one statement at another size - a rescan, another export, a different
scanner DPI - and treated *any* size difference as that. A landscape page is a size
difference: against an A4-portrait frame every `x` was multiplied by 0.707, so the
`Withdrawals` heading at x=341, inside the debit band, landed at x=241 - inside the
*description* band, with every column slid one to the left. It read 0 of 16 rows.
Not rescaling it reads **16 of 16**. The danger was never the total failure but the
near miss: an aspect ratio close enough that dates still parse while amounts slide
into the next column. Every real paper size is still rescaled (Letter is 9% from
A4's aspect ratio, Legal 17%; an orientation flip is 100%).

**A rotated page read nothing and said nothing.** A `/Rotate 90` page - ordinary
scanner output - produced an empty table with no reason given. It is now
`unsupported` with a high-severity `page_orientation` diagnostic naming both shapes
and the remedy. It speaks **only when nothing was read**, because a landscape
statement that parses perfectly must not carry a high-severity warning: that is how
an analyst learns to read past high-severity warnings.

The generator also found a fault in **itself**, recorded because it is the same
class: `money()` printed `abs(x)`, so an overdrawn balance printed without its sign
and the first scoring run blamed the engine for 221 wrong balances on a 260-row
statement. The reader was right and the generator was lying. A second case was
measuring an impossible document - 260 rows stepped 1-3 days apart made a *twenty
month* statement period, then blamed the reader for inferring the wrong year on
dates printed with no year at all. Both fixed; the corpus now reads 29 of 31 cases
clean with **0 fabricated figures**, and the two that are not clean are both correct
refusals: a reworded fingerprint is not detected, and a rotated page is refused with
its reason.

### Four more faults the corpus and the research found

**A typeset minus read as POSITIVE.** poppler returns a typeset minus as `U+2212
MINUS SIGN`, not an ASCII hyphen — this repository's own fixture generator carries
the note — and the number parser only looked for `-`. So `−123.45` came back as
**+123.45**: a withdrawal read as a deposit. `U+2010`, `U+2011`, `U+2013`, `U+2014`,
`U+FE63` and `U+FF0D` were wrong the same way. Every shipped fixture happens to use
an ASCII hyphen, so 5,145 passing tests went straight over it — the suite was
self-consistent and wrong together. `U+00AD` SOFT HYPHEN is now *removed* rather
than read as a minus: treating an invisible line-break hint as a negation would
invent a sign the page never showed.

**The sign the page draws, and the sign it hides.** Two constructions real
statements use, opposite faults, both silent:

- a minus drawn as a **line** of vector ink is absent from the text layer, so every
  withdrawal reads as a deposit;
- a minus printed in the **background colour** (banks do this on positive amounts
  to keep a column right-aligned) is in the text layer and not on the page, so
  every deposit reads as a withdrawal.

Both need a **signed single amount column** to bite, and `anz_investmentfunds_pdf`
— a shipped template — is that shape *and* has no balance column, so there was no
arithmetic to object with: 14 of 16 rows inverted one way, 3 of 16 the other, all at
trust `low` with nothing saying why. Both now read 16 of 16. The sign is taken from
the page itself with `pdftocairo`, from the poppler bundle the OCR path already
needs — no new dependency, no Python, no new R package. A new `sign_from_ink`
diagnostic says the page prints its signs this way, because a minus that is hard to
see by eye would otherwise make a correct negative look like a mistake.

**A hole the page can fill itself.** A statement printing a running balance says
what moved between two rows: the difference between their printed balances. Where
the amount cell could not be read but both balances could, the figure is arithmetic
on two printed numbers rather than a guess — 20 of 20 recovered on the measured
case, each flagged `amount_from_balance`. It never crosses a **redaction** (deriving
a hidden amount would make this tool the thing that defeats a redaction applied for
privilege), never overwrites a figure that *was* read (a disagreement is a conflict,
not a hole), and never invents a zero from two equal balances.

**And the verifier was taught to subtract it.** `sum(balance[i] - balance[i-1])`
telescopes to `closing - opening`, so if every amount were derived then
`opening + sum(amount) = closing` **by construction** and `balance_reconciliation`
— the strongest completeness proof this tool owns — would report a pass having
tested nothing. Both checks now count the derived rows: partial derivation stays a
real check and says how much was derived, total derivation is honest `na`.

### An identity the audit log can stand behind

**A live audit-integrity defect, not a hardening exercise.** The app trusted any of
**eight** identity header names with no check that the request had come through a
proxy, while listening on every network card. So

```
curl -H "X-Forwarded-User: some.other.detective" http://host:8100/
```

made the run log record a conversion against a name the sender chose, in the tier
`R/logging.R` defines as "an identity forwarded by a proxy/gateway. Also
per-person". A record that certifies a claim it cannot know is worse than one that
records nothing: a blank is a gap, a wrong name is evidence of the wrong thing.

The header now says *who*; a **shared secret** that exists nowhere but the proxy's
own configuration shows the claim came from something entitled to make it, compared
in constant time. No secret, no `sso` — and it **downgrades** rather than refusing,
because a mistyped secret must not take the tool away from a whole office. Eight
header names became one, from config (one of the eight was a Cloudflare header, on
an air-gapped server). Two `httpuv` behaviours that defeat a correctly configured
proxy are now refused: duplicates **join with a comma** into `"attacker,real.name"`,
and the **underscore spelling** `X_Remote_User` overwrites the hyphen one — nginx
drops underscore headers by default for this reason, **IIS and Apache do not**.

**`app.bind_host` makes the listening address configurable**, defaulting to
`0.0.0.0` on purpose: flipping it to loopback in an update would take a running
deployment offline, and that is your call, not an upgrade's.
`docs\operational\who-is-using-it.md` is the procedure, and is explicit about what
it does not fix.

### Who opened whose statement, and when

Downloads were recorded nowhere, so the first question anyone reviewing this tool
asks could not be answered. There is now one record per download in
`logs\downloads\` — what, which object, which conversion, when, who, how the
identity was established, and the **SHA-256 of the bytes handed over**. That last
field is the one that matters after retention deletes the source statement: it is
then the only proof of what was produced and taken. All six download handlers write
one, and a test walks `app.R` rather than naming them so the rule holds for handlers
nobody has written yet. It never breaks a download — one that works but is not
logged beats one that fails because the logging did.

**A record is not a gate.** It says who took a copy; it does not stop anyone taking
one. That needs case ownership, which does not exist yet.

### Two more things the health check now asks

Both name a way the server can be **wrong while looking fine**:

- **Identity** — which of the four configurations is in force, including the unsafe
  one (a header believed while the app is reachable without the proxy). An operator
  previously could not find out without reading two files.
- **Signs** — whether `pdftocairo` and `pdftotext` are installed. The reader fails
  **quiet** without them: it simply gets no extra evidence, so a server missing them
  reads a drawn or hidden minus with the sign inverted and reports nothing wrong.
  They ship in the same poppler zip as `pdftoppm`, so normally both are present.
  "Normally" is not something a forensic tool may rely on.

### Seventy lines that measured as worthless, and came back out

`pdftools` floors every coordinate to a whole point, pushing a word's **centre**
0.49pt left on average and up to 1.31pt — and the centre decides which column band
a word joins. `pdftotext -bbox-layout` fixes it with no new dependency, so it was
built and wired in. Scored against the corpus: **998 of 1,034 rows correct with
integer boxes, 998 of 1,034 with floats.** Not one row changed, on any of 33 cases,
including three that nudge a band by 1, 3 and 10 points — because a band boundary
lives in the **gutter** between columns, and a real gutter is several points wide.
The 70 lines came out; the measurement stayed in `R/read_pdf.R` under "MEASURED AND
NOT DONE". Unmeasured machinery is what the cull was against, and that standard
applies to new work too.

### Still true, and checked

The suite is **72 files, 985 tests, 5,293 passing assertions, 0 failed,
0 errors**, with one skip — a split test that needs a Westpac bundle kept out of
the repository on purpose. The app boots and serves. Nothing in `R/` reads
`app.R` or the two `ui_` files, and the directory map in
`docs/context/architecture/build-contract.md` matches `R/` in both directions,
because a test fails if it does not.

### Copying this release onto the offline box

Read [`docs/operational/updating-a-version.md`](docs/operational/updating-a-version.md)
for the procedure and the *Never copy* table. **This release both changes and
DELETES files, and the deletions matter**: an old `R/forms.R` left behind on the
box is still sourced at startup and will error on functions that no longer exist.
Delete first, then copy.

**Delete on the box:**

```
R\forms.R   R\extract_fields.R   R\tables.R   R\tables_detect.R   R\doc_extract.R
templates\fields\   templates\fields_user\   templates\documents\   templates\documents_user\
tools\corpus\
docs\operational\pulling-tables-out-of-a-report.md
tests\testthat\helper-doc.R   tests\testthat\helper-doc-hard.R
tests\testthat\fixtures\make_document_fixture.R
tests\testthat\test-doc_app.R        tests\testthat\test-doc_hard.R
tests\testthat\test-doc_pairs.R      tests\testthat\test-doc_pdf_roundtrip.R
tests\testthat\test-doc_tables.R     tests\testthat\test-extract_fields.R
tests\testthat\test-forms.R
```

**Then copy over:**

```
app.R   ui_labels.R   VERSION   CHANGELOG.md
R\   (18 files: analytics batch batch_audit config convert diagnose jobs
       labels logging metadata_capture normalise parse_pdf_table read_input
       read_pdf reconcile templates util)
R\params.R            (see the note below)
scripts\health-check.R   scripts\install-offline.R   scripts\run_app.R
templates\README.md
docs\   (9 files: design.md  operational\who-is-using-it.md (NEW)
         context\architecture\build-contract.md  context\how-it-fits-together.md
         context\roadmap.md  for-analysts\README.md  operational\README.md
         operational\maintaining-the-engine.md
         operational\when-something-goes-wrong.md)
tests\  (20 files: helper.R, test-download-log.R and test-read_pdf-ink.R are
         NEW; and test- app-ui batch batch_audit config convert deployment
         diagnose docs-truth jobs labels metadata_capture normalise
         parse_pdf_table reconcile robustness seams templates)
```

`tools\synth\` is NEW and is deliberately **not** on that list: it is a dev-time
measuring harness that needs Python, the box runs R alone, and nothing in the app
reads it. Copy it only if you want to run the corpus on a machine that has Python.

**`R\params.R` is the file you are told never to copy in a sweep, and its only
change here is a DELETION** — the two `PARAM_DOC_*` parameters nothing reads any
more. Nothing fails if you keep your old copy; it simply keeps two dead lines.
Either take the new file and re-apply your own values by hand as step 5 says, or
leave yours alone. Your choice, and both are safe — unlike 1.9.0's earlier draft
of this note, which was written when those parameters were live.

`www\app.css`, `config\`, `dictionaries\` and `samples\raw\` are **unchanged**
from 1.8.1. Do not copy them.

72 files, 985 tests, 5,293 passing, 0 failing, 1 skipped.
---

## 1.8.1

**A document nothing recognised is not "this statement".** Reported: drop a
non-statement PDF into Convert and the only thing offered is the statement
toolkit. The card said *"No template read this **statement** — set up a template
for this **statement**"*, and the headline above it said *"No template for this
**statement** yet"* — asserting, three times, the one fact nobody has at that
point. All three pipelines have just said they cannot tell what the file is.

**A clear decision, or a clear way to override it — never two big buttons.** Two
doors side by side with the likelier one styled primary hands somebody a choice
without telling them they are making one, and the only difference between the two
is a shade of green. So:

- **When the tool can tell, it decides and says so.** *"This looks like a report,
  not a bank statement."* One primary button does that thing. The other answer is
  underneath, spelled out as an answer — *"Not a report? It is a bank or card
  statement"* — not as a second equal button.
- **When it cannot tell, it says that and asks.** *"The tool cannot tell what kind
  of document this is. Which is it?"* Two equal buttons are honest there and
  nowhere else; dressing a coin toss as a decision is what this card exists to
  stop.

It is not a coin toss when it does decide. `doc_shape_hint()` reads the first
pages and counts two things: table-shaped blocks, and lines that start with a date
and carry an amount, which is what a transaction row is whatever bank printed it.
**The count is printed under the decision**, so it is checkable rather than
asserted.

- The words for the two kinds are the words on **Add a template** — *a bank or
  card statement* / *anything else* — because somebody meeting the same question
  twice should not have to learn it twice.
- Overriding onto the statement path with a document that has no transaction
  table now names the button already on screen (*"press Set it up as a report on
  the green card"*), instead of a radio called "Something else" on a tab it did
  not offer to open. The radio is called *Anything else*.
- **The report door carries the file through it** — no hunting for the same PDF
  on the next tab.
- One plain sentence says what the two kinds *are*, since the person is being
  asked to choose: a statement is a table of transactions with a running balance
  and it reconciles; anything else is read by pointing at what you want, and
  downloads without reaching the dashboards.
- The date test is deliberately looser than the one the value extractor uses.
  Statements print `02 May`, `02/05`, `2 May 26` and `02-05-2026` as the leading
  date of a row; requiring a parseable three-part date found 3 rows on a page of
  11.
- Pages are counted **individually and the best one wins**. A statement's first
  page is a cover — name, address, four summary figures — and its rows start on
  page 2, so summing across pages lets the cover outvote the evidence.

Measured: the tutorial statement reads as *statement* (12 dated amounts on one
page), a three-page report and a government health table both read as *report*
(N198).

---

## 1.7.1

A UI pass over the whole builder, walked screen by screen, plus four things
reported from using it.

**A row broken mid-word came out with a space in it.** An email the PDF wrapped
read `xys@ gmail.com`. A space is right nearly always — a wrapped description is
two words — and wrong exactly when the break falls inside one token. Line breaks
now close without a space when the first fragment ends on a joining character
attached to a word (`@ / \ _ = + -`) or the second opens with one. A trailing full
stop is deliberately *not* on that list: "Acme Ltd." followed by a new sentence is
far commoner than a token broken after a dot. The same rule applies to a value box
drawn round a wrapped address, and the picker reaches down one line to find the
rest of it (N194).

**The preview showed the first table and nothing else.** Under a summary that
faithfully listed all of them — so the screen said "6 tables, 412 rows" and then
showed you 38 of them, with no control anywhere to see the rest. Every table now
has its own block with its name, its row count, and its rows, in the order the
workbook has them (N195).

**Column names stopped colliding at the top of the page.** They were laid out two
rows per *table*, so a second table on the same page started again at row one and
wrote over the first. Placement is a page-level pass now: every label from every
table takes the first row where it clears what is already there, and the strip
grows downwards rather than overlapping (N196).

**Edit re-arms the drag**, on a column and on a value, because pressing Edit on a
specific thing *is* a statement of intent about that thing. The armed-intent model
is unchanged — one thing at a time, written across the top, cancellable — and the
banner now names what you are editing and says it is outlined on the page. On a
value, Edit arms the figure; "Re-draw the LABEL instead" is one press in the
banner (N197).

**Notifications are centred near the top**, not in the bottom-right corner. On a
screen whose work is a document on the left and a panel on the right, the corner
is the one place nobody is looking — so "that box holds one unbroken run of words"
was said, correctly, into empty space.

From the walkthrough itself:

- **The first screen contradicted itself.** The file picker said
  ".csv / .tsv / .tdv / .pdf / .xlsx" and the panel below it said "It has to be a
  PDF" — both true, of different halves. The kind of document is asked *first*
  now, and the picker then says which files that job takes. Uploading a
  spreadsheet for a report used to do nothing at all with no message; it now says
  what happened and offers the statement path.
- **"Read the whole document" is off until there is something to read**, with the
  reason beside it. A green button that answers "nothing to read yet" was the most
  prominent control on the lower half of the screen for the whole of the work.
- The instruction under the picture no longer repeats "nothing is armed" — the
  banner above it already says so, and it is pinned to the top of the window.

---

## 1.7.0

Seven things reported from the screen in one go. Two were crashes or losses, four
were the tool arguing with the person about what they could see, and one was a
question that turned out to be a design mistake.

**"Invalid 'type' (character) of argument" when you read a document with no
tables on it.** A template made entirely of label/value pairs — which is what a
form is — fell over at the last step, after all the work, on the screen that
shows what came out. A zero-row summary was ten `character(0)` columns, and
`sum()` over one of those is an error, not a zero. The empty frame now carries the
same column types as a full one, and the screen forces every count to a number
besides (N187).

**Columns may now have whitespace between them.** Asked directly: *"do columns
have to be joined? can there be white space between two column definitions?"*
They had to, and it was costing something on every screen — adding a column
either widened its neighbour over a column of figures or invented a column in the
gap. A report has real whitespace between its columns, and the tool insisting
otherwise was the tool arguing about what is printed on the page.

- **Overlaps are still impossible.** A gap is visible and counted (*unclaimed
  words*); an overlap silently reads a figure into one column and loses it from
  the other. A move or an insert that would overlap is trimmed at the neighbour
  and the screen says so.
- **Adding a column adds one column, exactly where you drew it.** Nothing else
  moves.
- **Moving a column moves that column.** Neighbours are no longer absorbed.
- **Deleting one leaves its space empty** rather than handing it to the column
  beside it (N188).

**Where it starts and where it stops are now steps, not settings.** Reported:
*"start and end also need to define where the table ends, there's lots of text at
the bottom of some pages which is not the table, but the table still spans
multiple pages."* The same sentence-at-a-time guide that gets the columns now gets
these: columns → starts → ends → **and, when the table runs over pages, the bottom
edge of the pages in between.**

That last one is the fact nobody was ever asked for. *Where it ends* is a place on
one page — the last one. On every page between, the table ran to the bottom of the
paper, so a footnote, a source line or a page footer under it was read in as rows
on every page but the last, and the person who set the end correctly had no way to
see why. Every step is skippable, and the guide stops the moment you steer
yourself (N189).

Building the step turned up a second fault behind it: **the setting was being
dropped on the way to the reader.** `document_template_from_proposal()` did not
carry the table's `band`, so the bottom edge was set on the draft, drawn on the
page, written in the panel — and never reached the engine. A control that changes
nothing is worse than no control: the question gets answered and stays answered
wrongly. Driven end to end in a browser on a three-page table with a footer: **40
rows with the footer in it before, 36 rows and a clean tick after** (N193).

**The values were half the builder and you could not see them.** Two reports, one
gap: *"the read whole doc should have the tables AND label value pair"* and
*"where do the value label pairs come out in the CSV long? I can only see it on
workbook"*. The pairs are now shown in the preview beside the tables, the verdict
counts them, and the values CSV — which was always written — has a download button
on both the builder and Convert. The long CSV is the tables stacked *page, row,
column, value*; a pair is none of those, which is why it needs its own file
(N190).

**A value's name is the key it comes out under**, so it is lower case with
underscores: type *IRD Number*, get `ird_number`. The line under the box says what
it will become while you type; the box is set to it on save. Never while you are
typing — rewriting a box under somebody's caret is the one thing this screen must
not do (N191).

**"Nothing was waiting for that drag" now names the button you needed.** Reported:
*"when I click edit, and drag where I actually want the column, it says nothing
was waiting."* True, and useless — the button was named nowhere in the sentence.
It now names the column and the button (N192).

---

## 1.6.1

**"+ Add a column" was stretching the first column over everything instead of
adding one.** Drag over the second printed column of a table whose first column
is already carved, and the answer was ONE column spanning both — every heading
and every figure of the second column swallowed into the first, and the first no
longer where it had been put (N185).

The drag's two sides were being fed through `doc_edge_click()` one after the
other, and a click **outside** the table means "move the outer edge out to here",
because that is what widening a table by clicking beside it has to do. So the
right-hand edge was moved out twice, exactly as asked, by a function answering a
different question. A drag is not two clicks: `doc_add_column()` now says *this
band is a column of its own* — both sides become edges, dividers strictly inside
the band are absorbed, and **every other edge is kept**, including the outer ones.

Ground between the new column and the one before it therefore becomes a column
too, rather than being swallowed into its neighbour. That is the cautious
choice on purpose: an unwanted column goes away with one click on its divider,
whereas a swallowed region cannot be recovered without redrawing both. The
screen says so when it happens.

**The instruction now sticks to the top of the window.** The one sentence saying
what the next drag will do was written at the top of the *page* — and with an
840px document image and a longer panel beside it, every real drag happens
scrolled down, so the sentence was above the window whenever it mattered. It is
pinned to the top of the screen for as long as the builder is open.

**Uploading swaps the screen.** The upload panel is a heading, a sentence, a file
picker, two radio buttons and a note — all of them answered the moment the
document is on screen, and four inches of it between the reader and the work. It
now folds down to one line naming the document, with a way back that says it also
changes the kind of document. The builder starts above the fold.

---

## 1.6.0

**Every template lives in one folder now.** There were seven at the root of the
app — `templates`, `templates_user`, `templates_seed`, `fields_templates`,
`fields_templates_user`, `doc_templates`, `doc_templates_user` — and telling them
apart meant already knowing the naming convention. They are now one folder with a
`README.md` that is the map:

```
templates\
  statements\  statements_user\  statements_seed\
  fields\      fields_user\
  documents\   documents_user\
```

A folder name is the template's `mode:`, so a template cannot be read as the
wrong kind — that is the same fact twice rather than two facts that can drift.
**The separations are untouched**: curated vs `_user` is still the Qlik
governance gate, and one folder per mode is still what keeps a report template out
of statement detection.

- **An existing install moves itself, once, and says so.** On the first start
  after the update the old folders are moved into place and each move is written
  to `logs\startup.log`. It moves files, never copies-and-deletes and never
  deletes; a destination file that already exists is never overwritten — the
  source is left alone and reported, every run, until a person deals with it; an
  emptied folder keeps a `MOVED.txt` so somebody who goes looking finds a sentence
  rather than nothing. Running it twice does nothing (N184).
- **A settings file naming the old folders is read as naming the new ones.** A
  `config.yaml` wins over the defaults — that is the point of it, and here it
  would have been a trap: the folders move, so a config still saying
  `templates_user` would point at an empty one and every template the team built
  would vanish from the app with nothing said. Only exact legacy names are
  rewritten; a path somebody chose on purpose is left alone.
- **New page: [updating a version](docs/operational/updating-a-version.md)** —
  merging a dev folder into the live one by hand. What must never be copied, what
  to copy deliberately, seven steps, and five checks that each prove a different
  folder came back.
- Templates built in the app can no longer be committed by accident: the three
  `templates\*_user\` folders carry nothing but a `README.md`, which is the entire
  reason a folder-replace update cannot overwrite somebody's template. Enforced by
  `.gitignore` **and** asserted in `test-deployment.R`, because a rule only a
  `.gitignore` knows is a rule nothing checks.
- Test helpers ask `templates_dir()` / `user_templates_dir()` / `fields_templates_dir()`
  / `document_templates_dir()` instead of spelling the path out, so the next move
  touches one file rather than forty.

---

## 1.5.6

**A folder of irreplaceable work that no backup procedure named.**
`doc_templates_user\` — where every report puller somebody builds by hand is
saved — was carried by the bundle, so an update could not destroy it. But it was
in none of the five pages that tell a person what to protect: the backup page
still said "four folders cannot be rebuilt", and the restore, rollback and folder
maps all listed the other two `*_user\` folders and not this one. A server
rebuilt from a documented backup would have come back with every bank template
and every taught word and none of the document templates (N183).

- `backup-and-restore.md` — the fifth folder, in the table, the `robocopy` block,
  the "check it worked" step and the restore list
- a restore check of its own, because **Admin → Templates does not list document
  templates** — they are matched at the front door, so the only way to see one is
  to upload the report and watch it be recognised
- `updating.md`, `rolling-back.md`, `running-and-keeping-it-up.md`, `design.md` —
  the same folder, in every place the others are named
- `test-deployment-docs.R` now fails if the backup page stops naming any of the
  three `*_user\` folders

**And the hand copy nobody had written down.** `updating.md` covered the
supported route — build a package, replace the folder — which is safe because
`bundle-offline.R` renames `dictionaries\*.yaml` to `*.example.yaml` before it
ships. Copying a few files straight out of the **source** folder skips that
rename, and the source folder carries those two files under the live names. The
result is silent: every wording and marker Admin has taught the tool is replaced,
nothing errors, and statements that reconciled last week quietly stop
reconciling. New section — *If you copy individual files by hand* — with that
warning, the other four things a sweep would take with it (`config.yaml`, the
three `*_user\` folders, `logs\`/`uploads\`/`feed\`, the private R), and
`R\params.R`, which must be re-applied by hand rather than dragged. Both facts
the warning depends on are now asserted, not just described.

---

## 1.5.5

**Measured what had only been claimed: is it actually nice to type in?** Driven in
a browser at 45ms a keystroke, across all five boxes of the builder:

- every character survives — nothing is eaten
- no input is a different DOM node afterwards — no box is rebuilt under the caret
- **drawing a box causes zero redraws while the mouse is held down**
- a whole typed phrase costs one or two plot recalculations, not one per letter

One residual found and fixed on the way: the numeric column-edge boxes
re-selected their own column after every commit, and assigning a `reactiveValues`
field invalidates it *even when the value is identical* — so the selection was
pushing values back into the two boxes the person was typing in. Guarded, and the
contract is now pinned by tests rather than by care.

---

## 1.5.4

**The suite is green, and it is the first time it has been.** Five red lines had
been carried for a long time and explained — in the maintainer's guide and by
more than one person — as "the environment, not the code". That was wrong. Four
of the five were **test bugs**; the engine was right throughout.

- Two test files built their input images with `magick::image_draw`, which hands
  back an image still attached to a live graphics device. The same drawing gave
  `detect_dark_regions` 0 regions or 1 depending only on what had run before it.
  Built through a PNG file instead, the answer is the same every time — and
  correct (N179).
- A fixture leaked a graphics device by restoring `par` *after* `dev.off()`,
  which silently opens `Rplots.pdf` in the app folder and leaves it open for the
  rest of the process. That stray file no longer appears after a suite run
  (N180).
- One assertion pinned the R version rather than the code. The rule it protects
  is asserted separately and passes (N181).

A board that is permanently red at five teaches everybody to read past red, and
the next real failure then arrives on a board nobody reads. **`failed: 0`,
`errors: 0` means what it says now.**

---

## 1.5.3

**Putting it on the server is one page now.**
[deploy-on-the-qlik-server.md](docs/operational/deploy-on-the-qlik-server.md) —
the service account and the three rights it needs (each of which fails
differently), claiming port 8100 so Windows cannot hand it to anything else, the
scheduled task that brings it back after every reboot, the firewall rule, and the
address people type. Every step says how to prove it worked before you move on,
and it ends in a checklist.

**A start at boot leaves evidence.** Started by Task Scheduler as a service
account there is no console, so everything the launcher printed went nowhere —
and "the task ran and nothing is listening" had nothing behind it. It writes
`logs\startup.log` now: a line per start with the version, folder, account, port,
address and upload ceiling, plus a line for anything that stopped it. Set up
before anything that can fail, and self-trimming so a flapping service cannot
fill the disk.

**A port already in use is a sentence, not a socket error.** Checked before the
app tries to listen: which port, what probably has it, the command that names the
process, and the two ways out. It exits non-zero, so restart-on-failure behaves.

**The address it prints has the machine's own name in it** instead of the
placeholder `<this-vm>`, which was being printed to somebody who had just
installed the thing and did not yet know what the network called the box.

**Documentation caught up with the last three releases.** `design.md` now
describes the third template mode (`mode: document`) — the schema, why it is a
separate engine, the four load-bearing decisions inside it, and the one line that
keeps it out of the dashboards. A new analyst page,
covers the builder end to end including what it still cannot do. The analyst
folder is five pages, not four.

**Two links named screens that no longer exist.** "Open the PDF form builder" and
"Open the report builder" were merged into one screen months earlier; both now
say "Set it up on Add a template", which is where they land.

---

## 1.5.2

**A rotated page can be pointed at.** `pdf_pagesize` reports the page box before
`/Rotate` is applied; the renderer and the text extractor both report after it.
Where they disagreed the builder drew a picture 612 points wide whose own image
was 792 wide, so the page was stretched one way and squashed the other and
nothing anybody drew on it landed where they put it. The words decide now, and
only when they do not fit the box and do fit it on its side. Measured on 81
third-party documents: 11 pages of 212, across 5 documents, every one of them
previously unusable in the builder. A US Senate expenditure report now reads its
six columns from two drags.

**Every column band is tinted.** It used to tint alternate ones, which reads as
"three of these are selected and three are not" while the list says six.

**One column is said out loud.** A drag round "the column names" that lands on a
sentence makes one column and every row then arrives whole in one cell — and a
one-column table cannot have an unclaimed word or a thin column, so it passed
every check and reported "every column filled". Now said at the drag, repeated
on the card, and the clean tick is withheld.

**"How many rows" renamed "Heading rows."** Under a picture of a table it read
as "how many rows has this table" — a question nobody is ever asked here.

---

## 1.5.1

The document builder rebuilt around one idea: **nothing happens until you ask for
it**, and everything the tool works out for itself is on the screen with a control
beside it to change it.

**One armed intent, named across the top**

- A drag used to mean "make me a table", which is also what it meant when you
  were trying to fix one. Now the screen holds exactly one armed intent at a
  time, written across the top in a sentence — *"Drag a box round the table's
  TITLE"*, *"Click the page where the table ENDS"* — and nothing is armed until a
  button armed it. A drag with nothing armed changes nothing and says so.
- The banner and the hint under the picture read the same sentence out of one
  list, so they cannot disagree about what a gesture will do.

**A value is two drags, and the tool remembers which SIDE**

- The label, then the value. Nothing is guessed. What is stored is the label's
  wording plus which side the value sits on — right, left, above or below — and
  on the next document the label is found by its wording and the search runs in
  that direction. A figure printed two digits longer, or a label a word wider,
  still reads; a fixed offset did not.
- The side is shown in words, editable before and after saving, and written into
  the template file where it can be read and changed by hand.

**Everything auto-derived is editable**

- Columns: rename, retype, reposition (drag the width on the page or type the two
  edges — both give way at the neighbours and keep the tiling) and delete.
- Start and end: stated in words with *Show me* and *Move it* beside each, plus
  one press each for *work it out for me*, *the bottom of its page* and *the
  bottom of the last page*.
- Every saved table and every saved value goes back into the same draft to be
  edited, so **Save** always means the same thing.

**The tool's labels no longer cover the words they describe**

- Column names float in a strip above the paper, joined to their columns by a
  hairline. START and END are short filled chips, not captions.
- Five layers — tables, column lines, names, values, start and end — switch off
  independently, the way the X-ray's do.

**Three engine defects, found by running it over 81 PDFs nobody here wrote**

`tools/corpus/` collects a folder of real documents from the test suites of
camelot, pdfplumber, tabula-java and tabula-py, and `run-corpus.R` surveys what
the engine does with each. No document crashed it. Measured before and after:
**40 documents where the proposed row count was not the count the reader
produced → 0**.

- The proposer counted lines; the reader folds wrapped cells and skips printed
  rules. The proposer now asks the reader (N169).
- A one-column table — a block of prose, a list — was ended by the first
  paragraph gap, because the stop rule's floor of two filled columns is
  unreachable in a table one column wide (N170).
- A landscape page scaled into a portrait frame stretches sideways and squashes
  vertically, so every column band on it claimed the wrong words. Three of the 81
  mixed orientations; one lost 133 words out of every column. Such a page is now
  read in its own space (N171). The bottom of the page is the document's own, not
  A4's (N172).

**Just as easy as a bank statement — measured, in a browser**

Driving both paths end to end: a bank statement took **4 decisions** (say what it
is, choose the file, open the toolkit, Save) and a document took **10**, because
the document builder asked by hand for the three things the statement toolkit
fills in for you. It now fills the same three the same way — the issuer guessed
from the filename, the identifying phrases found on the document (a saved
table's own title, which beats its column names), and the save name composed
from the two — and a document takes **7**. The remaining three are the
irreducible ones: *+ Add a table*, and the two drags that say **which** table.

**Generalisability**

- A document template no longer carries `currency: NZD`. Nothing in this mode
  reads it, and a report of rupees should not carry a false statement made by a
  converter that never looks at it.
- The default column kind stays `auto`: the page's own words, uncoerced. A
  declared kind puts the parsed value in `<column>__value` and leaves the raw
  text in the column, so a figure the parser does not recognise is never lost.

---

## 1.5.0

A third kind of document: a **report** — forty pages carrying thirty-odd tables
of completely different shapes, several to a page, some beginning half-way down
one page and finishing half-way down the page after next.

**Why it is a separate engine and not a wider statement parser**

The statement parser reads ONE table whose columns run the full height of every
page, and it judges what it read against a running balance. All three of those
facts are false for a report. Widening it would have put the least checkable case
inside the code path every real conversion runs through, so a report is read by
`R/tables.R` / `R/tables_detect.R` / `R/doc_extract.R` under its own template mode
(`mode: document`), and the statement path is untouched.

**Download only, enforced rather than assumed**

- `write_feed()` refuses `kind = "tables"` outright, beside the same refusal for
  forms. There is no reconciliation behind a report — nothing here can tell a
  right figure from a wrong one — so nothing from one reaches a dashboard.

**Found by its wording, not by its coordinates**

- A table declares a START `(page, y)` and an END `(page, y)` — positions on a
  page, not whole pages — with columns as x-bands meaningful only between them.
  That is what lets several tables share one page.
- It is located by its **heading wording** first, then by **the rows it starts
  with**, and only last by **where it sat on the example**. Which of the three was
  used is reported with every table, and the last two send a conversion to review:
  reading from a stale position on a page where the table has moved does not fail
  loudly, it quietly picks up a title line as if it were data.

**What it does instead of checking, because it cannot check**

- The **fill rate** of every column and every row is measured and reported. A
  column below the table's threshold is flagged, never dropped; a column marked
  `may_be_blank` — the middle cells of a totals row — is exempt.
- **Every word inside a table's boundary that no column claimed** is listed. A
  band drawn four points too narrow loses a whole column of figures and leaves
  every remaining row looking perfect; that list is the only thing standing
  between that and a wrong answer.
- **Overlapping column bands are refused at save time.** A word can land in only
  one column and the earlier band wins, so an overlap does not duplicate a value —
  it deletes it from the second column, invisibly.

**Truncation, which is invisible when it happens**

- A table drawn on a copy that ran to page 5 is **followed** while the pages after
  its declared end keep repeating its header, and the extension is reported. Every
  row that did come out of a truncated read looks right, which is why this cannot
  be left to be noticed.

**The line pitch is learned, not assumed**

- Per table, from the page's own line spacing. A tolerance too large merges two
  rows into one — the silent corruption the whole project exists to prevent — and
  one too small splits a row whose cells sit on different baselines. Capped at 14
  points so a widely-leaded title block cannot swallow the value printed under its
  label.

**Label/value pairs are two boxes, not one**

- What is stored is the label's wording plus the **offset** to the value, so the
  value can sit to the right of its label, under it, or *before* it without any of
  those being a special case. On the next copy the label is found wherever it has
  moved to and the same offset applied; the drawn box is the fallback, and which
  one was used is reported.

**Setting one up is confirming, not drawing**

- **Add a template → "A report"** reads the document and proposes the tables it
  found, drawn on the page with a list down the right to navigate them. Thirty
  tables drawn by hand is a template that never gets finished, and a half-drawn
  one is worse than none: it produces confident output with a column missing.
- A continuation across pages is joined into one table only on a repeated header
  or identical column bands. Interleaving two different tables is the worse of the
  two mistakes.

**Output**

- A workbook with a sheet per table, plus one **long** CSV (`table`,
  `table_name`, `page`, `row`, `column`, `value`) — the only honest single-file
  shape for tables of thirty different widths — plus the fill report. Where
  `openxlsx` is absent, a CSV per table instead: nothing is dropped for want of a
  package.

**ONE builder, and columns you click rather than draw**

The first version of this asked the wrong question first ("a form, or a
report?") and then made the commonest job the hardest one. Rebuilt around what a
person actually does:

- **One document type.** "A bank or card statement" or "anything else". There is
  no longer a form builder and a report builder: that split was the engine's, and
  a person with a PDF in front of her cannot answer it. Inside, two tabs -- Tables
  and Values -- which are about what she is doing at that moment.
- **Columns are DIVIDERS, not boxes.** A table's columns tile its width, so the
  real choice is the N-1 lines between them. Click between two columns to split,
  click on a divider to remove it, click outside the table to widen it. Six clicks
  instead of seven drags -- and a gap or an overlap becomes impossible to draw,
  which retires the whole class of "a column band a few points wrong" by
  construction.
- **The names come from the document.** Split a column and both halves are named
  from the two header cells that were in it. Nobody types a column name unless
  they want a different one.
- **"Work out the columns"** derives them from whatever is inside the table's
  boundary right now -- so the loop is: set the start, set the end, press it.
- **One control says what a click does** (columns / where it starts / where it
  ends / nothing), and it sits under the picture it controls. A drag always means
  one thing on a given tab. Two gestures, never a follow-up question.
- **A value takes ONE drag.** The tool reads the wording beside the box and calls
  the value that, finding the label's own box on the page so the pair still
  travels by wording-plus-offset. A second box is only needed when the label is
  somewhere the tool cannot see.
- **Typed values survive the merge.** A value described only by its wording,
  with no box at all, is read by the label matcher anywhere on the document -- the
  most portable of the three ways, and now expressible in the same template as a
  drawn table.

**Three defects the rebuild turned up**

- **`sp$label` partial-matched `label_text`.** R's `$` partial-matches on lists,
  so a value described by wording alone returned a character vector where a box
  was expected and took the WHOLE extraction down with "$ operator is invalid for
  atomic vectors". Every typed value is exactly that shape. Spec fields are now
  read by exact name.
- **The Convert page's link to the builder selected a document type that no
  longer existed**, so it arrived showing the statement toolkit instead.
- **`col = "#666"`** in the "this page could not be drawn" fallback:
  `col2rgb("#666")` is an error, so the branch that exists to survive an
  un-renderable page would itself have crashed.

**What sixteen awkward documents found**

The shapes above were built as fixtures (`tests/testthat/helper-doc-hard.R`) and
run against the engine. Nine of them broke it. Every one of these was measured,
not reasoned about:

- **A table whose row heights alternate lost half its rows.** 10pt and 26pt
  rows, a median gap of 26, and every 10pt gap swallowed: six rows came back as
  four with `R2 R3` in one cell and `20.00 30.00` in another. The row tolerance
  now follows the **lower quartile** of the gaps -- the tightest spacing the table
  actually uses -- so it is safe for every row rather than for the average one.
- **A table followed onto the next page swallowed the two tables under it**: 93
  rows instead of 83, ten of them with an amount in the wrong column and a date
  that was not a date. An open-ended window now stops where the rows stop: a gap
  much bigger than the ones this table has been using AND a line that fills fewer
  of its columns. Both halves are needed -- the gap alone cuts a schedule at the
  whitespace before its own total.
- **Two different schedules with the same header became one table** of twelve
  rows under the first one's name. A page that carries its **own heading** is a
  new table; a real continuation opens with the repeated column names and nothing
  above them.
- **A header repeating as "... (continued)" split an 18-row schedule into two.**
  A repeated header now matches on prefix -- while still refusing the same column
  names in a different order, which is a different table sharing a vocabulary.
- **A bordered table proposed no columns at all.** `+----+----+` rules span the
  full width and merge every band into one. Printed rules are now transparent:
  they neither start a run, nor end one, nor contribute a band, nor become a row.
- **A condensed table read as one cell per row.** 2pt gutters with a `|` in them
  -- narrower than any word space. A lone vertical bar is a border, dropped where
  both the proposer and the reader come through, so the columns separate on real
  whitespace and no cell comes out as `| 1,240.55`.
- **A total set apart by an underline and 45pt of white was left out** -- the one
  row somebody came for. A run now reaches down past a break for up to three lines
  that still land in at least two of its own bands.
- **A two-column table was not a table.** A valuation of item and amount, five
  rows, proposed as nothing. Two columns are allowed on a longer run: four
  consecutive rows in the same two bands is a schedule, not a coincidence.
- **A description that wrapped ended the table it was in.** Once the tighter
  tolerance correctly made the wrap its own line, the one-cell line broke the run.
  A wrap -- indented past where the rows start, within a line of the row above,
  filling one column -- is folded into that row instead.

**Still limitations, and asserted as tests so they are decisions**

- Two tables printed side by side read as one of six columns. The ink is
  identical to a genuine six-column table; it is drawn as one on the builder,
  which is where it gets split into two.
- A spanning header cell ("Balance" over "Opening"/"Closing") is offered as the
  table's name. Wrong, harmless, one text box fixes it.
- A table with no figures anywhere keeps its heading line as a row: with no font
  information there is no header signal, and the rule fails toward keeping a row
  rather than deleting one.

**Paths**

- `doc_templates/` (curated) and `doc_templates_user/` (built in the app), settable
  as `paths.docs` / `paths.user_docs`. A settings file written before this release
  still starts.

---

## 1.4.0

Two rounds in one release: the one that let a squad use the tool at the same
time, and the one that read the result back and found what it had cost.

**Why the version moved at all**

- `N120` — two run records existed for one statement carrying the **same**
  `engine_version`, the **same** `template_sha256` and **opposite verdicts**
  (`needs_review` / low, then `ok` / medium). Behaviour had changed inside a
  release while the stamp stood still. The incident procedure's headline test is
  *same version + same template hash, so any difference is a finding* — so it had
  begun crying wolf on exactly the statements the previous round had fixed. From
  1.4.0 the stamp moves with every shipped change of behaviour, and
  [`docs/operational/investigating-a-wrong-conversion.md`](docs/operational/investigating-a-wrong-conversion.md)
  §4 now carries the caveat for every record written before it, with the real
  pair on this box as the worked example.

**Five to ten analysts can convert at once**

- Every conversion runs in its **own short-lived R process** (`R/jobs.R`), and the
  app polls for the result. Before: R is single-threaded, so one scanned statement
  froze a second analyst's browser for **65 seconds** with 13 consecutive probes
  unanswered. After: the same scan, the same two browsers, and the second page
  answers in **0.06–0.30s** throughout while a colleague's CSV comes back. No new
  dependency — `run.R` was already a tested command-line entry point, and loading
  the whole engine costs 0.27s, so a worker pool would have bought ~7% on the
  cheapest conversion and cost a dependency.
- **A cap and a queue that says so.** Ten OCR jobs on a four-core box helps
  nobody, so the cap is one under the machine's cores (three here), settable per
  site with `app.max_concurrent_jobs`. It caps **threads** as well as processes,
  because `tesseract` is OpenMP-parallel and takes a thread per core by default:
  one scan alone reads in 36 seconds, and three uncapped had not finished a single
  page after ten minutes. Anyone past the cap is told where they are — *"3
  conversions ahead of yours - yours starts as soon as one finishes"* — and the
  number falls as the queue drains. Waiting is fine; waiting without being told is
  what this removes.

**What running in a child process broke, found by driving it**

- `N134` — **a wrong figure's close cousin.** The child inherited the *service's*
  locale rather than the app's, so a description with an accented character came
  back mangled **in the delivered CSV** (`Caf<U+00E9> Kr<U+00F6>ne` where an
  in-process run rendered `Cafe Krone` correctly); byte comparison put the first
  difference at position 186. It would only ever have shown up on a real
  customer's statement.
- Reaping a job killed the R child and left **`tesseract` orphaned** — three of
  them, a core each, still running 14 minutes after the tabs closed. Jobs now kill
  the whole process tree.
- `N131` — a killed conversion reported the child's **locale string** as its
  reason: `convert job job00023 (stopped): [1] "C.UTF-8"`. The maintainer's crash
  log now carries the reason.
- `N117` — the concurrency work **disarmed the only guard on the batch seam**, and
  it failed in the way guards fail worst: `test-batch.R` grepped `app.R` for one
  exact call spelling, the call moved into the child, and the guard *errored* on
  an empty match rather than failing. It now asks the invariant of every caller
  wherever the call lives, so moving code cannot silently disarm it again.

**Wrong figures, and a wrong figure that had already been published**

- `N118` — **a backwards statement period reached the governed feed as accepted.**
  On an auto-split bundle the merged header took the first segment's start and the
  last segment's end in *file* order; on a bundle printed newest-first that is
  "20 Apr 2026 to 19 Feb 2026", a period ending two months before it starts,
  stamped on every row of a court-facing extract. No check saw it: the checks run
  per statement against each statement's own correct period, and the merged header
  is assembled afterwards. Sorting by date was only half of it — a span is a claim
  about *coverage*, and min-start/max-end would have silently asserted two days
  this very file does not have. Gaps and overlaps now publish **no** period and
  say why, naming the uncovered dates, and a backwards period is refused at the
  point of construction rather than reported.
- `N121` — **a non-ASCII literal that ate bytes.** An en dash inside a regex
  character class in `R/labels.R`: under `LC_ALL=C` — the stated deployment
  locale, and the one the command-line entry point the incident procedure uses ran
  in — a labelled value carrying a currency symbol came back a byte short, and no
  longer verbatim. The byte scan that had covered the three UI files now covers
  `R/`, `run.R` and the test runner as well, and `run.R` and `tests/run_tests.R`
  gained the locale guard `app.R` already had.

**The screen says what it is, not what the engine calls it**

- `N127` — Field coverage said *not on this statement* about columns the statement
  **prints**: westpac.pdf heads three of them, and its template deliberately folds
  all three into one description band. That verdict reads the *template*, so it
  was making a claim about the file it is in no position to make. It now says
  *not read as its own column*, which is true either way.
- `N126` — Admin's Uploads table lists **every** upload, newest first, which is
  what the incident procedure sends a maintainer there for. It was headed *"new
  formats to pick up"* over help text saying the tool couldn't read them — so at
  step 1, hunting a successful conversion, the screen told him not to look in the
  one table that had it.
- `N123` — a raw template id appeared **twice** on the toolkit's save
  confirmation, on the screen a non-technical analyst uses to add a bank, while
  the card two inches away correctly gave the name.
- `N124` — on a run that never finished, the **Diagnostics** heading still sat
  over blank space while Checks and Field coverage had learned to say why they
  were empty. Its empty state has a different cause and now has its own sentence.

**The pages, re-read against the screen that exists**

- `N125` — **the first navigation instruction a new analyst gets was wrong.** The
  previous round moved Checks, Diagnostics, Field coverage and the dashboard line
  out from behind *Show me how it read this*; three sentences still sent her
  there, including the one on the page her own index tells her to start with. All
  three now describe where each surface really is, and a new guard reads `app.R`
  itself for what is behind that link, so the pages cannot drift from it again.
- `N128`, `N129`, `N130`, `N132` — `design.md` sent you to the wrong file for the
  step it flags as the one that catches people out; it also still said a non-ASCII
  string literal was fine, which had stopped being true. A page listed six kinds
  of *info* row and then referred back to five. And the maintainer's runbook
  recorded a build baseline this tree cannot produce, on the page that tells him a
  wrong number means stop.

**Suite:** re-measured on the tree this release was cut from and recorded, with
its date, in
[`docs/operational/maintaining-the-engine.md`](docs/operational/maintaining-the-engine.md).
The pass condition does not move and is not a total: *nothing failed, nothing
errored, nothing was skipped*. Register totals likewise live in
[`docs/context/findings-register.md`](docs/context/findings-register.md), which
states its own.

## 1.3.0

The release where the pages about the tool were held to the same standard as the
figures in it — and where the toolkit stopped asking for things it can work out
for itself.

**The tool answers more of it**

- The toolkit reads the bank's own name off the statement — the masthead first,
  then the legal imprint — instead of the first bank word anywhere on the page.
  That had answered "ANZ" on an ASB statement, because ANZ was one of the payees.
  A bank nobody has taught it now gets its own name too.
- When the wrong template read a statement, the way to correct it is on the
  result above the fold, not behind the evidence toggle. A statement read end to
  end by the wrong template looks perfect on screen; it is the failure this tool
  exists to prevent.

**Nothing fails quietly — including the front door**

- `convert_statement()` now wraps everything that touches the file path. Handing
  it something that is not a filename comes back as a status with a reason. Two
  documents had claimed "it never crashes" before it was true of the whole
  function.
- Admin controls that changed something on disk and said nothing now say so, and
  a table with nothing in it says so rather than instructing the reader to use a
  picker with no options in it.

**The documentation became part of the product**

- Three read-through pages — what it is, how it is built, how it got this shape —
  are linked from the README and inside the guards, instead of sitting where the
  orphan test could not see them.
- Both doc guards were widened: no page can be orphaned from an index, and a path
  named anywhere in the tree — a document, an engine comment, a README beside the
  templates — has to resolve to a file that exists. Each caught a real break
  before it was fixed.
- Every claim in those pages was read back against the running code, and the ones
  that had stopped being true were corrected — a fourth result word the docs never
  mentioned, a count of checks that was one short, one screen with four names, and
  measurements nobody had re-taken.

**Two things a reader could not do, and now can**

- The analyst's guides describe the screen she actually has: the first thing the
  app asks her for, the four words a check can come back with, and the column that
  tells her whose problem it is. "Teach it a new bank" now follows the toolkit's
  own numbered steps and starts where the work really starts — reading the preview.
- The maintainer has a procedure for *somebody says a conversion is wrong*: run
  id, original file, the same figure reproduced from the command line, template
  bug or bad scan. And a design document that says how to run it, how to ship a
  change to it, and how to put it back.

**The pages were measured against the running app, not against each other**

- `N107` — the folder an accountant was handed, `docs/for-analysts/`, did not
  exist; her pages sat in a directory named for operators. That folder now
  exists, as the short index of her four pages, and the README and the
  operational index both route to it. A new guard fails the suite when any
  `docs/` path named in prose — **including a folder, which has no `.md` on the
  end and which every earlier scan therefore skipped** — points at nothing.
- A second new guard loads **every fenced settings block in the documentation
  through the real config loader** and fails if the Admin tab would open. A
  printed admin password is a password everybody has; the shipped placeholder is
  safe only because the app treats it as "nobody has set one yet", and a
  near-miss like `change-me` is a working password that reads like a placeholder.
- `design.md` §8 said adding a check makes "existing tests" fail and pointed at
  three. Re-run and counted: one ordinary new check took the suite to **27 failed
  and 1 error, in 17 tests across 10 files**, and the runner's reporter shows the
  first ten and says so in a line that is easy to miss. The section now carries
  the real number, the command that shows all of it, and the order to read it in
  — the six bank goldens first, because they are the only failures that are
  evidence about the check. Its recipe also gained the step that was missing: a
  new diagnostic category must be **raised**, not just declared, or the suite
  fails it as a dead row.
- `when-something-goes-wrong.md` was re-checked against the Diagnostics table as
  it renders. It had told the accountant that an *info* row's "How to fix" says
  *No action needed* — false for three of the five, including the one the page
  used as its example, whose sentence asks her to review per account when several
  accounts are mixed. It had also filed *"check those pages against the source
  PDF"* under "ask the supplier for a cleaner scan"; that sentence belongs to the
  one diagnostic that means **do not release this output**. The page now lists
  every phrase the *What* column can print, against what it means for her.
- `adding-a-bank-template.md` now describes what the drafter really does with
  currency: every name found on the figures goes into one pile and is saved only
  if there is exactly one, so a code does *not* outrank a symbol that disagrees
  with it. Six codes and four symbols are recognised, the mark has to sit before
  a figure printed with cents, and **NZD in that box is usually a fallback nobody
  checked** — which looks identical to a reading.
- A doc said `scripts/run_app.R` "prints the address". It prints the literal
  placeholder `http://<this-vm>:8100`, which nothing resolves. Reworded, in both
  places that said it, with what the console really shows.
- The root pages were re-read against the code: `R/jobs.R` runs every conversion
  in its own child process, so "no lock, no queue, nothing to tune" is no longer
  true — there is a queue, it is capped in threads as well as processes, and
  anyone past the cap is told they are queuing. The stale findings tally and an
  un-retaken speed measurement were replaced with the register's own figure and a
  fresh one.

**Suite:** grew again this release (725 tests at 1.2.0, 792 partway through this
one). The live totals are not repeated here, because 1.3.0 is still open and a
number frozen mid-release reads as a promise about today; the last full run is
recorded in
[`docs/operational/maintaining-the-engine.md`](docs/operational/maintaining-the-engine.md),
and the pass condition that does not move is *nothing failed, nothing errored,
nothing was skipped*. Register totals likewise live in
[`docs/context/findings-register.md`](docs/context/findings-register.md), which
states its own.

## 1.2.0

Eighteen findings, all but two raised from real use on real statements. Five put
wrong figures on screen; two of those were regressions introduced inside this
release and caught by reviewing it.

**Wrong figures**

- `N37` — a `CR` marker printed on the line below its amount was folded into the
  payee, so every credit-card payment and refund came out with the charge sign.
  The marker is now appended to the money cell it sits under.
- `N30` — a printed opening or closing balance could be kept as a transaction, at
  the balance's own material amount. The summary-line guard now reads both the
  description cell and the whole line, always.
- The first fix for `N30` then ate real transactions whose payee began with a
  month abbreviation (`MAYFAIR CLOSING BALANCE`). Narrowed.
- `N38` — a stray date range on the page could move the inferred year on every
  transaction. A span is no longer claimed unless it was actually found.
- Balances could be paired across two different dormant accounts in one bundle.

**The toolkit**

- `N29` — a duplicate output id aborted Shiny's bind pass for the whole modal, so
  on a PDF not one control the toolkit drew statically was live. Fixed, and the
  id collision is now a test.
- `N34`, `N35` — the preview says how much it read, and the first question the
  builder asks is one the person in front of it can answer.
- `N28` — the escape hatch is reachable from the screens that need it.
- A correction to a shipped template is saved as `<id>_custom` carrying a
  `refines:` line, and detection honours it. Before this, a correction tied with
  the template it was correcting and lost.

**Cases, bundles and periods**

- Batch conversion: select a whole case folder, one row per file, click a row for
  that file's full result.
- Multi-period reconciliation, and `N39`/`N40`: auto-split is no longer opt-in
  (one of thirteen templates ever opted in), and an unfinished seed template can
  no longer join detection.
- `N33` — a page footer is no longer folded into the transaction above it.

**Wording and screen**

- Every check label now claims only what its check proves. "Every row was read"
  became "No row failed to read"; "Row count matches the statement" became "Row
  count"; "Redactions found and honoured" became "Redactions found".
- A check that could not run says "could not be checked" rather than borrowing
  the field-coverage wording "not on this statement", which meant something else
  on the same screen.
- A tie between two templates converts and holds for review instead of demanding
  a pick from a screen that had no picker on it.
- Admin's `user_template_ids` reactive shadowed the engine function of the same
  name, so the template origin line printed an R error and Hide and Delete did
  nothing, silently. Removed.

**For the maintainer**

- Admin refuses to open at all while the admin password is the shipped
  placeholder.
- A safe, shapes-only audit any statement or folder can be described with, and
  the no-PII AI survey prompt
  ([`docs/operational/survey-a-statement-with-ai.md`](docs/operational/survey-a-statement-with-ai.md))
  that produces the same description without the file leaving the room.
- The docs were culled: the point-in-time audits, the research briefings and the
  superseded plans are gone from `docs/`, and git history keeps them. Two new
  tests fail the suite if a doc link dangles or a page is orphaned from its index.

**Suite:** 725 tests, 3,366 passing assertions · 0 errors · 0 skipped (from 2,880
at 1.1.0). Register: 109 findings, 104 fixed, 4 open, 1 not-a-defect.

**Known and deliberate**

- `app.R` is ~4,600 lines and not yet split. What makes it hard is ~3,800 lines of
  interleaved reactives in one scope; a de-risked plan is in the register,
  including the test-helper change that must go first (60 tests grep `app.R` by
  physical line offset).
- Two older findings wait on **evidence, not effort**: a spread of real forms to
  tune the form fingerprint against, and one hand-keyed golden scan to measure OCR
  digit accuracy. Both fail closed and loudly today.
- Two mechanisms answer "one file, several periods" and both are needed — the
  bundle split (several separately-issued documents) and the span merge (one
  document, several sections). The register records why deleting either is wrong.

## 1.1.0

**Correctness**

- A tie between two templates was broken by template id, alphabetically, so a
  hand-built `aaa_bank` beat a tested `westpac_everyday_pdf` on nothing but its
  name. Shipped templates now win an equal score.
- `%y` / `%Y` year truncation could read a four-digit-year export into 2020.
- `type_dc` sign inversion in drafted templates: a bank marking debits `DR` or
  `Debit` had every debit read as a credit.
- Delimited and Excel statement metadata is threaded into the header, so the
  balance, count and period checks can run on those formats at all.
- A re-convert that flips accepted → withheld now withdraws the previously
  accepted rows from the feed.

**Privacy and governance**

- Destructive Admin actions are authorised server-side; visibility is not
  authorisation in Shiny.
- Local metadata capture: levels, per-category switches, account numbers stored
  only as a one-way hash, never in the Qlik feed.

**Suite:** 2,880 passing · 0 failed · 0 skipped.

## 1.0.0

First production build: the engine, the shipped templates, the governed Qlik
feed, the audit trail and the offline installer, as described in
[`docs/context/charter.md`](docs/context/charter.md).
