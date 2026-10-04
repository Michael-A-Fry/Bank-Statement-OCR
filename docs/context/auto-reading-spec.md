# Automatic reading: the specification

**Status: APPROVED and BUILT, released as 2.0.0 (3 Oct 2026).** The decisions in
section 2 are the product owner's and are binding. Built as specified, with these
exceptions, each listed with its reason in the 2.0.0 entry of `CHANGELOG.md` and
in `outstanding-work.md`: Admin -> Banks has confirm, rename and retire, but not
merge or move; Please check fixes a column with a role dropdown and Re-read, but
does not yet split or join a column by clicking a gap; reading another account's
mini-statement as a separate account (decided 3 Oct) is not built yet. The
held-back acceptance run (section 11, step 3) has not been done. Section 9.1 is
the pre-build baseline; the 2.0.0 figures are in `CHANGELOG.md`. Appendix A was
the contract the parts were built against; the interfaces as built are in
`architecture/build-contract.md`.

Written 3 Oct 2026 from the product owner's decisions, three research reports
(how existing tools find tables; bank identity; engineering practice for
self-learning tools), a root-cause analysis of today's column finder, and a
baseline measurement of today's tool on 128 realistic test statements.

---

## 1. What we are building, in one paragraph

You pick a **bank** (the tool pre-fills it from the statement itself) and drop
in statements. The tool reads each one from its **content**: what is a date,
what is a figure, what is words, what lines up with what. It then **proves**
the reading with the statement's own arithmetic (running balance; opening plus
movements equals closing; printed totals). A reading that proves out converts
with no clicks. A reading it cannot prove is shown to a person with the reason
and a click-to-fix. Every bank's distinct layouts are **learned automatically**
from the statements it has read and proved, so the next statement of that
layout is quicker and surer. Templates as you know them today are retired.

## 2. Decisions already made (product owner, 3 Oct 2026)

| Decision | What it means |
|---|---|
| Target | ~95% of statements processed with no template pick and no adjustment (100% ideal), and **zero silently wrong**. |
| Bank, not template | Upload picks a bank. Each bank holds N layouts the tool has learned. |
| Bank pre-filled | From the account number (bank + branch, via the official register) and the masthead/legal name. When those agree it goes ahead; it asks only when evidence is missing or conflicting. You can always override. |
| Training | Upload ALL statements you have for a bank; the tool works out its layouts. More can be added any time. A new statement that doesn't fit is put through the same process automatically. |
| New layouts | Provisional until proven (3 statements that prove themselves, or an admin confirm). Statements that prove themselves are converted straight away even while the layout is provisional. |
| Learning | Automatic; admin can undo anything learned. |
| Existing templates | Start fresh: retired. Training rebuilds each bank's layouts. |
| Scope | Everyday/savings PDFs, credit cards, scans, CSV/Excel. |
| Tracking | No personal data. Readable on an Admin page and as a carry-off summary. |
| Acceptance ("green flag") | 100 deliberately weird synthetic statements with made-up headings in odd places, 3-5 wrapping text columns, dates on either side, with and without balance, every money format. The reader must use page content, not heading words. |
| Switch-over | Today's templates are removed at once: the new reader is the only path from day one, with no shadow period on the server. So the gate must be passed on the test sets BEFORE release, and tracking plus spot checks run from the first day on the server. |
| No balance, no totals | Once its layout is proven, such a statement may convert automatically, counted as its own class and spot-checked more heavily. |
| Scans | Same pass mark as everything else: 95%. |
| Derived amounts | An unreadable or removed amount that the running balance fixes is filled, marked derived, and the statement goes to Please check. |
| Analyst fixes | A fix on Please check is learned straight away when the corrected reading then proves out; an unprovable fix applies to that file only until an admin confirms it. |
| Old editor | The "Add a template" builder and guided setup are removed. The drag-the-boxes column editor stays, reachable only from Please check, as the last resort. |
| Spot checks | Built, but off by default; an admin can turn them on and set the rate. |
| Qlik feed | Not part of this change. It keeps working as a downstream export; the one forced change is its gate: "curated template" no longer exists, so a statement feeds Qlik when it was proven or a person confirmed it. |
| Other accounts on the page | A statement pack can print another account's own transactions (a linked savings or term-deposit mini-statement). Each such table is read as a SEPARATE account: labelled with its account, proven by its own balance, never mixed into the main account. Schedules, pending items, rate and fee tables are not transactions and are ignored. (Product owner, 3 Oct 2026.) |
| Heading vs arithmetic | Where a heading says one thing and the arithmetic proves another (card and loan accounts run backwards), the arithmetic wins and a note is recorded. (Default; the product owner can overrule.) |

## 3. Why today's tool fails (root cause, measured)

Today's column finder (`suggest_pdf_columns`) is all-or-nothing. Measured on the
shipped ANZ sample:

* **One "USD 25.00" inside one description** made it find a fourth "money
  column" that no heading named, so it gave up, and the builder drew **fixed
  placeholder boxes**: a date box over the left margin (0-95pt, where nothing is
  printed) and one "amount" box straddling two columns. Those were shown as if
  they had been found. This is the "date column as empty space" and "boxes
  between pieces of text" you saw.
* Dates printed as 2026-02-03 drafted nothing at all.
* A bare "12" counted as a date; right-aligned figures were grouped by their
  centres; only one page was measured; boundaries went at midpoints; which
  column is withdrawals was taken from heading words or position and never
  checked against the arithmetic.

## 4. The reading pipeline

Geometry **proposes**; arithmetic **decides**. Each step is deterministic and
records what it did.

1. **Normalise.** Apply page rotation; deskew scans (the existing projection
   method, accurate to 0.05 deg) and measure any skew left from the slope of a
   figure column's right edge. One coordinate frame per page.
2. **Type every token.** DATE (with its possible formats), MONEY (sign style,
   CR/DR/OD, currency, decimal point or comma), NUMBER that is not money (units,
   prices, rates, references), MARKER, TEXT. Formats are decided by voting over
   the whole document (day/month order, decimal style, year printed or not). A
   bare number is never a date.
3. **Lines and cells, measured in the page's own type size**, not fixed points:
   lines by vertical overlap; cells split at gaps wider than the page's typical
   word space. Near-threshold gaps are kept as alternatives for step 9.
4. **Body rows.** A body row is a line with a figure in a figure column. A date
   is NOT required (dates printed once per day are carried down). Lines that run
   across columns (headings, summary bands, footers) are set aside before
   columns are measured.
5. **One column model for the whole document.** Figures are grouped by their
   RIGHT edge across all pages (after estimating each page's shift); text and
   dates by their LEFT edge. A column needs at least 3 aligned members, so one
   stray figure in a description can never become a column. Boundaries go in
   white space that runs down every body row, so a boundary can never cut a
   word. Every page is matched to the model; **no page is ever set aside**.
6. **Assemble rows.** Wrapped lines join the row above. Opening / brought
   forward / carried forward / closing lines are **chain anchors, not
   transactions**. Totals, subtotals and footers are classified, not dropped
   silently.
7. **Roles by arithmetic.** Every assignment of roles (money out, money in,
   signed amount, balance, other), every sign convention, and both orders
   (oldest or newest first) is tried. A reading is **proven** only when
   **every** balance step holds to the cent **and no other assignment does**.
   Without a running balance: opening + movements = closing, plus any printed
   totals. Description wording (SALARY = in, EFTPOS = out) is used as a **vote**
   where nothing proves the roles, never as proof. Heading words are a vote too.
8. **Hard checks, all or nothing**, before anything is called automatic:
   every balance step holds, and the chain continues across pages; printed
   opening + movements = printed closing; every page with transaction-shaped
   lines contributed rows; dates readable, in order, inside the statement
   period; every word in the table ends up in exactly one cell or one
   classified line; no sign left ambiguous; the reading that passes is the only
   one that passes.
9. **Repair (self-heal), bounded and in a fixed order:** other cell splits; a
   figure as its own cell vs part of the description; other date and sign
   conventions; other page shifts; the bank's other learned layouts; re-OCR of
   the failing rows on a scan. Where the balance chain breaks points at which
   row to look at. A repair is accepted only if it passes every check **and is
   the only candidate that does**. A figure is never "corrected": a value
   recovered from the balance is always marked derived.
10. **Match to the bank's layouts.** A known layout supplies what the page
    cannot prove (which way a card's signs run, which extra columns to keep)
    and is the first candidate next time, but it is re-checked every time: a
    layout is a hypothesis, never the truth.
11. **Ask.** If nothing proves, the statement goes to "please check" with the
    reason in a sentence and a click-to-fix (section 7).

Every figure carries where it came from (page, position) and how it was proven
(a one-row balance step, a step covering several rows, totals only, a person,
or not proven).

### Outcomes, per statement

| Outcome | Meaning | What the person does |
|---|---|---|
| **Proven** | Every check passed, unique reading | Nothing |
| **Matches a proven layout** | No running balance, but totals check (where printed) and it matches a layout already proven or confirmed | Nothing (counted separately) |
| **Please check** | Read, but not proven (e.g. no balance and no totals, a new layout, two readings both fit) | One look; confirm or click-to-fix |
| **Couldn't read** | A check failed and no repair passed | Reason shown; click-to-fix or set aside |

## 5. Bank identity

From the research (the official Payments NZ bank branch register, 3,295
branches under 28 codes, bundled with the tool and refreshed by an admin):

* **The account holder's own account number decides**, via bank + branch in the
  register; the check digits confirm it is a real NZ account number. The bank
  code alone does not identify the bank (02 includes the Co-operative Bank,
  03 includes SBS, Heartland, Rabobank and NZCU; old codes 06/11/25 are ANZ).
* Payee account numbers and bank names inside the transactions are ignored:
  ANZ, ASB, Westpac and TSB all appear as payees.
* Next strongest: the legal entity name in the footer; then website, 0800
  number, SWIFT code; then masthead brand words. Card prefixes are weak.
* Bundles are identified statement by statement.
* Only the bank code and institution are kept; the account number is not.
* If the document clearly names a different bank from the pick, nothing is
  learned until a person confirms which is right.

## 6. Banks, layouts and learning

**A layout** is the tool's memory of one bank design: kind (PDF, scan, CSV,
Excel), column roles in order, heading words seen, date style, money style,
how often the balance is printed, rows per transaction, fonts and PDF producer,
relative column positions, extra columns. Absolute positions are deliberately
NOT part of its identity, so a shifted copy is the same layout.

**Learning rules:**

* It learns only from statements that are **proven**, or that a person
  **confirmed or corrected**.
* A new design becomes a **provisional** layout; it is **proven** after 3
  proven statements (from at least 2 different accounts) or an admin confirm.
* Heading wordings it did not know are added to the layout (and the bank's
  vocabulary) when the arithmetic proved which column they sit over.
* Learned state is versioned and never edited in place. **Every output is
  stamped** with the engine version and the learned-state version that produced
  it, so any past conversion can be re-run and gives the same answer.
* An admin can undo any learned item; outputs already issued never change.

**Training a bank:** Admin -> Banks -> pick or create the bank -> upload
everything -> it runs in the background -> report: "ANZ: 7 layouts from 212
statements, 205 proven, 7 need a look", with each one's reason.

## 7. Screens

* **Convert:** drop files; the table shows each file, its bank (pre-filled,
  changeable), its layout (learned name) and its outcome. Proven rows need
  nothing. Click a row to see its result.
* **Please check:** the page with the found columns drawn and a balance tick
  per page. One click confirms. To fix, click a column's figures or its heading
  and say what it is ("money out"); click a gap to split or join. It re-reads
  instantly and shows whether the arithmetic now proves. The fix is learned.
  Dragging boxes remains only as a last resort.
* **Admin -> Banks:** each bank's layouts (provisional / proven / retired), how
  many statements each has read, rename / merge / move / forget.
* **Admin -> Automatic reading:** the tracking summary and the gate status.

## 8. Tracking (no personal data)

Per statement: engine version, learned-state version, bank code (never the
account number), layout id, outcome, which checks failed, which repairs were
tried, how it was proven, pages, rows, time taken. When a person corrects a
reading: which columns changed role and how far their boxes moved (points).
Spot-check results. Never: names, descriptions, amounts, dates, account
numbers, file names. Readable on the Admin page; a carry-off summary holds
counts only.

## 9. Measurement and the gate

**Test sets (all synthetic, all with an answer key):**

| Set | Size | Purpose |
|---|---|---|
| Realistic, dev | 128 PDFs + 15 scans + 12 exports, 8 banks x 40 layouts | Build against |
| Realistic, holdout | same size, different positions/fonts/wordings | Scored **once**, at the end |
| Green flag | 100 weird statements, ~10 undecidable | The acceptance test |
| Existing adversarial corpus + offset sweep | 43 + 26 | Regression |

**Baseline: today's tool on the realistic dev set** -- see section 9.1.

**Pass marks (to approve):**

* Green flag: every decidable statement **Proven** or correctly read; every
  undecidable one **Please check**; **none confidently wrong**.
* Realistic holdout: >= 95% of text PDFs, scans and exports (each class
  measured on its own) processed automatically and correct; **zero
  automatic-and-wrong**.
* Metamorphic tests: shifting a column, adding a stray figure, dropping a page
  or deleting a row must give either the same correct output or a flag, never a
  quiet wrong answer.
* On your server: no shadow period (templates are removed at once), so
  tracking and spot checks of a random sample run from day one. Zero errors in
  n checks only shows the error rate is below 3/n, so about 300 clean spot
  checks are needed to say "under 1%", and about 500 statements to say "at
  least 95% automatic" with confidence.

### 9.1 Baseline

Today's tool on the realistic dev set (128 text PDFs, 7 CSV, 5 Excel), every
figure scored against the answer key:

| Today's route | Text PDFs read perfectly | CSV / Excel | Came back "ok" but wrong |
|---|---|---|---|
| Shipped templates, auto-detect | 0 of 128 | 0 / 0 | 0 |
| Auto-drafted template per file, untouched | 32 of 128 (25%) | 0 / 0 | **3** |

The auto-drafted route got 1,408 of 3,886 figures right and added 1,413 wrong
or extra ones. Scans were not scored: today's OCR route stalled for over 20
minutes on a single scanned page.

## 10. What changes in the code

**Kept** (already tested hard): PDF reading and OCR, date and money parsing,
row assembly in the table reader (continuations, split rows, summary lines,
year resolution, derived amounts), reconciliation checks, outputs, run log,
background jobs.

**New:**

* token typing and the document column model (steps 2-5);
* the role/arithmetic prover and the hard checks (steps 7-8);
* the repair search (step 9);
* bank identity, with the bundled branch register;
* the bank/layout store and the learning rules;
* tracking and its Admin page;
* the Please-check screen with click-to-fix; the Banks admin page.

**Changed:** the table reader takes a column model per page instead of one
fixed set of boxes; the convert screen and batch use bank instead of template;
CSV/Excel files join the same bank/layout model (their headings map columns,
the arithmetic proves them).

**Retired:** statement templates and template detection for statements; the
template builder's column drafting; the shipped statement templates (kept only
as test material if useful). Most tests built around templates are rewritten.

## 11. Build plan

1. **Spec approved** (this document).
2. **Build**, as one workflow: reader core (steps 2-9) first, measured against
   the dev set after every change; then bank identity; then layouts and
   learning; then tracking; then screens. Each part has its own tests.
3. **Independent check:** the holdout and the green flag scored once; an agent
   tries to break it (metamorphic tests, missing pages, swapped columns).
4. **Your server:** release (templates removed), train on each bank, tracking
   and spot checks from day one.

## 12. Still needed from the product owner

* The ANZ variant names, roughly how many statements per bank, whether account
  numbers are usually visible, and one or two of the bad cases seen in use.
  They make the test sets look like the real ones.
* Approval of this specification, which starts the build.

---

## Appendix A. Internal contracts (for the build)

These are the shapes the parts agree on, so they can be built in parallel. A
template list in today's schema stays the in-memory form of a layout: the
table reader, reconciliation and outputs keep working on it unchanged.

### A1. The reader -- `R/auto_read.R`

`auto_read(input, layouts = list(), bank = NULL, opts = list())` -> a *reading*:

| Field | Meaning |
|---|---|
| `outcome` | `"proven"`, `"layout_match"`, `"check"` or `"unread"` (section 4) |
| `why` | one plain sentence a person reads |
| `template` | the candidate that produced the result, as a template list (format pdf / delimited / excel), with `table$columns` and, for PDFs, `table$columns_by_page` (one column list per page, NULL for pages with no table). Carries `signature` (A3). |
| `parsed`, `recon` | `parse_statement()` and `reconcile()` output for that candidate |
| `transactions` | `parsed$transactions` (columns include `date` ISO and signed `amount`) |
| `proof` | `kind` (`chain`, `totals`, `layout`, `none`), `links`, `held`, `unique`, `pages_with_rows`, `pages_used`, `derived` (count of amounts filled from the balance) |
| `checks` | data.frame(check, ok, why): every hard check of step 8 |
| `candidates` | data.frame(source, passed, why): `content`, `layout:<id>@<version>`, `repair:<step>` |
| `columns` | data.frame(page, field, kind, x_min, x_max, ink_min, ink_max, heading): what was found, for the screens and tracking |
| `matched_layout` | `"<id>@<version>"` of the bank layout the reading matched, or NULL |

`parse_pdf_table()` gains one thing: when `template$table$columns_by_page[[p]]`
exists it is used for page p instead of `table$columns`. Nothing else about the
reader changes.

Rules: deterministic; never throws (an error becomes `unread` with the reason);
"proven" only when every check in step 8 passes and the reading is unique;
derived amounts force `check`; a page with transaction-shaped lines that
contributed no rows forces `check`.

### A2. Bank identity -- `R/bank_identity.R`

`bank_identify(input)` -> `list(institution, bank_code, confidence = high |
medium | low | unknown, why, evidence = data.frame(kind, institution, strength,
zone))`. Never returns or stores an account number. Data shipped with the tool:
`dictionaries/nz_bank_branches.csv` (bank_code, branch, institution) from the
Payments NZ register, and `dictionaries/nz_banks.yaml` (institution, display name, legal names
current and former, domains, phone numbers, SWIFT, brand words, agency family).
`bank_pick(identified, chosen)` -> what to use and whether learning is blocked.

### A3. Layouts -- `R/layouts.R`

A layout is a template list plus a `layout` block: `bank`, `status`
(provisional / proven / retired), `version`, `created`, `proved_by` (sha256 of
each statement that proved it), `origin` (auto / confirmed / corrected),
`signature` (kind, roles in order, date format, money style, balance frequency,
heading tokens, producer, relative column positions). Stored at
`<paths$layouts>/<bank_slug>/<id>@v<version>.yaml`; a change writes a new
version, never edits one. Retiring keeps the file.

`layouts_load(dir, bank = NULL)`, `layout_match(signature, layouts)`,
`layout_learn(reading, bank, file_sha, dir)` -> action (`created`,
`evidence_added`, `promoted`, `none`), `layout_confirm()`, `layout_retire()`,
`layouts_state_id(dir)` -> the learned-state version stamped on every output.

### A4. Tracking -- `R/tracking.R`

`track_record(fields, path)` writes one JSON line through an allowlist of named,
typed fields (no free text); `track_summary(path)` -> counts for the Admin page
and the carry-off summary.

### A5. Measuring -- `tools/synth/score_auto.R`

Fixed before the reader exists. Outcome matrix per statement: `auto_right`,
`AUTO_WRONG` (must be zero), `check_right`, `check_wrong`, `unread`. Modes:
`cold` (nothing learned) and `trained` (bank by bank, proven layouts handed
forward).
