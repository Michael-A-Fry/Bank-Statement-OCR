# Edge-case register

Every real-world statement edge case we know of, with an **honest** status and
how it is (or will be) handled. This is the checklist the engine is measured
against - "this is real world."

> **Since 2.0.0 there are no templates.** Where a row below says "the template"
> or names a template key (`delimiter`, `decimal_mark`, `merge_continuation`),
> the automatic reader now settles that fact from the statement itself, by
> voting over the whole document and proving the result with the arithmetic.
> The table reader underneath is the same one, so each mechanism and its test
> still stand. Section H is rewritten for 2.0.0. What the 2.0.0 reader does not
> yet handle is in `outstanding-work.md` and the 2.0.0 entry of `CHANGELOG.md`.

**Status key**
- ✅ **handled + tested** - covered by the engine with a passing test.
- 🟡 **handled (reader) / partial** - mechanism exists but not proven on a real
  statement, or only part of the flow covers it.
- ⛔ **needs real data** - cannot be built correctly without a real sample; the
  design is known, the code is not written/verified.

Tests proving the ✅ items live in `tests/testthat/` (fixtures under
`tests/testthat/fixtures/`).

**How a ⛔ becomes a ✅.** Not by guessing — by describing a real statement that
has the case in it. The prompt in
[survey-a-statement-with-ai.md](../operational/survey-a-statement-with-ai.md)
produces that description with no client information in it, so real statements
can be surveyed without leaving the building. Sections 8, 9 and 10 of the survey
map onto this register almost line for line; a batch of them is what moves rows
down this page.

---

## A. File / format

| Case | Status | How |
|---|---|---|
| UTF‑8 special chars in descriptions (`O'Connor & Sons`) | ✅ | preserved verbatim; only outer whitespace trimmed |
| Ragged rows (fewer/more fields than header) | ✅ | `bnz_ragged_short/long` fixtures - row flagged `malformed`, **never dropped** |
| Embedded newlines inside quoted fields | ✅ | `bnz_embedded_newline` fixture |
| Merged / escaped quotes | ✅ | `bnz_merged_quote` fixture |
| Trailing/leading empty columns, CRLF endings | ✅ | reader normalises; fixtures carry CRLF |
| Preamble lines before the header (ASB) | ✅ | `preamble.header_regex` skips to the real header |
| Empty file / wrong file type / unreadable | ✅ | returns `failed`/`unsupported` with an actionable reason, never crashes |
| Delimiter variants (`,` `\t` `;` `|`) | ✅ | `delimiter` in the template |
| Password‑protected PDF | ⛔ | detect + report `failed` ("needs password"); decrypt step not built |

## B. Amounts / numbers

| Case | Status | How |
|---|---|---|
| Single signed amount column | ✅ | `amount_sign: signed` (tested) |
| `D`/`C` type column (credit cards) | ✅ | `amount_sign: type_dc` (ANZ Visa, tested) |
| Separate debit / credit columns | 🟡 | `amount_sign: debit_credit_cols` implemented; needs a fixture to mark ✅ |
| `DR`/`CR` suffix | 🟡 | `amount_sign: dr_cr_suffix` implemented; needs a fixture |
| Unsigned amounts, `CR` = payment (credit cards) | ✅ | `amount_sign: unsigned` - bare = charge (−), `CR` = payment (+); `unsigned_default: credit` to reconcile an owed balance (tested) |
| Closing/opening balance printed in the amount column | ✅ | dropped as a summary row, still captured for reconciliation (tested) |
| Parentheses negatives `(45.00)` | ✅ | normaliser handles `(45.00)` / `45.00-` (tested) |
| Thousands separators `1,234.56` | ✅ | stripped in normalise, raw kept in `amount_raw` |
| `DR`/`OD` = negative, `CR` = positive balance markers | ✅ | read by `.num` (tested) |
| European decimal comma `1.234,56` | ✅ | auto when both separators present; per-template `decimal_mark: comma` for bare `1.234`=1234 (tested) |
| Foreign currency + conversion (FX) | ✅ | captured as `extras` (`anz_creditcard_fx`, `test-extras`) |
| Blank / zero amounts | ✅ | `NA` amount, row kept, flagged |

## C. Dates

| Case | Status | How |
|---|---|---|
| `DD/MM/YYYY`, `YYYY-MM-DD`, 2‑digit year `%y` | ✅ | `columns.date.format` (all in use across the 6 banks) |
| Raw kept alongside ISO | ✅ | `date_raw` + normalised `date` |
| Unparseable date | ✅ | `date = NA`, `date_raw` retained, not dropped |
| Statement spanning a year boundary | 🟡 | parses fine; `dates_within_period` KPI needs period metadata |

## D. Descriptions

| Case | Status | How |
|---|---|---|
| Verbatim special characters | ✅ | never stripped (tested) |
| Interior double spaces preserved | ✅ | `Auckland      Nz` kept byte‑for‑byte (ANZ Visa test) |
| Very long / embedded delimiters (quoted) | ✅ | quoted‑field parsing |
| Wrapped multi‑line description (one txn, several lines) - **PDF** | ✅ | the continuation merge in `R/parse_pdf_table.R` folds a date‑less, money‑less line into the transaction above it (proximity‑gated, and footer/"continued on next page" noise is excluded). Every word is accounted for — none is dropped — and the row is flagged `row_text_merged` so a reviewer can see the description was **assembled**, not printed on one line. Opt out per template with `merge_continuation: false` |

## E. Transaction structure

| Case | Status | How |
|---|---|---|
| Running balance present → continuity check | ✅ | Kiwibank; `running_balance_continuity` **passes** |
| Broken running balance | ✅ | `kiwibank_broken_balance` fixture → KPI **fails**, flagged (not "fixed") |
| No balance column | ✅ | KPI reports `na` with reason, trust stays medium |
| Opening/closing balance reconciliation | 🟡 | KPI implemented; runs when header carries opening+closing (needs a source that supplies them) |
| Redacted amount mid‑statement | ✅ | the cell reads as whatever is in the file; if it is genuinely empty the amount is derived from the balance delta (`amount_from_balance`) and the row is kept |
| No silent drops (completeness) | ✅ | `no_unparsed_rows` KPI proves every data row became a transaction |
| Reversals / duplicates / out‑of‑order dates | 🟡 | preserved verbatim; not *flagged* as such yet (design: an optional advisory KPI) |
| Subtotals / carried‑forward lines interleaved - **PDF** | ⛔ | this is the "gap in the middle of a block" case; needs the PDF parser + real sample |

## F. Redaction (forensic‑critical)

**The tool never redacts anything, and it no longer withholds anything either.**
Statements arrive however the sender sent them; the reader pulls what is in the
document. The requirement is only that a redaction must not *break* the read.

The suppression machinery that used to live here — marker glyphs, overlay
rectangles, a per-page rasterised occlusion scan — is deleted; see
[charter.md](charter.md) for the decision. What a redaction still costs is the
amount cell itself, and that is recovered from the running balance. Expected
outcomes:

- **Part of a row hidden** (e.g. amount blacked, date/description still visible):
  the row is recorded with its visible cells, the hidden cell is `[REDACTED]`
  (value `NA`, never back‑calculated), and the row carries a `redacted` flag.
- **A whole row hidden**: it simply does not appear. Its neighbours above and
  below are unaffected. We do **not** guess it was there.
- **A block of many rows hidden**: rows above and below are recorded; the hidden
  ones do not appear. We do **not** estimate how many transactions the block hid.
- **A header / non‑transaction area covered**: no transaction is produced there.

| Case | Status | How |
|---|---|---|
| Text‑layer marker (`[REDACTED]`, block glyphs, `XXXXXX`) | ✅ | read as `[REDACTED]`; original text under a supplied overlay is never emitted (tested) |
| Redacted value never derived/inferred | ✅ | as‑shown only; never back‑calculated from balance |
| Partial‑row redaction on a scanned page | ✅ | the black box is auto‑detected; the visible cells are kept, the blacked cell is `[REDACTED]` + flagged (tested) |
| Whole‑row / full‑block redaction on a scanned page | ✅ | no visible anchor → the hidden rows do not appear; neighbours untouched; **no count estimated** |
| Auto‑detect a rasterised black rectangle (no supplied coords) | ✅ | `detect_dark_regions()` finds solid black boxes on OCR pages (tested) |
| Black **vector** box over still‑live text (improper redaction by sender) | ⛔ | not yet detected from the content stream - a known follow‑up |

## G. PDF‑specific

| Case | Status | How |
|---|---|---|
| Multi‑page: read all pages | ✅ (reader) | `read_pdf` returns per‑page text + word boxes + `page_count` |
| Mixed selectable + scanned pages | ✅ (reader) | per‑page: text layer where present, **OCR fallback** where empty, `ocr` flag |
| Scanned / image‑only page | ✅ (reader) | Tesseract via `pdftoppm`, flagged `ocr`, lower trust |
| Section detection by anchor phrases | ✅ | `detect_pdf_sections` |
| **Transaction table → rows** | ✅ | **built** - declarative `format: pdf` parser, golden round-trip tested (`tutorial_everyday_pdf`, `anz_investmentfunds_pdf`); five shipped PDF templates in `templates/statements/` |
| Table split across a page break | ⛔ | design: stitch by column‑band continuity; needs real multi‑page sample |
| Repeated page headers/footers, page numbers | ⛔ | design: drop by y‑band + repetition; needs real sample |
| Rotated / multi‑column pages | ⛔ | needs real sample |

## H. Detection & robustness

| Case | Status | How |
|---|---|---|
| Two readings of the columns both add up | ✅ | not proven: a reading is proven only when it is the **only** one that passes every check (`unique`). The statement goes to Please check with the reason, and nothing reaches the dashboards (tested, `test-auto-read.R`) |
| Nothing on the statement can prove the reading (no balance, no totals) | ✅ | Please check, until a layout of that bank proved by other statements matches it; then *Matches a learned layout*, spot-checked at twice the rate |
| A layout the tool has never seen | ✅ | read from its content like any other; proven -> converts and is learned as a provisional layout. No setup needed |
| Wrong bank picked by the person | ✅ | the figures come from the statement's arithmetic, not the bank; a confident disagreement blocks learning until a person answers *Which bank?*, and a statement with no arithmetic is not converted on a layout while its bank is in question (tested) |
| A bank given as an account number | ✅ | not used; the bank is taken from the statement and a message says so (tested) |
| A bundle of several statements | ✅ | split only when an independent count confirms the boundaries; each statement identified and proven on its own pages. Unsplittable: converts only if every row proves, else Please check with *several statements in one file* |
| Two date columns (value date and transaction date) | 🟡 | read, but which one becomes `date` currently depends on the column order on one Kiwibank export (a proven reading whose dates differ by a day). Open; the fix is to choose by heading or content, or ask |
| A scanned page whose OCR runs out of time (60 s) | 🟡 | read as blank and named in `input$meta$ocr_timed_out`; the reader does not yet force Please check for it. Open |
| Any error anywhere | ✅ | wrapped → `failed` with actionable message; one JSON log line per run |

---

## The honest bottom line (updated)

The delimited, Excel, PDF and OCR paths are built and tested, and since 2.0.0
every statement is read by the automatic reader: columns from the content, roles
proved by the arithmetic, nothing automatic unless it proves uniquely. On the
synthetic dev set none was read automatically and wrongly.

What remains is **evidence on real statements**: the held-back acceptance sets,
and tracking plus spot checks on the server from the first day. A layout that
does not prove is a reader improvement measured on every test set, not a
template.
