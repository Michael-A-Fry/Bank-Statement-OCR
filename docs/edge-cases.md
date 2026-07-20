# Edge-case register

Every real-world statement edge case we know of, with an **honest** status and
how it is (or will be) handled. This is the checklist the engine is measured
against — "this is real world."

**Status key**
- ✅ **handled + tested** — covered by the engine with a passing test.
- 🟡 **handled (reader) / partial** — mechanism exists but not proven on a real
  statement, or only part of the flow covers it.
- ⛔ **needs real data** — cannot be built correctly without a real sample; the
  design is known, the code is not written/verified.

Tests proving the ✅ items live in `tests/testthat/` (fixtures under
`tests/testthat/fixtures/`).

---

## A. File / format

| Case | Status | How |
|---|---|---|
| UTF‑8 special chars in descriptions (`O'Connor & Sons`) | ✅ | preserved verbatim; only outer whitespace trimmed |
| Ragged rows (fewer/more fields than header) | ✅ | `bnz_ragged_short/long` fixtures — row flagged `malformed`, **never dropped** |
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
| Parentheses negatives `(45.00)` | 🟡 | normaliser handles; add a fixture to lock it |
| Thousands separators `1,234.56` | ✅ | stripped in normalise, raw kept in `amount_raw` |
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
| Wrapped multi‑line description (one txn, several lines) — **PDF** | ⛔ | needs the PDF row parser + a real sample |

## E. Transaction structure

| Case | Status | How |
|---|---|---|
| Running balance present → continuity check | ✅ | Kiwibank; `running_balance_continuity` **passes** |
| Broken running balance | ✅ | `kiwibank_broken_balance` fixture → KPI **fails**, flagged (not "fixed") |
| No balance column | ✅ | KPI reports `na` with reason, trust stays medium |
| Opening/closing balance reconciliation | 🟡 | KPI implemented; runs when header carries opening+closing (needs a source that supplies them) |
| Redacted amount mid‑statement | ✅ | `bnz_redacted_amount` → `[REDACTED]` + `redacted` flag, row kept |
| No silent drops (completeness) | ✅ | `no_unparsed_rows` KPI proves every data row became a transaction |
| Reversals / duplicates / out‑of‑order dates | 🟡 | preserved verbatim; not *flagged* as such yet (design: an optional advisory KPI) |
| Subtotals / carried‑forward lines interleaved — **PDF** | ⛔ | this is the "gap in the middle of a block" case; needs the PDF parser + real sample |

## F. Redaction (forensic‑critical)

| Case | Status | How |
|---|---|---|
| Text‑layer marker (`[REDACTED]`, block glyphs, `XXXXXX`) | ✅ | swept to `[REDACTED]`, original discarded (tested) |
| Black box **over** text (text still in layer) | ✅ | overlay guard rebuilds page text from guarded word boxes — **text under the box never leaks** (tested, incl. partial overlap) |
| Redacted value never derived/inferred | ✅ | as‑shown only; never back‑calculated from balance |
| Heavy / full‑block redaction (many rows blacked out) | 🟡→⛔ | the *guard* scales (any covered word is dropped); turning a heavily‑redacted **table** into flagged rows needs the PDF parser + real sample |
| Redaction inside an OCR'd (scanned) page | ✅ | OCR reads only visible pixels — a black box is unreadable, so nothing under it is recovered |
| Auto‑detect the black rectangle itself (no supplied coords) | ⛔ | documented hook in `read_pdf.R`; needs content‑stream/raster analysis on a real file |

## G. PDF‑specific

| Case | Status | How |
|---|---|---|
| Multi‑page: read all pages | ✅ (reader) | `read_pdf` returns per‑page text + word boxes + `page_count` |
| Mixed selectable + scanned pages | ✅ (reader) | per‑page: text layer where present, **OCR fallback** where empty, `ocr` flag |
| Scanned / image‑only page | ✅ (reader) | Tesseract via `pdftoppm`, flagged `ocr`, lower trust |
| Section detection by anchor phrases | ✅ | `detect_pdf_sections` |
| **Transaction table → rows** | ⛔ | **not built** — the core missing piece; needs a real statement |
| Table split across a page break | ⛔ | design: stitch by column‑band continuity; needs real multi‑page sample |
| Repeated page headers/footers, page numbers | ⛔ | design: drop by y‑band + repetition; needs real sample |
| Rotated / multi‑column pages | ⛔ | needs real sample |

## H. Detection & robustness

| Case | Status | How |
|---|---|---|
| Ambiguous match (two templates tie) | ✅ | requires strict > 2nd best → else `unsupported` (tested) |
| No template matches | ✅ | `unsupported` + closest‑match diagnostic |
| Wrong bank forced by user | ✅ | hint is a hard filter; mismatch → `unsupported`, not a wrong parse |
| Any error anywhere | ✅ | wrapped → `failed` with actionable message; one JSON log line per run |

---

## The honest bottom line (updated)

The delimited path, redaction guard and OCR are built and tested. The **PDF
transaction‑table parser is now built** (declarative `format: pdf`), tested on a
real populated table, and **mid‑block gaps / headings / annotations are handled**
by the date‑parse row filter (a row is kept only if its date cell parses).
**Excel (.xlsx)** and **key‑value (IRD‑style) extraction** are in too, and a
**visual PDF wizard** creates PDF templates by drawing boxes.

What remains is **data‑gated, not code**: real native export files for more
banks, and real per‑bank PDF statements to add more `format: pdf` templates —
each is a YAML + a wizard session, not an engine change.
