# Open-source landscape — statement extraction, PDF tables & OCR

A practical survey of open-source projects and techniques for **bank-statement
extraction, PDF table recovery, and OCR**, read through the lens of *our* engine:
pure **R** (base R + `pdftools`, `tesseract` CLI, poppler, `magick`, `openxlsx`,
`yaml`), no Python/reticulate, no cloud OCR, no heavy new deps, and the standing
design rule **"a new bank is a YAML template, never new code."** For every useful
project there is an **Adopt:** note tying it to a concrete part of the engine and a
LICENSE flag.

Companion doc: OCR pixel/pipeline detail lives in
[`ocr-preprocessing.md`](ocr-preprocessing.md); this doc is the *ecosystem* view
and the cross-cutting backlog. It does not repeat the binarise/deskew mechanics.

## The license lens (read first)

- **Permissive — MIT / BSD / Apache-2.0 / MPL-2.0:** safe to *learn from* and even
  to vendor small, attributed snippets. Most of the best table/parse work is here.
- **Copyleft — GPL-2.0/3.0, AGPL:** *learn the technique, never copy the source*
  into our R. Copying a GPL function makes the whole engine GPL.
- **NOASSERTION / custom:** treat as "learn from structure only" until the licence
  is confirmed.
- **Running a binary is not copying.** We shell out to `tesseract` (Apache-2.0) and
  use poppler via `pdftools` (poppler is GPL-2.0+, but `pdftools` itself is MIT and
  we only *call* it). Invoking a separate process/CLI is not a derivative work — so
  our poppler/tesseract usage is clean even where the tool is GPL. The rule only
  bites if we paste GPL *source* into `R/`.

---

## 1. PDF table extraction

| Project | Lang | License | Core technique |
|---|---|---|---|
| [Camelot](https://github.com/camelot-dev/camelot) | Python | MIT | Two modes: **lattice** (OpenCV morphology finds ruling lines → cell grid from line intersections) and **stream** (whitespace/text clustering; accepts explicit column x-separators) |
| [Tabula / tabula-java](https://github.com/tabulapdf/tabula-java) | Java | MIT | "Spreadsheet/lattice" (ruling lines) vs "guess/stream" (whitespace-gap column detection); GUI to draw the table area |
| [pdfplumber](https://github.com/jsvine/pdfplumber) | Python | MIT | Word/char objects with x0/x1/top/bottom; table finder with `vertical_strategy`/`horizontal_strategy` = `lines`\|`text`\|`explicit` + snap/join/text tolerances; visual debugger |
| [Excalibur](https://github.com/camelot-dev/excalibur) | Python | MIT | Web UI over Camelot — point-and-click table area + column separators |
| pdftotext `-layout` | C++ (poppler) | GPL-2.0+ | Reflows the text layer preserving column whitespace; no cell model |

**Lattice vs stream — and where we sit.** *Lattice* needs drawn gridlines and
recovers cells from line intersections; it is unbeatable on boxed credit-card
tables but useless on the whitespace-column layouts most NZ statements use.
*Stream* infers columns from whitespace: group words into rows by `y`, then find
the vertical gutters between `x`-clusters. **Our `format: pdf` x-band approach is
stream mode with human-supplied column separators** — i.e. exactly Camelot
`stream` with an explicit `columns=` list, or pdfplumber
`vertical_strategy="explicit"`. That is deliberately the *most robust* variant:
the wizard lets an analyst draw the gutters instead of trusting a gap-detector, so
we never mis-split a sparse column. `parse_pdf_table.R` already does row-grouping
by `y` (`row_tol`) and column assignment by band — the same primitives these
tools use.

- **Adopt (Camelot/pdfplumber → wizard auto-suggest):** their whitespace column
  detection is worth porting *as a suggestion* in `wizard_detect.R` — build an
  x-histogram of word left-edges (and, for amounts, **right-edges** so decimal
  points align) and pre-place the band boundaries at the valleys, which the analyst
  then nudges. Learn-from only, both MIT so a small snippet is fine.
- **Adopt (pdfplumber tolerances → template schema):** their `snap_tolerance` /
  `text_tolerance` / `join_tolerance` are the exact knobs a real statement needs;
  we have `row_tol` — add an optional column-snap tolerance for wobbly renders.
- **Adopt (Tabula/Excalibur → our wizard):** their "draw the area + columns" UX is
  precisely our visual wizard; validates the design. Nothing to port, but their
  `area`+`columns` model = our `region`+`columns.x_min/x_max`.
- **Skip:** pdftotext `-layout` — it throws away the cell geometry `pdf_data`
  already gives us, and it is GPL.

---

## 2. Bank-statement importers / parsers

| Project | Lang | License | Per-bank format defined by |
|---|---|---|---|
| [bank2ynab](https://github.com/bank2ynab/bank2ynab) | Python | custom (NOASSERTION) | **One plain-text config file** (`bank2ynab.conf`); each bank = a named section of keys, ~100 banks, *zero per-bank code* |
| [ofxstatement](https://github.com/kedder/ofxstatement) | Python | GPL-3.0 | A **plugin class per bank** + an `.ini` for parameters; `CsvStatementParser` base maps CSV columns → `StatementLine` fields |
| [sebastienrousseau/bankstatementparser](https://github.com/sebastienrousseau/bankstatementparser) | Python | Apache-2.0 | Deterministic parsers for CSV / OFX-QFX / MT940 / CAMT (ISO 20022) with continuity ("Golden Rule") verification, page provenance, dedupe, accuracy evals |
| GnuCash CSV importer | C/C++ | GPL-2.0+ | Interactive column-map UI + saved "import settings" presets |
| [hledger](https://github.com/simonmichael/hledger) CSV rules | Haskell | GPL-3.0 | Declarative `.rules` file: `fields`, `if` blocks (regex match → set field), `date-format`, `skip` |
| [Benrnz/BudgetAnalyser](https://github.com/Benrnz/BudgetAnalyser) | C# | MIT | NZ-built PFM with per-bank CSV import matching |

**Config vs code — the key finding.** The projects that scale to many banks all
push the format into *data*, and the cleanest of them (**bank2ynab**) proves our
central thesis: ~100 banks as pure config, no per-bank code — the same bet our
YAML-template rule makes. Its section keys are effectively a **schema checklist**
we should measure our template format against:

- `Source Filename Pattern` → we have `fingerprint.filename_regex`.
- `Header Rows` / rows-to-skip, `Delete Columns` → we handle via `preamble` and the
  date-parse row filter, but an explicit "skip N header rows / ignore these source
  columns" key would be clearer.
- `Input Columns` → our `columns:` map (we already do this, by name not position).
- `Date Format` → we have `columns.date.format`.
- `Delimiter`, `Encoding`, `Has Headers` → we have `delimiter`; **encoding is
  implicit** — worth an explicit key given the UTF-8-BOM / cp1252 edge cases in
  `samples/`.
- `Inverted` (flip sign), `Fill/Swap` → maps to our `amount_sign`; an explicit
  per-column sign-inversion flag would cover debit-positive exports.

  **Adopt (bank2ynab → `templates/` schema + `schema.R`):** treat its config keys
  as the gap-analysis for our YAML — add explicit `encoding`, `skip_rows`, and a
  per-amount `invert` flag. Pure config, no engine code, fits the rule exactly.
  *License is unspecified (NOASSERTION) → copy the idea/keys, not the file.*

- **Adopt (bankstatementparser → `reconcile.R` + tests):** its **"Golden Rule"**
  (opening + Σ movements = closing) and running-continuity check are what we
  already do in `reconcile.R`; borrow two refinements — (a) **localise** the first
  row where running balance diverges and point the diagnostic there, and (b) its
  **accuracy-eval harness** idea to formalise our golden-file tests into a scored
  regression. Also its **input-format breadth** (MT940 / OFX-QFX / CAMT) is a
  future declarative path — and we already ship synthetic `.mt940` / `.qfx`
  samples. *Apache-2.0 → safe to learn from generously (attribute if we vendor).*
- **Adopt (ofxstatement → schema sanity):** its `StatementLine` field set
  (date, amount, payee, memo, check_no, refnum, trntype) is a battle-tested
  superset — a good cross-check that our 16-col core schema isn't missing a common
  field. *GPL-3.0 → technique/shape only, never copy the parser code.*
- **Adopt (hledger `if` rules → optional template rules):** conditional field
  logic ("if description matches FEE → force sign negative", "if TYPE=DR →
  debit") is the one expressive thing our declarative templates lack; add an
  optional `rules:` block (match → set) *only when a real statement needs it*, so
  we stay declarative without a code path per bank. *GPL-3.0 → concept only.*
- **Adopt (GnuCash → nothing new):** its saved import presets == our saved YAML;
  confirms persistence approach. *GPL.*
- **Adopt (bank2ynab NZ sections / BudgetAnalyser):** bank2ynab ships configs for
  ASB, ANZ, BNZ, Kiwibank, Westpac — **our exact banks** — so their column maps are
  a free correctness cross-check for our six delimited templates. *MIT (BudgetAnalyser)
  / NOASSERTION (bank2ynab) → compare, don't copy.*

**NZ-specific note (honest):** there is no mature, maintained NZ-native
bank-statement library to adopt. NZ coverage in the wild is exactly the generic
tools above (bank2ynab/ofxstatement plugins) plus ad-hoc gists. That gap is the
space this engine fills.

---

## 3. OCR tooling

| Project | Lang | License | Notes for us |
|---|---|---|---|
| [Tesseract](https://github.com/tesseract-ocr/tesseract) | C++ | Apache-2.0 | Already our engine (shelled out). LSTM (`--oem 1`), PSM, `tsv` conf/bbox output |
| [OCRmyPDF](https://github.com/ocrmypdf/OCRmyPDF) | Python | MPL-2.0 | Orchestrates the *right* pipeline around Tesseract — the recipe to mirror |
| [kraken](https://github.com/mittagessen/kraken) | Python | Apache-2.0 | Trainable line recogniser; ML models, PyTorch — **not portable to pure R** |
| [docTR](https://github.com/mindee/doctr) | Python | Apache-2.0 | Strong detection+recognition+layout, but TF/PyTorch + model weights — **Python-only, out of scope** |
| [PaddleOCR](https://github.com/PaddlePaddle/PaddleOCR) | Python | Apache-2.0 | Best-in-class layout/table, but Paddle runtime + weights — **out of scope** |

**The portable one is OCRmyPDF.** It is the canonical "wrap Tesseract properly"
tool and its *order of operations* is exactly what our `magick` pipeline should
do: **skip pages that already have a text layer** (`--skip-text` == our
text-layer-first rule), **deskew**, **clean** (via `unpaper` — our
`image_deskew`/`image_lat`/`image_despeckle` equivalents), **rotate by OSD**, then
OCR, then attach a searchable layer. It also proves the "don't OCR digital PDFs"
decision at scale.

- **Adopt (OCRmyPDF → `ocr.R`/`read_pdf.R`):** mirror its pipeline *ordering* and
  its skip-if-text-layer gate; it is the reference implementation for the recipe
  already written up in `ocr-preprocessing.md` but not yet fully wired. *MPL-2.0 is
  file-level copyleft → learn freely, don't copy whole files into `R/`.*
- **Adopt (Tesseract → `ocr.R`):** switch to `tsv` output for per-word
  `conf`+bbox, `--oem 1`, PSM 4/6 for tables, and per-column char allowlists — all
  detailed in `ocr-preprocessing.md`. *Apache-2.0.*
- **Avoid:** kraken / docTR / PaddleOCR — all require Python + ML runtimes + model
  weights, which violates the no-Python / no-heavy-deps constraint outright. Note
  them only as the "if we ever leave pure-R" ceiling for scanned-table accuracy.

---

## 4. Statement validation / requirements (mirror Hubdoc)

Hubdoc/Xero publish a crisp set of ingest rules for statement extraction — a good
template for **pre-flight input validation** that produces our fail-loud
diagnostics *before* wasting a parse. Rules (per
[Xero Central / Hubdoc](https://central.xero.com/s/article/About-bank-statement-extraction-in-Hubdoc)):
min **200 DPI**, **one statement only**, **English**, a **debit/credit statement**
(not a form), **scanned straight**, **≤100 pages**, **not empty**, **PDF** with max
height/width **40 in / 2880 pt**.

| Rule | Enforce as | Status in our engine |
|---|---|---|
| Single statement only | reject/flag if multi-statement detected | **Have** multi-statement detection → wire it to a hard diagnostic |
| All pages present | continuity KPI + parse "Page X of Y" if printed | **Partly** (reconciliation + page_count) → add page-sequence check |
| Min 200 DPI (scans) | measure source/render DPI; warn `<200`, block very low | **New** — cheap `pdftools`/`magick` check on the OCR path |
| ≤ 100 pages | input guard | **New** — trivial pre-flight |
| ≤ 2880 pt (40 in) page size | input guard from `pdf_pagesize` | **New** — trivial pre-flight |
| English only | `-l eng`; flag script/lang mismatch via OSD | **Partly** (we only load `eng`) → make the assumption explicit + flagged |
| Debit/credit statement, not a form | detect step / metadata | **Have** (`detect.R`, metadata) |
| Not empty / no text and no image | guard → `unsupported` | **Partly** (`page_needs_ocr`) → add explicit empty-doc guard |
| Straight scan | deskew (OSD/`image_deskew`) | **Planned** in `ocr-preprocessing.md` |
| PDF file type | reader dispatch | **Have** (`read_input.R`) |

- **Adopt (Hubdoc rules → `diagnose.R` pre-flight):** implement all of the above as
  a single cheap **pre-flight validation pass** that emits our structured
  where/why/how-bad/how-to-fix diagnostics. The four "New" ones (DPI floor, page
  count, page dimensions, empty doc) are a few lines of `pdftools` each and turn
  silent bad-input failures into actionable messages. *Hubdoc is a spec, not code —
  nothing to license; we mirror the requirements.*

---

## 5. R-native options (honest verdict)

| Package | License | Verdict |
|---|---|---|
| [`pdftools`](https://github.com/ropensci/pdftools) | MIT | **Keep — our foundation.** `pdf_text` + `pdf_data` (word boxes x/y/w/h) is exactly the stream primitive; no Java, no Python |
| [`magick`](https://github.com/ropensci/magick) | MIT | **Keep.** All preprocessing (greyscale/deskew/lat/despeckle/trim) in one MIT package |
| [`tesseract`](https://github.com/ropensci/tesseract) (R pkg) | (Apache-2.0/MIT) | **Optional.** `ocr_data()` returns word+conf+bbox as a data frame — the `tsv` glue for free. We deliberately shell out (no binding). Legitimate low-cost alt, but adds a package that bundles libtesseract; given "no new deps", keep shelling out unless the glue gets painful |
| `tabulizer` (retired from CRAN) / [`tabulapdf`](https://cran.r-project.org/package=tabulapdf) | MIT | **Avoid.** R bindings to Tabula (Java) via `rJava` → drags in a full JVM + `rJava`. That is precisely the heavy dep we forbid, and it buys nothing: our `pdf_data` + x-band already reproduces Tabula **stream** without Java. Lattice-only tables would be the only reason, and we don't have those yet |

**Honest bottom line for R:** there is **no mature R-native bank-statement
parser** to adopt — the space is empty, which is why this engine exists. The right
R stack is exactly what we run: `pdftools` for geometry, `magick` for pixels,
`tesseract` CLI for OCR, `yaml`/`openxlsx` for config/output. Adding `tabulapdf`
(Java) or any Python OCR would trade our clean deployment story for marginal gains
we can already get with heuristics.

---

## Prioritized "aspects to adopt" backlog

Highest leverage first; each fits pure-R / YAML-template / no-new-deps. Tags:
**[done]** already in the engine, **[enhance]** extend existing, **[new]** net-new
(but small).

1. **[new] Hubdoc-style pre-flight input validation** → `diagnose.R`. Page-count
   ≤100, page-size ≤2880 pt, non-empty, DPI≥200 on scans, single-statement (wire
   existing detection), explicit English assumption. A few lines of `pdftools`
   each; converts silent bad-input into actionable diagnostics. *Highest ROI,
   lowest cost.* (Source: Hubdoc/Xero spec — no license.)
2. **[enhance] bank2ynab-style config-key gap-fill** → template schema +
   `schema.R`. Add explicit `encoding`, `skip_rows`, and per-amount `invert`
   keys; audit our six templates against bank2ynab's ASB/ANZ/BNZ/Kiwibank/Westpac
   sections. Pure config, no code path per bank — dead-on the design rule. (MIT
   cross-checks / NOASSERTION config → copy ideas only.)
3. **[enhance] Continuity-diagnostic localisation** → `reconcile.R`. Keep our
   Golden-Rule/running-balance checks (already **[done]**) but, per
   bankstatementparser, report the *first divergent row* as the suspect and (for
   scans) trigger a targeted cell re-OCR there. (Apache-2.0 → learn generously.)
4. **[new] Wizard column auto-suggest via x-histogram** → `wizard_detect.R`.
   Port Camelot/pdfplumber whitespace clustering as a *suggestion*: left-edge
   histogram for text columns, **right-edge** for amount columns, seed bands from
   the header row; analyst confirms. Makes adding a PDF bank faster without giving
   up human-drawn gutters. (MIT → snippet-safe.)
5. **[enhance] Wire the OCRmyPDF/Tesseract recipe** → `ocr.R`/`read_pdf.R`. The
   `tsv`-output + preprocess-order pipeline is already specced in
   `ocr-preprocessing.md`; OCRmyPDF is the reference for ordering and the
   skip-if-text-layer gate. (Tesseract Apache-2.0; OCRmyPDF MPL-2.0 → mirror,
   don't copy files.)
6. **[new, optional] hledger-style `rules:` block** in templates — conditional
   match→set for sign flips / type mapping — *only if* a real statement needs
   logic a static column map can't express. Keeps banks declarative. (GPL →
   concept only.)
7. **[new, data-gated] Declarative MT940 / OFX-QFX / CAMT input paths** — we ship
   synthetic samples already; bankstatementparser (Apache-2.0) shows the parse
   shape. Adds new *formats*, still no per-bank code.

**Explicitly avoid:** `tabulizer`/`tabulapdf` (Java/rJava), PaddleOCR/docTR/kraken
(Python + ML weights), pdftotext `-layout` (GPL + loses geometry), and copying any
GPL source (ofxstatement, hledger, GnuCash, poppler) into `R/`.

---

## Sources

- Camelot vs Tabula vs pdfplumber (lattice/stream): [Camelot comparison wiki](https://github.com/camelot-dev/camelot/wiki/Comparison-with-other-PDF-Table-Extraction-libraries-and-tools), [pdfplumber](https://github.com/jsvine/pdfplumber), [invoicedataextraction comparison](https://invoicedataextraction.com/blog/python-pdf-table-extraction-invoices)
- Bank importers: [bank2ynab](https://github.com/bank2ynab/bank2ynab) (NOASSERTION), [ofxstatement](https://github.com/kedder/ofxstatement) (GPL-3.0), [sebastienrousseau/bankstatementparser](https://github.com/sebastienrousseau/bankstatementparser) (Apache-2.0), [hledger CSV rules](https://hledger.org/hledger.html#csv), [Benrnz/BudgetAnalyser](https://github.com/Benrnz/BudgetAnalyser) (MIT)
- OCR tooling: [OCRmyPDF](https://github.com/ocrmypdf/OCRmyPDF) (MPL-2.0), [Tesseract](https://github.com/tesseract-ocr/tesseract) (Apache-2.0), [open-source OCR comparison 2025/26](https://unstract.com/blog/best-opensource-ocr-tools/), [modal.com OCR model comparison](https://modal.com/blog/8-top-open-source-ocr-models-compared)
- Statement validation: [About bank statement extraction in Hubdoc — Xero Central](https://central.xero.com/s/article/About-bank-statement-extraction-in-Hubdoc)
- R-native: [tabulapdf on CRAN](https://cran.r-project.org/package=tabulapdf) & [tabulapdf paper (arXiv 2409.14524)](https://arxiv.org/pdf/2409.14524) (Java/rJava), [pdftools](https://github.com/ropensci/pdftools), [R-bloggers tabulapdf intro](https://www.r-bloggers.com/2024/04/tabulapdf-extract-tables-from-pdf-documents/)

*Licences verified via the GitHub API where shown; permissive projects (MIT/Apache-2.0/
MPL-2.0) may be learned from and lightly vendored with attribution, GPL/NOASSERTION
projects for technique only.*
