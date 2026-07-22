# OCR & pre-processing research briefing

Practical, opinionated guidance for the pure-R conversion engine. Scope: how to
squeeze the most accuracy out of the **OCR fallback path** (`R/ocr.R`,
`R/read_pdf.R`) for image-only bank-statement pages, and how to make every
failure **loud and diagnosable**. No Python/reticulate, no cloud OCR. Tooling on
the host: `tesseract` 5.3.4 + `leptonica` 1.82.0 CLI, poppler `pdftoppm`, and the
R `magick` package (bundles ImageMagick; `eng` + `osd` traineddata present -
verified on this box).

**The one-sentence thesis:** for a bank statement, *how you get the pixels/text
matters far more than the OCR engine*. A digital PDF read via the text layer is
already at ~100%; a scan is only as good as the render + binarisation you feed
Tesseract. So the priority order is: **don't OCR when you don't have to → render
big and clean → binarise adaptively → tell the engine it's a table → verify with
arithmetic → fail loud on anything that doesn't reconcile.**

---

## 1. The 80/20 pre-processing pipeline

Ordered by bang-for-buck. Everything here operates on the raster produced by
`pdftoppm`/`pdf_render_page`, sitting **between** the render and the `tesseract`
call in `ocr_pdf_page()`. The current code renders at 300 DPI and calls
Tesseract directly - items 1–5 below are the cheap wins that are *not yet* wired
in.

Rough accuracy impact is the industry-reported range, not a promise: binarise +
deskew together is widely cited at **+15–30%** on messy scans, deskew alone up to
**+10%** ([Tesseract ImproveQuality](https://tesseract-ocr.github.io/tessdoc/ImproveQuality.html),
[SparkCo](https://sparkco.ai/blog/boost-tesseract-ocr-accuracy-advanced-tips-techniques)).

### Priority 1 - Render at ≥300 DPI (bigger for small print)
- **What:** rasterise the PDF page at 300 DPI minimum; go to 400–600 for the
  small mono type used in transaction rows and fine print.
- **Why for statements:** amounts and dates are small, dense, and unforgiving -
  a `3`/`8` or `.`/`,` confusion is a wrong number, not a typo. Tesseract wants a
  capital-letter height of **~30–33 px**; below ~20 px accuracy falls off a
  cliff. 300 DPI is the documented floor, <200 DPI is "unclear", >600 just
  bloats with no gain ([ImproveQuality](https://tesseract-ocr.github.io/tessdoc/ImproveQuality.html),
  [Markaicode](https://markaicode.com/tutorial/tesseract-tutorial-production-setup-guide/)).
- **When:** always. This is already done (`dpi = 300L` in `ocr_pdf_page`). Make
  it a parameter and bump to 400 when a page's median glyph height comes back
  small.
- **How:** `pdftoppm -r 400`, or `magick::image_read_pdf(path, density = 400)`.

### Priority 2 - Greyscale
- **What:** collapse to a single luminance channel before thresholding.
- **Why:** statement colour (bank-brand headers, coloured "CR" text, watermarks,
  zebra-striped rows) is noise to the recogniser; greyscale removes chroma and
  halves the work of the binariser.
- **When:** always, immediately after render.
- **How:** `image_convert(img, colorspace = "gray")`. (`pdftoppm -gray` also
  works but you lose the later magick steps.)

### Priority 3 - Binarisation: adaptive (`image_lat`) by default, Otsu for clean scans
- **What:** convert greyscale → pure black/white. Two families:
  - **Global / Otsu** (`image_threshold(type="black")` picks one threshold for
    the whole page): fast, great on **evenly-lit, clean** scans.
  - **Local adaptive / Sauvola-style** (`image_lat` = ImageMagick's
    `-lat`, threshold computed per-pixel from a local window): wins whenever
    illumination varies - phone photos, faxed/re-scanned statements, grey
    security tint, shadow down one margin, bleed-through from the back.
- **Why it's the highest-leverage cleanup:** binarisation is the step that most
  moves Tesseract's numbers ([Medium/Shouman](https://medium.com/@maxshouman/efficiency-of-image-binarization-as-a-preprocessing-technique-for-tesseract-ocr-637ee8e6609f)).
  Adaptive methods "consistently outperform simple global thresholding on
  degraded material, often by a substantial margin", while Otsu "degrades
  significantly when background intensity varies across the page"
  ([Sauvola vs Otsu study](https://journal.unnes.ac.id/journals/sji/article/view/40245),
  [arXiv 1905.13038](https://arxiv.org/pdf/1905.13038)). Note Tesseract 4/5
  already binarise internally with Otsu, so **only override when Otsu is failing**
  - don't double-binarise a clean page.
- **When adaptive beats global:** uneven lighting, coloured/tinted background,
  low contrast, camera captures. **Rule of thumb:** if the page has a background
  tint or a confidence dip concentrated in one region → adaptive.
- **How (R):**
  ```r
  # Otsu-ish global:
  magick::image_threshold(img, type = "black", threshold = "50%")
  # Sauvola-style local adaptive (window WxH, offset %): tune window to ~2x glyph
  magick::image_lat(img, geometry = "25x25+10%")
  ```
  Suggested params from the literature: Sauvola `k≈0.5` for scans, `k≈0.4` for
  camera shots ([handwriting.guru](https://handwriting.guru/articles/image-binarization-methods/)).
  In `-lat` terms that maps to a modest positive offset (~8–12%).

### Priority 4 - Deskew
- **What:** detect and correct the page tilt (skew) introduced by scanning.
- **Why:** Tesseract groups characters into lines by horizontal bands; even 1–2°
  of skew smears rows together and misassigns cells in a table. Cited at up to
  **+10%** on affected pages ([SparkCo](https://sparkco.ai/blog/advanced-optimization-techniques-for-tesseract-ocr-in-enterprises)).
- **When:** any scanned/photographed page (digital renders are never skewed -
  skip it there, don't burn the cycles).
- **How:** ~~`image_deskew(img, threshold = 40)`~~ **Correction (measured on real
  statement scans):** ImageMagick's deskew estimator misreports statement pages -
  it returned 4.3° / 4.4° / 5.3° for true tilts of 1° / 2° / 3°, and 0° for a
  tilted 150 dpi page - leaving residual tilt that smears rows across template
  bands. The engine now uses its own projection-profile estimator
  (`.detect_skew_angle()` in `R/ocr_preprocess.R`): shear dark pixels across a
  ±5° grid in 0.05° steps, score row-stacking, rotate by the winner and crop
  back to the original canvas anchored on the ink's centre so geometry (and the
  template's bands) are preserved. Pages under 0.3° are returned untouched.

### Priority 5 - Denoise / despeckle
- **What:** remove isolated speckles, scanner dust, JPEG mosquito noise.
- **Why:** stray dots become phantom punctuation - a speck next to `100` becomes
  `100.` or `1O0`. On statements, spurious characters in the amount column are
  the expensive kind of error.
- **When:** noisy scans and low-quality faxes; skip on clean renders.
- **How:** `image_despeckle(img)` (cheap, safe, can repeat 1–2×), or
  `image_reducenoise(img, radius = 1)` for heavier noise. Don't over-apply -
  aggressive denoise erodes thin strokes (the `1`/`l`/`i` and decimal points).

### Priority 6 - Contrast / normalisation
- **What:** stretch the tonal range so faint print goes properly black and paper
  goes properly white, **before** binarising.
- **Why:** faded thermal-printer statements and third-generation photocopies
  have low dynamic range; normalising rescues strokes the threshold would
  otherwise drop.
- **When:** low-contrast/faded pages. Do it *before* Priority 3.
- **How:** `image_normalize(img)` (auto stretch) or `image_level(img)` /
  `image_contrast(img)` for manual control.

### Priority 7 - Border / margin crop (trim)
- **What:** remove the black scan borders, punch-hole shadows, and page-edge
  gutter.
- **Why:** dark borders confuse layout analysis and skew estimation, and can be
  mis-read as characters or table rules.
- **When:** scans with visible borders.
- **How:** `image_trim(img, fuzz = 10)` to auto-crop uniform edges; or
  `image_crop`/`image_chop` with a fixed inset if the layout is known.

### Priority 8 - Upscale small text
- **What:** if glyphs are still under ~20 px tall after render, enlarge.
- **Why:** below Tesseract's comfort zone accuracy collapses; upscaling a small
  region gives the LSTM more pixels to work with ([ImproveQuality](https://tesseract-ocr.github.io/tessdoc/ImproveQuality.html)).
- **When:** genuinely small source type, or a low-DPI source you can't re-render.
- **How:** re-render at higher DPI first (best). Failing that,
  `image_resize(img, "200%", filter = "Lanczos")`. Interpolated upscaling is a
  weaker substitute for real DPI - prefer Priority 1.

### Priority 9 - Remove rules / gridlines
- **What:** strip the horizontal/vertical table lines so they don't touch glyphs
  or get read as `-`, `|`, `_`.
- **Why:** boxed statement tables (common on credit-card statements) have rules
  that merge with descenders and underscore digits.
- **When:** only when the statement has drawn table borders. Many statements use
  whitespace columns and need nothing here.
- **How:** morphology - build a long thin horizontal/vertical structuring
  element, detect the lines, subtract them:
  `image_morphology(img, "Open", "Rectangle:40x1")` to isolate horizontal rules,
  then composite-subtract. Keep it conservative; over-aggressive line removal
  eats hyphens and decimal points.

### Priority 10 - Orientation / rotation detection (OSD)
- **What:** detect 0/90/180/270° page rotation (and script) *before* OCR.
- **Why:** a sideways scanned page OCRs to garbage; you must auto-rotate first.
  The `osd` traineddata needed for this is present on the host.
- **When:** any scan batch where orientation isn't guaranteed. Cheap insurance.
- **How:** two options - (a) `magick::image_orient(img)` / EXIF auto-orient for
  camera shots; (b) ask Tesseract: `tesseract page.png stdout --psm 0` runs
  **OSD only** and prints the detected rotation and orientation confidence; then
  `image_rotate(img, angle)` to correct. PSM 1 and 12 fold OSD into the OCR pass.

**A sane default order:** render 400 DPI → greyscale → normalise (if faded) →
deskew (if angle>0.3°) → OSD-rotate (if needed) → adaptive/Otsu threshold →
despeckle → trim. Cache the intermediate so a reviewer can see exactly what
Tesseract saw.

---

## 2. Tesseract settings that matter

The engine shells out via `system2("tesseract", ...)` (`R/ocr.R`). The flags
below are the ones that change results for statements.

- **Page segmentation mode (`--psm`)** - the biggest single Tesseract lever.
  Authoritative list from the host binary (`tesseract --help-psm`):

  | PSM | Meaning | Use for statements |
  |----|---------|--------------------|
  | 0 | OSD only (no OCR) | orientation probe (Priority 10) |
  | 1 | Auto segmentation **with OSD** | scans of unknown rotation |
  | 3 | Fully automatic, no OSD (**default**) | whole mixed pages |
  | **4** | **Single column of variable sizes** | **the sweet spot for a statement transaction table** - keeps columns/rows aligned |
  | **6** | Single uniform block of text | a cleanly-cropped table region; current code uses `psm=6` |
  | 7 | Single text line | re-OCR one row/cell |
  | 8 | Single word | re-OCR one amount cell with an allowlist |
  | 11 | Sparse text, any order | scattered labels; **loses row/column order - avoid for tables** |
  | 12 | Sparse text **with OSD** | sparse + unknown rotation |

  Opinion: **PSM 4** for a full statement column, **PSM 6** for a pre-cropped
  table block, **PSM 7/8** when re-reading a single flagged cell. Avoid 11/12 for
  tabular data - they discard the geometry you need for column recovery
  ([PyImageSearch PSM guide](https://pyimagesearch.com/2021/11/15/tesseract-page-segmentation-modes-psms-explained-how-to-improve-your-ocr-accuracy/)).

- **OCR engine mode (`--oem`)** - leave at default (`3`) or force `1` (LSTM
  only). LSTM is the accurate modern engine; the legacy engine (0) is only for
  niche cases. `--oem 1` is the safe explicit choice on 5.x.

- **`--dpi`** - pass the true render DPI (`--dpi 400`). Tesseract uses it to size
  its internal thresholding/scaling; a wrong or missing DPI hurts small text.

- **Character allowlists** - restrict the alphabet **per column**, which kills
  the classic statement confusions. Re-OCR an amount cell with digits only:
  ```r
  system2("tesseract", c(cell_png, "stdout", "--psm", "8",
    "-c", "tessedit_char_whitelist=0123456789.,-()CRDR"))
  ```
  Use a date-only set (`0123456789/-.` plus month letters) for the date column.
  This is a targeted, high-value trick for the two columns that must be perfect.
  Note: allowlists are a per-run config, so apply them on **cell/column re-OCR**,
  not the whole page.

- **Language data (`-l eng`)** - `eng` only here. A general dictionary can
  "helpfully" correct a merchant name into a real word; if descriptions come out
  over-corrected, disable the dictionary with
  `-c load_system_dawg=0 -c load_freq_dawg=0`. Don't disable it globally without
  cause - it helps clean address/description text.

- **Confidence output - use `tsv`, not plain text.** The `tsv`, `hocr`, and
  `alto` configs are all installed. Run:
  ```r
  system2("tesseract", c(png, "stdout", "--psm", "4", "-l", "eng", "tsv"))
  ```
  TSV gives one row per token with `level` (1=page…5=word), `page/block/par/
  line/word` indices, the bounding box (`left top width height`), **`conf`
  (0–100)**, and `text` ([Tesseract TSV format](https://tomrochette.com/tesseract-tsv-format/)).
  This single change unlocks *both* the table-structure recovery in §4 (you get
  word x/y/w/h for free) and the per-field confidence in §6. **Switching
  `ocr.R` from plain `stdout` to `tsv` is the highest-value change on this list.**

---

## 3. Text-layer-first strategy

**Digital PDFs must never be OCR'd.** The text layer is the ground truth the bank
typeset - extracting it is lossless and deterministic; OCR of a rendered digital
page can only *introduce* error. This is the single biggest accuracy decision in
the whole engine, and `read_pdf.R` already gets it right: it reads
`pdftools::pdf_text` / `pdf_data` first and only falls back to OCR when a page
has no usable text layer.

- **How to detect a usable text layer, per page:** the current test
  (`page_needs_ocr()` in `ocr.R`) - a page is image-only if its extracted text is
  empty or has `< 20` non-space characters. Good baseline. Harden it with:
  - **word-box density:** `nrow(pdf_data[[p]])` near zero ⇒ no real layer;
  - **glyph-junk ratio:** a "text layer" that's mostly `□`/replacement chars or
    a broken CID font is *worse* than none - detect a high non-printable ratio
    and force OCR (this is the `encoding` failure category in §6);
  - **coverage sanity:** a statement page with 5 words of text but a full-page
    image is image-only in practice.
- **Mixed pages (text + scanned insert on the same page):** decide **per page,
  not per document** - `read_pdf.R` already loops per page and sets the `ocr`
  flag individually. For a page that is *partly* image (e.g. a scanned cheque
  pasted into a digital statement), prefer the text layer for the typeset region
  and OCR only the image region if you can bound it; when in doubt, take the text
  layer and flag the image region as `unread`, never silently merge.
- **Redaction interaction (already handled, keep it):** when any redaction
  touches a page, `read_pdf.R` rebuilds that page from *guarded* word boxes
  instead of raw `pdf_text`, so text under an overlay never leaks. OCR only ever
  sees visible pixels, so a black box is inherently unreadable. Don't regress
  this to "OCR everything" for uniformity.

---

## 4. Table structure recovery (heuristics only)

Once you have word boxes - from `pdf_data` (digital) or Tesseract `tsv` (OCR),
**the same `x/y/width/height/text` shape** - reconstruct the table with
common-sense geometry. No ML.

1. **Row grouping by y.** Sort words by `y`; start a new row when the vertical
   gap exceeds a tolerance (the engine's `words_to_text()` already does this with
   `line_tol = 3` on rounded `y`). For a table, set the tolerance to a fraction
   (~0.5–0.7) of the median glyph height so a slightly-tall cell doesn't split.
2. **Column detection by x-position clustering.** Words in the same column share
   a near-identical **left edge** (left-aligned text: dates, descriptions) or a
   near-identical **right edge** (right-aligned numerics: amounts, balances).
   Cluster the x-coordinates into vertical bands: build a histogram of left-edges
   (and right-edges) across all rows; peaks are column boundaries; the valleys
   between peaks are the gutters. This is the standard heuristic - "cluster text
   by near-identical starting x-coordinate" ([PyImageSearch multi-column table](https://pyimagesearch.com/2022/02/28/multi-column-table-ocr/)).
   Amount columns are best located by **right-edge** clustering because the
   decimal points line up.
3. **Assign each word to (row, column)** by which band its x falls in. Empty
   cells are real information (a debit-only row has an empty credit cell) - keep
   them, don't collapse.
4. **Wrapped cells (multi-line descriptions).** A transaction whose description
   spills onto 2–3 lines shows as extra rows that have text **only in the
   description column** and empty date/amount cells. Heuristic: a row with no
   date and no amount, immediately under a full row, is a continuation - merge
   its description up into the parent. (This is the `edge-cases.md` ⛔ "wrapped
   multi-line description" case.)
5. **Multi-page tables.** Detect the same column bands (x-positions) recurring on
   the next page and stitch. Drop repeated page headers/footers by **y-band +
   repetition**: text that appears at the same top/bottom y on every page and
   doesn't fit the column model is chrome, not data. Never let a repeated header
   become a phantom transaction.
6. **Anchor to known columns.** `detect_pdf_sections()` already finds header
   phrases ("TRANSACTION DETAILS", "OPENING BALANCE", …). Use the header row's
   word x-positions to *seed* the column bands instead of inferring them cold -
   far more robust than pure clustering.

Keep every heuristic **auditable**: store the detected column bands and the
row/column each word landed in, so a misalignment can be seen, not guessed at.

---

## 5. The honest take on "99.9%"

- **Digital-PDF path: ~100% is real** and already largely achieved, because
  there is no recognition step - you're reading typeset text. The residual error
  is *parsing* (wrong column mapping, date format), not *character* error, and
  parsing errors are catchable by arithmetic (§6). For the six delimited banks
  and any digital PDF, 99.9%+ at the field level is a reasonable target.
- **Scanned/photographed path: 99.9% character accuracy is not honestly
  guaranteed.** Good scans with the §1 pipeline reach the high-90s%; a per-page
  ~98–99% character rate on a dense statement still means several wrong
  characters per page, and a single wrong digit in an amount is a material error.
  Promising 99.9% on arbitrary scans would be dishonest.
- **Therefore the right goal is not a number - it's *fail-loud*.** For a bank
  statement the cost of a **silent** wrong amount hugely exceeds the cost of a
  flagged "couldn't read this - check it". The engine's existing philosophy is
  correct: **never silently drop a row, never back-calculate a redacted value,
  flag low-trust, and reconcile against arithmetic.** "99.9%" should mean *99.9%
  of fields are either correct or flagged* - i.e. the silent-error rate is driven
  toward zero even when the raw OCR error rate isn't. Reconciliation
  (`reconcile.R`: `balance_reconciliation`, `running_balance_continuity`) is what
  turns an unverifiable OCR guess into a **checkable** one: if opening + Σamount ≠
  closing, you *know* something is wrong even if you don't yet know which row.

---

## 6. Fail-loud diagnostics

Goal: every failure answers four questions - **WHERE, WHY, HOW BAD, HOW TO FIX**
- in a structured, machine-readable record, not a log string. Proposed record
(one per detected problem), emitted alongside the existing per-run JSON log:

```
diagnostic {
  where:  { page:int, row:int|NA, field:str|NA, bbox:[x,y,w,h]|NA }
  why:    <category>            # enum below
  how_bad:{ confidence:0..100|NA, discrepancy:num|NA, severity:info|warn|error }
  how_to_fix: <actionable string>
  evidence: { raw_text, snippet_png_path|NA }   # so a human can eyeball it
}
```

`where` comes straight from the `tsv` bounding box (§2) and the (row,column)
assignment (§4); `how_bad` comes from Tesseract `conf` and/or a `reconcile.R`
discrepancy. Categories and their fixes:

| Category | Detection signal | HOW BAD | HOW TO FIX |
|---|---|---|---|
| **no-text-layer** | `page_needs_ocr()` true; word-box count ≈ 0 | info (expected) → warn if OCR then also weak | falls back to OCR; flag page `ocr`, lower trust; escalate to `low-ocr-confidence` if that also fails |
| **low-ocr-confidence** | word `conf` below threshold (e.g. <80 for amounts, <60 for descriptions) | warn/error by field: an amount cell <80 = error | re-OCR that cell at higher DPI + `--psm 8` + numeric allowlist; if still low, surface the `snippet_png` for human review - **do not emit a guessed number** |
| **column-misalignment** | word x falls between column bands / row has wrong field count / amount in a non-amount band | error | re-derive bands from the header row (§4.6); try `--psm 4` vs 6; flag row `malformed`, keep it verbatim, never drop |
| **reconciliation-mismatch** | `balance_reconciliation` fail or `running_balance_continuity` break in `reconcile.R` | error (whole statement suspect) | localise: the row where running balance first diverges is the likely bad read → re-OCR that row's amount cell; report expected vs actual discrepancy |
| **unknown-format** | no template matches / ambiguous tie (`detect.R`) | error | report closest template + why it missed; ask for a sample; return `unsupported`, never a wrong parse |
| **redaction-heavy** | redacted-word count high / whole rows covered (`read_pdf.R` `redactions`) | warn | emit rows with `[REDACTED]` cells + `redacted` flag; if a table is mostly blacked out, flag the *table* low-trust rather than pretending to parse it |
| **encoding** | text layer present but high non-printable / replacement-char ratio (broken CID font) | error | treat as no-text-layer and force OCR; flag that the digital layer was unusable |
| **rotation** | OSD (`--psm 0`) reports rotation ≠ 0, or OSD confidence low | warn | auto-rotate with `image_rotate` and re-OCR; if OSD confidence is low, flag page for review |

Design rules that make it trustworthy:
- **Field-level thresholds, not one global number.** An amount/date cell must
  clear a higher `conf` bar than a description. A page can be "90% overall" and
  still have one un-trustable amount - that amount must flag.
- **Cross-check beats confidence.** Tesseract `conf` is a hint, not truth; the
  *real* safety net is arithmetic (`reconcile.R`). A high-confidence wrong digit
  is caught by a balance mismatch, not by `conf`. Wire reconciliation failures
  back to the offending row.
- **Every diagnostic is actionable and localisable.** "OCR failed" is useless;
  "page 3, row 12, amount cell bbox=[…], conf 41, re-OCR with numeric allowlist,
  balance diverges by \$40.00 here" is a work order.

---

## 7. Prioritized backlog (implement in this order)

1. **Switch OCR to `tsv` output** (`R/ocr.R`). One flag change; unlocks per-word
   `conf` + bounding boxes, which everything below depends on. *Highest ROI.*
2. **Add the pre-process step to `ocr_pdf_page()`:** greyscale → deskew (if
   angle>0.3°) → adaptive `image_lat` (fallback Otsu) → despeckle → trim, on the
   `pdftoppm`/magick raster before Tesseract. Bump default render to 400 DPI and
   pass `--dpi`. Cache the pre-processed PNG for audit. *Biggest raw-accuracy
   gain on scans.*
3. **Field-level confidence gating + diagnostics record** (§6): thresholds per
   column, structured `diagnostic{}` objects, integrated with the existing JSON
   log. Turns OCR from "trust me" into "here's exactly what to check".
4. **Numeric/date allowlist re-OCR of low-confidence cells** (`--psm 8` +
   `tessedit_char_whitelist`). Cheap, targets the two columns that must be right.
5. **OSD orientation probe** (`--psm 0`) + auto-rotate before OCR. Cheap
   insurance against sideways scans producing garbage.
6. **Table-structure recovery** (§4): header-seeded column bands, row grouping,
   wrapped-cell merge, multi-page stitch. This is the big `edge-cases.md` ⛔ -
   build it once a real multi-page scanned sample exists, reusing the shared
   `x/y/w/h` word shape so it works for both digital and OCR inputs.
7. **Gridline removal via morphology** (§1.9) - only if boxed-table samples show
   rules bleeding into digits; skip until a sample demands it.

The first three are small, safe, and compounding: they make the OCR path *both*
more accurate *and* self-diagnosing, which is exactly the "near-100% or fail
loud" posture the engine is built around.

---

## Sources

- [Tesseract - Improving the quality of the output (ImproveQuality)](https://tesseract-ocr.github.io/tessdoc/ImproveQuality.html) - DPI floor, glyph height, deskew/binarise gains, borders, inversion.
- [Tesseract - Command Line Usage](https://tesseract-ocr.github.io/tessdoc/Command-Line-Usage.html) and local `tesseract --help-psm` / `--help-oem` (v5.3.4) - authoritative PSM/OEM lists.
- [PyImageSearch - Tesseract PSMs Explained](https://pyimagesearch.com/2021/11/15/tesseract-page-segmentation-modes-psms-explained-how-to-improve-your-ocr-accuracy/)
- [PyImageSearch - Multi-Column Table OCR](https://pyimagesearch.com/2022/02/28/multi-column-table-ocr/) - x-coordinate column clustering.
- [Tesseract TSV format](https://tomrochette.com/tesseract-tsv-format/) - conf 0–100, level 1–5, bbox columns.
- [Performance Evaluation of Otsu and Sauvola Thresholding (SJI)](https://journal.unnes.ac.id/journals/sji/article/view/40245) and [arXiv 1905.13038 - fast local adaptive binarization](https://arxiv.org/pdf/1905.13038) - adaptive vs global.
- [Image Binarization Methods for OCR (handwriting.guru)](https://handwriting.guru/articles/image-binarization-methods/) - Sauvola k parameters.
- [Efficiency of image binarization for Tesseract (M. Shouman)](https://medium.com/@maxshouman/efficiency-of-image-binarization-as-a-preprocessing-technique-for-tesseract-ocr-637ee8e6609f)
- [SparkCo - Tesseract optimization](https://sparkco.ai/blog/boost-tesseract-ocr-accuracy-advanced-tips-techniques) & [advanced techniques](https://sparkco.ai/blog/advanced-optimization-techniques-for-tesseract-ocr-in-enterprises) - deskew/binarise accuracy ranges.
- [Markaicode - Tesseract production setup](https://markaicode.com/tutorial/tesseract-tutorial-production-setup-guide/) - 300 DPI production guidance.
- R `magick` package (installed; exact functions verified on host): `image_convert`, `image_deskew`/`image_deskew_angle`, `image_lat`, `image_threshold`, `image_despeckle`/`image_reducenoise`, `image_normalize`/`image_level`/`image_contrast`, `image_trim`, `image_orient`/`image_rotate`, `image_morphology`, `image_resize`, `image_ocr_data`.
