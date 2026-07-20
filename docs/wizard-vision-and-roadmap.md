# Visual wizard vision + A/B/C roadmap

How template creation stays **easy for a zero-background analyst** and **generic
enough for the weirdest statement** — and the build order for the three
outstanding capabilities.

## 1. The visual, confirm-or-drag wizard

For PDFs (and any positional document), the wizard shows the **actual rendered
page** and works like this:

1. **Render, don't guess in the dark.** The page is rasterised (poppler) and
   shown as an image.
2. **Auto-detect proposes boxes.** Using the word-box geometry from `pdftools`,
   the engine clusters words into **columns (by x)** and **rows (by y)**, finds
   the transaction table as the largest run of aligned rows, and guesses each
   column's role from its *content* (a column of dates → "date"; a column of
   currency amounts → "amount"). Boxes are drawn over the image, colour-coded by
   confidence.
3. **The human is the final say.** For each box the analyst can **Confirm**,
   **drag** its edges, **redraw** it, **relabel** it, or **delete** it. They can
   also **draw a new box** the engine missed.
4. **"Ignore" is the default for anything untagged.** Logos, marketing blocks,
   footers, page numbers, irrelevant regions — if nobody tags them, they are
   simply never extracted. Weirdness costs a little human correction, never a
   crash or a wrong row.
5. **The template stores the CONFIRMED geometry + anchors**, not assumptions.
   Re-runs are deterministic; the same template handles that layout forever.

## 2. Why this is generic (covers the weirdest statements)

- **Geometry + content, not per-bank code.** Detection is based on where words
  sit and what they look like — so it works on a layout it has never seen. No
  bank is hard-coded.
- **Auto-detect is only a starting guess.** When it is completely wrong, or the
  region is irrelevant, the human overrides it in seconds. The floor is never
  "it failed" — it is "the analyst drew the boxes."
- **Two extraction strategies per field, pick what fits:**
  - **Positional bands** — `x_min/x_max` per column (clean tabular statements).
  - **Anchored / regex** — "the value after the label 'Closing balance'", or a
    regex on a line (free-text statements where columns don't align).
  A template can mix both.
- **Key-value mode for forms (IRD etc.).** Same canvas, but the analyst boxes a
  **label** and its **value** (or the engine pairs "Label: value" by proximity).
  Output is a named record, not a transaction table. Generic to any form.
- **Same forensic model throughout.** Nothing is dropped or fabricated;
  low-confidence extractions are flagged; redactions stay honoured; a run that
  can't be trusted says so.

Net: genericness = *geometry-based proposal + human-confirmable boxes on the
real page + "ignore by default" + two strategies + never-guess forensics.*

## 3. Roadmap — A, B, C (all wanted)

Dependency: **B (visual) and C (parser) need one real bank PDF; A needs one real
IRD PDF.** Sourcing those is in progress (bank-published sample statements with
populated example tables + synthetic fixtures). We already hold at least one real
populated PDF table (the ANZ Investment Funds statement guide) to start against.

| Step | What | Needs |
|---|---|---|
| **C** | PDF transaction-table parser: `format: pdf` template (column bands / anchors, multi-page stitch, drop repeated headers), consumed by a new PDF parse path. Turns every ⛔ in `edge-cases.md` into a tested ✅. | a real bank PDF |
| **B** | Visual wizard: render page → auto-proposed boxes → confirm/drag/relabel/ignore → writes the `format: pdf` template. Point-and-click, zero background. | C's template schema |
| **A** | Key-value / IRD mode: `mode: fields` templates + the "what kind of document?" first step in the wizard + named-record output. | a real IRD PDF |

Build order: **C → B → A** (parser first so the wizard has something real to
write and preview against; then the visual layer; then generalise to forms).

## 4. Guarantees carried into all of it
- Deterministic, no ML. Human-confirmed geometry, not model guesses.
- Verbatim descriptions; redactions honoured; no silent drops; never crashes.
- Declarative templates a single analyst can create by point-and-click and
  maintain when the original author is gone.
