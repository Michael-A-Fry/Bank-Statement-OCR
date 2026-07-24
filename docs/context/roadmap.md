# Roadmap - ranked by ROI (value ÷ effort × maintenance cost)

One canonical, prioritised backlog. **Simplicity is a first-class filter here:**
value is discounted by how much ongoing maintenance an item adds, because a
codebase that's hard to maintain is a failed one. The design rule that keeps this
whole thing simple - **a new bank is a YAML template, never new code** - is
protected in every ranking decision below.

_Last updated: 2026-07-24._

## ✅ Done (progress so far)
- Pure-R engine: **delimited path end-to-end for 6 banks**, golden-file tested.
- **PDF path shipped:** declarative `format: pdf` text-layer table parser **and** a
  visual box/band editor that draws column bands over the page and writes the same
  YAML the parser consumes.
- **Excel (`.xlsx`) path** and a **key-value (`mode: fields`) extraction** foundation
  for IRD / form documents - both shipped and tested.
- Reconciliation KPIs + trust score; **fail-loud diagnostics** (where/why/how/fix)
  in result, workbook, JSON and the app.
- Redaction guard (forensic no-leak); OCR (tesseract/poppler) + pre-processing,
  including an **adaptive (Sauvola) scan profile** and **per-word OCR confidence**
  gating (`low_ocr_confidence`).
- **Zero-background wizard** (auto-detect delimiter/date/amount, plain English).
- Shiny GUI (convert · wizard · help); onboarding, edge-case register, research.
- **13 proven, golden-tested templates** shipping in `templates/`: 7 delimited
  (ANZ everyday/credit card, ASB, BNZ, Kiwibank, Westpac, plus a cross-bank
  Xero-standard import), **5 PDF** (`anz_everyday_pdf`, `anz_investmentfunds_pdf`,
  `asb_everyday_pdf`, `tutorial_everyday_pdf`, `westpac_everyday_pdf`) and **1 Excel**
  (`excel_generic_xlsx`). The PDF and Excel extraction paths are covered by golden
  round-trip tests.
- Test suite: **1,430 assertions across 343 tests (58 files), 0 failures.**

**The C→B→A engine pass is complete and shipped** - the PDF text-layer parser (C),
the visual band editor (B) and the `mode: fields` IRD/form foundation (A), plus the
Excel path, OCR-confidence gating and adaptive scan tuning. There is **no
outstanding engine work on the critical path**; what remains below is validation,
deployment proof, and data-gated template growth - not new engine code.

## 🎯 Backlog - highest ROI first

| # | Item | Value | Effort | Maint. | Why here |
|---|------|-------|--------|--------|----------|
| 1 | **Validation & adoption** - watch a real non-technical analyst build a template end-to-end on a real file | ★★★★★ | ● | ~zero | The engine is built; the open question is *adoption*. Proving the "a data analyst adds a bank by pointing and clicking, not by writing code" claim on a **real person with a real statement** is now the single highest-value move. Surfaces the last UX rough edge (drawing/nudging PDF column boxes) before wider rollout. Observation + small fixes, not new subsystems. |
| 2 | **Air-gapped Windows dry-run** | ★★★★ | ●● | low | Prove the real deployment: unzip + `RUN-ME.bat` on a locked-down, offline Windows box with bundled R + packages, no internet. De-risks install before it's in front of users. Deployment proof, not code. |
| 3 | **More bank templates as real files arrive** (pure YAML) | ★★★★ | ● | ~zero | SBS, TSB, Co-op, Heartland… and more per-bank **PDF** statements - each a template + one golden test, *no engine change*. This **is** the simple-scaling win - the reason the codebase stays small as coverage grows. Gated only on receiving a real sample file. |
| 4 | **Local-ML learning loop** (upgrade the scaffold) | ★★★ | ●●● | med | Today the loop is a **deterministic scaffold**: `lexicon_suggestions()` frequency-ranks the "unrecognised" signal from the local metadata corpus for a human to approve. A local model could later rank/classify that signal better. Real value, but only once the corpus is large enough to justify it - and it stays **proposal-only behind human approval**, never changing engine behaviour directly. |

`★` = value to the forensic-accounting job · `●` = build effort · Maint. = ongoing
cost to keep alive.

## 🧱 Simplicity guardrails (protect these, always)
1. **A new bank = a YAML template, never new code.** If a change would force
   per-bank R code, redesign it as a template option or a named transform.
2. **Two extraction strategies only** - positional bands + anchored/regex.
   Resist adding a third paradigm unless it clearly earns its keep.
3. **Be skeptical of the interactive subsystems** (the visual box editor, the
   `mode: fields` form path). They carry the maintenance weight; keep them
   *writing templates*, not holding their own logic.
4. **R modules stay small + single-concern** (currently 45, each ~one job).
5. **Docs and config are cheap; code is expensive.** Bias to YAML + a short doc
   over another module.

## Recommended sequence
**Validation & adoption → air-gapped Windows dry-run → templates as real files
arrive → local-ML loop (only once the metadata corpus justifies it).**
The engine is done; the value now is proving it lands with a non-technical user,
proving the offline deploy, and letting coverage grow the cheap way - one YAML
template per bank, never new code.
