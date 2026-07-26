# Roadmap - ranked by ROI (value ÷ effort × maintenance cost)

One canonical, prioritised backlog. **Simplicity is a first-class filter here:**
value is discounted by how much ongoing maintenance an item adds, because a
codebase that's hard to maintain is a failed one. The design rule that keeps this
whole thing simple - **a new bank is a YAML template, never new code** - is
protected in every ranking decision below.

_Last updated: 2026-07-26._

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
- **Deterministic Admin analytics** over the run/feedback logs (`R/analytics.R`):
  **template-drift detection** (`template_drift()`) and **unseen-layout clustering**
  (`unsupported_clusters()`), plus feed health and template usage - the two things a
  learning loop was once proposed for, answered with plain, testable functions.
- Shiny GUI (convert · wizard · help); onboarding, edge-case register, research.
- **13 proven, golden-tested templates** shipping in `templates/`: 7 delimited
  (ANZ everyday/credit card, ASB, BNZ, Kiwibank, Westpac, plus a cross-bank
  Xero-standard import), **5 PDF** (`anz_everyday_pdf`, `anz_investmentfunds_pdf`,
  `asb_everyday_pdf`, `tutorial_everyday_pdf`, `westpac_everyday_pdf`) and **1 Excel**
  (`excel_generic_xlsx`). The PDF and Excel extraction paths are covered by golden
  round-trip tests.
- Test suite: **1,585 assertions across 378 tests (59 files), 0 failures, 0 skips** —
  measured 2026-07-26; the runner prints the current figures.

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

`★` = value to the forensic-accounting job · `●` = build effort · Maint. = ongoing
cost to keep alive.

## ❌ Killed - do not re-propose without new evidence

**Local-ML learning loop** *(was item 4; removed 2026-07-26).* Its two headline
uses **already ship, deterministically, and are already on screen in Admin**:

- *template-drift detection* → `template_drift()` in `R/analytics.R` (a sustained
  drop in a template's clean-run rate, surfaced in Admin → Insights);
- *unseen-layout clustering* → `unsupported_clusters()` in `R/analytics.R` (every
  unsupported/failed run grouped by layout signature, biggest gap first, with the
  closest template and an example file).

A model would replace two auditable, testable functions with a probabilistic one
that answers the same questions less defensibly - against the charter's explicit
**"not machine learning / not a guesser"** clause and against guardrail 5 below
(*docs and config are cheap; code is expensive*). The remaining unbuilt ideas from
that item (draft-quality scoring, reconciliation-anomaly hints) are parked with
their reasons in `engine-audit.md` → *Future directions*; the deterministic
suggestion scaffold (`lexicon_suggestions()`, human-approved) stays as it is.

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
arrive.** That is the whole list, and it is deliberately short.
The engine is done; the value now is proving it lands with a non-technical user,
proving the offline deploy, and letting coverage grow the cheap way - one YAML
template per bank, never new code. Nothing on this roadmap adds an R module.
