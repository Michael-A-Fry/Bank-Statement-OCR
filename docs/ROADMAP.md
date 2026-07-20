# Roadmap — ranked by ROI (value ÷ effort × maintenance cost)

One canonical, prioritised backlog. **Simplicity is a first-class filter here:**
value is discounted by how much ongoing maintenance an item adds, because a
repo that's hard to maintain is a failed one. The design rule that keeps this
whole thing simple — **a new bank is a YAML template, never new code** — is
protected in every ranking decision below.

_Last updated: 2026-07-20._

## ✅ Done (progress so far)
- Pure-R engine: delimited path end-to-end for **6 banks**, golden-file tested.
- Reconciliation KPIs + trust score; **fail-loud diagnostics** (where/why/how/fix)
  in result, workbook, JSON and the app.
- Redaction guard (forensic no-leak); OCR (tesseract/poppler) + pre-processing.
- **Zero-background wizard** (auto-detect delimiter/date/amount, plain English).
- Shiny GUI (convert · wizard · help); onboarding, edge-case register, research.
- Corpus: 46 sample files (incl. 3 real PDF tables + 20 edge-case fixtures).
- Test suite: **390 assertions across 104 tests, 0 failures.**

## ✅ Delivered in the C→B→A pass
- **C** PDF transaction-table parser (declarative `format: pdf`; real ANZ table, tested).
- **2** Cross-bank Xero-standard import template.
- **3** OCR TSV per-word confidence + `low_ocr_confidence` diagnostic.
- **B** Visual PDF wizard (draw column boxes → live preview → generate template).
- **5** Adaptive-threshold (Sauvola) scan profile.
- **6** Excel (.xlsx) path + generic template.
- **A** Key-value (`mode: fields`) extraction foundation for IRD/form docs.
- **cleanup** `debit_credit_cols` + `dr_cr_suffix` promoted to tested.

**Remaining is data-gated, not code:** real native SBS/TSB/Co-op/Heartland export
files; real per-bank PDF statements to add more `format: pdf` templates (each a
wizard session, not engine work); a real IRD document to wire `mode: fields` into
the full output. Categorisation stays a downstream concern.

## 🎯 Backlog — highest ROI first

| # | Item | Value | Effort | Maint. | Why here |
|---|------|-------|--------|--------|----------|
| 1 | **C — PDF text-layer table parser** (digital PDFs, no OCR) | ★★★★★ | ●●● | low* | PDFs are the *dominant real input*; unlocks the whole category. *Stays declarative (`format: pdf` template), so per-bank scaling adds no code. Build once, then it's YAML forever. We have 3 real tables to build against. |
| 2 | **More bank templates (YAML)** | ★★★★ | ● | ~zero | SBS, TSB, Co-op, Heartland… pure config + one golden test each. This *is* the simple-scaling win — the reason the repo stays small as coverage grows. |
| 3 | **OCR `tsv` output + field-confidence gating** | ★★★★ | ●● | low | One tesseract flag unlocks per-word confidence + boxes → feeds diagnostics and the "99.9% correct-or-flagged" goal. Small change, big safety payoff. |
| 4 | **B — visual box/drag wizard for PDFs** | ★★★★ | ●●●● | **high** | Point-and-click PDF templates for a non-technical analyst. High value **but the biggest maintenance risk** (interactive canvas). Do *after* C; keep it as thin as possible — it should only *write* the same `format: pdf` YAML C consumes. |
| 5 | **Scan tuning: adaptive threshold + PSM** | ★★★ | ●● | low | `image_lat` (Sauvola) + PSM 4 for tables. Only helps *scanned* inputs; opportunistic, contained. |
| 6 | **Excel (.xlsx) path + template** | ★★★ | ●● | low | Reader exists; needs a template + parse wiring. Same declarative model. |
| 7 | **A — IRD / key-value document mode** | ★★★ | ●●●● | **high** | A *second extraction paradigm* (forms, not tables). Real value, but real complexity + needs a real IRD PDF. Defer until explicitly wanted; build minimally on the same wizard canvas. |
| 8 | **Robustness cleanup** | ★★ | ● | low | ASB-preamble in the wizard; `debit_credit_cols`/`dr_cr_suffix` fixtures to promote 🟡→✅; password-PDF detect+report. Small completeness. |
| 9 | **Categorisation** | ★★ | ●●● | med | Explicitly downstream / out of MVP; ingest the Auckland analysts' CSV when the time comes. Lowest priority. |

`★` = value to the forensic-accounting job · `●` = build effort · Maint. = ongoing
cost to keep alive.

## 🧱 Simplicity guardrails (protect these, always)
1. **A new bank = a YAML template, never new code.** If a change would force
   per-bank R code, redesign it as a template option or a named transform.
2. **Two extraction strategies only** — positional bands + anchored/regex.
   Resist adding a third paradigm unless it clearly earns its keep (that's the
   bar A must clear).
3. **Be skeptical of subsystems (B, A).** They carry the maintenance weight.
   Build the smallest version that works; make them *write templates*, not hold
   their own logic.
4. **R modules stay small + single-concern** (currently 17, each ~one job).
5. **Docs and config are cheap; code is expensive.** Bias to YAML + a short doc
   over another module.

## Recommended sequence
**C → 2 → 3 → B → (5, 6 as they fit) → A → cleanup.**
C first because it unlocks the real input format; the cheap wins (2, 3) ride
alongside; the expensive UI/paradigm work (B, A) comes only once the value is
proven and stays deliberately thin.
