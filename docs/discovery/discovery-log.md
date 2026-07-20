# Bank Statement OCR Platform — Discovery Log

> Living requirements & decisions record. Seeded during scoping interviews and
> updated as decisions are confirmed. This file is the source of truth for
> *why* the platform is built the way it is.

_Last updated: 2026-07-20_

---

## 1. Purpose & background

Replace a brittle, per-statement custom-coded conversion process (built with
Qlik ODAG + Inphinity Mole, requiring exact-location/phrase/template code per
statement) with a maintainable, template-driven engine written in **pure R**
(no reticulate, no Python).

The platform is **"the meat in the sandwich"**: a bank/other statement comes in,
it is matched to a template (or flagged as unsupported), and it is converted
into clean, structured, downloadable data. It must outlive Qlik — the future
front-end/analytics tool is unknown, so the engine must be portable and
front-end agnostic.

### Hard constraints
- **Language:** R only. No `reticulate`, no Python, ever.
- **Attribution:** every commit authored as *Michael Fry
  `<95201544+Michael-A-Fry@users.noreply.github.com>`*. No mention of AI /
  assistants / Claude anywhere in commits, code, comments, or docs.
- **Environment:** cannot self-host a Shiny app for users outside the immediate
  team. Core capability must therefore be usable when plugged into whatever
  analytics tool is adopted.

---

## 2. Confirmed decisions — Bank 1 (Foundations)

### 2.1 Primary users & purpose
- Primary users are **forensic accountants**.
- They **download the structured data** and perform analysis in their own
  tools — they do **not** primarily analyse inside this application.
- Implication: the deliverable is **faithful, audit-grade extraction** —
  completeness (no silently dropped transactions), fidelity, and
  **provenance** (traceability of each row back to its source page/row) are
  paramount. In-app analytics are secondary.

### 2.2 Runtime & delivery (dual-purpose, engine-first)
- **Core = a headless, portable R engine** (an R package) producing generic,
  well-documented outputs that *any* analytics tool can ingest.
- A **Shiny app** is a valued front-end **for the internal team** (upload,
  template wizard, review/QA, download) — but it is a thin layer, not the
  product, because it cannot be published to external users.
- The engine must not depend on Shiny to function.

### 2.3 Inputs (the real-world mess)
- Document types: **bank statements, Visa/Mastercard (credit card) statements,
  and other bank-issued statements.**
- Formats: **PDF and Excel.**
- PDF text: a single PDF may contain **both selectable and non-selectable
  text** → OCR fallback is a **day-one** requirement, not a later phase.
- **Redactions** are a first-class concern:
  - can appear in **PDF and Excel**;
  - can be at the **start, middle, or end**, in **any volume**, in
    **inconsistent** places;
  - the engine must **tolerate and flag** them and **must never crash**
    because of them.
- High format variance: different banks, statement types, card schemes (Visa
  vs Mastercard), and **year-to-year layout variations** — all must be
  supported.

### 2.4 Auto-detection
- Today users **manually select** the statement type before upload.
- Target: **auto-detect** the bank/statement type, and **auto-detect
  unsupported formats** (return a clean "unsupported" status rather than
  failing hard).

### 2.5 Logging / audit (first-class subsystem)
- Every request must be logged: **who requested it, what they input, and the
  outcome/status of the output.**
- Purpose is twofold: **audit** *and* **operational failure-mode analysis**
  (understanding what is failing and why).

### 2.6 Output
- A **stable, documented data contract** (structured schema) that the current
  and any future analytics tool can read.
- Plus **generic file outputs** (e.g. CSV/Excel) for any tool, and consumable
  within the internal Shiny app.

---

## 3. Open items (to be resolved in later banks)
- Bank 2 — OCR & parsing engine: redaction behaviour, mixed text-layer
  handling, provenance model, credit-card statement specifics, number/sign
  normalisation.
- Bank 3 — Template system & wizard.
- Bank 4 — Categorisation, reconciliation & manual review.
- Bank 5 — Consistency, maintainability, governance, and non-bank docs (IRD
  etc.).

---

## 4. Interview progress
- [x] Bank 1 — Foundations
- [ ] Bank 2 — OCR & parsing engine
- [ ] Bank 3 — Template system & wizard
- [ ] Bank 4 — Categorisation, reconciliation & review
- [ ] Bank 5 — Consistency, maintainability & governance
