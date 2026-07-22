# Statement Studio — charter (purpose · vision · scope)

The fixed points everything else is measured against. If a change conflicts with
this, the change is wrong.

## Purpose
Statement Studio deterministically converts bank & financial statements
(PDF, CSV, Excel) into clean, checked, structured data for forensic accountants —
it **never guesses**, honours redactions, and flags anything it can't verify, so
every figure can be relied on and defended.

## Primary users (priority order)
1. **Forensic accountants ("Beth")** — careful, non-technical, evidence-minded. The
   correctness and UX bar is set for her.
2. **Financial-crime investigators** — defensible extraction for casework / evidence.
3. **General accounting / finance staff** — routine conversion.

Plus the **Qlik analysts** who consume the governed analytics feed.

## Vision (1–3 years)
The trusted, deterministic **standard** for turning any statement into defensible
data: every bank layout is just a template, dashboards are fed automatically, there
are **zero silent errors**, and one analyst can run and grow it — no engineer
required. Over time it expands beyond statements to **all financial documents**
(IRD forms, summaries, letters) as the org's document-extraction backbone.

Scale to design for: **department** — tens of users, hundreds of statements a week,
with per-user isolation and feed integrity under genuine concurrent use.
Dominant input: **genuinely mixed, heavy on PDFs** (both text-layer and scanned/image).

## Objectives
- **Correctness first — never silently wrong.** A wrong figure that looks right is
  the cardinal failure; better to fail or flag loudly than emit a plausible guess.
- **Any layout via templates, no code.** The template system *is* the product; a
  non-technical analyst adds a bank in minutes.
- **Prove completeness.** Reconcile (opening + transactions = printed closing);
  flag anything that can't be proven.
- **Honour redactions absolutely; keep descriptions verbatim; deterministic**
  (same input + template ⇒ identical output).
- **Governed analytics.** Only clean, reconciled conversions from proven templates
  reach the Qlik dashboards.
- **Air-gapped, pure-R, one-analyst maintainable.**

## What this is NOT
- **Not machine learning / not a guesser** — purely deterministic templates + rules;
  no probabilistic inference of what a value is.
- **Not an accounting / general-ledger system** — it extracts and checks; it does
  not do bookkeeping or treat categorisation as financial truth.
- **Not the system of record** — it converts and feeds; it is not the durable
  archive or source of truth for statements.
- **Not a redactor / de-identifier** — statements arrive already redacted; it only
  ever reads what is visible and never hides or reveals.
- **Not a transaction categoriser (today)** — the extraction core never assigns
  categories. Categorisation is a *planned downstream* capability (step 1: recreate
  the existing maintained-keyword logic; step 2: something better), kept out of the
  extraction core and never guessed. Tracked in `engine-audit.md` → Future directions.

## Operating principles (how it behaves when it matters)
- **Fail-closed when unsure:** emit `NA` + a flag (or `needs_review`), never a
  plausible guess.
- **Verbatim content; deterministic format-only normalisation** of dates/amounts per
  the template's declared rules. No silent content "correction".
- **No silent row drops** — completeness is proven, not assumed.
- Every conversion is logged with a stable run id; nothing that isn't clean +
  reconciled + from a proven template reaches org dashboards.
