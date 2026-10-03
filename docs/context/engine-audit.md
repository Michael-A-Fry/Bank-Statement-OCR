# Engine audit — the decisions it left standing

The engine and platform audit (four independent domain reviews, cross-checked and
re-verified against source, 2026-07-22) found and fixed a set of silently-wrong
paths. **Every prioritised finding from it is resolved**, each with a regression
test. The findings themselves, with evidence, are in
[findings-register.md](findings-register.md); what shipped is in
[`../../CHANGELOG.md`](../../CHANGELOG.md).

What survives here is the part that is still worth reading: **the questions that
were asked and deliberately answered "no", so nobody re-derives them.** Each says
what new fact would change the answer.

---

## Answered deterministically, not with a model

A local-ML learning loop over the metadata corpus was proposed twice. Its two
headline uses are built, tested and on screen in **Admin → Health**, with no
model at all:

- **Layout-drift detection — SHIPPED (`layout_drift()` in `R/analytics.R`).**
  For every learned layout with enough history (a layout's versions count as one
  layout) it splits the runs into earlier and recent by timestamp and compares
  the share that were healthy, meaning proven. A sustained drop of 25 points or
  more is reported with the layout, the run count, both percentages and the
  last-seen date. That is what a bank quietly changing its print looks like: the
  arithmetic stops proving the readings, so they go to a person. It is reported
  *before* an analyst notices. Covered by
  `tests/testthat/test-coverage-drift-retention.R`. (Until 2.0.0 this was
  `template_drift()`, over templates.)
- **Unseen-layout clustering — SHIPPED (`unsupported_clusters()` in
  `R/analytics.R`).** Every run that read nothing, or whose file could not be read,
  is grouped by its PII-light layout signature and ranked biggest group first.
  Each group carries the reader's most common reason, an example filename and the
  last-seen date: the queue of statements the reader cannot yet read. Covered by
  `tests/testthat/test-analytics.R`. `R/batch_audit.R` does the same for a folder
  on demand.

A layout's health and a reading gap are both **countable facts** in the logs.
Counting them is auditable, testable and cheap to keep alive; ranking them with a
model would replace two readable functions with a probabilistic one that answers
the same questions less defensibly — against the charter's explicit *not machine
learning / not a guesser* clause. The deterministic suggestion scaffold
(`lexicon_suggestions()`, frequency-ranked, human-approved) stays as it is.

## Parked — considered, deliberately not built

| Idea | Why it is parked | What would un-park it |
|---|---|---|
| **Draft-quality scoring**: learn which drafted templates went on to reconcile, and warn at save time | **Overtaken at 2.0.0.** Nothing is drafted and nothing is saved by hand any more. A reading is accepted only when the arithmetic proves it, which is the deterministic rule this idea was reaching for. | Nothing. Retired with the drafter. |
| **Reconciliation-anomaly hints** — flag totals far outside an account's own history | Cross-statement history is not this tool's job: it is not the system of record, and an account's "own history" here is only the statements that happen to have been converted on this box. A hint drawn from a partial history is a plausible guess about someone's money. | Nothing here. The full history lives downstream in Qlik; this belongs there, not in the extraction core. |
| **Template synthesis from two examples**: propose bands, date format and amount style from the intersection of two statements | **Overtaken at 2.0.0, by something stricter.** The automatic reader finds the columns from ONE statement's content and proves them with its arithmetic (`R/auto_read*.R`). Statements of one design are grouped into a learned layout only after each has proved itself (`R/layouts.R`). Nothing is inferred from an intersection, so nothing has to be explained when the intersection is wrong. | Nothing. Kept in this table so it is not proposed again. |
| **Deterministic OCR "second reader"** — two OCR engines or preprocessings, trust only where they agree | Contract-wise exactly right, and tempting. Cost-wise it doubles OCR time on the dominant input and adds a second OCR tool to an air-gapped Windows bundle that one analyst has to install and keep alive — for a failure mode that is already caught, not silent: per-cell confidence gating (`ocr_low_conf`), page-mean confidence caveats, and reconciliation over the totals. | Evidence that misreads are slipping past the confidence floor **and** reconciliation on real scanned statements — a demonstrated silent error, not a theoretical one. Cheaper first move: a second *preprocessing* profile through the same engine, which needs no new dependency. |

## Transaction categorisation (planned downstream)

The charter names this as a planned downstream capability, never part of the
never-guess extraction core, and always visibly derived rather than presented as
extracted truth.

- **Step 1 — recreate the existing maintained-keyword logic.** Port the current
  keyword→category list as a deterministic, auditable rule pass over the
  already-extracted descriptions. Same inputs ⇒ same categories; every assignment
  traceable to the keyword that produced it; unmatched ⇒ uncategorised, never
  guessed.
- **Step 2 — something better.** Only once step 1 is trusted: a local,
  explainable model that proposes a category with a confidence and its evidence,
  for a human to accept. Downstream, non-authoritative, local-only.

What the legacy Qlik app derived, as the parity reference to measure step 1
against, is in
[architecture/legacy-qlik-mapping.md](architecture/legacy-qlik-mapping.md).
