# Roadmap

One prioritised backlog, deliberately short. Each item's value is weighed
against the maintenance it adds, because a codebase that is hard to maintain has
failed. The rule that keeps this list small changed at 2.0.0. It used to be "a
new bank is a YAML template". It is now **a new bank is a pile of its
statements**: the tool reads them from their content, proves each with its own
arithmetic, and learns the bank's layouts. It needs no code and no template.

_Last updated 2026-10-03, at 2.0.0. Test-suite figures quoted anywhere in the
docs are a measurement, not a promise. The current ones come from the last full
run, printed by `tests/run_tests.R` and recorded in
[`../operational/maintaining-the-engine.md`](../operational/maintaining-the-engine.md)._

## Backlog, highest value first

| # | Item | Value | Effort | Maintenance | Why here |
|---|------|-------|--------|-------------|----------|
| 1 | **The independent acceptance run.** Score the realistic holdout set and the 100-statement green-flag set once, and have someone try to break the reader (metamorphic tests, missing pages, swapped columns). | 5 | 1 | none | Spec section 11, step 3. Every 2.0.0 figure so far comes from sets the build was developed against. This is the gate the product owner approves on. |
| 2 | **Close the release blockers.** Get a clean full suite run. (`scripts/health-check.R`, `audit-statement.R`, `bulk-audit.R` and `bundle-offline.R` are fixed.) | 5 | 2 | low | Without it nothing says the engine that ships is the one that was tested. See the 2.0.0 entry in [`../../CHANGELOG.md`](../../CHANGELOG.md). |
| 3 | **The engine faults that could give a wrong figure.** Pick between two date columns by heading or content, never by position. Treat a scanned page whose OCR timed out as missing. | 5 | 2 | low | The only two found that could let a proven reading carry the wrong dates or miss rows. Both are in [outstanding-work.md](outstanding-work.md). |
| 4 | **Reach 95% on each class.** Text PDFs are at 94.5%, scans at 80% and CSV at 4 of 7 on the dev set. The biggest remaining loss on scans is ruled table lines. | 4 | 3 | medium | The product owner's target (spec section 2). Measure against the dev set after every change, and never move AUTO_WRONG off zero to get there. |
| 5 | **Train each bank on the server, and run spot checks from day one.** | 4 | 1 | none | Spec section 9: about 300 clean spot checks are needed to say "under 1% wrong", and about 500 statements to say "at least 95% automatic". Only the server can produce those numbers. |

Value and effort are scored 1 (least) to 5 (most).

**Sequence: acceptance run, then blockers, then the two faults, then the rest.**
Nothing goes to the server before items 1 to 3 are done.

## Killed - do not re-propose without new evidence

**Local-ML learning loop.** Both of its main uses already ship,
deterministically, and both are on screen in Admin -> Health:

- *layout-drift detection*: `layout_drift()` in `R/analytics.R`. It reports a
  sustained drop in the share of a learned layout's statements that prove
  themselves.
- *unseen-layout clustering*: `unsupported_clusters()` in `R/analytics.R`. Every
  run that read nothing is grouped by layout signature, biggest group first,
  with an example file.

Automatic reading did not bring the model back. What the tool "learns" is a
versioned record of readings that its own arithmetic proved. It holds no
weights, and every item can be traced to the statements that proved it and
undone by an admin. A model would replace auditable, testable functions with a
probabilistic one that answers the same questions less defensibly. That goes
against the charter's explicit **not machine learning / not a guesser** clause,
and against guardrail 5 below. The unbuilt ideas from that item are parked with
their reasons and their un-parking conditions in
[engine-audit.md](engine-audit.md).

**Templates, in any form.** Hand-drawn column bands, fingerprint phrases, the
template library and the guided setup were retired at 2.0.0 on measured
evidence. On the realistic dev set the shipped templates read 0 of 128 PDFs, and
a template drafted for each file read 32. Automatic reading reads 121, with no
automatic answer wrong (spec section 9.1).

## Simplicity guardrails (protect these, always)

1. **A new bank needs no code and no template.** If supporting a statement would
   need per-bank R code, the reader is missing something general. Fix it there,
   and measure the fix on every test set.
2. **Geometry proposes, arithmetic decides.** Nothing becomes automatic because
   a heading, a layout or a person said so. It becomes automatic only when the
   statement's own figures prove it, and no other reading does.
3. **Be sceptical of the interactive subsystems** (above all Please check and the
   column editor). They carry the maintenance weight. Keep them feeding the
   reader (`overrides$roles`, `overrides$columns`), never holding their own
   logic.
4. **R modules stay small and single-concern**, one job each. The current set is
   mapped in [architecture/build-contract.md](architecture/build-contract.md)
   section 1, and a test fails if that map and `R/` disagree in either
   direction.
5. **Docs and config are cheap; code is expensive.** Prefer a setting and a
   short doc to another module.
