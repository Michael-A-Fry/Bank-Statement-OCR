# Roadmap

One prioritised backlog, deliberately short. Value is discounted by the ongoing
maintenance an item adds, because a codebase that is hard to maintain is a failed
one. The rule that keeps this small — **a new bank is a YAML template, never new
code** — wins every ranking argument below.

_Last updated 2026-07-27. Test-suite figures quoted anywhere in the docs are a
measurement, not a promise; the current ones come from the last full run, printed
by `tests/run_tests.R` and recorded in
[`../operational/maintaining-the-engine.md`](../operational/maintaining-the-engine.md)._

## Backlog — highest value first

| # | Item | Value | Effort | Maintenance | Why here |
|---|------|-------|--------|-------------|----------|
| 1 | **Validation & adoption** — watch a real non-technical analyst build a template end to end on a real statement | ★★★★★ | ● | ~zero | The engine is built; the open question is *adoption*. Proving "an analyst adds a bank by pointing and clicking" on a real person with a real file is the highest-value move left. It also settles whether drawing PDF column bands is the actual bottleneck — which is the evidence that would un-park template synthesis ([engine-audit.md](engine-audit.md)). Observation and small fixes, not a new subsystem. |
| 2 | **Air-gapped Windows dry-run** | ★★★★ | ●● | low | Prove the real deployment: copy the folder onto a locked-down offline Windows box, `RUN-ME.bat`, set the password, open the port, register the task. De-risks the install before it is in front of users. Deployment proof, not code. |
| 3 | **More bank templates as real files arrive** (pure YAML) | ★★★★ | ● | ~zero | SBS, TSB, Co-op, Heartland, and more per-bank PDFs — each a template plus one golden test, no engine change. This **is** the simple-scaling win. Gated only on receiving a real sample. |

`★` = value to the forensic-accounting job · `●` = build effort.

**Sequence: validation → dry-run → templates as they arrive.** The engine is
done. Nothing on this list adds an R module.

## Killed - do not re-propose without new evidence

**Local-ML learning loop.** Both of its headline uses already ship,
deterministically, and are already on screen in Admin:

- *template-drift detection* → `template_drift()` in `R/analytics.R` — a sustained
  drop in a template's clean-run rate, surfaced in Admin → Insights;
- *unseen-layout clustering* → `unsupported_clusters()` in `R/analytics.R` — every
  unsupported/failed run grouped by layout signature, biggest gap first, with the
  closest template and an example file.

A model would replace two auditable, testable functions with a probabilistic one
that answers the same questions less defensibly — against the charter's explicit
**not machine learning / not a guesser** clause, and against guardrail 5 below.
The unbuilt ideas from that item are parked with their reasons and their
un-parking conditions in [engine-audit.md](engine-audit.md).

## Simplicity guardrails (protect these, always)

1. **A new bank = a YAML template, never new code.** If a change would force
   per-bank R code, redesign it as a template option or a named transform.
2. **Two extraction strategies only** — positional bands, and anchored/regex.
   Resist a third paradigm unless it clearly earns its keep.
3. **Be sceptical of the interactive subsystems** (the visual band editor, the
   `mode: fields` form path). They carry the maintenance weight; keep them
   *writing templates*, never holding their own logic.
4. **R modules stay small and single-concern** — one job each. The current set is
   mapped in [architecture/build-contract.md](architecture/build-contract.md) §1,
   and a test fails if that map and `R/` disagree in either direction.
5. **Docs and config are cheap; code is expensive.** Bias to YAML plus a short
   doc over another module.
