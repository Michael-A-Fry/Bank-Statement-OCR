# Context — how it works, and why

Reference for whoever owns or changes the engine. None of this tells you how to
*do* a task — for that, see [`../operational/`](../operational/README.md).

**Just inherited it?** [charter.md](charter.md), then
[how-it-fits-together.md](how-it-fits-together.md), then
[`../operational/maintaining-the-engine.md`](../operational/maintaining-the-engine.md).

| Doc | What it is, and when you read it |
|---|---|
| [charter.md](charter.md) | **Read first.** Purpose, users, scope and the non-negotiables. If a change conflicts with the charter, the change is wrong. |
| [how-it-fits-together.md](how-it-fits-together.md) | **Read second.** One page: how a statement flows from upload to dashboard, which module owns each step, where the on-screen wording lives, and where each kind of change goes. |
| [architecture/build-contract.md](architecture/build-contract.md) | The data contract every conversion honours — transaction schema, flags vocabulary, template YAML spec, statuses, output artefacts. Check it before changing anything the engine emits. |
| [engine-parameters.md](engine-parameters.md) | Every numeric threshold, what it decides, and the effect of moving it. Read before editing `R/params.R`. |
| [metadata-capture.md](metadata-capture.md) | What the local-only metadata corpus stores per run, and the per-level privacy notes. Read before answering "what does this tool keep about our clients?". |
| [edge-cases.md](edge-cases.md) | Real-world statement edge cases, each with an honest status. Read to decide whether an odd statement is a known limit or a bug. |
| [outstanding-work.md](outstanding-work.md) | **The live register.** Everything raised in review that is not yet done, in the words it was raised in, with the design already agreed for each. Leads with the faults that produce a wrong figure. Start here before picking up work. |
| [findings-register.md](findings-register.md) | Every defect found by review or real use, with its evidence and whether it is fixed. The audit trail behind the changelog. |
| [findings-2026-08-26.md](findings-2026-08-26.md) | The investigation behind the register: thirteen faults root-caused against the real code and reproduced, each with its exact edit sites. Most are now fixed; the page is kept because it is the evidence for *why* they were fixed that way. |
| [ird-document-plan.md](ird-document-plan.md) | Generalising `mode: document` for IRD reports: the running score against the source analysis. What is built (page furniture, order-independent section boundaries), what is **not** (period matrices, row reconstruction, components), and why no IRD template ships yet. |
| [roadmap.md](roadmap.md) | The short prioritised backlog, the simplicity guardrails, and what was killed. |
| [engine-audit.md](engine-audit.md) | What the engine audit left standing: the ideas deliberately **not** built, each with the fact that would change the answer. Read before proposing one of them again. |
| [architecture/legacy-qlik-mapping.md](architecture/legacy-qlik-mapping.md) | What the legacy Qlik tool produced, kept as the parity reference for downstream categorisation. Cited as evidence by the findings register. |
