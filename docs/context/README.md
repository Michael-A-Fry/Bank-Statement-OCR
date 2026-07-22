# Context — background and reference

The "good to know" material: **what was built, how we got here, what was researched,
and the honest limits.** None of this tells you how to *do* a task — for that, see
[`../operational/`](../operational/README.md).

## Start here
| Doc | What it is |
|---|---|
| [launch-audit.md](launch-audit.md) | Readiness: what's ready, the honest boundaries, and the go/no-go. |
| [edge-cases.md](edge-cases.md) | Real-world edge cases and known limits — what the tool handles and what it doesn't. |
| [roadmap.md](roadmap.md) | The prioritised backlog, ranked by value ÷ effort. |

## How we got here
| Doc | What it is |
|---|---|
| [discovery/discovery-log.md](discovery/discovery-log.md) | The requirements and decisions history — every constraint and why. |
| [template-suite-plan.md](template-suite-plan.md) | The plan for building out the bank-template suite. |
| [wizard-tutorial.md](wizard-tutorial.md) | A detailed walkthrough of the template wizard (the deep version of [adding-a-bank-template](../operational/adding-a-bank-template.md)). |
| [wizard-vision-and-roadmap.md](wizard-vision-and-roadmap.md) | The design thinking behind the visual wizard, and its A/B/C roadmap. |

## How it's built (architecture)
| Doc | What it is |
|---|---|
| [architecture/build-contract.md](architecture/build-contract.md) | The template format and the full data contract every conversion honours. |
| [architecture/deployment-integration-plan.md](architecture/deployment-integration-plan.md) | Server deployment, concurrency, and access-control design. |
| [architecture/qlik-sense-integration.md](architecture/qlik-sense-integration.md) | The Qlik architecture: the app converts, Qlik loads the analytics feed. |
| [architecture/qlik-options-analysis.md](architecture/qlik-options-analysis.md) | The Qlik-side options that were weighed, and why the chosen shape won. |
| [architecture/legacy-qlik-mapping.md](architecture/legacy-qlik-mapping.md) | What the legacy Qlik app produced, as a reference to measure against. |

## Research
| Doc | What it is |
|---|---|
| [research/ocr-preprocessing.md](research/ocr-preprocessing.md) | OCR pre-processing research — how scanned-page accuracy is lifted. |
| [research/open-source-landscape.md](research/open-source-landscape.md) | Survey of the open-source tools considered, and what was borrowed vs. built. |
