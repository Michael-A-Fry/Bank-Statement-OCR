# templates\ — every template, in one place

A **template** is what tells the tool how to read one layout: where the columns
are, what the dates look like, which words identify it. Nothing here is code.
Every file is a `.yaml` an analyst can read, and almost all of them are written by
the app rather than by hand.

There are seven folders and they answer two questions: **what kind of document is
it**, and **who wrote the template**.

## What kind of document

| Kind | Folders | What it is |
|---|---|---|
| **statements** | `statements\`, `statements_user\`, `statements_seed\` | A bank or card statement: one long table of transactions with a running balance. The only kind that **reconciles**, and the only kind that can reach the Qlik dashboards. |
| **fields** | `fields\`, `fields_user\` | A form: labelled figures with no transaction table — an IRD summary, a KiwiSaver statement. |
| **documents** | `documents\`, `documents_user\` | Anything else: a report carrying many tables of different shapes, a schedule, a valuation. Download only — nothing here reaches the dashboards. |

The folder name is the template's `mode:` — `statement`, `fields`, `document`. A
template can never be read as the wrong kind, because the two are the same fact.

## Who wrote it

| Suffix | Who | Rules |
|---|---|---|
| *(none)* — `statements\`, `fields\`, `documents\` | **The team.** Curated, tested, shipped in the package. | A template here must be valid: an invalid one is a hard error, not a skip. Only `statements\` feeds Qlik. |
| `_user` — `statements_user\`, `fields_user\`, `documents_user\` | **Whoever built it in the app**, on the box. | Never overwritten by an update. Never reaches Qlik. An invalid one is skipped with a reason, so a single bad file cannot stop everybody else converting. **Irreplaceable — back these up** (`..\docs\operational\backup-and-restore.md`). |
| `_seed` — `statements_seed\` | Nobody yet. **Unfinished skeletons**, positions not set. | **Not loaded by the app at all.** A seed is a head start for a person, not a template. |

A curated template always wins an id clash, so a user template can never quietly
shadow a team-blessed one.

## Promoting one

Move the `.yaml` up from the `_user` folder into the curated folder beside it —
`statements_user\` → `statements\`, and so on. For a statement template, add a
golden test in the same move: only `statements\` feeds the dashboards, and a
template dropped in there without a test fails the suite on purpose.

- The recipe: `..\tests\HOWTO-add-template-test.md`
- Who does it, and what to check first:
  `..\docs\operational\maintaining-the-engine.md` §3

## Which folder does a change go in?

Almost never one you edit by hand. Templates are built on the app's **Add a
template** tab and saved into the matching `_user` folder for you. Editing the
YAML directly is the maintainer's path, not the analyst's.
