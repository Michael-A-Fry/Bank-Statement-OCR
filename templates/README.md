# templates\ — every template, in one place

A **template** is what tells the tool how to read one bank statement layout:
where the columns are, what the dates look like, which words identify it.
Nothing here is code. Every file is a `.yaml` an analyst can read, and almost all
of them are written by the app rather than by hand.

There are three folders and they answer one question: **who wrote the template**.

| Folder | Who | Rules |
|---|---|---|
| `statements\` | **The team.** Curated, tested, shipped in the package. | A template here must be valid: an invalid one is a hard error, not a skip. This is the only folder that feeds the Qlik dashboards. |
| `statements_user\` | **Whoever built it in the app**, on the box. | Never overwritten by an update. Never reaches Qlik. An invalid one is skipped with a reason, so a single bad file cannot stop everybody else converting. **Irreplaceable — back these up** (`..\docs\operational\backup-and-restore.md`). |
| `statements_seed\` | Nobody yet. **Unfinished skeletons**, positions not set. | **Not loaded by the app at all.** A seed is a head start for a person, not a template. |

A curated template always wins an id clash, so a user template can never quietly
shadow a team-blessed one.

## Promoting one

Move the `.yaml` up from `statements_user\` into `statements\`, and add a golden
test in the same move: only `statements\` feeds the dashboards, and a template
dropped in there without a test fails the suite on purpose.

- The recipe: `..\tests\HOWTO-add-template-test.md`
- Who does it, and what to check first:
  `..\docs\operational\maintaining-the-engine.md` §3

## Which folder does a change go in?

Almost never one you edit by hand. Templates are built on the app's **Add a
template** tab and saved into `statements_user\` for you. Editing the YAML
directly is the maintainer's path, not the analyst's.
