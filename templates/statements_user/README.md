# statements_user\ — statement templates built on the box

Templates created by accountants on the app's **Add a template** tab (the toolkit
pre-fills a template from their own file, they read the preview and Save) land
here as `<id>.yaml`.

How they differ from the curated templates in `..\statements\`:

- **Origin:** loaded with `origin: "user"` (curated ones are `"default"`).
- **Precedence:** a curated (default) template always **wins** an id clash — a
  user template can never shadow a team-blessed one.
- **Forgiving load:** if a user template here is invalid, it is **skipped with a
  warning**, never a hard error, so one bad file can't break everyone's
  conversions. (Curated templates must be valid — that's a hard error.)

**This folder is irreplaceable.** It is your team's accumulated work and exists
nowhere else — back it up (`..\..\docs\operational\backup-and-restore.md`).

Promoting a good user template to a curated default = move its `.yaml` into
`..\statements\`, tidy it, and add a golden test. Both moves, never one: only
`..\statements\` feeds the Qlik dashboards, and a template dropped in there with
no golden test fails the suite on purpose.

- The recipe, step by step for the delimited and the PDF path:
  `..\..\tests\HOWTO-add-template-test.md`.
- Who does it and what to check first:
  `..\..\docs\operational\maintaining-the-engine.md` §3.
