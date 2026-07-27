# User templates

Templates created by accountants on the app's **Add a template** tab (the toolkit
pre-fills a template from their own file, they read the preview and Save) land
here as `<id>.yaml`.

How they differ from the curated templates in `../templates/`:

- **Origin:** loaded with `origin: "user"` (curated ones are `"default"`).
- **Precedence:** a curated (default) template always **wins** an id clash — a
  user template can never shadow a team-blessed one.
- **Forgiving load:** if a user template here is invalid, it is **skipped with a
  warning**, never a hard error, so one bad file can't break everyone's
  conversions. (Curated templates must be valid — that's a hard error.)

Promoting a good user template to a curated default = move its `.yaml` into
`../templates/`, tidy it, and add a golden test. Both moves, never one: only
`../templates/` feeds the Qlik dashboards, and a template dropped in there with
no golden test fails the suite on purpose.

- The recipe, step by step for the delimited and the PDF path:
  `../tests/HOWTO-add-template-test.md`.
- Who does it and what to check first:
  `../docs/operational/maintaining-the-engine.md` §3.
