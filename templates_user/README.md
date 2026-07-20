# User templates

Templates created by accountants through the app's **Guided setup** (when a
statement can't be read, the tool pre-fills a template and they confirm + Save)
land here as `<id>.yaml`.

How they differ from the curated templates in `../templates/`:

- **Origin:** loaded with `origin: "user"` (curated ones are `"default"`).
- **Precedence:** a curated (default) template always **wins** an id clash — a
  user template can never shadow a team-blessed one.
- **Forgiving load:** if a user template here is invalid, it is **skipped with a
  warning**, never a hard error, so one bad file can't break everyone's
  conversions. (Curated templates must be valid — that's a hard error.)

Promoting a good user template to a curated default = move its `.yaml` into
`../templates/`, tidy it, and add a golden test (see `../docs/wizard-tutorial.md`).
