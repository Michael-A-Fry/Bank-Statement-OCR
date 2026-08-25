# documents_user\ — report templates built on the box

Report templates (`mode: document`) saved from the app's **Add a template** tab
land here as `<id>.yaml`. See `..\documents\README.md` for what a report template
is and what it does instead of reconciliation.

- **Forgiving load:** an invalid template here is skipped rather than breaking the
  others — but never silently. The reason comes back on the load result and the
  app can name the template that vanished and why.
- **Validated before it is written:** `save_document_template()` refuses an
  invalid template, so a half-drawn one never reaches this folder in the first
  place. Overlapping column bands, a table that ends before it starts, a column
  with no width, and a fingerprint made of words every report carries are all
  refused there.
- **Download only:** nothing from a report template reaches the Qlik feed, from
  this folder or from `..\documents\`. See `..\..\R\feed.R`.

**This folder is irreplaceable** — every report puller somebody drew by hand is
in it, and **Admin → Templates does not list them** (document templates are
matched at the front door, not chosen from a list), so a missing one is not
obvious. Back it up (`..\..\docs\operational\backup-and-restore.md`).

Promoting one to a curated template = move its `.yaml` into `..\documents\`.
