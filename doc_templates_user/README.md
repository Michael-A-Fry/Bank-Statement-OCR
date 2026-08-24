# User report templates

Report templates (`mode: document`) saved from the app's **Add a template** tab
land here as `<id>.yaml`. See `../doc_templates/README.md` for what a report
template is and what it does instead of reconciliation.

- **Forgiving load:** an invalid template here is skipped rather than breaking the
  others — but never silently. The reason comes back on the load result and the
  app can name the template that vanished and why.
- **Validated before it is written:** `save_document_template()` refuses an
  invalid template, so a half-drawn one never reaches this folder in the first
  place. Overlapping column bands, a table that ends before it starts, a column
  with no width, and a fingerprint made of words every report carries are all
  refused there.
- **Download only:** nothing from a report template reaches the Qlik feed, from
  this folder or from `../doc_templates/`. See `../R/feed.R`.

Promoting one to a curated template = move its `.yaml` into `../doc_templates/`.
