# fields_user\ — form templates built on the box

Form templates (`mode: fields`) built on the app's **Add a template** tab land
here as `<id>.yaml`. A form template names labelled figures — "IRD number",
"total contributions" — rather than a table of transactions, so nothing here
reconciles and nothing here reaches the Qlik dashboards.

Curated form templates live beside this folder in `..\fields\`. As everywhere
else in `templates\`: a curated template wins an id clash, an invalid curated
template is a hard error, and an invalid one **here** is skipped with a reason so
one bad file cannot stop everybody else converting.

**This folder is irreplaceable** — back it up
(`..\..\docs\operational\backup-and-restore.md`).

Promoting one to a curated template = move its `.yaml` into `..\fields\`.
