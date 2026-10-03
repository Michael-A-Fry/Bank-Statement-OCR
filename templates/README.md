# templates\ - the bank layouts the tool has learned

Statement templates are gone. The tool now reads every statement from its
**content** and proves the reading with the statement's own arithmetic (the
running balance; opening plus movements equals closing). Nobody draws columns or
picks a template any more: you pick the **bank** (the tool pre-fills it from the
statement) and convert.

What the tool remembers about each bank lives here, in `layouts\`:

| Folder | What | Rules |
|---|---|---|
| `layouts\<bank>\` | **Learned layouts**, one folder per bank. Each file is one version of one layout: `<bank>_<n>@v<version>.yaml`. | Written by the tool, never by hand. A layout starts **provisional** and becomes **proven** after three statements prove themselves (or an admin confirms it). A change never edits a file: it writes the next version, so every output can be traced to exactly what had been learned when it was made. **Irreplaceable - back it up** with the logs (`..\docs\operational\backup-and-restore.md`). |
| `layouts\.pending\` | A person's fix the arithmetic could not prove, held for an admin to confirm or discard. | Never read as a layout. Applies to the one file it was made on until an admin confirms it. |

The folder is created the first time a statement is learned, so a fresh install
has only this README. Where it lives is the `paths: layouts:` setting in
`config\config.yaml`.

An admin can confirm, rename or retire any learned layout on **Admin -> Banks**;
retiring keeps the files, so nothing already issued changes.

The templates that used to ship in `statements\` now live in
`tests\testthat\fixtures\templates\`, as test material for the engine's table
reader only. The app does not read them.
