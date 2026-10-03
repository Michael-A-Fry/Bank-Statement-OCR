# templates\ - the bank layouts the tool has learned

Statement templates are gone (2.0.0). The tool reads every statement from its
**content** and proves the reading with the statement's own arithmetic: the
running balance, and opening plus movements equals closing. Nobody draws
columns or picks a template any more. You pick the **bank** (the tool pre-fills
it from the statement) and convert.

What the tool remembers about each bank lives here, in `layouts\`:

| Folder | What | Rules |
|---|---|---|
| `layouts\<bank>\` | **Learned layouts**, one folder per bank (`anz`, `westpac`, ...). Each file is one version of one layout: `<bank>_<n>@v<version>.yaml`, for example `anz_1@v3.yaml`. A file holds the column roles, date and money styles, heading words and relative column positions. It holds no names, figures or account numbers. | Written by the tool, never by hand. A layout starts **provisional** and becomes **proven** after three statements prove themselves, or when an admin confirms it. A change never edits a file: it writes the next version beside it. So every output, which is stamped with the learned state it was read against, can be traced to exactly what had been learned when it was made. **Irreplaceable: back it up** with the logs (`..\docs\operational\backup-and-restore.md`). |
| `layouts\.pending\` | A person's word that the arithmetic could not back: a fix on Please check that did not prove, or a reading confirmed with *This is right*. Held for an admin to **accept** or **discard**. | Never read as a layout, and not part of the learned state. It applies to the one file it was made on until an admin accepts it. |

The folder is created the first time a statement is learned, so a fresh install
has only this README. Where it lives is the `paths: layouts:` setting in
`config\config.yaml`.

An admin can confirm, rename or retire any learned layout on **Admin -> Banks**,
and accept or discard a held fix there. Retiring keeps the files, so nothing
already issued changes.

**Never copy a `layouts\` folder from one install to another.** File names are
the same everywhere (`anz\anz_1@v1.yaml`), so a copy silently replaces what the
other install learned.

The templates that used to ship in `statements\` now live in
`tests\testthat\fixtures\templates\`, as test material for the engine's table
reader only. The app does not read them. A server updated from 1.x still has
`statements\`, `statements_seed\` and `statements_user\` here. None of them is
read any more, and
[the 2.0.0 hand-carry list](../docs/operational/release-2.0.0-hand-carry.md)
says what to keep and what to delete.
