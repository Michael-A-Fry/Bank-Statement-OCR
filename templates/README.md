# templates\ - what this server has learned

Since 3.0.0 every bank design the tool knows is a **recipe**: which words
recognise the design, the table's heading, what each column is, how dates and
money are printed. A recipe is never the answer on its own: every reading is
checked against the statement's own arithmetic, every time.

There are two kinds of recipe:

| Where | What | Rules |
|---|---|---|
| `..\recipes\` (in the install) | The **shipped recipes**, one file per design (`anz_cashback_visa.yaml`, ...). | Part of the product. An update replaces them. |
| `recipes\` (here) | **This server's own recipes**: drafts written from a person's check on Please check, and every change an admin makes on **Admin -> Recipes** (on/off, an edit, a merge, accept, retire, undo). Files are `<id>@v<version>.yaml`, for example `anz_draft_1@v1.yaml`; the proof counts are in `recipes\.evidence\`. They hold the table heading and the period label, never a name, a figure or an account number. | Written by the tool, never by hand. A change never edits a file: it writes the next version beside it. A draft reads on its own after **3 checked statements from 2 accounts**, or when an admin accepts it. Where this folder lives is the `paths: recipes:` setting in `config\config.yaml`; without it, it is here, beside `layouts\`. **Irreplaceable: back it up** (`..\docs\operational\backup-and-restore.md`). |

A statement whose design no accepted recipe recognises is **always shown to a
person once** (`auto_reading: unknown_design: ask`, the default). It comes back
filled in, and their "It's right" writes the draft.

`layouts\` is what 2.x learned: one folder per bank of learned layouts, and
`layouts\.pending\` for fixes held for an admin. 3.0.0 still reads them, so
statements that converted on a proven layout still do, but a text PDF or a
spreadsheet now teaches a recipe instead. Keep the folder and keep backing it up.

The folders are created the first time something is learned, so a fresh
install has only this README.

**Never copy `recipes\` or `layouts\` from one install to another.** File names
are the same everywhere (`anz_draft_1@v1.yaml`), so a copy silently replaces
what the other install learned. The offline package never carries them.

The templates that used to ship in `statements\` before 2.0.0 now live in
`tests\testthat\fixtures\templates\`, as test material only. A server updated
from 1.x may still have `statements\`, `statements_seed\` and
`statements_user\` here; none of them is read, and
[the 2.0.0 hand-carry list](../docs/operational/release-2.0.0-hand-carry.md)
says what to keep and what to delete.
