# Updating a server from 2.x to 3.0.0 (recipes)

3.0.0 reads every bank design the tool knows with a **recipe**, and proves every
reading with the statement's own arithmetic, as 2.x did. This page says, in plain
words, what changes for the accountants and the admins, the two new settings,
where the new files live, how to update, how to put 2.x back, and how to run the
new tool side by side with the QVF. The full list of changes is the 3.0.0 entry in
[../../CHANGELOG.md](../../CHANGELOG.md).

**Good news first: 3.0.0 deletes nothing.** Every 2.x file is still used or is
harmless, so the normal package update in [updating.md](updating.md) is all it
takes. Nothing in `config\config.yaml` has to change.

---

## What changes for the accountants

- **One sentence, one table, three buttons.** The result says *Done* (with the
  number of transactions and "the balance adds up") or *Needs you* (and where the
  balance stops adding up). The table is laid out like the QVF's: Date,
  Description, Money out, Money in, Balance and a **Check** tick or cross per row.
  The buttons are *Read it again*, *It's right - accept it* and *Set aside*.
  Everything else (checks, diagnostics, charts) is behind one *More detail* link.
- **A new design is always shown once.** A statement whose design the tool has
  not been taught comes back as *Needs you*, already filled in, even when it adds
  up. Looking it over and pressing *It's right* teaches the design. The next
  statements of it come back filled in too, and after **3 checked statements from
  2 different accounts** they are read on their own.
- **Plain questions, only when needed.** When the tool is unsure what a column
  is, it asks one plain question per column ("Is this column money out?"),
  shown with that column's own lines.
- **Progress inside a file.** The queue shows the stage and the page, for example
  *File 2 of 5 - reading the table, page 37 of 150*.
- **Downloads have more columns.** Excel and CSV now carry the account number and
  account name on every row, the full description as printed, and separate
  Type, Particulars, Code and Reference where the statement prints them.
- **Two recipes that disagree go to a person.** If two recipes both read a
  statement and both add up but give different figures, the person picks one
  (side by side, with the row counts and totals). If they agree, the newer one is
  used.

## What changes for the admins

Admin has four tabs: **Needs attention**, **Recipes**, **Words** and **Health**.
The *Banks* tab is gone: its job is done by Recipes. You never see YAML.

- **Needs attention**: statements that need a look, drafts waiting to be
  accepted, recipes failing lately (a bank may have changed its design), and
  suggested merges. Each has one button: *Fix*, *Accept*, *Retire* or *Merge*.
- **Recipes**: one row per recipe with an **On/Off** switch, how many statements
  it read and how many needed help in the last 30 days, and whether it is a draft
  or proven. Click a row for the recipe card: change an answer, press **Test**
  (it re-reads every kept statement this recipe read and says whether they all
  still add up with the same figures), then **Save**. Every save is a new version;
  *Use this version* undoes. A change that would break or change an earlier
  statement is refused, with the reason.
- **New recipe from a statement**: upload one statement, answer the pre-filled
  questions, *Read it*, *Save*.
- Nothing is ever deleted. Off means "not tried"; Retire means Off and hidden.

## The two new settings

Both have working defaults, so you do not have to add either.

| Setting in `config\config.yaml` | Default | What it does |
|---|---|---|
| `auto_reading:` `unknown_design:` | `ask` | `ask`: a statement whose design no accepted recipe knows is never converted without a person (see above). `auto`: such a statement converts on its arithmetic alone, as 2.x did. Keep `ask`; it is the owner's rule. |
| `paths:` `recipes:` | `templates/recipes` | The folder for **this server's own recipes**: drafts written from people's checks, their proof counts, and every admin change. Set it only to keep them somewhere else. |

`config\config.example.yaml` shows both, with an explanation.

## Where things live

| Folder | What | An update... |
|---|---|---|
| `recipes\` (in the install) | The **shipped recipes**, one file per design. | replaces it. It is product. |
| `templates\recipes\` (or `paths: recipes:`) | **This server's own recipes**: drafts (`anz_draft_1@v1.yaml`), admin changes (a new version each time), and the proof counts in `.evidence\`. No names, figures or account numbers. | **never touches it.** If an update ships a recipe with the same name and version number as one of yours, yours is used. |
| `templates\layouts\` | What 2.x learned. Still read, so statements that converted on a proven layout still do; a text PDF or spreadsheet now teaches a recipe instead. | never touches it. |

**Add `templates\recipes\` to the backup now**, beside `templates\layouts\`
([backup-and-restore.md](backup-and-restore.md) has the line). It is created the
first time somebody checks a new design, and it exists nowhere else.

## The update, step by step (about 15 minutes)

1. **Back up** ([backup-and-restore.md](backup-and-restore.md)) and keep the 2.x
   package folder: it is your way back.
2. **Stop the app** (`Ctrl-C`, or end the scheduled task) and tell the team.
3. **Build the 3.0.0 package** on the internet PC (`make-bundle.bat`) and copy it
   over the server's folder, choosing **Replace the files in the destination**
   ([updating.md](updating.md)). The package never carries `templates\layouts\`,
   `templates\recipes\`, `config\config.yaml` or your `labels.yaml` and
   `lexicon.yaml`, so nothing of yours is replaced.
4. **Start it** with `RUN-ME.bat` or the scheduled task.
5. **Prove it:**
   - `scripts\health-check.R` ([maintaining-the-engine.md](maintaining-the-engine.md) §1):
     every line `PASS`.
   - Convert `samples\raw\tutorial\sample_everyday_statement.pdf`. It is a
     made-up bank, so with `unknown_design: ask` it comes back *Needs you* with
     "the tool has not seen this statement design before". That is right.
   - Open its JSON download: `build.engine_version` reads `3.0.0`.
   - Open Admin: the tabs are Needs attention, Recipes, Words and Health, and
     Recipes lists the shipped recipes.
6. Tell the accountants the four bullets at the top of this page.

The words your team taught (`labels.yaml`, `lexicon.yaml`) are kept as they are.
3.0.0's copies have more general wordings and a plain explanation at the top;
the reader has the important ones built in, so the server's own copies need
nothing. To have them, teach them from Admin -> Words.

## Putting 2.x back

1. **Stop the app.**
2. Copy the **2.x package folder** back over the server's folder (Replace the
   files). [rolling-back.md](rolling-back.md) §2 has the details and the warning
   about using a copy of the live folder instead.
3. **Leave `templates\recipes\` where it is.** 2.x does not read it, and it is
   what 3.0.0 learned: if you update again later, it is used again.
   `templates\layouts\` works with both versions.
4. **Delete the `recipes\` folder from the install** after the copy, or leave it:
   2.x never reads it.
5. Start the app and run step 5 above; the JSON must read your 2.x version.

The anonymous tracking lines 3.0.0 wrote (`logs\tracking\`) stay readable by
2.x: it counts the conversions, and counts 3.0.0's timing lines as "unreadable
lines" instead of statements. Nothing to clean up. If 3.0.0 already sent wrong
rows anywhere, [rolling-back.md](rolling-back.md) §4 applies as it always has.

## Side by side with the QVF

The plan is to move one bank type over at a time, once the new tool matches or
beats the QVF on your own statements. Two scripts help, and both print **counts
only** (no names, numbers, descriptions or figures), so the printout is safe to
share. Run them on your own PC from the app folder, with `Rscript` on the path.

**1. Does the new tool read your real statements on its own?**

```
Rscript tools\recipes\try-real.R "C:\path\to\a folder of statements"
```

One numbered line per file: the kind, pages, the recipe that matched, the
result (*automatic*, *check* or *unread*) and the reason, then a count per
recipe. Nothing is written anywhere except a temporary folder that is deleted
at the end. A type is ready to move when it reads at least as often as the QVF
does, with no wrong figures.

**2. Does it give the same transactions as the QVF?** For one statement converted
by both:

```
Rscript tools\compare-qvf.R "<the QVF's .csv or .xlsx>" "<this tool's .csv or .xlsx>"
```

It prints the rows in each, money out and money in totals in each, the rows that
match on date and amount, and the rows found by only one of them. Matching
totals and no rows found by only one means the two agree.

Note: `tools\` is not part of the server package. Run these from a full copy of
the app folder on your own PC.
