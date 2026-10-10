# Backup and restore

Most of the app folder can be rebuilt in ten minutes from the package on the
internet PC. **Four folders cannot.** They are what the tool has learned on this
server and what your team has taught it. They exist nowhere else, and nobody
else has a copy.

Back them up once a week, and before every update. It takes five minutes and is
a folder copy.

## What is irreplaceable

| Path (inside the app folder) | What you lose without it |
|---|---|
| `templates\layouts\` | **Every bank layout the tool has learned on this server**, and the person-made fixes waiting for an admin (`templates\layouts\.pending\`). Each layout was proven by your own statements. Rebuilding it means finding those statements again and training each bank from scratch. Until then, statements with no balance of their own go back to *Please check*. **This is the accumulated value of the tool.** |
| `templates\recipes\` | **This server's own recipes** (3.0.0): the drafts written from people's checks on *Please check*, their proof counts (`templates\recipes\.evidence\`), and every change an admin made on **Admin -> Recipes**. The shipped recipes come back with any package; these do not. Without them every design taught on this server is asked about again. (If `config\config.yaml` sets `paths: recipes:`, back up that folder instead.) |
| `dictionaries\` | `labels.yaml` and `lexicon.yaml`: every wording and marker taught in Admin -> Words. Losing these crashes nothing, but statements that read last week can quietly stop being proven, which is worse. (The two bank reference files beside them, `nz_banks.yaml` and `nz_bank_branches.csv`, ship with the tool and are easy to replace, so backing them up does no harm.) |
| `logs\metadata\` | The permanent record of how every conversion went, kept forever and never archived. Admin -> Health is computed from it. Once gone, it cannot be recreated. |
| `logs\tracking\` | The automatic-reading counts: how much was proven, what failed, and every spot-check answer. Admin -> Health -> Automatic reading and the carry-off summary are read from it, and it is the evidence for the 95% target. Codes and counts only, no client data. |

Worth having, easy to live without: `config\config.yaml` (your settings;
`RUN-ME.bat` keeps a copy on the same machine under
`%LOCALAPPDATA%\StatementStudio`, but that copy dies with the server) and
`logs\runs\` (the Admin run history).

**After the 2.0.0 update, keep one last copy of `templates\statements_user\`**
(the templates built in the app before 2.0.0) with the backups, then let the
folder go. 2.0.0 does not read it, but it is the record behind every 1.x
conversion made with one of those templates, and a rollback to 1.x needs it
([rolling-back.md](rolling-back.md)).

**Do not back up** `R-runtime\`, `R-lib\`, `offline\`, `uploads\` or `feed\`.
The first three rebuild from the package, and the last two from re-running
conversions. `uploads\` also holds real client statements, so extra copies of
it are a liability, not an asset.

## Backing up

1. Nobody needs to stop work. Every file here is written whole, one at a time.
   The layout store never edits a file; it only adds new ones.
2. Copy these to your approved backup location, into a **dated** folder, for
   example `\\backup-share\StatementStudio\2026-10-03\`. Dated copies are what
   let you go back to *before* a bad change, not just to the latest state.

```
set "APP=D:\StatementStudio-offline"
set "DEST=\\backup-share\StatementStudio\%DATE:~-4%-%DATE:~3,2%-%DATE:~0,2%"
robocopy "%APP%\templates\layouts"  "%DEST%\templates\layouts"  /E
robocopy "%APP%\templates\recipes"  "%DEST%\templates\recipes"  /E
robocopy "%APP%\dictionaries"       "%DEST%\dictionaries"       /E
robocopy "%APP%\logs\metadata"      "%DEST%\logs\metadata"      /E
robocopy "%APP%\logs\tracking"      "%DEST%\logs\tracking"      /E
robocopy "%APP%\config"             "%DEST%\config" config.yaml
```

`/E` copies the hidden-looking `.pending` folder inside `templates\layouts\`
and `.evidence` inside `templates\recipes\` too. `robocopy` exits with **1** for "files were copied", which is success, not
an error. Anything **8 or above** is a real failure, so read the message.

Keep at least the last four weekly copies, plus one from before each update.

**Check it worked.** Open the newest dated folder. `templates\layouts\` should
hold a folder for each bank the tool has learned, with `.yaml` files in it, and
`dictionaries\labels.yaml` should be there. (On a server that has not learned
anything yet, `templates\layouts\` does not exist, and robocopy says so.) A
backup nobody has ever opened is not a backup.

## Restoring

Restore onto a folder that has already been set up once (`RUN-ME.bat` has run
and the app starts), so the private R and packages are in place.

1. **Stop the app.** Press `Ctrl-C`, or end the scheduled task.
2. Copy back **over** the app folder, replacing what is there:
   `templates\layouts\`, `templates\recipes\`, `dictionaries\`, `logs\metadata\`, `logs\tracking\`,
   `config\config.yaml`.
3. **Start the app.**
4. **Admin -> Recipes** lists every recipe, and Admin -> Health its learned
   layouts. The counts should match what you backed up.
5. Then convert one statement you know is proven, and confirm it still is. That
   proves `dictionaries\` came back too.

A restored layout store is exactly the state it was in when backed up. Every
output stamps the learned state it was read with (`layouts_state` in its JSON),
so a conversion made after the backup was taken can be told apart from one the
restored state would give.

### Rebuilding a lost server from nothing

1. Copy a fresh `StatementStudio-offline` package to the new server,
   double-click `RUN-ME.bat`, and let it finish and start once. This installs the
   private R, the packages and the OCR software, and **seeds** a default
   `config.yaml` and default dictionaries.
2. Stop it.
3. Restore the folders above, overwriting the seeded defaults.
4. Start it, re-open the firewall port, re-register the scheduled task, and
   re-check the admin password:
   [running-and-keeping-it-up.md](running-and-keeping-it-up.md).

### Going back one save

If someone saved a bad wording an hour ago and you have no fresh backup: **every
save first writes the previous contents beside the file as `<name>.bak`**, that
is `dictionaries\labels.yaml.bak` and `dictionaries\lexicon.yaml.bak`. Stop the
app, copy the `.bak` over the `.yaml` (dropping the `.bak`), and start it again.
That is **one step of history only**: a second bad save overwrites the good
`.bak`.

The layout store needs no `.bak`, because it never overwrites anything. To undo
something learned, **Retire** the layout on Admin -> Health. Every change,
including a retirement, is a new version written beside the old one.

## What backup does not cover

- **The statements themselves.** `uploads\` is deliberately excluded. The source
  files live wherever your team keeps case evidence, and that system is the
  record.
- **The Qlik feed.** It is regenerated from conversions, and Qlik keeps its own
  loads.
- **The app itself.** It comes from the package on the internet PC:
  [first-time-setup.md](first-time-setup.md).

Related: [updating.md](updating.md) · [maintaining-the-engine.md](maintaining-the-engine.md)
