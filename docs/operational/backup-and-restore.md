# Backup and restore

Most of the app folder can be rebuilt in ten minutes from the package on the
internet PC. **Five folders cannot.** They are your team's accumulated work, they
exist nowhere else, and nobody has another copy.

Five minutes, once a week, and before every update. It is a folder copy.

## What is irreplaceable

| Path (inside the app folder) | What you lose without it |
|---|---|
| `templates\statements_user\` | Every layout your team taught the tool. Rebuilding one means finding the original statement again and redoing the whole toolkit session. **This is the accumulated value of the tool.** |
| `templates\fields_user\` | The same, for form / IRD labelled-value templates. |
| `templates\documents_user\` | The same, for the report / document pullers built on **Add a template → anything else** — the tables and figures somebody drew out of a report by hand. |
| `dictionaries\` | `labels.yaml` + `lexicon.yaml` — every wording and marker taught in Admin. Losing these crashes nothing: statements that reconciled last week quietly stop reconciling, which is worse. |
| `logs\metadata\` | The permanent record of how every conversion went, kept forever and never archived. Admin → Health, drift detection and the layout-gap queue are computed from it. Once gone it cannot be recreated. |

Worth having, easy to live without: `config\config.yaml` (your settings —
`RUN-ME.bat` keeps a same-machine copy under `%LOCALAPPDATA%\StatementStudio`,
but that dies with the server) and `logs\runs\` (the Admin run history).

**Do not back up** `R-runtime\`, `R-lib\`, `offline\`, `uploads\` or `feed\`.
The first three rebuild from the package, the last two from re-running
conversions. `uploads\` also holds real client statements, so extra copies of it
are a liability, not an asset.

## Backing up

1. Nobody needs to stop work — every file here is written whole, one at a time.
2. Copy these to your approved backup location, into a **dated** folder, e.g.
   `\\backup-share\StatementStudio\2026-07-27\`. Dated copies are what let you go
   back to *before* a bad change, not just to the latest state.

```
set "APP=D:\StatementStudio-offline"
set "DEST=\\backup-share\StatementStudio\%DATE:~-4%-%DATE:~3,2%-%DATE:~0,2%"
robocopy "%APP%\templates\statements_user" "%DEST%\templates\statements_user" /E
robocopy "%APP%\templates\fields_user"     "%DEST%\templates\fields_user"     /E
robocopy "%APP%\templates\documents_user"  "%DEST%\templates\documents_user"  /E
robocopy "%APP%\dictionaries"               "%DEST%\dictionaries"               /E
robocopy "%APP%\logs\metadata"              "%DEST%\logs\metadata"              /E
robocopy "%APP%\config"                     "%DEST%\config" config.yaml
```

`robocopy` exits **1** for "files were copied" — that is success, not an error.
Anything **8 or above** is a real failure; read the message.

Keep at least the last four weekly copies plus one from before each update.

**Check it worked.** Open the newest dated folder: `templates\statements_user\`
should hold a `.yaml` for each bank layout your team has made,
`templates\documents_user\` one for each report puller, and
`dictionaries\labels.yaml` should be there. A backup nobody has ever opened is
not a backup.

## Restoring

Restore onto a folder that has already been set up once (`RUN-ME.bat` has run and
the app starts), so the private R and packages are in place.

1. **Stop the app** — `Ctrl-C`, or end the scheduled task.
2. Copy back **over** the app folder, replacing what is there:
   `templates\statements_user\`, `templates\fields_user\`, `templates\documents_user\`,
   `dictionaries\`, `logs\metadata\`, `config\config.yaml`.
3. **Start the app.**
4. Check, in one command: `Rscript scripts\health-check.R` (the exact line for the
   server's private R is in
   [maintaining-the-engine.md](maintaining-the-engine.md) §1). It counts the
   templates that loaded, **all three kinds** — bank statement, form, report — and
   names any that were refused. The counts should match what you backed up.
5. Then convert one statement you know reconciles and confirm it still does. That
   is the half health-check cannot do: it proves `dictionaries\` came back too.
   **Admin → Templates** lists all three kinds by name if you want to read them.

### Rebuilding a lost server from nothing

1. Copy a fresh `StatementStudio-offline` package to the new server, double-click
   `RUN-ME.bat`, let it finish and start once. This installs the private R,
   packages and OCR, and **seeds** a default `config.yaml` and default
   dictionaries.
2. Stop it.
3. Restore the folders above, overwriting the seeded defaults.
4. Start it, re-open the firewall port, re-register the scheduled task, and
   re-check the admin password —
   [running-and-keeping-it-up.md](running-and-keeping-it-up.md).

### Going back one save

If someone saved a bad wording — or a bad template — an hour ago and you have no
fresh backup: **every save first writes the previous contents beside the file as
`<name>.bak`**. That is `dictionaries\labels.yaml.bak` and
`dictionaries\lexicon.yaml.bak`, and now also a `.yaml.bak` beside every template
in `templates\statements_user\`, `templates\fields_user\` and
`templates\documents_user\`. Stop the app, copy the `.bak` over the `.yaml`
(dropping the `.bak`), start it again. **One step of history only** — a second bad
save overwrites the good `.bak`.

A `.yaml.<number>.part` file is a save that died half-way. Nothing loads it and
nothing loads a `.bak`, so neither can ever be read as a template; leave them
where they are.

## What backup does not cover

- **The statements themselves.** `uploads\` is deliberately excluded; the source
  files live wherever your team keeps case evidence, and that system is the
  record.
- **The Qlik feed.** Regenerated from conversions; Qlik keeps its own loads.
- **The app itself.** It comes from the package on the internet PC —
  [first-time-setup.md](first-time-setup.md).

Related: [updating.md](updating.md) · [maintaining-the-engine.md](maintaining-the-engine.md)
