# Backup and restore

Most of the app folder can be rebuilt in ten minutes from the package on the
internet PC. **Four folders cannot.** They are the work your team has put in, they
exist nowhere else, and nobody else has a copy. This page is how you get them off
the server and how you put them back.

Five minutes, once a week (or before every update). No tools, no scripts — it is a
folder copy.

---

## What is irreplaceable

| Path (inside the app folder) | What you lose without it |
|---|---|
| `templates_user\` | Every bank layout your team taught the tool in the wizard. Rebuilding one means finding the original statement again and redoing the whole wizard. **This is the accumulated value of the tool.** |
| `fields_templates_user\` | The same, for form / IRD key-value templates. |
| `dictionaries\` | `labels.yaml` + `lexicon.yaml` — every wording and recognition marker taught in Admin. Losing these doesn't crash anything: statements that reconciled last week quietly stop reconciling, which is worse. |
| `logs\metadata\` | The permanent record of how every conversion went (marked *retain forever* — the app never archives or deletes it). It is what Insights, drift detection and the layout-gap queue are computed from. Once gone, that history cannot be recreated. |

Worth having, easy to live without:

| Path | Why |
|---|---|
| `config\config.yaml` | Your settings. `RUN-ME.bat` already keeps a same-machine copy under `%LOCALAPPDATA%\StatementStudio` — but that dies with the server, so include it. |
| `logs\runs\` | Per-conversion run records (the Admin run history). |

**Do not back up** `R-runtime\`, `R-lib\`, `offline\`, `uploads\` or `feed\`:
they are large, and they are all rebuildable — the first three from the package, the
last two by re-running conversions. `uploads\` also holds real statements, so keeping
extra copies of it is a liability, not an asset.

---

## Backing up (do this before every update)

1. Make sure nobody is mid-conversion (the app can stay running; these files are
   written whole, one file at a time).
2. On the server, open the app folder and copy these four folders plus the config
   file to your approved backup location — a network share, or a drive that leaves
   the room:

   ```
   templates_user\
   fields_templates_user\
   dictionaries\
   logs\metadata\
   config\config.yaml
   ```

3. Put them in a **dated** folder, e.g.
   `\\backup-share\StatementStudio\2026-07-26\`. Dated copies are what let you go
   back to *before* a bad change, not just to the latest state.

Prefer a command you can paste into an Administrator Command Prompt (change the two
paths):

```
set "APP=D:\StatementStudio-offline"
set "DEST=\\backup-share\StatementStudio\%DATE:~-4%-%DATE:~3,2%-%DATE:~0,2%"
robocopy "%APP%\templates_user"        "%DEST%\templates_user"        /E
robocopy "%APP%\fields_templates_user" "%DEST%\fields_templates_user" /E
robocopy "%APP%\dictionaries"          "%DEST%\dictionaries"          /E
robocopy "%APP%\logs\metadata"         "%DEST%\logs\metadata"         /E
robocopy "%APP%\config"                "%DEST%\config" config.yaml
```

`robocopy` reports exit code 1 for "files were copied" — that is success, not an
error. Anything **8 or above** is a real failure; read the message.

Keep at least the last four weekly copies plus one from before each update.

> **Check it worked.** Open the newest dated folder and confirm `templates_user\`
> contains a `.yaml` for each template your team has made, and that
> `dictionaries\labels.yaml` is there. A backup nobody has ever opened is not a
> backup.

---

## Restoring

Restore onto a folder that has already been set up once (`RUN-ME.bat` has run and
the app starts), so the private R and packages are in place.

1. **Stop the app** — `Ctrl-C` in its window, or end the scheduled task.
2. Copy the folders back from the dated backup **over** the app folder, replacing
   what is there:
   `templates_user\`, `fields_templates_user\`, `dictionaries\`, `logs\metadata\`,
   and `config\config.yaml`.
3. **Start the app** (`RUN-ME.bat`).
4. Check: **Admin → Template management** lists your user templates, and
   **Admin → Templates → Label dictionary** shows your own wordings. Convert one
   statement you know reconciles and confirm it still does.

### Rebuilding a lost server from nothing
1. Copy a fresh `StatementStudio-offline` package to the new server and double-click
   `RUN-ME.bat`. Let it finish and start once — this installs the private R,
   packages and OCR, and **seeds** a default `config.yaml` and default dictionaries.
2. Stop it.
3. Restore the folders above, overwriting the seeded defaults.
4. Start it, re-open the firewall port and re-register the scheduled task —
   [running-and-keeping-it-up.md](running-and-keeping-it-up.md).

### Going back one dictionary save
If someone saved a bad wording an hour ago and you have no fresh backup, the app
keeps the previous contents of each dictionary beside it as
`dictionaries\labels.yaml.bak` / `dictionaries\lexicon.yaml.bak`. Stop the app, copy
the `.bak` over the `.yaml`, start it again. That is one step of history only.

---

## What backup does *not* cover
- **The statements themselves.** `uploads\` is deliberately excluded; the source
  files live wherever your team keeps case evidence, and that system is the record.
- **The Qlik feed.** It is regenerated from conversions; Qlik keeps its own loads.
- **The app itself.** It comes from the package on the internet PC —
  [first-time-setup.md](first-time-setup.md).

Related: [updating.md](updating.md) ·
[maintaining-the-engine.md](maintaining-the-engine.md)
