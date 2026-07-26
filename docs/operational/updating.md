# Updating to a new version

You get a new version the same way you first built it — build a fresh package on
the internet PC, then drop it on the server. Your settings, templates, logs and the
installed R are all kept automatically.

---

## Steps
0. **Take a backup off the box first** — five minutes, and it is the only thing that
   protects your templates, taught words and metadata if the update or the server
   goes wrong: [backup-and-restore.md](backup-and-restore.md).
1. **On the internet PC:** get the new app folder, then **double-click
   `make-bundle.bat`** to build a fresh `StatementStudio-offline` folder (same as
   first-time setup, Step 1).
2. **Copy it to the server, over the existing folder**, and when Windows asks,
   choose **"Replace the files in the destination"**.
3. **Double-click `RUN-ME.bat`.** It starts on the new version.

> **Which version am I on?** `offline\manifest.txt` in the package records the app
> version, the exact R it ships, and every bundled package version. Check it on the
> server when the two machines behave differently.

That's it. The update refreshes the app; it does **not** re-install R or the
packages (those are already there), so it's quick.

---

## What is kept automatically
Because the fresh package doesn't carry these, replacing the folder leaves them in
place:

- **`config\config.yaml`** — your settings (admin password, Qlik URL, feed folder).
- **`dictionaries\labels.yaml` and `dictionaries\lexicon.yaml`** — every wording and
  recognition marker the team has taught the tool in Admin. The package ships only
  `labels.example.yaml` / `lexicon.example.yaml`, which are used to *seed* a brand-new
  install and are ignored once your own files exist.
- **`R-runtime\` and `R-lib\`** — the installed private R and its packages.
- **`logs\`, `feed\`, `uploads\`** — run history, the Qlik feed, uploads.
- Any **templates you created** in the app (`templates_user\`, `fields_templates_user\`).

Your settings and taught words are safe even if you delete the whole folder and paste
a fresh one: `RUN-ME.bat` also keeps a backup of `config\config.yaml` and of both
dictionaries outside the folder (under `%LOCALAPPDATA%\StatementStudio`) and restores
them automatically if the folder's copies are missing.

> That backup is **on the same machine**. It does not survive losing the server —
> for that, and for your templates and metadata, follow
> [backup-and-restore.md](backup-and-restore.md) *before* you update.

### If a dictionary does get clobbered
Every time someone saves a dictionary in Admin, the app first writes the previous
contents beside it:

- `dictionaries\labels.yaml.bak`
- `dictionaries\lexicon.yaml.bak`

To go back one save, close the app, copy the `.bak` over the `.yaml` (drop the
`.bak`), and start it again. There is one level of history, not many — the real
safety net is the off-box copy in
[backup-and-restore.md](backup-and-restore.md).

---

## Notes
- **To also refresh the R packages** (only needed if a new version changes them):
  delete the file `offline\.installed` before running `RUN-ME.bat`, and it will
  reinstall packages on the next start. (You don't need this after a *failed* setup:
  the marker is only written when the install succeeds, so `RUN-ME.bat` retries on
  its own.)
- **To roll back:** keep the previous `StatementStudio-offline` folder; to go back,
  restore it and run its `RUN-ME.bat`. Your `config`, `logs` and `feed` are
  unaffected.
- **If you changed a setting in `R\params.R`** (an engine threshold — rare), the
  folder replace overwrites it. Re-apply it after the update:
  [maintaining-the-engine.md](maintaining-the-engine.md).
