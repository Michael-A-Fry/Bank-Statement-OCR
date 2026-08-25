# Updating to a new version

Same as building it the first time: build a fresh package on the internet PC,
drop it on the server. Settings, templates, taught words, logs and the installed
R are all kept.

## Steps

0. **Take a backup off the box first.** Five minutes, and it is the only thing
   that protects your templates, taught words and metadata if the update or the
   server goes wrong — [backup-and-restore.md](backup-and-restore.md).
1. **On the internet PC:** get the new app folder, double-click
   `make-bundle.bat`. Check its last lines for `MISSING` before you carry it
   across.
2. **Copy it to the server, over the existing folder.** When Windows asks, choose
   **Replace the files in the destination**.
3. **Double-click `RUN-ME.bat`.** It starts on the new version. It does not
   re-install R or the packages, so it is quick.
4. **Re-apply any `R\params.R` change you had made** (see below), then convert
   one statement you know reconciles and confirm it still does.
5. **Check the new version is actually stamped on the output.** Open that
   conversion's `.json` download and look at `build.engine_version` near the top.
   It should read the version you just shipped. It is the same value written into
   every `logs\runs\<run_id>.json` and into the Qlik feed manifest, and it is what
   makes "which build produced this figure?" answerable afterwards — so if it says
   `unknown`, the `VERSION` file did not reach the server, and that is worth
   fixing before anyone converts anything real.

Which version is on the server? `offline\manifest.txt` records the app version,
the exact R and every bundled package version.

## What survives, and what is replaced

**"Replace the files in the destination" replaces exactly the files the package
carries, and leaves everything else alone.** So what protects your live state is
that the package does not carry it — not anything clever in the copy.

| Survives | Replaced (in the package) |
|---|---|
| `config\config.yaml` | `R\` — **including `R\params.R`** |
| `dictionaries\labels.yaml`, `dictionaries\lexicon.yaml` | `app.R`, `ui_content.R`, `ui_labels.R`, `run.R` |
| your `templates_user\<id>.yaml` files, and all of `fields_templates_user\` and `doc_templates_user\` | `templates\`, `templates_seed\`, `fields_templates\`, `doc_templates\` |
| `logs\`, `feed\`, `uploads\` | `scripts\`, `tests\`, `samples\`, `docs\`, `README.md`, `RUN-ME.bat` |
| `R-runtime\`, `R-lib\` | `config\config.example.yaml`, `dictionaries\*.example.yaml` |

One detail worth knowing, because it is the one that could bite: the package
*does* contain `templates_user\`, `fields_templates_user\` and
`doc_templates_user\` folders, but the only file in each is that folder's own
`README.md`. Your saved templates survive because nothing in the package shares
their filenames — so if a `.yaml` is ever added to one of them in the source, a
server template of the same name would be replaced by it. Your backup is what
makes that recoverable ([backup-and-restore.md](backup-and-restore.md)).

The package ships only `labels.example.yaml` / `lexicon.example.yaml`, which seed
a brand-new install and are ignored once your own files exist. `RUN-ME.bat` also
keeps a copy of `config.yaml` and both dictionaries under
`%LOCALAPPDATA%\StatementStudio` and restores them if the folder's copies are
missing — so even deleting the whole folder and pasting a fresh one keeps your
settings and taught words.

That copy is **on the same machine** and does not survive losing the server. For
that, and for your templates and metadata, use
[backup-and-restore.md](backup-and-restore.md).

### If a dictionary does get clobbered

Every Admin save first writes the previous contents beside the file:
`dictionaries\labels.yaml.bak`, `dictionaries\lexicon.yaml.bak`. Close the app,
copy the `.bak` over the `.yaml` (dropping the `.bak`), start it again. One level
of history, not many.

### Re-applying an `R\params.R` change

`R\params.R` holds the engine's numeric thresholds and is the one code file a
maintainer is expected to edit. It lives in `R\`, so **an update overwrites it**.
Keep a copy of your edited version beside your backups, then after the update
open the new and old files side by side and re-apply your values **by hand** —
never copy the old file over the new one, because a new version may have added
parameters the rest of the engine now expects. Full procedure:
[maintaining-the-engine.md](maintaining-the-engine.md).

## Other notes

- **To also refresh the R packages** (only if a new version changes them): delete
  `offline\.installed` before running `RUN-ME.bat`. You do **not** need this after
  a failed setup — the marker is only written when the install succeeded, so
  setup retries on its own.
- **Keep the previous bundle** — the `StatementStudio-offline` folder as it came
  off the internet PC, not a copy of the live server folder. It is what a rollback
  needs, and the two are not interchangeable. Keep the last two.
- **Run the test suite after updating** —
  [maintaining-the-engine.md](maintaining-the-engine.md) §1.

## If the new version is wrong

Going back is its own page, because the app folder is the easy half:
[rolling-back.md](rolling-back.md). Copying the old bundle over the folder takes
ten minutes and leaves your settings, dictionaries, templates and logs alone.

What does **not** come back with it is the Qlik feed. Every conversion the bad
version published is already in `feed\transactions\`, and if Qlik has reloaded
since, it is already on the dashboards. Rolling back does not touch those rows and
nothing withdraws them on your behalf — somebody has to re-convert the statements
or withdraw the rows by hand, and then force a Qlik reload. That is
[rolling-back.md](rolling-back.md) §4, and it is the reason to decide quickly.
