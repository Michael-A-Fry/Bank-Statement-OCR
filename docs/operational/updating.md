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

Which version is on the server? `offline\manifest.txt` records the app version,
the exact R and every bundled package version.

## What survives, and what is replaced

The package deliberately does not carry your live state, so replacing the folder
leaves it in place.

| Survives (not in the package) | Replaced (in the package) |
|---|---|
| `config\config.yaml` | `R\` — **including `R\params.R`** |
| `dictionaries\labels.yaml`, `dictionaries\lexicon.yaml` | `app.R`, `ui_content.R`, `ui_labels.R`, `run.R` |
| `templates_user\`, `fields_templates_user\` | `templates\`, `templates_seed\`, `fields_templates\` |
| `logs\`, `feed\`, `uploads\` | `scripts\`, `tests\`, `samples\`, `docs\`, `README.md`, `RUN-ME.bat` |
| `R-runtime\`, `R-lib\` | `config\config.example.yaml`, `dictionaries\*.example.yaml` |

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
- **To roll back:** keep the previous `StatementStudio-offline` folder, restore it
  and run its `RUN-ME.bat`. Your `config`, `logs` and `feed` are unaffected.
- **Run the test suite after updating** —
  [maintaining-the-engine.md](maintaining-the-engine.md) §1.
