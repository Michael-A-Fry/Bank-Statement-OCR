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

## If you copy individual files by hand

Sometimes a change is three files and building a whole package feels like
overkill. It is still the safer route — but if you are dragging files out of the
**source folder** instead, know that the source folder is not the package, and
the difference is exactly the thing that protects your live state.

**The one that will bite you:**

> **Never copy `dictionaries\labels.yaml` or `dictionaries\lexicon.yaml` out of
> the source folder onto a running server.**

The source folder holds those two files under **those exact names** — they are the
shipped starting vocabularies. On the server the same two names are Admin-edited
live state: every wording and marker your team has taught the tool. The package
build is what renames them to `labels.example.yaml` / `lexicon.example.yaml` so an
update cannot touch them. A hand copy skips that step and silently replaces the
lot. Nothing errors. Statements that reconciled last week quietly stop
reconciling, and the cause is a week old by the time anybody notices. Recovery is
`dictionaries\labels.yaml.bak` if you catch it before the next Admin save, and
your backup after that.

**Also never carry across:**

| Do not copy | Why |
|---|---|
| `config\config.yaml` | Your settings — port, admin password, feed folder. The source folder does not contain one (deliberately), so the only way to move one is from another machine, and then you have moved that machine's admin password and feed path onto your server. |
| `templates_user\`, `fields_templates_user\`, `doc_templates_user\` | Every template your team built. The source folder has only a `README.md` in each, so copying the *folder* is harmless — but never delete-and-replace one. |
| `logs\`, `uploads\`, `feed\`, `requests\` | The audit trail, the kept statements, and what Qlik reads. The source folder may hold a developer's own `logs\` — it is not yours. |
| `R-runtime\`, `R-lib\`, `offline\.installed` | The private R that `RUN-ME.bat` installed **on that box**. Not in the source folder at all. Deleting `.installed` forces a full re-install on next start. |

**And one to copy deliberately, never in a sweep:**

`R\params.R` holds the engine's numeric thresholds and is the one code file a
maintainer is expected to edit. It lives in `R\`, so dragging the whole `R\`
folder across reverts every value you set. If a release does not change it, do not
copy it. If it does, open the two side by side and re-apply your values by hand —
[maintaining-the-engine.md](maintaining-the-engine.md) §2.

Take a backup before any hand copy ([backup-and-restore.md](backup-and-restore.md)).
It is five minutes, and it is the difference between "that was the wrong file" and
"we have lost a year of taught words".

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
