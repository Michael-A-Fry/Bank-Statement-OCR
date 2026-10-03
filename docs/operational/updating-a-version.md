# Updating a version — merging dev into prod by hand

You have two folders: **dev** (the new version) and **prod** (the one people use).
This page moves dev into prod without destroying what prod has learned.

**The whole risk is one idea:** prod contains work that exists in no other folder —
every bank layout the tool has learned there and every word Admin has taught it. Dev contains
placeholder copies of some of those files, **under the same names**. Copy the
wrong one and it is replaced silently. Nothing errors. The tool just quietly gets
worse.

(The package route — `make-bundle.bat` on the internet PC, replace the whole
folder — is the one to use when you have it: [updating.md](updating.md).)

**Going from 1.23.1 to 2.0.0?** That update also *deletes* files, which neither
route does by itself. Follow
[release-2.0.0-hand-carry.md](release-2.0.0-hand-carry.md) instead of this page's
steps 4 and 5.

---

## Never copy these from dev to prod

| Never copy | Why |
|---|---|
| **`dictionaries\labels.yaml`**<br>**`dictionaries\lexicon.yaml`** | **The dangerous pair.** Dev has these two files under these exact names — they are the shipped starting vocabularies. In prod the same names hold every wording and marker your team has taught it. Overwrite them and statements that reconciled last week stop reconciling, weeks before anyone connects the two. |
| `config\config.yaml` | Prod's settings: port, admin password, feed folder. Dev's copy (if it has one) is a developer's. |
| `templates\layouts\` | Every bank layout the tool has learned in prod, and the fixes waiting for an admin (`.pending\`). A dev folder may have its own `templates\layouts\` from testing, **with the same file names** (`anz\anz_1@v1.yaml`): copy it and prod's learning is overwritten by a developer's. |
| **any `.yaml.bak` or `.yaml.*.part`** in `dictionaries\` | Prod's undo. Every save writes the previous contents beside the file first, so a `.bak` is one step of history. A `.part` is a save that died half-way; leave it for whoever is diagnosing that. |
| `logs\` | The audit trail. `logs\metadata\` is kept forever and cannot be recreated, and `logs\tracking\` holds the automatic-reading counts. |
| `uploads\` | Copies of real client statements. |
| `feed\` | What Qlik reads. |
| `requests\` | "None of these fits" raises from analysts. |
| `R-runtime\`, `R-lib\`, `offline\` | The private R installed **on that box**. Not in dev at all. |

## Copy deliberately, never in a sweep

| File | Rule |
|---|---|
| `R\params.R` | The engine's numeric thresholds — the one code file a maintainer edits. It sits inside `R\`, so **dragging the whole `R\` folder reverts every value you set.** If the release did not change it, do not copy it. If it did, open both side by side and re-apply your values by hand. |
| `dictionaries\nz_banks.yaml`<br>`dictionaries\nz_bank_branches.csv` | The bank reference data. These two **are** product (unlike `labels.yaml` and `lexicon.yaml` beside them): copy them when the release changed them. |

## Everything else is the product — copy all of it

```
app.R   ui_content.R   ui_labels.R   run.R   RUN-ME.bat   VERSION
R\ (except params.R)   scripts\   www\   templates\README.md
config\config.example.yaml   docs\   tests\   samples\
README.md   CHANGELOG.md
```

That is the same list `scripts\bundle-offline.R` ships (`app_items` near the top
of it), so it cannot quietly drift from what a real package contains. Two on it
are easy to leave behind and both are silent when you do:

- **`ui_content.R` and `ui_labels.R`** hold every word on every screen. `app.R`
  sources them. A new `app.R` against last month's copies gives you the old
  wording — or a blank panel where a label it now expects is missing.
- **`VERSION`** is stamped into every conversion, every run log and the Qlik feed
  manifest. Without it every output answers "which build produced this figure?"
  with `unknown`.

---

## The steps

1. **Back prod up.** Five minutes, and it is the only thing that makes a mistake
   here recoverable — [backup-and-restore.md](backup-and-restore.md).

2. **Stop prod.** `Ctrl-C`, or end the scheduled task. Copying over a running app
   is how you get a half-old, half-new folder.

3. **Note anything you changed in prod by hand.** In practice that is
   `R\params.R` and nothing else. Copy it somewhere safe now.

4. **Copy the changed files from dev to prod.** Choose **Replace the files in the
   destination** when Windows asks. Do not copy anything in the *Never copy*
   table above.

   The safest version of this step is to copy only the files the release actually
   changed — `CHANGELOG.md` says which. Copying `R\` wholesale is fine **as long
   as** you then redo step 5.

5. **Re-apply your `R\params.R` values by hand.** Open the new file and your saved
   copy side by side and set your values again. Never paste the old file over the
   new one — a new version may have added parameters the engine now expects.

6. **Ask the box whether it is fit to convert.** One command, before you start it:

   ```
   cd /d D:\StatementStudio-offline
   set "R_LIBS_USER=%CD%\R-lib"
   set "R_LIBS_SITE="
   "R-runtime\bin\x64\Rscript.exe" scripts\health-check.R
   ```

   It prints the version it is about to run and one line per check — the settings
   file parsed, an admin password is set, every learned layout can be read, every
   folder it must write to is writable, and the scan-reading software is still
   installed. `PASS` on the last line means all of it passed. It reads and changes
   nothing, so it is safe to run at any time
   ([maintaining-the-engine.md](maintaining-the-engine.md) §1 lists every check).

7. **Start prod** — `RUN-ME.bat`, or the scheduled task. No internet needed and
   nothing is reinstalled, so it is quick.

8. **Convert a statement you know is proven.** It is still proven, and its
   `.json` download reads the **new** `build.engine_version`. That is the one
   check health-check cannot make for you: it proves the engine and
   `dictionaries\` came through together, and that `VERSION` travelled.

   If either step 6 or step 8 fails, stop and restore from step 1's backup rather
   than converting anything real.

---

## If something is wrong afterwards

Going back is its own page: [rolling-back.md](rolling-back.md). Note that rolling
the folder back does **not** withdraw rows the new version already published to
`feed\` — that is a separate job, and it is the reason to decide quickly.
