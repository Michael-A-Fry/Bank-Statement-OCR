# Updating a version — merging dev into prod

You have two folders: **dev** (the new version) and **prod** (the one people use).
This page moves dev into prod without destroying what prod has learned.

**The whole risk is one idea:** prod contains work that exists in no other folder —
every template your team built and every word Admin has taught it. Dev contains
placeholder copies of some of those files, **under the same names**. Copy the
wrong one and it is replaced silently. Nothing errors. The tool just quietly gets
worse.

---

## Never copy these from dev to prod

| Never copy | Why |
|---|---|
| **`dictionaries\labels.yaml`**<br>**`dictionaries\lexicon.yaml`** | **The dangerous pair.** Dev has these two files under these exact names — they are the shipped starting vocabularies. In prod the same names hold every wording and marker your team has taught it. Overwrite them and statements that reconciled last week stop reconciling, weeks before anyone connects the two. |
| `config\config.yaml` | Prod's settings: port, admin password, feed folder. Dev's copy (if it has one) is a developer's. |
| `templates\statements_user\`<br>`templates\fields_user\`<br>`templates\documents_user\` | Every template your team built in the app. Dev has only a `README.md` in each, so copying the *folder* is survivable — but never delete-and-replace one. |
| `logs\` | The audit trail. `logs\metadata\` is kept forever and cannot be recreated. |
| `uploads\` | Copies of real client statements. |
| `feed\` | What Qlik reads. |
| `requests\` | "None of these fits" raises from analysts. |
| `R-runtime\`, `R-lib\`, `offline\` | The private R installed **on that box**. Not in dev at all. |

## Copy deliberately, never in a sweep

| File | Rule |
|---|---|
| `R\params.R` | The engine's numeric thresholds — the one code file a maintainer edits. It sits inside `R\`, so **dragging the whole `R\` folder reverts every value you set.** If the release did not change it, do not copy it. If it did, open both side by side and re-apply your values by hand. |
| `templates\statements\` | Curated templates. Copying replaces same-named files. A template you **promoted** in prod but never put in dev survives (nothing shares its name) — but check first. |

## Everything else is safe to copy

`app.R`, `R\` (except `params.R`), `scripts\`, `www\`, `docs\`, `tests\`,
`samples\`, `templates\statements_seed\`, `config\config.example.yaml`,
`README.md`, `CHANGELOG.md`, `RUN-ME.bat`, and **`VERSION`**.

`VERSION` is not optional. It is stamped into every conversion, every run log and
the Qlik feed manifest. Without it every output answers "which build produced this
figure?" with `unknown`.

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

6. **Start prod** — `RUN-ME.bat`, or the scheduled task. No internet needed and
   nothing is reinstalled, so it is quick.

7. **Check it came back whole**, in this order — each one checks a different
   folder you were told not to overwrite:

   | Check | Proves |
   |---|---|
   | Convert a statement you know reconciles. It still reconciles. | The engine and `dictionaries\` |
   | Open that conversion's `.json` download. `build.engine_version` reads the **new** version. | `VERSION` travelled |
   | **Admin → Templates** lists your team's templates. | `templates\statements_user\` |
   | Upload a report you built a puller for. It is recognised, not asked for. | `templates\documents_user\` — **Admin does not list these**, so this is the only way to see them |
   | **Admin → Templates → Label dictionary** shows your own wordings. | `dictionaries\` again, directly |

   If any check fails, stop and restore from step 1's backup rather than
   converting anything real.

---

## If something is wrong afterwards

Going back is its own page: [rolling-back.md](rolling-back.md). Note that rolling
the folder back does **not** withdraw rows the new version already published to
`feed\` — that is a separate job, and it is the reason to decide quickly.

## The safer route, when you have it

Building a package on the internet PC (`make-bundle.bat`) and replacing the whole
prod folder does all of this for you, and cannot get the dangerous pair wrong: the
build renames `dictionaries\*.yaml` to `*.example.yaml`, so an update physically
cannot touch prod's taught words. Use it when you can —
[updating.md](updating.md). This page is for when you are merging two folders by
hand instead.
