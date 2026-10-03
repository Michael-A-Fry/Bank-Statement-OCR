# Release 2.0.0: the hand-carry list

What the offline server needs changed to go from **1.23.1** to **2.0.0**:
every file to update, add or delete. Templates are gone in 2.0.0. Each statement
is now read from its content and proved by its own figures, and each bank's
layouts are learned on the server
([../../CHANGELOG.md](../../CHANGELOG.md) has the full entry).

The list comes from `git diff --name-status pre-auto-reading..HEAD`, plus the
documentation changed afterwards. To check it against the tree you are about to
carry, run this on the build PC:

```
git diff --name-status -M pre-auto-reading..HEAD -- . ":(exclude)tools" ":(exclude).claude"
```

Anything in that output that is not listed below changed after this page was
written. Carry it too, and tell whoever wrote this page.

---

## Read this first: 2.0.0 cannot simply be copied over 1.23.1

The usual update is to copy the new folder over the old one and choose
**Replace the files in the destination** ([updating.md](updating.md)). That
replaces files but **never deletes any**. That has always been safe because no
release had removed an engine file before. 2.0.0 removes eight.

The app loads every `R\*.R` file it finds, in alphabetical order. If the old
files stay, they load after the new ones and replace some of the new functions
with the 1.23.1 versions, without any error. `R\templates.R` would replace
`resolve_date_format`, `R\wizard_detect.R` would replace the date-format
tables, and `R\wizard_auto.R` would replace two of the role helpers the new
reader uses. The app would start, and some of its figures would come from old
code.

**So the deletions in section 2 are required.** The simplest safe way to
update `R\` is to replace the whole folder (section 3).

---

## 1. Before you touch anything

1. **Back up** as [backup-and-restore.md](backup-and-restore.md) says, and add
   `templates\statements_user\` to that backup. 2.0.0 no longer reads it, so
   this backup is the only copy of the templates your team built. You will want
   it if you ever roll back ([rolling-back.md](rolling-back.md)), and it is the
   record behind every 1.x conversion that used one of those templates.
2. **Keep the 1.23.1 bundle.** It is what a rollback needs.
3. **Stop the app.** Press `Ctrl-C`, or end the scheduled task.
4. **Copy `R\params.R` somewhere safe.** 2.0.0 does not change it, so the
   server's copy goes back unchanged (section 3).

## 2. Delete these from the server

| Delete | Why |
|---|---|
| `R\column_fit.R`, `R\column_profile.R`, `R\detect.R`, `R\draft.R`, `R\learned.R`, `R\templates.R`, `R\wizard_auto.R`, `R\wizard_detect.R` | Retired engine files. Left in place they are still loaded, and they replace new functions with old ones (see above). |
| `templates\statements\` (the whole folder: `anz_creditcard_csv.yaml`, `anz_everyday_csv.yaml`, `anz_everyday_pdf.yaml`, `anz_investmentfunds_pdf.yaml`, `asb_everyday_csv.yaml`, `asb_everyday_pdf.yaml`, `bnz_everyday_csv.yaml`, `excel_generic_xlsx.yaml`, `kiwibank_everyday_csv.yaml`, `tutorial_everyday_pdf.yaml`, `westpac_everyday_csv.yaml`, `westpac_everyday_pdf.yaml`, `xero_standard_csv.yaml`) | The shipped templates. They now live in `tests\testthat\fixtures\templates\` as test material only. |
| `templates\statements_seed\` (the whole folder) | Unfinished template drafts. Nothing reads them any more. |
| `templates\statements_user\` (the whole folder, **after** step 1 has backed it up) | Templates built in the app, `_learned_choices.json`, their `.yaml.bak` copies and `README.md`. 2.0.0 reads none of them. Keep the backup off the server. Do not leave the folder in place, because a folder nothing reads gets backed up and restored for years for no reason. |
| `tests\testthat\test-column-fit.R`, `test-column_profile.R`, `test-detect.R`, `test-draft.R`, `test-draft_excel.R`, `test-learned.R`, `test-templates.R`, `test-user_templates.R`, `test-wizard_auto.R`, `test-wizard_detect.R` | Tests of the retired files. Left in place, they fail the suite on the server. |

If the server still has `templates\fields\`, `templates\fields_user\`,
`templates\documents\` or `templates\documents_user\` from before 1.9.0, they
are not read either. Back them up with `statements_user\` and delete them.

## 3. Replace these whole folders

No server data is stored in these folders, so replacing them whole is the
safest way, and it carries every deletion above with it:

| Folder | How |
|---|---|
| `R\` | Delete the server's `R\` folder, copy in the new one, then put back the `R\params.R` you saved in step 1.4. |
| `tests\` | Delete it and copy in the new one. (5 test files are new, 51 are changed and 10 are deleted, and the 13 old templates are now fixtures under `tests\testthat\fixtures\templates\`.) |
| `docs\` | Delete it and copy in the new one. Pages were rewritten for 2.0.0. `docs\operational\adding-a-bank-template.md` is now about training a bank, and this page is new. |
| `www\` | Copy over it. `app.css` changed. |

If you would rather copy file by file, the `R\` changes are:

- **New:** `auto_read.R`, `auto_read_pdf.R`, `auto_read_prove.R`,
  `auto_read_tabular.R`, `bank_identity.R`, `fixes.R`, `layouts.R`,
  `tracking.R`.
- **Changed:** `analytics.R`, `audit.R`, `batch.R`, `batch_audit.R`,
  `config.R`, `convert.R`, `diagnose.R`, `feed.R`, `identify.R`, `jobs.R`,
  `normalise.R`, `ocr.R`, `ocr_preprocess.R`, `outputs.R`,
  `parse_pdf_table.R`, `read_input.R`, `read_pdf.R`, `split.R`, `util.R`.
- **Deleted:** the eight files in section 2.
- **Unchanged:** every other file, including `params.R`.

## 4. Replace these single files

| File | What changed |
|---|---|
| `VERSION` | `2.0.0`. It is stamped on every output, so it must travel. |
| `app.R` | The new screens: Convert by bank, Please check, and Admin's Banks and Automatic reading tabs. |
| `ui_labels.R`, `ui_content.R` | New wording. They must travel with `app.R`, because `app.R` uses labels that only the new copies have. |
| `run.R` | The command line prints the bank and the reading instead of the template. |
| `config\config.example.yaml` | The template settings are gone. `paths: layouts`, `paths: tracking` and `auto_reading: spot_check_rate` are new. |
| `templates\README.md` | Now describes the learned-layout store. |
| `README.md`, `CHANGELOG.md` | The 2.0.0 description. |

## 5. Add these two reference files to `dictionaries\`

| File | What it is |
|---|---|
| `dictionaries\nz_bank_branches.csv` | The Payments NZ bank branch register (bank code, branch, institution), used to tell which bank issued a statement from the account holder's own account number. |
| `dictionaries\nz_banks.yaml` | Each NZ bank's display name, legal names, websites, phone numbers, SWIFT code and brand words. |

These two files **are part of the product**, unlike `labels.yaml` and
`lexicon.yaml` beside them. They are safe to copy in and to overwrite at any
later update. Without them the tool cannot work out the bank. Every statement
then asks *Please choose the bank*, and a statement that names a different bank
from the one picked can no longer be caught.

**Do not copy `dictionaries\labels.yaml` or `dictionaries\lexicon.yaml`.**
These are the words your team has taught the tool. The rule in
[updating-a-version.md](updating-a-version.md) has not changed.

**The package route currently loses these two files.** `scripts\bundle-offline.R`
ships the dictionaries folder as `*.example.yaml` copies only. It renames
`nz_banks.yaml` to `nz_banks.example.yaml`, and it leaves
`nz_bank_branches.csv` out because the file is not YAML. Until that script is
fixed, copy both files by hand even when you use a package.

## 6. Edit `config\config.yaml` by hand

Do not replace the file. It holds this server's settings. Open it in Notepad,
delete the lines below that it has, and save. None of them does anything in
2.0.0, so leaving them in would only suggest a setting that no longer works.

```
app:
  user_templates_default: ...
paths:
  templates: ...
  user_templates: ...
  fields: ...
  user_fields: ...
  docs: ...
  user_docs: ...
  learned_choices: ...
feed:
  require_status_ok: ...
  min_trust: ...
  allowed_template_origins: ...
  template_allowlist: ...
```

The Qlik gate no longer has settings. A statement reaches the dashboards when
its own arithmetic proved it, when it matched a proven layout, or when a person
confirmed it on Please check ([connecting-qlik.md](connecting-qlik.md) §3).

You do not need to add anything. The new settings have working defaults:
learned layouts in `templates\layouts`, tracking in `logs\tracking`, and spot
checks off. An admin can set the spot-check rate from **Admin -> Automatic
reading**.

## 7. Folders the app creates, and that you must now back up

| Folder | Created when | Back it up? |
|---|---|---|
| `templates\layouts\` | the first statement is proven and learned | **Yes, from day one.** It is every layout the tool has learned on this server, and it exists nowhere else. |
| `templates\layouts\.pending\` | a person's unproven fix is held for an admin | Yes. It is inside `templates\layouts\`. |
| `logs\tracking\` | the first conversion | Yes. It holds the automatic-reading counts that Admin shows. |

[backup-and-restore.md](backup-and-restore.md) already lists both.

## 8. Not on the server, so not carried

`tools\` (the test-set generators and scorers, and the browser check),
`.claude\`, and anything under `samples\_private_staging\`.

## 9. Not ready yet: these block the release, and their owners have them

Check that each of these has been fixed in the tree you carry. Until it is,
the step that depends on it does not work.

| File | Problem | What it breaks |
|---|---|---|
| `scripts\health-check.R` | It still loads templates, so a healthy 2.0.0 server reports `FAIL Templates`. | The "is it fit to convert" check after the update. Until it is fixed, use the conversion check in section 10. |
| `scripts\audit-statement.R`, `scripts\bulk-audit.R` | They call the 1.x audit with templates and fail. | The command-line audits. Admin's *Check a pile of files at once* works. |
| `scripts\bundle-offline.R` | It ships `templates\` whole, so any `templates\layouts\` on the build PC would travel and overwrite the server's layouts of the same name. It also loses the two reference files (section 5). | The package route. |
| `scripts\run_app.R` | It still calls a template migration that now does nothing. | Nothing. It is untidy, not broken. |

When these scripts are fixed, they join this list under section 4.

## 10. After the copy: prove it

1. **Start the app** with `RUN-ME.bat` or the scheduled task.
2. **Convert the sample statement**
   `samples\raw\tutorial\sample_everyday_statement.pdf`. It must come back
   **Proven** with **12 transactions**. Leave the bank empty: it is a made-up
   bank, and the row asks you to choose one.
3. **Open its JSON download.** `build.engine_version` must read `2.0.0`. The
   build stamp also shows `layouts_state`, which is `empty` on a server that
   has learned nothing yet.
4. **Open Admin.** The tabs are **Banks**, **Automatic reading**, **Words** and
   **Health**. Banks is empty until a statement has been learned.
5. **Run the test suite**
   ([maintaining-the-engine.md](maintaining-the-engine.md) §1). It must end
   `failed: 0`, `errors: 0`, `skipped: 0`.
6. **Train each bank** before people rely on it: Admin -> Banks -> *Train a
   bank*, with every statement you have for that bank
   ([adding-a-bank-template.md](adding-a-bank-template.md)). A bank with no
   training still converts. Every statement that proves itself converts at
   once. Training is what lets the statements with no balance of their own
   convert without a person.
7. **Turn on spot checks** if the unit wants them from day one: Admin ->
   Automatic reading -> *Spot-check rate*. They are off by default.

If step 2 or step 3 fails, stop and roll back ([rolling-back.md](rolling-back.md))
before anything real is converted.
