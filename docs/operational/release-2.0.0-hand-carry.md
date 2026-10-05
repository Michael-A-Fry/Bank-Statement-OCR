# Release 2.0.0: the hand-carry list (1.x to 2.0.0)

What the offline server needs changed to go from **1.x** (written against
1.23.1; the same steps work from any earlier 1.x, because sections 3 and 4
replace everything the app runs) to **2.0.0** (commit `10665d8`): every file to
update, add or delete. Templates are gone in 2.0.0. Each statement
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

## The short version (about 20 minutes)

Each step is explained in the section named after it.

1. **Back up** the server, including `templates\statements_user\`; keep the
   1.x bundle; **stop the app** (section 1).
2. Copy `R\params.R` aside (section 1).
3. **Delete** the server's `R\`, `tests\` and `docs\` folders and the old
   `templates\statements\`, `templates\statements_seed\` and
   `templates\statements_user\` folders (sections 2, 3).
4. **Copy in** the new `R\`, `tests\`, `docs\` and `www\` folders, then put
   `R\params.R` back (section 3).
5. **Copy over** the single files: `VERSION`, `app.R`, `ui_labels.R`,
   `ui_content.R`, `run.R`, `RUN-ME.bat`, `README.md`, `CHANGELOG.md`,
   `config\config.example.yaml`, `templates\README.md` and everything in
   `scripts\` (section 4).
6. **Add** `dictionaries\nz_bank_branches.csv` and `dictionaries\nz_banks.yaml`.
   Do **not** copy `labels.yaml` or `lexicon.yaml` (section 5).
7. In `config\config.yaml`, **delete** the old template and feed lines listed in
   section 6. Add nothing.
8. Add `templates\layouts\` and `logs\tracking\` to the backup (section 7).
9. **Prove it** (section 9): `scripts\health-check.R` all PASS; the sample
   statement Proven with 12 transactions; the JSON says `2.0.0`; the test suite
   ends `failed: 0`, `skipped: 0`.
10. **Train each bank** from Admin -> Banks (section 9, step 7).

If anything in step 9 fails, stop and roll back ([rolling-back.md](rolling-back.md)).

## What people will notice

- Convert asks for a **bank**, not a template, and fills it in from the
  statement. Most statements convert with no clicks.
- A statement the tool cannot prove goes to **Please check**, with the reason in
  a sentence and a dropdown per column of figures.
- A file holding several statements is split and each one proven; when they join
  up (each opens on the previous closing) it needs no clicks.
- Another account's table printed in the same file (a linked savings account, a
  loan, a term deposit) is kept apart: it goes to the workbook's *Other accounts*
  sheet and the JSON, never into the statement's rows, CSV or the Qlik feed.
- Admin has **Banks** (learned layouts, training, held fixes) and **Automatic
  reading** (the automatic rate and what failed).

---

## Read this first: 2.0.0 cannot simply be copied over 1.x

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
| `R\column_fit.R`, `R\column_profile.R`, `R\detect.R`, `R\draft.R`, `R\inspect.R`, `R\learned.R`, `R\row_coverage.R`, `R\templates.R`, `R\wizard_auto.R`, `R\wizard_detect.R` | Retired engine files. Left in place they are still loaded, and they replace new functions with old ones (see above). |
| `templates\statements\` (the whole folder: `anz_creditcard_csv.yaml`, `anz_everyday_csv.yaml`, `anz_everyday_pdf.yaml`, `anz_investmentfunds_pdf.yaml`, `asb_everyday_csv.yaml`, `asb_everyday_pdf.yaml`, `bnz_everyday_csv.yaml`, `excel_generic_xlsx.yaml`, `kiwibank_everyday_csv.yaml`, `tutorial_everyday_pdf.yaml`, `westpac_everyday_csv.yaml`, `westpac_everyday_pdf.yaml`, `xero_standard_csv.yaml`) | The shipped templates. They now live in `tests\testthat\fixtures\templates\` as test material only. |
| `templates\statements_seed\` (the whole folder) | Unfinished template drafts. Nothing reads them any more. |
| `templates\statements_user\` (the whole folder, **after** step 1 has backed it up) | Templates built in the app, `_learned_choices.json`, their `.yaml.bak` copies and `README.md`. 2.0.0 reads none of them. Keep the backup off the server. Do not leave the folder in place, because a folder nothing reads gets backed up and restored for years for no reason. |
| `tests\testthat\test-column-fit.R`, `test-column_profile.R`, `test-detect.R`, `test-draft.R`, `test-draft_excel.R`, `test-inspect.R`, `test-learned.R`, `test-row_coverage.R`, `test-templates.R`, `test-user_templates.R`, `test-wizard_auto.R`, `test-wizard_detect.R` | Tests of the retired files. Left in place, they fail the suite on the server. |

If the server still has `templates\fields\`, `templates\fields_user\`,
`templates\documents\` or `templates\documents_user\` from before 1.9.0, they
are not read either. Back them up with `statements_user\` and delete them.

## 3. Replace these whole folders

No server data is stored in these folders, so replacing them whole is the
safest way, and it carries every deletion above with it:

| Folder | How |
|---|---|
| `R\` | Delete the server's `R\` folder, copy in the new one, then put back the `R\params.R` you saved in step 1.4. |
| `tests\` | Delete it and copy in the new one. (21 test files are new, 54 are changed and 12 are deleted, and the 13 old templates are now fixtures under `tests\testthat\fixtures\templates\`.) |
| `docs\` | Delete it and copy in the new one. Pages were rewritten for 2.0.0. `docs\operational\adding-a-bank-template.md` is now about training a bank, and this page is new. |
| `www\` | Copy over it. `app.css` changed. |

If you would rather copy file by file, the `R\` changes are:

- **New:** `auto_read.R`, `auto_read_blocks.R`, `auto_read_pdf.R`,
  `auto_read_prove.R`, `auto_read_summ.R`, `auto_read_tabular.R`,
  `bank_identity.R`, `fixes.R`, `layouts.R`, `tracking.R`, `words.R`.
- **Changed:** `analytics.R`, `audit.R`, `batch.R`, `batch_audit.R`,
  `config.R`, `convert.R`, `coverage.R`, `diagnose.R`, `extract_metadata.R`,
  `feed.R`, `identify.R`, `jobs.R`, `labels.R`, `layout.R`, `lexicon.R`,
  `metadata_capture.R`, `normalise.R`, `ocr.R`, `ocr_preprocess.R`,
  `outputs.R`, `parse_pdf_table.R`, `read_delimited.R`, `read_input.R`,
  `read_pdf.R`, `reconcile.R`, `requests.R`, `split.R`, `suggestions.R`,
  `util.R`.
- **Deleted:** the ten files in section 2.
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
| `scripts\health-check.R` | Its **Templates** line is now **Layouts**: how many learned layouts each bank has, proven or provisional, and any layout file it could not read. It also checks that `templates\layouts\` and `logs\tracking\` can be written to. The 1.23.1 copy reports `FAIL Templates` on a healthy 2.0.0 server. |
| `scripts\audit-statement.R`, `scripts\bulk-audit.R` | They audit with the automatic reader and this server's learned layouts. The 1.23.1 copies stop with an error. `bulk-audit.R` no longer writes `audit-drafts\`. |
| `scripts\run_app.R` | The call to the retired template migration is gone. |
| `scripts\bundle-offline.R` | Used on the build PC, not the server, but carried so the next package is built right: it ships the bank reference files (section 5) and never ships `templates\layouts\`. |
| `RUN-ME.bat` | A comment only. Nothing it does has changed. |

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

2.0.0's `labels.yaml` has one new entry, `statement_period` (the words a
statement prints in front of its own period, such as "Statement period"). The
reader has the same wordings built in, so a server keeping its own
`labels.yaml` needs nothing. Only if your team wants to teach new period
wordings from Admin -> Words, copy that one block (it is commented in the new
`labels.yaml`) into the server's file.

The new `labels.yaml` and `lexicon.yaml` also open with a plain explanation and
examples, and `labels.yaml` no longer lists `total_credits`, `total_debits` or
`account_name` (nothing read them). A server's own copies keep their old
headings and those three entries. That is harmless: Admin -> Words no longer
offers them, and the tool never read them. To have the new explanation on the
server, copy the comment lines at the top of each new file over the comment
lines at the top of the server's file. Leave every line that does not start
with `#` as it is.

A package built with `make-bundle.bat` carries both files under these names,
so on the package route they arrive with everything else. Only `labels.yaml`
and `lexicon.yaml` travel as `*.example.yaml` seeds. The package's
`offline\manifest.txt` has a `bank_list:` line that says whether both
travelled.

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

## 9. After the copy: prove it

1. **Ask the box whether it is fit to convert:** `scripts\health-check.R`
   ([maintaining-the-engine.md](maintaining-the-engine.md) §1 has the command
   line). Every line must say `PASS`. On a server that has learned nothing
   yet, **Layouts** says so, and that is a pass. This needs the 2.0.0 copy of
   the script (section 4).
2. **Start the app** with `RUN-ME.bat` or the scheduled task.
3. **Convert the sample statement**
   `samples\raw\tutorial\sample_everyday_statement.pdf`. It must come back
   **Proven** with **12 transactions**. Leave the bank empty: it is a made-up
   bank, and the row asks you to choose one.
4. **Open its JSON download.** `build.engine_version` must read `2.0.0`. The
   build stamp also shows `layouts_state`, which is `empty` on a server that
   has learned nothing yet.
5. **Open Admin.** The tabs are **Banks**, **Automatic reading**, **Words** and
   **Health**. Banks is empty until a statement has been learned.
6. **Run the test suite**
   ([maintaining-the-engine.md](maintaining-the-engine.md) §1). It must end
   `failed: 0`, `errors: 0`, `skipped: 0`.
7. **Train each bank** before people rely on it: Admin -> Banks -> *Train a
   bank*, with every statement you have for that bank
   ([adding-a-bank-template.md](adding-a-bank-template.md)). A bank with no
   training still converts. Every statement that proves itself converts at
   once. Training is what lets the statements with no balance of their own
   convert without a person.
8. **Turn on spot checks** if the unit wants them from day one: Admin ->
   Automatic reading -> *Spot-check rate*. They are off by default.

A `FAIL` in step 1 names what to fix; fix it before you go on. If step 3 or
step 4 fails, stop and roll back ([rolling-back.md](rolling-back.md)) before
anything real is converted.
