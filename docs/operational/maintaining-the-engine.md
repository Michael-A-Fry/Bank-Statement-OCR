# Maintaining the engine (the maintainer's runbook)

For the **one analyst who owns this tool** — the person who inherits it, not a
software engineer. Everything on this page is a copy-paste command or a file copy.

Day-to-day upkeep (Insights, review queue, tidying logs) is
[admin-and-maintenance.md](admin-and-maintenance.md). This page is the three things
that page doesn't cover:

1. [Running the test suite on the server](#1-run-the-test-suite-on-the-server)
2. [What an update keeps, what it overwrites, and how to re-apply an engine change](#2-what-an-update-keeps-and-what-it-overwrites)
3. [Promoting a user template to proven — including its golden test](#3-promote-a-user-template-to-proven)

The rules everything here serves are in
[../context/charter.md](../context/charter.md). Read it once: it is one page, and
it is what tells you whether a change is allowed.

---

## 1. Run the test suite on the server

The test suite is the guarantee. If it is green, the engine still honours every
promise (verbatim descriptions, honoured redactions, no silent drops, every shipped
template still parses its golden file). Run it after **any** change to `R\`,
`templates\` or the dictionaries, and after every update.

### The catch: `Rscript` alone will not work

The app installs and uses its **own private R inside the app folder**, deliberately
unregistered (`RUN-ME.bat` passes `!recordversion`), so it never becomes the
machine's R and never disturbs an existing R/RStudio install. Consequences:

- typing `Rscript` gets you *some other R* (or nothing) — not the one with the app's
  packages;
- the app's packages live in `R-lib\` inside the app folder, not in the usual place.

So you must name both. From an ordinary Command Prompt on the server (change the
first path to your app folder):

```
cd /d D:\StatementStudio-offline
set "R_LIBS_USER=%CD%\R-lib"
set "R_LIBS_SITE="
"R-runtime\bin\x64\Rscript.exe" tests\run_tests.R
```

> On a 32-bit-only box the executable is `R-runtime\bin\Rscript.exe` (no `x64\`).
> `RUN-ME.bat` falls back to it the same way.

### Reading the result

The last block is the whole answer (a real run, 2026-07-26 — your totals will be
higher as tests are added):

```
==== Test summary ====
files:   59
tests:   378
passed:  1585
failed:  0
errors:  0
warnings:0
skipped: 0
```

- **`failed` / `errors` above 0** — stop. Something is genuinely broken; do not
  promote a template or ship the change. The failing test names the file and the
  expectation.
- **`skipped` above 0** — the board is **not** a clean bill of health. A skip means
  a test never ran, almost always because a tool is missing (Tesseract/Poppler for
  the OCR tests, or an R package). Install what's missing and run again. The runner
  deliberately exits non-zero on skips; only if you *know* the box has no OCR by
  design, accept the gap knowingly with `set BSO_ALLOW_SKIPS=1` before the command —
  and write down why.
- **The pass condition is `failed: 0`, `errors: 0`, `skipped: 0`** — not a
  particular total. The `files` / `tests` / `passed` numbers grow every time a test
  is added, so a higher total than the block above is normal and healthy; a
  noticeably *lower* one means something didn't run, and is worth chasing.

The runner's exit code is `0` only when everything passed and nothing was skipped.

---

## 2. What an update keeps, and what it overwrites

An update is "replace the app folder with a fresh package"
([updating.md](updating.md)). The package deliberately does **not** carry your live
state, so those files survive; everything the package *does* carry is replaced.

| Survives an update (not in the package) | Replaced by the update (in the package) |
|---|---|
| `config\config.yaml` | `R\` — **including `R\params.R`** |
| `dictionaries\labels.yaml`, `dictionaries\lexicon.yaml` | `app.R`, `ui_content.R`, `ui_labels.R`, `run.R` |
| `templates_user\`, `fields_templates_user\` | `templates\` (the proven set), `templates_seed\`, `fields_templates\` |
| `logs\`, `feed\`, `uploads\` | `scripts\`, `tests\`, `samples\`, `docs\`, `README.md`, `RUN-ME.bat` |
| `R-runtime\`, `R-lib\` | `config\config.example.yaml`, `dictionaries\*.example.yaml` |

### Re-applying an `R\params.R` change

`R\params.R` is the one code file a maintainer is expected to edit — the engine's
numeric thresholds (year window, money tolerance, OCR DPI and confidence floors, PDF
row tolerance, redaction darkness), catalogued in
[../context/engine-parameters.md](../context/engine-parameters.md). It lives in `R\`,
so **an update overwrites it**.

The habit that prevents silently losing a tuning decision:

1. **Before** the update, note your changes. The reliable way is to keep a copy of
   your edited `R\params.R` beside your backups (see
   [backup-and-restore.md](backup-and-restore.md)) — one file, easy to read.
2. Do the update.
3. Open the **new** `R\params.R` and the **old** one side by side and re-apply your
   values by hand. Do not copy the old file over the new one: a new version may have
   added parameters that the rest of the engine now expects, and an old file would
   remove them.
4. Run the test suite (section 1). A parameter change with a failing test is the
   parameter's blast radius made visible — decide whether the new behaviour is
   intended before you accept it.
5. Convert one statement you know reconciles, and confirm it still does.

If you find yourself re-applying the same edit at every update, that value belongs in
the shipped defaults — send it back to whoever builds the package so the next version
carries it.

---

## 3. Promote a user template to proven

**Proven** is not a label, it is a gate: only templates in `templates\` feed the Qlik
dashboards ([connecting-qlik.md](connecting-qlik.md)), and only a template with a
golden test can prove it still parses what it claims to parse. A template in
`templates\` **without** a golden test fails the suite's guard test on purpose — that
guard is what stops an untested layout reaching the dashboards.

Promotion is therefore two moves, never one:

1. **Move the YAML** from `templates_user\<id>.yaml` into `templates\<id>.yaml`.
2. **Give it a fixture and a golden snapshot**, and run the suite.

The full step-by-step (fixture, generating and eyeballing the snapshot, writing the
test, for both the delimited and PDF paths) is
[`tests/HOWTO-add-template-test.md`](../../tests/HOWTO-add-template-test.md).

The short version:

| Step | Delimited (CSV/TSV) or Excel | PDF |
|---|---|---|
| Fixture | A small **synthetic** export under `samples\raw\<bank>\` | A synthetic one-page PDF, generated by `tests\testthat\fixtures\make_pdf_fixtures.R` |
| Golden | `tests\testthat\expected\<id>.csv` from the engine's own parse — **read it by eye first** | same |
| Test | `tests\testthat\test-<id>.R` using `expect_statement_ok(...)` | add an entry to `PDF_GOLDENS` in `tests\testthat\test-pdf_template_goldens.R` |
| Prove | Run the suite (section 1): the new template passes **and no other bank breaks** | same |

Never use a real customer statement as a fixture — fixtures are committed and travel
with the package. Invent the people, the accounts and the figures, and make the
invented statement reconcile (opening + transactions = printed closing) so the test
exercises the checks too.

After promotion, tell the team: a promoted template starts feeding Qlik on the next
conversion.

---

## Where else to look

| Question | Page |
|---|---|
| What is this tool allowed to do / never do? | [../context/charter.md](../context/charter.md) |
| What does the data contract guarantee? | [../context/architecture/build-contract.md](../context/architecture/build-contract.md) |
| What does each numeric threshold do? | [../context/engine-parameters.md](../context/engine-parameters.md) |
| What does the engine *not* handle yet? | [../context/edge-cases.md](../context/edge-cases.md) |
| What's planned next? | [../context/roadmap.md](../context/roadmap.md) |
| Getting the irreplaceable folders off the box | [backup-and-restore.md](backup-and-restore.md) |
