# Maintaining the engine (the maintainer's runbook)

For the **one analyst who owns this tool** — the person who inherits it, not a
software engineer. Everything here is a copy-paste command or a file copy.

Day-to-day upkeep (Insights, requests, dictionaries, tidying logs) is
[admin-and-maintenance.md](admin-and-maintenance.md). This page is the three
things that page does not cover:

1. [Run the test suite on the server](#1-run-the-test-suite-on-the-server)
2. [Re-apply an engine parameter after an update](#2-re-apply-an-engine-parameter-after-an-update)
3. [Promote a template to proven](#3-promote-a-template-to-proven)

The rules everything here serves are in
[../context/charter.md](../context/charter.md). Read it once — it is one page,
and it is what tells you whether a change is allowed.

---

## 1. Run the test suite on the server

The suite is the guarantee. Green means the engine still honours every promise:
verbatim descriptions, honoured redactions, no silent drops, and every shipped
template still parsing its golden file. Run it after **any** change to `R\`,
`templates\` or the dictionaries, and after every update.

### `Rscript` on its own will not work

The app installs and uses its **own private R inside the app folder**,
deliberately unregistered (`RUN-ME.bat` passes `!recordversion`) so it never
becomes the machine's R and never disturbs an existing R/RStudio install. So
typing `Rscript` gets you some *other* R, or none — and the app's packages live
in `R-lib\` inside the app folder, not in the usual place. Name both:

```
cd /d D:\StatementStudio-offline
set "R_LIBS_USER=%CD%\R-lib"
set "R_LIBS_SITE="
"R-runtime\bin\x64\Rscript.exe" tests\run_tests.R
```

On a 32-bit-only box the executable is `R-runtime\bin\Rscript.exe`, with no
`x64\`. `RUN-ME.bat` falls back to it the same way.

It takes several minutes and prints a summary block at the end:

```
==== Test summary ====
files:   NN
tests:   NNN
passed:  NNNN
failed:  0
errors:  0
warnings:0
skipped: 0
```

### Reading it

- **`failed` or `errors` above 0** — stop. Something is genuinely broken. Do not
  promote a template or ship the change. The failing test names the file and the
  expectation.
- **`skipped` above 0** — **not** a clean bill of health. A skip means a test
  never ran, almost always because a tool is missing (Tesseract/Poppler for the
  OCR tests, or an R package). Install what is missing and run again. The runner
  exits non-zero on skips on purpose. Only if you *know* the box has no OCR by
  design, accept the gap knowingly with `set BSO_ALLOW_SKIPS=1` before the
  command — and write down why.
- **The pass condition is `failed: 0`, `errors: 0`, `skipped: 0`** — not a
  particular total. The `files` / `tests` / `passed` figures grow every time a
  test is added, so a higher total than last time is normal and healthy; a
  noticeably *lower* one means something did not run, and is worth chasing.

The last full run measured **76 files, 1,025 tests, 5,231 passing assertions** —
taken on 2026-08-25, at `VERSION` 1.5.0, on R 4.4.3. Treat it as a floor to
compare against, not a target to match.

That run was **not** clean, and the three that failed are worth knowing before
you chase them:

| What failed | Why |
|---|---|
| `no chart colour is three-digit hex` | its second assertion expects `col2rgb("#fff")` to error. Newer R accepts three-digit hex, so the assertion is stale, not the code. The first half of the test — no three-digit hex in `app.R` — still holds and still matters. |
| `the ANZ bundle splits into its statements with every KPI passing` | needs the OCR tools (tesseract / poppler-rasterising magick) |
| `the tutorial sample reconciles` | same |

18 tests skipped, all of them OCR. On a box **with** the OCR tools installed the
last two should pass; if they do not, that is a real finding.

**Check the `files` figure first.** If it is not the number of
`tests\testthat\test-*.R` files on your box, this line was written against a
different tree and the other two numbers are worth nothing to you. Re-measure
before you use them. (The suite checks that one figure for itself, so a stale
`files` count fails the board rather than sitting there being believed.)

**One exception to "stop".** If you have just added a reconciliation check, a
suite in the twenties of failures is the expected outcome of a correct change,
not a broken engine — and the runner will show you only the first ten of them.
Read [`../design.md`](../design.md) §8 *"Adding a check breaks about thirty
tests"* before you start unpicking them: it has the measured number, the command
that prints all of them, and the order to read them in.

The runner's exit code is `0` only when everything passed and nothing was
skipped.

---

## 2. Re-apply an engine parameter after an update

An update replaces the app folder ([updating.md](updating.md)). `R\params.R` —
the engine's numeric thresholds, catalogued in
[../context/engine-parameters.md](../context/engine-parameters.md) — lives in
`R\`, so **it is overwritten**. It is the one code file a maintainer is expected
to edit, and losing a tuning decision silently is exactly the failure this tool
exists to prevent.

1. **Before** the update, keep a copy of your edited `R\params.R` beside your
   backups ([backup-and-restore.md](backup-and-restore.md)). One file, easy to
   read.
2. Do the update.
3. Open the **new** `R\params.R` and the **old** one side by side and re-apply
   your values by hand. **Do not copy the old file over the new one** — a new
   version may have added parameters the rest of the engine now expects, and the
   old file would remove them.
4. Run the suite (§1). A parameter change with a failing test is that parameter's
   blast radius made visible — decide whether the new behaviour is intended
   before you accept it.
5. Convert one statement you know reconciles, and confirm it still does.

If you re-apply the same edit at every update, that value belongs in the shipped
defaults — send it back to whoever builds the package.

---

## 3. Promote a template to proven

**Proven is a gate, not a label.** Only templates in `templates\` feed the Qlik
dashboards ([connecting-qlik.md](connecting-qlik.md)), and only a template with a
golden test can prove it still parses what it claims to. A template dropped into
`templates\` **without** a golden test fails the suite's guard test on purpose —
that guard is what stops an untested layout reaching the dashboards.

So promotion is two moves, never one:

1. **Move the YAML** from `templates_user\<id>.yaml` to `templates\<id>.yaml`.
2. **Give it a fixture and a golden snapshot**, and run the suite.

| Step | Delimited (CSV/TSV) or Excel | PDF |
|---|---|---|
| Fixture | a small **synthetic** export under `samples\raw\<bank>\` | a synthetic one-page PDF from `tests\testthat\fixtures\make_pdf_fixtures.R` |
| Golden | `tests\testthat\expected\<id>.csv` from the engine's own parse — **read it by eye first** | same |
| Test | `tests\testthat\test-<id>.R` using `expect_statement_ok(...)` | an entry in `PDF_GOLDENS` in `tests\testthat\test-pdf_template_goldens.R` |
| Prove | the suite (§1): the new template passes **and no other bank breaks** | same |

Step by step, both paths:
[`tests/HOWTO-add-template-test.md`](../../tests/HOWTO-add-template-test.md).

**Never use a real customer statement as a fixture.** Fixtures are committed and
travel with the package. Invent the people, the accounts and the figures, and
make the invented statement reconcile (opening + transactions = printed closing)
so the test exercises the checks too.

If the template you are promoting was saved as a correction of a shipped one it
carries a `refines:` line naming the original. Decide whether you are promoting
it alongside the original or replacing it; two templates with the same
fingerprint and no `refines:` between them tie on every statement.

After promotion, tell the team: it starts feeding Qlik on the next conversion.

---

## Where else to look

| Question | Page |
|---|---|
| Somebody says a conversion is wrong — where do I start? | [investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) |
| What is this tool allowed to do, and never do? | [../context/charter.md](../context/charter.md) |
| How is it built, and how do I ship a change to it? | [../design.md](../design.md) |
| How does a statement get from upload to dashboard, and which module owns each step? | [../context/how-it-fits-together.md](../context/how-it-fits-together.md) |
| What does the data contract guarantee? | [../context/architecture/build-contract.md](../context/architecture/build-contract.md) |
| What does each numeric threshold do? | [../context/engine-parameters.md](../context/engine-parameters.md) |
| What does the engine not handle yet? | [../context/edge-cases.md](../context/edge-cases.md) |
| What is planned, and what was deliberately not built? | [../context/roadmap.md](../context/roadmap.md) · [../context/engine-audit.md](../context/engine-audit.md) |
| Getting the irreplaceable folders off the box | [backup-and-restore.md](backup-and-restore.md) |
