# Maintaining the engine (the maintainer's runbook)

For the **one analyst who owns this tool** — the person who inherits it, not a
software engineer. Everything here is a copy-paste command or a file copy.

Day-to-day upkeep (Health, requests, dictionaries, tidying logs) is
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
verbatim descriptions, no silent drops, and every shipped
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
  noticeably *lower* one means something did not run, and is worth chasing —
  unless scope was deliberately removed, which is what happened at 1.9.0 and is
  the only reason the figures below are lower than 1.8.1's.

The last full run measured **73 files, 1,031 tests, 5,553 passing assertions,
0 failed, 0 errors** — taken on 2026-10-03, at `VERSION` 1.21.0, on R 4.3.3,
with 1 skipped: one split test needs a Westpac bundle that lives in
`samples/_private_staging/` and is deliberately not committed. Treat it as a
floor to compare against, not a target to match.

**After any change to `app.R`, `www/app.css` or `R/identify.R`, also press the
buttons:** `node tools/ui/check.mjs` drives the Convert screen in a real browser
and fails when it does not do what it says (see [`tools/ui/README.md`](../../tools/ui/README.md)).
The suite reads `app.R` as text; only this proves the screen works.

Run it as `NOT_CRAN=true BSO_ALLOW_SKIPS=1 Rscript tests/run_tests.R`. Without
`NOT_CRAN` the OS-level concurrency proof in `test-jobs.R` skips itself — ten
assertions quietly not run, and they are the ones that prove the job cap is
enforced by the operating system rather than merely bookkept.

## How long a big statement takes

Measured at 1.10.0 on this build (R 4.3.3, poppler 24.02.0, four cores), on synthetic
statements that reconcile exactly — `tools/synth/make_bench.py` draws them,
`tools/synth/bench.R` times them:

| pages | rows | total | per page | peak extra memory | wrong figures |
|---|---|---|---|---|---|
| 30 | 900 | 5.7 s | 0.19 s | ~6 MB | 0 |
| 100 | 3,000 | 10.2 s | 0.10 s | ~2 MB | 0 |
| 200 | 6,000 | 18.8 s | 0.09 s | ~2 MB | 0 |
| 400 | 12,000 | 37.6 s | 0.09 s | ~5 MB | 0 |

**It is linear in pages and flat per page**, which is the property that matters: a
statement twice the size takes twice as long and no more. Memory is negligible, so
there is no size at which the server runs out.

Two things to know when a number here looks wrong:

- **`parse` is about 70% of it** (47.8 s of the 400-page run) and it is diffuse —
  profiling puts a third of the time in `sub`/`gsub`/`grepl` spread across cell
  normalisation, with no single hot spot. There is no cheap win left in it; a faster
  parse means changing `.num()`, which is the money reader, and that is not a change
  to make for speed alone.
- **Conversions run in child processes** (`R/jobs.R`), so a 400-page job does not
  freeze anyone else's browser. The cap on concurrent jobs is what protects the
  machine: tesseract is OpenMP-parallel, and three concurrent *scans* on four cores
  had not finished a single page after ten minutes.

### A SCAN is 55 times slower, and that is the number that matters

| | per page | 120 pages |
|---|---|---|
| digital PDF (a text layer) | 0.17 s | ~20 seconds |
| **scanned PDF (OCR)** | **9.3 s** | **~19 minutes** |

Measured on a rasterised specimen: 3 pages, 27.8 s, OCR confidence 81, and it detected
and parsed correctly -- the quality is fine, it is only slow. tesseract dominates it.

Nineteen minutes behind a screen that says "Converting statement..." is
indistinguishable from a hung tool, so `conversion_estimate()` probes the file first
(pdfinfo for the page count, the first three pages' text for the scan test -- 0.05 s on
a 400-page file) and the wait is stated up front. It says nothing under half a minute.

### Several analysts at once

Five 100-page conversions started together, cap of 3, four cores:

```
analyst 1: running   0 ahead     t=  0.6s  running running running queued queued
analyst 2: running   1 ahead     t= 15.2s  done    done    running running running
analyst 3: running   2 ahead     t= 17.3s  done    done    done    running running
analyst 4: queued    3 ahead     t= 43.8s  done    done    done    done    done
analyst 5: queued    4 ahead
```

**43.8 s wall for five, against 17.0 s for one**, and all five produced the full 3,000
rows plus a workbook. Nothing thrashed and nothing was dropped. `job_queue_ahead()` is
what lets the screen say "you are fifth in line" rather than leaving someone guessing.

The cap is 3 on four cores, and it is the number that protects the machine: tesseract
is OpenMP-parallel, and three concurrent **scans** on four cores had not finished a
single page after ten minutes. Raise it only with a measurement.

### The biggest output

400 pages / 12,000 rows produces a 1.1 MB workbook with all six sheets
(Transactions, Summary, Checks, Provenance, Diagnostics, Metadata), 12,000 rows on
sheet 1, status `ok`. Writing it is 1.4 s of the 69.5 s.

Re-run the benchmark after any change to reading, parsing or the diagnostics, and
compare against the table. Two of the four stages were rewritten on the strength of
it: the sign scan was reading the whole document once per call, and the drift check
was 46% of a 100-page conversion before it was capped to eight sampled pages.

**They fell again at 1.11.0, and that is also correct.** The redaction-suppression
machinery was deleted — nothing withholds readable text any more — so two test files
and about twenty-five test blocks went with it, along with two KPIs and two
diagnostics. Nothing stopped running that still has code behind it.

**The figures fell between 1.8.1 and 1.9.0, and that is correct.** 1.9.0 removed
the form (`mode: fields`) and report (`mode: document`) routes entirely — the
tool converts bank statements and nothing else — so seven test files and the
engine modules under them went with them. Nothing stopped running that still has
code behind it.

**That run was clean, and it is the first one that was.** For a long time the
board carried five red lines, explained here and elsewhere as "the environment,
not the code". That explanation was wrong, and wrong in the worst available way:
four of the five were **test bugs**, the engine was correct throughout, and a
permanently red board teaches everybody to read past red. What they actually
were:

| What it looked like | What it was |
|---|---|
| `detect_dark_regions finds a solid black box` (3) | the test built its picture with `magick::image_draw`, which hands back an image still attached to a live graphics device — so the answer depended on what else had touched magick first. Rendered to a PNG and read back, it is right every time. |
| `the skew estimator` and `deskew straightens the page` (2) | the same mistake in the same way, in another file. |
| `no chart colour is three-digit hex` | asserted `col2rgb("#fff")` errors. It did on the R this shipped on; newer R accepts it. The rule it protects — no three-digit hex in `app.R` — is asserted separately and still passes. |

The same fixture also leaked a graphics device, which is why an `Rplots.pdf`
used to appear in the app folder after every suite run. It no longer does.

**So the pass condition means what it says: `failed: 0`, `errors: 0`.** A red
line is a finding. Do not inherit an explanation for one from anybody, including
this page.

19 tests skipped on that box, all of them needing tesseract/poppler or a fixture
those tools generate. On a box **with** the OCR tools installed they should pass
too; if they do not, that is a real finding.

### The five-second version: `scripts\health-check.R`

The suite takes minutes and proves the *engine* is right. It says nothing about
whether *this server* can run it. That is a different question, asked the same
way, with the same two lines of setup:

```
cd /d D:\StatementStudio-offline
set "R_LIBS_USER=%CD%\R-lib"
set "R_LIBS_SITE="
"R-runtime\bin\x64\Rscript.exe" scripts\health-check.R
```

Five checks, one line each, `PASS` or `FAIL`, and a non-zero exit code on any
failure so a scheduled task can watch it without anybody reading it:

| Check | Answers |
|---|---|
| **Settings** | did `config\config.yaml` parse, and was every setting in it understood |
| **Admin** | is Admin still shut behind the placeholder password shipped in the example file |
| **Templates** | how many bank statement / form / report templates loaded — **and how many were refused, with the reason** |
| **Folders** | can this account really write to `logs\`, `uploads\`, `requests\`, the three `templates\*_user\` folders and the feed folder |
| **Scans** | is tesseract/poppler still installed, or has scanned-statement reading gone away |

It reads and changes nothing, so run it whenever. **Templates is the one to read
twice.** A template that stops validating after an update used to simply vanish —
gone from Admin, gone from detection, no message anywhere — and the statements it
used to read quietly became "no template for this statement yet". This is the
line that says so.


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

**Proven is a gate, not a label.** Only templates in `templates\statements\` feed the Qlik
dashboards ([connecting-qlik.md](connecting-qlik.md)), and only a template with a
golden test can prove it still parses what it claims to. A template dropped into
`templates\statements\` **without** a golden test fails the suite's guard test on purpose —
that guard is what stops an untested layout reaching the dashboards.

So promotion is two moves, never one:

1. **Move the YAML** from `templates\statements_user\<id>.yaml` to
   `templates\statements\<id>.yaml`.
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
