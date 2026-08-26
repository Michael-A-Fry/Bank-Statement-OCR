# Someone says a conversion is wrong

For the **maintainer**, on the day it happens. The analyst's own page is
[when-something-goes-wrong.md](when-something-goes-wrong.md) — that one is about
reading the verdict. This one is about going back to a conversion that already
happened, reproducing the exact figure, and deciding whose problem it is.

Everything you need is already on the box. Nothing here needs the internet, and
nothing here changes a stored figure — there is no cell to edit, and the fix is
always a template correction and a re-run.

---

## 1. Get the run id

Every conversion has a **run id** — a content hash, a UTC timestamp and a short
random suffix, e.g. `bbf83b15b9-20260727200534-bbee`. It is not printed on the
analyst's screen, so do not ask her for one. You get it yourself:

**Both routes below are the Admin tab, and Admin will not open at all until the
admin password has been set** — `app.admin_password` in `config\config.yaml`,
[first-time-setup.md](first-time-setup.md) §4. On a server where nobody ever did
that, do that first; it is a two-minute job and a restart. (Everything from step 2
on works without it.)

| Where | Table | Use it when |
|---|---|---|
| **Admin → Templates** | *What the team said about these conversions* — `ts`, `verdict`, `comment`, `template_id`, `run_id`, and the document it was left on | somebody rated a result **wrong** or **minor issues** in the app |
| **Admin → Health** | *Uploads* — `ts`, `file_ext`, `status`, `template`, `trust`, `needs_pickup`, `purged`, `run_id` | you know the file or roughly when it was converted |

If she can only tell you the file name and the day, the Uploads table is the
route: it lists every statement converted on this server, newest first.

A verdict of **wrong** has already withdrawn that run's rows from the published
feed before you read it, and marked the run `retracted:user_reported_wrong` in the
feed manifest so the coverage table shows a retraction rather than silently losing
a row. With `feed.include_review_feed` on (the shipped default) the rows are moved
to `feed\review\` rather than destroyed. Either way the dashboards are safe while
you work.

## 2. Read the run record

One JSON file per run, never overwritten: `logs\runs\<run_id>.json`. Open it in
Notepad. The fields that decide what you do next:

| Field | What it tells you |
|---|---|
| `source_file`, `source_sha256` | which file, and its exact bytes |
| `detected_template`, `template_origin` | which template ran, and whether it was shipped (`default`) or built in the app (`user`) |
| `template_version`, `template_sha256` | **which content** of that template ran |
| `detect_detail` | how it was chosen, e.g. `matched anz_everyday_csv (score 7/6)` |
| `engine_version` | which build |
| `status`, `trust_level`, `kpi_fail_count` | the verdict she saw |
| `row_count`, `pages`, `period_start`, `period_end` | the shape of what was read |
| `requested_by` | the identity the engine had. The screen amends the record afterwards with `attested_by` (the QID that was typed), `detected_identity` and `identity_source`, so a claim and a proven identity are never the same field |

`logs\metadata\<run_id>.json` holds the local-only capture for the same run (how
the columns looked, how long it took). `logs\feedback\<run_id>__<stamp>.json`
holds her verdict and comment.

## 3. Find the original file

The uploaded statement is kept byte-for-byte under `uploads\<upload_id>\`,
beside a `record.json` that names the `run_id`. So the run id finds the file —
search every upload record for it:

```
cd /d D:\StatementStudio-offline\uploads
findstr /s /m "<run_id>" record.json
```

The folder that comes back holds the statement itself, under its original name.
(The upload id is *not* the run id: one uploaded file can be converted more than
once, so they are deliberately separate.)

**If nothing comes back**, the copy has passed its retention period and been
deleted (`purged: true` in the record, and in the Admin Uploads table). The
record of what happened is kept forever; the statement is not. Ask for the file
again — and note that without it you cannot reproduce anything, which is the
honest answer to give.

## 4. Reproduce the figure

`run.R` is the whole engine with no Shiny in front of it. From the app folder:

```
cd /d D:\StatementStudio-offline
set "R_LIBS_USER=%CD%\R-lib"
set "R_LIBS_SITE="
"R-runtime\bin\x64\Rscript.exe" run.R uploads\<upload_id>\<file> "" out\check
```

Usage is `Rscript run.R <file> [bank] [outdir]`. The second argument is a **bank
hint** — leave it `""` unless you are deliberately forcing detection down one
bank's templates.

Its first line is the thing you need next:

```
run id:      d53202504d-20260727233840-b6e0
status:      ok
template:    anz_everyday_pdf
trust:       medium (score 92)
```

That id names the record this re-run just wrote — **`logs\runs\` holds one file
per conversion and there are hundreds of them**, so read it off the console
rather than hunting by timestamp. It then prints every check with its detail and
the three output paths.

The re-run is a conversion like any other, so it **writes its own new record**
under `logs\runs\` with `requested_by: cli`. That is deliberate — the
investigation leaves a trail too — but it means Admin's Health tab and coverage
tables will show your check runs alongside the analyst's. They are the ones with
`cli` as the requester.

Run it **from the app folder**, not from anywhere else: `templates\statements_user\` is
resolved relative to the working directory, and a run started somewhere else
would silently leave out every template built in the app — including, quite
possibly, the one that produced the figure you are chasing.

Three things to confirm before you believe the re-run, in this order:

1. **The same build ran.** Compare `engine_version` in the new
   `logs\runs\<run_id>.json` against the old one. If the app has been updated
   since her conversion they will differ, and then the two *downloads* **cannot**
   be byte-identical however right the figures are: the same stamp is written
   *inside* every output JSON, on its third line, as `build.engine_version` — so
   one number changing changes the file. That on its own is not a fault and not
   an answer — carry on to 2 and 3. And if the two stamps are the **same** but
   read `1.3.0` or earlier, that is not proof the same code ran either: see the
   caveat at the end of this section.
2. **The same template ran.** Compare `detected_template` and `template_sha256`.
   If the hashes differ, the template has been edited since — *that is already
   your answer*, and the old figure cannot be reproduced from today's template.
3. **The figures match.** Compare `row_count`, `status`, `trust_level` and the
   figures themselves — the check details the re-run printed against the ones in
   the old record, and if you still have her download, the `Transactions` sheet
   against the new one.

**Compare the figures, not the bytes.** Same input + same template + *same build*
gives the same bytes, and that is a real guarantee — the suite proves it on every
run. But two entirely ordinary things break it with nothing wrong at all: the app
has been updated, or the template has been edited. So:

- same `engine_version` **and** same `template_sha256` → a byte-for-byte match is
  the expected outcome, and any difference is a finding. **Read the caveat below
  first**: this holds only when both records were written by **1.4.0 or later**;
- either one different → the bytes will differ, and that on its own tells you
  nothing. Compare the **figures**: `row_count`, `status`, `trust_level`,
  `kpi_fail_count`, and the check details the re-run printed against the ones in
  the old record. A difference in any of those is the finding. If it was the
  *build* that changed, read [`../../CHANGELOG.md`](../../CHANGELOG.md) for the
  newer version before you call it one — a verdict that moved from *please
  double-check it* to *converted successfully* between two builds is usually a fix
  that is written down there, and the changelog names it.

A re-run that reproduces 11 rows, `ok`, `medium` and the same `template_sha256`
off a newer build has reproduced the figure. It has not failed to.

### The caveat: before 1.4.0, `engine_version` does not identify a build

The stamp was only moved at release time, while the engine was changed many times
*within* a release. So two records can carry the **same** `engine_version` and the
**same** `template_sha256` and still disagree — and pairs like that exist on this
box, permanently, because the records are never rewritten.

A real one, still there: the same 7-row ANZ PDF, the same template hash
`f4e79203...`, both stamped `1.3.0` —

| Run | Written | `status` | `trust_level` | `kpi_fail_count` |
|---|---|---|---|---|
| `0fdca7c700-20260728004032-3a82` | 00:40, 28 Jul 2026 | `needs_review` | low | 1 |
| `0fdca7c700-20260728061535-5908` | 06:15, 28 Jul 2026 | `ok` | medium | 0 |

Nothing is broken. An engine fix landed between the two, and the later record is
the right one. The headline test above, applied to that pair, would send you
hunting a defect that is really the fix.

**So when both records read `1.3.0` or earlier, the byte-for-byte rule does not
apply.** Order the two by the timestamp inside the run id, read the changelog for
what changed between them, and decide on the figures. From **1.4.0** the stamp
moves with every shipped change of behaviour, which is what makes it usable as a
test at all — that is why 1.4.0 exists.

**And if `engine_version` reads `unknown`, it is not a build id at all.** That
means the `VERSION` file never reached this server, so every record on the box
carries the same word whatever produced it — the same trap as above, permanently,
for every run. The rule does not apply to any of them; compare the figures and
the timestamps instead. Fix the cause before it grows:
[go-live-checklist.md](go-live-checklist.md) §2.

## 5. Template bug, or bad file?

| Symptom in the re-run | Read it as |
|---|---|
| `detected_template` names a template for a **different bank** | a fingerprint that is too loose. Fix the phrase or the `min_score` on the template that matched, not on the one that should have. |
| The right template, but rows missing from the middle of a PDF | a column band in the wrong place. Open the file in the toolkit and look at **See it on the page**: amber dashed rows look like transactions and were skipped. **Template.** |
| Dates real but wrong (day/month swapped), or a low count on *Row dates could be read* | the date format. **Template.** |
| Balance out by exactly twice one transaction, or every sign inverted | the amount style, or *which value means money out* on an indicator column. **Template.** |
| The run was OCR'd (`pages` set, *Scan / OCR read quality* present) and individual digits are wrong | **the file.** Ask for a better scan or the bank's CSV/Excel export. OCR accuracy is not something the engine can improve from here. |
| A delimited or Excel export reads wrong | never a scan — it is the template or the export itself. Compare the file's own header row against the template's `columns:`. |
| Everything reconciles and she still says it is wrong | ask which row and which figure. A conversion that reconciles to the cent and reads a description verbatim is usually a disagreement about what the statement means, not about what it says. |

A template fix goes back through the toolkit like any other
([adding-a-bank-template.md](adding-a-bank-template.md)); correcting a shipped
template saves as `<id>_custom` with a `refines:` line and takes effect on the
next conversion. Converting the file again is what puts the corrected figures
back in front of the dashboards — there is no way to publish a withheld run
without re-running it, and that is deliberate.

## 6. Close it out

- Convert the file again in the app so the corrected run is the one on record.
- If you promoted or edited a shipped template, run the suite
  ([maintaining-the-engine.md](maintaining-the-engine.md) §1).
- If the cause was the engine rather than a template, it belongs in
  [`../context/findings-register.md`](../context/findings-register.md) with its
  evidence — that register is what the changelog is built from.

---

Related: [when-something-goes-wrong.md](when-something-goes-wrong.md) ·
[maintaining-the-engine.md](maintaining-the-engine.md) ·
[backup-and-restore.md](backup-and-restore.md) ·
[`../design.md`](../design.md) (the map: which module owns each step)
