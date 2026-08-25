# Rolling back a bad version

[updating.md](updating.md) goes forward. This page goes back, and it is written
for the morning you need it.

Rolling the app back is the easy half and takes ten minutes. The hard half is
**the conversions the bad version already sent to the Qlik dashboards** — those
do not roll back with it, and nothing removes them on your behalf. That is
section 4, and it is the section to read first if analysts are already using the
numbers.

---

## 1. First, make sure it is the version

Ten minutes here saves rolling back for nothing — and a rollback of its own
accord loses work.

| Ask | How | Read it as |
|---|---|---|
| Did **several unrelated banks** break at once? | Convert one statement from each of two banks that worked last week | Two banks = the build. One bank = that bank's template. |
| Did the **template** change too? | Compare `template_sha256` in the two `logs\runs\<run_id>.json` records — the old good one and today's bad one | Different hash = somebody edited or promoted that template. **That is your answer; do not roll back.** Fix the template. |
| Did the **build** change? | Compare `engine_version` in the same two records | Same version and different figures on 1.4.0 or later is a finding in itself — [investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) §4 |
| Is it the **file**? | Was it a scan? | OCR quality varies statement to statement and is not a version problem |

Full procedure for a single disputed figure:
[investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md). Come
back here only when the build is the answer.

**Whatever you decide, stop conversions first.** Close the app (`Ctrl-C`, or end
the scheduled task) and tell the team. Every conversion that runs while you are
deciding adds another row to the feed you may have to withdraw.

## 2. Roll the app folder back

**What you need is the previous *bundle* — the `StatementStudio-offline` folder
as it came off the internet PC** — not a copy of the live server folder. Keep the
last two bundles for exactly this. If all you have is a copy of the live folder,
read the warning at the end of this section before you touch anything.

1. **Stop the app.** `Ctrl-C`, or end the scheduled task.
2. **Rescue anything promoted since the update.** `templates\statements\` **is**
   in the bundle, so it is replaced by the rollback. If a template was promoted to
   proven since the update, copy `templates\statements\<id>.yaml` somewhere safe
   now — and its golden test with it if you added one.
3. **Do not delete the current folder.** Its `logs\` are the evidence of what the
   bad version did, and section 4 needs them.
4. **Copy the old bundle over the app folder** and choose **Replace the files in
   the destination** — exactly the update procedure
   ([updating.md](updating.md) §2), with an older bundle. Only the files the
   bundle carries are replaced; `config\`, `dictionaries\`, `templates\statements_user\`,
   `templates\fields_user\`, `templates\documents_user\`, `logs\`, `uploads\` and
   `feed\` are not in it and are not touched.
5. **Re-apply your `R\params.R` values by hand** if you had any. `R\` is in the
   bundle, so the rollback overwrote them just as the update did
   ([maintaining-the-engine.md](maintaining-the-engine.md) §2).
6. **Double-click `RUN-ME.bat`.** No internet is needed and nothing is
   uninstalled — the private R lives inside whichever folder you run.

**Check you are actually on the old version.** Convert
`samples\raw\anz\anz_transaction_export_01.csv` and open the JSON download:
`build.engine_version` must read the **old** version. If it reads `unknown` the
`VERSION` file did not travel — fix that before anything else, because section 4
depends on being able to tell one build's rows from another's
([go-live-checklist.md](go-live-checklist.md) §2).

Then run the suite ([maintaining-the-engine.md](maintaining-the-engine.md) §1)
and convert one statement you know reconciles.

> **If all you kept is a copy of the whole live folder, do not restore it
> wholesale.** That copy contains its own snapshot of `dictionaries\`,
> `templates\statements_user\` and `config\config.yaml` as they were on the day you took it.
> Putting it back reverts every word Admin has taught the tool and every template
> your team has built since — the two things
> [backup-and-restore.md](backup-and-restore.md) calls irreplaceable — and
> `RUN-ME.bat` will then overwrite its own `%LOCALAPPDATA%\StatementStudio`
> backup with the stale copies, taking the safety net with it. Instead: copy the
> current `dictionaries\`, `templates\statements_user\`, `templates\fields_user\`,
> `templates\documents_user\` and `config\config.yaml` out of the live folder first,
> restore, and put them back before you start the app.

## 3. What the rollback costs you

| Kept — none of this is in the bundle | Lost — all of this is |
|---|---|
| `config\config.yaml` — your settings | every fix and change in the newer version |
| `dictionaries\` — every wording and marker taught in Admin | any template **promoted** into `templates\statements\` since the update (step 2 rescues it) |
| `templates\statements_user\`, `templates\fields_user\`, `templates\documents_user\` — every template built in the app | any `R\params.R` value you re-applied after the update (step 5) |
| `logs\` — the whole audit trail, including `logs\metadata\` | |
| `uploads\` — the kept copies of the statements | |
| `feed\` — **including the rows the bad version published** | |

The last row is the one people miss, and it is section 4.

## 4. The rows the bad version already sent to Qlik

**They stay.** Rolling the app back does not touch `feed\`, and if Qlik has
reloaded since the bad version went in, those figures are already on the
dashboards and may already be in somebody's report. **Nothing withdraws them
automatically. Somebody has to.**

### 4a. Work out which rows they are

`feed\runs\*.csv` is one row per statement and carries both `engine_version` and
`converted_ts`. In Qlik, the same fields are on the `Runs` table.

- **Filter `engine_version` to the bad version.** That is the whole list, and it
  is why the build stamp is worth checking at go-live.
- **If `engine_version` reads `unknown`**, the `VERSION` file never reached the
  server and every row on the box carries the same value, good and bad alike. Fall
  back to `converted_ts` and take everything between the update and the moment you
  stopped conversions in section 1. That window is wider than the real damage, so
  you will withdraw some good rows too — they come back by re-converting.
- `gate_result` = `accepted` marks the ones that actually reached the dashboards.
  Anything already `withheld:...` never got there.

### 4b. Put the right figures back — the route that fixes rather than deletes

Every feed file is keyed by the statement's **content hash**, so **re-converting
the same statement overwrites its transactions file and its manifest row in
place**. The corrected figures replace the wrong ones, the statement is still
counted once, and no dashboard total goes hollow.

So for every affected statement you can still lay hands on — they are under
`uploads\`, unless retention has passed
([investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) §3) —
**convert it again on the rolled-back version.** That is the fix. Do it in the
app, with the Convert button; the command-line scripts deliberately do not feed.

### 4c. Withdraw what you cannot re-convert

Only for a statement you **no longer have** — retention has passed, or it came
from a case that is closed. If you have the file, 4b is the answer and this
section is not: a re-convert on the good version has already replaced the row,
and rating that corrected run *Wrong* would withdraw the **right** figures.

Rating a run **Wrong** is still the right move in one case here: you re-convert,
and the rolled-back version reads it wrongly too. Then the rating moves its rows
out of `feed\transactions\` into `feed\review\`, stamps the manifest row
`retracted:user_reported_wrong`, and tells you on screen how many rows were
withdrawn — per-run, by a person, which is what an evidence trail wants.

**There is no way to rate a historic run from Admin** — the rating lives on the
conversion result in front of you, not on the run record. So for a statement you
cannot re-convert, it is by hand, with the app stopped. For each `<hash>`:

1. **move** `<feed_dir>\transactions\<hash>.csv` to a folder **outside**
   `feed_dir` — a dated `withdrawn\` folder beside it. Move, not delete: it is
   evidence, and nothing here should destroy a record;
2. open `<feed_dir>\runs\<hash>.csv` in Notepad and change `gate_result` from
   `accepted` to `retracted:withdrawn_by_hand`, and blank the `feed_file` value.
   One row, two cells.

**Step 2 is not optional.** Move the transactions file alone and the manifest
still says that statement was published: the coverage table claims a row the
dashboard no longer has, which is a silent loss of exactly the kind this tool
exists to prevent. Doing both is what the app itself does when somebody rates a
run *Wrong*.

Outside `feed_dir`, not into `feed\review\`, on purpose. `review` is a table Qlik
may be loading, and the rows inside the file still carry `accepted` in their own
`gate_result` column — dropping them there could put the very figures you are
withdrawing straight back onto a sheet.

### 4d. Nothing has happened until Qlik reloads

Withdrawn rows are still on the dashboards until the next reload. **Force one in
the QMC** rather than waiting for the schedule, and confirm afterwards that the
row count moved and that `Runs` shows the retractions.

### 4e. Tell the Qlik analysts, and be honest about the limit

The tool can withdraw a row from the feed. **It cannot withdraw anything
downstream of Qlik** — a figure already exported to a spreadsheet, pasted into a
report or quoted in a statement of evidence is out of its reach entirely.

So send the analysts three things:

1. the **window** — from the update to the moment conversions stopped;
2. the **list** of `source_file` / `run_id` values you withdrew, straight out of
   `feed\runs\*.csv`;
3. what was wrong with the figures, in one sentence, so they can judge whether
   anything they have already used is affected.

That message is the only thing that reaches work already done. Send it the same
day.

## 5. Close it out

- Record what happened and why in
  [`../context/findings-register.md`](../context/findings-register.md), with the
  run ids as evidence. The changelog is built from that register.
- Send the bad version back to whoever builds the package, with the two run
  records — the good one and the bad one — attached. A rollback that nobody
  diagnoses gets shipped again.
- Take a backup ([backup-and-restore.md](backup-and-restore.md)) before you go
  forward again, and keep **both** bundles this time.

---

Related: [updating.md](updating.md) ·
[go-live-checklist.md](go-live-checklist.md) ·
[backup-and-restore.md](backup-and-restore.md) ·
[investigating-a-wrong-conversion.md](investigating-a-wrong-conversion.md) ·
[connecting-qlik.md](connecting-qlik.md)
