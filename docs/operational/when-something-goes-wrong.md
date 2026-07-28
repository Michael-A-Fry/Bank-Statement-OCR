# When something looks wrong

The tool never returns a silent wrong answer: a run that is not clean always says
**where**, **why**, **how bad** and **how to fix it**. This page is how to act on
that.

## Start here

Open **Show me how it read this** under the verdict. Everything below lives there.

| What you see | What it means | What to do |
|---|---|---|
| **Converted - please double-check it** | Usually a check failed. It can also mean the file looks like several statements bundled together, or that a second template nearly fitted it too. | Go to **Checks**, then **Diagnostics**. Read the *Detail* column, not just the result word. |
| **No template for this statement yet** | Nothing matched the layout. | [Add a template](adding-a-bank-template.md) — unless the card offers a choice between templates that fit equally well, in which case pick one and tell the maintainer: that is a duplicate to retire. |
| **Could not read this file** | The file is the problem. | The message says which: empty, wrong type, password-protected, or a scan with no OCR installed. |
| A check says it **could not be checked** | The check did not run. | Read its detail. Not an error; an absence of proof. |
| Confidence **low** | A check failed. | Do not rely on the figures until you know why. |
| Confidence **medium** on a PDF | Normal and healthy. | A PDF can never reach *high* — see [converting-statements.md](converting-statements.md). |

## Reading the Diagnostics table

Five columns: **Where**, **What**, **Severity**, **Detail**, **How to fix**.

- **Where** is the part of the *run* the row is about, and it is usually **not**
  a place in the statement. Only three of its forms point at rows —
  `rows 4,9,12`, `rows 1,2 (date)`, `rows 3 (amount)`. The rest name a stage:
  `file`, `upload`, `document`, `detection`, `template`, `template period`,
  `parse`, `rows`, `dates`, `completeness`, `balance check`, `running balance`,
  `amount direction`, `currency`, `redactions`, `pages (OCR)`, `OCR text`, or a
  bare `check`. On a run with nothing at all to note it is a bare `-`. On a file
  the tool split into several statements, each row's *Where* carries
  `[statement 2]` on the end so you can tell which one it is about.
- **What** is the problem in plain words, from a fixed list of twenty-seven.
  Every one of them is in the table below.
- **Detail** is the evidence — the figures, the row numbers, the wording it
  looked for.
- **How to fix** is a full sentence written for this statement.

Severity orders the table — **high** first, then **medium**, then **info** — so
the first row is always the one worth reading.

### Whose job is it? Read the *What* column

Every phrase the *What* column can print, and what it means for you:

| What it says | Whose job |
|---|---|
| layout not recognised · more than one template fits · matched the wording, read no transactions · the balance doesn't add up · running balance jumps · row count doesn't match · rows didn't parse · dates couldn't be read · dates in a different style than expected · date range doesn't look right · amounts couldn't be read · money in / out may be the wrong way round | **Yours, and it is most problems.** Open the toolkit on this file, change the one setting the *How to fix* sentence names, watch the preview, Save. |
| file could not be read · a scan with no readable text · several statements in one file · unusually large file · unusually large page · scan read with low confidence · scan quality unknown · completeness not auto-verified | **The file's.** Back to whoever supplied it: one statement per file, a cleaner scan at a higher DPI, a standard page size, or the bank's CSV/Excel export instead of the PDF. |
| several accounts in one statement · more than one currency · page(s) machine-read (OCR) | **A look, not a fix.** The figures were extracted. Read the *How to fix* sentence and give the data the glance it asks for. |
| redactions found and kept · what the PDF says about itself · no issues found | **Nothing.** Stated for the record. |
| hidden text could not be checked | **Stop.** The tool could not prove that text under a black box stayed hidden. **Do not release this output.** It is not the file's fault and not yours — the server is missing its image reader. Tell the maintainer. |

**"info" does not mean "nothing to do".** Six kinds of row are *info* severity
and three of them still ask you for something:

- *several accounts in one statement* — **info**, and its *How to fix* ends
  "review per account". If transactions from more than one account are mixed,
  the running balance is not continuous across them, which is exactly the kind
  of thing that looks like a break and is not one.
- *page(s) machine-read (OCR)* — **info**, and it says "spot-check machine-read
  values against the image".
- *more than one currency* — **info**, and it asks you to confirm how the
  foreign-currency lines are handled downstream.
- *redactions found and kept* — **info**, and it really does say "No action
  needed".
- *what the PDF says about itself* — **info**, and it says "No action — this is
  recorded, not a problem", then adds one conditional: if the origin of the
  document matters to this case, compare those details against the copy the bank
  issued.
- *no issues found* — **info**, and it is the whole table. Described at the end
  of this section.

So read the sentence, not the severity word.

**A clean run still has rows here, and that is normal.** The table is a record of
what the tool noticed, not a list of faults, so *Converted successfully* and
*Sent to the dashboards* routinely sit above one, two or three diagnostic rows.
Two real examples, both clean, both published, both re-run on 2026-07-28:

| Statement | What the Diagnostics table showed |
|---|---|
| A 7-row CSV with no balances printed | one **medium** row — Where `completeness`, What *completeness not auto-verified*, Detail "no balance or stated count to reconcile against" |
| A 311-row, 11-page PDF | two **info** rows — Where `upload`, *several accounts in one statement*, "5 account numbers appear in one statement period"; and Where `document`, *what the PDF says about itself* |

Both of those are *Converted successfully*, confidence **medium**, published to
the dashboards — with rows on this table. Read the *How to fix* sentence, not the
row count and not the severity word:

- **info** is a note. It is not always "nothing to do" — see the five kinds
  above. On the 311-row PDF, one of the two info rows asks you to review per
  account.
- **medium** on *completeness not auto-verified* is the same fact the confidence
  level already told you: the statement prints no balance and no count, so
  nothing could prove a row was not dropped. It is an absence of proof, not a
  discrepancy — count the rows against the statement if the total matters, or ask
  for a CSV export next time.
- Nothing on this table can quietly downgrade the verdict. The headline, the
  confidence level and the dashboard line are decided by the **checks**; the
  diagnostics explain them.

The single row reading *No issues detected; all applicable checks passed* is real
but rare — it appears only when the tool found **nothing at all** to note, which
a statement with any structure to it almost never manages. You will know it by
its shape as much as its words: *Where* and *How to fix* are both a bare `-`, and
*What* reads *no issues found*. Its absence is not a sign that something is
wrong.

The **Diagnostics** sheet in the downloaded workbook holds these same rows, plus
one extra column the screen leaves off: a short `fix_owner` code (`template`,
`input`, `review`, `none`, `escalate`) that says in one word what the table above
says in a row. It is there for whoever maintains the tool; you do not need it to
act, and where it disagrees with the *How to fix* sentence the sentence is the
one written for your statement. One row disagrees today: *page(s) machine-read
(OCR)* is coded `none` while its sentence asks you to spot-check the machine-read
values against the image. Do what the sentence says.

## The three most common causes, in order

1. **A column band in the wrong place (PDF).** Symptom: rows missing from the
   middle, or the check *No row failed to read* reporting more rows missed than
   read. Open **See it on the page**: green rows were kept, amber dashed rows
   look like transactions and were skipped. Fix in the toolkit by dragging the
   band.
2. **The wrong date format.** Symptom: dates that are real but wrong (day and
   month swapped), or *Row dates could be read* with a detail like "1 of 500 row
   date(s) read". Fix in the toolkit.
3. **The amount direction backwards.** Symptom: everything reconciles except the
   signs, or the balance is out by exactly twice a transaction. On an indicator
   (`D`/`C`) statement, check *which value means money out* in the toolkit.

## A genuine one-off row

Under **See it on the page**, *Rows skipped on this page — and why* lists what was left
out. If one of them is a real transaction, select it and click **This IS a
transaction — add the selected row**. It is added flagged `forced`, and the
confidence level drops accordingly: the row is visibly a human addition, for
good. Use it for a one-off. If several rows need it, the template is wrong, not
the statement.

## Reporting a problem safely (no client data)

**Never send the statement or any personal data** — no names, account or card
numbers, addresses, merchants, descriptions, real amounts or real dates.

Three ways, best first:

1. **Send it to the team from the app.** On a *No template for this statement
   yet* result, click **Send it to the team instead**. It raises a request with
   the layout shape and no statement content; it lands in **Admin → Insights →
   Format requests**.
2. **Download the shareable diagnostic.** Under **See it on the page**, the *Download
   shareable diagnostic* button gives a small markdown file: the band frame, the
   page count, kept and skipped row counts, per-band word counts. No statement
   text at all.
3. **Survey the layout with an AI assistant.** Attach the statement to Copilot
   and paste the ready-made prompt in
   [survey-a-statement-with-ai.md](survey-a-statement-with-ai.md). It asks the
   right questions in a fixed order and is written to keep every client detail
   out of the answer. Doing this for a stack of problem statements at once is the
   single most useful thing to send — it is how awkward layouts get fixed
   properly instead of one at a time.

If you write the description yourself, turn every real value into its shape —
letters to `x`/`X` by case, digits to `9`, so `Countdown 47.20 on 17 Sep` becomes
`Xxxxxxxxx 99.99 on 99 Xxx`. What is actually useful:

- Whether it is a transaction statement or a form/summary.
- The columns left to right and what each holds, and whether there are two dates
  per row.
- The date format as a shape (`99/99/9999`, `99 Xxx`), and whether the year is on
  each row or only in the period line.
- How direction is shown: a minus sign, separate in/out columns, a `DR`/`CR`
  suffix, a `D`/`C` column.
- Any redactions: which column, roughly where.
- Anything unusual: several accounts in one file, summary rows inside the amount
  column, running-balance resets, foreign-currency lines.

The maintainer can also generate all of this from the file itself —
**Admin → Insights**, pick the upload, *Download its safe summary (no personal
data)*. Every value is masked to its shape.
