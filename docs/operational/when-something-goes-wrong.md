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
*Where* is the part of the statement — a row range, `file`, `detection`,
`redactions`, `pages (OCR)`. *What* is the problem in plain words. *Detail* is
the evidence. **The How to fix column is the one that tells you whose job it
is**, and it does it by naming the action:

| If How to fix says… | It is | Do |
|---|---|---|
| anything naming **the toolkit** — "re-map it in the template toolkit", "fix the Date column / format", "set the correct amount style", "check this template's delimiter and header/footer settings" | **yours, and it is most problems** | Open the toolkit on this file, change that one setting, watch the preview, Save. |
| **split** it into one statement per file · **re-export** at a standard page size · **re-scan** at higher DPI · "check those pages against the source PDF" | **the file's** | Back to whoever supplied it: one statement per file, a cleaner scan, or the bank's CSV/Excel export instead of the PDF. |
| **compare** · **check** · **review** · **spot-check** — "compare the total against the source", "check the rows around the break", "review per account", "spot-check machine-read values against the image" | **a look, not a fix** | The figures were extracted; they want a human glance against the statement. |
| **"No action needed"**, or a plain statement of what the PDF says about itself | **nothing** | Informational. Redactions kept as `[REDACTED]`, OCR was used, the PDF names the tool that wrote it. |
| none of the above, and nothing in it you can act on | **not yours** | Report it (below). Rare, and it means the engine needs a general improvement that then helps every bank. |

Severity orders the table — **high** first, then **medium**, then **info** — so
the first row is always the one worth reading. A clean run shows a single row
saying so.

The **Diagnostics** sheet in the downloaded workbook holds these same rows, plus
one extra column the screen leaves off: a short `fix_owner` code (`template`,
`input`, `review`, `none`, `escalate`) that says the same thing as the table
above in one word. It is there for whoever maintains the tool; you do not need it
to act.

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
