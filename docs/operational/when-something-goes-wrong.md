# When something looks wrong

The tool never returns a silent wrong answer. A statement is converted
automatically only when its own arithmetic proves the reading. Anything else
says **where**, **why**, **how bad** and **how to fix it**. This page is how to
act on that.

## Start here

1. **Read the outcome in the table, and its reason.** *Please check: The balance
   does not add up at row 14 (page 2)* names the row, and the row is usually the
   answer.
2. **Open Please check** (*Please check ->* in the row). It shows the columns the
   tool found, drawn on the page for a PDF or listed for a CSV or Excel file,
   with a tick or a cross for each page, and the checks that did not hold. Most
   problems are fixed there in two clicks: set the column that is wrong, then
   **Re-read** ([converting-statements.md](converting-statements.md)).
3. **For the detail**, the **Checks**, the **Diagnostics** and the **Field
   coverage** sit under your transactions in one disclosure headed
   **Checks & detail (for review)**. It is already open whenever the run needs a person. On
   a clean run you open it yourself, in one click.

| What you see | What it means | What to do |
|---|---|---|
| **Please check** | Read, but not proven. Common causes: a balance step that does not add up, a page that gave no rows, a dated line the reading missed, no balance and no totals to prove it by, an amount filled in from the balance, or two readings that both fit. | Open Please check. If the reading is wrong, set the column and Re-read. If it is right, press **This is right**. |
| **Couldn't read** | Nothing usable was read, or the file could not be opened. | The reason says which. A damaged, empty or password-protected file needs a fresh copy. A scan on a server with no scan-reading software cannot be read here at all. |
| **Which bank? The statement looks like X** | You picked one bank and the statement names another. | Answer it with the two buttons on the result. The figures are not affected; only learning waits. |
| A check says it **could not be checked** | The check did not run. | Read its detail. It is not an error, just an absence of proof. |
| A row **shaded**, with *worked out from the balance column* in Flags | The amount could not be read, and the balances either side say what it must be. | Check that amount against the statement before relying on it. |

## Reading the Diagnostics table

Five columns: **Where**, **What**, **Severity**, **Detail** and **How to fix**.

- **Where** is the part of the *run* the row is about, and it is usually **not**
  a place in the statement. Only three of its forms point at rows: `rows 4,9,12`,
  `rows 1,2 (date)` and `rows 3 (amount)`. The rest name a stage: `file`,
  `reading`, `Please check`, `bank`, `amounts`, `upload`, `document`, `signs`,
  `completeness`, `balance check`, `running balance`, `parse`, `rows`, `dates`,
  `account number`, `amount direction`, `currency`, `pages (OCR)`, `OCR text`, or
  a bare `check`. On a run with nothing to note it is a bare `-`. On a file
  split into several statements, a check's row carries `[statement 2]` so you
  can tell which statement it is about.
- **What** is the problem in plain words, from a fixed list. Every entry is in
  the table below.
- **Detail** is the evidence: the figures, the row numbers, the reader's reason.
- **How to fix** is a full sentence written for this statement.

Severity orders the table, **high** first, then **medium**, then **info**, so
the first row is always the one worth reading.

### Whose job is it? Read the *What* column

| What it says | Whose job |
|---|---|
| the arithmetic did not prove the reading · nothing usable was read · the fix could not be applied · amounts filled in from the balance · the bank needs confirming · the balance doesn't add up · running balance jumps · row count doesn't match · rows didn't parse · dates couldn't be read · dates in a different style than expected · date range doesn't look right · amounts couldn't be read · money in / out may be the wrong way round | **Yours, on Please check, and it is most problems.** Set the column the *How to fix* sentence points at and Re-read. If the reading is right, confirm it. If the bank is in question, answer *Which bank?*. |
| file could not be read · a scan with no readable text · several statements in one file · unusually large page · scan read with low confidence · scan quality unknown · completeness not auto-verified · the account number does not look right | **The file's.** Go back to whoever supplied it: one statement per file, a cleaner scan at a higher DPI, a standard page size, or the bank's CSV or Excel export instead of the PDF. |
| several accounts in one statement · more than one currency · page(s) machine-read (OCR) · a minus sign the page draws rather than prints · unusually large file | **A look, not a fix.** The figures were extracted. Read the *How to fix* sentence and give the data the glance it asks for. |
| what the PDF says about itself · no issues found | **Nothing.** Stated for the record. |
| the sign-on-the-page check could not run | **Stop, and tell the maintainer.** The tool could not check minus signs drawn on the page rather than printed. That is a missing part on the server, not your fault and not the file's. |

**"info" does not mean "nothing to do".** Eight kinds of row are *info* severity,
and several of them still ask you for something:

- *the bank needs confirming*: **info** when the evidence is only thin. It asks
  you to check that the bank picked for the file is the one that issued it. The
  same row at **medium** means the statement names a different bank from the
  pick, and nothing is learned until you confirm which.
- *several accounts in one statement*: **info**, and its *How to fix* ends
  "review per account". If transactions from more than one account are mixed,
  the running balance is not continuous across them.
- *page(s) machine-read (OCR)*: **info**, and it says "spot-check machine-read
  values against the image".
- *more than one currency*: **info**, and it asks you to confirm how the
  foreign-currency lines are handled downstream.
- *what the PDF says about itself*: **info**, and it says "No action - this is
  recorded, not a problem". If where the document came from matters to the case,
  compare those details against the copy the bank issued.
- *a minus sign the page draws rather than prints*: **info**, and it says
  "Nothing to fix". The sign was read from the page itself. This row exists so
  that a minus that is hard to see by eye does not make a correct negative look
  like a mistake.
- *unusually large file*: **info**, and it says "No action". **Do not split a
  long statement into smaller files.** The opening + transactions = closing
  check only works across the whole statement.
- *no issues found*: **info**, and it is then the whole table.

So read the sentence, not the severity word.

**A clean run still has rows here, and that is normal.** The table records what
the tool noticed, not only faults, so a proven statement routinely carries one or
two info rows (see the eight kinds above). Nothing on this table can quietly change the outcome. The outcome
is decided by the reader's checks, and the diagnostics explain it.

The **Diagnostics** sheet in the downloaded workbook holds these same rows, plus
one extra column the screen leaves off: a short `fix_owner` code (`reading`,
`input`, `review`, `none`, `escalate`) that says in one word what the table
above says in a row. Where it disagrees with the *How to fix* sentence, follow
the sentence.

## The most common causes, in order

1. **A column read as the wrong thing.** Symptom: *the balance does not add up*
   from the first row, or every sign the wrong way round. Fix it on Please check:
   set the column (money out, money in, balance) and Re-read.
2. **A row the reading missed or added.** Symptom: *the balance does not add up
   at row N*, with the rows on either side right. Look at that row on the page in
   Please check. A wrapped description, a summary line, or a smudge on a scan is
   usually the cause. If the columns are right and the arithmetic still fails,
   the statement itself may not add up. Compare it with the bank's figures
   before confirming anything.
3. **No balance and no totals.** Symptom: *Please check* with a reason that
   says nothing could prove the reading. Nothing on such a statement can prove
   it, so the first ones from a design always ask. Once the bank's layout is
   proven (by statements with a balance, or by an admin), statements of that
   design convert as *Matches a learned layout*.

## Reporting a problem safely (no client data)

**Never send the statement or any personal data**: no names, account or card
numbers, addresses, merchants, descriptions, real amounts or real dates.

Two ways, best first:

1. **Survey the layout with an AI assistant.** Attach the statement to Copilot
   and paste the ready-made prompt in
   [survey-a-statement-with-ai.md](survey-a-statement-with-ai.md). It asks the
   right questions in a fixed order and is written to keep every client detail
   out of the answer.
2. **Ask the maintainer for the safe summary.** **Admin -> Health**, pick the
   upload, *Download its safe summary (no personal data)*. Every value is masked
   to its shape.

If you write the description yourself, turn every real value into its shape:
letters to `x`/`X` by case and digits to `9`, so `Countdown 47.20 on 17 Sep`
becomes `Xxxxxxxxx 99.99 on 99 Xxx`. What is actually useful:

- The columns left to right and what each holds, and whether there are two dates
  per row.
- The date format as a shape (`99/99/9999`, `99 Xxx`), and whether the year is on
  each row or only in the period line.
- How direction is shown: a minus sign, separate in and out columns, a `DR`/`CR`
  suffix, or a `D`/`C` column.
- Anything unusual: several accounts in one file, summary rows inside the amount
  column, running-balance resets, foreign-currency lines.
