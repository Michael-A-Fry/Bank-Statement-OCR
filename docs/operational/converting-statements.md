# Converting a statement

The everyday job: turn a statement into clean, checked data you can download.

Since 2.0.0 there are no templates to choose. You check the **bank**. The tool
reads each statement from what is printed on it and **proves** the reading with
the statement's own arithmetic. A statement it cannot prove is shown to you, with
the reason, on **Please check**.

## Do it

1. Open `http://<server-name>:8100` and go to **Convert**.
2. **Browse** and pick the file: `.csv`, `.tsv`, `.tdv`, `.xlsx`, `.xls` or `.pdf`, up
   to **200 MB**. You can pick up to **50** at once, from as many banks as you
   like, to do a whole case folder in one go.
3. **Check the bank.** A table appears with one row per file. Each row shows the
   file's name, its type (*PDF*, *Scanned PDF*, *CSV*, *Tab-delimited*, *Excel*,
   with the page count) and its **bank**, already filled in from the statement
   itself. The tool looks first for the account holder's own account number in
   the official bank branch register, then for the bank's legal name, website,
   0800 number and brand words. The note under the dropdown says how sure it is:

   | Note | What it means | Do |
   |---|---|---|
   | **From the statement** | The statement says clearly which bank issued it. | Nothing. |
   | **From the scan** | The same, for a scan, once its first pages have been read. | Nothing. |
   | **Please check the bank** | The evidence is thin or points two ways. | Make sure the bank is right. |
   | **Please choose the bank** | The statement does not say. | Choose it. A statement that proves itself still converts without a bank, but nothing is learned from it. |
   | **Reading the scan...** | A scan's first two pages are being read as pictures, a few seconds each. You do not have to wait: Convert stops this, and the bank is worked out during the conversion. | Nothing. |
   | **Scanned - can't be read here** | This server has no scan-reading software. | Ask whoever looks after the tool, or get a text PDF or an export from the bank. |
   | **Can't be read** | The file is damaged, empty or not a table. Hover over it to see why. | Get a fresh copy. |
   | **your choice** | You changed this row. | - |

   The dropdown lists the NZ banks and every bank the tool has learned.
   **Another bank - type its name...** is for a bank that is not listed. It
   refuses anything that looks like an account number, because the bank name is
   kept in logs and folder names.

   **Change the bank only if it is wrong.** A bank you leave alone is worked out
   again from the whole statement while it converts.
4. **Type your QID**: six letters or numbers, your staff ID, for example `AB1234`.
   The audit trail records it as the person who ran this conversion, so
   **Convert does nothing until it is filled in**. You are asked once per
   session. After that the page says *Recording as AB1234*, with a **change**
   link for when somebody else takes the keyboard. If there is no QID box at
   all, the server already knows who you are.
5. Click **Convert**. A scanned PDF takes a few seconds a page.
6. Read the outcome, then **Download** Excel, CSV or JSON.

## The four outcomes

Once a file has converted, its row shows the **layout** that read it and the
**outcome**. A layout is the tool's name for one bank design it has learned,
for example *ANZ layout 1: Date | Description | Money out | Money in | Balance*.
It is marked *(new)* when this statement started it.

| Outcome | What it means | Do |
|---|---|---|
| **Proven** | Every running-balance step adds up to the cent, or opening + every transaction = closing and the printed totals agree. No other reading of the columns adds up. | Download it. |
| **Matches a learned layout** | The statement prints no running balance, so its own arithmetic cannot prove it. It matches a layout of this bank that other statements have already proved, and nothing on it contradicts the reading. | Download it. Spot checks pick these twice as often. |
| **Please check: *reason*** | You have the data, but the reading is not proven. The reason says why, usually naming a row and a page: *The balance does not add up at row 4 (page 1)*. | Click **Please check ->** in the row (see below). |
| **Couldn't read: *reason*** | Nothing usable was read, or the file could not be opened. | The reason says why. |

A conversion that a person confirmed reads **Confirmed on Please check**, or
**Proven with the columns you drew**. Its card is amber, not green, because a
person vouched for it rather than the arithmetic.

You should never see `needs_review`, `unsupported`, `layout_match` or a check's
internal name on screen. The engine's codes stay in the logs. If a raw code does
appear, that is a bug, so report it.

### Which bank? A pick the statement disagrees with

If you picked one bank and the statement clearly names another, the row says
**Which bank? The statement looks like Westpac**. The result shows a note with
two buttons, **It is ASB** and **It is Westpac**. The figures are not affected:
they come from the statement's arithmetic, not from the bank. **Nothing is
learned from that statement until you answer**, so one wrong pick cannot teach
one bank's layout to another.

## Please check

Please check opens by itself under the result of any statement that did not
prove. On a proven statement it is one quiet link, **See how it was read**.

- **A PDF:** the page, with each column it found drawn over it and labelled
  (*Date*, *Description*, *Money out*, *Money in*, *Balance*). The page chooser
  doubles as a strip of ticks: **✓** the balance adds up on that page, **✗** it
  does not, **-** nothing to check.
- **A CSV or Excel file:** a table of the file's headings and what each was read
  as.
- **What each column of figures is:** one dropdown per column. The choices are
  *Money out*, *Money in*, *Amount (+ in, - out)*, *Balance* and *Not money in
  or out*.
- **The checks that did not hold**, each in a sentence with the row and page.

Then one of three things:

1. **The reading is wrong. Fix it.** Under **What is each column?**, each column
   of figures is numbered on the page and shown by two of its own lines, with
   the question in plain words: money going **out** of the account (things
   bought, bills paid, cash taken out), money coming **in** (pay, deposits,
   refunds, payments onto a card), both in one column, or what is left after
   each line (the balance). The tool's guess is ticked. Change any that is
   wrong and press **Read it again**. It says at once whether the reading now proves.
   If it does, it converts, and the fix is **learned** for that bank, so the
   next statement like this one is read right without you.
2. **The reading is right. Press This is right.** The conversion becomes yours,
   *Confirmed on Please check*, and goes to the dashboards on your word. It is
   **refused** when the statement's own arithmetic contradicts the reading: a
   balance step that does not add up, an opening and closing that do not agree,
   or printed totals that disagree. A confirm only teaches the tool after an
   admin accepts it.
3. **Nothing fits. Draw the columns yourself.** This is the last resort, for
   this file only. The editor starts from the columns that were found, page by
   page. **Set** or **Remove** a column, or use one page's columns on every page,
   then **Re-read with these columns**. Columns you draw are never learned.

You changed an answer and it made things worse? **Undo my changes** puts the
reading back as it was. It is offered on any reading made with your fix, even
after you open another file and come back.

On a **bundle** (one file holding several statements), a statement picker shows
which statement you are fixing. A fix goes to that statement only.

**A fix that does not prove** applies to this one file and is held for an admin
(Admin -> Banks). One person's word never teaches the tool on its own.

## Amounts filled in from the balance

When an amount cannot be read (a black box over it, a smudge on a scan) but the
running balances either side say exactly what it must be, the tool fills it in.
The row is **shaded** in your transactions table, and the Flags column says
*worked out from the balance column, not read from the amount*. Such a statement
**always goes to Please check**, because one of its figures was worked out
rather than read. Check each shaded amount against the statement before relying
on it.

## Spot checks

If an admin has turned spot checks on, some proven conversions carry a **Spot
check** card: *Compare a few dates and amounts in the table below with the
statement. Are they right?* Answer **They're right**, **Something is wrong** or
**I can't tell**. Which statements are picked depends on the file itself, so the
same statement is always picked or never picked. The answers are counted on the
Admin page. They are how the unit knows how often the tool is right, beyond what
arithmetic can prove.

## Under the result

Everything the tool proved stays on the page under the verdict: the summary
cards, the strip of ticks, your transactions, and one disclosure headed **Checks
& detail (for review)**, which holds the **Checks**, the **Diagnostics** and the
**Field coverage**. That disclosure opens itself whenever a check has failed or
the run needs a person, so on the runs that need reading it is already open.
**Show the charts** opens the money-in, money-out and balance charts. Once you
open them, they stay open for the rest of your session.

### The strip of ticks under the figures

Between the summary cards and your transactions is a row of coloured pills, the
checks a forensic reviewer would ask about, with a key under them:

> ✓ = checked and passed · ✗ = a problem · – = could not be checked (why, in
> Checks below)

A grey dash is not a pass and not a fault. It is a check that had nothing to run
against, and the Checks table says which.

### Reading the Checks table

Five columns: **Check**, **Result**, **Expected**, **Read** and **Detail**.
**The Detail column carries the sentence, so read it, not just the result word.**
The Result column says one of four things:

- **OK**: the check ran and passed.
- **Problem**: the check ran and failed. The detail names the rows.
- **could not be checked**: the check did not run. The detail says why, for
  example no opening balance printed. This is an absence of proof, not an error.
- **for information**: a count, not a verdict. *Scan / OCR read quality* is
  one.

These are the reconciliation checks the workbook's `Checks` sheet holds. The
reader's own checks, which decide whether a reading is proven, are the ones
listed on Please check.

## What you download

| | Contains |
|---|---|
| **Excel `.xlsx`** | Six sheets: `Transactions`, `Summary` (the statement header), `Checks`, `Provenance` (which source line or page position each row came from), `Diagnostics`, `Metadata`. |
| **CSV** | The `Transactions` table only. |
| **JSON** | Everything, including the build stamp: the engine version, the learned layouts' state and the layout it matched. Any conversion can be re-run and gives the same answer. |

## Rating the conversion

*Was this conversion correct?* sits under the result. Three choices, **Correct**,
**Minor issues** and **Wrong**, and **nothing is pre-selected**. Marking a result
**Wrong** immediately pulls back the figures that run sent to the dashboards, and
the screen says how many rows that was.

## A whole case folder

Select several files and Convert. **While it runs, the table is the progress**:
a bar, *Converting 3 of 12*, and each row going from *Waiting* to
*Converting...* to its outcome as soon as that file is done. **Stop** ends it.
On a first run the table goes back to how it was, and nothing from the run is
kept.

When it finishes, a summary line sits above the table, with **Download
everything**: one zip with every file's Excel, CSV and JSON. The rows re-order
worst first, so the files that need a person are at the top. **Click a row** to
open that file's full result underneath. **Please check ->** in a row opens the
file and scrolls to the check.

**Converting again.** Change a row's bank and the button becomes *Convert 1
changed file*. It converts only that one, and the rest keep their results.
With nothing changed, it reads *Convert all N again*.

## Good habits

- Prefer a bank's **CSV or Excel export** over its PDF when one exists. It is
  exact.
- When a statement goes to Please check, read the reason first. It names the
  row, and the row is usually the answer.
- Nobody sees anyone else's upload or result. The tool reads only what is
  visible on the statement and never makes up a figure. A figure worked out
  from the balance is always marked as worked out.

Something looks wrong? See [when-something-goes-wrong.md](when-something-goes-wrong.md).
