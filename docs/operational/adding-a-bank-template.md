# A bank or a layout the tool has not seen

Since 2.0.0 there are **no templates to build**. (This page used to explain
how to build one, which is why it has this name.) A statement from a bank or a
layout the tool has never seen is read the same way as every other: from what
is printed on it, and proved with its own arithmetic. So the first thing to do
with a new bank's statement is **just convert it**.

## For everyone: convert it

1. On **Convert**, pick the file. Check the **bank** in its row. For a bank that
   is not in the dropdown, choose **Another bank - type its name...** and type
   its name as people know it ("Heartland", not an account number).
2. Press **Convert**.

What happens next depends on the statement, not on the tool having seen the
bank before:

| Outcome | What it means for the new bank |
|---|---|
| **Proven** | Done. The tool has also **learned** this design as a new layout of that bank, marked *(new)* in the row. It is *provisional* until three statements of that design have proved it, or until an admin confirms it. |
| **Please check** | Read, but not proven. Open **Please check**, answer the question for the column that is wrong (money going *out*, money coming *in*, the *balance*...) and press **Read it again**. If it now proves, the fix is learned for that bank straight away. |
| **Couldn't read** | Nothing usable was read. Try **Please check** if it offers columns. Otherwise **Draw the columns yourself** (the last resort, for this file only), or set the file aside and tell whoever looks after the tool. |

**A statement with no running balance and no totals** cannot prove itself
(there is nothing to add up), so the first ones from a new bank come back
**Please check**. Once a statement of the same design **with** a balance has
proved the layout, and the layout is proven, statements without one convert as
**Matches a learned layout**. This is why training (below) helps most for
banks whose exports carry no balance.

You never need a template, a sample file or a test to add a bank. The tool
learns only from statements that proved themselves, and it never learns from a
bank it is unsure of. If the statement names a different bank from the one you
picked, nothing is learned until someone answers *Which bank?*.

## For the admin: train a bank on everything you have

When a bank arrives with a pile of statements, give it all of them at once.

1. **Admin -> Banks -> Train a bank.** Pick the bank, or type a new bank's name.
2. Add **every statement you have for it**, up to 200 at a time: PDFs, scans,
   CSV and Excel together. More can be added any time.
3. Press **Train**. They are read in the background, as a case conversion would
   read them, and nobody's page freezes. The progress says *Reading 37 of 120*.
4. Read the report:

   > **ANZ: 7 layouts from 212 statements, 205 proven, 7 need a look.**

   Each learned layout is listed by name, and so is each statement that needs a
   look, with its reason. A statement that looks like another bank's is listed
   with *It looks like a Westpac statement, so nothing was learned from it*.

Training **feeds nothing to Qlik and records no uploads**. It only teaches. A
statement that needs a look teaches nothing. Convert it on the Convert tab and
use Please check to set it right, and a fix that then proves is learned.

### After training: look at the layouts

**Admin -> Banks** lists every bank, then the chosen bank's layouts: name,
status, how many statements proved it, when it was created, how it was learned
(*auto*, *confirmed*, *corrected*) and its version.

- **Confirm** a provisional layout you have checked by eye. It is proven from
  then on, and statements matching it convert on their own, including ones with
  no balance.
- **Rename** a layout to what the team calls it ("Everyday account").
- **Retire** a layout that is wrong. It is no longer used, its files are kept,
  and conversions already issued are unchanged. Confirming it brings it back.

Every one of these writes a new version of the layout and never edits one, so
any past conversion can still be traced to exactly what had been learned when
it ran.

### Fixes waiting for an admin

Two kinds of a person's word are held under **Fixes waiting for an admin**
rather than learned: a fix on Please check that the statement's arithmetic could
not prove, and a reading somebody confirmed as right with **This is right**.
Each applies to the one file it was made on.

- **Accept** one to make it a proven layout of its bank.
- **Discard** it to turn it down.

## Describing a hard statement to someone, without client data

If a statement will not read and you need help, describe its layout with no
client information in it:
[survey-a-statement-with-ai.md](survey-a-statement-with-ai.md). The maintainer
can also download a shapes-only summary of any upload from **Admin -> Health**.
