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
| **Please check, though it adds up** | A design the tool has not seen is always **asked once**: check it and press *It's right*. A text PDF or a spreadsheet is then written down as a **draft recipe** of that bank: statements like it come back already filled in, and after three checked statements from two accounts they convert on their own. (Since 3.0 a text PDF no longer teaches a *layout*; layouts a server learned before are still read, so nothing is lost.) |
| **Proven** | Done: a recipe that knows the design read it, and the statement's own arithmetic proved every figure. |
| **Please check** | Read, but not proven. Open **Please check**, answer the question for the column that is wrong (money going *out*, money coming *in*, the *balance*...) and press **Read it again**. If it now proves, the fix is learned for that bank straight away. |
| **Couldn't read** | Nothing usable was read. Try **Please check** if it offers columns. Otherwise **Draw the columns yourself** (the last resort, for this file only), or set the file aside and tell whoever looks after the tool. |

**A statement with no running balance and no totals** cannot prove itself
(there is nothing to add up), so the first ones from a new bank come back
**Please check**. Once a statement of the same design **with** a balance has
proved the layout, and the layout is proven, statements without one convert as
**Matches a learned layout** (for a text PDF, its recipe does this job once
it is proven). This is why training (below) helps most for
banks whose exports carry no balance.

You never need a template, a sample file or a test to add a bank. The tool
learns only from statements that proved themselves, and it never learns from a
bank it is unsure of. If the statement names a different bank from the one you
picked, nothing is learned until someone answers *Which bank?*.

## For the admin: train a bank on everything you have

When a bank arrives with a pile of statements, give it all of them at once.

1. **Admin -> Health -> Train a bank.** Pick the bank, or type a new bank's name.
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

### After training: look at the recipes

Each design of statement is read by a **recipe**. **Admin -> Recipes** lists
one row per recipe: bank, name, an ON/OFF switch, how many statements it read
and how many needed help in the last 30 days, and whether it is a draft or
proven. Click a recipe to open its card:

- **Test** reads a statement you add with the recipe (with any change you have
  made, before saving) and says in one sentence whether it adds up. Its page is
  drawn with the columns numbered.
- The card asks the same plain questions as Please check: what each column is,
  how a date is printed, how money is shown, and the words that recognise the
  design (remove one with its cross, add one in the box).
- **Save** writes a new version and keeps the old one; **Undo** brings the last
  one back. **Turn off** stops the recipe being tried (statements like it get
  the questions instead). **Merge with...** makes two recipes of one design one.
- **New recipe from a statement** reads one statement, fills in the answers
  from what the tool found, and saves a draft after you Test it. Please check
  offers the same ("Save as a recipe for ...?") when a person's answers made a
  statement add up.

**Two readings that both add up.** Now and then two recipes of one design read
a statement differently and both readings add up. The arithmetic cannot say
which is right, so Please check shows the two side by side - rows, money in,
money out, closing balance and a few of the rows - and the person presses
**This one is right** under the one that matches the page. That pick is read
again with that recipe alone, and it counts as one checked statement for it.

**Spot checks for cards.** A recipe whose statements are proven only by
*opening balance + rows = closing balance* (no running balance on the rows, as
on most card statements) has every such conversion marked for a spot check at
first, then two in three, then one in three, until three spot checks say
*right*; after that it follows the normal spot-check rate.

**Drafts from the old 1.x templates (D14).** The 13 templates of 1.x (and any
1.x templates a server still holds) can be turned into draft recipes once:

    Rscript tools/recipes/from_templates.R <the server's recipes folder> [1.x templates folder ...]

With no templates folder named it converts the 13 in
`tests/testthat/fixtures/templates`. It writes one `<id>_from_1x@v1.yaml` draft
per template, never over a file already there, and reads no statement. A draft
only fills Please check in: the first statements of the design are still asked
once, and it converts on its own only after its proofs. A PDF template whose
dates print no year is given the period label "Statement period"; if the design
prints another, correct it on the recipe's card.

Drafts wait on **Admin -> Needs attention** to be accepted (or retired). No
screen ever shows a recipe file: every change is a new version, never an edit,
so any past conversion can still be traced to exactly what read it. A learned
layout that read a statement wrongly is retired on **Admin -> Health**.

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
