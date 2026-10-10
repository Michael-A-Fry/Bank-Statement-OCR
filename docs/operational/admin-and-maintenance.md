# Admin and maintenance

Admin is where you see what the tool has learned and how it is doing, teach it
new wordings, and tidy up. Everything is point-and-click, and there is no
database.

Day-to-day upkeep is on this page. Running the test suite and re-applying an
engine parameter are in [maintaining-the-engine.md](maintaining-the-engine.md).

## Getting in

Add **`?admin`** to the address:

```
http://your-server:8100/?admin
```

Bookmark it. Without it the tab is not offered. Nobody converting a statement
has the password, and a tab they cannot open only invites them to try. Hiding
the tab is not the lock; the password is, and every admin action is re-checked
on the server whatever the browser was shown.

The password is `app.admin_password` in `config\config.yaml`, or the
`BSO_ADMIN_PASSWORD` environment variable, which wins. **Until it is changed
from the shipped placeholder, Admin refuses to open for anybody.** See
[first-time-setup.md](first-time-setup.md) §4. Wrong passwords back off: the
first three tries are free, then each wait doubles, up to five minutes.

## The four tabs

An admin has four questions, so there are four tabs:

| Tab | The question |
|---|---|
| **Needs attention** | What is waiting for me? Each thing with one button. |
| **Recipes** | How is each design of statement read, and is it right? |
| **Words** | Which words does it look for? |
| **Health** | What is failing, and how is automatic reading doing? Also the uploads, the queues, training and the housekeeping. |

No screen shows a recipe file (YAML). Every change is made in plain words, and
every change writes a NEW version: nothing is edited in place, so a conversion
already issued can always be traced to what read it, and every undo is one more
version. The statement's own arithmetic still gates every reading, so no change
here can make a wrong conversion look right.

### Needs attention: what is waiting for you

Cards, each with its count:

- **Statements waiting for a look**: set aside on Please check. **Fix** opens
  the statement on Convert, with Please check.
- **New recipes waiting for you**: drafts saved from a person's answers or from
  *New recipe from a statement*. **Accept** makes one proven (statements like
  it are read on their own, and must still add up); **Retire** takes it out of
  use and off the list. A draft is also proven by itself after three checked
  statements from two accounts.
- **Recipes that stopped adding up**: recognised recently but their reading did
  not add up; the bank may have changed the design. **Fix** opens the recipe.
- **Recipes that look like one**: two recipes of the same bank reading the same
  table. **Merge** keeps the one with more checked statements, with both
  recipes' words, and turns the other off.

**Fixes waiting for an admin**, below the cards: A fix on Please check that the statement's
  arithmetic could not prove, or a reading a person confirmed as right. Each
  applies to the one file it was made on. **Accept** one to make it a proven
  layout of its bank, or **Discard** it to turn it down. Accept only what you
  have checked against the statement: an accepted fix converts the next
  statement of that design without anyone looking.

### Recipes: how each design is read

One row per recipe: bank, name, an **ON/OFF** switch (off: statements like it
get the questions instead), statements read, how many needed help in the last
30 days, and draft or proven. Click a recipe for its card:

- its sample page, with the columns drawn and numbered, once you **Test** a
  statement of the design with it (the answer is one sentence: it adds up, or
  why not). Test uses any change on the card before it is saved;
- the plain questions Please check asks, with this recipe's answers: what each
  column is, how a date is printed, how money is shown;
- the words that recognise the design, as chips: remove one with its cross,
  add one in the box;
- **Save** (a new version), **Undo the last change**, **Merge with...**, and
  **Turn off** / **Turn on**; and its versions, newest first.

**New recipe from a statement** reads one statement, fills in the answers from
what the tool found, lets you Test them, and saves a draft. Please check offers
the same, *Save as a recipe for ...?*, when a person's answers made a statement
add up.

Where they are kept: the shipped recipes in `recipes\` (replaced by an update,
never written by the app), and every change and draft in the server's own
recipes folder (`paths.recipes`, else `templates\recipes\`). **Back it up.**

### Learned layouts and training (on Health)

The tool also learns a bank's **layouts** from statements whose own arithmetic
proved them. A learned layout that read a statement wrongly is taken out of use
on Health (*Take a learned layout out of use*); its files are kept and
conversions already issued are unchanged.

- **Train a bank.** Add every statement you have for a bank (up to 200 at a
  time), press **Train**, and read the report: *"ANZ: 7 layouts from 212
  statements, 205 proven, 7 need a look"*, with each one's reason. It runs in
  the background, feeds nothing to Qlik and records no uploads. The full
  procedure is in [adding-a-bank-template.md](adding-a-bank-template.md).

Where it is kept: `templates\layouts\<bank>\<layout>@v<version>.yaml`, and held
fixes in `templates\layouts\.pending\`. **Back it up**
([backup-and-restore.md](backup-and-restore.md)). It is learned from your own
statements and exists nowhere else.

### Health: automatic reading, how it is doing

Read from `logs\tracking\`, which holds codes and counts only. No names,
descriptions, amounts, dates, account numbers or file names are ever written
there. Press **Refresh** to re-read it.

- **The summary tiles**: statements read, the share read automatically
  (proven, or matched a learned layout), Please check, and Couldn't read,
  against the **95% target for each kind of file with nothing automatic and
  wrong**.
- **By kind of file**: PDF, scan, CSV and Excel each measured on its own,
  because the target applies to each.
- **Checks that failed**: which of the reader's checks stopped a statement
  being proven, most often first. A sudden new top row is worth a look.
- **What each reading was checked against, and what was learned**: the running
  balance, the opening, closing and printed totals, a proven layout, or a
  person on Please check; and new layouts started, evidence added, layouts
  proven, corrected and confirmed.
- **Spot checks**: the answers people gave (right, wrong, can't tell), and the
  **spot-check rate**. It is off (0) by default. When you set a percentage, that
  share of automatic conversions asks the person who ran it to compare a few
  figures with the statement. Which statements are picked depends on the file
  itself, so the same statement is always picked or never picked. A statement
  converted on a layout with no balance of its own is picked at twice the rate.
  Saving the rate writes `auto_reading: spot_check_rate` into
  `config\config.yaml` and leaves your other settings alone.
- **The summary download** holds counts only, so it is safe to carry off the
  box for the product owner.

**How many is enough.** No wrong answers in *n* spot checks only shows that the
error rate is below about 3 in *n*. About **300** clean spot checks are needed to
say "under 1% wrong", and about **500** statements to say "at least 95%
automatic" with confidence (spec section 9).

### Words: what it looks for

Type the word or wording **as the statement prints it** and say what it means.
Each meaning on the list carries an example:

| What it means | e.g. | Kept in |
|---|---|---|
| Opening balance / Closing balance | "Balance brought forward" / "Balance carried forward" | `labels.yaml` |
| Statement period (the words in front of its two dates) | "Period covered" | `labels.yaml` |
| The statement's first day / last day / date issued | "Opening date" / "Closing date" / "Date of issue" | `labels.yaml` |
| A word marking a row as money out / money in | "DR" / "CR" | `lexicon.yaml` |
| The heading of a money-out / money-in column | "Withdrawals" / "Deposits" | `lexicon.yaml` |
| A line in the table that is not a transaction | "Page total" | `lexicon.yaml` |

The answer decides which file it is written to; nobody has to know. A wording
that would clash with another meaning is refused, and the screen says why:
"balance" alone would also catch the "Closing balance" line, and "DR" already
means money out. Nothing is ever added automatically, and a backup of the file
is kept each time.

**From the statement itself.** An admin signed in on the same screen sees
**Teach it a wording from this statement** on Please check. It offers the
wordings that statement prints in front of a figure or a date which the tool
does not read yet, or takes one typed in. Teach it, and the statement is read
again with it straight away.

- **Words your statements used that the tool didn't recognise**: the suggestion
  queue, harvested from every conversion. Clicking one puts it in the box above.
- **Columns in your statements that nothing reads**: a column that keeps turning
  up unread. Expect some noise from files that were never statements.
- **Edit the whole vocabulary file or the whole dictionary file**: for a pattern,
  a page rule, or a value that is not listed yet. Save refuses anything that is
  not laid out properly.

### Health: what is failing

A live picture from the run and feedback logs. Press **Refresh from logs** first.

- **Conversions by status** and **Layouts that started failing recently**: a
  learned layout whose statements are suddenly going to a person more often.
  That usually means the bank changed its statement slightly, by moving a column
  or renaming a heading. An empty table is good.
- **Statements nothing could be read from**: one row per layout, biggest count
  first. **Files that could not be opened at all**: damaged, password-protected,
  or not a statement.
- **Learned layouts in use**, and **What the team said about these
  conversions**: every rating, newest first, with the layout that read it.
- **Uploads**: every document converted here, newest first. Pick one to
  **download its safe summary** (shapes only, no personal data), or **Read it
  again on Convert**.
- **Format requests**, **Folder intake** and **Analytics feed**. A feed write
  that failed is a server fault, so it is announced here.
- **Check a pile of files at once**: drop in a pile and get one picture of what
  the reader proves on its own, and of the statements it cannot read, grouped by
  layout, biggest first. **It audits; it does not convert, save or learn.** To
  teach the tool, use Health -> Train a bank.
- **Housekeeping**: **Tidy up logs** archives run and feedback records older
  than the retention window into `logs\archive\` (nothing is deleted). **Saved
  statements - retention** deletes copies past `retention.uploads_keep_days`,
  **immediately and with no undo**. **Data capture** sets what the server
  records about its own conversions
  ([../context/metadata-capture.md](../context/metadata-capture.md)).

## Where each kind of change goes

| To change | Where | Edited in |
|---|---|---|
| What the tool knows about a bank's layouts | `templates\layouts\` | **Admin -> Recipes** (on/off, change, merge, undo) and **Admin -> Needs attention** (accept or discard a fix, accept a draft); a layout is retired and a bank trained on **Admin -> Health**. Never by hand. |
| What a fact about the statement is called ("Balance brought forward" for the opening balance) | `dictionaries\labels.yaml` | **Admin -> Words**, or Please check |
| What words inside the transaction table mean: DR / CR marks, money column headings, lines that are not transactions; and the money and date shapes | `dictionaries\lexicon.yaml` | **Admin -> Words**, or Please check |
| The NZ banks themselves: names, legal names, websites, branch register | `dictionaries\nz_banks.yaml`, `dictionaries\nz_bank_branches.csv` | Shipped with the tool. The register is refreshed by a maintainer, never in the app. |
| Built-in defaults | `R/lexicon.R` | maintainer only |

`config\config.yaml` holds deployment switches (port, password, paths, the
spot-check rate), never vocabulary.

Both dictionaries are **your** state, not shipped files. An update never
overwrites them ([updating.md](updating.md)). The bank list beside them is the
opposite: it ships with the tool, and an update replaces it. Each save first writes the
previous contents beside the file as `….yaml.bak`, and both are on the short
list to copy off the box ([backup-and-restore.md](backup-and-restore.md)).

## Feedback closes the loop

Anyone can rate a result (correct, minor issues or wrong) with a comment.
Ratings show up under **What the team said about these conversions** on Health.
Marking a result **wrong** also withdraws its rows from the dashboards
immediately.

## Routine upkeep

- **Is the server fit to convert?** `scripts\health-check.R`, one command, after
  every update. The exact command line is in
  [maintaining-the-engine.md](maintaining-the-engine.md) §1. Its **Layouts**
  line counts each bank's learned layouts and fails on any layout file that
  can no longer be read.
- **Look at Automatic reading weekly.** Watch the share read automatically for
  each kind of file, the checks that failed, and the spot-check answers. A
  single *wrong* spot-check answer is a finding.
- **Clear Fixes waiting for an admin.** Accept only what you have checked.
- **Watch Layouts that started failing recently.** It usually means a bank
  changed its print. Convert one of its statements and look at Please check.
- **Train a bank** whenever a new pile of its statements arrives.
- **Tidy up logs** every so often.
- **Back up what cannot be rebuilt**: `templates\layouts\`, `dictionaries\`,
  `logs\metadata\`, `logs\tracking\`.
- **Run the test suite after any engine change and after every update**:
  [maintaining-the-engine.md](maintaining-the-engine.md).
