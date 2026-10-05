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

## The five tabs

An admin has five questions, so there are five tabs:

| Tab | The question |
|---|---|
| **Banks** | What has the tool learned, is it right, and how do I teach it more? |
| **Review** | What did not work, and what does each layout and held fix look like on a real page? |
| **Automatic reading** | How is it doing? Counts only, plus the spot checks. |
| **Words** | Which words does it look for? |
| **Health** | What is failing? Also the uploads, the queues and the housekeeping. |

### Banks: what the tool has learned

The tool learns each bank's **layouts**, one per statement design, from
statements whose own arithmetic proved them. The rules, in short:

- **Only a proven reading teaches.** A new design starts **provisional**. It
  becomes **proven** after three statements of it have proved it, or when you
  confirm it here.
- **A person's fix that then proves is learned at once.** A fix that does not
  prove, or a *This is right*, applies to that one file and waits for you (see
  below).
- **Nothing is learned while the bank is in question**, that is, while a
  statement names a different bank from the one picked and nobody has said
  which is right.
- **Nothing is edited in place.** Every change writes a new version of the
  layout file, so a conversion already issued can always be traced to what was
  learned then, and every undo is one more version.

The tab has three parts:

- **The bank overview, then the chosen bank's layouts.** For each layout: its
  name, status (*provisional*, *proven*, *retired*), how many statements proved
  it, when it was created, how it was learned (*auto*, *confirmed* by an admin,
  *corrected* by a person's fix) and its version. Select one and:
  - **Confirm**: it is proven from now on, and statements that match it convert
    on their own.
  - **Rename**: give it the name the team uses ("Everyday account").
  - **Retire**: it is no longer used to read statements. Its files are kept,
    conversions already issued are unchanged, and confirming it brings it back.
- **Fixes waiting for an admin.** A fix on Please check that the statement's
  arithmetic could not prove, or a reading a person confirmed as right. Each
  applies to the one file it was made on. **Accept** one to make it a proven
  layout of its bank, or **Discard** it to turn it down. Accept only what you
  have checked against the statement: an accepted fix converts the next
  statement of that design without anyone looking.
- **Train a bank.** Add every statement you have for a bank (up to 200 at a
  time), press **Train**, and read the report: *"ANZ: 7 layouts from 212
  statements, 205 proven, 7 need a look"*, with each one's reason. It runs in
  the background, feeds nothing to Qlik and records no uploads. The full
  procedure is in [adding-a-bank-template.md](adding-a-bank-template.md).

Where it is kept: `templates\layouts\<bank>\<layout>@v<version>.yaml`, and held
fixes in `templates\layouts\.pending\`. **Back it up**
([backup-and-restore.md](backup-and-restore.md)). It is learned from your own
statements and exists nowhere else.

### Review: see it, then decide

Three lists. Click a row and its statement page is shown beside it, with the
columns drawn the way Please check draws them, under the buttons that decide it.

- **Needs a look**: every file whose newest conversion did not prove itself
  (Please check, nothing read, or the file failed), newest first, with its name,
  bank, date and the reason. **Open it on Please check** reads it again on
  Convert, where it can be set right.
- **Layouts**: every learned layout, shown on the newest kept statement it read,
  with **Confirm**, **Retire** and **Rename** beside it.
- **Held fixes**: each fix waiting for an admin, shown on the file it was held
  from and read the way the person set it, with **Accept** and **Discard**. An
  accepted fix's file becomes the new layout's example.

Nothing new is stored. A page can only come from a file the server already keeps:
an upload (deleted after `retention: uploads_keep_days`) or an original in the
folder intake's `failed\` or `processed\`. Files used for **Train a bank** are not
kept, so a layout learned only from them shows *No example kept*, and a file that
is gone is said to be gone. The file is read again in its own process, and nothing
is learned or written while it is shown.

### Automatic reading: how it is doing

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

The label dictionary and the recognition vocabulary in one place, as before.
Type the word or wording **as the statement prints it** and say what it means
(an opening balance, a money-out marker...). The answer decides which file it is
written to. Nothing is ever added automatically, and a backup of the file is
kept each time.

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
  teach the tool, use Banks -> Train.
- **Housekeeping**: **Tidy up logs** archives run and feedback records older
  than the retention window into `logs\archive\` (nothing is deleted). **Saved
  statements - retention** deletes copies past `retention.uploads_keep_days`,
  **immediately and with no undo**. **Data capture** sets what the server
  records about its own conversions
  ([../context/metadata-capture.md](../context/metadata-capture.md)).

## Where each kind of change goes

| To change | Where | Edited in |
|---|---|---|
| What the tool knows about a bank's layouts | `templates\layouts\` | **Admin -> Banks**: confirm, rename, retire, accept or discard a fix, train. Never by hand. |
| The wordings for single labelled values ("opening balance" vs "balance brought forward") | `dictionaries\labels.yaml` | **Admin -> Words** |
| The generic vocabulary: debit/credit markers, money and date shapes, summary-line labels | `dictionaries\lexicon.yaml` | **Admin -> Words** |
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
