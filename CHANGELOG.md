# Changelog

The version record. Newest release first.

`VERSION` is stamped into every run log, every JSON output and the feed manifest,
so the heading below has to match it: that is what makes "which build produced
this figure?" answerable after the fact.

This file is deliberately terse. Each entry says what changed and, where a wrong
figure was possible, what stopped it. The evidence for every line is in
[`docs/context/findings-register.md`](docs/context/findings-register.md), keyed by
finding id.

---

## 1.7.1

A UI pass over the whole builder, walked screen by screen, plus four things
reported from using it.

**A row broken mid-word came out with a space in it.** An email the PDF wrapped
read `xys@ gmail.com`. A space is right nearly always — a wrapped description is
two words — and wrong exactly when the break falls inside one token. Line breaks
now close without a space when the first fragment ends on a joining character
attached to a word (`@ / \ _ = + -`) or the second opens with one. A trailing full
stop is deliberately *not* on that list: "Acme Ltd." followed by a new sentence is
far commoner than a token broken after a dot. The same rule applies to a value box
drawn round a wrapped address, and the picker reaches down one line to find the
rest of it (N194).

**The preview showed the first table and nothing else.** Under a summary that
faithfully listed all of them — so the screen said "6 tables, 412 rows" and then
showed you 38 of them, with no control anywhere to see the rest. Every table now
has its own block with its name, its row count, and its rows, in the order the
workbook has them (N195).

**Column names stopped colliding at the top of the page.** They were laid out two
rows per *table*, so a second table on the same page started again at row one and
wrote over the first. Placement is a page-level pass now: every label from every
table takes the first row where it clears what is already there, and the strip
grows downwards rather than overlapping (N196).

**Edit re-arms the drag**, on a column and on a value, because pressing Edit on a
specific thing *is* a statement of intent about that thing. The armed-intent model
is unchanged — one thing at a time, written across the top, cancellable — and the
banner now names what you are editing and says it is outlined on the page. On a
value, Edit arms the figure; "Re-draw the LABEL instead" is one press in the
banner (N197).

**Notifications are centred near the top**, not in the bottom-right corner. On a
screen whose work is a document on the left and a panel on the right, the corner
is the one place nobody is looking — so "that box holds one unbroken run of words"
was said, correctly, into empty space.

From the walkthrough itself:

- **The first screen contradicted itself.** The file picker said
  ".csv / .tsv / .tdv / .pdf / .xlsx" and the panel below it said "It has to be a
  PDF" — both true, of different halves. The kind of document is asked *first*
  now, and the picker then says which files that job takes. Uploading a
  spreadsheet for a report used to do nothing at all with no message; it now says
  what happened and offers the statement path.
- **"Read the whole document" is off until there is something to read**, with the
  reason beside it. A green button that answers "nothing to read yet" was the most
  prominent control on the lower half of the screen for the whole of the work.
- The instruction under the picture no longer repeats "nothing is armed" — the
  banner above it already says so, and it is pinned to the top of the window.

---

## 1.7.0

Seven things reported from the screen in one go. Two were crashes or losses, four
were the tool arguing with the person about what they could see, and one was a
question that turned out to be a design mistake.

**"Invalid 'type' (character) of argument" when you read a document with no
tables on it.** A template made entirely of label/value pairs — which is what a
form is — fell over at the last step, after all the work, on the screen that
shows what came out. A zero-row summary was ten `character(0)` columns, and
`sum()` over one of those is an error, not a zero. The empty frame now carries the
same column types as a full one, and the screen forces every count to a number
besides (N187).

**Columns may now have whitespace between them.** Asked directly: *"do columns
have to be joined? can there be white space between two column definitions?"*
They had to, and it was costing something on every screen — adding a column
either widened its neighbour over a column of figures or invented a column in the
gap. A report has real whitespace between its columns, and the tool insisting
otherwise was the tool arguing about what is printed on the page.

- **Overlaps are still impossible.** A gap is visible and counted (*unclaimed
  words*); an overlap silently reads a figure into one column and loses it from
  the other. A move or an insert that would overlap is trimmed at the neighbour
  and the screen says so.
- **Adding a column adds one column, exactly where you drew it.** Nothing else
  moves.
- **Moving a column moves that column.** Neighbours are no longer absorbed.
- **Deleting one leaves its space empty** rather than handing it to the column
  beside it (N188).

**Where it starts and where it stops are now steps, not settings.** Reported:
*"start and end also need to define where the table ends, there's lots of text at
the bottom of some pages which is not the table, but the table still spans
multiple pages."* The same sentence-at-a-time guide that gets the columns now gets
these: columns → starts → ends → **and, when the table runs over pages, the bottom
edge of the pages in between.**

That last one is the fact nobody was ever asked for. *Where it ends* is a place on
one page — the last one. On every page between, the table ran to the bottom of the
paper, so a footnote, a source line or a page footer under it was read in as rows
on every page but the last, and the person who set the end correctly had no way to
see why. Every step is skippable, and the guide stops the moment you steer
yourself (N189).

Building the step turned up a second fault behind it: **the setting was being
dropped on the way to the reader.** `document_template_from_proposal()` did not
carry the table's `band`, so the bottom edge was set on the draft, drawn on the
page, written in the panel — and never reached the engine. A control that changes
nothing is worse than no control: the question gets answered and stays answered
wrongly. Driven end to end in a browser on a three-page table with a footer: **40
rows with the footer in it before, 36 rows and a clean tick after** (N193).

**The values were half the builder and you could not see them.** Two reports, one
gap: *"the read whole doc should have the tables AND label value pair"* and
*"where do the value label pairs come out in the CSV long? I can only see it on
workbook"*. The pairs are now shown in the preview beside the tables, the verdict
counts them, and the values CSV — which was always written — has a download button
on both the builder and Convert. The long CSV is the tables stacked *page, row,
column, value*; a pair is none of those, which is why it needs its own file
(N190).

**A value's name is the key it comes out under**, so it is lower case with
underscores: type *IRD Number*, get `ird_number`. The line under the box says what
it will become while you type; the box is set to it on save. Never while you are
typing — rewriting a box under somebody's caret is the one thing this screen must
not do (N191).

**"Nothing was waiting for that drag" now names the button you needed.** Reported:
*"when I click edit, and drag where I actually want the column, it says nothing
was waiting."* True, and useless — the button was named nowhere in the sentence.
It now names the column and the button (N192).

---

## 1.6.1

**"+ Add a column" was stretching the first column over everything instead of
adding one.** Drag over the second printed column of a table whose first column
is already carved, and the answer was ONE column spanning both — every heading
and every figure of the second column swallowed into the first, and the first no
longer where it had been put (N185).

The drag's two sides were being fed through `doc_edge_click()` one after the
other, and a click **outside** the table means "move the outer edge out to here",
because that is what widening a table by clicking beside it has to do. So the
right-hand edge was moved out twice, exactly as asked, by a function answering a
different question. A drag is not two clicks: `doc_add_column()` now says *this
band is a column of its own* — both sides become edges, dividers strictly inside
the band are absorbed, and **every other edge is kept**, including the outer ones.

Ground between the new column and the one before it therefore becomes a column
too, rather than being swallowed into its neighbour. That is the cautious
choice on purpose: an unwanted column goes away with one click on its divider,
whereas a swallowed region cannot be recovered without redrawing both. The
screen says so when it happens.

**The instruction now sticks to the top of the window.** The one sentence saying
what the next drag will do was written at the top of the *page* — and with an
840px document image and a longer panel beside it, every real drag happens
scrolled down, so the sentence was above the window whenever it mattered. It is
pinned to the top of the screen for as long as the builder is open.

**Uploading swaps the screen.** The upload panel is a heading, a sentence, a file
picker, two radio buttons and a note — all of them answered the moment the
document is on screen, and four inches of it between the reader and the work. It
now folds down to one line naming the document, with a way back that says it also
changes the kind of document. The builder starts above the fold.

---

## 1.6.0

**Every template lives in one folder now.** There were seven at the root of the
app — `templates`, `templates_user`, `templates_seed`, `fields_templates`,
`fields_templates_user`, `doc_templates`, `doc_templates_user` — and telling them
apart meant already knowing the naming convention. They are now one folder with a
`README.md` that is the map:

```
templates\
  statements\  statements_user\  statements_seed\
  fields\      fields_user\
  documents\   documents_user\
```

A folder name is the template's `mode:`, so a template cannot be read as the
wrong kind — that is the same fact twice rather than two facts that can drift.
**The separations are untouched**: curated vs `_user` is still the Qlik
governance gate, and one folder per mode is still what keeps a report template out
of statement detection.

- **An existing install moves itself, once, and says so.** On the first start
  after the update the old folders are moved into place and each move is written
  to `logs\startup.log`. It moves files, never copies-and-deletes and never
  deletes; a destination file that already exists is never overwritten — the
  source is left alone and reported, every run, until a person deals with it; an
  emptied folder keeps a `MOVED.txt` so somebody who goes looking finds a sentence
  rather than nothing. Running it twice does nothing (N184).
- **A settings file naming the old folders is read as naming the new ones.** A
  `config.yaml` wins over the defaults — that is the point of it, and here it
  would have been a trap: the folders move, so a config still saying
  `templates_user` would point at an empty one and every template the team built
  would vanish from the app with nothing said. Only exact legacy names are
  rewritten; a path somebody chose on purpose is left alone.
- **New page: [updating a version](docs/operational/updating-a-version.md)** —
  merging a dev folder into the live one by hand. What must never be copied, what
  to copy deliberately, seven steps, and five checks that each prove a different
  folder came back.
- Templates built in the app can no longer be committed by accident: the three
  `templates\*_user\` folders carry nothing but a `README.md`, which is the entire
  reason a folder-replace update cannot overwrite somebody's template. Enforced by
  `.gitignore` **and** asserted in `test-deployment.R`, because a rule only a
  `.gitignore` knows is a rule nothing checks.
- Test helpers ask `templates_dir()` / `user_templates_dir()` / `fields_templates_dir()`
  / `document_templates_dir()` instead of spelling the path out, so the next move
  touches one file rather than forty.

---

## 1.5.6

**A folder of irreplaceable work that no backup procedure named.**
`doc_templates_user\` — where every report puller somebody builds by hand is
saved — was carried by the bundle, so an update could not destroy it. But it was
in none of the five pages that tell a person what to protect: the backup page
still said "four folders cannot be rebuilt", and the restore, rollback and folder
maps all listed the other two `*_user\` folders and not this one. A server
rebuilt from a documented backup would have come back with every bank template
and every taught word and none of the document templates (N183).

- `backup-and-restore.md` — the fifth folder, in the table, the `robocopy` block,
  the "check it worked" step and the restore list
- a restore check of its own, because **Admin → Templates does not list document
  templates** — they are matched at the front door, so the only way to see one is
  to upload the report and watch it be recognised
- `updating.md`, `rolling-back.md`, `running-and-keeping-it-up.md`, `design.md` —
  the same folder, in every place the others are named
- `test-deployment-docs.R` now fails if the backup page stops naming any of the
  three `*_user\` folders

**And the hand copy nobody had written down.** `updating.md` covered the
supported route — build a package, replace the folder — which is safe because
`bundle-offline.R` renames `dictionaries\*.yaml` to `*.example.yaml` before it
ships. Copying a few files straight out of the **source** folder skips that
rename, and the source folder carries those two files under the live names. The
result is silent: every wording and marker Admin has taught the tool is replaced,
nothing errors, and statements that reconciled last week quietly stop
reconciling. New section — *If you copy individual files by hand* — with that
warning, the other four things a sweep would take with it (`config.yaml`, the
three `*_user\` folders, `logs\`/`uploads\`/`feed\`, the private R), and
`R\params.R`, which must be re-applied by hand rather than dragged. Both facts
the warning depends on are now asserted, not just described.

---

## 1.5.5

**Measured what had only been claimed: is it actually nice to type in?** Driven in
a browser at 45ms a keystroke, across all five boxes of the builder:

- every character survives — nothing is eaten
- no input is a different DOM node afterwards — no box is rebuilt under the caret
- **drawing a box causes zero redraws while the mouse is held down**
- a whole typed phrase costs one or two plot recalculations, not one per letter

One residual found and fixed on the way: the numeric column-edge boxes
re-selected their own column after every commit, and assigning a `reactiveValues`
field invalidates it *even when the value is identical* — so the selection was
pushing values back into the two boxes the person was typing in. Guarded, and the
contract is now pinned by tests rather than by care.

---

## 1.5.4

**The suite is green, and it is the first time it has been.** Five red lines had
been carried for a long time and explained — in the maintainer's guide and by
more than one person — as "the environment, not the code". That was wrong. Four
of the five were **test bugs**; the engine was right throughout.

- Two test files built their input images with `magick::image_draw`, which hands
  back an image still attached to a live graphics device. The same drawing gave
  `detect_dark_regions` 0 regions or 1 depending only on what had run before it.
  Built through a PNG file instead, the answer is the same every time — and
  correct (N179).
- A fixture leaked a graphics device by restoring `par` *after* `dev.off()`,
  which silently opens `Rplots.pdf` in the app folder and leaves it open for the
  rest of the process. That stray file no longer appears after a suite run
  (N180).
- One assertion pinned the R version rather than the code. The rule it protects
  is asserted separately and passes (N181).

A board that is permanently red at five teaches everybody to read past red, and
the next real failure then arrives on a board nobody reads. **`failed: 0`,
`errors: 0` means what it says now.**

---

## 1.5.3

**Putting it on the server is one page now.**
[deploy-on-the-qlik-server.md](docs/operational/deploy-on-the-qlik-server.md) —
the service account and the three rights it needs (each of which fails
differently), claiming port 8100 so Windows cannot hand it to anything else, the
scheduled task that brings it back after every reboot, the firewall rule, and the
address people type. Every step says how to prove it worked before you move on,
and it ends in a checklist.

**A start at boot leaves evidence.** Started by Task Scheduler as a service
account there is no console, so everything the launcher printed went nowhere —
and "the task ran and nothing is listening" had nothing behind it. It writes
`logs\startup.log` now: a line per start with the version, folder, account, port,
address and upload ceiling, plus a line for anything that stopped it. Set up
before anything that can fail, and self-trimming so a flapping service cannot
fill the disk.

**A port already in use is a sentence, not a socket error.** Checked before the
app tries to listen: which port, what probably has it, the command that names the
process, and the two ways out. It exits non-zero, so restart-on-failure behaves.

**The address it prints has the machine's own name in it** instead of the
placeholder `<this-vm>`, which was being printed to somebody who had just
installed the thing and did not yet know what the network called the box.

**Documentation caught up with the last three releases.** `design.md` now
describes the third template mode (`mode: document`) — the schema, why it is a
separate engine, the four load-bearing decisions inside it, and the one line that
keeps it out of the dashboards. A new analyst page,
[pulling-tables-out-of-a-report.md](docs/operational/pulling-tables-out-of-a-report.md),
covers the builder end to end including what it still cannot do. The analyst
folder is five pages, not four.

**Two links named screens that no longer exist.** "Open the PDF form builder" and
"Open the report builder" were merged into one screen months earlier; both now
say "Set it up on Add a template", which is where they land.

---

## 1.5.2

**A rotated page can be pointed at.** `pdf_pagesize` reports the page box before
`/Rotate` is applied; the renderer and the text extractor both report after it.
Where they disagreed the builder drew a picture 612 points wide whose own image
was 792 wide, so the page was stretched one way and squashed the other and
nothing anybody drew on it landed where they put it. The words decide now, and
only when they do not fit the box and do fit it on its side. Measured on 81
third-party documents: 11 pages of 212, across 5 documents, every one of them
previously unusable in the builder. A US Senate expenditure report now reads its
six columns from two drags.

**Every column band is tinted.** It used to tint alternate ones, which reads as
"three of these are selected and three are not" while the list says six.

**One column is said out loud.** A drag round "the column names" that lands on a
sentence makes one column and every row then arrives whole in one cell — and a
one-column table cannot have an unclaimed word or a thin column, so it passed
every check and reported "every column filled". Now said at the drag, repeated
on the card, and the clean tick is withheld.

**"How many rows" renamed "Heading rows."** Under a picture of a table it read
as "how many rows has this table" — a question nobody is ever asked here.

---

## 1.5.1

The document builder rebuilt around one idea: **nothing happens until you ask for
it**, and everything the tool works out for itself is on the screen with a control
beside it to change it.

**One armed intent, named across the top**

- A drag used to mean "make me a table", which is also what it meant when you
  were trying to fix one. Now the screen holds exactly one armed intent at a
  time, written across the top in a sentence — *"Drag a box round the table's
  TITLE"*, *"Click the page where the table ENDS"* — and nothing is armed until a
  button armed it. A drag with nothing armed changes nothing and says so.
- The banner and the hint under the picture read the same sentence out of one
  list, so they cannot disagree about what a gesture will do.

**A value is two drags, and the tool remembers which SIDE**

- The label, then the value. Nothing is guessed. What is stored is the label's
  wording plus which side the value sits on — right, left, above or below — and
  on the next document the label is found by its wording and the search runs in
  that direction. A figure printed two digits longer, or a label a word wider,
  still reads; a fixed offset did not.
- The side is shown in words, editable before and after saving, and written into
  the template file where it can be read and changed by hand.

**Everything auto-derived is editable**

- Columns: rename, retype, reposition (drag the width on the page or type the two
  edges — both give way at the neighbours and keep the tiling) and delete.
- Start and end: stated in words with *Show me* and *Move it* beside each, plus
  one press each for *work it out for me*, *the bottom of its page* and *the
  bottom of the last page*.
- Every saved table and every saved value goes back into the same draft to be
  edited, so **Save** always means the same thing.

**The tool's labels no longer cover the words they describe**

- Column names float in a strip above the paper, joined to their columns by a
  hairline. START and END are short filled chips, not captions.
- Five layers — tables, column lines, names, values, start and end — switch off
  independently, the way the X-ray's do.

**Three engine defects, found by running it over 81 PDFs nobody here wrote**

`tools/corpus/` collects a folder of real documents from the test suites of
camelot, pdfplumber, tabula-java and tabula-py, and `run-corpus.R` surveys what
the engine does with each. No document crashed it. Measured before and after:
**40 documents where the proposed row count was not the count the reader
produced → 0**.

- The proposer counted lines; the reader folds wrapped cells and skips printed
  rules. The proposer now asks the reader (N169).
- A one-column table — a block of prose, a list — was ended by the first
  paragraph gap, because the stop rule's floor of two filled columns is
  unreachable in a table one column wide (N170).
- A landscape page scaled into a portrait frame stretches sideways and squashes
  vertically, so every column band on it claimed the wrong words. Three of the 81
  mixed orientations; one lost 133 words out of every column. Such a page is now
  read in its own space (N171). The bottom of the page is the document's own, not
  A4's (N172).

**Just as easy as a bank statement — measured, in a browser**

Driving both paths end to end: a bank statement took **4 decisions** (say what it
is, choose the file, open the toolkit, Save) and a document took **10**, because
the document builder asked by hand for the three things the statement toolkit
fills in for you. It now fills the same three the same way — the issuer guessed
from the filename, the identifying phrases found on the document (a saved
table's own title, which beats its column names), and the save name composed
from the two — and a document takes **7**. The remaining three are the
irreducible ones: *+ Add a table*, and the two drags that say **which** table.

**Generalisability**

- A document template no longer carries `currency: NZD`. Nothing in this mode
  reads it, and a report of rupees should not carry a false statement made by a
  converter that never looks at it.
- The default column kind stays `auto`: the page's own words, uncoerced. A
  declared kind puts the parsed value in `<column>__value` and leaves the raw
  text in the column, so a figure the parser does not recognise is never lost.

---

## 1.5.0

A third kind of document: a **report** — forty pages carrying thirty-odd tables
of completely different shapes, several to a page, some beginning half-way down
one page and finishing half-way down the page after next.

**Why it is a separate engine and not a wider statement parser**

The statement parser reads ONE table whose columns run the full height of every
page, and it judges what it read against a running balance. All three of those
facts are false for a report. Widening it would have put the least checkable case
inside the code path every real conversion runs through, so a report is read by
`R/tables.R` / `R/tables_detect.R` / `R/doc_extract.R` under its own template mode
(`mode: document`), and the statement path is untouched.

**Download only, enforced rather than assumed**

- `write_feed()` refuses `kind = "tables"` outright, beside the same refusal for
  forms. There is no reconciliation behind a report — nothing here can tell a
  right figure from a wrong one — so nothing from one reaches a dashboard.

**Found by its wording, not by its coordinates**

- A table declares a START `(page, y)` and an END `(page, y)` — positions on a
  page, not whole pages — with columns as x-bands meaningful only between them.
  That is what lets several tables share one page.
- It is located by its **heading wording** first, then by **the rows it starts
  with**, and only last by **where it sat on the example**. Which of the three was
  used is reported with every table, and the last two send a conversion to review:
  reading from a stale position on a page where the table has moved does not fail
  loudly, it quietly picks up a title line as if it were data.

**What it does instead of checking, because it cannot check**

- The **fill rate** of every column and every row is measured and reported. A
  column below the table's threshold is flagged, never dropped; a column marked
  `may_be_blank` — the middle cells of a totals row — is exempt.
- **Every word inside a table's boundary that no column claimed** is listed. A
  band drawn four points too narrow loses a whole column of figures and leaves
  every remaining row looking perfect; that list is the only thing standing
  between that and a wrong answer.
- **Overlapping column bands are refused at save time.** A word can land in only
  one column and the earlier band wins, so an overlap does not duplicate a value —
  it deletes it from the second column, invisibly.

**Truncation, which is invisible when it happens**

- A table drawn on a copy that ran to page 5 is **followed** while the pages after
  its declared end keep repeating its header, and the extension is reported. Every
  row that did come out of a truncated read looks right, which is why this cannot
  be left to be noticed.

**The line pitch is learned, not assumed**

- Per table, from the page's own line spacing. A tolerance too large merges two
  rows into one — the silent corruption the whole project exists to prevent — and
  one too small splits a row whose cells sit on different baselines. Capped at 14
  points so a widely-leaded title block cannot swallow the value printed under its
  label.

**Label/value pairs are two boxes, not one**

- What is stored is the label's wording plus the **offset** to the value, so the
  value can sit to the right of its label, under it, or *before* it without any of
  those being a special case. On the next copy the label is found wherever it has
  moved to and the same offset applied; the drawn box is the fallback, and which
  one was used is reported.

**Setting one up is confirming, not drawing**

- **Add a template → "A report"** reads the document and proposes the tables it
  found, drawn on the page with a list down the right to navigate them. Thirty
  tables drawn by hand is a template that never gets finished, and a half-drawn
  one is worse than none: it produces confident output with a column missing.
- A continuation across pages is joined into one table only on a repeated header
  or identical column bands. Interleaving two different tables is the worse of the
  two mistakes.

**Output**

- A workbook with a sheet per table, plus one **long** CSV (`table`,
  `table_name`, `page`, `row`, `column`, `value`) — the only honest single-file
  shape for tables of thirty different widths — plus the fill report. Where
  `openxlsx` is absent, a CSV per table instead: nothing is dropped for want of a
  package.

**ONE builder, and columns you click rather than draw**

The first version of this asked the wrong question first ("a form, or a
report?") and then made the commonest job the hardest one. Rebuilt around what a
person actually does:

- **One document type.** "A bank or card statement" or "anything else". There is
  no longer a form builder and a report builder: that split was the engine's, and
  a person with a PDF in front of her cannot answer it. Inside, two tabs -- Tables
  and Values -- which are about what she is doing at that moment.
- **Columns are DIVIDERS, not boxes.** A table's columns tile its width, so the
  real choice is the N-1 lines between them. Click between two columns to split,
  click on a divider to remove it, click outside the table to widen it. Six clicks
  instead of seven drags -- and a gap or an overlap becomes impossible to draw,
  which retires the whole class of "a column band a few points wrong" by
  construction.
- **The names come from the document.** Split a column and both halves are named
  from the two header cells that were in it. Nobody types a column name unless
  they want a different one.
- **"Work out the columns"** derives them from whatever is inside the table's
  boundary right now -- so the loop is: set the start, set the end, press it.
- **One control says what a click does** (columns / where it starts / where it
  ends / nothing), and it sits under the picture it controls. A drag always means
  one thing on a given tab. Two gestures, never a follow-up question.
- **A value takes ONE drag.** The tool reads the wording beside the box and calls
  the value that, finding the label's own box on the page so the pair still
  travels by wording-plus-offset. A second box is only needed when the label is
  somewhere the tool cannot see.
- **Typed values survive the merge.** A value described only by its wording,
  with no box at all, is read by the label matcher anywhere on the document -- the
  most portable of the three ways, and now expressible in the same template as a
  drawn table.

**Three defects the rebuild turned up**

- **`sp$label` partial-matched `label_text`.** R's `$` partial-matches on lists,
  so a value described by wording alone returned a character vector where a box
  was expected and took the WHOLE extraction down with "$ operator is invalid for
  atomic vectors". Every typed value is exactly that shape. Spec fields are now
  read by exact name.
- **The Convert page's link to the builder selected a document type that no
  longer existed**, so it arrived showing the statement toolkit instead.
- **`col = "#666"`** in the "this page could not be drawn" fallback:
  `col2rgb("#666")` is an error, so the branch that exists to survive an
  un-renderable page would itself have crashed.

**What sixteen awkward documents found**

The shapes above were built as fixtures (`tests/testthat/helper-doc-hard.R`) and
run against the engine. Nine of them broke it. Every one of these was measured,
not reasoned about:

- **A table whose row heights alternate lost half its rows.** 10pt and 26pt
  rows, a median gap of 26, and every 10pt gap swallowed: six rows came back as
  four with `R2 R3` in one cell and `20.00 30.00` in another. The row tolerance
  now follows the **lower quartile** of the gaps -- the tightest spacing the table
  actually uses -- so it is safe for every row rather than for the average one.
- **A table followed onto the next page swallowed the two tables under it**: 93
  rows instead of 83, ten of them with an amount in the wrong column and a date
  that was not a date. An open-ended window now stops where the rows stop: a gap
  much bigger than the ones this table has been using AND a line that fills fewer
  of its columns. Both halves are needed -- the gap alone cuts a schedule at the
  whitespace before its own total.
- **Two different schedules with the same header became one table** of twelve
  rows under the first one's name. A page that carries its **own heading** is a
  new table; a real continuation opens with the repeated column names and nothing
  above them.
- **A header repeating as "... (continued)" split an 18-row schedule into two.**
  A repeated header now matches on prefix -- while still refusing the same column
  names in a different order, which is a different table sharing a vocabulary.
- **A bordered table proposed no columns at all.** `+----+----+` rules span the
  full width and merge every band into one. Printed rules are now transparent:
  they neither start a run, nor end one, nor contribute a band, nor become a row.
- **A condensed table read as one cell per row.** 2pt gutters with a `|` in them
  -- narrower than any word space. A lone vertical bar is a border, dropped where
  both the proposer and the reader come through, so the columns separate on real
  whitespace and no cell comes out as `| 1,240.55`.
- **A total set apart by an underline and 45pt of white was left out** -- the one
  row somebody came for. A run now reaches down past a break for up to three lines
  that still land in at least two of its own bands.
- **A two-column table was not a table.** A valuation of item and amount, five
  rows, proposed as nothing. Two columns are allowed on a longer run: four
  consecutive rows in the same two bands is a schedule, not a coincidence.
- **A description that wrapped ended the table it was in.** Once the tighter
  tolerance correctly made the wrap its own line, the one-cell line broke the run.
  A wrap -- indented past where the rows start, within a line of the row above,
  filling one column -- is folded into that row instead.

**Still limitations, and asserted as tests so they are decisions**

- Two tables printed side by side read as one of six columns. The ink is
  identical to a genuine six-column table; it is drawn as one on the builder,
  which is where it gets split into two.
- A spanning header cell ("Balance" over "Opening"/"Closing") is offered as the
  table's name. Wrong, harmless, one text box fixes it.
- A table with no figures anywhere keeps its heading line as a row: with no font
  information there is no header signal, and the rule fails toward keeping a row
  rather than deleting one.

**Paths**

- `doc_templates/` (curated) and `doc_templates_user/` (built in the app), settable
  as `paths.docs` / `paths.user_docs`. A settings file written before this release
  still starts.

---

## 1.4.0

Two rounds in one release: the one that let a squad use the tool at the same
time, and the one that read the result back and found what it had cost.

**Why the version moved at all**

- `N120` — two run records existed for one statement carrying the **same**
  `engine_version`, the **same** `template_sha256` and **opposite verdicts**
  (`needs_review` / low, then `ok` / medium). Behaviour had changed inside a
  release while the stamp stood still. The incident procedure's headline test is
  *same version + same template hash, so any difference is a finding* — so it had
  begun crying wolf on exactly the statements the previous round had fixed. From
  1.4.0 the stamp moves with every shipped change of behaviour, and
  [`docs/operational/investigating-a-wrong-conversion.md`](docs/operational/investigating-a-wrong-conversion.md)
  §4 now carries the caveat for every record written before it, with the real
  pair on this box as the worked example.

**Five to ten analysts can convert at once**

- Every conversion runs in its **own short-lived R process** (`R/jobs.R`), and the
  app polls for the result. Before: R is single-threaded, so one scanned statement
  froze a second analyst's browser for **65 seconds** with 13 consecutive probes
  unanswered. After: the same scan, the same two browsers, and the second page
  answers in **0.06–0.30s** throughout while a colleague's CSV comes back. No new
  dependency — `run.R` was already a tested command-line entry point, and loading
  the whole engine costs 0.27s, so a worker pool would have bought ~7% on the
  cheapest conversion and cost a dependency.
- **A cap and a queue that says so.** Ten OCR jobs on a four-core box helps
  nobody, so the cap is one under the machine's cores (three here), settable per
  site with `app.max_concurrent_jobs`. It caps **threads** as well as processes,
  because `tesseract` is OpenMP-parallel and takes a thread per core by default:
  one scan alone reads in 36 seconds, and three uncapped had not finished a single
  page after ten minutes. Anyone past the cap is told where they are — *"3
  conversions ahead of yours - yours starts as soon as one finishes"* — and the
  number falls as the queue drains. Waiting is fine; waiting without being told is
  what this removes.

**What running in a child process broke, found by driving it**

- `N134` — **a wrong figure's close cousin.** The child inherited the *service's*
  locale rather than the app's, so a description with an accented character came
  back mangled **in the delivered CSV** (`Caf<U+00E9> Kr<U+00F6>ne` where an
  in-process run rendered `Cafe Krone` correctly); byte comparison put the first
  difference at position 186. It would only ever have shown up on a real
  customer's statement.
- Reaping a job killed the R child and left **`tesseract` orphaned** — three of
  them, a core each, still running 14 minutes after the tabs closed. Jobs now kill
  the whole process tree.
- `N131` — a killed conversion reported the child's **locale string** as its
  reason: `convert job job00023 (stopped): [1] "C.UTF-8"`. The maintainer's crash
  log now carries the reason.
- `N117` — the concurrency work **disarmed the only guard on the batch seam**, and
  it failed in the way guards fail worst: `test-batch.R` grepped `app.R` for one
  exact call spelling, the call moved into the child, and the guard *errored* on
  an empty match rather than failing. It now asks the invariant of every caller
  wherever the call lives, so moving code cannot silently disarm it again.

**Wrong figures, and a wrong figure that had already been published**

- `N118` — **a backwards statement period reached the governed feed as accepted.**
  On an auto-split bundle the merged header took the first segment's start and the
  last segment's end in *file* order; on a bundle printed newest-first that is
  "20 Apr 2026 to 19 Feb 2026", a period ending two months before it starts,
  stamped on every row of a court-facing extract. No check saw it: the checks run
  per statement against each statement's own correct period, and the merged header
  is assembled afterwards. Sorting by date was only half of it — a span is a claim
  about *coverage*, and min-start/max-end would have silently asserted two days
  this very file does not have. Gaps and overlaps now publish **no** period and
  say why, naming the uncovered dates, and a backwards period is refused at the
  point of construction rather than reported.
- `N121` — **a non-ASCII literal that ate bytes.** An en dash inside a regex
  character class in `R/labels.R`: under `LC_ALL=C` — the stated deployment
  locale, and the one the command-line entry point the incident procedure uses ran
  in — a labelled value carrying a currency symbol came back a byte short, and no
  longer verbatim. The byte scan that had covered the three UI files now covers
  `R/`, `run.R` and the test runner as well, and `run.R` and `tests/run_tests.R`
  gained the locale guard `app.R` already had.

**The screen says what it is, not what the engine calls it**

- `N127` — Field coverage said *not on this statement* about columns the statement
  **prints**: westpac.pdf heads three of them, and its template deliberately folds
  all three into one description band. That verdict reads the *template*, so it
  was making a claim about the file it is in no position to make. It now says
  *not read as its own column*, which is true either way.
- `N126` — Admin's Uploads table lists **every** upload, newest first, which is
  what the incident procedure sends a maintainer there for. It was headed *"new
  formats to pick up"* over help text saying the tool couldn't read them — so at
  step 1, hunting a successful conversion, the screen told him not to look in the
  one table that had it.
- `N123` — a raw template id appeared **twice** on the toolkit's save
  confirmation, on the screen a non-technical analyst uses to add a bank, while
  the card two inches away correctly gave the name.
- `N124` — on a run that never finished, the **Diagnostics** heading still sat
  over blank space while Checks and Field coverage had learned to say why they
  were empty. Its empty state has a different cause and now has its own sentence.

**The pages, re-read against the screen that exists**

- `N125` — **the first navigation instruction a new analyst gets was wrong.** The
  previous round moved Checks, Diagnostics, Field coverage and the dashboard line
  out from behind *Show me how it read this*; three sentences still sent her
  there, including the one on the page her own index tells her to start with. All
  three now describe where each surface really is, and a new guard reads `app.R`
  itself for what is behind that link, so the pages cannot drift from it again.
- `N128`, `N129`, `N130`, `N132` — `design.md` sent you to the wrong file for the
  step it flags as the one that catches people out; it also still said a non-ASCII
  string literal was fine, which had stopped being true. A page listed six kinds
  of *info* row and then referred back to five. And the maintainer's runbook
  recorded a build baseline this tree cannot produce, on the page that tells him a
  wrong number means stop.

**Suite:** re-measured on the tree this release was cut from and recorded, with
its date, in
[`docs/operational/maintaining-the-engine.md`](docs/operational/maintaining-the-engine.md).
The pass condition does not move and is not a total: *nothing failed, nothing
errored, nothing was skipped*. Register totals likewise live in
[`docs/context/findings-register.md`](docs/context/findings-register.md), which
states its own.

## 1.3.0

The release where the pages about the tool were held to the same standard as the
figures in it — and where the toolkit stopped asking for things it can work out
for itself.

**The tool answers more of it**

- The toolkit reads the bank's own name off the statement — the masthead first,
  then the legal imprint — instead of the first bank word anywhere on the page.
  That had answered "ANZ" on an ASB statement, because ANZ was one of the payees.
  A bank nobody has taught it now gets its own name too.
- When the wrong template read a statement, the way to correct it is on the
  result above the fold, not behind the evidence toggle. A statement read end to
  end by the wrong template looks perfect on screen; it is the failure this tool
  exists to prevent.

**Nothing fails quietly — including the front door**

- `convert_statement()` now wraps everything that touches the file path. Handing
  it something that is not a filename comes back as a status with a reason. Two
  documents had claimed "it never crashes" before it was true of the whole
  function.
- Admin controls that changed something on disk and said nothing now say so, and
  a table with nothing in it says so rather than instructing the reader to use a
  picker with no options in it.

**The documentation became part of the product**

- Three read-through pages — what it is, how it is built, how it got this shape —
  are linked from the README and inside the guards, instead of sitting where the
  orphan test could not see them.
- Both doc guards were widened: no page can be orphaned from an index, and a path
  named anywhere in the tree — a document, an engine comment, a README beside the
  templates — has to resolve to a file that exists. Each caught a real break
  before it was fixed.
- Every claim in those pages was read back against the running code, and the ones
  that had stopped being true were corrected — a fourth result word the docs never
  mentioned, a count of checks that was one short, one screen with four names, and
  measurements nobody had re-taken.

**Two things a reader could not do, and now can**

- The analyst's guides describe the screen she actually has: the first thing the
  app asks her for, the four words a check can come back with, and the column that
  tells her whose problem it is. "Teach it a new bank" now follows the toolkit's
  own numbered steps and starts where the work really starts — reading the preview.
- The maintainer has a procedure for *somebody says a conversion is wrong*: run
  id, original file, the same figure reproduced from the command line, template
  bug or bad scan. And a design document that says how to run it, how to ship a
  change to it, and how to put it back.

**The pages were measured against the running app, not against each other**

- `N107` — the folder an accountant was handed, `docs/for-analysts/`, did not
  exist; her pages sat in a directory named for operators. That folder now
  exists, as the short index of her four pages, and the README and the
  operational index both route to it. A new guard fails the suite when any
  `docs/` path named in prose — **including a folder, which has no `.md` on the
  end and which every earlier scan therefore skipped** — points at nothing.
- A second new guard loads **every fenced settings block in the documentation
  through the real config loader** and fails if the Admin tab would open. A
  printed admin password is a password everybody has; the shipped placeholder is
  safe only because the app treats it as "nobody has set one yet", and a
  near-miss like `change-me` is a working password that reads like a placeholder.
- `design.md` §8 said adding a check makes "existing tests" fail and pointed at
  three. Re-run and counted: one ordinary new check took the suite to **27 failed
  and 1 error, in 17 tests across 10 files**, and the runner's reporter shows the
  first ten and says so in a line that is easy to miss. The section now carries
  the real number, the command that shows all of it, and the order to read it in
  — the six bank goldens first, because they are the only failures that are
  evidence about the check. Its recipe also gained the step that was missing: a
  new diagnostic category must be **raised**, not just declared, or the suite
  fails it as a dead row.
- `when-something-goes-wrong.md` was re-checked against the Diagnostics table as
  it renders. It had told the accountant that an *info* row's "How to fix" says
  *No action needed* — false for three of the five, including the one the page
  used as its example, whose sentence asks her to review per account when several
  accounts are mixed. It had also filed *"check those pages against the source
  PDF"* under "ask the supplier for a cleaner scan"; that sentence belongs to the
  one diagnostic that means **do not release this output**. The page now lists
  every phrase the *What* column can print, against what it means for her.
- `adding-a-bank-template.md` now describes what the drafter really does with
  currency: every name found on the figures goes into one pile and is saved only
  if there is exactly one, so a code does *not* outrank a symbol that disagrees
  with it. Six codes and four symbols are recognised, the mark has to sit before
  a figure printed with cents, and **NZD in that box is usually a fallback nobody
  checked** — which looks identical to a reading.
- A doc said `scripts/run_app.R` "prints the address". It prints the literal
  placeholder `http://<this-vm>:8100`, which nothing resolves. Reworded, in both
  places that said it, with what the console really shows.
- The root pages were re-read against the code: `R/jobs.R` runs every conversion
  in its own child process, so "no lock, no queue, nothing to tune" is no longer
  true — there is a queue, it is capped in threads as well as processes, and
  anyone past the cap is told they are queuing. The stale findings tally and an
  un-retaken speed measurement were replaced with the register's own figure and a
  fresh one.

**Suite:** grew again this release (725 tests at 1.2.0, 792 partway through this
one). The live totals are not repeated here, because 1.3.0 is still open and a
number frozen mid-release reads as a promise about today; the last full run is
recorded in
[`docs/operational/maintaining-the-engine.md`](docs/operational/maintaining-the-engine.md),
and the pass condition that does not move is *nothing failed, nothing errored,
nothing was skipped*. Register totals likewise live in
[`docs/context/findings-register.md`](docs/context/findings-register.md), which
states its own.

## 1.2.0

Eighteen findings, all but two raised from real use on real statements. Five put
wrong figures on screen; two of those were regressions introduced inside this
release and caught by reviewing it.

**Wrong figures**

- `N37` — a `CR` marker printed on the line below its amount was folded into the
  payee, so every credit-card payment and refund came out with the charge sign.
  The marker is now appended to the money cell it sits under.
- `N30` — a printed opening or closing balance could be kept as a transaction, at
  the balance's own material amount. The summary-line guard now reads both the
  description cell and the whole line, always.
- The first fix for `N30` then ate real transactions whose payee began with a
  month abbreviation (`MAYFAIR CLOSING BALANCE`). Narrowed.
- `N38` — a stray date range on the page could move the inferred year on every
  transaction. A span is no longer claimed unless it was actually found.
- Balances could be paired across two different dormant accounts in one bundle.

**The toolkit**

- `N29` — a duplicate output id aborted Shiny's bind pass for the whole modal, so
  on a PDF not one control the toolkit drew statically was live. Fixed, and the
  id collision is now a test.
- `N34`, `N35` — the preview says how much it read, and the first question the
  builder asks is one the person in front of it can answer.
- `N28` — the escape hatch is reachable from the screens that need it.
- A correction to a shipped template is saved as `<id>_custom` carrying a
  `refines:` line, and detection honours it. Before this, a correction tied with
  the template it was correcting and lost.

**Cases, bundles and periods**

- Batch conversion: select a whole case folder, one row per file, click a row for
  that file's full result.
- Multi-period reconciliation, and `N39`/`N40`: auto-split is no longer opt-in
  (one of thirteen templates ever opted in), and an unfinished seed template can
  no longer join detection.
- `N33` — a page footer is no longer folded into the transaction above it.

**Wording and screen**

- Every check label now claims only what its check proves. "Every row was read"
  became "No row failed to read"; "Row count matches the statement" became "Row
  count"; "Redactions found and honoured" became "Redactions found".
- A check that could not run says "could not be checked" rather than borrowing
  the field-coverage wording "not on this statement", which meant something else
  on the same screen.
- A tie between two templates converts and holds for review instead of demanding
  a pick from a screen that had no picker on it.
- Admin's `user_template_ids` reactive shadowed the engine function of the same
  name, so the template origin line printed an R error and Hide and Delete did
  nothing, silently. Removed.

**For the maintainer**

- Admin refuses to open at all while the admin password is the shipped
  placeholder.
- A safe, shapes-only audit any statement or folder can be described with, and
  the no-PII AI survey prompt
  ([`docs/operational/survey-a-statement-with-ai.md`](docs/operational/survey-a-statement-with-ai.md))
  that produces the same description without the file leaving the room.
- The docs were culled: the point-in-time audits, the research briefings and the
  superseded plans are gone from `docs/`, and git history keeps them. Two new
  tests fail the suite if a doc link dangles or a page is orphaned from its index.

**Suite:** 725 tests, 3,366 passing assertions · 0 errors · 0 skipped (from 2,880
at 1.1.0). Register: 109 findings, 104 fixed, 4 open, 1 not-a-defect.

**Known and deliberate**

- `app.R` is ~4,600 lines and not yet split. What makes it hard is ~3,800 lines of
  interleaved reactives in one scope; a de-risked plan is in the register,
  including the test-helper change that must go first (60 tests grep `app.R` by
  physical line offset).
- Two older findings wait on **evidence, not effort**: a spread of real forms to
  tune the form fingerprint against, and one hand-keyed golden scan to measure OCR
  digit accuracy. Both fail closed and loudly today.
- Two mechanisms answer "one file, several periods" and both are needed — the
  bundle split (several separately-issued documents) and the span merge (one
  document, several sections). The register records why deleting either is wrong.

## 1.1.0

**Correctness**

- A tie between two templates was broken by template id, alphabetically, so a
  hand-built `aaa_bank` beat a tested `westpac_everyday_pdf` on nothing but its
  name. Shipped templates now win an equal score.
- `%y` / `%Y` year truncation could read a four-digit-year export into 2020.
- `type_dc` sign inversion in drafted templates: a bank marking debits `DR` or
  `Debit` had every debit read as a credit.
- Delimited and Excel statement metadata is threaded into the header, so the
  balance, count and period checks can run on those formats at all.
- A re-convert that flips accepted → withheld now withdraws the previously
  accepted rows from the feed.

**Privacy and governance**

- Destructive Admin actions are authorised server-side; visibility is not
  authorisation in Shiny.
- Local metadata capture: levels, per-category switches, account numbers stored
  only as a one-way hash, never in the Qlik feed.

**Suite:** 2,880 passing · 0 failed · 0 skipped.

## 1.0.0

First production build: the engine, the shipped templates, the governed Qlik
feed, the audit trail and the offline installer, as described in
[`docs/context/charter.md`](docs/context/charter.md).
