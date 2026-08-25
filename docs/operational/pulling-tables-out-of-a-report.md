# Pulling tables out of a report

For anything that is **not** a bank statement: a valuation report, a trust
summary, a schedule of assets, a court exhibit, a government return. Anything
with a table on it, or a figure with a label beside it.

You point at what you want. Nothing is guessed at, nothing is created until you
ask for it, and everything the tool works out for itself is on the screen with a
control beside it to change it.

**It takes two drags per table.** That is the whole thing.

---

## Before you start: what comes out, and where it goes

A statement gets checked against its own running balance. **A report has no
running balance**, so there is nothing behind these numbers that could tell a
right one from a wrong one.

So: what you get is **a file to download** — a workbook with a sheet per table,
and one long CSV. **None of it reaches the Qlik dashboards.** That is enforced in
the engine, not left to anybody's care, and it is why you check what comes out
before you save.

---

## Do it once

### 1 · Upload it and say what it is

**Add a template** → choose the file → **Anything else — a report, a form, a
summary, a letter**.

The page appears with your document on it and a grey bar across the top:

> Nothing is waiting for a drag. Nothing you drag on the page will change anything.

That is the normal resting state. Drag anywhere you like — look around the
document, turn pages — and nothing happens. The tool only listens when you have
asked it a question.

### 2 · Press **+ Add a table**

The bar turns amber and asks for one thing:

> **Drag a box round the table's TITLE.**
> Its wording becomes the table's name. No title? Press Skip.

The bar always says exactly what the next drag will do. When it is amber,
something is waiting. When it is grey, nothing is.

### 3 · Drag round the title

Whatever wording you box becomes the table's name. If the table has no title,
press **Skip — it has no title** and name it yourself afterwards.

The bar immediately asks for the next thing:

> **Drag a box round the ROW OF COLUMN NAMES.**

### 4 · Drag round the column names

Include the whole heading block. If the heading is printed over three lines —
*Revenue* over *Medical &* over *Welfare* — box all three. The tool counts the
lines and fills in **Heading rows** for you.

That drag gives you the **columns**, named from the headings you boxed — and a
first guess at where the table starts and ends.

> **If you get one column**, the box had one unbroken run of words in it — a
> sentence, or the table's title — rather than a row of separate headings. The
> tool says so. Drag round the headings themselves.

### 5 · Click where it starts

> **Click the page where the table STARTS.**

Just under the column names. Skip it and the tool's own guess stands.

### 6 · Click where it ends

> **Click the page where the table ENDS.**

Turn the page first if the table carries on past this one — the page box and the
**Next page →** button are above the picture, and the bar keeps waiting.

### 7 · If it runs over pages: the bottom edge of the ones in between

> **Click the lowest line the table reaches on a page it CARRIES ON past.**

This step only appears when the table spans more than one page, and it is the one
worth understanding. *Where it ends* is a place on **one** page — the last one. On
every page in between, the table runs down to whatever bottom the reader is
given, and that is the bottom of the paper unless you say otherwise. **A footnote,
a source line or a page footer under the table is then read in as rows, on every
page but the last.**

Click just below the last real row and everything under it is footer, not table.
Skip it if the table really does run to the bottom of the page.

You can change it later: it is the **Bottom edge on the pages in between** line on
the right, with **Run to the bottom of every page** to undo it.

### 8 · Check it, fix anything, save

Look at the picture. Every column is tinted; the lines between them are the
column edges. The green **START** line and the red **END** line are where the
table begins and ends.

Fix whatever is wrong (next section), then **Save this table**.

### 9 · Next table, or read it

Press **+ Add a table** again for the next one, or **Read the whole document** to
see what actually comes out — the tables **and** the label/value pairs, with a
download for each.

---

## Changing what it worked out

Everything below can be changed before you save, and again afterwards — every
saved table has an **Edit** button that puts it straight back.

### The columns

Each column is a row in the list, with its name, its left and right edge, and
what is in it.

| I want to… | Do this |
|---|---|
| Rename one | **Edit** on that row, type the name |
| Say what is in it (money, date, text…) | **Edit**, then **What is in it** |
| Move its edges | **Edit**, then just drag where it should go — pressing Edit arms the drag and the bar names the column. Or type **Left edge** / **Right edge**. |
| Add one, or split one in two | **+ Add a column**, then drag over the column you want |
| Delete one | the **×** on its row |
| Start the columns again from scratch | **Work the columns out again** |

**Columns may have space between them. They can never overlap.** A report has
real whitespace between its columns, so you can draw them where they actually
are. What you cannot draw is an overlap — a column trying to run into its
neighbour is trimmed at it, and the tool says so — because anything inside an
overlap is read into one column and lost from the other.

Nothing falls silently into the space between: every word inside the table that
no column claimed is counted and reported as **unclaimed words** when you read the
document. A gap is visible. A lost column would not be.

> **Only the column you touch moves.** Add one and you get one, exactly where you
> drew it. Move one and its neighbours stay where they are.

> **"Work the columns out again"** is the one to reach for when the columns look
> nearly right but the figures are not landing in them. It ignores the headings
> and works the columns out from **the data instead**. It is the fix for the
> commonest awkward layout: a heading printed further right than the column of
> figures underneath it.

### Where it starts and where it ends

Both are stated in words — *Starts page 2, 14% down the page* — with:

- **Show me** — turns to that page
- **Move it** — then click the page where it should be
- **Work the end out for me** — reads down and finds where the rows stop
- **End at the bottom of its page** / **End at the bottom of the last page**
- **Type the exact positions instead** — for when you want it exactly on a number

On a table that runs over pages there is a third line: **Bottom edge on the pages
in between**, which is step 7 above. Until you set it, the table runs to the
bottom of the paper on every page but the last, and a footer under it is read in
as rows.

### The heading

**Its top rows** — *name the columns, and are not read as data* or *are data,
like every other row* — and **Heading rows**, how many printed lines the heading
takes up.

**You never say how many rows of data there are.** That is read from the page,
between the start and the end.

### Seeing the page underneath

The tick boxes above the picture turn each layer off on its own: **Tables**,
**Column lines**, **Names**, **Values**, **Start and end**.

Turn **Names** off when you want to read the document's own column headings and
compare them with the ones the tool worked out. Nothing the tool draws sits over
the page's own words — its labels float in a strip above the paper — but with a
dense table it still helps to see it bare.

---

## Values: one figure with a label

A **value** is a single thing with a name beside it — *Prepared for*,
*Closing balance*, *Client reference*. The **Values** tab, then **+ Add a value**.

**Two drags, and neither is a guess.**

1. **The label** — the words that name it.
2. **The value** — the figure or words themselves.

The tool then tells you which side the value sat on, in words:

> On this page the value sits **to the right of it**, about 40 points away. That
> is what will be looked for next time.

**That is what makes it survive the next copy of the document.** On the next
report the label is found by its wording wherever it has moved to, and the value
is taken from that side. A figure two digits longer, or a label a word wider,
still reads.

You can override the side from the dropdown — *to the right of / to the left of /
under / above* — before or after saving. Every saved value has **Edit**, and
pressing it arms the drag for the **value**; **Re-draw the LABEL instead** is one
press in the bar.

**The name is a column heading in a file**, so it comes out lower case with
underscores: type *IRD Number* and it is saved as `ird_number`. The line under the
box says what it will become while you type, and the box is set to it when you
save. Type it however reads best to you.

> Already know the wording and do not want to drag? **"Know the wording already?
> Type them instead"** takes a list. Those are found by wording anywhere on the
> document, with no coordinates at all — the most portable option of the lot when
> the wording is reliable.

---

## Read it before you save it

**Read the whole document** runs the real extraction and shows you:

- a verdict card — how many tables, how many rows, how many values
- **every table**, each with its name, its row count and its rows
- **the values**, each with what was found beside its label
- a line per table: pages, rows, columns, **thin columns**, **unclaimed words**,
  and how each table was found

A template can be **all values and no table at all** — that is what a form is —
and reading one works exactly the same way.

### What you can download

| | What is in it |
|---|---|
| **the workbook** | a sheet per table, a **Values** sheet, and a report sheet |
| **one long CSV** | every table stacked: page, row, column, value |
| **the values CSV** | one row per label/value pair — the pairs are **not** in the long CSV, because a pair is not a page/row/column/value |

The same three appear on the **Convert** screen when somebody runs a document
through a saved template.

Two numbers are worth understanding, because they are how the tool tells you it
went wrong rather than pretending it did not:

| | What it means | What to do |
|---|---|---|
| **unclaimed words** | Words inside the table's boundary that no column claimed. They are **not in your output**. | Widen a column, or **Work the columns out again**. |
| **thin columns** | A column that came out mostly empty. | Sometimes true (that column really is blank on most rows). Sometimes a column edge in the wrong place — check the picture. |

A clean read says *Every table was found by its own heading and every column
filled.*

---

## Name it and save it

The bottom of the page fills itself in: the **issuer** from the file name, the
**save name** from the issuer and kind, and **a phrase printed on it** from the
document — a saved table's own title, which is what makes this document
recognisable and the next one not.

Check the phrase is something that appears on **this** kind of document and not
on every document you own. That phrase is the whole of how the tool recognises
the next one; words like *Statement* or *Total* would match everything.

**Save template.** From then on, dropping that kind of document on **Convert**
produces the workbook without anybody touching the builder again.

---

## What it still cannot do

Worth knowing before you spend an afternoon on one:

- **A cell that spans several columns** lands in one of them, not across them.
- **Two tables printed side by side** can both be built, but each will report the
  other's words as unclaimed — the numbers are right, the warning is noise.
- **A page with no text layer at all** (a photograph of a page) has nothing to
  point at. It needs OCR first — see
  [when-something-goes-wrong.md](when-something-goes-wrong.md).

---

Related: [adding-a-bank-template.md](adding-a-bank-template.md) (the same job for
a bank statement, which is a different screen because a statement is checked
against its own balance) ·
[converting-statements.md](converting-statements.md) ·
[when-something-goes-wrong.md](when-something-goes-wrong.md)
