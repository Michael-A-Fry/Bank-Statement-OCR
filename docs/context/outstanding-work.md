# Outstanding work - the Beth-proofing register

Everything raised in review that is **not yet done**, with the words it was
raised in. Nothing here is a nice-to-have: each line is a real failure somebody
hit on a real document.

The two routes are equal. Every item is judged against both:

* **STATEMENT** - a table of transactions with a running balance, reconciled,
  feeds the dashboards.
* **OTHER** - a form (labelled values) or a report (many tables). Download only.

> "You need to think about the two key use routes, and they both need to be
> perfect. Converting and creating templates for STATEMENTS and ALL OTHER DOCS.
> They both need to be considered and treated fairly. Clear. Concise. Simple.
> Understandable. Bethable."

---

## A. The engine - what comes out wrong

### A1. Repeated headers on continuation pages come back as rows
> "Need an in-between pages start and end - sometimes there's headers on the
> next x pages that are being pulled as rows. Again this needs to be as simple
> as humanly possible."
>
> Asked again, as a design question: *"Can this be as easy as the in-between
> start and end? Define where the data starts and stops?"*

**Yes - and both edges already exist on the model.** `doc_locate_table()`
(R/tables.R:500-501) sets, for every page in the run:

```r
y_min <- if (p == p0) y_start else band_top   # band$y_min
y_max <- if (p == p1) y_end_declared else band_bot   # band$y_max
```

`band$y_max` is the in-between BOTTOM edge, and the builder already asks for it
(the `bottom` step). `band$y_min` is the in-between TOP edge, is read on exactly
the same line, and **nothing ever asks for it or draws it** - it silently
defaults to 0, the top of the paper. So this is one concept, not two: *on the
pages in between, the data starts here and stops here.* The work is to ask for
the top edge as the mirror of the bottom one, draw both, and default both.

**And the automatic answer already exists too** (R/tables.R:506-512): on a
continuation page the reader looks for the repeated header and, when it finds
it, moves the window down to it and skips `header_rows` lines. So the common
case should already work. When it does not, the cause is upstream:
`need_hdr <- tab$anchor$header_text %||% .doc_column_names(tab)` (R/tables.R:463)
and the builder writes that wording from the COLUMN NAMES
(app.R:3146 `d$anchor$header_text <- ... cc$name`) - so **renaming a column
breaks the match**, and `.doc_find_header` needs 0.6 of the names to hit.

So the fix is two belts, in this order:

1. Keep the remembered header wording as what was PRINTED on the page, separate
   from whatever the column was renamed to. Then the automatic skip works and
   nobody is asked anything.
2. Add the in-between TOP edge as the mirror of the bottom edge that already
   exists, defaulted and drawn, so when the automatic answer is wrong it is
   corrected with a gesture already learned.

Both belts, not one: the automatic path cannot cover a page whose header is not
reprinted but which still carries a running title or a page banner.

### A2. A template that finds however many tables the document has
> "If I'm creating a generic template for an overarching document that may
> contain x number of tables, but that varies between every report - can I
> create a template with the YAML that specifies all possible tables so that it
> auto picks up as many as possible?"

Clarified by the user when asked:

> "A number of these 'other' reports will NOT have consistent page numbers, apart
> from MAYBE the first. These have standardish formats but there may be x number
> of pages in between, and sometimes there may only be 2 tables and others 40.
> How do we best account for that? **We already collect table titles.**"

That last sentence is the design. The builder's first gesture is "drag a box
round the table's TITLE" and the wording is stored as the table's `name` - but
today it is only a LABEL. It should be the SEARCH KEY.

**What the code already does.** `doc_locate_table()` (R/tables.R:437-560)
already finds a table by evidence rather than position - it tries, in order:
its header wording (`.doc_find_header`), the rows it starts with
(`.doc_find_first_column`), and only then the coordinates it was drawn at. And
`follow: true` (R/tables.R:534-548) already extends a table across as many extra
pages as keep repeating its header, recording `extended_to` so the extension is
never silent.

**The two things missing.**

1. **The search is pinned to one page.** `p0 <- tab$start$page`, and the header
   hunt runs on `lines0`, the lines of THAT page only. It has to sweep the
   document instead. Nothing else about the anchor logic needs to change.
2. **One entry can only match once.** A table entry needs to be able to say
   "every place this appears is another one of me", producing as many tables as
   the document actually contains.

**The rule, in one sentence a person can hold:** *a table starts at its title and
stops at the next one* (or at the bottom edge, or where its header stops
repeating - whichever comes first).

That single rule covers both shapes the user described:

* 40 tables with DIFFERENT titles and the same columns - each occurrence becomes
  its own table, named by its own title text.
* the same table continuing over unknown pages - already handled by `follow`.

A table the document does not contain this month produces NOTHING - not an empty
sheet. That falls out of the rule for free: no title, no table.

**In the builder** this must not become YAML. You draw ONE example table as you
do now, and tick *"this document has more tables like this one"*. Nothing else
changes. Page numbers stop being part of the template at all for these.

Still open, and worth confirming with the user when it is built: what happens
when two occurrences carry the SAME title (numbered `_1`, `_2`, presumably), and
whether the output should be one sheet per table or one long sheet with a
`table` column naming which it came from. The workbook already has a
`table` column, so the second is nearly free.

### A3. A column whose FIRST row is blank swallows the column beside it
> "When I define columns, but one of the columns doesn't have a first row of
> data, it pulls all info in the last column into the second-to-last column.
> It's not common, but it is okay for the first row of a column to be empty."

A defined column band is a fact about the template. It must not be re-derived,
narrowed or merged because the first data row happens not to fill it.

### A4. One row printed over three lines comes back as three rows
> "One row with data on 3 different lines coming back as 3 rows. This edge case
> is where it's only two columns, where the first column's first row and only
> row data is [on the first line]."

`.doc_is_wrap()` was rewritten for the indented case; this shape - two columns,
the left one filled only on the first line - still needs proving.

---

## B. Convert - what happens when a document arrives

### B1. "Other" must open the editor rather than quietly converting
> "For Other statements the likelihood that something will need to change on
> every convert - it shouldn't just auto process, it should process and open up
> the editor. For statements I want a threshold where it does and where it
> doesn't, but ensure there is an option even if it's confident."

* OTHER: convert, then open the builder on the result. Always.
* STATEMENT: a confidence threshold decides whether the toolkit opens by
  itself - and the way to open it is on screen even on a confident, clean
  result.

The threshold is a deployment setting with a stated default, not a magic number
buried in the code.

### B2. Feedback belongs on an "other" result too
> "Ensure submit feedback is also there on other reports."

The feedback control renders for statement results only. A report result has no
reconciliation behind it, which makes a human's "this is wrong" the ONLY signal
there is - so it matters more there, not less.

---

## C. Admin - seeing and tracing the whole library

### C1. Templates split by statement and other
> "All templates are accessible, but broken down by statement and other... All
> templates should also be viewable in admin, but split by statement and other."

The library now lists all three kinds with a `kind` column (done). What is
outstanding is the SPLIT: statement and other as two clearly separated groups,
not one list sorted by a column.

### C2. All feedback in one place, traceable to the document and the template
> "All feedback accessible in admin in same area, but ensure it can be traced to
> the statement and template."

Feedback must sit beside the templates it is about, and each entry must name the
document it came from and the template that read it - for both routes.

---

## D. The builder - simple enough for somebody who has never seen it

### D1. Pop it out, like the statement toolkit
> "I like the bank statement pop out. Do this as well for the other."
> "When creating template, like the convert xray wizard, we need to pop it out
> so you only see the info related to creating the template. User is finding it
> hard to understand what to do next - select table, title, columns etc."

### D2. Far fewer things on screen
> "There are LOTS of buttons, lots of things to interact with. It NEEDS to be
> even more simple."
> "'It's top row' - does that need to be prominent? Or can it be collapsed?"
> "There should be CLEAR distinctions between what's needed info wise NOW and
> report level - everything MUST be SUPER CLEAR or simple."

### D3. Save has to pull the eye
> "Save - this needs to be more prominent / draw in the user to know what to do
> next."

### D4. The bottom edge should already be set
> "The bottom edge on pages in between needs to default show up. Shouldn't need
> to work out end for me - end at bottom of page, or end at bottom of last
> page."

### D5. Columns must move out of each other's way
> "If I need to expand or contract a column in the middle of others, that clashes
> where the auto boundaries are, the other columns NEED to adapt to it rather
> than just do nothing. The auto column widths are sometimes too big and or too
> small for the data, and quickly resizing the column that needs to be bigger is
> SUPER hard - I've got to first collapse the larger one."

Today a middle column is clamped at its neighbour and the neighbour does not
move. Widening one must push the next along.

### D6. It must always be obvious what is being edited
> "Clear distinction between when I'm editing columns, tables etc."

---

## E. Still unexplained

### E1. The brush dies with `state.panel === null`
Driving the real builder in Chromium, a drag on `#rb_plot` throws
`TypeError: Cannot read properties of null (reading 'range')` from shiny.js
`brushTo` -> `boundsCss`, repeatedly, and `input$rb_brush` never fires. The blue
rectangle is left on screen.

`state.panel = coordmap.getPanelCss(state.down, expandPixels)` returned null.
The server DOES send a correct coordmap for `rb_plot` (confirmed on the
websocket frame: one panel, range `left 0, right 743.5, top -1, bottom 839`), so
the panel lookup is failing against a coordmap that looks right. Unfinished:
whether the drag point is being scaled against stale image dimensions, whether a
re-render between mousedown and mousemove replaces the coordmap, or whether
`expandPixels` / the negative-`ylim` gutter is involved.

This is the strongest remaining lead on:
> "Click and drag just stopped working at one point. Cancel out of table, create
> it again, reload page, still no drag. The blue box appeared and does not
> disappear even when the next column etc is added."

Three layers of that were fixed (resetBrush ordering, `resetOnNew = TRUE`, and
the mousedown-click/drag reconciliation), and a fourth (the sticky instruction
banner swallowing mousedowns) - but this one is still open.

### E2. Reliability
> "What's going on, stuff just keeps failing. Sort it out."

Every fix above has to arrive with a test that would have caught the original,
and the two routes have to be walked end to end in a real browser on a real
messy document before anything is called done.

---

## Done today (for orientation, not for re-doing)

* Admin sees every kind of template; every action dispatches on the kind.
* Opening a report template opens the report builder, not the statement toolkit
  (`document_proposal_from_template()` is the exact inverse of
  `document_template_from_proposal()` and round-trips identically).
* Hiding works for forms and reports, not just statements.
* Convert asks what the document is, and the answer outranks detection
  (`convert_document(kind = auto | statement | other)`).
* The identifying phrase can be dragged off the page.
* A drag that Shiny also reports as a click no longer acts twice.
* The sticky instruction banner no longer swallows drags.
