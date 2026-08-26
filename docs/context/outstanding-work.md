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

A table that runs over pages reprints its column headings at the top of each
one. Those heading lines land in the output as data rows. `band$y_max` closes
the BOTTOM of a continuation page (`doc_locate_table`); nothing closes the TOP.

Must be automatic where it can be - the header row's own wording is already on
the template (`anchor$header_text`), so a line on a continuation page that
matches it is a header, not a row. Any control added for it is a last resort,
not the first answer.

### A2. A template that finds however many tables the document has
> "If I'm creating a generic template for an overarching document that may
> contain x number of tables, but that varies between every report - can I
> create a template with the YAML that specifies all possible tables so that it
> auto picks up as many as possible?"

Today `tables:` is a fixed, named list and each entry is pinned to a page and a
y. A quarterly pack with three holdings tables one month and seven the next
needs one template that finds each table wherever it appears and however many
there are - matched by its heading wording and its column shape, not by its
coordinates. Tables the document does not contain must be absent, not empty.

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
