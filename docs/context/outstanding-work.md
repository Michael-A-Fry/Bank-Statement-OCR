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

## 0. THE CRUX: a table is found by its heading, but its COLUMNS are still pinned to the page

Everything in section A2 - one template finding however many tables a document
has, anywhere in it - rests on this, and it does not hold today.

`doc_locate_table()` finds a table by evidence: its heading wording, then the rows
it starts with, then the coordinates it was drawn at. When it finds the heading it
moves the reading WINDOW vertically to it. **It does nothing horizontally.** The
column bands stay exactly where they were drawn on the one example document, in
absolute page points. So a template that finds a table perfectly can still read
every value into the wrong column.

Measured, by calling the real engine on a two-column table (`Code`, `Units`) whose
bands were drawn at 55-150 and 195-260, then reading the SAME table on a copy
where it sits further right:

| shift | located by | confidence | what came out | `low_fill` | unclaimed words |
|-------|-----------|-----------|----------------|-----------|-----------------|
| 0pt   | header | 1.00 | `ABC/100  DEF/250  GHI/375` | - | 0 |
| 40pt  | header | 1.00 | `ABC/100  DEF/250  GHI/375` | - | 0 |
| 60pt  | header | 1.00 | `ABC/NA   DEF/NA   GHI/NA`  | Units | 3 |
| 80pt  | header | 1.00 | *no rows at all* | - | 6 |
| 120pt | header | 1.00 | **`NA/ABC   NA/DEF   NA/GHI`** | Code | 3 |

Read the last row carefully. The table was found, with **confidence 1.00**, and
the `Code` values were delivered in the `Units` column. `Units` reads as a
perfectly full column of clean values and every one of them is wrong.

To be fair to what exists: it is **not silent**. `low_fill` fires on the emptied
column, which makes the result `needs_review`. But the message that reaches the
screen is *"a column came out mostly empty - check the column edges on those, a
band drawn slightly too narrow loses the whole column"* (R/doc_extract.R:539-543),
which sends the reviewer to fix the empty column - the wrong problem entirely -
while the full-looking column of wrong values carries no mark at all.

**Why this is the crux, not just another fault.** It is the single thing standing
between where the tool is and the mode the owner described:

> "us as admins when it comes to making custom templates with lots of different
> tables that don't appear in a single doc, but can come up in many different ways"

A template built from many examples, finding tables wherever they appear, is
exactly the case where the bands and the page will not line up. Today the tool
would find those tables confidently and read them wrongly. It also generalises
several symptoms already in this register:

* A3 (a blank first row "pulls all info in the last column into the second to
  last") is this same class - bands and words disagreeing about where a column is.
* D5 (auto widths "sometimes too big and or too small for the data") is the same
  thing seen from the builder's side.
* The whole of A2 depends on it.

**The shape of the fix: anchor the columns to the header words, not to the page.**

When the table is located by its heading, the reader is holding that heading
LINE - its words and their x positions on THIS document. The bands were drawn
relative to the heading words on the example. So the correction is available for
free at the moment of the match: derive the offset (and, where page widths
differ, the scale) between the example's heading words and this copy's, and move
the bands by it. One measurement per table per document, and every horizontal
shift, margin change and page-width difference stops mattering. `doc_header_names()`
already relates a band to the heading word inside it, so half the machinery is
there.

Two things must come with it:

1. **A band that still does not fit must say THAT.** `unclaimed_words` is the real
   tell - words inside the table's own window that no column claimed. It was 3 in
   both wrong cases above and 0 in both right ones, which makes it a far better
   signal than `low_fill`. It is already computed and already carried
   (`unclaimed_words` in the summary frame); it needs to drive its own message -
   *"N words inside this table were not in any column - the bands do not fit this
   document"* - separately from the empty-column one.
2. **Do not correct silently.** If the bands are moved, the result must say by how
   much and on the strength of what, in the same place it says how the table was
   found. A correction nobody can see is how the next wrong answer gets built.

### 0b. Building a template across many documents already WORKS - and until 0 is fixed it is a trap

The mode described - a template holding tables that never all appear in one
document, built up from many examples - is already possible mechanically, and
nobody has noticed. `rb_open_template()` (app.R) loads a saved template's tables
back onto the page through `document_proposal_from_template()`, `rb_handoff()`
puts a DIFFERENT document under them, `+ Add a table` appends to `rb$tables`, and
Save writes every table back under the same id. So: open template A on example B,
draw the three tables B has that A did not, save. The library grows.

**And that is exactly the wrong thing to do today.** The tables carried over from
example A are drawn on example B at A's coordinates, so they appear in the wrong
place on the page in front of you - and the obvious, helpful, human response is to
drag them until they fit B. Doing that re-pins them to B and breaks them for A,
silently, and there is nothing on the screen that would tell anyone. Build a
template over ten examples that way and each table ends up pinned to whichever
example was open when it was last touched.

Three things this needs, in this order:

1. **Fix 0.** Bands anchored to the heading words stop being pinned to any one
   example at all, which removes the trap at its root.
2. **Say which document each table was drawn on.** A table carried in from another
   example must be drawn and labelled as such - "drawn on 2024-Q1.pdf, not this
   one" - so nobody adjusts it by accident. That is provenance ON the template,
   and it is worth having for its own sake: with forty tables, "has this one ever
   actually been seen in a real document, or did somebody guess it?" is a question
   an admin will ask constantly.
3. **A way to check the whole library against the whole folder.** An admin with
   forty tables and twelve example documents needs one button that says which
   tables still read correctly on which examples. Without it, every edit is a
   change whose blast radius nobody can see. R/batch_audit.R and Admin's
   "Batch & audit" tab exist for statements - whether they can serve this is worth
   establishing before anything new is built.

---

## 0c. A badly OCR'd page can come back "ok"

For a police deployment this is the most important single line in this document,
because a large share of what arrives will be a scan of a photocopy.

The plumbing is all there. Tesseract's per-word confidence is carried the whole
way through to the table parser (`ocr_conf`, R/ocr.R:59-75) specifically "so the
table parser can flag a transaction whose amount/date/balance cell contains a
low-confidence word - a misread digit that a page-mean confidence would otherwise
hide". `PARAM_OCR_PAGE_MIN_CONF <- 70` exists (R/params.R:34), and
`low_ocr_confidence` is raised at severity **high** (R/diagnose.R:422).

**And none of it decides the verdict.** The status gate is
(R/convert.R:387-393):

```r
status <- if (row_count == 0L) "unsupported"
          else if (kpi_fail_count > 0 || identical(recon$trust$level, "low") ||
                   isTRUE(multi_resolved$likely_multiple) || thin_match || ambiguous)
            "needs_review"
          else "ok"
```

OCR confidence is not in it. So a statement read off a page whose OCR scored 40
comes back **ok**, with a green card and a clean download, and the "loud caveat"
lives in the diagnostics panel behind "Show me how it read this" - a panel most
people never open. A misread `8` for a `3` in an amount is exactly the wrong
figure that looks right, and reconciliation only catches it if it happens to
break the balance.

Three changes, cheapest first:

1. **Put OCR confidence in the status gate.** A page below the threshold is
   `needs_review`, never `ok`. One clause.
2. **Mark the CELL, not the page.** `ocr_conf` is already per word and already
   reaches the parser. A figure containing a low-confidence word should be
   flagged in the OUTPUT - a column beside it, or a marker - so a reviewer opening
   the spreadsheet in six months sees which numbers the machine was unsure of.
   A page-level caveat on a 300-row statement tells nobody which row to check.
3. **Say it on the card, not in a panel.** "This was read from a scan; N figures
   were read with low confidence" belongs above the fold, next to the download.

And the same gate is missing on the OTHER routes entirely, which have no
reconciliation to fall back on - see 0 for the equivalent there
(`unclaimed_words`, currently a footnote, should be a gate).

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

### A2b. Title first, then header - and ALL the variants of each

Clarified further by the user:

> "It should be finding a table by the title AND header wording. If no title it
> should search for headers. If it matches an entire header row then it's a
> pretty good chance it's the same. I want to be able to specify all possible
> title/header and header variants so that tables can automatically be found."

**Yes, and the scoring for it already exists.** `.doc_header_hit(d, need)`
(R/tables.R:371) scores a line as the FRACTION of the remembered wordings that
appear on it, after `.doc_norm()` strips case, spacing and punctuation
(`"Unit price"` -> `unitprice`), so it is already robust to how the wording is
set. `.doc_find_header()` takes the best-scoring line and requires 0.6, and
`.doc_anchor_confidence()` (R/tables.R:422) already returns **the hit fraction
itself** as the table's confidence - so "an entire header row matched" already
means 1.0 and a partial match already scores lower and says so.

Three changes, all small:

1. **Variants.** `need` is one flat set of words today, and `unlist()` would
   flatten a nested list into one set - which would score every variant together
   and match none of them. So `.doc_header_hit()` must branch on the structure:
   a list of character vectors means *variants*, and the score is the **max**
   over them. A flat vector behaves exactly as it does now, so every template
   already saved is untouched.

2. **Titles.** There is no title matching at all - `tb$name` is only a label.
   Add `.doc_find_title(lines, any_of)`: the first line carrying any of the
   remembered title phrases, matched through `.doc_norm` like everything else.

3. **The order.** `doc_locate_table()` tries header -> first_column -> position.
   It becomes title -> header -> first_column -> position, because a title says
   WHICH table this is and a header only says what SHAPE it is.

The template shape (all of `find:` optional, so nothing already saved changes):

```yaml
tables:
  holdings:
    name: Holdings
    find:
      title_any:
        - "Portfolio holdings"
        - "Investments held"
        - "Holdings at period end"
      header_any:                       # each entry is one whole header row
        - ["Code", "Units", "Price", "Value"]
        - ["Security", "Units", "Unit price", "Market value"]
      repeats: true                     # see A2 - as many as the document has
```

**THE TRAP, and it is the wrong-figure-that-looks-right kind.** A header alone
says what shape a table is, never which one it is. So a template carrying two
table entries with no titles and overlapping header wording cannot tell their
occurrences apart, and would assign rows to whichever entry scored first -
silently. That must be refused at save time by `validate_document_template()`,
with the reason, not discovered later in a spreadsheet. Two entries may share
header wording ONLY if both carry titles.

**AND THE THRESHOLD IS TOO LOOSE TODAY - measured, not suspected.** Matching is
`grepl(fixed = TRUE)` of each normalised wording against the whole normalised
line, so one header's words are found inside another's. Run against the real
`.doc_header_hit()` on the line `"Security Units Unit price Market value"`:

| what was remembered                          | score |
|----------------------------------------------|-------|
| `Security, Units, Unit price, Market value`   | 1.00  |
| `Code, Units, Price, Value`  (a DIFFERENT table) | **0.75** |

0.75 clears `min_hit = 0.6`, so the wrong header already matches - `units`
appears, `price` is inside `unitprice`, and `value` is inside `marketvalue`.
On a document with forty similarly-shaped tables that is not a corner case, it
is the normal case.

Two consequences for the design:

* Variants make this BETTER, not worse: with both wordings listed, the max is
  1.00 for the right one against 0.75 for the wrong one, so the correct entry
  wins outright. That is an argument for listing variants rather than relying on
  a loose threshold to cover them.
* But the bar has to rise with it. Once the true wordings are listed, a match
  should need to be at or very near 1.0, and a whole-word match (not a
  substring) - `Price` should not be found inside `Unit price`. Both changes are
  in `.doc_header_hit()`, and both need the existing shipped templates run
  against them before they land, because they tighten a rule those templates
  currently pass under.

**In the builder**, variants are collected the way the fingerprint now is: a box
per table for "other wordings this can have", one per line, and **draggable off
the page** so a variant met on a second example is captured word for word rather
than retyped. Retyping is what breaks exact matching, which is the whole reason
the fingerprint got a drag.

A later refinement worth noting but not building yet: when a table is found on a
partial header hit (0.6-0.99), the tool KNOWS it has met a new wording. It could
offer "this document called it X - remember that too?" and learn the variant
instead of being told it.

### A2c. All of this lives INSIDE one template - and that means two levels

Confirmed by the user: *"This is, within a specific template."*

So there are two separate decisions and they must not be confused:

| level | what decides it | what it answers |
|-------|-----------------|-----------------|
| `fingerprint.page_contains_all` | `detect_document_template()`, R/doc_extract.R | **WHICH** template reads this document |
| `tables[].find.title_any` / `.header_any` | `doc_locate_table()`, R/tables.R | **WHERE** the tables are, once that template has been chosen |

The working shape is therefore ONE template per document FAMILY: a fingerprint
made only of the part that never changes (the issuer's name, the report's
standing title), and `find:` blocks on each table carrying all the variation
inside the family. **An over-specific fingerprint is fatal to this** - the
template never fires at all, so its variants never get a chance to be tried, and
the document falls through to some other template or to unsupported. Guidance
in the builder has to say that plainly, because the instinct is the opposite:
somebody trying to make matching more reliable will add MORE phrases.

**AND A REAL BUG, found while confirming this.** The two levels compare text
differently:

* `detect_document_template()` uses `grepl(phrase, hay, fixed = TRUE)` - case
  and spacing sensitive, no normalisation.
* `.doc_header_hit()` uses `.doc_norm()` - case, spacing and punctuation all
  stripped.

Measured against a page printing `QUARTERLY PORTFOLIO REPORT`:

| fingerprint phrase as typed  | matches |
|------------------------------|---------|
| `QUARTERLY PORTFOLIO REPORT` | TRUE    |
| `Quarterly Portfolio Report` | **FALSE** |
| `Quarterly  Portfolio Report` (double space) | **FALSE** |

This is very likely the original complaint - *"I put a phrase printed on it, bang
smack on front page, still used another template"*. Typed in title case against a
page printed in capitals, it can never match, and nothing on screen says why.

Dragging the phrase off the page (built) sidesteps it by capturing the exact
printed text. But the fingerprint should normalise the same way the header
matcher does, so a typed phrase works too - and the same rule then applies
everywhere text is compared to a page, instead of two rules that disagree.
Statement templates use the same `.fp_fingerprint_problems` gate, so whatever is
done here has to be checked against them as well.

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
