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

> **DONE - commit 34ede16.** `.doc_band_shift()` measures how far this copy prints
> the table off the heading line the locator already holds, and `.doc_shift_cols()`
> moves the bands. Four refusals (fewer than two columns matched, columns
> disagreeing, no heading found, everything already inside its band). Said out
> loud in `loc$detail`. Pinned 0-200pt. **Note I2: this fixes a uniform
> TRANSLATION; a table that gains or loses a column is a different fault and is
> still open.**

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

## 1. THE STRIP LIST - 182 controls, and the job needs four

> "This needs to be as SIMPLE as possible. No fancy bullshit. What can we strip?
> What doesn't NEED to exist?"

Counted, not estimated (`app.R`, every `actionButton` / `selectInput` /
`textInput` / `fileInput` / `radioButtons` / `checkbox*` / `download*`):

| | |
|---|---|
| interactive controls | **182** |
| outputs | 177 |
| observers | 148 |
| `app.R` | 8,793 lines |
| engine (`R/`, ~50 files) | 16,763 lines |
| docs | 34 files, 6,939 lines |

By screen: **report builder 56 · Admin 45 · Convert 34 · statement toolkit 32**
· Add-a-template front 4 · About 2 · misc 9.

Beth's whole job is: pick a file, say what it is, press Convert, download.
**Four controls.** The other 178 exist to teach a layout or to administer one.

### 1a. TWO BUILDERS DOING ONE JOB - the biggest cut available

The statement toolkit (`g_`, 32 controls, a modal) and the report builder
(`rb_`, 56 controls, an inline tab) are two applications, two mental models and
88 controls for a single task: *point at a page, say what is there, save it*.

A bank statement **is** a table with columns. The only real differences are:

* three extra questions - which column is the date, which the amount, which the
  balance,
* an amount-sign and date-format setting,
* and reconciliation, which happens after reading and not during it.

Those are **fields on a template, not a second application.**

One builder. When the kind is "bank statement" it asks the three extra questions
and reconciles afterwards. That single change:

* removes roughly a third of the app's surface,
* delivers the "50/50, treated fairly" requirement by removing the thing that has
  to be made equal - there is one way to teach a layout, so the two routes cannot
  drift apart,
* **makes D1 moot.** D1 asks for the report builder to be popped out to look like
  the statement toolkit. Making them *look* alike is a worse answer than there
  being one of them.

This is the largest single item in this document and it should be costed before
any of the D-series simplifications are attempted, because most of them stop
existing if it goes ahead.

### 1b. Five doors into one function

A conversion can be started from: Convert (one file), Convert (a case folder),
Admin -> Batch & audit, the folder poller (`R/inbox.R`), and "try it on a
sample". Five paths into one engine call.

Every alternative route is a route that rots without anyone noticing, because
nobody exercises it weekly. Convert already handles one file or thirty through
the same control - that is the pattern. Keep it, keep the folder poller **if the
team actually uses it** (establish that; it is 43 lines and cheap either way),
and delete the rest.

### 1c. Admin: four sub-tabs, two jobs

Insights / Templates / Data capture / Batch & audit - 45 controls. An admin has
two questions: *what is in the library and is it right*, and *what is failing*.

* **Data capture** (a whole tab plus `R/metadata_capture.R`, 313 lines) is on-box
  analytics. Nobody in a police unit asks that question.
* **Insights** and **Batch & audit** overlap heavily - both answer "what is not
  converting".

Two tabs: **Templates** (with feedback beside them, which C2 asks for anyway) and
**Health**.

### 1d. Two vocabularies

`dictionaries/labels.yaml` (the label dictionary) and `dictionaries/lexicon.yaml`
(recognition vocabularies), each with its own Admin editor. Both mean "words the
tool looks for". One file, one editor.

### What NOT to strip

**None of the ~50 R modules is dead code.** Every one was checked for references
outside its own file and its tests; all are wired in. The engine is not bloated
with orphans - `R/split.R`, `R/detect_redaction.R`, `R/column_profile.R`,
`R/row_coverage.R` and the rest all earn their place.

**The bloat is entirely surface.** Screens and options, not code. Any strip that
deletes engine capability is cutting the wrong thing; any strip that removes a
second way to do something already possible is cutting the right thing.

### The rule to apply from here

Before anything is added to a screen: *does this answer a question the tool could
have answered itself?* If it could, the tool should answer it and the control
should not exist. That single rule is what turns A1 (repeated headers), D4 (the
bottom edge) and half of D2 from new controls into no controls.

---

## A. The engine - what comes out wrong

### A1. Repeated headers on continuation pages come back as rows

> **DONE - commit 051daae.** The heading is learned off the table's own first
> page (`.doc_header_printed`) and matched whole-line on later pages
> (`.doc_find_printed_header`), as a fallback only. Refused if a learned line
> carries a figure. Reported in `loc$detail`. **The in-between TOP EDGE
> (`band$y_min`, the mirror of the bottom edge) is still not asked for or drawn -
> that half is open.**
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

> **DONE - commit 5e20df1.** The leading is read off the two lines' type heights
> instead of a page-wide quartile that a tighter footnote block could poison.
> Net deletion of `win_pitch`. **See I1 for the mirror image, still open: an
> unlabelled total folded INTO the row above it.**
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

**Decide 1a first.** If the two builders become one, this stops existing: there
is nothing to make match. Making two builders *look* alike is a worse answer than
there being one of them, and doing this work first means doing it twice.

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

---

# G. Evidence and provenance

Everything below was measured against the running engine, not inferred. This
section is about what survives into the FILE and the RECORD, because a screen
gets closed and a court date is two years later.

## G1. An "other" output names nothing that produced it, and its template is always version 1

A report converts in March, the workbook goes in the bundle, and in September the
defence asks how row 14 was derived. The file contains sheet names, values and a
Report sheet of counts. No template id, no template version, no engine version,
no source filename, no source hash, no run id, no date.

The statement workbook carries all of it (Summary sheet: `template_id`,
`template_version`, `source_file`, `source_sha256`, `page_count`, `row_count`;
JSON `build` block: `engine_version`, `template_sha256`). The report and form
writers carry none.

And there is nothing to look it up in. `template_sha256` is `NA_character_` on
both other routes (R/forms.R:323, :352) - and the fallback is dead too:
`document_template_from_proposal()` hardcodes `version = 1`
(R/doc_extract.R:678) and `rb_save` writes that object every time, so **every
report template is version 1 forever, including after "Save replaces it"**.
Today's template and last month's are indistinguishable in every record kept.

The file has no handle either: the download name is the source filename, so two
conversions either side of a template edit produce two identically-named files.

**Severity: blocks-work.** It does not make a figure wrong; it makes a right
figure indefensible, which here costs the same.

**Fix:** `template_sha256()` (R/convert.R:48) already works on any template -
call it and stop writing NA. Give both writers the `build` argument
`write_outputs()` already takes (R/outputs.R:154, 203-205): one sheet, one CSV
header block, carrying engine version, template id/version/hash, source
file/hash, run id, UTC timestamp. Bump `version` on a save over an existing id.

## G2. The form output drops `conflict` - the only signal a form has

Measured on the shipped sample and template: `extract_fields()` returns
`conflict = TRUE` for three values (contributions, tax, fees) - labels printed
more than once with different values, of which the tool silently took the first.
The screen says so. The downloaded `.fields.csv` shows those rows as
`matched TRUE, flagged FALSE` with **no `conflict` column at all**, because
`write_form_outputs()` selects a fixed seven columns (R/forms.R:135-136) and
`conflict` - produced at R/extract_fields.R:60, counted at R/forms.R:204, and
used to DECIDE THE STATUS at R/forms.R:206 - is not among them. The JSON is built
from the same frame and drops it too.

There is no reconciliation behind a form. `conflict` is the entire quality
signal, and it exists only on a screen that gets closed.

Also: a form value carries **no page number**. `match_label()` flattens every
page into one string. The report route's pairs already carry `page` and
`found_by` - so this is a parity gap inside one product, not a hard problem.

**Severity: wrong-figure.**

**Fix:** add `conflict` and `n` to the tidy column list and mark a conflicting
value visibly, not just as a boolean. Carry the page through `match_label()` the
way `doc_pairs()` does.

## G3. A report read into the wrong columns comes out `ok`, and the evidence never leaves memory

Re-running the shipped fixture with page 2 shifted 120pt (the crux, section 0):

| | |
|---|---|
| status from the gate (R/doc_extract.R:521-523) | **`ok`** - not needs_review |
| confidence | 1.00 |
| `Opening` column (declared `money`) | `Transaction`, `Savings`, `TermDeposit`, `CreditCard` |
| `Closing` column | the OPENING balances |

Three things the engine already knows and does not use:

1. **`unclaimed_words` does not gate the status.** It was 4. The gate looks only
   at empty/thin/weak. The *screen* does colour the verdict on it (app.R:6339),
   so the analyst gets a warning the record and the file never receive.
2. **The unclaimed words themselves are computed and thrown away.** `spilled` is
   built at R/tables.R:1187 and returned carrying `page, y, x, text` - in this
   run, exactly the four real closing balances. The only reader anywhere is
   `nrow()` at R/doc_extract.R:316. **That frame is the single best piece of
   evidence this route can produce and it never leaves memory.**
3. **A typed column that parsed nothing is not counted.** `Opening__value` was NA
   for 4 of 4 rows because "Transaction" is not money, yet that column reports
   `fill_rate 1.0, low_fill FALSE` - `.doc_col_report()` measures fill on the raw
   text and never looks at the `__value` companions. A free, already-computed
   signal that catches wrong-column reads, A3's swallow, and any band drift.

And where evidence does exist it reaches one sheet of one optional file:
`found_by`, `confidence`, `unclaimed_words` and `pages` are written **only** into
the xlsx Report sheet (R/doc_extract.R:443-445). On a box without openxlsx the
code takes the CSV-per-table path and **the summary is written nowhere at all** -
measured. The comment at R/doc_extract.R:413-414 says "nothing is dropped for
want of a package"; the one thing unique to this route is. That sheet is also
unreadable as an exhibit: two frames of different widths are stacked into one.

One more: `.doc_page_words()` reads a page whose orientation differs from the
template's frame **unscaled** (R/tables.R:110-112). That is the right call, and
it is silent - `same_way` has no reader anywhere. A page read under a different
geometry rule from the rest of its own document carries no mark anywhere.

**Severity: wrong-figure.**

**Fix:** put `unclaimed_words` and a typed-column parse-failure rate in the status
gate. Write `spilled` as a fourth output (`.unclaimed.csv`) - it is already a
data.frame and costs one `write.csv`. Move the per-table summary into
`.report.csv` so it survives a missing package. Note when a page was read
unscaled.

## G4. Report and form workbooks change bytes on every run; statements do not

Same input, two writes three seconds apart:

```
STATEMENT   xlsx SAME        csv SAME   json SAME
FORM        xlsx DIFFERENT   csv SAME   json SAME
REPORT      xlsx DIFFERENT   (the CSVs SAME)
```

`docProps/core.xml` inside the report workbook carries the wall clock.
`write_outputs()` pins it and normalises the ZIP mod-times before saving
(R/outputs.R:186-188); `write_document_outputs()` (R/doc_extract.R:446) and
`write_form_outputs()` (R/forms.R:146) call `saveWorkbook` raw. The suite's
byte-reproducibility test exercises `write_outputs()` only, and
`docs/design.md:478` records the guarantee as "yes".

So "re-run it and show it produces the same file" - the check a maintainer would
run to prove the tool has not drifted - fails on both other routes for a reason
that has nothing to do with the figures. That is the worst kind of false alarm.

**Severity: blocks-work.**

**Fix:** route both writers through the same two lines the statement writer uses,
and extend the reproducibility test to all three so it cannot regress again.

## G5. Two unversioned files decide what every figure parses to

`.value_from_line()` - the money/date parser behind every `__value` column and
every money/date field on every form - takes its patterns from the lexicon AT RUN
TIME (R/labels.R:118). `dictionaries/lexicon.yaml` is Admin-editable and a
`regex` category REPLACES the built-in (R/lexicon.R:31-33). Separately
`convert_form()` passes `default_label_dict()` from `dictionaries/labels.yaml`,
also Admin-editable, which decides which labels a field matches at all.

Grep for `lexicon_sha|dict_sha|dictionary_sha|lexicon_version` across `R/` and
`app.R`: **nothing**. `template_sha256` proves which TEMPLATE ran; nothing proves
which VOCABULARY ran. Add a wording in June and the same PDF re-run in September
extracts different values, with `engine_version` and `template_sha256` both
unchanged and both saying the run is identical.

**Severity: wrong-figure.**

**Fix:** hash both files at load (`.text_sha256()` exists, R/convert.R:35), cache
beside `.VERSION_CACHE`, stamp `lexicon_sha256` and `dictionary_sha256` into the
run log and the `build` block next to `template_sha256`.

## G6. The other routes' CSVs are not formula-guarded

Measured: a cell reading `=1+1` reaches `doc.tables-long.csv` verbatim; a pair
value reading `=HYPERLINK(...)` reaches `doc.values.csv` verbatim; same for
`.fields.csv`. Excel evaluates all of them on open. The statement path
neutralises exactly this (`.neutralize_formula` / `.spreadsheet_safe`,
R/outputs.R:105-115, applied at :161 and :173). Neither other writer calls it.

The point for this lens is not the injection - it is that **the reviewer's screen
shows `2` where the document printed `=1+1`**. A wrong figure that looks right,
delivered by the tool's own output. (The xlsx is safe; the exposure is the CSVs,
which is what people open.)

**Severity: wrong-figure.**

**Fix:** one call each, on the CSV writers only, applied to every character
column since a report has no fixed schema. Leave the JSON verbatim, as the
statement path does.

## G7. Redactions are honoured on both other routes; the ACCOUNTING is statement-only

Good news first, and worth not re-investigating: the guard is inside the reader,
not the statement pipeline. `read_input()` -> `read_pdf()` applies the marker
sweep, the vector-occlusion scan (R/read_pdf.R:353-375) and the OCR dark-box
detector (:329-347), and both other routes call `read_input()` (R/forms.R:171,
R/doc_extract.R:492). Measured: `Account XXXXXXXXXX` -> `Account [REDACTED]`,
`redacted_words = 1`. **A report does not leak a redacted name through the text
layer.**

What is statement-only is the accounting. `input$meta` carries six forensic facts
- `redactions`, `redaction_scan_incomplete`, `ocr_pages`, `ocr_min_conf`,
`scanned_no_ocr`, `pdf_doc` (R/read_input.R:176-193). Grep for any of them in
R/forms.R, R/doc_extract.R, R/tables.R: **not one hit**. So a page that could not
be rasterised FAILS a statement's KPI (R/reconcile.R:546-553, "text hidden under
a redaction box may have been read and emitted") but on a report produces an R
`warning()` to a console nobody reads, status stays `ok`, and no file mentions
it. A report where the redaction scan could not run is indistinguishable from one
where it ran clean.

(Smaller: `convert_document()` has no `redaction_rects` parameter at all
(R/forms.R:266-273) and app.R never passes one, so the manual-rectangle channel
is unreachable from the GUI on all three routes.)

**Severity: wrong-figure** - an unverified redaction that reads clean is a
disclosure incident.

**Fix:** the six `input$meta` facts want a route-independent "how this file read"
block, computed once and written by all three writers and into all three records.
`redaction_scan_incomplete > 0` then demotes any route's status.

## G8. A form or report run record is the statement pass's verdict plus two counts

| field | statement | form | report |
|---|---|---|---|
| `template_sha256` | real hash | **NA** | **NA** |
| `template_version` | template's | template's | **always 1** (G1) |
| `trust_level` | ok/medium/low | **NA** | **NA** |
| `row_count` | rows | **literal 0** | **literal 0** |
| `kpi_fail_count` | real | **literal 0** | **literal 0** |
| `closest_template` / `detect_detail` | the verdict | *carried over from the failed statement pass* | *carried over* |
| `period_start` / `period_end` | real | *carried over* - measured "1 April 2025" off a form | *carried over* |
| `n_values`, `n_conflicts`, `required_missing` | - | **absent** | - |
| min `confidence`, `empty/thin/weak_tables`, `unclaimed_words` | - | - | **absent** |
| the output files written | absent | absent | absent |

Two consequences that generalise. `kpi_fail_count = 0` and `row_count = 0` are
LITERALS, not NA, so every report and form run is counted in Admin's per-template
reports as a clean zero-failure conversion alongside statements that genuinely
reconciled. And the numbers that decide the other routes' status are computed and
discarded - a table found by position at confidence 0.40 reaches the record only
as prose inside a truncated message string.

`logs/metadata/<run_id>.json` - kept **forever** (R/config.R:94) - describes the
FAILED STATEMENT ATTEMPT for these runs: `status: "unsupported"`,
`template_id: null`, no `kind` field at all. So the forever-corpus that
`R/suggestions.R` mines for "build these next" cannot tell a genuinely
unsupported layout from a report that converted perfectly. The same bug was fixed
for the run log (findings-register #15) and left standing here.

**And it answers the open question in 0b:** `batch_audit()` takes `templates` and
`fields_templates` only (R/batch_audit.R:35-36) - no document-template argument,
no `mode:document` branch. A folder of reports fully covered by report templates
comes back "unsupported" and inflates the gap count. So Batch & audit **cannot**
serve the library check as it stands.

**Severity: blocks-work.**

**Fix:** give each route's own numbers named fields rather than prose; write NA,
not 0, where a measure does not apply; add `kind` to the metadata record and
clear the carried-over detection fields when the route changes; record the output
filenames so a file and a record tie together in either direction.

## G9. The PII gate guards one field; verbatim page text leaves by two others

`.fp_has_pii()` (R/wizard_auto.R:87) is called from exactly one place,
`.fp_fingerprint_problems()` (R/templates.R:54). Measured on one string:

```
as a fingerprint phrase : REFUSED - "'Prepared for Mr John Smith' names a PERSON"
as a table title        : (no problems - the template saves)
as a pair label_text    : (no problems - the template saves)
```

The builder's FIRST gesture is "drag a box round the table's TITLE", and A2b is
about to make those titles the primary search key with variant lists dragged off
the page - which multiplies this surface rather than shrinking it. Saved report
templates live in `templates/documents_user/` and are what gets copied off the
box.

The larger exit is `layout_signature()`. When no line matches >=2 header keywords
it falls back to **the twelve most frequent 4+-letter non-stopword words on the
page** (R/layout.R:52-57). Measured on a page whose commonest words are two
surnames: `layout_hint` came back as **`ambrose | whitcombe`**. That is written to
`logs/runs/<run_id>.json` (rolled into an archive nothing ever deletes) AND to
`logs/metadata/<run_id>.json`, whose own module header states as rule 2 "NO RAW
CONTENT / PII-CONSCIOUS... Descriptions, payees, references and raw amounts are
NEVER stored".

This bites the OTHER routes specifically: a bank statement hits the
header-keyword branch and yields `amount | balance | date | description`. A
document that reaches the form or report route is by definition one with no
transaction header - so it is exactly the class that takes the fallback.

On retention: `purge_uploads()` deletes the file and keeps `record.json`
(R/retention.R:74-83), which keeps `file`, the filename AS SENT - routinely
"SURNAME, First - Bank - period.pdf". The run record keeps `source_file`,
`period_start`, `period_end` and `layout_hint` forever.
`uploads_retention_note()` tells the uploader the file is deleted after N days
and says nothing about any of that.

**Severity: blocks-work** - a disclosure question, not an arithmetic one, but the
kind that ends a case.

**Fix:** run the same `.fp_has_pii()` gate over table names and `label_text` at
save time. Restrict the `layout_hint` fallback to words in the lexicon's
`header_keywords`, or drop the hint for non-statement runs - it exists to cluster
layouts and a surname clusters nothing. Extend the retention note to say what the
record keeps after the file is gone.

## G10. One instant, three clocks

Measured under `TZ=Pacific/Auckland`, all from the same second:

```
upload folder id (R/uploads.R:46,  local) : 20260826193118
run_id           (R/convert.R:226, UTC  ) : 20260826073118
record.json ts   (R/uploads.R:52,  %z   ) : 2026-08-26T19:31:18+1200
```

The two primary handles a reviewer uses to tie an upload to a conversion carry
timestamps **twelve hours apart**, and nothing on either says which zone it is
in. Anyone matching them by eye - which the Admin Uploads table invites - pairs
the wrong rows across the overnight boundary.

And the PDF's own forensic timestamps are rendered in the server's local zone
with no marker (`.pdf_doc_info()`, R/read_pdf.R:477). Same file, three zones:
`04:12:49` / `16:12:49` / `00:12:49`. "The PDF says it was last modified at
16:12" does not survive the question "where?".

The run log, feedback, uploads and requests use local time with `%z`; the feed
and metadata capture use UTC with `Z`. Both are defensible; having both in one
evidence trail is not - and a string sort of `ts` misorders the one hour a year
when the clocks go back.

**Severity: blocks-work.**

**Fix:** one rule - every timestamp WRITTEN is UTC with a `Z`, every timestamp
SHOWN is local with the zone named. The upload id and the run id then agree by
construction. Append the zone to the two `.pdf_doc_info` timestamps.

## Already fine - do not re-investigate

* **Statement outputs ARE byte-reproducible.** Measured: xlsx, csv, json all
  identical across runs. R/outputs.R:186-188.
* **The statement workbook carries its own provenance**, including per-row
  `source_ref` (`csv:line=2`, R/parse.R:245; `pdf:p%d`, R/parse_pdf_table.R:1243).
* **Redactions ARE honoured on both other routes** - see G7.
* **The feed cannot be reached by a form or a report.** R/feed.R:248, the first
  substantive line of `write_feed`.
* **The builder never persists example cell values.** `d$anchor$first_column <-
  list()` on save (app.R:3656).
* **The report route's pairs already carry their evidence to the file** -
  `values.csv` has `pair,label,value,raw,page,found_by,matched`.
* **Metadata capture stores no raw transaction content** - accounts hashed,
  amounts bucketed, descriptions reduced to length statistics
  (R/metadata_capture.R:44-79). `layout.hint` is the exception, see G9.
* **Every Convert records an upload regardless of route**, and `purge_uploads`
  keeps the record stamped `purged`.
* **The upload path is hardened** against traversal (R/uploads.R:26-31, :119-121).
* **`run_id` is reproducible across hosts and collision-safe**
  (R/convert.R:224-227).
* **The log rollup loses nothing** (R/retention.R:24-37, :133-150).

## Questions only the owner can answer

1. **Should a report or form output be downloadable while its template has no
   content hash?** (a) always write it and stamp "not recorded"; (b) refuse until
   the hash is written; (c) write it and mark the file unverifiable.
   **Recommend (a)** - refusing takes work away from Beth for a defect she did
   not cause, and once G1 lands the case disappears.
2. **How long should `logs/archive/` and `logs/metadata/` keep a record naming
   the client's file, period and page words?** Uploads are purged after
   `uploads_keep_days`; these two are forever. (a) forever; (b) same as uploads;
   (c) a separate stated retention. **Recommend (c)**, defaulted long but STATED
   in `uploads_retention_note()`, so what is promised at upload is what happens.
3. **Should unclaimed words demote a report to `needs_review`?** (a) any word;
   (b) a deployment-settable threshold; (c) leave the status, add a column.
   **Recommend (b)**, defaulting to "more than one" - measured 4 on the
   wrong-column case and 0 on both correct reads, so a low bar is not noisy.
4. **What should `layout_hint` say for a non-statement?** (a) the raw twelve
   words; (b) only words in `header_keywords`; (c) drop it, keep the signature
   hash. **Recommend (b)**.
5. **Should the download name carry the run id?** (a) leave it; (b) append the
   run id; (c) append a short hash. **Recommend (b)** - it is the only handle
   tying a file in a bundle back to a record, and it is the question asked two
   years later.

---

# H. Building and maintaining ONE template across many documents

The admin mode: a template holding 40 tables that never all appear in one copy,
grown from many examples over months. Everything below was measured.

**The headline: a table that is NOT in this document does not come back empty -
it comes back full of the table next to it.** A 35-table family template run on a
document containing one table returned 34 copies of that table's row, each under
a different table's name and columns, in 34 worksheets, with `empty` counting
zero and the workbook looking complete.

### H1. An absent table is fabricated from whatever sits at its old coordinates
`doc_locate_table()` has no "not found" outcome. Its ladder is header ->
first_column -> **position**, and the last rung always succeeds: `p0` is clamped
into range (R/tables.R:443) and a window opens regardless (R/tables.R:484-491).
`empty` was 0, so `convert_tables` reported nothing wrong.
**wrong-figure.** *Fix:* a fifth outcome, `anchor = "not_found"`, returned when
no title/header/first-column evidence exists anywhere; a table found ONLY by
position refused outright in family mode; and "expected but missing" separated
from "optional and absent".

### H2. A declared page the document does not have clamps to the last page
`p0 <- min(p0, max(npg, 1L))`. A template built on a 12-page example, run on a
2-page document, reads every table off page 2 - **including tables that ARE
present on page 1 and would have matched by heading**.
**wrong-figure.** *Fix:* a declared page past the end aborts that table and says
so, rather than silently substituting the last page.

### H3. The "different length document" safeguard is dead code
`doc_locate_table` grades a positional match 0.4 or 0.2 on
`identical(npg, .doc_int(tab$doc_pages, npg))` (R/tables.R:486) - reading
`doc_pages` off the TABLE. `document_template_from_proposal` writes it at the
template ROOT (R/doc_extract.R:679). So `tab$doc_pages` is always NULL, the
comparison is always TRUE, and the 0.2 tier can never be reached.
**wrong-figure.** *Fix:* read it from the template with a per-table override, and
test that the 0.2 tier is reachable. Worth sweeping for other write-here/read-
there pairs - the same class already bit `band`.

### H4. Loading a second example re-stamps the template's reference page size
`rb_frame()` reads page 1 of whatever is open and `rb_template()` overwrites
`ref_width`/`ref_height` with it (app.R:4193). Open an A4 template, upload a US
Letter example to add table 41, Save: the file says 612x792 while every band is
still in 595x842 space and was never converted. Measured: a word at x=400,y=700
then moves 11pt across and 41pt up.
**wrong-figure.** *Fix:* the frame is part of the template - restore it on open,
do not overwrite it while editing, and convert bands rather than relabelling them.

### H5. There is no "add tables from a second example"
`observeEvent(input$ts_file, { rb_editing(NA_character_) })` (app.R:2746) clears
the editing flag the moment a second document is uploaded, which un-blocks the
seeding observers: the issuer is overwritten from the new FILENAME and the
fingerprint box is overwritten wholesale.
**blocks-work.** *Fix:* make "another example of the same template" a first-class
action distinct from "a new document". A new upload changes only the PAGE.

### H6. Every open-and-save round trip loses parts of the template
The register lists this under "Done today" as identical. **It is not.**
`document_proposal_from_template` -> `document_template_from_proposal` keeps only
a whitelist, so a template opened and saved loses `hidden: true` (a parked
template silently returns to live detection), `notes`, `version` (7 becomes 1 -
hardcoded at R/doc_extract.R:678), and per-table `row_tol` and `max_gap`.
**wrong-figure.** *Fix:* round-trip by MODIFYING the loaded template rather than
rebuilding it; keep the original list, replace only tables/pairs/fingerprint,
bump version. Test that an arbitrary template survives unchanged.

### H7. A template cannot be tested against a folder of examples
`batch_audit(paths, templates, max_recommendations, fields_templates)` -
R/batch_audit.R:35. No document-template parameter, `detect_document_template`
never called. The Admin job passes even less (R/jobs.R:430-432).
**blocks-work.** *Fix:* one admin action - pick a template, pick a folder, get a
grid of document x table showing rows and how each was found.

### H8. A table titled "Report" or "Values" destroys the whole workbook
`write_document_outputs` adds its own sheets with those names after the table
sheets (R/doc_extract.R:434-441). A table called either - or two differing only
in case - makes openxlsx refuse, the `tryCatch` returns FALSE, and the path is
silently never added. The CSV fallback is gated on `!have_xlsx` so it does not
run either.
**wrong-figure.** *Fix:* reserve the reader's own sheet names, make uniqueness
case-insensitive, and say so when the workbook fails.

### H9. Two admins, one server: last save wins, blindly
`save_document_template` is `yaml::write_yaml(t, path)` (R/doc_extract.R:199) -
no lock, no compare-against-disk, no copy kept. All three savers are the same
shape. The app knows how to do this properly elsewhere (R/logging.R:24-38).
**blocks-work.** *Fix:* keep the previous file, stamp who and when, bump version,
and compare the on-disk version against the one that was opened.

### H10. When two templates both match, NEITHER is used
`detect_document_template` picks the most phrases; a tie returns
`matched = FALSE`. Two good one-phrase templates that both match means
"unsupported" on a document the library can read. Nothing checks for it at save
time.
**blocks-work.** *Fix:* break ties by evidence - prefer the rarest phrases - and
when genuinely ambiguous ASK. Check the fingerprint against the whole library at
save time.

### H11. Admin reports every report template as a duplicate of every other
`.template_shape` builds its signature from statement keys only, so every
document template collapses to `pdf~~~~~~` and `duplicate_template_groups` puts
them all in one group.
**harder-screen.** *Fix:* a branch per kind. (Partly mitigated already - the
dupes panel now says "bank statement templates" - but the grouping is still wrong.)

### H12. A 40-table template re-reads every page from scratch for every table
No cache anywhere. Measured on 30 pages: 2 tables 0.75s, 10 tables 1.37s, 40
tables 5.6s - tolerable only because each table looks at ONE page. A2's sweep
projects to 13.2s.
**blocks-work.** *Fix:* cache page lines per (page, frame, row_tol) for one
`extract_document` call. **Do this BEFORE A2 lands, not after.**

### H13. "Edit" removes a saved table, and the button beside it says "Throw it away"
Edit runs `rb$tables <- rb$tables[-i]` (app.R:3694-3696); the only other button is
`rb_cancel`, labelled "Throw it away" (app.R:854), which nulls the draft. One
click, no confirmation, no undo - table 27 gone from a template that took months.
Also `.RB_MAX_ROWS <- 60L` caps how many Edit/Remove observers exist while the
list renders one pair per table with no cap.
**blocks-work.** *Fix:* Edit COPIES into the draft and leaves the saved table in
place; cancel says "Cancel - keep the saved table"; removing needs a confirm.

### H14. A template cannot say where any of its 40 tables came from
No provenance in the saved shape. `origin` is only "default"/"user" and is
stripped on save.
**blocks-work.** *Fix:* two stamps per table, written by the builder and
preserved by the round trip - `drawn_from` (example filename + date) and
`last_seen` (updated whenever a conversion finds it by title or header).

---

# I. Document shapes the engine cannot represent

> "The reader can never say *this is not here* and never say *this is not it* -
> both ends of the pipe are open, and every silent wrong figure comes through one
> of them."

### I1. An unlabelled total is folded into the row above and REPLACES its value
`.doc_is_wrap()`'s indented branch has no guards: a right-aligned figure alone on
a line is "indented" by construction. Measured: a 3-row schedule with an
unlabelled total of 6,000.00 came back with 3 rows and `Value` =
`"3,000.00 6,000.00"` - and `.value_from_line` takes the FIRST match, so the
parsed value is the row's, not the total's. **The exact mirror of A4.**
**wrong-figure.** *Fix:* give the indented branch the same guards the un-indented
one has, plus: a wrap NEVER contributes to a money/number/date column that already
has a value.

### I2. A comparative table that gains a period column shifts every figure across
A table right-anchored to the page (Q1..Q4, Total) that gains Q5 pushes every
older column left. Measured: `Q1` held Q2's figure, `Total` held Q5's. **Every
signal said perfect** - rows 4, unclaimed 0, low_fill FALSE on all six,
anchor header, confidence 1.00, status ok. Not fixed by section 0's shift, which
is a uniform translation; this is a change of column COUNT.
**wrong-figure.** *Fix:* call the existing `doc_header_names()` on THIS document
and compare to the template's names through `.doc_norm()`. Any position that
disagrees is a band no longer over the column it was drawn for.

### I3. No cell is ever checked against its column's declared kind
`fill_rate` asks only whether a cell has ANY text. Measured: a `money` column
whose band landed 60pt left held "Legal fees", "Court filing", "Disbursements" -
reported `filled 3, fill_rate 1.00, low_fill FALSE`, unclaimed 0, confidence
1.00, status ok. Yet `Amount__value` was NA in all three rows: **the engine
already knows not one cell parsed as money and discards it.**
**wrong-figure.** *Fix:* two columns on `.doc_col_report` for any typed column -
`parsed` and `multi_token`. A money column at parse rate 0.00 is a band failure,
not a thin column. **This one guard catches A3, section 0, band overflow, a
column drawn over the wrong ink, and OCR damage.**

### I4. A real row is deleted whenever it echoes the header wording and carries no figure
The reader drops any line scoring >= 0.6 on the header wordings with no money
token (R/tables.R:1091-1095). Measured: a fee schedule with a genuine row
"Fee | basis under review" was deleted - rows 3 where the document prints 4,
fill_rate 1.00, confidence 1.00, `document_summary()` shows nothing. Bites
hardest on tables with no decimals, because `.MONEY_RX` needs two decimal places
or a `$`.
**wrong-figure.** *Fix:* restrict the mid-table wording rule to a repeated header
at the TOP of a continuation window, not anywhere in the body. And surface
`n_header` - a table that dropped more header lines than `header_rows * pages`
has deleted something.

### I5. `__value` is not actually parsed - `parse_amount()` is never called
`.doc_cell_value()` runs `.value_from_line()`, which returns the matched
SUBSTRING verbatim. Measured: `(1,234.56)` -> `(1,234.56)`; `987.65-` ->
`987.65-`; `55.00 CR` -> `55.00 CR`; `12.34 DR` -> `12.34 DR` - **no sign is
resolved**, so a bracketed negative is indistinguishable from a positive. Two
currencies collapse: `A$2,000.00` and `US$4,000.00` both -> `$...`.
**wrong-figure.** *Fix:* call `parse_amount()` - the correct parser is one file
away and already carries the statement route. Record an undeclared currency
marker rather than stripping it; `currency: mixed` forces needs_review.

### I6. Date columns have no format, no ISO support and no year bound
`.DATE_RX` cannot start at a 4-digit year. Measured: `2025-04-07` -> `25-04-07`,
which reads as 25 April 2007 or 1925 depending on who opens it. `Apr 2025` -> NA
with no message.
**wrong-figure.** *Fix:* the statement route has all the guards already
(`date_format`, `.date_strict`'s round-trip and year bound) - use them.

### I7. A merged header spanning two columns cannot be described
**wrong-figure.** *Fix:* needs a template shape for it, or an explicit refusal.

### I8. A page break MID-ROW always produces an orphan and truncates the row it broke
**wrong-figure.**

### I9. Two tables side by side on one page cannot be described as two
Their title and header anchors are the same LINE. **blocks-work.**

### I10. A table continuing in a second column of the SAME page reads as one wide table with half the rows
**blocks-work.**

### I11. A table with no header row at all is permanently `needs_review`
...and `validate` accepts a table with no way to find itself. **blocks-work.**

---

# J. Beth on a bad day - driven in a real browser

### J1. A hidden "force this exact template" silently overrides "this is not a statement"
Browser-proven. Beth opens "It picked the wrong bank?", picks a template, then
ticks "Something else". The disclosure is hidden by `conditionalPanel` (app.R:444)
- **but hiding does not clear**, the selection survives, `tpl_choice()` still
reads it, and `convert_document` turns a forced template into `kind = "statement"`
outright.
**blocks-work.** *Fix:* clear `cv_template` and `cv_bank_quick` when `cv_kind`
becomes "other". A forced statement template plus an explicit "not a statement"
is a contradiction that should be said out loud.

### J2. A template that fails validation vanishes and NOTHING says so
All three loaders collect skip reasons on `attr(x, "load_errors")` and the
comments promise it is "never SILENT". **`app.R` never reads the attribute once.**
Proven end to end: a report template failing the fingerprint gate produced
"unsupported" with no hint the file existed.
**blocks-work.** *Fix:* render it at the top of Admin -> Templates and write a
census line into `startup.log`. The data is already computed and thrown away.

### J3. "Why didn't my template fire?" has no answer on the other route
`detect_document_template` returns three details and never names the near-miss,
never names which phrase was missing, and never distinguishes zero templates from
zero MATCHING templates. Hiding the one report template reports "no document
templates are installed".
**blocks-work.** *Fix:* score every template the way the statement detector does
and carry the best near-miss back in `detail`.

### J4. There is no route from a converted report back to the template that read it
`cv_teach` contains a branch written for exactly this (app.R:7610-7613) - but
`uiOutput("cv_teach")` sits inside a conditionalPanel requiring
`output.cv_is_tables != true` (app.R:527-529). **Dead code on the route it was
written for.**
**blocks-work.** *Fix:* move `cv_teach` out of the statement-only panel - it
already branches on kind internally.

### J5. Half a template exists only in browser memory
`rb` is a plain `reactiveValues`. No `saveRDS`, no bookmarking, no autosave
anywhere. `session$onSessionEnded` deletes the scratch folder holding the PDF too.
And Save is all-or-nothing - `validate_document_template` refuses a partial.
**blocks-work.** *Fix:* a saved DRAFT that is allowed to be invalid; validation
gates PUBLISHING, not writing anything down.

### J6. A case folder: every converted report reads "0 rows, no bank, no confidence"
`.rows_of()` (R/batch.R:68-72) special-cases forms but sends everything else to
`run_log$row_count`, which `convert_document` deliberately sets to 0 for a report.
A report that read 83 rows looks identical to one that read nothing.
**harder-screen.** *Fix:* a kind-aware "how much came out" and "what to check".

### J7. Thirty files, three failed: no way to re-run just those three
`cv_batch` is `selection = "single"` and `cv_batch_open` renders only a heading.
No batch downloadHandler exists. The deliverable for a 30-file case is 30
row-clicks and 30 separate downloads.
**blocks-work.** *Fix:* multi-select plus "Convert these again" and "Download
everything". The re-run path already exists.

### J8. The engine's own high-severity diagnosis never reaches the headline
Driven with no tesseract and an image-only PDF: the card says "no template for
this layout", the primary green button says "Set it up as a report", and the
diagnostics table further down says correctly "this machine has no OCR software
installed... building a template will NOT help until that is done."
**blocks-work.** *Fix:* a `severity == "high"`, `fix_owner == "file"` diagnostic
takes the headline and REPLACES the primary action.

### J9. The OTHER route ships empty
`templates/documents/` contains one file - README.md. Zero report templates ship,
so on a fresh install the route cannot be demonstrated, learned or smoke-tested.
The sample button is a single ANZ CSV, and `cv_empty` only ever describes
statements.
**harder-screen.** *Fix:* ship one synthetic report and a template for it, wire a
"Try it on a sample report" button, and branch `cv_empty` on `cv_kind`.

---

# K. Running it offline for years

**The register contained NOTHING about deployment, updates, backups, concurrency,
disk, health or time.** All of this is beside it, not in it.

### K1. The Qlik feed stamps UTC while every other clock is local
Measured in Pacific/Auckland: one conversion at 09:30 NZST appears as
`startup.log` "09:30:00" (no offset), `logs/runs` "09:30:00+1200", and feed
`converted_ts` "2026-08-25T21:30:00Z" - **the day before**.
**wrong-figure.** *Fix:* one timestamp helper in R/util.R used by every writer.
Rename the feed column `converted_ts_utc` if it stays UTC.

### K2. Nothing can tell you an update changed what a template READS
Golden snapshots exist only for the six SHIPPED statement templates. There is no
golden for any user-built, form or report template. "Run the suite after every
update" proves the repo's six layouts, not the forty the team built.
**wrong-figure.** *Fix:* keep the example's sha256 and a small snapshot of what it
produced beside each template; one button re-runs every template against its own
recorded example.

### K3. A one-letter typo in the feed config stops the dashboards, reported in green
Config validation covers five BOOLEANS only. `.trust_ok('medium','meduim')` is
FALSE and `allowed_template_origins: [Default]` (capital D) returns
`withheld:not_proven`. Both fail closed - correct - but silently, and Admin marks
the run 'handled as intended'.
**wrong-figure.** *Fix:* extend config coercion to enums the way it handles
booleans; an unrecognised value keeps the default and is reported.

### K4. Templates are saved with a bare `write_yaml` - no backup, no atomic write
All three savers. The DICTIONARIES get a `.bak` on every save; templates - which
`backup-and-restore.md` calls "the accumulated value of the tool" - get nothing.
**blocks-work.** *Fix:* one `save_yaml_safely()` in R/util.R (copy to `.bak`, then
write-temp-then-rename) used by all three savers, plus "Undo the last save".

### K5. There is no health check an operator can run after an update
Nothing on the box answers, in one command: did config.yaml parse; is the admin
password still the placeholder; how many templates loaded and how many did not.
**blocks-work.** *Fix:* `scripts/health-check.R` - pure R, PASS/FAIL, non-zero
exit. Same code behind an Admin "Check this server" button.

### K6. The builders read and OCR documents in the one Shiny process
R/jobs.R exists because "the engine call inside the Convert observer froze EVERY
other analyst's browser - measured at 65 seconds of a dead page". Convert was
moved to child processes. **The builders were not** (app.R:7512, 7553-7565,
2803-2806).
**blocks-work.** *Fix:* a third job task ("read") through the existing
`job_slot()`/`job_seed_input_cache()` pair - the hand-back is already built.

### K7. Admin/Insights does unbounded synchronous work in the one process
`read_runs_all()` parses every line of every archive file with `fromJSON` one at a
time, automatically, the moment an admin opens the tab. Measured: 20,000 archived
rows took 10.5 seconds - 13 months at 50 conversions a day.
**blocks-work.** *Fix:* roll up "feed" too, read the archive pre-summarised or
bounded, and run `load_admin` through the existing job slot.

### K8. Two of four scratch prefixes are never swept
`sweep_temp_dirs` defaults to `c("cv_", "ts_")`. The app makes four kinds:
`cv_`, `ts_`, **`cvb_`** (the 50-file CASE folder, the biggest) and **`ba_`** (the
Admin bulk audit). `cvb_*` does not match `cv_*`. Real client statements pile up
in `%TEMP%` for the life of the server, and nothing reclaims a dead process's
folder.
**blocks-work.** *Fix:* one named constant used by both `tempfile()` and the
sweep; sweep the PARENT of `tempdir()` at startup.

### K9. A run log that cannot be written is swallowed
`log_run` wraps `write_log_record` in `safe()` and both callers wrap it again.
Proven with an uncreatable logdir: no file, nothing propagated. On a full disk or
a vanished UNC path, **every conversion still succeeds and produces a complete
workbook with no audit record, and nobody is told.**
**wrong-figure.** *Fix:* `log_run` returns the path or a reason; Convert says
"this conversion was NOT recorded in the audit log - <reason>".

### K10. `feed\`, `logs\feed\` and `logs\errors.log` grow without bound
One file per statement, never removed. Qlik loads with a wildcard, so at 50/day
the reload walks ~18,000 CSVs after a year and ~55,000 after three. No
operational doc mentions disk at all.
**harder-screen.** *Fix:* add "feed" to the rollup, trim `errors.log`, give
`feed\` a stated archive rule and say it in `connecting-qlik.md`.

### K11. Two operational pages document a screen behaviour that is no longer true
`updating-a-version.md:80` and `backup-and-restore.md` both tell the operator to
check document templates by uploading a report "because Admin -> Templates does
not list them". **Admin now lists all three kinds.** Also
`templates\statements\` - where promotion writes - is in no backup list.
**harder-screen.** *Fix:* fix the three sentences, add the folder to the
irreplaceable list, and assert both in `test-deployment-docs.R`.

---

# L. Statement / other parity

### L1. A form or report template cannot be forced, anywhere in the product
`convert_form(template_id=)` and `convert_tables(template_id=)` both exist and
**nothing in the app ever passes them.** `convert_document`'s only override turns
into `kind = "statement"` outright.
**wrong-figure.** *Fix:* one `template_id` that travels the whole way.

### L2. Two report templates that both fit = NOTHING converts, and nobody is told which two
Measured with two valid templates over one fixture, both phrases genuinely
printed. Status "unsupported"; it does not name either template.
**blocks-work.** *Fix:* give `detect_form`/`detect_document_template` the same
return shape `detect_statement` has (candidates, tied, margin, runner_up) and the
same resolution - pick the best, convert, mark needs_review, name the tie.

### L3. A template that ALMOST matched says nothing at all
The statement route says "closest anz_everyday_pdf score 1/3 (missing
'Transaction type and details', 'Deposits')". The other route says "no document
template's identifying phrases were all found" - no candidates, no detail_plain.
**blocks-work.** *Fix:* score fractionally, keep all-must-hit as the eligibility
gate, and return the same frame. The wording generator already exists.

### L4. Admin's batch audit reports a perfectly-converting report as a gap
Measured: the fixture converts as ok, 6 tables, 102 rows. `batch_audit()` on the
SAME file returns `detected=FALSE, template=anz_everyday_pdf,
status="unsupported", n_rows=0` - clustered as a layout gap, consuming one of the
eight draft recommendations, **and the draft it produces is a STATEMENT draft for
a document that is not a statement.**
**wrong-figure.** *Fix:* add `doc_templates` beside `fields_templates`, with a
"report" status beside the existing "form" one.

### L5. Saving a report template neither clears the pickup queue nor guards against shadowing
`g_save` calls `set_upload_status(..., "wizard_saved")`, which is what clears
`needs_pickup`. `rb_save` does none of it, so every document taught as a report
stays on Admin's pickup list for ever.
**blocks-work.** *Fix:* `rb_save` gains the same three lines `g_save` has.

### L6. Admin's drift and usage tables are statement-shaped
`template_drift` defines health as `st == "ok" & kf == 0 & tr != "low"`. For a
report `trust_level` is NA, so `healthy` is all-NA, both percentages are NA, and
the selection returns an all-NA row. **Measured: `adm_drift` renders one row of
`<NA> NA NA NA NA <NA>` for any other-route template with six or more runs.**
**harder-screen.** *Fix:* define health per kind - reconciliation for statements,
"every table found by heading and no unclaimed words" for reports.

### L7. Metadata capture files every successful form and report as an unsupported statement
The run log correctly says kind tables, status ok, 6 tables, 102 rows.
`logs/metadata/<run_id>.json` for that same run says `status: unsupported,
template_id: null` - and it is kept **forever**.
**harder-screen.** *Fix:* give `capture_metadata` a kind and let
`convert_document` overwrite it the way it overwrites the run log.
