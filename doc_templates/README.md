# Report templates (`mode: document`)

Curated templates for **reports** — documents that carry many tables of different
shapes rather than one table of transactions: a position report, a schedule of
holdings, a valuation, a consolidated statement. One template per **kind of
report**, matched by the phrases printed on it.

Built on the app's **Add a template** tab: pick *"Anything else"*, upload one
example, press **Find the tables**, then confirm, name and correct what it found.
The same builder covers a form: its **Values** tab draws label/value pairs, and
one template holds both. Templates saved there land in `../doc_templates_user/`;
a good one is promoted by moving its `.yaml` here.

**Columns are dividers.** A table's columns tile its width, so what you set is
the lines *between* them: click between two columns to split, on a divider to
remove it, outside the table to widen it. The names are read from the document's
own header row, so splitting a column names both halves without anybody typing.
A gap or an overlap between bands cannot be drawn at all — adjacent columns share
an edge — which is why the two faults that lose or delete a column of figures are
not reachable from this screen.

## How a report template differs from the other two kinds

| | `templates/` (statement) | `fields_templates/` (form) | `doc_templates/` (report) |
|---|---|---|---|
| What it reads | one table of transactions | labelled values | many tables + labelled values |
| How it finds them | column x-bands, full page height | the label's **wording** | heading wording first, position last |
| What checks it | a running balance, reconciled | nothing — each value is checked by eye | nothing — fill rates are **measured**, not judged |
| Where the output goes | download **and** the Qlik feed | download only | download only |

## Download only, and why

There is no reconciliation behind a report. Nothing in the tool can tell a right
figure from a wrong one here, so nothing from a report reaches a dashboard —
`write_feed()` (`../R/feed.R`) refuses `kind = "tables"` outright, next to the
same refusal for forms. That is enforced in code, not left to convention.

## What it does instead of checking

Because it cannot judge the figures, it measures and reports:

- **How each table was found** — by its heading, by the rows it starts with, or
  by where it sat on the example. The last is the one that goes wrong when a
  document changes, and it is what sends a conversion to *needs review*.
- **The fill rate** of every column and every row. A column below the table's
  threshold is flagged, never dropped; a column marked `may_be_blank` (the middle
  cells of a totals row) is exempt.
- **Every word inside a table's boundary that no column claimed.** A band drawn a
  few points too narrow loses a whole column and leaves every remaining row
  looking perfect — this list is what makes that visible.

## What a table declares

A start `(page, y)` and an end `(page, y)` — positions on a page, not whole
pages, because a table can begin below a paragraph and finish above the next
heading. Columns are x-bands meaningful only between those two points, which is
what lets several tables share one page.

`follow: true` keeps a table going past its declared end while the following
pages repeat its header, for the copy of the document that runs longer than the
example. The extension is always reported.

- The engine: `../R/tables.R` (reading), `../R/tables_detect.R` (proposing),
  `../R/doc_extract.R` (the template, the outputs, the front door).
- The tests: `../tests/testthat/test-doc_tables.R`.
