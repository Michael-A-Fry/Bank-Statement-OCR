# Report templates (`mode: document`)

Curated templates for **reports** — documents that carry many tables of different
shapes rather than one table of transactions: a position report, a schedule of
holdings, a valuation, a consolidated statement. One template per **kind of
report**, matched by the phrases printed on it.

Built on the app's **Add a template** tab: pick *"Anything else"*, upload one
example, press **Find the tables**, then confirm, name and correct what it found.
The same builder covers a form: its **Values** tab draws label/value pairs, and
one template holds both. Templates saved there land in `..\documents_user\`;
a good one is promoted by moving its `.yaml` here.

**Columns are dividers.** A table's columns tile its width, so what you set is
the lines *between* them: click between two columns to split, on a divider to
remove it, outside the table to widen it. The names are read from the document's
own header row, so splitting a column names both halves without anybody typing.
A gap or an overlap between bands cannot be drawn at all — adjacent columns share
an edge — which is why the two faults that lose or delete a column of figures are
not reachable from this screen.

## How a report template differs from the other two kinds

| | `..\statements\` | `..\fields\` | `..\documents\` (here) |
|---|---|---|---|
| What it reads | one table of transactions | labelled values | many tables + labelled values |
| How it finds them | column x-bands, full page height | the label's **wording** | heading wording first, position last |
| What checks it | a running balance, reconciled | nothing — each value is checked by eye | nothing — fill rates are **measured**, not judged |
| Where the output goes | download **and** the Qlik feed | download only | download only |

## Download only, and why

There is no reconciliation behind a report. Nothing in the tool can tell a right
figure from a wrong one here, so nothing from a report reaches a dashboard —
`write_feed()` (`..\..\R\feed.R`) refuses `kind = "tables"` outright, next to the
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

---

## `schema_version: 2` — recognition rules instead of one example's coordinates

A version 1 template describes **where the table was** on the document it was
drawn on. That works while the next copy paginates the same way, and it stops
working the moment any earlier section is absent or gains a row — because every
`(page, y)` below it moves. Version 2 adds rules that describe **how to
recognise** the table, and keeps the coordinates as evidence rather than as law.

**Nothing changes for a version 1 template.** Both features below are opt-in: a
template with no `schema_version` reads exactly as it did, coordinates and all.
That is asserted in `../tests/testthat/test-doc_structure.R`.

### A section ends where the next section starts

```yaml
schema_version: 2
tables:
  nominated_persons:
    name: Nominated Persons History
    detector:
      headings: [Nominated Persons History]
      heading_match: normalized_exact      # or contains / exact
      header_signatures:
        - [Person, Role, Date]
    # start/end are still here, and are still what finds the table. They are no
    # longer what ENDS it.
```

Turn it on with `schema_version: 2`, or with `boundary_policy:
{end_at_nearest_later_section: true}` on a template you are not ready to
version-bump.

Every section start in the document is then detected **independently**, the
accepted ones are sorted by where they physically landed, and each table stops
immediately before the nearest later start — **whichever section that turns out
to be**. No table names its successor, and no expected order is stored anywhere:
sections may be absent, may repeat, may be last, and a report variant may print
them in a different order. `test-doc_structure.R` runs all six permutations of a
three-section document and demands one answer.

**A heading string alone is never a section start.** A report can print a list of
the sections it holds *no* data for:

```
No Information Held
  Nominated Persons History      <- a value, not a section
  Tax Agent History              <- a value, not a section
```

Each is on its own line and matches its alias exactly, so wording alone would
close three open sections and lose their rows. What separates them is what is
*not* under them: a real section start is followed by its **column header**. So
`header_signatures` is required evidence, and a section that genuinely has no
header must say `detector: {allow_headerless: true}` to be excused it.

### Page furniture

The disclaimer, the confidentiality stamp and the "Page 4 of 44" sit inside
whatever table is open on that page, and no band, end coordinate or fill
threshold can separate them from records — on the page they are all just text
between the boundaries.

Two mechanisms, and the first needs no configuration at all:

```yaml
page_noise:
  lines:                                   # for what is printed only ONCE
    - {match: contains, value: 'The information contained in this report'}
    - {match: regex,    value: '^Page [0-9]+( of [0-9]+)?$'}
    - {match: normalized_exact, value: '*** IN CONFIDENCE ***'}
  recurrence:                              # all optional; these are the defaults
    use_repeated_margin_lines: true
    minimum_pages: 2
    top_margin_ratio: 0.12
    bottom_margin_ratio: 0.18
```

**Recurrence carries no vocabulary.** Furniture repeats: the same line, in the
same margin, at the same height, page after page. That is a fact about the
document, so it works on a report the tool has never seen and never needs another
branch — unlike `.is_footer_noise()` in `../R/parse_pdf_table.R`, which is a
hand-maintained list of footer wordings that every new report grows.

Three tests must all pass before a line is called furniture, and the last two
exist because the first is not safe alone:

| test | why |
|---|---|
| printed on ≥ `minimum_pages` | once is not furniture |
| at the **same height** each time (±4pt) | a row lands wherever its table reached |
| carries **no money** | furniture is a page number and a stamp, never a figure |

The height and money guards are not belt-and-braces. Digit runs are collapsed
before comparing (so "Page 3 of 44" and "Page 4 of 44" are one line), and that
also makes every row of a numeric table normalise to the same string —
`01/02/2024 Payment-17 -45.20` and a different row on the next page both become
`#/#/# payment-# -#.#`. Without those two guards, four real rows were suppressed
from an 83-row table. `test-doc_noise.R` pins that case by name.

The explicit rules are asked **anywhere** on the page; recurrence only inside the
margin it learned from. A statutory paragraph can be printed mid-page; a line
that merely resembles a footer must not be suppressed in the body of a table.

**Nothing is deleted.** Suppressed lines are counted and the count is stated in
the table's own `detail` sentence, because a reader who cannot see what was
withheld cannot audit it.

- The engine: `../R/doc_noise.R`, `../R/doc_structure.R`.
- The tests: `../tests/testthat/test-doc_noise.R`,
  `../tests/testthat/test-doc_structure.R`.
