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

## `shape: matrix` — the table whose columns are filing periods

An ordinary table has named columns and a variable number of rows. A filing-period
matrix is the other way round: the **rows** are measures and the **columns** are
whatever periods this customer has filed for.

```
Filing Period   31-Mar-18   31-Mar-19   31-Mar-20   Total
Return Type     IR3         IR3         IR3
Taxable Income  41,200.00   38,905.00   44,010.00   124,115.00
Refund          (1,204.55)              (880.10)
```

**A template cannot name those columns.** A drafted one did — it wrote
`31-Mar-18` in as a permanent column name — and that template is wrong for every
customer with a different filing history and for the same customer next year.
The years are *data*, read off the document every time and never stored.

```yaml
tables:
  individual_income_return:
    name: Individual Income Return
    shape: matrix
    start: {page: 1, y: 80}          # still where to begin looking
    end:   {page: 5, y: 812}
    matrix:
      header_anchor: [Filing Period] # the wording that DOES hold still
      row_key: {name: measure, x_min: 0, x_max: 250}
      periods:
        type: date
        formats: ["%d-%b-%y", "%d-%b-%Y"]
        x_region: {x_min: 250, x_max: 700}
      summary_column: {aliases: [Total, Totals], optional: true}
      output: {name_column: filing_period, value_column: value}
      blank_rows: drop               # or keep
```

Everything to the right of `header_anchor` is derived. Three periods or twelve,
2018–2020 or 2021–2023 — same template.

### The output is long, and that is the point

One row per (measure, period), so every report this will ever read has one schema:

| measure | filing_period | value | value\_\_value | is_summary | page |
|---|---|---|---|---|---|
| Taxable Income | 2018-03-31 | `41,200.00` | 41200 | FALSE | 1 |
| Refund | 2018-03-31 | `(1,204.55)` | -1204.55 | FALSE | 1 |

A wide sheet would have a different column set per report, so two customers could
not be stacked and last year's workbook would not line up with this year's.
`value` is what the page prints, **verbatim**; `value__value` is the parsed
number, the same `__value` naming rule an ordinary typed column already uses.

### What it gets right, and where each is pinned

| | |
|---|---|
| **A missing cell does not shift its neighbours** | every value is placed by *where it sits*, never by how many came before it on the line |
| **One date format for the whole header set** | resolving element-wise would put two periods under one name, or invent a year |
| **A total column is marked, not counted as a period** | `is_summary`, and its `filing_period` is `NA` |
| **Continuation pages need no header** | the column model found once is reused; these returns establish periods on page 1 and simply carry on |
| **A reprinted header is consumed** | not read as a measure called "Filing Period" |
| **A code stays a code** | `PTS Status` of `ISS` keeps its text and gets no number; type is not forced across the matrix |
| **Parenthesised figures are negative** | `(1,204.55)` → `-1204.55`, with the brackets kept in `value` |
| **A header whose columns are not periods is refused** | rather than filing every figure under a name that is not a period |

- The engine: `../R/doc_matrix.R`.
- The tests: `../tests/testthat/test-doc_matrix.R`.
