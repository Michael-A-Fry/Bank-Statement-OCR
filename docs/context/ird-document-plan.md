# Generalising `mode: document` for IRD reports — what is built, what is not

The source analysis is `IRD_PDF_YAML_Conversion_Analysis(2).txt` at the repo
root. This page is the running score against it: what is in the engine now, what
is deliberately not, and what the next change should be. It exists so nobody has
to re-derive the plan from a 2,000-line document, and so that "we did the IRD
work" can never mean more than it does.

## The one-sentence diagnosis

The template schema stored **answers** — `(page, y)` for a table's start and end,
`x_min`/`x_max` for its columns, a filing year as a column *name* — where the
document itself carries the **evidence**. Every stored answer is a measurement
taken from one example, and every one of them rots the moment an earlier section
is absent, a history gains a row, or the customer files for a different year.

The correction is not to delete coordinates. It is to use them for the job they
are good at (assigning words to columns *inside* a located table) and to stop
using them for the job they are bad at (finding the thing, and deciding its
extent).

## Two branches, deliberately parallel

This work is split across two branches cut independently from `main`. Neither
contains the other; they are meant to be reviewed and merged separately.

| Branch | What it adds |
|---|---|
| `claude/user-concurrency-review-u68nnr` | page furniture (`R/doc_noise.R`) and order-independent section boundaries (`R/doc_structure.R`) |
| `claude/ird-period-matrix` **(this one)** | the filing-period matrix (`R/doc_matrix.R`) |

**They will need reconciling at merge**, and the seam is known: the analysis
requires that on a matrix's continuation pages "repeated footers must be removed
without dropping table rows near the bottom of the page". That is
`R/doc_noise.R`, which is on the other branch. Until the two are merged, a matrix
read from a report with a running footer will carry footer text into its
measures. Nothing here works around that — working around it would mean writing a
second, worse copy of the furniture detector.

## Built by this branch

**`shape: matrix`** — `R/doc_matrix.R`, `test-doc_matrix.R`.
Analysis section 11 and section 3 of the validation appendix. The filing periods
are read off the document and never stored, so one template covers three periods
or twelve, 2018–2020 or 2021–2023. Output is long and stable
(`measure`, `filing_period`, `value`, `value__value`, `is_summary`, `page`), so
two customers stack and last year's workbook lines up with this year's.

The analysis's own acceptance list for this shape is the test file's spine, and
each item is pinned by name: a missing cell does not shift its neighbours; one
date format is resolved for the whole header set rather than element-wise; a
total column is marked and not counted as a period; continuation pages reuse the
column model without a repeated header; a reprinted header is consumed; a code
(`PTS Status` = `ISS`) keeps its text and is not forced to a number;
parenthesised figures are negative with the brackets kept verbatim; and a header
whose columns are not periods is refused rather than read.

Schema documented for template authors in `templates/documents/README.md`.

## Built earlier, on `main`

| | Where |
|---|---|
| A table is found by what it prints, not by its page | `R/tables.R` |
| A table is as long as it is on **this** document | `R/tables.R` |
| Column bands corrected from the located heading, bounded | `R/tables.R` |
| The unheaded total column | `R/tables.R` |

## NOT built — and none of it is started

**Logical row reconstruction** (Phase 3, analysis section 9). A wrapped email
address, a name split across lines, a continuation line with a blank leading
column: each still becomes its own row. `merge_previous`, `append_by_column` and
declarative new-row evidence are not implemented. Section anchoring does not fix
these — the analysis says so explicitly, and it is right.

**Component detection** (Phase 5, analysis section 12 and appendix 5–7). A
combined PDF holds a Level 1 report followed by appended return components with
different schemas. Until components are detected, a matrix's `end` must be given
in the template rather than derived, and the last section of a Level 1 report has
no correct end — "end of document" would absorb the appended returns.

**The other two matrix orientations** (appendix section 4). `measures_by_period`
is what `shape: matrix` implements. Still missing: `records_by_period` (GST Sales
and Income — periods as *rows*, fixed columns) and repeated-period records
(partner attribution, where duplicate periods are legitimate and must not be
collapsed).

**Structural validation and table verdicts** (analysis section 13). Per-table
counts exist; the verdict vocabulary (`absent_optional`, `missing_required`,
`excessive_unclaimed_words`, `ambiguous_boundary`, `fallback_location_used`) does
not.

**Header alias normalisation to canonical names** (PROBLEM 6, STAGE 5). Output
column names are still the template's, not normalised canonical ids.

**Builder changes** (analysis section 17). The toolkit still asks an analyst to
draw start and end points. It does not ask whether columns are dynamic periods,
how continuation pages behave, or how a new logical row is recognised. Until it
does, a `shape: matrix` template is written by hand.

## The promotion gate has NOT been met, and no IRD template ships

Analysis sections 18 and 20 require a validation corpus — unseen fixtures, a
missing optional section, a multi-page table, a differing filing-year range, a
negative fixture from another subtype, and one from a non-IRD document — before
any IRD template is promoted.

None of that corpus exists in this repo, so **no IRD template has been added to
`templates/documents/`**. Shipping one built from a single example is the exact
overfitting this whole change is against, and the analysis names it in WHAT NOT
TO DO: *"promoting a template from the same document used to build it."*

The schema is documented in `templates/documents/README.md` and exercised by the
synthetic fixtures in `test-doc_matrix.R` instead.

## Next change, in order

1. Merge the two branches and reconcile the furniture seam named above.
2. Logical row reconstruction (Phase 3) — what makes the Level 1 sections correct
   rather than merely correctly bounded.
3. Component detection (Phase 5), which also gives a matrix a derived end.
4. The remaining two matrix orientations, then table verdicts, then the builder.
