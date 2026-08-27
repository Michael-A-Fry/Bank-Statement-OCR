# Generalising `mode: document` for IRD reports — what is built, what is not

The source analysis is `IRD_PDF_YAML_Conversion_Analysis(2).txt` at the repo
root. This page is the running score against it: what is in the engine now, what
is deliberately not, and what the next change should be. It exists so that
nobody has to re-derive the plan from a 2,000-line document, and so that "we did
the IRD work" can never mean more than it does.

## The one-sentence diagnosis

The template schema stored **answers** — `(page, y)` for a table's start and end,
`x_min`/`x_max` for its columns, a list of footer wordings — where the document
itself carries the **evidence**. Every stored answer is a measurement taken from
one example, and every one of them rots the moment an earlier section is absent
or gains a row.

The correction is not to delete coordinates. It is to use them for the job they
are good at (assigning words to columns *inside* a located section) and to stop
using them for the job they are bad at (finding the section, and deciding where
it ends).

## Built, tested, and on by default

| | Where | Tests |
|---|---|---|
| A table is found by what it prints, not by its page | `R/tables.R` | `test-doc_hard.R` |
| A table is as long as it is on **this** document | `R/tables.R` | `test-doc_tables.R` |
| Column bands corrected from the located heading, bounded | `R/tables.R` | `test-doc_hard.R` |
| The unheaded total column | `R/tables.R` | `test-doc_hard.R` |

Those four landed before this page existed. They are the reason the two items
below could be additive rather than a rewrite.

## Built by this change (opt-in, `schema_version: 2`)

**Page furniture** — `R/doc_noise.R`, `test-doc_noise.R`.
Analysis PROBLEM 4 and STAGE 1; called "your biggest issue" there. Disclaimers,
confidentiality stamps and page numbers no longer become rows. Recurrence is
generic and learns per document, so it needs no vocabulary; explicit template
rules cover what is printed only once. Suppressed lines are counted and stated,
never silently dropped.

**Order-independent section boundaries** — `R/doc_structure.R`,
`test-doc_structure.R`. Analysis PROBLEM 3, PROBLEM 5, and the CRITICAL
CORRECTION. Every section start is detected independently, sorted physically, and
each table ends at the nearest later validated start — whichever section that is.
No successor is named anywhere. A heading string with no column header under it
is a value, not a section start, which is what keeps a "No Information Held" list
from closing three open sections.

Both are opt-in. A version 1 template reads exactly as it did; that is asserted,
both directions, in `test-doc_structure.R`.

The schema is documented for template authors in `templates/documents/README.md`.

## NOT built — and none of it is started

These are the analysis's Phases 3 to 5. They are listed with the failure each one
leaves open, so nobody mistakes this branch for a finished IRD capability.

**Logical row reconstruction** (Phase 3, analysis section 9). A wrapped email
address, a name split across lines, a continuation line with a blank leading
column: each still becomes its own row. `merge_previous`, `append_by_column` and
declarative new-row evidence are not implemented. *Section anchoring does not fix
these — the analysis says so explicitly, and it is right.*

**Dynamic period columns / `shape: matrix`** (Phase 4, analysis sections 11 and
3 of the validation appendix). The filing-period matrices — Individual Income
Return, Financial Statements Summary, Companies Return, Imputation Return — need
columns generated from the detected period header and a stable long-form output
(`measure`, `filing_period`, `value`, `is_summary`). Without it, no single
template generalises across report dates, because the years become column names.
**This is the largest remaining gap and the one to do next.**

**Component detection** (Phase 5, analysis sections 12 and 5-7 of the appendix).
A combined PDF holds a Level 1 report followed by appended return components with
different schemas. Until components are detected, the last Level 1 section has no
correct end: "end of document" would absorb the appended returns.

**Structural validation and table verdicts** (analysis section 13). Per-table
counts exist in `report` and `spilled`; the verdict vocabulary
(`absent_optional`, `missing_required`, `excessive_unclaimed_words`,
`ambiguous_boundary`, `fallback_location_used`) does not.

**Header alias normalisation to canonical names** (PROBLEM 6, STAGE 5).
`header_signatures` accepts alternative wordings for *detection*. Output column
names are still the template's, not normalised canonical ids.

**Builder changes** (analysis section 17). The toolkit still asks an analyst to
draw start and end points. It does not yet ask for a heading alias, a
continuation policy, or how a new logical row is recognised. Until it does, a
version 2 template is written by hand.

## The promotion gate has NOT been met, and no IRD template ships here

Analysis sections 18 and 20 require a validation corpus — unseen fixtures,
a missing optional section, a multi-page table, a differing filing-year range, a
negative fixture from another subtype, and one from a non-IRD document — before
any IRD template is promoted.

None of that corpus exists in this repo, so **no IRD template has been added to
`templates/documents/`**. Shipping one built from a single example is the exact
overfitting this whole change is against, and the analysis names it in WHAT NOT
TO DO: *"promoting a template from the same document used to build it."*

The v2 schema is demonstrated in `templates/documents/README.md` and exercised by
the synthetic fixtures in `test-doc_structure.R` instead.

## Next change, in order

1. `shape: matrix` with dynamic period columns and long-form output (Phase 4).
   It unblocks the return components, which are most of the appended pages.
2. Logical row reconstruction (Phase 3). It is what makes the Level 1 sections
   correct rather than merely correctly bounded.
3. Component detection (Phase 5), then table verdicts, then the builder.
