# Noise-robustness prototypes (4 Oct 2026) - merged

Two prototypes were built on a frozen copy of commit 5c64333 and measured with
`tools/synth/score_auto.R --mode trained`. They have now been **merged** into the
reader on top of the round 1 and round 2 safety fixes (4900bc1, d9b59ec, a7dc3cc).
They were ported, not applied: each rule was re-read against today's checks, given
regression tests, and measured again. The patches stay here as the record of what
was prototyped.

| Patch | Merged as | Working set (a7dc3cc -> merged) |
|---|---|---|
| `decoy-prototype.patch` | `R/auto_read_blocks.R` (a pack read a table at a time), hooks in `R/auto_read.R` and `R/auto_read_pdf.R`, check `tables_set_aside`, other accounts in `rd$other_accounts` and in the outputs | decoy packs 0 -> 65 of 80 |
| `greenflag-prototype.patch` | edge balance lines, dates, figures, sign marks, wrapped and drifting rows in `R/auto_read_pdf.R`, `R/normalise.R`, `R/parse_pdf_table.R`; summary figures named by the arithmetic in `R/auto_read_summ.R`; checks `edge_lines`, `compact_dates` | green-flag PDFs 0 -> 69 of 100, scans 0 -> 2 of 8 |

AUTO_WRONG stayed 0 on every set. The measured numbers, the fresh sets and the
safety harnesses are in `CHANGELOG.md` ("What is not done") and
`docs/context/outstanding-work.md` (item 1).

## What was left out, and why

- **Keyword lists copied from the test generators** (the product owner's overfitting
  policy). The decoy prototype told another account's table by phrases taken from
  `make_decoys.py` ("linked account", "for information only", "activity this
  period"...) and by its "TD-" number format; the green-flag prototype read page
  numbers worded "Leaf 2 of 3" (a made-up word of `make_greenflag.py`) and took
  its list of standard sign marks from the generator. In their place: another
  account is told by an account or card number in its title that is not the
  statement's own (the lexicon's own number patterns); sign marks are the
  lexicon's own debit and credit markers or + and -; page numbers are "Page 2 of
  3", "Page 2/3", "p. 2/3" or "Page 2 (of 3)". Cost on the working sets: the
  "Leaf" wording alone would have added 17 green-flag PDFs.
- **Combined statements stay with a person.** The prototype read a statement of
  several accounts as one reading with sections (and a section opening on
  "Balance brought forward"). Round 2's `one_statement` check sends any such
  reading to a person, so those rules were left out. Added instead: a table of
  another account's transactions printed like the statement's own (in its
  columns or under its heading row) sends the file to a person, because it may be
  a section of a combined statement, whose rows are all the statement's.
- **The set-aside guard is stricter than the prototype's.** The prototype
  accepted a set-aside table when opening + movements = closing with any opening
  and closing balance, carried ones included. A statement whose first page is
  printed in other columns then proved from its second page alone, opening on
  the balance brought forward (test-blocks.R). Now the closing must be the
  statement's own printed closing, and a brought-forward opening is refused when
  its figure is printed in a table that was set aside. Blocks on different pages
  also join when their columns line up under one common shift.
- **Arithmetic-named summary figures** (green-flag F8) prove rows, but count as
  the statement's printed start or end only when printed above the table's first
  row; under the rows they could be a balance carried to a missing page.
- **The optional rule `AR_SUMMARY_ORDER`** (an opening printed above the closing
  in a header or box settles which column is money in) is merged but OFF by
  default: it is used only when the environment variable `AR_SUMMARY_ORDER` is
  `1`. It waits on a product-owner decision.
- **Display-only `show_blocks`** (show the statement's own table read alone when
  nothing proves) was not merged: unmeasured, and not needed for safety.

`diagnosis-notes.md` has the original root causes, fix designs and per-family
gains as measured on the old engine.
