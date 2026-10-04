# Noise-robustness prototypes (4 Oct 2026) - not yet merged

Two prototypes built on a frozen copy of commit 5c64333 and measured with
`tools/synth/score_auto.R --mode trained`. Both kept AUTO_WRONG at 0 and left the
dev set unchanged. They are NOT in the engine yet: they must be re-applied on top
of the later safety fixes (4900bc1 and the round-2 fixes), given tests, and
measured on the freshly generated held-back sets before they ship.

| Patch | What it does | Measured |
|---|---|---|
| `decoy-prototype.patch` | Cuts each page into separate tables, reads the statement's own table with the others set aside (only when its printed opening and closing confirm it), reads other accounts' tables separately and labelled; new `R/auto_read_blocks.R` | decoy packs 0 -> 80 of 80; two fresh decoy sets 80 of 80 each |
| `greenflag-prototype.patch` | Unlabelled opening / closing / carried-forward lines at a table's edge, and the other green-flag families; new `R/auto_read_summ.R` | green-flag PDFs 45 -> 79 of 100, scans 0 -> 2 of 8 |

`diagnosis-notes.md` has the root causes, fix designs and per-family gains. One
optional green-flag rule (an opening printed above the closing in a box settles
direction, `AR_SUMMARY_ORDER`) needs a product-owner decision before it is used.
Apply with `patch -p1` from a copy whose `R/` matches 5c64333, then port the hunks.
