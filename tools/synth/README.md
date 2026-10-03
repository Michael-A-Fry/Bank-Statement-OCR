# tools/synth — the measured test sets

Synthetic bank statements, each drawn together with the **answer key** it was
drawn from, and the scorers that measure the engine against them. A golden file
can only say whether the output has changed; it cannot say whether it is right,
because it is the reader's own output. These sets can, because the right answer is
known independently of the reader.

**Nothing here ships to the server.** The offline box runs R only. The generators
are Python; the scorers are R and use the engine's own functions.

## The sets

| Generator | What it draws | Used for |
|---|---|---|
| `make_layouts.py` | The **realistic** set: 8 bank families (ANZ, ASB, BNZ, Westpac, Kiwibank, TSB, Co-operative Bank, the fictional Rimu Bank), about 40 designs, 3-5 statements per design, about 130 text PDFs, 15 scans and 12 CSV/Excel exports per split. `--split dev` to build against, `--split holdout` to score **once**. | the 95% target and AUTO_WRONG, cold and trained |
| `make_greenflag.py` | The **green flag**: 100 deliberately weird statements (made-up headings in odd places, 1-5 wrapping text columns, every money format, about 10 undecidable on purpose). Held back. | the acceptance test (spec section 9) |
| `make_decoys.py` | About 80 statements buried among tables that look like transactions (loan schedules, mini-statements, fee tables). Held back. | the noise test |
| `make_corpus.py` | 43 adversarial cases, one layout pushed until it breaks. | regression |
| `make_bench.py` | 30 to 400-page statements. | speed |

```
python3 tools/synth/make_layouts.py --out /tmp/zoo/dev --split dev
python3 tools/synth/make_corpus.py  --out /tmp/corpus           # 43 cases
python3 tools/synth/make_layouts.py --split dev --list           # what each case tests
```

**Never build against a held-back set.** The realistic holdout, the green flag and
the decoy set are scored once, at the end, by someone who did not build the
reader. Looking at them while building turns an acceptance test into a dev set.

## The scorers

```
Rscript tools/synth/score_auto.R    /tmp/zoo/dev --mode cold    --out auto.csv
Rscript tools/synth/score_convert.R /tmp/zoo/dev --mode trained --out conv.csv
```

- `score_auto.R` scores the **reader** (`auto_read()`).
- `score_convert.R` scores **conversion end to end** (`convert_statement()`,
  with the bank, the layout store and the outputs), and reads the figures back from
  the CSV it writes, so what is scored is what an analyst downloads. It also
  counts any `-0.00` written.

Every statement lands in exactly one cell:

| cell | meaning |
|---|---|
| `auto_right` | converted with no person, and every figure right |
| **`AUTO_WRONG`** | converted with no person, and **any** figure wrong, missing or extra. **This must be 0.** It is the silent failure. |
| `check_right` | sent to a person; the reading on screen is right |
| `check_wrong` | sent to a person; the reading on screen needs fixing |
| `unread` | nothing read; the reason is shown |

A figure is right when its (date, signed amount) pair matches the answer key in
printed order (the longest common subsequence, so a missing row, an extra row and
a wrong figure all count against it). A statement the key marks `decidable: false`
can only be answered right by asking: an automatic answer on one is AUTO_WRONG
even when its figures happen to be right.

`--mode cold` reads each statement alone with nothing learned. `--mode trained`
reads bank by bank in file-name order through one layout store per bank, learning
exactly as the server would.

**The definitions were fixed before the reader was built.** Do not change what
counts as right to make a number move; change the reader.

The 2.0.0 figures (dev set: 121/128 text PDFs, 4/7 CSV, 5/5 Excel, 12/15 scans
automatic and right; AUTO_WRONG 0 everywhere) are in the 2.0.0 entry of
[CHANGELOG.md](../../CHANGELOG.md).

## Pictures of what the reader picked

```
Rscript tools/synth/gallery_dump.R <engine_dir> <cases.tsv> <out_dir>
python3 tools/synth/gallery_draw.py <out_dir> <png_dir>
```

One picture per case: the pages with the columns the reader picked drawn over
them, and what it decided, why, and where its figures differ from the answer key.

## Retired with templates: `score.R` and `bench.R`

`score.R` (the corpus scored through the shipped templates, with its `FABR` /
`refus` / `bands` columns) and `bench.R` (per-stage timings) call template
functions that were removed at 2.0.0, and **do not run** until they are rewritten
for the automatic reader. Score the corpus with `score_convert.R` meanwhile. The
history below is what they found, and it still stands.

`truth.R` holds the truth-file reader every harness uses. It is one file because
`bench.R` was first written with its own copy, read a field the truth does not
have, and reported **900 of 900 amounts fabricated** on a statement the engine had
read perfectly.

**Python 3.9 or newer**, plus `reportlab` and `pymupdf`. Tested on **3.11, 3.12 and
3.13**, which produce **byte-identical ground truth**, so a set can be
regenerated, bisected against, or handed to somebody else and still mean the same
thing. (The per-case seed is `zlib.crc32`, not `hash()`: Python salts the hash of
a string per process, and once 0 of 37 truth files matched between 3.11 and 3.12.)

## Why this exists

The suite's golden files answer **"has this changed?"**. They cannot answer
**"how accurate is it?"** — a golden file *is* the reader's own output, so if the
reader is wrong the golden is wrong with it, and agrees with itself for ever.

Every PDF here carries a `.truth.json` written by the generator that drew it, so
the right answer is known independently of the reader. That is the only way to find
a **wrong figure**, as opposed to a crash.

## The number that mattered (the 1.x corpus scorer)

| column | meaning |
|---|---|
| `FABR` | **fabricated** — an amount that is not the amount on the page, and is not NA. **This must be 0.** |
| `refus` | **refused** — the reader could not read the cell, returned NA, and the row carries `malformed`. A gap a reviewer can see. Not the same fault. |
| `miss` | rows missing or invented. Visible to anyone counting rows. |
| `bands` | what `column_fit` said about the template's geometry: `-`, `info` or `medium`. Checked **both ways** against `.EXPECT_BANDS` in `score.R` — a case not named there must report `-`. |

Scoring *fabricated* and *refused* together hides the only improvement that
matters. When the contaminated-cell guard went into `.num_one`, this corpus went
from **13 fabricated to 0**, while refused rose 0 → 20: a strictly better engine
that one combined figure would have scored as unchanged.

## What it found

Three faults, all measured here first, all now fixed with a suite test each:

1. **An amount cell holding words became a number.** `"ASSESSMENT 2291104A 7.44"`
   — a reference number beside a $7.44 credit — read as `22911047.44`. A plausible
   figure four orders of magnitude wrong, in a row whose date and description were
   both right. (`.money_contaminated`, `R/normalise.R`.)
2. **A landscape page was squashed into a portrait frame.** Every `x` multiplied by
   0.707, so the `Withdrawals` heading at x=341 — inside the debit band — landed at
   x=241, inside the *description* band. Every column slid one to the left. Not
   rescaling it reads 16 of 16 rows. (`.pdf_orientation_differs`,
   `R/parse_pdf_table.R`.)
3. **A rotated page read nothing and said nothing.** Now `unsupported` with a
   high-severity diagnostic naming the orientation and the remedy.
   (`page_orientation`, `R/diagnose.R`.)

4. **A column that moved was caught, but never named.** A 55pt shift puts the running
   balance inside the debit band: 14 of 20 amounts came back as the *balance* rather
   than the transaction. Reconciliation caught the run every time — trust `low`,
   nothing wrong published — but "something is wrong with this statement" is a long
   way from "your balance column is empty on 19 of 20 rows". `R/column_fit.R` answers
   the second question. (`mustflag_drift_amounts_55pt`, `mustflag_drift_balance_55pt`;
   **25pt did not break it**, which `drift_amounts_left_25pt` holds in place — the
   bands are 65-77pt wide, so a third of a column is absorbed.)

   The same check cross-validated something already in the engine. On
   `band_overflow_debit_only` the output is correct on all 20 rows — but only because
   11 amounts were **derived from the running balance**, the debit cells being
   unreadable (`STORE 214.29 0114`). `column_fit` names 11 rows. They are the same 11.

5. **A per-page check that had only ever been tested on one page.** The sign-from-ink
   scan split the renderer's output on a marker this poppler does not emit, so a
   multi-page statement collapsed into ONE ink page: pages 2+ got no sign correction,
   and page 1 got false positives from strokes elsewhere in the document. The two
   3-page cases (`signed_minus_as_ink_3page`, `signed_minus_invisible_3page`) report
   **48 fabricated figures** against the old code, every one a sign inversion from row
   31 on, at trust `medium` -- they would have published. They are 3 pages for exactly
   that reason, and the layout has no balance column because that is the only shape
   where the ink is the *only* evidence of the sign.

It also found a fault in **itself**, which is worth recording because it is the
same class: `money()` printed `abs(x)`, so an overdrawn balance printed without its
sign and the first scoring run blamed the engine for 221 wrong balances on a
260-row statement. The reader was right and the generator was lying. A harness that
can accuse the engine has to be as checkable as the engine.

## Why Python, when the product is R

`reportlab` can place a glyph at an exact point, draw a minus sign as **vector ink
that is absent from the text layer**, rotate a page, and emit a 120-page file in a
second. R's `pdf()` device cannot do the second of those at all, and a negative
amount drawn as a line — which silently inverts a transaction — is exactly the
fault worth hunting.

**Nothing here ships to the server.** The offline box runs R only. This writes PDFs
and JSON; `score.R` reads them with the engine's own functions. The generator is a
dev-time tool and the product gains no Python dependency. The shipped regression
fixtures are still built by R (`tests/testthat/fixtures/make_pdf_fixtures.R`).

## Adding a case

One entry in `CASES` in `make_corpus.py`:

```python
case("struct_two_dates", "a value date beside the transaction date",
     n=16, second_date=True),
```

A case says what it is testing in its own note, and the generator writes the truth.
If a new case needs the page drawn differently, add the keyword to
`draw_statement()` — keep the data (`make_rows`) and the printing (`draw_statement`)
separate, so a difference in the score is attributable to the one thing the case
changed.
