# tools/synth — the measured corpus

Two scripts. `make_corpus.py` draws adversarial synthetic bank statements, each one
paired with the exact rows it was drawn from. `score.R` runs the engine over them
and scores it against that ground truth.

```
python3 tools/synth/make_corpus.py --out /tmp/corpus    # 37 cases, PDF + .truth.json
Rscript  tools/synth/score.R /tmp/corpus                # the score board
python3 tools/synth/make_corpus.py --list               # what each case tests
```

**Python 3.9 or newer**, plus `reportlab` and `pymupdf`. Tested on **3.11, 3.12 and
3.13**, which produce **byte-identical ground truth** — so the corpus can be
regenerated, bisected against, or handed to somebody else and still mean the same
thing.

> That reproducibility had to be fixed to be true. The per-case seed was
> `hash(case_name)`, and Python **salts the hash of a string per process** unless
> `PYTHONHASHSEED` is set — so two runs of the same interpreter produced different
> figures and 0 of 37 truth files matched between 3.11 and 3.12. Nothing measured was
> wrong, because each PDF carries its own truth and every score compared like with
> like; but a corpus you cannot regenerate is one you cannot bisect against. It is
> `zlib.crc32` now.

## Why this exists

The suite's golden files answer **"has this changed?"**. They cannot answer
**"how accurate is it?"** — a golden file *is* the reader's own output, so if the
reader is wrong the golden is wrong with it, and agrees with itself for ever.

Every PDF here carries a `.truth.json` written by the generator that drew it, so
the right answer is known independently of the reader. That is the only way to find
a **wrong figure**, as opposed to a crash.

## The number that matters

| column | meaning |
|---|---|
| `FABR` | **fabricated** — an amount that is not the amount on the page, and is not NA. **This must be 0.** |
| `refus` | **refused** — the reader could not read the cell, returned NA, and the row carries `malformed`. A gap a reviewer can see. Not the same fault. |
| `miss` | rows missing or invented. Visible to anyone counting rows. |

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
