# Recipes not made

Every statement design the owner described in `docs/context/RECIPE_STATEMENTS`
(80 blocks) is read by a recipe in `recipes/` except the two below. Each is left
out because a recipe for it could not be proven by the statement's own
arithmetic, or would put figures on the output that are not what the statement
says. A statement of either design is still read by the automatic reader, and
goes to Please check unless its arithmetic proves it, exactly as before.

Run `python3 tools/recipes/from_designs.py docs/context/RECIPE_STATEMENTS --out /tmp/x`
to see which blocks the converter writes; the ones it cannot write and that are in
`recipes/` were written by hand, each with a comment at its top saying why
(Bank of China Online Saver, American Express Gold Card, HSBC Everyday).

## Bank of China -- STATEMENT OF LOAN ACCOUNT (block 28)

**Not made: one transaction moves three amounts, and the balance follows only one.**

Each row prints Principal, Interest and Interest Penalty, and the running
"Principal Balance" changes by the Principal alone. An interest-settlement row
("0066") prints `0.00` principal and `1,882.19` interest; a repayment ("0018")
prints `-1,960.71` principal and `-1,882.19` interest. A recipe reads one amount
per row (or money out and money in), so it can either:

* read the Principal as the amount -- the arithmetic proves it (the balance moves
  by exactly that), but the interest rows come out as `0.00` and every interest
  and penalty figure is missing from the output: proven, and wrong for an
  accountant; or
* read Principal + Interest + Penalty as the amount -- then the balance does not
  follow it, so nothing proves it.

The first is an automatic-but-wrong reading, which the product never makes. The
block also prints no opening principal ("There is no explicit opening-principal
label in the transaction section"), so the first row of each statement has
nothing to prove it against, and the transaction type is only a code ("0018")
with no description column.

What would make it possible: a recipe word for a row with several money columns
(each kept as its own output column, the balance proven on one of them), and a
decision from the owner on how a loan's interest rows should appear in the output.

## Co-operative Bank -- ELECTRONIC ACCOUNT, older print (block 34)

**Not made: no heading line, no column positions, and no opening balance.**

This print of the Co-op statement has no table heading ("no heading line is
printed"), so its columns cannot hang under heading words. A recipe for such a
design gives each column as a fixed band (`x_min`/`x_max` in points), but the
block gives no positions -- only the text in reading order -- so any bands would
be invented. Bands that are wrong do not read wrongly (nothing would prove), but a
recipe that never proves only adds a "the recipe did not prove" note to every
statement of the design.

More decisive: the statement prints no opening balance and no closing balance
label ("The statement does not print a fixed opening-balance or closing-balance
label"), and the running balance is printed only after each group of
transactions. The rows before the first printed balance therefore have nothing
to be checked against, so a reading of this design can never be proven complete,
whatever the recipe. The newer print of the same product (block 1, with the
heading "DATE TRANSACTION DETAILS FEE TYPE AMOUNT BALANCE" and an "OPENING
BALANCE") is read by `recipes/cooperative_electronic_account.yaml`.

What would make it possible: one statement of this print measured (the x position
of each column), and a printed opening balance -- or the owner accepting that
statements of this print are always checked by a person, which is what happens
today.
