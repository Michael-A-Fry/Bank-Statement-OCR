# Recipes + proof (direction C, chosen 5 Oct)

## The idea in one paragraph

Each statement design the team meets is read by a **recipe**: a short YAML file
saying how to recognise the design, where its table is, its columns left to
right, which way money goes, how the year is found, and which lines are not
transactions. One small reader runs any recipe. The statement's own arithmetic
(running balance, opening + rows = closing, printed totals) **proves every
result**, so a bank that changes its design is caught, never read wrong. The
automatic reader stays, but only as the drafter of new recipes and the last
resort.

## Why this, and why templates failed before

| 1.x templates failed because | Recipes answer it with |
|---|---|
| written by hand in YAML | drafted by the automatic reader from the first statement |
| a wizard nobody finished | confirmed by plain questions on Please check (one per column, with the column's own lines) |
| auto-pick matched 33% | each recipe recognises itself by its own words, scored, the bank pre-filled and confirmed |
| nothing checked them | the sums check every result; a recipe that stops proving goes to a person |

## A recipe

```yaml
recipe: anz_everyday_pdf          # file name = id; versioned like layouts (never edited in place)
bank: anz
kind: pdf                         # pdf | scan | excel | delimited
status: proven                    # draft | proven | retired
recognise:                        # words on the statement, case-insensitive
  all: ["Account at a glance", "Transaction type and details"]
  none: ["Credit card", "Loan"]
statement_starts: "Account at a glance"   # several statements in one file
table:
  header: ["Date", "Transaction type and details", "Withdrawals", "Deposits", "Balance"]
  columns:                        # left to right; each column hangs under its header word
    date:        {under: "Date"}
    description: {under: "Transaction type and details"}
    debit:       {under: "Withdrawals", align: right}
    credit:      {under: "Deposits",    align: right}
    balance:     {under: "Balance",     align: right}
  ends_at: ["Totals at end of page", "Totals at end of period"]
  skip: ["Balance brought forward", "No transactions for this period"]
dates: {format: "%d %b", year: period}
money: {style: debit_credit_cols, overdrawn: ["OD"]}
```

Columns hang under their header words, so a page that sits a few points to
the left still reads. Absolute `x_min/x_max` bands (the 1.x form) are still
accepted for a design with no header row.

## How a file is read

1. **Recognise**: score every recipe of the chosen (or pre-filled) bank by its
   `recognise` words; the best one over the bar is used. None: step 4.
2. **Read** with the recipe (the 1.x table reader, `parse_statement`, given the
   recipe's columns for each page). Seconds for 120 pages: no guessing.
3. **Prove**: the reader's checks. Proven: done, automatically. Not proven:
   step 4, and the recipe is flagged (the design may have changed).
4. **Automatic reader** (today's), as now. On Please check the person answers
   one plain question per column; when the answers prove, the reading is saved
   as a **draft recipe** for an admin to accept.

## Order of work

1. Recipe file format + loader + recogniser + reader + proof (on top of
   `parse_statement` and the reader's checks). Prototype on ANZ everyday and
   one card type; measure on their QVF lookalikes.
2. The 11 QVF types as 11 recipes, from the format cards in
   `scratchpad/qvf/cards/`. Target on the 86 lookalikes: all automatic, none
   wrong. Then the owner's real files.
3. Convert flow: recipe first, automatic second; Please check answers save a
   draft recipe; Admin -> Banks lists recipes (accept, retire).
4. `.xls` input (BNZ's real Excel export) and the Excel fixes the QVF handles
   (OD in the number format, a "." zero, two accounts in one sheet).
5. Qlik output compatible with the QVF's (unsigned Amount + Transaction Type,
   Code Description ...), if dashboards depend on it: owner to confirm.
