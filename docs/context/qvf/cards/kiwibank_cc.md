# Format card: Kiwibank Credit Card (type key `kiwibank_cc`)

QVF section: `script.qvs` lines 5995-6496 (form value `Kiwibank - Credit Card`, PDF only).
Shared code it calls: `sAddToPairs`, `sBalances` (136-187), `sConcatenateStrings`
(335-395), `sLoopToDate` with `'full'` (233-262), `sFindDeposits` (436-1000), and three
Kiwibank-only branches inside `sEndofData` (1200-1240 ordering, 1276-1295 closing,
1391-1440 opening row); then the PDF final transform (7479-7642) and `sBalanceCheck`.

The script never looks at x/y positions: it walks the words in reading order with fixed
landmark words. "Layout" below means the order the words come in.

Lookalikes: `gen/make_kiwibank_cc.py` -> `sets/visa2/kiwibank_cc_1..7.pdf` (+ truth).
Each is read back with a replay of these rules (`qvf_replay()`); results per statement in
`visa2_debug/qvf_replay/`.

---

## 1. The statement, as the script expects it

Words are used raw (no `Date_Tidy_Up`, nothing removed).

### One statement = one "Credit limit" (6025-6033)
Every `limit` straight after `Credit`/`credit` starts a statement. So the phrase must
appear once per statement (its details block: `Credit limit $5,000.00`). Any other
"credit limit" in the small print would split the statement.

### Account name and card number (6087-6146, read again at each new statement 6276-6286)
`Your card number 5412 XXXX XXXX 1234` / `Your account name J SAMPLE` / `Statement date ...`
* name = the words after `account name` up to `Statement` or `Closing` (exact case);
* number = the words after the last `card number` / `account number` before that
  `account name`, less the words `Your` and `account`.

### Opening balance (6035-6058 + `sBalances`)
* start: `Opening balance` (or `Current Due` in another wording), paired with the next of
  `Thank`, `Kiwibank`, `Total`, `Includes`;
* the figure = the word after the start up to the next `$` word or that end word.
* So the opening figure must be followed directly by another `$` figure or by one of
  those words: the lookalikes print a vertical summary whose next line is
  `Total credits $...`.

### Closing balance (6060-6085)
* `Closing balance` (but not when the word before is `Debits`), then the figure, ended by
  the next `If` or `Interest` (exact case). **Why `Debits`:** another summary layout
  prints `... Debits Closing balance` as a strip of labels with the figures under them,
  where this rule would read the wrong figure.
* So the closing figure must be followed directly by an `If ...` or `Interest ...` line:
  the lookalikes print `Interest rates: purchases 13.95% p.a., ...` straight under it.

### Sign of the balances (`sBalances` 157-176)
`CR` = in credit, kept positive; anything else is owed and made negative.

### The transaction table (6148-6230)
* starts after the heading word `Debit` when the word before is `Credit` or `amount` and
  the one before that is `Details`, `detail` or `credit`:
  `Transaction date | Date processed | Card | Details | Credit | Debit` (credit column
  FIRST) or `... Details | Credit amount | Debit amount` (the `amount` after `Debit` is
  dropped, 6203);
* ends at the first `TOTALS`, `Totals`, `Air`, `Low` or `Statement` printed straight after
  a figure (a word with a `.`): the totals row, or the product's block under the table
  (`Air New Zealand Airpoints ...`, `Low Rate ...`);
* a heading start counts only when the next marker after it is an end: **one heading per
  statement**. A heading repeated on page 2 would make the script drop page 1.
* **Page breaks (6305-6317):** the table runs on across pages with nothing in between
  except either `Continued over Page n of N` as the LAST words of a page (skipped as 6
  words) or `Page n of N` as the FIRST words of the next (skipped as 4). Any other text
  there (a repeated heading, a masthead in text) lands inside a row. The lookalikes put
  the synthetic banner on such pages as an image for that reason.
* **The Card column (6177-6191):** every 4-digit word straight after a word containing
  `/` (the date processed) is removed: the last four digits of the card that made the
  purchase. **Why:** several cards on one account are listed in ONE table, each row
  saying whose card.

### A row (6292-6410)
* column 1: one word, the transaction date `dd/mm/yy`; the next word (the date processed)
  is skipped (6330-6334);
* column 2: the details, up to the word before a `$` figure that is followed by the next
  row's date (a word with `/`), a number, `Continued`, `Page` or a new statement, and not
  when the word is `of` or the third word on is `Includes` (6340);
* column 3: the `$` figure and anything after it up to the next word containing `/`,
  `$` removed, spaces turned to commas. So **nothing may follow the figure before the next
  row's date**: a wrapped description prints its figure on the LAST line; a `CR` or a
  trailing `-` as a separate word makes the amount unreadable.
* Credit and Debit figures look the same in the word stream: the columns are not used.

### Money in or out: keywords, then a subset-sum (6372-6382, `sFindDeposits`)
1. Deposit when the details contain (case-sensitive) `Payment Thankyou`,
   `Payment Received`, `Assmnt Rev`, `Fee Rev`, `Interest Adjustment`, `Account Fee Credit`,
   `Refund`, `CC Pment`, `Creditpment`, `Creditcar`, `Currency Conv Assmnt Rev`,
   `Foreign Curr Txn Fee Rev`, **or** a figure with two decimals followed straight by `-`
   (`30.00-`) somewhere in the details. `CC Pment` / `Creditpment` / `Creditcar` are
   payments from other banks whose narrative is cut short.
2. Then the same subset-sum as BNZ (difference / -2, sets of up to 6 rows, one set =
   deposits, several sets = `Unidentified`), except that rows whose details are exactly
   `Currency Conv Assessment` or `Foreign Currency Txn Fee` are never candidates (586):
   **why:** a Kiwibank foreign purchase is followed by those two small fee rows (usually
   under $1), which would otherwise make many equal sums.
* Lookalike 4 has a merchant refund with no deposit word; the replay's subset-sum finds
  it (the one set).

### Year
Dates carry their year (`dd/mm/yy`). A transaction dated before the period (bought on the
14th, processed on the 16th) needs nothing special.

### Order (sEndofData 1200-1240, 1276-1295, 1391-1440)
* When the last row is dated before the first, the statement was printed newest first: the
  rows are re-sorted oldest first and the opening row takes the LAST opening balance read.
* Otherwise rows are sorted by date (then printed order). **For Kiwibank the statement
  number is not part of the sort**, and only ONE closing balance (the last statement's)
  is joined: a file of several statements becomes one date-sorted run, balances worked
  back from the last closing, opening row = the first statement's opening. Right for
  consecutive statements of one card; wrong for anything else.

### What is dropped
Everything outside heading-to-end: summary, page furniture, the TOTALS row, the product
block. The card digits. The date processed. A statement with no rows is not handled (with
no word containing `/` in the data, `sLoopToDate 'full'` never finds a date).

---

## 2. What the QVF writes for this type

| QVF column | How it is filled for Kiwibank Credit Card |
|---|---|
| File Name, Doc Reference Bank Statement / Voucher | upload form |
| Row ID | file - row, row 1 = made-up **Opening Balance** row carrying the PRINTED opening balance |
| Bank | `Kiwibank` |
| Account Name, Account Number | from `Your account name` and `Your card number`, per statement |
| Other Party Account Name / Number, Transaction Time | empty |
| Date | the TRANSACTION date (first date); the processed date is dropped |
| Year, Tax Year | Tax Year = year - 1 for Jan-Mar |
| Description as per bank statement | the details (card digits removed) |
| Transaction Type | Deposit / Withdrawal / Unidentified (keywords + subset-sum) |
| Transaction Code, Code Description, Transaction Category | keyword workbook lookup, defaults 200/400 "To be done", 199/399 inter-account transfers |
| Amount | absolute value |
| Balance | worked backwards from the (last) closing balance in DATE order; owed = negative |
| Balance Check, Balance Pass | forward running sum from the opening row, Pass/Fail per row |
| Sort Number | position after the date re-sort (not the printed order) |

---

## 3. The lookalikes (`sets/visa2/kiwibank_cc_*.pdf`)

All: A4, Helvetica 7.5, page 1 = masthead, address, details block (card number, account
name, statement date, statement period, credit limit, available credit), summary
(`Opening balance`, `Total credits`, `Total debits`, `Closing balance`, the interest-rate
line, minimum payment, due date), then the two-line heading and the rows; TOTALS row; no
running balance; every figure `$1,234.56`; rows in processed-date order.

| # | What it tests | QVF replay |
|---|---|---|
| 1 | one card, one page, full payment `Payment Thankyou` | 14/14 right |
| 2 | two cards in one list (Card column), 48 rows over 2 pages, `Continued over Page 1 of 2` then page 2 straight into rows (no heading), 2 foreign purchases each followed by `Currency Conv Assessment` and `Foreign Currency Txn Fee`, `Refund`, part payment, interest | 48/48 right |
| 3 | newest first (`Payment Received`) | 16/16 right (re-sorted) |
| 4 | period 15 Dec 2024 - 14 Jan 2025, a row dated before the period, closes IN CREDIT, merchant refund with no deposit word, `Foreign Curr Txn Fee Rev`, `Interest Adjustment`, a foreign purchase | 18/18 right (subset-sum finds the refund) |
| 5 | three consecutive statements in one PDF | 33/33 right |
| 6 | no transactions, $15.00 CR to $15.00 CR, TOTALS $0.00 $0.00 | (the real script would not finish; replay reads 0 rows) |
| 7 | Airpoints card: heading `Credit amount / Debit amount`, no TOTALS (table ends at `Air New Zealand Airpoints ...`), wrapped rows with the figure on the last line, two cards, `Page 2 of 2` as the first words of page 2, a payment `J SAMPLE CC Pment`, interest | 50/50 right |

Not printed, because the script cannot read it: a `CR` or `-` after a transaction figure,
a heading repeated on a continuation page, two sections per card each with its own table.
