# ASB Visa (QVF type "ASB - Visa", type key `asb_visa`)

Script section: `script.qvs` lines 5049-5471, plus the shared subroutines
`sAddToPairs` 194-211, `sCreateVariables` 217-227, `sLoopToDate` 233-307,
`sConcatenateStrings` 335-395, `sRowSetup` 401-418, `sEndofData('balances')`
1081-1194 (the "statement already has a running balance" branch) and the final
transform 7479-7642 with `sBalanceCheck` 1474-1590.

The script searches for words, never positions; where it only implies a page
shape the shape is marked *inferred*. Amounts such as `1,234.56` arrive from the
connector as one word (comment at 2536).

## 1. What the statement looks like to the QVF

### Landmarks it searches for

| Landmark (exact words) | Lines | What it gives |
|---|---|---|
| `Account Summary` ... next `Payment` | 5077-5098 | the statement period: the words between. Normally `dd Mon yy dd Mon yy` with NOTHING between the two dates: start month = +2, start year = `20` & +3, end month = +5, end year = `20` & +6 (5213-5222). When word +1 is `-` there is no start date: start = `Unknown`, end month = +3, end year = `20` & +4 (5203-5212). *Inferred*: the two dates sit in a row headed by "Account Summary", their labels on the line above. |
| every `Account Summary` | 5100-5109 | one statement per `Account Summary`: how a file of several statements is split. |
| `Payment Advice` (not after `the`) ... `Instructions` | 5111-5133 | the tear-off payment slip. Account name = the words from `Advice` up to `Customer` (5231-5233); account number = the LAST 19 characters before `Instructions` (5229), i.e. a card number printed `nnnn nnXX XXXX nnnn` immediately before `Instructions`. |
| `Date Processed` | 5135-5145 | start of the transactions on each page (the heading must END with these words in reading order). |
| `Carried forward`, `Closing Balance` | 5135-5145 | end of the transactions on a page / at the end. Only the first end after each start is used, so other "Closing Balance" lines (summary box) are ignored. |

### Page furniture (as the QVF expects it)

- Page 1: masthead, customer, the `Account Summary` row with the two dates, the
  summary figures (Opening Balance, purchases, payments, Closing Balance) and the
  payment figures (`Minimum Payment` ends the period words), the transaction table,
  and the `Payment Advice` slip (name, `Customer Number`, `Card Number`,
  `Instructions`).
- The table heading, ending `Date Processed` (the words `Balance` and `$` after
  it are dropped from the stream at 5175, so a heading such as `... Date Processed
  Balance` is fine; any other word after `Processed` on a continuation page would
  be glued to the first row's date).
- First table line: `Opening Balance <figure>` (passed over by `sLoopToDate`,
  which moves to the first month word).
- Last line on a page that continues: `Carried forward <figure>`; last line of
  the statement: `Closing Balance <figure>`.

### Table columns, in order (word order)

`Date` (transaction date, `dd Mon`) | `Details` | `Amount` [`CR`] | processed date
(`dd Mon`, skipped at 5335) | `Balance` [`CR`] | card used (one word, skipped at
5306). Columns table at 5060-5067. A running balance is printed on every row.

### Money in and out

One amount column; `CR` = credit (positive), otherwise negative (5365-5383). The
balance follows the same rule: `CR` = card in credit (positive), otherwise owed
(negative). Thousands commas are fine here: the details end at any word with two
digits after a point (5323).

### How the year is found

From the `Account Summary` dates (5385-5401):
1. start month Oct/Nov/Dec and the row is Oct/Nov/Dec: start year;
2. start date unknown (`-`), row in Dec, end month Jan: end year minus one;
3. otherwise the end year.
So a December transaction on a statement whose (known) period starts in early
January is dated a year late.

### Several statements in one file

Split at every `Account Summary` (5100-5109); at each `New Statement` marker the
period and the slip's name and card number are re-read (5249-5275). The `-`
(unknown start) case is handled only for the first statement in a file: the
`New Statement` branch always reads +2, +3, +5, +6.

### Lines it drops

- everything outside `Date Processed` ... `Carried forward` / `Closing Balance`
  on each page (headers, footers, the slip, the summary);
- the words `Balance` and `$` (5175);
- the `Opening Balance` line (before the first month word, `sLoopToDate`);
- the processed date and the card-used word of every row (5306, 5335);
- rows whose details came out empty (5428).

## 2. Every special case, and why it exists

| # | Lines | What | Why (the real-world quirk) |
|---|---|---|---|
| 2.1 | 5203-5212 | `Account Summary -` with no start date | A card's first statement has no previous statement date. |
| 2.2 | 5391-5395 | December row, unknown start, January end: previous year | The first statement of a card opened in December. |
| 2.3 | 5296-5308 | Row ends when the word 3 ahead is a month; then skip one word | The card-used column (which card of the account was used) sits after the balance. |
| 2.4 | 5331-5337 | Amount ends when the word 2 ahead is a month; then skip two words | The processed date sits between the amount and the balance in the word order. |
| 2.5 | 5323 | Details end at a two-decimal figure followed by `CR` or by a word, then a month | The amount is detected from what follows it (the processed date). |
| 2.6 | 5175 | Drop `Balance` and `$` | Heading words after `Date Processed`. |
| 2.7 | 5121 | `Payment Advice` not after `the` | Text elsewhere mentions "the Payment Advice". |
| 2.8 | 5229 | Account number = last 19 characters | The slip prints the card number last, right before `Instructions`. |

Weaknesses found while building the lookalikes (QVF behaviour, not the new tool's):
- the final row of each statement: the balance column only ends at end of data, so
  the card-used word is appended to the balance (`1,234.56 3456`); harmless as a
  number (`1234.563456` rounds to the right cents) unless the balance is `CR`,
  when the 3-character `CR` strip removes the wrong characters;
- in a file of several statements, the last row before a `New Statement`
  likewise takes the card-used word into its balance;
- rule 2.2 needs an unknown start: a late-December transaction on a statement
  whose period starts in January is dated a year late (lookalike asb_visa_7);
- a statement with no transactions gives the row loop no month word to find.

## 3. Output columns the QVF fills for this type

| Column | How |
|---|---|
| Sort Number | `RowNo()` over the whole load (1557) |
| File Name, Doc Reference Bank Statement, Doc Reference Bank Voucher | from the upload form (1802-1813, 1830-1840) |
| Row ID | `<file number>-<row number>`; row 1 is the added Opening Balance row |
| Bank | `ASB` (5052) |
| Account Name | slip words from `Advice` to `Customer` |
| Account Number | last 19 characters before `Instructions`: the masked card number |
| Other Party Account Name / Number | empty for PDFs |
| Date | the transaction date (first date), `DD/MM/YYYY` |
| Transaction Time | empty |
| Year, Tax Year | `Year(Date)`; Tax Year = year - 1 for Jan-Mar |
| Description as per bank statement | the details words (processed date and card-used word removed) |
| Transaction Type | `Withdrawal` / `Deposit` / `Unidentified` from the sign |
| Transaction Code, Code Description, Transaction Category | keyword lookup in the "Transaction Codes" workbook; 200 / 400 `To be done` by default; 199 / 399 inter-account transfers by own account numbers (7539-7597) |
| Amount | ABSOLUTE value, 2 dp |
| Balance | the printed running balance, owed = negative, in printed order (1150-1170) |
| Opening Balance row | added per file: first balance - first amount, Type `Deposit` (1172-1190) |
| Balance Check, Balance Pass | forward recalculation from the opening row against the printed balances: a real per-row check here, because the balances are printed |

## 4. The lookalikes (`gen/make_asb_visa.py`, `sets/visa1/asb_visa_*.pdf`)

| Case | Tests |
|---|---|
| asb_visa_1 | plain month, payment CR, interest |
| asb_visa_2 | two pages with `Carried forward`, two cards in the card-used column, foreign-currency amounts at the end of the details, refund CR |
| asb_visa_3 | period 16 Dec 24 - 15 Jan 25, first rows dated before the period |
| asb_visa_4 | first statement of a new card: `Account Summary - 09 Jan 25`, opening 0.00, December and January rows (2.1, 2.2) |
| asb_visa_5 | overpaid: the running balance crosses from owed into credit (balances `CR`) |
| asb_visa_6 | two consecutive statements in one file |
| asb_visa_7 | period 03 Jan 25 - 02 Feb 25 with a row dated 30 Dec (the QVF dates it a year late) |
| asb_visa_8 | no transactions: dormant card in credit |

The rows are drawn in the QVF's word order (date, details, amount, processed
date, balance, card), and the table heading's `Card` label sits on the line above
so that in reading order the heading ends with `Date Processed Balance`.
`make_asb_visa.py` checks the `Account Summary` words, the slip's name and
19-character card number, and runs a simplified copy of the QVF row loop
(5199-5467) on the PDF's words: all rows of asb_visa_1-6 read like the answer key
(one final `CR` balance aside, as above), asb_visa_7's 30 Dec row comes out a year
late, asb_visa_8 gives nothing (`sets/visa1/qvf_lite_asb_visa.csv`).
