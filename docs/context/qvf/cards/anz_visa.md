# ANZ Visa (QVF type "ANZ - Visa", type key `anz_visa`)

Script section: `script.qvs` lines 4120-5048, plus the shared subroutines it calls
(`sBalances` 136-187, `sAddToPairs` 194-211, `sCreateVariables` 217-227,
`sLoopToDate` 233-307, `sConcatenateStrings` 335-395, `sRowSetup` 401-418,
`sEndofRow` 1034-1075, `sEndofData` 1081-1470 with no type, so the "calculate the
balances" branch) and the final transform 7479-7642 with `sBalanceCheck` 1474-1590.

Everything below is read from the script. Where the script only implies a page
shape (it searches for words, never for positions), the shape is marked
*inferred*. The connector ("Mole") hands over every word with a `WordPosition`;
with the custom country pack an amount such as `1,234.56` arrives as ONE word
(comment at 2536).

## 1. What the statement looks like to the QVF

### Landmarks it searches for

| Landmark (exact words) | Lines | What it gives |
|---|---|---|
| `Period` ... first `Credit` after it | 4179-4205 | the statement period: the words between. Start month = word +2, start year = `20` & word +3, end month = word +6, end year = `20` & word +7 (4799-4807). So the period must print as `dd Mon yy - dd Mon yy` (2-digit years, one separator word) and be followed by a word `Credit` (inferred: "Credit Limit"). |
| every `Period` | 4207-4216 | one statement per `Period` word: this is how several statements in one file are split (a `New Statement` marker at each). |
| `Closing Balance` <amount> `Minimum Payment` (or `Make Payment`, `For cheques`) | 4218-4263 | the closing balance of each statement: only a `Closing Balance` directly followed by the payment wording counts, so other "Closing Balance" lines on the page are ignored. |
| `Account Number` ... first `For` or `Closing` | 4265-4292 | account number and name: the words between, minus `Account`, `Name`, `Visa`, `Platinum`. `Card Account Number` is excluded (4278), so a continuation-page header does not count. |
| `Enjoy Your` ... `Monthly CashBack` | 4294-4346 | the CashBack reward amount (the words with digits between). |
| `Card Number <4 groups> <NAME>` | 4348-4362, 4685-4743 | start of a cardholder's transactions (two shapes: after a heading, or straight after the previous row's amount, where the third group `****` is the marker). |
| `in NZ$` followed by `Page n of N` | 4376-4397 | start of the transactions on a page (the heading of the amount column, then the page number); the block starts 4 words after `NZ$`. |
| `Sundry` | 4401-4469, 4507-4646 | end of the card blocks, start of the account-level ("sundry") block. |
| block ends: `CashBack`, `Sundry`, `$`, `Page`, `Card Total`, words starting `%`, `Points Earned`, `Closing Balance`, `Credit Card Account` (not after `ANZ`), `We`, `Your`, `Opening`, `DummyLastString` | 4348-4646 | where a run of transaction words stops. A lone `$` word is an end marker and is also removed from the stream (4502). |
| `C` + 6 digits | 4401-4407, 4546-4552 | a printer's page code: transactions resume 3 words after it. |

### Page furniture (as the QVF expects it; *inferred* from the markers)

- Page 1: masthead, customer, a panel with `Statement Period`, `Credit Limit`,
  `Account Number`, `Account Name`, `Closing Balance`, `Minimum Payment`,
  `Payment Due`; an account summary box.
- The transaction table heading ends with the amount heading `... in NZ$`, and in
  reading order the page number `Page n of N` comes right after it.
- First table line: `Opening Balance` with the figure (skipped, see 2.4).
- One `Card Number <masked card> <NAME>` heading per cardholder, optionally a
  `Card Total` line after each cardholder's rows.
- A `Sundry` block (account-level items: payments, interest, fees).
- Continuation pages start with `Credit Card Account Number <card>` (an end
  marker, not an account number) and repeat the heading and page number.
- After the table, a notice starting `We ...` or `Your ...` closes the last block.

### Table columns, in order

`Date` (transaction date, `dd Mon`), processed date (`dd Mon`, removed),
`Details`, `Amount` (4131-4137). No balance column.

### Money in and out

One amount column. A figure followed by `CR` is a credit, kept positive; every
other figure is made negative (4979-4993). The closing balance follows the same
rule in `sBalances` (152-170): `CR` = in credit (positive), otherwise owed
(negative), `Unknown` when no figure. So the QVF's sign convention is the
holder's: a purchase and an owed balance are negative.

### How the year is found

From the period words only (5001-5021):
1. period starts in Oct/Nov/Dec and the row is Oct/Nov/Dec: the start year;
2. the row is in Dec, neither the start nor the end month is Dec and both years
   are the same: start year minus one (a late December transaction on a
   January-February statement);
3. otherwise the end year.

### Several statements in one file

Split at every `Period` word (4207-4216). Each statement gets its own period,
account details (`StringCount` lookups, 4852-4872) and closing balance.

### Lines it drops

- the processed date of every row (4139-4161);
- `Opening Balance ...` (4878-4884);
- the `Card Number ...` heading words: up to 10 words after the `****` group with
  no digits or 4 digits, and up to 10 words before it (4685-4743);
- `Card Total`, `Sundry`, `Closing Balance`, page headers and footers: outside the
  word pairs;
- `$` words (4502), words starting `%IP%` / `%IX%` (PDF artefacts, 4502, 4636);
- every word after the last word containing a point, apart from `CR` (4656-4671):
  so the trailing `Closing Balance` label at the very end is dropped;
- rows whose details came out empty (`sEndofRow`, 1038).

## 2. Every special case, and why it exists

| # | Lines | What | Why (the real-world quirk) |
|---|---|---|---|
| 2.1 | 4139-4161 | Remove the second of two consecutive dates and its day | Every row prints the transaction date and the processed date; only the first is kept. |
| 2.2 | 4163-4173 | Add `DummyLastString` when the file ends with `CR` or a month | Some files end on a transaction with no trailing text, so no end marker closes the last block. (The third test, `Index(...,'.') = -1`, can never be true: `Index` returns 0.) |
| 2.3 | 4183, 4186 | Collapse repeated `Credit`, stop at `See Final` | The page prints "Credit" several times after the period (card name, limit); a `See Final ...` notice ends the search. |
| 2.4 | 4878-4884 | Column 1 = `Opening` -> jump to the next date | The opening balance line sits inside the table on the first page. |
| 2.5 | 4886-4910 | Skip leading date-only rows (first rows, first two statements) | Redacted statements: the details and amount are removed but the dates are still visible. |
| 2.6 | 4353, 4361 | Two shapes of the cardholder heading | `Card Number` after a heading starts a block; `Card Number` straight after the previous row's amount both ends one block (`Card` after a figure or `CR`) and starts the next (`****`). |
| 2.7 | 4685-4743 | Strip the card number and the cardholder name | The heading `Card Number 4xxx xx** **** nnnn NAME` sits in the transaction stream. |
| 2.8 | 4745-4795 | Move the NZ$ amount (and its `CR`) after `Incl Currency Conversion Charge <x>` | A foreign-currency purchase prints a second line under the description; in reading order it comes after the amount, so without this the second line would be read as part of the amount. |
| 2.9 | 4294-4346, 4831-4850 | Add a `CashBack Reward` row (positive, dated at the last row) for statements before 1 Mar 2014 | The reward was credited to the balance without a transaction line, so the backward-calculated balances would not reach the closing balance. Added only when the NEXT statement starts, so the last statement in a file never gets it. |
| 2.10 | 4401-4469 | `C` + 6 digits page code, `Amount Paid`, `For` | Page codes and the tear-off payment slip interrupt the transaction run. |
| 2.11 | 4512, 4509 | `Credit Card Account` shifted back 2 words, or 6 when `Page` is 6 words back | The previous page's `Page n of N` and the next page's `Credit Card Account` header sit between two pages of rows. |
| 2.12 | 4656-4671 | Drop words after the last figure | The table's closing label at the end of the data. |
| 2.13 | 4947 | Details end only at a figure of digits and one point with 2 decimals, followed by `CR` / a month / `New Statement` | The detection of the amount. A row amount printed with a thousands comma (`1,234.56`, one word from the connector) is NOT recognised, so the description would swallow it; the lookalikes print row amounts without a comma for that reason. Also a `CR` row whose description ends in a 2-decimal figure ends one word early, which is why the conversion charge must print with its `$` (`$3.00`) for a foreign-currency refund to read. |
| 2.14 | 5003-5017 | Year rules | see 1, "How the year is found". |
| 2.15 | 1198-1257 | Rows re-ordered by date within each statement; whole file treated as reverse order when the last row is dated before the first | Card statements list cardholder sections and the sundry block separately, not in date order. |
| 2.16 | 1265-1386 | Balances calculated backwards from each statement's closing balance; an `Opening Balance` row added (opening = first balance - first amount) | There is no running balance; the opening balance printed on the statement is never read. |

## 3. Output columns the QVF fills for this type

| Column | How |
|---|---|
| Sort Number | `RowNo()` over the whole load (1557) |
| File Name, Doc Reference Bank Statement, Doc Reference Bank Voucher | from the upload form (1802-1813, 1830-1840) |
| Row ID | `<file number>-<row number>`; row 1 is the added Opening Balance row (7606) |
| Bank | `ANZ` (4123) |
| Account Name | the words after the card number in the `Account Number ... Closing` run (4815) |
| Account Number | the digits, `*` and spaces of that run: the masked card number (4813) |
| Other Party Account Name / Number | empty for PDFs (7512, 1142) |
| Date | the transaction date, year from the period rules, `DD/MM/YYYY` |
| Transaction Time | empty (7515) |
| Year, Tax Year | `Year(Date)`; Tax Year = year - 1 for Jan-Mar (7516-7517) |
| Description as per bank statement | the details words joined by single spaces; a foreign-currency row includes `USD 30.00 Incl Currency Conversion Charge $x` |
| Transaction Type | `Withdrawal` (amount <= 0), `Deposit` (> 0), `Unidentified` |
| Transaction Code, Code Description, Transaction Category | keyword lookup of the description in the "Transaction Codes" workbook (7497-7521); defaults 200 / 400 and `To be done`; 199 / 399 `Transfers in/out`, category `Inter-account transfers` when the description contains one of the subject's own account numbers from any file in the load (7539-7597) |
| Amount | ABSOLUTE value, 2 dp (1576, 7621) |
| Balance | calculated backwards from the closing balance (owed = negative), in date order |
| Balance Check, Balance Pass | forward recalculation from the Opening Balance row, Pass/Fail per row (1474-1590); within one statement it cannot fail (same arithmetic both ways), across statements it catches a gap |

## 4. The lookalikes (`gen/make_anz_visa.py`, `sets/visa1/anz_visa_*.pdf`)

| Case | Tests |
|---|---|
| anz_visa_1 | plain month, one cardholder, refund CR, Sundry block (payment CR, interest) |
| anz_visa_2 | two cardholders each with a `Card Total` line, four foreign-currency rows on two lines (one a refund CR), a 1284.02 amount printed without a comma, 2 pages |
| anz_visa_3 | period 15 Dec 24 - 14 Jan 25, first rows dated 13-14 Dec (before the period) |
| anz_visa_4 | period 03 Jan 25 - 02 Feb 25, first row dated 30 Dec (rule 2.14, Dec on a January statement) |
| anz_visa_5 | three consecutive statements in one file, chained balances |
| anz_visa_6 | card paid into credit (closing CR); file ends on a CR row with nothing after it (2.2) |
| anz_visa_7 | no transactions: dormant card in credit, opening = closing |
| anz_visa_8 | first two rows redacted (details and amount removed, dates visible) (2.5) |
| anz_visa_9 | LOW REALISM: two 2013 statements with a CashBack reward credited outside the table (2.9) |
| anz_visa_10 | two cardholders with NO Card Total: the second `Card Number` heading follows the first cardholder's last amount (2.6) |

Each PDF draws the landmarks above in the reading order the script needs, and
`make_anz_visa.py` checks it: the period words and the `Credit` after them, the
account number the QVF would read, `Closing Balance ... Minimum Payment`,
`in NZ$ Page n of N`. It also runs a simplified copy of the QVF row loop
(4797-5044, with the processed-date removal and the FX re-ordering) on the
PDF's words: it reads every row of anz_visa_1-6 and 10 exactly like the answer
key, skips the two redacted rows of anz_visa_8 as the QVF does, and reads nothing
on anz_visa_7 (`sets/visa1/qvf_lite_anz_visa.csv`). The block-finding steps
(4348-4683) are not emulated.
