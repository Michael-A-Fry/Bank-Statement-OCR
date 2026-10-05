# Format card: Kiwibank - PDF (transaction enquiry report)

Type key `kiwibank_pdf`. QVF section `///$tab Kiwibank - PDF`, script.qvs lines 6497-6965.
The form value is `vStatementType = 'Kiwibank - PDF'` with a `.pdf` file.

## What the statement is

This is not Kiwibank's customer statement. It is a bank-produced **transaction
enquiry report**: a header naming the customer, the account and an "Enquiry
Period", then every transaction in that period, **newest first**, with a running
balance on every row. One report can hold several enquiries (several periods of one
account, or several accounts of one customer). It prints **no opening balance, no
closing balance and no totals**: the only arithmetic is the running balance.

## How the QVF reads it

The PDF connector hands the script one word per row, in reading order (`WordPosition`).
The section never uses x positions. Everything is found from neighbouring words.

### Landmarks it searches for

| Landmark (exact words, case matters) | Lines | Used for |
|---|---|---|
| `Enquiry` `Period` | 6528-6535 | One per statement. Each one becomes a `New Statement` marker. The count is `vNumberOfStatements`. |
| `Name` ... `Address` | 6538-6555, 6649-6653 | The account name is every word between `Name` and `Address`. |
| `Account` `No` ... `Enquiry` | 6557-6572, 6655-6659 | The account number is every word between `No` and the next `Enquiry`. |
| `Credit` `Amt` | 6580-6585 | Table start on a page with the column heading. The **one word after `Amt` is also skipped** (`WordPosition + 1`, line 6583): that is the `Balance` heading. |
| `33` `55` | 6585 | Table start on a page **without** a column heading: the last two groups of the phone number `0800 11 33 55` in the page header. The word after `55` is skipped too: that is `www.kiwibank.co.nz`. |
| `www.kiwibank.co.nz` (not straight after `55`) | 6585 | Table end on every page: the footer. |

How starts and ends are paired (6574-6608): the script keeps each start plus the
landmark straight after it (6576-6578), then keeps every end plus the start straight
before it (6589-6593). On a page that has both the phone line and the column
heading, only the **later** start (the heading) survives. That is how page 1, which
has both, and a continuation page, which may have only the phone line, are both
handled. `sAddToPairs` (194-211) then takes every word strictly between each start
and its end. The landmark words themselves are dropped (6615).

### Page furniture, in reading order

1. Top of every page: `Kiwibank`, then `0800 11 33 55` directly followed by `www.kiwibank.co.nz`.
2. Page 1 of each statement: a title, then `Name <holder>`, `Address <lines>`,
   `Account No <number>`, `Enquiry Period <dd Mon yyyy> to <dd Mon yyyy>`.
3. Column heading, one line: `Posted  Date  Details  Debit Amt  Credit Amt  Balance`.
   It may be printed on page 1 only, or on every page.
4. Rows.
5. Footer that **starts** with `www.kiwibank.co.nz` (page number after it).

Anything printed between the last row and the footer's `www.kiwibank.co.nz` would be
glued onto the last row's balance. Anything between the header's
`www.kiwibank.co.nz` and the first row on a page with no heading would be glued to
the previous row. So the format has nothing there.

### Table columns, in order

| # | Column | Printed as | QVF |
|---|---|---|---|
| 1 | Posted | `dd Mon yyyy` (3 words) | **Skipped**: `vPosition + 3` at the start of every row (6647, 6679, 6726). |
| 2 | Date (transaction date) | `dd Mon yyyy` | Kept. The column ends at the 4-digit year (6734). Parsed `DD-MMM-YYYY` (6783-6789). |
| 3 | Details | free text | Ends when the next word starts with `$`, the one after starts with `$` or `-$`, and the third has no `$` (6743). |
| 4 | Debit Amt **or** Credit Amt | `$1,234.56` | One figure per row, in whichever column. The QVF cannot tell which (no x position). Ends when the next word starts with `$` or `-$` (6751). `$` is purged (6805-6809). |
| 5 | Balance | `$1,234.56`, `-$123.45` when overdrawn | The row ends when the word two ahead is a month name, i.e. the next row's posted date, or the next word is `New Statement` (6716). |

Every row must have exactly two `$` figures: an amount and a balance. A row with an
empty amount, or with a `$0.00` printed in the other money column, would break the
walk.

### Money in and money out

Word order loses which column a figure sat in, so the sign comes from two places:

1. **Description keywords** (6791-6801, case-insensitive `WildMatch`).
   Withdrawal: `PAY *`, `TRF *`, `*BILL *`, `*FEE *`, `POS W/D *`, `ATM W/D *`,
   `* DEBIT *`, `CASH WITHDRAWAL`, `* FEES *`, unless the text has `REVRSL`.
   Deposit: `TRANSFER FROM *`, `Direct Credit *`, `CASH DEPOSIT*`, `FROM *`,
   `POS DEP *`, `*REVRSL*`.
2. **Every other row**: the signed movement is this row's balance minus the older
   row's balance (6924-6937, `Balance - Previous(Balance)` with the rows put oldest
   first). The **oldest row** has no older balance, so its amount stays unknown
   ("Unidentified") unless its description hit a keyword, or it is exactly 0
   (`If(Amount=0, Amount, Null())`, 6933).

Quirks of the keyword rule: `MONTHLY ACCOUNT FEE` (no word after `FEE`) is not
matched, and falls back to the balance. A credit such as `VISA DEBIT REFUND` would
be matched as a withdrawal. A wrong keyword sign is caught later by the balance
check (below), because the opening comes from a printed balance.

### The year

It is printed in every date (`dd Mon yyyy`). There is no inference. A period that
crosses a new year needs nothing special.

### Several statements in one file

- Each `Enquiry Period` is a `New Statement` marker. The markers are merged into the
  word stream at their own positions (6618-6626), so each one sits just before its
  statement's rows.
- In the row loop (6675-6691), a `New Statement` word triggers a skip to the next
  date. Then `+3` skips the Posted date, the statement count goes up by one, and the
  name and number are re-read for that statement.
- **A statement with no rows** is handled: while seeking the next date,
  `sLoopToDate` counts every further `New Statement` it passes (lines 251, 291), so
  an empty enquiry still uses up its number and the next one gets the right name
  and number.
- The filter at 6626 (`WordPosition < Peek(... 'Transaction_Data_Step2')`) is meant
  to drop a `New Statement` marker that comes after the last table word, which would
  otherwise send `sLoopToDate` looking for a date forever. **As written it peeks a
  table already dropped at 6600**, so it may evaluate to null and drop *every*
  marker, in which case multi-statement files would be read as one. This needs
  checking on the server.
- Statements are then separated by **account number + statement number**
  (`NoCount`, 6873-6880). Each gets its own `Statement ID` (`i + AccNo - 1`, 6887-6961,
  and the special ordering in `sEndofData` at 1239-1245).
- Each statement must start on a new page. A second statement's `Credit Amt` start,
  on the same page as the end of the first, would win the start/end pairing, and the
  first statement's last rows would be lost.
- Only one `Name`, `Address`, `Account No` and `Enquiry` per statement is allowed.
  A repeated `Account No` on a continuation page would shift every later statement's
  account number. The word `Enquiry` anywhere else (for example a title "Transaction
  Enquiry") would break the account-number pairs.

### Opening and closing balance

- Closing = the balance on the first printed row (the newest), 6912-6921.
- "Opening" = the balance on the **last printed row** (6906-6910). That is the
  balance *after* the oldest transaction, not before it. `sBalanceCheck` corrects
  for it for this type only: opening = that balance minus the oldest row's amount
  (1511-1518).
- The printed balances appear not to be carried through
  (`If("Statement Row ID" = 1, Balance)`, 6934, and the row ids start at 2). Balances
  are rebuilt in `sEndofData` from the closing balance upward (1267-1360), and an
  `Opening Balance` row is added (1448-1462).

### Lines it drops

- Everything outside the start/end pairs: header, address block, column headings, footers.
- The Posted date of every row.
- The `Balance` heading word straight after `Credit Amt`, and the `www.kiwibank.co.nz` straight after `55`.
- A "no transactions" message inside an empty enquiry is skipped while seeking the next date.
- There are no totals, carried-forward or interest-summary lines in this format. Interest
  and withholding tax are ordinary rows.

## Output columns the QVF fills for this type

Final assembly is at 7479-7642 and `sBalanceCheck` at 1474-1590.

| Output column | How |
|---|---|
| File Name, Doc Reference Bank Statement, Doc Reference Bank Voucher | From the upload form. |
| Sort Number | `RowNo()` over the final table (1557). |
| Row ID | `<Statement ID>-<Statement Row ID>`. Row 1 of each statement is the synthetic `Opening Balance` row. |
| Bank | `'Kiwibank'`. |
| Account Name | Words between `Name` and `Address`. |
| Account Number | Words between `Account No` and `Enquiry`. |
| Other Party Account Name / Number | Empty for PDFs (7512, 1142). |
| Date | The second date on the row (transaction date), `DD/MM/YYYY`. |
| Transaction Time | Empty for PDFs (7515). The details often carry a time (`-14:32`); it is not extracted. |
| Year, Tax Year | `Year(Date)`. The tax year is the year minus one for Jan-Mar (NZ April-March tax year), 7516-7517. |
| Description as per bank statement | The Details words joined by single spaces. |
| Transaction Type | `Withdrawal` (amount <= 0) / `Deposit` / `Unidentified` (unknown amount). |
| Transaction Code, Code Description, Transaction Category | Keyword lookup of the description in a user-maintained keyword workbook (deposit and withdrawal lists, 1646-1684, 7493-7522). Defaults `200` / `400` with "To be done" (7567-7569). `BP...` gives "Transfers to/from third parties" (7498-7502). A description that names **another account number of the same subject loaded in the same run** gives `199`/`399`, "Transfers in/out", "Inter-account transfers" (7541-7591). |
| Amount | **Absolute value** (`Fabs`, 1576). The sign lives in Transaction Type. |
| Balance | Rebuilt from the closing balance. |
| Balance Check, Balance Pass | The opening (corrected as above) plus the running sum, compared with Balance. Because the opening comes from a printed figure, a wrong keyword sign shows up as `Fail`. |
| (row) Opening Balance | A synthetic first row per statement: Details `Opening Balance`, type `Deposit`, Amount = Balance = opening. |
| Order | Rows are re-sorted **oldest first by date** (6953, 1239-1245), the reverse of the printed order. |

## Lookalikes (`gen/make_kiwibank_pdf.py` writes into `sets/kbwp/`)

Every file is checked by `qvf_walk()` in the generator, which re-does the QVF's word
walk on the PDF's words in reading order (and fails a negative control). All 8 pass,
so they carry the landmarks above.

| Case | What it tests |
|---|---|
| kbpdf_1 | One enquiry, one page, 18 rows. |
| kbpdf_2 | Three pages. Continuation pages have **no column heading**: the rows start straight after the `www.kiwibank.co.nz` header line (the `33 55` start). The enquiry period crosses a new year. Interest and withholding-tax rows. |
| kbpdf_3 | Overdrawn: the balance prints `-$`. Two pages, heading repeated. |
| kbpdf_4 | Long details wrap onto two or three lines, with the money on the **last** line of the row (the only wrapping the QVF's walk survives). Interest and withholding tax. The oldest row is a `$0.00` waived fee. |
| kbpdf_5 | Two enquiries of one account in one report, consecutive periods, page numbers running on. |
| kbpdf_6 | One report, three accounts, the same period, the **middle enquiry empty** ("No transactions found."), and transfers between the customer's own accounts. |
| kbpdf_7 | 110 rows over four pages. Many card rows posted 1-3 days after the transaction date; some transaction dates fall before the enquiry period. |
| kbpdf_8 | Three separately issued enquiries for one account, concatenated (each restarts "Page 1 of N"), Dec-Feb across a new year, chaining balances. |

Truth convention: date = the second (transaction) date, which is the one the QVF
keeps. Rows are in printed order (newest first). The opening is the balance before the
oldest row.

## Measured on the new tool (2026-10-05, `score_auto.R`, cold = trained)

8 statements: auto_right 4 (kbpdf_1, 2, 3, 7), AUTO_WRONG 0, check_right 3 (kbpdf_5, 6, 8),
check_wrong 1 (kbpdf_4), unread 0. The new tool picks the right date column (the heading
"Posted" ranks below "Date"), and reads newest-first rows, the `-$` overdraft, wrapped
details and continuation pages with no heading. The misses: a `$0.00` row on a
newest-first statement is nulled (`R/parse_pdf_table.R:1211-1236`); a report whose page
numbers run on is never split (`R/split.R:44-48`); and split statements with no printed
opening or closing cannot pass the bundle join (`R/convert.R:505-521`). The full
diagnosis is in the area report.
