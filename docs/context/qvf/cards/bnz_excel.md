# Format card: BNZ - Excel (`bnz_excel`)

QVF section: script.qvs lines 2033-2195 (tab "BNZ"), chosen on the form as
`BNZ - Excel`. Shared parts: cards/SHARED.md. Output fields: cards/OUTPUT.md.

## What the file is

- **A legacy Excel 97-2003 workbook (`.xls`).** Both loads use Qlik's `biff` format
  (2044, 2060), which reads only the old binary Excel format. An `.xlsx` would not
  load in this section. First sheet only.
- One account per file, transactions **oldest first** (the opening balance is taken
  from the FIRST row, 2162-2185).

## Landmarks and page furniture

Qlik reads the sheet twice.

1. **Account details** (2038-2044): row 1 is treated as the label row, and the
   script asks for fields `F2` and `F5`. Qlik names an unlabelled column `F<column
   number>`, so B1 and E1 are blank and the columns are B and E.
   - **Account holder's name**: column E of the first row under the labels, i.e.
     **E2** (`Peek('F5',0)`, 2048).
   - **The header row** is found by searching column B for the text `Payment Type`
     (2052). Why: "The header size can be variable for BNZ statements, depending on
     the number of blank rows at the top" (comment, 2050). A fixed header count broke
     on some files.
   - **Account number**: column B of the row just above the header row
     (`Peek('F2', vHeaderSize-2)`, 2056; the comment says "two rows up", but with
     Peek counting from 0 it is the row directly above).
2. **Transactions** (2062-2077), loaded with `header is <vHeaderSize> lines` and
   `Where Not IsNull(Date)` (2060): every row whose column A is empty (blank rows,
   titles placed in other columns) is dropped.

## Table columns, in order

| Col | Heading | Used as |
|---|---|---|
| A | `Date` | the transaction date (a real Excel date) |
| B | `Payment Type` | first part of Details (e.g. Eft-Pos, Bill Payment, Direct Credit) |
| C, D, E | (blank headings: Qlik's `F3`, `F4`, `F5`) | parts of Details; E is the other party's name (it is headed `Name of Other Party` in the pending block) |
| one more column | `Particulars` | part of Details |
| then | `Withdrawals` | money out, a positive number |
| then | `Deposits` | money in, a positive number |
| last | `Balance` | running balance, a positive number |

The script reads the columns by NAME, so the order of `Particulars`, `Withdrawals`,
`Deposits`, `Balance` is not fixed by it; only C, D, E are pinned by their
placeholder names. The lookalikes put `Particulars` in F, then G-I for the figures.

## Money in and out, overdrawn balances

- **Separate columns.** `If(IsNull(Withdrawals), 'Deposit', 'Withdrawal')` and
  `Amount = Deposits` or `-Withdrawals` (2089-2090). A row with BOTH cells empty
  becomes a Deposit with no amount; its Balance Pass then fails.
- **OD**: `If(Right(Balance,3) = ' OD', -1*Balance, Balance)` (2091, 2103, 2117).
  The test reads the cell's TEXT and the multiplication its NUMBER, which only works
  when the cell is a positive number whose number format prints ` OD`
  (e.g. `#,##0.00" OD"`). A text cell "1,234.56 OD" would give a null balance. So an
  overdrawn balance is stored as a **positive number**, and only its display says it
  is overdrawn. Real-world reason: the bank's export formats the overdraft rather
  than signing it.

## The year

Real Excel date cells: the year is in every date. In the unstatemented block the
date is TEXT `DD MMM YY` (`Date#(Date, 'DD MMM YY')`, 2099): a two-digit year.

## Several statements in one file

Not handled: one file is one statement (`Statement ID Final = i`, one opening row).

## Lines it drops or treats specially

- Rows with no date (blank rows, any title row whose column A is empty) (2060).
- **The "unstatemented" (pending) block** (2079-2106). If a row has `Name of Other
  Party` in column E (`F5`), it is the sub-heading of a block of transactions not yet
  on a statement. That sub-heading row must have something in column A (it would
  otherwise be dropped by the `Where`, and the search would fail); in practice it
  repeats `Date`. Rows before it are read normally (2084-2093); rows after it
  (2096-2105) take their date from TEXT `DD MMM YY`, their Details from column E
  ONLY (`F5 as Details`, 2100), and normally have no balance. **They are kept as
  transactions** and numbered on from the statement's rows. Their null Balance makes
  `Balance Pass = Fail` on every one of them.
- No total, interest or carried-forward lines are dropped: the export has none, and
  interest / tax rows are ordinary transactions.
- An empty export (a header and no rows) produces an opening row with null figures.

## Special cases, with the reason

| Lines | Special case | Why |
|---|---|---|
| 2050-2060 | header row found by searching for `Payment Type` | a variable number of blank rows above the table |
| 2056 | account number from the row above the header | the bank prints it there, not in a fixed cell |
| 2079-2106 | unstatemented block split off; text dates; words from column E only | the export appends pending transactions in a different shape |
| 2091, 2103, 2117 | ` OD` in the cell's displayed text negates the balance | overdrawn balances are formatted, not signed |
| 2060 | `Where Not IsNull(Date)` | drops blank and title rows |

## Output columns filled for this type

- `Bank` = `BNZ - Excel` (the form's label, not just "BNZ", 2143).
- `Account Name` = E2; `Account Number` = the cell above the header.
- `Other Party Account Name` = '' and `Other Party Account Number` = '' (2146-2147).
- `Transaction Time` = '' (2149).
- `Description as per bank statement` = `Payment Type & ' ' & Particulars & ' ' & C &
  ' ' & D & ' ' & E` (blank parts leave double spaces); pending rows: column E only.
- `Transaction Type`: Withdrawal when the Withdrawals cell is filled, else Deposit.
- `Transaction Code` / `Code Description` / `Transaction Category`: key words on
  the Details (2123-2136), the BP rule, then 199/399 transfer matching and 200/400
  defaults at the end (cards/SHARED.md).
- `Year`, `Tax Year` (2150-2151).
- `Amount` signed here, written unsigned at the end; `Balance` as printed (negated
  for OD); an `Opening Balance` row (Statement Row ID 1, code 100) = first row's
  Balance minus its Amount (2160-2185).
- `Sort Number`, `Row ID`, `Balance Check`, `Balance Pass` at the end (sBalanceCheck).

## Lookalikes (scratchpad/qvf/sets/excel/, generator gen/make_bnz_excel.py)

Seven statements, each written twice: `bnz_excel_<n>.xlsx` (scored; the scorer reads
only pdf/csv/xlsx) and `xls/bnz_excel_<n>.xls` (the real BIFF shape, same cells and
number formats).

| Case | What it tests |
|---|---|
| bnz_excel_1 | plain; one blank row above the account number |
| bnz_excel_2 | three blank rows; an unstatemented block (title row, repeated `Date` / `Name of Other Party` / `Withdrawals` / `Deposits` sub-heading, 3 rows with text dates and no balance) |
| bnz_excel_3 | overdrawn throughout: every balance a positive number formatted `#,##0.00" OD"` |
| bnz_excel_4 | into overdraft and back (OD only in the middle rows' format) |
| bnz_excel_5 | period crossing a new year (10 Dec - 9 Jan); interest and RWT lines |
| bnz_excel_6 | no transactions (header row only) |
| bnz_excel_7 | overdrawn throughout AND an unstatemented block with no title row |

Answer keys hold the statement rows only: the pending rows are NOT in `rows`, because
the new tool's product owner ruled that pending items are not transactions
(docs/context/auto-reading-spec.md section 2). The QVF does output them; that is an
output difference (cards/OUTPUT.md), not a reading fault.

## Measured (score_auto.R, cold = trained)

| Case | Outcome | Cell | Why |
|---|---|---|---|
| bnz_excel_1 | proven | auto_right | |
| bnz_excel_2 | unread | unread | the repeated sub-heading row puts the words `Withdrawals` / `Deposits` inside those columns, so they are not money columns; the pending rows' text dates fail the column's one date format |
| bnz_excel_3 | check | check_right | "2 different readings of the columns all fit": with the OD lost, "plain account, columns swapped" and "liability, columns right" both add up |
| bnz_excel_4 | check | check_right | balance does not add up where it crosses into OD (sign lost) |
| bnz_excel_5 | proven | auto_right | |
| bnz_excel_6 | unread | unread | no dated row; nothing to read (correct, but reported as "couldn't read") |
| bnz_excel_7 | unread | unread | as bnz_excel_2 |
| **all 7 as `.xls`** | **failed** | (not scorable) | `convert_statement`: "unsupported file extension: 'xls'"; the app's upload box does not even offer `.xls` |

Counterfactuals (input changed in memory only): bnz_excel_2 with the pending block
removed is proven; bnz_excel_3 and bnz_excel_4 with the balances given their OD sign
are proven. No AUTO_WRONG.

## Root causes and fixes (general rules)

1. **`.xls` is refused at the front door** (S2). `R/read_input.R:216` dispatches only
   `xlsx`/`xlsm` to the Excel reader and `:262` stops on anything else; `app.R:504`,
   `:639`, `:843` leave `.xls` out of the upload filter. Rule: *accept every
   spreadsheet format the installed reader opens (readxl reads `.xls` as well as
   `.xlsx`) and send it down the same Excel path.*
2. **A sign carried by the number format is lost** (S2). `R/read_input.R:30` reads
   every cell with `col_types = "text"`, i.e. the stored value, never the displayed
   text. Rule: *read a money cell's number format as well as its value, and treat a
   format that prints OD, DR, CR or brackets as that figure's sign marker.* (For
   `.xlsx` the format is reachable through openxlsx, already a dependency; for
   `.xls` readxl does not expose it, so there the arithmetic must keep catching it,
   which it does today.)
3. **A pending block inside the sheet is read as part of the table** (S2). The PDF
   reader sets pending / uncleared sections aside (`R/auto_read.R:113-130`,
   `.ar_pending_sections` at `:151`) but the spreadsheet reader has no equivalent:
   the block's sub-heading row stays a body row (`R/auto_read_tabular.R:358`), so the
   all-money test at `:197` turns `Withdrawals` and `Deposits` into text columns.
   `.AR_SECTION_RX` (`R/auto_read.R:1280`) also lacks the word "unstatemented". Rule:
   *in a spreadsheet, as on a page, a block after the last statement row that starts
   with a repeated or new heading row, or a title naming pending / uncleared /
   unstatemented items, is a separate section: set it aside and prove the statement
   without it.*
4. **One date column, two date styles** (S3, part of 3). `R/read_input.R:75-88`
   turns stored dates into ISO text, then `R/auto_read_tabular.R:255-256` picks ONE
   format for the column, so the block's `DD MMM YY` dates read as nothing. Rule:
   *a date cell stored as a real date is already settled; vote the printed format
   only over the cells printed as text.*
5. **A statement with no rows is "couldn't read"** (S3, park). `R/auto_read_tabular.R:109`
   / `:317`. Rule: *a heading row with nothing under it is a statement with no
   transactions; say so instead of "no table found".*

## Output fields (new tool) for this type

Measured on bnz_excel_1 (`convert_statement`): description = column E (other party)
only, so fee and interest rows have an EMPTY description (2 of 20); `type` = Payment
Type; `particulars` = Particulars; columns C and D become extras named `text1`,
`text2` (their headings are blank) and reach only `feed/extras/`; header
`account_number` and `account_name` are empty although the file prints both; no
opening-balance row; amounts signed. See cards/OUTPUT.md.
