# Format card: Kiwibank - Excel (`kiwibank_excel`)

QVF section: script.qvs lines 2196-2413 (tab "Kiwibank"), chosen on the form as
`Kiwibank - Excel`. Shared parts: cards/SHARED.md. Output fields: cards/OUTPUT.md.

## What the file is

- An `.xlsx` (Qlik `ooxml`), first sheet. The column names (`KiwiAcc`,
  `ProcessDate`, `ReceiptTime`, `Narration1`, `PayingBankDRN`, `PayeeDetails`,
  `ThisParty...`, `OtherPartyAcc`, `Running Balance`) are a bank data extract, not
  an internet-banking statement export.
- Transactions **newest first**: the script reverses the file (`Order By RowID desc`,
  2330) before numbering rows and taking the opening balance from the oldest row.
- **One file may hold several accounts.** The account number is on every row
  (`KiwiAcc`), and each distinct account becomes its own statement.

## Landmarks and page furniture

The sheet is first read with no labels, columns A, B, C (2203-2210), to find the
row whose column A is `Customer`: that row is the heading row (2212). Only four
shapes are known (2214-2292):

| Heading row | Preamble | Account holder name | Columns |
|---|---|---|---|
| 1 | none | none (`''`, 2216) | full set |
| 3 | rows 1-2 | `B2 & ' ' & C2` (2236) | full set |
| 4 | rows 1-3 | `B2 & ' ' & C2` (2256) | **no** `ThisPartyParticulars` / `ThisPartyCode` / `ThisPartyReference` (2266) |
| 8 | rows 1-7 | `B7 & ' ' & C7` (2276) | full set |

Any other heading row loads no data and the reload fails. Why four branches: four
versions of the extract were seen, with different preambles, and one without the
"this party" columns. The holder name is one per FILE, even when the file holds
several accounts.

Preamble rows should not be empty: Qlik numbers the rows it loads, so a fully blank
row could move the `Customer` row's number off the four known values.

## Table columns (by name; the order is not fixed by the script)

| Column | Used as |
|---|---|
| `Customer` (first column, A) | only to find the heading row |
| `KiwiAcc` | Account Number, per row; splits statements (2314-2315) |
| `ProcessDate` | the date, `Date#(ProcessDate, 'YYYY-MM-DD')` (2222) |
| `ReceiptTime` | Transaction Time (2223), the only type that fills it |
| `Narration1`, `PayingBankDRN`, `PayeeDetails`, `ThisPartyParticulars`, `ThisPartyCode`, `ThisPartyReference` | joined with spaces into Details (2226; heading-row-4 shape: the first three only, 2266) |
| `OtherPartyAcc` | Other Party Account Number, '' when empty (2227) |
| `Amount` | signed: `> 0` Deposit, otherwise Withdrawal (2224) |
| `Running Balance` | Balance (2228) |

## Money in and out

One signed `Amount`. Zero counts as a Withdrawal. Overdrawn balances are simply
negative numbers.

## The year

In every date (`YYYY-MM-DD`). The text is parsed with `Date#`; a real date cell
formatted `yyyy-mm-dd` gives the same text to Qlik and also works.

## Several statements in one file

- `Statement ID Final = AutoNumber(KiwiAcc & '-' & i) + i - 1` (2314): one statement
  per account, in the order accounts appear after the reversal (so the account
  printed LAST in the file gets the first Statement ID).
- `Statement Row ID Final = AutoNumber(KiwiAcc & '-' & RowID, per account) + 1`
  (2315): rows numbered from 2 within each account, oldest first.
- One Opening Balance row per account (2339-2375): for every row numbered 2, the
  opening is that row's `Running Balance - Amount`.
- The file counter `i` then jumps by the number of accounts minus one (2377), so the
  next file's statements are numbered after these.

## Lines it drops

None: every row under the heading is a transaction. No totals, no pending block.

## Special cases, with the reason

| Lines | Special case | Why |
|---|---|---|
| 2203-2212 | heading row found by searching column A for `Customer` | the extract comes with 0, 2, 3 or 7 preamble rows |
| 2252-2270 | a shape without the "this party" columns | an older / narrower version of the extract |
| 2236, 2256, 2276 | holder name assembled from two cells | first name and last name are separate cells |
| 2330 | `Order By RowID desc` | the file is newest first |
| 2314-2315, 2339-2377 | per-account statement numbering and opening rows | one extract can cover several accounts |
| 2227 | null `OtherPartyAcc` written as '' | keeps the field text, never null |

## Output columns filled for this type

- `Bank` = `Kiwibank - Excel`; `Account Name` = the preamble names (or ''), the same
  for every account in the file; `Account Number` = `KiwiAcc` per row.
- `Other Party Account Name` = ''; **`Other Party Account Number` = `OtherPartyAcc`**.
- **`Transaction Time` = `ReceiptTime`** (no other type fills it).
- `Description as per bank statement` = the joined text columns; Transaction Type
  from the sign; codes / descriptions / categories from key words, BP rule, 199/399
  transfer matching (a description holding one of the run's own account numbers),
  200/400 defaults.
- `Year`, `Tax Year`; `Amount` (unsigned at the end); `Balance` = Running Balance;
  an Opening Balance row (code 100) per account; `Balance Check` / `Balance Pass`.

## Lookalikes (sets/excel/, generator gen/make_kiwibank_excel.py)

| Case | What it tests |
|---|---|
| kiwibank_excel_1 | heading on row 1, no preamble |
| kiwibank_excel_2 | heading on row 3, names in B2 / C2 |
| kiwibank_excel_3 | heading on row 4, no "this party" columns |
| kiwibank_excel_4 | heading on row 8 after seven preamble rows |
| kiwibank_excel_5 | two accounts in one file (KiwiAcc changes) |
| kiwibank_excel_6 | crosses a new year; into overdraft (negative balance) and back |
| kiwibank_excel_7 | heading on row 8; ProcessDate as real date cells, ReceiptTime as Excel times |

All newest first, signed amounts, `Customer` a 7-digit number. Two-account answer key:
`accounts` + `account_index` on rows (the make_layouts convention), rows in printed
order.

## Measured (score_auto.R, cold = trained)

| Case | Outcome | Cell |
|---|---|---|
| kiwibank_excel_1, _2, _3, _4, _6, _7 | proven | auto_right |
| kiwibank_excel_5 | check ("The balance does not add up at row 18": the second account starts) | check_right |

Counterfactual: each account of kiwibank_excel_5 read alone is proven. No AUTO_WRONG.

## Root cause and fix (general rule)

- **Two accounts in one sheet are read as one statement** (S2). The spreadsheet
  reader reads one table as one statement; the `one_statement` check
  (`R/auto_read.R:1528-1539`, `.ar_ends_inside`) looks only for opening / closing
  rows inside the table, so on kiwibank_excel_5 it even reports "The table holds one
  statement of one account" while the KiwiAcc column holds two numbers. The broken
  balance chain catches it, so it is safe. Rule: *when a spreadsheet column holds
  account numbers and more than one distinct value, split the rows by that column
  and prove each account's rows as its own statement; never say "one account" while
  such a column holds two.*

## Output fields (new tool) for this type

Measured on kiwibank_excel_2: description = `Narration1` only; `other_party` =
`PayeeDetails`; `particulars` = `ThisPartyParticulars`; `reference` =
`ThisPartyReference`; but `ThisPartyCode` lands in an extra `text4` (the heading is
matched with `\bcode\b` on the lower-cased run-together name at
`R/auto_read_tabular.R:527`, so "thispartycode" is missed; rule: *name text columns
from their heading's words, split at capitals, hyphens and underscores*, S3); `KiwiAcc` -> `text1`, `ReceiptTime` -> `text2`, `PayingBankDRN` -> `text3`,
`OtherPartyAcc` -> `text5`, `Customer` -> `other1`. Extras are named `textN` /
`otherN`, not by their headings, and go only to `feed/extras/`. So the Qlik feed has
no account number per row, no transaction time and no other-party account number
for this type. Header `account_number` / `account_name` are empty.
