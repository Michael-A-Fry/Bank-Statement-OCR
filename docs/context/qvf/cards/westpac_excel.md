# Format card: Westpac - Excel (`westpac_excel`)

QVF section: script.qvs lines 2414-2519 (tab "Westpac"), chosen on the form as
`Westpac - Excel`. Shared parts: cards/SHARED.md. Output fields: cards/OUTPUT.md.

## What the file is

- An `.xlsx` (Qlik `ooxml`) whose sheet holding the account rows is named
  **`Sheet1`** (2423) and is also the FIRST sheet (the transaction load names no
  sheet, 2444). Hyphenated column names suggest a bank data extract rather than an
  internet-banking export.
- One account per file, **oldest first** (opening = first row's balance minus its
  amount, 2505-2506).

## Landmarks and page furniture

- **Account details** (2419-2429): column B read with no labels. **B1 = account
  name**, **B2 = account number** (A1 / A2 hold their labels).
- **Heading row = row 3** (`header is 2 lines`, 2444). Fixed: there is no search.

## Table columns (by name; the order is not fixed by the script)

| Column | Used as |
|---|---|
| `Date` | the date (a real Excel date; `Year(Date)` is used directly) |
| `Amount` | signed: `> 0` Deposit, otherwise Withdrawal (2439) |
| `Running-Balance` | Balance; the text `.` means 0.00 (2441) |
| `Source-Type` | first part of Details (a short code: BP, AP, DC, DD, EFTPOS, ...) |
| `This-Party-Reference`, `This-Party-Desc`, `This-Party-Code` | Details |
| `Other-Party_Name` (an UNDERSCORE, unlike the others) | last part of Details |
| `Other-Party-Account-Number` | Other Party Account Number (2442) |

Details = `Source-Type & ' ' & This-Party-Reference & ' ' & This-Party-Desc & ' ' &
This-Party-Code & ' ' & Other-Party_Name` (2438). Because `Source-Type` comes first,
a bill payment's Details starts with `BP`, which is exactly what the shared BP rule
looks for (2450-2454).

## Money in and out, zero balances

- One signed `Amount`; zero counts as a Withdrawal; overdrawn balances are negative.
- **A zero running balance can arrive as `.`**: `If("Running-Balance" = '.', 0.00,
  "Running-Balance")` (2441). Qlik compares the cell's displayed text, so this
  catches both a text cell holding `.` and a numeric 0 under a number format such as
  `#,###.##` (which displays zero as `.`). Real-world reason: the extract formats the
  balance column with a format that prints nothing but the point for zero.

## The year

In every date cell.

## Several statements in one file

Not handled: one file is one statement, one opening row.

## Lines it drops

None: every row under the heading is loaded (there is not even a `Where` on the
date, so a stray note row under the table would become a transaction).

## Special cases, with the reason

| Lines | Special case | Why |
|---|---|---|
| 2419-2429 | name and number from B1 / B2 of `Sheet1` | the extract prints them above the table |
| 2441 | `.` read as 0.00 | zero balances print as a bare point |
| 2438 | `Other-Party_Name` spelt with an underscore | the extract's own heading; the script must match it exactly |

## Output columns filled for this type

- `Bank` = `Westpac - Excel`; `Account Name` = B1; `Account Number` = B2.
- `Other Party Account Name` = ''; **`Other Party Account Number` =
  `Other-Party-Account-Number`**; `Transaction Time` = ''.
- Details as above; Transaction Type from the sign; key-word codes, BP rule, 199/399
  transfer matching, 200/400 defaults; `Year`, `Tax Year`; Amount unsigned at the
  end; Balance as printed; Opening Balance row (code 100) = first Balance - first
  Amount (2482-2509); `Balance Check` / `Balance Pass`.

## Lookalikes (sets/excel/, generator gen/make_westpac_excel.py)

| Case | What it tests |
|---|---|
| westpac_excel_1 | plain |
| westpac_excel_2 | balance brought to exactly zero twice, each zero a TEXT cell `.` |
| westpac_excel_3 | overdrawn throughout (negative balances) |
| westpac_excel_4 | crosses a new year (5 Dec - 4 Jan); interest and RWT lines |
| westpac_excel_5 | new account opened at zero; balance column formatted `#,###.##`, so a zero balance DISPLAYS as `.` while the cell holds 0 |
| westpac_excel_6 | no transactions (account rows and heading only) |
| westpac_excel_7 | into overdraft and back; account name with an apostrophe and an ampersand |

`Sheet1` first, a second `Sheet2` holding only the synthetic notice.

## Measured (score_auto.R, cold = trained)

| Case | Outcome | Cell |
|---|---|---|
| westpac_excel_1, _3, _4, _5, _7 | proven | auto_right |
| westpac_excel_2 | check ("Nothing on the statement proves which column is which") | check_right |
| westpac_excel_6 | unread ("No row of the file has a date with a figure beside it") | unread |

Counterfactual: westpac_excel_2 with `.` read as 0 is proven. No AUTO_WRONG.

## Root causes and fixes (general rules)

1. **A `.` zero balance makes the balance column text** (S2). A money cell must hold
   a digit (`R/auto_read_tabular.R:18` and `:76`), and a column is money only when
   EVERY cell is (`:197`), so `Running-Balance` becomes the extra `text2` and the
   statement has no running balance to prove with. Rule: *in a column whose other
   cells are all figures, a cell holding only `.` (what `#,###.##` prints for zero)
   or a lone `-` is the figure zero; the arithmetic then proves or refutes it.*
2. **A statement with no rows is "couldn't read"** (S3, park; as bnz_excel_6).
   `R/auto_read_tabular.R:109` / `:317`. Rule: *a heading row with nothing under it
   is a statement with no transactions.*

## Output fields (new tool) for this type

Measured on westpac_excel_1: `other_party` = `Other-Party-Account-Number` (the heading
contains "other party", so the ACCOUNT NUMBER column is taken as the other party);
description = `Other-Party_Name` only, so fee, interest and deposit rows have an
EMPTY description (6 of 22); `type` = Source-Type; `This-Party-Desc` -> extra
`text1`; `This-Party-Code` (card digits) is taken as a figure column (`other1`) on
westpac_excel_1 and as `code` on westpac_excel_2, depending on its cells;
`reference` = This-Party-Reference. Header account name / number empty although
B1 / B2 print them. See cards/OUTPUT.md.
