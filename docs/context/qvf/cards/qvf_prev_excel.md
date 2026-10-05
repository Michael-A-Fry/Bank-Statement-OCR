# Format card: N/A - Previous conversion (`qvf_prev_excel`)

QVF section: script.qvs lines 1858-2030 (tab "Excel"), chosen on the form as
`N/A - Previous conversion`. Not a bank's file: it is the QVF's OWN output, the
"Statement Data" table downloaded to Excel, corrected by an analyst and uploaded
again. Shared parts: cards/SHARED.md. Output fields: cards/OUTPUT.md.

## Why it exists

It is the QVF's correction loop. Rows the QVF could not decide come out as
`Unidentified` (type, amount or balance). The analyst downloads the table, fixes
those cells in Excel, uploads the workbook as a "previous conversion", and the QVF
re-signs, re-categorises, recomputes missing balances and re-checks. It is also how
several old conversions are combined into one run.

## What the file is

- An `.xlsx` (`ooxml, embedded labels`, 1917-1918), first sheet, headings on row 1.
- The columns the script reads (by name, 1895-1916): `File Name`, `Row ID`, `Bank`,
  `Account Name`, `Account Number`, `Other Party Account Name`, `Other Party Account
  Number`, `Transaction Type`, `Description as per bank statement`, `Date`,
  `Transaction Time`, `Amount`, `Balance`, `Doc Reference Bank Statement`, `Doc
  Reference Bank Voucher`, `Year`, `Tax Year`. Every other column of the download
  (`Sort Number`, `Code Description`, `Transaction Category`, `Transaction Code`,
  `Balance Check`, `Balance Pass`) is IGNORED, so an analyst's edits to the codes or
  categories are lost; only edits to type, amount, balance and text survive.
- `Amount` is UNSIGNED (the QVF writes `Fabs`); `Transaction Type` carries the sign.
- The download's row order follows the on-screen table, which sorts on `Row ID` as
  TEXT: `1-1, 1-10, 1-11, ..., 1-2, 1-20, 1-3 ...`. The script does not care (it
  sorts on the numbers it splits out of Row ID), and `Sort Number` exists so a
  person can put the rows back in order in Excel.

## How it is read

| Lines | Step |
|---|---|
| 1919 | the old `Opening Balance` rows are dropped |
| 1898-1899 | `Row ID` "s-r" split into statement s (shifted by the file counter: `s - 1 + i`) and row r |
| 1911 | Amount re-signed: `-Amount` for a Withdrawal |
| 1912 | `Balance = 'Unidentified'` becomes null |
| 1906-1907 | Code Description and Category recomputed from the key words, using the (possibly corrected) Transaction Type |
| 1886-1894 | Transaction Code from the description; the BP rule |
| 1877-1879 | 200 / 400 / To be done / Unidentified defaults |
| 1922-1960 | rows sorted newest first; every null balance recomputed backwards from the next known one (`Balance(k-1) = Balance(k) - Amount(k)`) |
| 1994-2023 | one new Opening Balance row (code 100) per statement = row 2's balance minus its amount |
| 1988-1992 | the file counter `i` jumps by the number of statements in the workbook |

## Lookalikes (sets/excel/, generator gen/make_qvf_prev_excel.py)

| Case | What it tests |
|---|---|
| qvf_prev_excel_1 | one statement of 8 rows (Row ID text order = numeric order) |
| qvf_prev_excel_2 | one statement of 20 rows in the table's Row ID TEXT order |
| qvf_prev_excel_3 | two statements (two accounts), put back in Sort Number order |

All 23 columns of the Statement Data table in its order, Opening Balance rows
included, `Balance Check` equal to `Balance`, `Balance Pass` = Pass. Answer key:
the transactions only (no Opening Balance rows), signed, in printed order.

## Measured (score_auto.R, cold = trained)

| Case | Outcome | Cell | Why |
|---|---|---|---|
| qvf_prev_excel_1 | check | check_wrong | the QVF's Opening Balance row is read as a transaction (9 rows for 8); "1 unsigned amount could take either sign" |
| qvf_prev_excel_2 | check | check_wrong | row order scrambled by the text sort; read as rows printing figures in both money columns |
| qvf_prev_excel_3 | check | check_wrong | as _2, plus two statements |

No AUTO_WRONG: nothing a QVF export holds was converted automatically.

## Root causes (S3, park unless re-reading old QVF exports is wanted)

- The `Deposit` / `Withdrawal` words ARE in the tool's marker vocabulary, but the
  marker column is glued to the NEAREST money column (`R/auto_read_tabular.R:367-371`),
  here `Transaction Code`, not `Amount`. Rule: *a sign-word column signs the money
  column whose arithmetic it completes, not merely the nearest one.*
- The Opening Balance row is not recognised as an opening line because a row's label
  is all its text cells joined (`R/auto_read_tabular.R:110-114`), and this row
  carries file name, bank, account and category text too. Rule: *a row whose
  description cell is exactly an opening / closing label is that anchor, whatever its
  other cells hold.*
- A QVF export is not a bank statement. If old conversions are to be re-read, the
  right tool is a small importer for the QVF's own 23-column schema (it carries Row
  ID, Sort Number and the sign), not the statement reader.
