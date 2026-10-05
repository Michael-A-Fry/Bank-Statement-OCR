# Output schema: the QVF's, and the new tool's, field by field

Sources: script.qvs "Final Transform" / "End" (7479-7642), `sEndofData` (1081-1470),
`sBalanceCheck` (1474-1590), each type's section; the QVF's front end (the
"Statement Viewer" sheet, read from the .qvf's object definitions); the new tool's
`R/outputs.R`, `R/feed.R`, `R/schema.R`, `docs/operational/connecting-qlik.md`,
`docs/context/architecture/build-contract.md` sections 2-3 and 9; and conversions
of this set's statements with `convert_statement()` (outputs under
`scratchpad/qvf/convert_out/`).

## 1. The QVF's output

One table, `Transactions`, built by `sBalanceCheck` (1555-1580), stored as a QVD per
run (7630-7636; the file name carries the user's id and a timestamp). The app shows
it on the "Statement Viewer" sheet as the table "Statement Data" (all 23 fields),
which analysts download to Excel; that download is what the type `N/A - Previous
conversion` reads back (cards/qvf_prev_excel.md).

**Row model.** Every statement starts with an added row "s-1": Details `Opening
Balance`, Transaction Type `Deposit`, code `100`, Amount = Balance = the opening
balance. Transactions follow as "s-2", "s-3", ... oldest first. The sheet's KPIs
filter that row out by its description.

### The 23 fields, in the stored order

| # | Field | How it is made (lines) |
|---|---|---|
| 1 | `Sort Number` | `RowNo()` over the final table, which is ordered by statement then row (1557, 7624). The only true order key: the on-screen table sorts on Row ID as TEXT (1-1, 1-10, 1-2 ...) |
| 2 | `File Name` | the uploaded file's name without the forms prefix (up to the 2nd `-`) and without the extension (1810) |
| 3 | `Doc Reference Bank Statement` | typed by the person on the upload form (the forms table, 1802-1812); '' when empty (7505) |
| 4 | `Doc Reference Bank Voucher` | the same, a second reference (7506) |
| 5 | `Row ID` | `"<Statement ID>-<Statement Row ID>"` (7606). Statement ID: one per statement over the whole run (the file counter `i`, moved on by multi-account files); Row ID 1 is the opening row |
| 6 | `Bank` | PDF types: the bank (`ANZ`, `ASB`, `BNZ`, `Kiwibank`, `Westpac`: `vBank`, e.g. 2579). Excel types: the FORM's label (`BNZ - Excel`, `Kiwibank - Excel`, `Westpac - Excel`; 2143, 2316, 2465). Previous conversion: as in the file |
| 7 | `Account Name` | from the statement (per type, below) |
| 8 | `Account Number` | from the statement (per type, below) |
| 9 | `Other Party Account Name` | always `''` (7512, 2146, 2319, 2468); only a previous conversion can carry a value a person typed |
| 10 | `Other Party Account Number` | `''` (1142, 2147), except Kiwibank Excel `OtherPartyAcc` (2227) and Westpac Excel `Other-Party-Account-Number` (2442) |
| 11 | `Date` | a Qlik date, shown D/MM/YYYY (7612) |
| 12 | `Transaction Time` | `''` (7515, 2149, 2471), except Kiwibank Excel `ReceiptTime` (2223) |
| 13 | `Year` | `Year(Date)` |
| 14 | `Tax Year` | NZ tax year named by the year it starts: Jan-Mar -> Year-1, else Year (7517, 2151, 2324, 2473) |
| 15 | `Description as per bank statement` | `Details`, renamed at 1571; per type below |
| 16 | `Transaction Type` | `Deposit` / `Withdrawal` / `Unidentified`. PDFs: amount `<= 0` Withdrawal, `> 0` Deposit, none Unidentified (1166, 1318, 7519). BNZ Excel: Withdrawals cell filled (2089). Kiwibank / Westpac Excel: `Amount > 0` (2224, 2439). Opening row: always `Deposit`, even for a negative opening |
| 17 | `Transaction Code` | `100` opening; key-word code (`1xx` deposits, `3xx` withdrawals, from the external Transaction Codes workbook); `199` / `399` inter-account transfer; `200` / `400` uncategorised; `Unidentified` (7567, 7589; cards/SHARED.md) |
| 18 | `Code Description` | `Transaction Description` renamed (1574): key-word description, `Transfers in` / `Transfers out`, `Opening Balance`, `To be done`, `Unidentified` |
| 19 | `Transaction Category` | key-word category; BP rule (`Transfers to / from third parties`); `Inter-account transfers`; `Opening Balance`; `To be done`; `Unidentified` |
| 20 | `Amount` | **UNSIGNED**: `Fabs(Amount)` (1576), rounded to cents (7621), or `Unidentified`. The sign lives only in Transaction Type |
| 21 | `Balance` | running balance, cents, or `Unidentified` (a missing balance, or one holding a word with two or more lower-case vowels, 7523). PRINTED for ANZ, ANZ Loan, ASB Visa and all Excel types; COMPUTED BACKWARDS from the printed closing balance for ANZ Visa, BNZ Visa, Kiwibank Credit Card, Kiwibank PDF, Westpac Credit Card (1329-1385). Null on BNZ unstatemented rows |
| 22 | `Balance Check` | the running sum: opening row's Balance, then plus each signed amount (1476-1552) |
| 23 | `Balance Pass` | `Fail` when Balance and Balance Check differ by a cent or Balance is `Unidentified` / null, else `Pass` (1579) |

The "Statement Data" table shows them in this order: File Name, Row ID, Sort
Number, Bank, Account Name, Account Number, Other Party Account Name, Other Party
Account Number, Code Description, Transaction Category, Transaction Type, Date,
Transaction Time, Description as per bank statement, Transaction Code, Amount,
Balance, Doc Reference Bank Statement, Doc Reference Bank Voucher, Year, Tax Year,
Balance Check, Balance Pass (Amount and Balance shown as money, or the word
`Unidentified`).

Front-end measures that read these fields: KPI "Balance Check" (`Pass` only if every
row passes), "Missing Amounts", "Missing Types", "Max / Min Balance", "Max / Min
Withdrawal" (`Max({<[Transaction Type]={'Withdrawal'}>} Amount)`, which relies on
Amount being POSITIVE), "Max / Min Deposit" (excluding the Opening Balance row),
"# Transactions", "Date Range"; master measures `Balances Check` = `[Balance Check]`,
`Amounts`, `Balances`; treemaps on Transaction Category and Code Description.

### Where each field comes from, per statement type

| Field | BNZ Excel | Kiwibank Excel | Westpac Excel | PDF types | Previous conversion |
|---|---|---|---|---|---|
| Account Name | E2 (2048) | B&C of row 2 or row 7, or '' (2216-2276); one per file | B1 (2427) | the statement's header words (ANZ 2870, ANZ Loan 3777, ANZ Visa 4815, ASB Visa 5233, BNZ Visa 5744, Kiwibank CC 6243, Kiwibank PDF 6653, Westpac CC 7278) | file |
| Account Number | column B above the header (2056) | `KiwiAcc` on every row | B2 (2429) | header words (card types: the last 19 characters or the digits and `*`) | file |
| Other Party Account No. | '' | `OtherPartyAcc` | `Other-Party-Account-Number` | '' | file |
| Transaction Time | '' | `ReceiptTime` | '' | '' | file |
| Description | Payment Type + Particulars + C + D + E; pending rows E only (2088, 2100) | Narration1 + PayingBankDRN + PayeeDetails (+ ThisParty Particulars / Code / Reference) (2226, 2266) | Source-Type + This-Party-Reference + This-Party-Desc + This-Party-Code + Other-Party_Name (2438) | the row's description words | file |
| Type / sign | which money column | sign of Amount | sign of Amount | sign; CR / OD; or the unknown-sign solver (BNZ Visa, Kiwibank CC) | file's Transaction Type |
| Balance | printed; ` OD` format negates | printed | printed; `.` = 0 | printed or computed backwards | file, `Unidentified` recomputed backwards |
| Opening row | first Balance - first Amount | per account: oldest Balance - Amount | first Balance - first Amount | derived, or the printed opening (Kiwibank CC) | rebuilt from row 2 |
| Statements per file | 1 | 1 per account | 1 | 1 per file (Kiwibank PDF: per account); several statements in one PDF share one Statement ID | as many as the file's Row IDs |

## 2. The new tool's outputs

- **CSV and the workbook's `Transactions` sheet** (`display_transactions`,
  `R/outputs.R`): the 16 core columns minus the three `_raw` ones: `row_id, date,
  description, amount, direction, balance, particulars, code, reference,
  other_party, type, currency, flags`, with `debit` / `credit` spliced in after
  `amount` when the statement has separate money columns, then every extra column
  (`text1`.., `other1`.., `date2`, named extras on PDFs). Other sheets: `Summary`
  (the header: bank, statement_type, template_id/version, account_number,
  account_name, period_start, period_end, opening_balance, closing_balance,
  currency, source_file, source_sha256, page_count, row_count), `Checks`,
  `Provenance`, `Diagnostics`, `Metadata`, and `Other accounts` when present.
- **Qlik feed `feed/transactions/<hash>.csv`** (`R/feed.R` 55-64, fixed 30 columns):
  `run_id, converted_ts_utc, source_file, source_sha256, bank, statement_type,
  template_id, template_version, template_origin, trust_level, gate_result,
  period_start, period_end, account_number, statement_index, row_id, date,
  description, amount, debit, credit, direction, balance, particulars, code,
  reference, other_party, type, currency, flags`. Only accepted statements (proven,
  layout match, or confirmed by a person); withheld ones go to `feed/review/`.
- **`feed/extras/<hash>.csv`**: `run_id, row_id` + the statement's extra columns,
  under the names the reader gave them.
- **`feed/runs/<hash>.csv`** (manifest, one row per statement): `run_id,
  converted_ts_utc, source_file, source_sha256, bank, template_id, template_origin,
  status, trust_level, row_count, period_start, period_end, gate_result, feed_file,
  engine_version, layouts_state`.

## 3. Field by field

| QVF field | New CSV / workbook | New Qlik feed | Verdict |
|---|---|---|---|
| Sort Number | `row_id` (order within the file) | `row_id` + `statement_index` | **different**: no single run-wide order key; fine per file |
| File Name | Summary `source_file` (with extension) | `source_file` | same idea, different text (extension kept, no form prefix to strip) |
| Doc Reference Bank Statement | none | none | **missing**: the person's case / document reference typed on the QVF form has no field and no input in the new app |
| Doc Reference Bank Voucher | none | none | **missing** (as above) |
| Row ID | `row_id` | `row_id`, `statement_index` | **different shape**: no "s-r" string; no row 1 for the opening |
| Bank | Summary `bank` | `bank` | **different values**: identified bank name (`BNZ`), never the form label (`BNZ - Excel`); empty when identification is not sure. Measured on this set: empty on every Excel lookalike (their invented account numbers are not in the branch register, so only "low" evidence) |
| Account Name | Summary `account_name` | **not in the feed** | **missing in the feed**; and **empty for all three Excel types** although each file prints it (measured) |
| Account Number | Summary `account_number` | `account_number` (per statement) | **empty for all three Excel types** although each file prints it (measured on bnz_excel_1, kiwibank_excel_2, westpac_excel_1: header `account_number` and `account_name` null; `extract_metadata()` on the same preamble text returns the number only under `accounts`, never as `account_number`). Kiwibank's per-row `KiwiAcc` becomes extra `text1`, feed/extras only |
| Other Party Account Name | `other_party` (the other party's NAME where a column is headed payee / other party) | `other_party` | new tool has MORE; the QVF field is always blank |
| Other Party Account Number | no dedicated field: Westpac's `Other-Party-Account-Number` lands in `other_party` (because its heading says "other party"), Kiwibank's `OtherPartyAcc` in extra `text5` | Westpac: `other_party`; Kiwibank: feed/extras only | **different / missing**: account numbers and names mixed in one field across banks |
| Date | `date`, ISO `YYYY-MM-DD` text | `date` | same day, different type: Qlik must read it as a date (`Date#(date,'YYYY-MM-DD')` is the safe form) |
| Transaction Time | Kiwibank `ReceiptTime` -> extra `text2` | feed/extras only, as `text2` | **missing in the feed**; unnamed in the CSV |
| Year | none | none | **missing**, derivable in Qlik |
| Tax Year | none | none | **missing**, derivable: Jan-Mar -> year-1 |
| Description as per bank statement | `description` = ONE column (the longest text column, or the one headed Description); the rest in particulars / code / reference / other_party / type / extras | `description` + the separate columns; extras not in the feed | **different**: the QVF joins 3-6 columns. Measured empty descriptions where the QVF has text: BNZ 2 of 20 rows, Westpac 6 of 22 (fees, interest, deposits with no other party). Key-word coding on `description` alone would miss the words in the other columns |
| Transaction Type | `direction` = `debit` / `credit` / NA | `direction` | **different words**: map debit -> Withdrawal, credit -> Deposit, NA -> Unidentified. The QVF also calls a zero amount a Withdrawal |
| Transaction Code | none | none | **missing** (100 / 1xx / 3xx / 199 / 399 / 200 / 400) |
| Code Description | none | none | **missing** |
| Transaction Category | none | none | **missing** (incl. the BP rule and same-owner transfer matching) |
| Amount | `amount` SIGNED (money out negative); plus `debit` / `credit` only for statements with two money columns | `amount`, `debit`, `credit` | **different sign convention**: QVF Amount is unsigned. A dashboard built on the QVF's `Max({<Type={'Withdrawal'}>} Amount)` returns the SMALLEST withdrawal on the new feed, and Sum(Amount) is net instead of turnover. `debit` / `credit` are empty for signed-amount banks (Kiwibank, Westpac), so they cannot replace it either |
| Balance | `balance`, printed only (NA where the statement prints none) | `balance` | **different coverage**: the QVF computes a balance for every row of five PDF types; the new tool leaves NA (it never invents a figure) |
| Balance Check | none (the proof is statement-level: `Checks` sheet, outcome) | none | **missing** as a column |
| Balance Pass | none per row; statement-level `trust_level`, `gate_result`, and the outcome | `trust_level`, `gate_result` per row | **different granularity**. In the feed every accepted statement passed its own arithmetic (or a person vouched for it), so a per-row pass/fail adds nothing there; withheld rows carry `gate_result = withheld:...` |
| (Opening Balance row) | Summary `opening_balance`, `closing_balance` | **none**: neither the transactions feed nor the manifest carries opening or closing balances | **missing in the feed** |
| BNZ unstatemented (pending) rows | not transactions (product owner, auto-reading-spec section 2) | not fed | **deliberately different**: the QVF outputs them as transactions with a blank balance |

Fields the QVF never had: `run_id`, `converted_ts_utc`, `source_sha256`,
`statement_type`, `template_id` / `template_version` / `template_origin`,
`trust_level`, `gate_result`, `period_start` / `period_end`, `statement_index`,
`particulars`, `code`, `reference`, `type`, `currency`, `flags`, and the manifest.

## 4. Problems in the documented Qlik load (docs/operational/connecting-qlik.md section 4)

1. **Field names are case-sensitive in Qlik.** The documented script derives
   `Year(Date)` and `If(Amount <= 0, 'Withdrawal', 'Deposit')`, but the feed's headers
   are lower case (`date`, `amount`). As written the load refers to fields that do
   not exist (expect "Field 'Date' not found"); it needs `Year(date)` and `amount`.
2. Even corrected, that `If` turns an amount the tool left empty (a redacted row in a
   statement a person confirmed) into `Deposit`, where the QVF says `Unidentified`.
   Mapping `direction` is safer: `Pick(Match(direction,'debit','credit')+1,
   'Unidentified','Withdrawal','Deposit')`.
3. `Year` and `Transaction Type` are the only QVF fields the doc rebuilds. Tax Year,
   Transaction Code, Code Description, Transaction Category, Amount as an unsigned
   figure, Account Name and the opening balance are not mentioned, and the doc says
   "any richer categorisation belongs in Qlik", while the categorisation workbook it
   would need is the QVF's external Transaction Codes workbook, not in this repo.

## 5. What a Qlik port of the QVF's dashboards needs from the new feed

- `Amount` as the QVF had it: `Fabs(amount)`, plus `Transaction Type` from `direction`.
- `Year`, `Tax Year` from `date`.
- Account Name and the opening / closing balances: today only in the workbook's
  Summary sheet and the JSON, not the feed.
- A description equivalent to the QVF's Details: join `type`, `particulars`, `code`,
  `reference`, `description`, `other_party` (and the extras, which are only in
  feed/extras under generic names) before matching key words.
- The categorisation (codes, descriptions, categories, BP rule, same-owner transfer
  matching across the run's own account numbers) rebuilt in Qlik from the external
  Transaction Codes workbook. The transfer matching needs every account number the
  person converted, which the feed gives only per statement and only when the header
  was filled (never, today, for the Excel types).
