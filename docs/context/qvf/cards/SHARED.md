# Shared subroutines and maps (script.qvs lines 107-1786)

Area: excel. What each shared piece does, in plain words, with the real-world
reason where the script implies one. Line numbers are script.qvs lines.

## Global settings that shape the output (lines 53-73)

- `ThousandSep ','`, `DecimalSep '.'`, `MoneyFormat '$#,##0.00;-$#,##0.00'`,
  `DateFormat 'D/MM/YYYY'`. Every date the QVF writes is a Qlik date shown D/MM/YYYY,
  and every money field is shown with a `$`.

## Subroutines (tab "Subroutines", lines 107-1590)

| Sub | Lines | What it does | Why it exists |
|---|---|---|---|
| `sCreateTable(table)` | 110-130 | Makes an empty table with one placeholder field (`Statement ID` for `Temp_Result` / `Statement_Rows`, `ID` otherwise). | Qlik cannot `Concatenate` into a table that does not exist yet. |
| `sBalances(table, OpeningOrClosing, type)` | 136-189 | For each statement in the file (`vNumberOfStatements`), joins the words after a landmark up to the word `<Opening/Closing>BalanceEnd` into one figure and stores it in `Opening_Balances` / `Closing_Balances` by statement number. **Sign rule:** a figure ending in `CR` (` CR`, `,CR` or glued `CR`) has the marker stripped and stays positive; any other figure is made NEGATIVE, except `Unknown` and `0.00`. | Card statements print the amount owed plain and a credit balance with `CR`; the QVF stores balances from the holder's side (owed = negative). |
| `sAddToPairs(table)` | 194-212 | For each (start, end) landmark pair found, copies every word strictly between the two positions into the table. | Cuts the transaction region out of the page furniture. |
| `sCreateVariables` | 217-227 | Resets the statement and row counters and walks to the first date. | |
| `sLoopToDate(position, table, DateType)` | 233-307 | Walks forward to the next date. `DateType = 'full'`: the next word containing `/` (DD/MM/YYYY). Otherwise: the next three-letter month word (`Jan`..`Dec`), then steps back one word (the day number sits before the month). On the way it counts `New Statement` markers (a statement with no rows in a bundle still moves the statement count on), and for ANZ / ANZ Loan it picks up the previous balance printed after the word `balance` (or after `p.a.` on an ANZ Loan's first row), negated when the next word is `OD`. | Dates are the row landmark on every PDF type. |
| `sGetStrings(position, table)` | 313-329 | Looks up the next three words. | Lookahead for multi-word landmarks. |
| `sConcatenateStrings(start, end, table, type)` | 335-397 | Joins words from `start` until the word equal to `end`. Strips a leading `$` from every word. For `type = 'number'`: a string containing `OD` becomes negative (last three characters ` OD` dropped), and a space inside a figure is turned into a comma. | The PDF connector can split `1 234.56` into two words; `OD` marks an overdrawn balance. |
| `sRowSetup`, `sColumnSetup` | 401-430 | Per-row and per-cell state (`CurrentRow` table, deposit / withdrawal / missing-amount flags). | |
| `sFindDeposits` | 436-1002 | **The unknown-sign solver** (see below). | Statements that print amounts with no sign and no running balance (BNZ Visa, Kiwibank Credit Card). |
| `sEndCurrentString` | 1006-1028 | Appends the current word to the current cell and moves on; flags the end of data. | |
| `sEndofRow` | 1034-1077 | Writes the finished row to `Temp_Result` with `Statement ID` (= file counter `i`), a row number, `Statement Number`, `Bank`, account name / number, `Date`, `Details`, and `Amount` when one was read (a row without one keeps a null amount). | |
| `sEndofData(type)` | 1081-1470 | Finishes one PDF: drops work tables; adds `Other Party Account Number = ''` to every row (1139-1144). **`type = 'balances'`** (the statement prints a running balance: ANZ, ANZ Loan, ASB Visa): Transaction Type from the amount's sign (`<= 0` Withdrawal, `> 0` Deposit, else Unidentified) and an `Opening Balance` row = first row's balance minus its amount (1150-1191). **Otherwise:** detects a newest-first file (last date earlier than the first, 1200) and re-orders oldest first (by statement number descending, then date; Kiwibank Credit Card by date only, Kiwibank PDF numbers statements per account, 1200-1262); joins each statement's printed closing balance onto its last row (1267-1298) and computes every balance BACKWARDS, `Balance(k-1) = Balance(k) - Amount(k)` (1329-1385); when unknown-sign rows remain it does not compute balances at all (1302-1325); adds the `Opening Balance` row (Kiwibank Credit Card: the PRINTED opening balance, so the later check really tests opening + rows = closing, 1387-1460). | One shape of output whatever the bank. |
| `sBalanceCheck` | 1474-1590 | Builds the final `Transactions` table and the per-row check (see "The check" below). | The QVF's only arithmetic control. |

### `sFindDeposits` in detail (lines 436-1002)

For a statement whose amounts carry no sign (`Unknown_type`) and that prints only
an opening and a closing balance:

1. Assume every unknown is money out. Then `difference = (opening + known deposits
   - unknowns - closing) / -2` in cents (511). If the difference is 0, every unknown
   is a withdrawal (515-570).
2. Otherwise some unknowns are deposits, and those deposits must sum to exactly the
   difference. Candidates are the unknowns no larger than the difference, excluding
   `Currency Conv Assessment` and `Foreign Currency Txn Fee` (576-588; 586: Kiwibank fees
   that are never deposits), largest first.
3. A recursive search (`sRecurse`, 642-842) finds every combination of at most six
   candidates (688) that sums to the difference.
4. **It accepts the answer only when it is unique** (922): every combination found
   is appended to `Solutions`, so two combinations make the solutions' sum twice the
   difference and nothing is accepted. Then the candidates stay `Unidentified` for a
   person.
5. A time limit stops the search (455: 420 s; 467: 30 s for a BNZ Visa statement
   with ATM rows, ` SD*`), and on a time-out every candidate stays `Unidentified`
   (882-918).

Quirk: the candidate filter at 586 compares dollars (`Unknown_type`) with the
difference in cents, so it prunes almost nothing; harmless, only slower.

## Maps (tab "Mapping", lines 1592-1786)

| Map | Lines | What it does |
|---|---|---|
| `Date_Tidy_Up` | 1595-1642 | Word by word: `January`..`December` to `Jan`..`Dec`, and ordinal days `1st`..`31st` to `1`..`31`. Applied with `ApplyMap` to every word in ANZ Loan (3407) and BNZ Visa (5495), whose statements print "1st January" style dates. |
| `Transaction_Description_Deposits` / `_Withdrawals` | 1646-1668 | Key word to `<Description>`, loaded from an external workbook `Transaction Codes.xlsx`, sheet `Key Words` (columns `Key Words`, `Description`, `Category`, `Code`). Deposits are the key words whose `Code` starts with `1`, withdrawals those starting with `3`. Keys are `' ' & lower(key word)`. |
| `Transaction_Category_Deposits` / `_Withdrawals` | 1654-1676 | Key word to `<Category>`, same workbook and split. Keys have NO leading space. |
| `Transaction_Code` | 1678-1683 | `Description` to `Code`, from the same workbook's `Codes` sheet. |
| `HexToASCII` | 1687-1785 | Hex byte pairs `21`..`7E` to their ASCII character, plus `E28099` (UTF-8 right single quote) to `'`. |

### The code legend

**The legend itself is not in the script.** Descriptions, categories and codes live
in the external `Transaction Codes.xlsx`, maintained by users. What the script fixes:

| Code | Code Description | Category | Where |
|---|---|---|---|
| `100` | Opening Balance | Opening Balance | the added opening row of every statement (BNZ 2182, Kiwibank 2370, Westpac 2504, previous conversion 2016) |
| `1xx` | from the key-word workbook | from the key-word workbook | deposit key words (first digit 1) |
| `3xx` | from the key-word workbook | from the key-word workbook | withdrawal key words (first digit 3) |
| `199` | Transfers in | Inter-account transfers | a deposit whose description contains one of the account numbers converted in the same run (7589-7591) |
| `399` | Transfers out | Inter-account transfers | the same for a withdrawal |
| `200` | To be done | To be done | a deposit no key word matched (7567-7569) |
| `400` | To be done | To be done | a withdrawal no key word matched |
| `Unidentified` | Unidentified | Unidentified | a row whose type could not be decided |

How a description is matched: `MapSubString` on `' ' & lower(Details)`, then the text
between the first `<` and `>` (`TextBetween`). So the key word must start a word
(the leading space), matching ignores case, and the left-most key word in the
description wins. The category map has no leading space, so a category key word can
match inside a word. The code is then looked up from the matched DESCRIPTION, not
from the key word.

**The `BP` rule** (BNZ 2128-2132, Kiwibank 2301-2305, Westpac 2450-2454, previous conversion 1890-1894, PDFs 7498-7502): a
description starting with `BP` (bill payment) that no category key word matched gets
category `Transfers to third parties` (withdrawal) or `Transfers from third parties`
(deposit).

**Same-owner transfer matching** (7541-7545, 7589-7591): every distinct `Account
Number` converted in the run (all files together) becomes a key; a row whose
description CONTAINS one of them (case-sensitive substring, exact print) is an
inter-account transfer, codes 199 / 399. It looks in the description, not in the
Other Party Account Number field.

### Hex and text clean-up

- The PDF connector hands every word as hex (`SQL SET enableWordHex = 1`, 2542) and
  a custom country pack keeps an amount with commas as one word (2536-2538).
- `Initial Transform` (2553-2575): each word goes through `HexToASCII`, empty words
  are dropped, and a word listed at several positions (`WordPosition` like `12;57`)
  becomes one row per position (`SubField(...,';')`).
- Quirks: only printable ASCII and the curly apostrophe are mapped. Any other
  non-ASCII character (an accented letter, an en dash, a non-breaking space) is left
  part-decoded: `MapSubString` works left to right, so `C3A9` (`é`) comes out as
  `C:9`. Descriptions with such characters are silently altered.
- Concatenated descriptions keep double spaces where a part is blank, because Qlik's
  `&` treats a null as an empty string (e.g. BNZ `"Payment Type" & ' ' & Particulars &
  ' ' & F3 ...`, 2088).

### Date and amount rules (shared)

- Dates: PDF types use `DD MMM` (year taken elsewhere) or `DD/MM/YYYY` (`'full'`);
  Excel types read real date cells, `Date#(ProcessDate,'YYYY-MM-DD')` (Kiwibank) or
  `Date#(Date,'DD MMM YY')` (BNZ unstatemented rows). Output `Date(Date)`.
- Amounts: `$` stripped; `OD` makes a figure negative; `CR` on a balance keeps it
  positive and a plain balance is negated (cards); a space inside a figure becomes a
  comma; rounded to cents at 7621-7622; written UNSIGNED at the end (`Fabs`, 1576).
- Year = `Year(Date)`. **Tax Year = the NZ tax year labelled by the calendar year it
  STARTS in**: January-March gives `Year - 1`, April-December gives `Year`
  (e.g. 15 Feb 2025 is tax year 2024).

## The check (`sBalanceCheck`, 1474-1590) and the sheet KPIs

- `Balance Check` (per row): row 1 of a statement (the Opening Balance row) takes its
  own `Balance`; every later row is the previous row's `Balance Check` plus this
  row's SIGNED amount, rounded to cents (1476-1552). Kiwibank PDF starts from the
  printed opening balance instead (1511-1520).
- `Balance Pass`: `Fail` when `Round(Balance*100) <> Round("Balance Check"*100)`, or
  Balance is `Unidentified` or null; else `Pass` (1579).
- What it can catch: on a statement whose running balance is PRINTED (ANZ, ANZ Loan,
  ASB Visa, all three Excel types) any row where the printed balance and the running
  sum disagree. Where the QVF COMPUTED the balances backwards from the closing
  balance, the check passes by construction, except where the opening is the printed
  one (Kiwibank Credit Card), which makes it a real opening + rows = closing test.
  Because the opening row is derived from the first row (balance minus amount), the
  first row always passes.
- Front end (the "Statement Viewer" sheet): a KPI titled **Balance Check** shows
  `Pass` only when every row's `Balance Pass` is Pass; the master measure **Balances
  Check** is simply `[Balance Check]` in money format. Other KPIs: Missing Amounts
  (blank or `Unidentified` amount, not counting Opening Balance rows), Missing Types
  (`Unidentified`), Max / Min Balance, Max / Min Withdrawal and Deposit (deposits
  exclude the Opening Balance row), # Transactions, Date Range.
