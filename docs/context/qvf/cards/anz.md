# Format card: ANZ transaction account ("ANZ - 9 Columns"), type key `anz`

QVF section: `script.qvs` lines 2576-3375 (tab "ANZ"). It runs when the form says
`ANZ` and the file is a PDF (line 2577). Shared subroutines used: `sCreateTable`,
`sAddToPairs` (194-211), `sCreateVariables`, `sLoopToDate` (233-307),
`sConcatenateStrings` (335-395), `sRowSetup`, `sColumnSetup`, `sEndofData('balances')`
(1081-1190), then the PDF final transform (7479-7642) and `sBalanceCheck` (1474-1590).

Lookalikes: `gen/make_anz.py` -> `sets/anz/anz_qvf_1..9.pdf` (+ `.truth.json`).

## 1. What the statement looks like, as the QVF expects it

The QVF never looks at x/y positions. It reads the words in the order the PDF
connector hands them over, and walks them with landmark words.

**Start of each statement: "Account at a glance".** Every time the word `glance`
follows `a` (2642-2648) the script inserts a "New Statement" marker into the word
stream (2804-2810). Reaching one in the main loop moves to the next statement's
period, account name and account number (2884-2930). An empty statement still
counts, because `sLoopToDate` adds one to the count when it walks past a marker
(289-294).

**The glance box, in this order:**
`Account name <name>`, then `Statement number <n>`, then `Account number <number>`,
then `Statement period <period>`, and then the table heading `Date Transaction type
and details ...`.
- Account name: the words between `Account name` and `Statement number`. The name
  stops at a word `Statement` or `Account` (2695-2718).
- Account number: the one word after `Account number`. In marker order it must come
  straight after the glance heading and straight before `Statement period`
  (2720-2756).
- Statement period: the words between `Statement period` and `Date Transaction`
  (2653-2693). The period is the last thing in the box before the table heading.
- **The first statement's year** is the word five places after `glance` (2640). So
  the box's first line carries a date, for example "Account at a glance ...
  Statement date 30 Sep 2019". The lookalikes print exactly that line.

**The period is read by position** (2832-2862):
- `dd Mon yyyy to dd Mon yyyy`, with `to` removed: word +2 is the start month, +3 the
  start year, +5 the end month, +6 the end year. A year is rebuilt as `20` plus its
  last two digits, so `19` and `2019` both work.
- **`START - dd Mon yyyy`** is a new account's first statement. The hyphen is
  presumably dropped by the connector as a delimiter. The start month and year become
  `Unknown`; +3 is the end month; +4 is the end year. If +4 is the heading word
  `Date`, meaning the year is not printed, the first statement's year is used
  (2834-2850). A commented-out `vPreviousBalance = '0.00'` (2836) shows such a
  statement has no opening-balance row, so its first rows start with an unknown
  previous balance.

**The table.** It has 9 physical columns, read as 4 logical ones: Date, Details,
Amount, Balance (2587-2593).
- `Date`: `dd Mon`, with no year.
- `Transaction type and details`: one heading over five physical columns. These are
  the type code (`AP`, `BP`, `DC`, `DD`, `EP`, `AT`, `CQ`, `VT`, `IP`, `IF`, `FX`,
  `IA`, `ED`), the other party, particulars, code and reference.
- `Withdrawals`, `Deposits`, `Balance`.

A block of transaction words **starts at the word `Withdrawals`** (2758-2770). The
heading words `Deposits` and `Balance` straight after it are dropped (2774-2779).
**A block ends** at the first of these (2765-2767):
- `Totals` ("Totals at end of page", "Totals at end of period");
- a lone `*` after a month, a `CR` or a figure (a footnote);
- `Available Credit`;
- `PTO AP`;
- `Your available ...` ("Your available credit is $X as at the closing date of this
  statement");
- `<figure> Balance Carried ...` (a page-end carried-forward line).

So every page repeats the heading, and a page ends with page totals or a
carried-forward line.

**Money in or out is NOT taken from the column.** A row yields one amount and one
balance. The sign is balance minus previous balance: zero or below means a
withdrawal (3294-3301). The previous balance comes from the dated `Opening balance`
row (3076-3135), from `sLoopToDate` when a figure follows the word `balance`
(267-283), or from the previous row. When the previous balance is unknown (a
redaction, or a START statement), the direction comes from the code after the date:
`DD` means a withdrawal and `DC` a deposit (3064-3070). Details that are exactly
`DEPOSIT` also mean a deposit (3193-3198). Otherwise the amount is left empty and
becomes `Unidentified`.

**OD.** A balance printed `1,234.56 OD` becomes negative. This applies to the row
balance (3183-3187), the opening balance (3124-3127) and `sLoopToDate` (273-281).
The 2024 revision note ("adding `or String = 'OD'`") is line 2789. The script cuts
off every word after the last figure of the data (2785-2800). Before the fix, the
`OD` after the final balance was cut off, so the last row's balance and sign were
read as positive.

**Commas.** The hex-to-ASCII map row `2C,,` (1702) maps a comma to nothing, so
`1,234.56` arrives as `1234.56`. Every "pure decimal" test in this section relies
on that.

**How the year is found** (3240-3270). The date is the last 6 characters, `dd Mon`.
- If the period starts in Oct, Nov or Dec and the date's month is Oct, Nov or Dec,
  the start year is used.
- If the date is in December, neither end of the period is December, and the start
  and end years are equal, the start year minus 1 is used. This covers a December
  transaction shown on a January statement.
- If the period is `START` and the date is in December with an end month of
  January, the end year minus 1 is used.
- Otherwise the end year is used.

## 2. Special cases (each one is a real-world quirk)

| Lines | What it does | Why it exists |
|---|---|---|
| 2596-2608 | Drops the `Withdrawals` heading word when the next words are `No transactions for this period` | An empty statement must not open a transaction block. It would swallow the next statement's words. |
| 289-294 (sub) | Counts a New Statement marker passed while looking for a date | Keeps account and period in step after an empty statement |
| 2610-2617 | Of two consecutive `dd Mon` dates, removes the first | A row redacted except for its date: the date stays, the rest is blacked out |
| 2950-3016 | "Loop past redacted transactions (when the dates are not redacted)" at the start of the first statements | The same quirk on the first rows, before any row is output. Lines 2968-2970 read the period at +6/+7, not +5/+6: an inconsistency (bug). |
| 2618-2638 | Deletes `Premium interest $x` and `Standard interest $x` | The interest breakdown printed with an interest row. Its `$` figures would otherwise be taken as money. |
| 2817-2819 | Drops `D` after `D` or after a month | Probably letters a redaction tool leaves behind (uncertain) |
| 2942-2946 | A row whose 1st or 3rd word is `Balance` is skipped | "Balance brought forward from previous page", dated or not |
| 2978-2985 | Skips `Balance` / `brought` / `Carried` lines in the redacted-row loop | Page-break lines |
| 3076-3135 | A dated `Opening balance` row sets the previous balance and is not output. If its balance is redacted, the previous balance is Unknown. | Opening row in the table |
| 3141 | Details end when the next two words are both plain 2-decimal figures followed by a month, `*`, New Statement, `OD`, `Balance` or `incl`, or at the end of the data | The only way to find the amount. The script's own comment says a number in the details can break it (for example a reference `1043.20`). |
| 3149 | The amount ends at the first decimal | One amount per row, whichever column it is printed in |
| 3203-3214 | Text found after the balance is moved back into Details | A wrapped description: its second line comes after the first line's figures in reading order |
| 3195-3198 | Details exactly `DEPOSIT` means a deposit | A branch cash deposit with an unknown previous balance |
| 2898-2910 | START handling for a later statement | Bug: with no `Else`, a printed year is not read for a 2nd or later START statement |
| 3334 | A row is output only if it has a balance | Skips rows whose date shows but whose rest is redacted. **Every ANZ row must print a balance**; a row without one is lost. |
| 1150-1190 | Adds one synthetic `Opening Balance` row per FILE: amount and balance = first balance minus first amount, type `Deposit` | Gives the Qlik balance check a starting point. There is only one per file, even with several statements. |

## 3. Output columns the QVF fills for this type

Built in 3338-3360, 1150-1190 and 7487-7642, and checked in 1474-1590.

| QVF field | How |
|---|---|
| File Name, Doc Reference Bank Statement, Doc Reference Bank Voucher | From the upload form |
| Row ID (`file-row`), Sort Number | Running numbers |
| Bank | `ANZ` |
| Account Name, Account Number | From each statement's glance box |
| Other Party Account Name | Always empty (7512) |
| Other Party Account Number | Always empty (1142) |
| Date | `DD/MM/YYYY`, with the year inferred as above |
| Transaction Time | Always empty (7515) |
| Year | Year of the date |
| Tax Year | Jan-Mar gives year - 1 (the NZ tax year, labelled by its start year) (7517) |
| Description as per bank statement | The whole details string, including the type code: `DD HARBOUR TELECOM POLICY 84757` |
| Transaction Type | `Withdrawal` (amount ≤ 0), `Deposit`, `Unidentified` |
| Transaction Code | From a keyword list (Excel) via the code description. Default `200` (deposit) / `400` (withdrawal); `199`/`399` when the details contain one of the file's own account numbers (7567-7596) |
| Code Description | Keyword match on the details, with separate lists for deposits and withdrawals. `To be done` if none; `Transfers in/out` for own accounts. |
| Transaction Category | Keyword match. `BP` at the start of the details gives `Transfers to/from third parties` (7498-7500). Own-account matches give `Inter-account transfers`. Otherwise `To be done`. |
| Amount | Absolute value (1576); the sign is in Transaction Type |
| Balance | As printed; negative when OD |
| Balance Check, Balance Pass | Running sum from the opening row compared with the printed balance (1557-1579) |

## 4. Lookalike set (`sets/anz`, `anz_qvf_*`)

| Case | Edge case |
|---|---|
| anz_qvf_1 | Business current account, 2 pages: Totals at end of page, Balance brought forward from previous page, Totals at end of period, available-credit line, code legend, one wrapped party |
| anz_qvf_2 | OD: opening and closing `OD`, balance crosses zero both ways, debit interest |
| anz_qvf_3 | Period 15 Dec 2021 to 14 Jan 2022 (crosses a new year), year-less dates |
| anz_qvf_4 | 3 statements in one file: `START - 30 Sep 2019` (no opening row), then `No transactions for this period`, then a normal month |
| anz_qvf_5 | Wrapped party names; `CREDIT INTEREST PAID` with a `Premium interest $x Standard interest $y` second line; reference `1043.20` (money-like); cash deposit with a `CASH HANDLING FEE`; `Balance Carried Forward` at page foot |
| anz_qvf_6 | 2 consecutive statements in one file |
| anz_qvf_7 | 2 rows redacted except their dates |
| anz_qvf_8 | A single statement with `No transactions for this period` |
| anz_qvf_9 | A standalone `START - 31 Mar 2022` statement with no statement date. The year can only come from the START period. |
