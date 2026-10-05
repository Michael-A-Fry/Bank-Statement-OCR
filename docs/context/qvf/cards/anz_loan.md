# Format card: ANZ Loan (home and term loans), type key `anz_loan`

QVF section: `script.qvs` lines 3376-4119 (tab "ANZ Loan"). It runs when the form
says `ANZ - Loan` and the file is a PDF (line 3377). Shared subroutines used:
`sAddToPairs`, `sCreateVariables`, `sLoopToDate` (with an ANZ-Loan branch at 267-283),
`sConcatenateStrings`, `sEndofData('balances')`, then the PDF final transform
(7479-7642) and `sBalanceCheck`.

Lookalikes: `gen/make_anz.py` -> `sets/anz/anz_loan_qvf_1..6.pdf` (+ `.truth.json`).

## 1. What the statement looks like, as the QVF expects it

The QVF reads words in connector order and never looks at positions.

**Start of each statement: "The following ..."**. Each `The following` (`following`
after `The`) inserts a "New Statement" marker (3416-3423). The lookalikes print
"The following is a summary of your loan for the period ...".

**Period: "... the period <date> to <date>"** (3427-3439). This is the word `period`
after `the`, not when the word three places back is `fees`, so "Summary of total
loan fees for the period ..." is skipped. The words up to the table heading
`Date Description` belong to the period. They are read by their ORIGINAL word
positions: +2 is the start month, +3 the 4-digit start year, +6 the end month, +7 the
end year (3763-3771). Long month names and ordinals are normalised first through the
`Date_Tidy_Up` map (`November` becomes `Nov`, `1st` becomes `1`) (1595-1640, applied
at 3407). So both "1 November 2015 to 30 November 2015" and "01 Jul 2018 to 30 Sep
2018" work. The year must have four digits; it is used as printed.

**Account number:** the word after `Home Loan Number` or `Term Loan Number`. Each
must come before the statement's `the period` (3441-3450). The lookalikes print
"Summary of Home Loan Number 0xxx-0xxxxxxxxx-100x" (a 4-10-4 loan identifier).

**Account name:** the words between `Account Name` and `Account Number` (3452-3459).
When there is no such block, the name is taken from the letterhead, between `Box` /
`AZNLS` and `Helpline`, and stops at the first word that contains a digit, the
address (3461-3524). **Only the first statement's name is used for every statement
in the file** (3773-3777). The New Statement branch updates the period and account
number but not the name.

**Table heading:** `Date | Description | Withdrawals | Deposits | Principal Balance`.
A block of transaction words starts two words after `Deposits`, which skips
`Principal Balance` (3526-3554).

**Dates:** `dd Mon yy`. The printed 2-digit year is THROWN AWAY: any 2-character
word after a month is excluded (3398-3404). The year is then re-inferred from the
period (4006-4022). If the period starts in Oct, Nov or Dec and the date is in Oct,
Nov or Dec, the start year is used; otherwise the end year.

**Balances:** `245,000.00 DR`. `DR` is deleted everywhere (3398-3404). A balance with
no `CR` becomes negative, as an amount owed; `CR` means positive (3968-3984).

**Money in or out comes from words, not columns** (3988-3994, 4030-4034). A row has
one amount, made negative only when the description contains `DRAWDOWN` or
`REVERSAL`, or is exactly `LOAN INTEREST`. Everything else is a deposit.

**Rows with no balance:** after the amount, if the next row's date follows at once,
the row has no balance of its own (3930-3939). Its balance becomes previous balance
plus amount (4047-4065). Interest and a payment on the same day print one balance.

**End of a block:** `Closing` (the `Closing Balance` line), two words that start
with `*`, or `Balance Carried` (3526-3554). `Balance Carried` does NOT end a block
when the word before is another `Balance`, that is, when the line comes straight
after the heading's `Principal Balance`. **A new block starts after `Carried
Forward`**, skipping the figure and its `DR`. So a page ends with `Balance Carried
Forward <bal> DR` and the next page repeats the heading and then the same line.

## 2. Special cases (each one is a real-world quirk)

| Lines | What it does | Why it exists |
|---|---|---|
| 3398-3404 | Removes the `- processed on: dd Mon yy` words (`-` unless after PAYMENT/ARREARS/REVERSAL, `processed`, `on`, the 2 words after `on`, and the 2-char year) | The ANZ secondary line under a transaction (the processing date). It must not become a row. |
| 3398-3404 | Keeps `-` after `PAYMENT`, `ARREARS`, `REVERSAL` | Descriptions like `LOAN PAYMENT - INTEREST`, `REVERSAL - LOAN PAYMENT` |
| 3405-3411 | Drops the merged token `DRRate` | A `DR` run into the next word `Rate` by the connector |
| 3557-3612 | Splits `DRLOAN` and words that mix digits with LOAN/INTEREST/DR/PAYMENT | The connector found no gap between a figure and the next word in some files |
| 3625-3667 (MoveUp) | Moves LOAN / PAYMENT / DRAWDOWN / REVERSAL / INTEREST words found after the figures back to the row's date | In some files the description comes after the figures in the word stream |
| 3646-3656 (FlagRemove) | Removes digit-only words longer than 3 characters, vowel-less fragments, and a second figure that is not next to the first | Strips reference numbers and split fragments out of loan descriptions |
| 3825-3832 | Skips rows whose 3rd word is `Opening`, `Rate`, `change` or `Balance` | `Opening Balance`, `Opening interest rate x% p.a.`, `Rate change x% p.a.`, `Balance Carried Forward`: events and balance lines, not transactions |
| 267-283 (sub) | On row 1, a figure after `p.a.` becomes the previous balance | The opening line can be `Opening interest rate 5.25% p.a. 245,000.00 DR`: the rate line carries the opening balance |
| 3731-3755 | If the 7th-last word is `Rate` or `change`, cuts the last row | A rate change printed as the table's last line |
| 3919 | Details end when the next word is a decimal or a number shorter than 4 characters | One-line description |
| 4078 | A row is output only if it has a balance (printed or derived) | |
| 1150-1190 | One synthetic `Opening Balance` row per FILE | Same as ANZ |
| (none) | Notices (fixed-rate expiry, rate change) are not recognised | They have no `Deposits` heading, so they are ignored. But a notice containing "The following" would start a phantom statement, and one printing `Home Loan Number` would shift the account numbers. |

**QVF weaknesses these lookalikes expose:**
- Loan **fees** (`LOAN SERVICE FEE`, `DISHONOUR FEE`) come out as deposits, because
  only DRAWDOWN, REVERSAL and LOAN INTEREST are made negative. The QVF's own Balance
  Pass then fails.
- The printed year is discarded, so a statement that starts before October and
  crosses a year end (for example 1 Jul 2018 to 30 Jun 2019) puts its Jul-Dec rows
  in the wrong year.
- In a bundle, the account name stays the first statement's.

## 3. Output columns the QVF fills for this type

These are the same fields as ANZ (`cards/anz.md` §3), filled the same way, except:
- **Description as per bank statement** is the description with the
  `- processed on:` line and the reference numbers removed. The processed date is
  lost.
- **Balance** is the principal balance, negative when owed.
- **Transaction Type** comes from the description words above, so fees are wrongly
  `Deposit`.
- **Account Name** is the `Account Name` block or the letterhead name, and the first
  statement's for the whole file.

## 4. Lookalike set (`sets/anz`, `anz_loan_qvf_*`)

| Case | Edge case |
|---|---|
| anz_loan_qvf_1 | Home loan, one month, Account Name block, fortnightly payments with `- processed on:` lines, month-end LOAN INTEREST, LOAN SERVICE FEE, fee summary |
| anz_loan_qvf_2 | Term loan, a whole year, 2 pages: first row `Opening interest rate 6.10% p.a.` carrying the opening balance; `Rate change` rows with no money; interest and payment on the same day (balance on the last row of the day only); `Balance Carried Forward` at foot and head |
| anz_loan_qvf_3 | 2 quarterly statements with a fixed-rate expiry NOTICE between them; no Account Name block |
| anz_loan_qvf_4 | New loan: `Opening Balance 0.00`, `LOAN DRAWDOWN` on day one, 1 Oct 2016 to 31 Mar 2017 (crosses a year end), 2 pages |
| anz_loan_qvf_5 | Dishonoured payment: `REVERSAL - LOAN PAYMENT`, `DISHONOUR FEE`, `LOAN PAYMENT - ARREARS`; `Rate change` as the last table row; interest-rate change NOTICE after the statement |
| anz_loan_qvf_6 | A NOTICE first, then 3 monthly statements in one file |

Every loan statement also prints the loan summary: Principal Paid, Interest Paid,
Interest Owing as at <date>, Next Loan Payment Amount, Next Loan Payment Date Due,
Fixed Rate Review Date and Maturity Date. It also prints "Summary of total loan
fees for the period ..." with Fee Description / Amount / Total Loan Fees.
