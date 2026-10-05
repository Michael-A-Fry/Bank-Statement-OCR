# Format card: BNZ Visa (type key `bnz_visa`)

QVF section: `script.qvs` lines 5472-5994 (form value `BNZ - Visa`, PDF only).
Shared code it calls: `sCreateTable`, `sAddToPairs`, `sBalances` (136-187), `sConcatenateStrings`
(335-395), `sCreateVariables` / `sLoopToDate` (217-307), `sFindDeposits` (436-1000),
`sEndofData` (1081-1470), the `Date_Tidy_Up` map (1592-1640), the PDF final transform
(7479-7642) and `sBalanceCheck` (1474-1590).

The script never looks at x/y positions. The connector hands it every word in reading
order, numbered, and the script walks that word stream with fixed landmark words. So
"what the statement looks like" below means "what order the words come in".

Lookalikes: `gen/make_bnz_visa.py` -> `sets/visa2/bnz_visa_1..9.pdf` (+ `.truth.json`).
Every lookalike is read back with a replay of these rules (`qvf_replay()` in the
generator); the replay's output per statement is in `visa2_debug/qvf_replay/`.

---

## 1. The statement, as the script expects it

### Word clean-up first (5492-5499)
* Every word goes through `Date_Tidy_Up`: long month names become short (`February` ->
  `Feb`) and ordinals become numbers (`13th` -> `13`). **Why:** BNZ prints the statement
  period with long month names (and possibly ordinals); the date rule below needs `Mon`.
* The words `Continued` and `over...` are thrown away. **Why:** every BNZ page foot says
  "Continued over..." and those words would otherwise land inside the last row of the page.

### One statement = one word "Statement" (5503-5512)
Every word `Statement` (exact case) starts a new statement; the number of statements in
the file is the number of times the word appears. So the word must appear exactly once
per statement, in its title. A second "Statement" on the page (for example a "Statement
date" label) would split one statement into two.

### The period, which gives the year (5514-5554, 5727-5735, 5866-5881)
`Statement for the period 13 February 2025 to 12 March 2025 for ...`
* the start is the word `period` straight after `the`, and the period counts only when
  the word `for` is exactly 8 words later (period, day, month, year, `to`, day, month,
  year, `for`);
* only the first such period after each "Statement" is used;
* start month/year = words +2/+3 after `period`, end month/year = +6/+7.

### Account name and card number (5631-5744)
* the terms URL `www.bnz.co.nz/cardterms`, then three words (a small heading, e.g.
  `Name Card number`), then `<cardholder name> <card number>`, then the card type: `Y` or
  `Lite` (exact case), or `Advantage` straight after `BNZ`, no more than 13 words after the
  URL;
* card number = the last 19 characters of that text (`4999 XXXX XXXX 1234` is 19),
  name = the rest.
* **Quirk:** when the end word is `Advantage`, the word `BNZ` before it is inside the text,
  so the 19 characters come out as `XXXX XXXX 1234 BNZ` and the name gains the first four
  digits. The replay reproduces this on the Advantage lookalikes (2, 5, 7). Either the real
  Advantage layout prints its card type somewhere else, or this is a latent bug.
* Read once per file (5737-5744): a bundle's later statements keep the first statement's
  name and number.

### Opening balance (5556-5584 + `sBalances`)
* a start 4 words after `Previous Balance` (not `the Previous Balance`), paired with the
  next `Credit Limit`;
* the opening figure is the word straight after that start, up to (not including) the
  next word containing `$`; `DR` is dropped.
* So the opening figure must be followed directly by another `$` figure (or `CR` and
  then a `$` figure). Anything else (a label) becomes part of the "figure". That fixes
  the layout as a **strip**: one line of labels, the figures on the line under it:
  `Previous Balance | Credits | Debits | Current Balance` over
  `$560.36 | $560.36 | $932.39 | $932.39`. The lookalikes print exactly that.

### Closing balance (5586-5629 + `sBalances`)
* `Current Balance` followed by the figure and then one of: `Current Minimum` (payment),
  `Please note` (after a `$` figure or a `CR`), or `Over Limit/Overdue`.
* So the closing comes from a second, vertical box (label and figure on one line):
  `Current Balance $932.39` / `Current Minimum Payment $27.00` ... `Credit Limit $3,000.00`.
  The strip's own `Current Balance` is followed by the next `Current Balance`, not by an
  end word, so the script ignores it.
* **Quirk (5596):** the brackets are missing in
  `(String = 'note' and Previous = 'Please' and PP has '$' or PP = 'CR')`, so ANY word two
  places after a `CR` becomes a marker. Harmless unless it falls between `Current Balance`
  and its end word, when the closing is lost.

### Sign of the balances (`sBalances` 157-176)
`$` removed; a `CR` means the card is in credit and the figure stays positive; any other
figure is owed and becomes negative; `0.00` stays `0.00`. Balances are therefore held from
the cardholder's side (owed = negative), the same convention as the new tool's key.

### The transaction table (5683-5725)
* starts after each column heading that ends `Credit Amount $` (the `$` word, after
  `Amount`, after `Credit`): the lookalikes print `Date | Transaction details |
  Debit Amount $ | Credit Amount $`;
* ends at the first `Page`, `Total` or `Our`, or a `BNZ` that is not straight after a
  month word, or the `Yo` artefact (a split "Your" a few words after a figure);
* so: **one heading per cardholder section and per page**, a section ends at its
  `Total for card ending ...` line, a page ends at `Page n of N`. Text between a `Total`
  and the next heading (the next cardholder's name line) is never read.
* **Quirk:** a description containing `BNZ` anywhere but first (e.g. `CASH ADVANCE BNZ ATM`)
  ends the table there; `BNZ ATM ...` straight after the date is fine. Same for a
  description containing the words `Page`, `Total` or `Our` (exact case).

### A row (5786-5900)
* column 1, the date: words up to the first month word: `12 Mar` (exact case `Mar`);
* column 2, the details: words up to the word before one that starts with `$`;
* column 3, the amount: the `$` word and every word after it until the word two ahead is
  a month (the next row's `dd Mon`), joined with commas. So **nothing may follow the
  figure before the next row's date**: a wrapped description (e.g. a foreign-currency line
  `USD 30.00 @ 0.6123`) must be printed ABOVE the figure, which sits on the row's last
  line. A `CR` or a second figure after the amount makes the amount unreadable.
* **There is no money-in / money-out column for the script.** Debit Amount and Credit
  Amount figures look the same in the word stream; the only signs are the words (below)
  and the arithmetic.
* **Missing amount (5819-5829):** a dated line with no `$` figure before the next date is
  dropped silently. **Why:** BNZ "sometimes has a missing transaction amount" (script
  comment); lookalike 9 prints such a line (`CARD REISSUED - REPLACEMENT CARD SENT`).

### Year (5866-5881)
Rows in Oct, Nov or Dec take the period's START year when the period starts in Oct, Nov or
Dec; every other row takes the END year. **Why:** a period like 13 Dec 2024 - 12 Jan 2025.
Weak spot: a row dated before a January period start (a late-posted December purchase)
would get the wrong year; BNZ prints one date per row, inside the period, so it does not
arise on the lookalikes.

### Money in or out: keywords, then a subset-sum (5857-5864, `sFindDeposits` 436-1000)
1. A row is a deposit when its details contain `BNZ Cash Reward`, `PAYMENT THANK YOU`,
   `Purchase Return/Refund`, `ACCOUNT FEE CREDIT`, `PAYMENT CHQ THANK YOU`, `Credit` or
   `CREDIT` (case-sensitive substrings). Every other row is "unknown".
2. Per statement: difference = (opening + deposits - unknowns - closing) / -2. If 0, all
   unknowns are purchases.
3. Otherwise it searches the unknowns no bigger than the difference for sets (up to 6
   rows) that add up to exactly the difference. **One** set: those rows are deposits.
   **More than one** set: those rows are written as `Unidentified` (a person decides).
   No set: the rows stay as the keywords said and the file's Balance Check fails.
4. Time limit 420 s, cut to 30 s when any description contains ` SD` (BNZ ATM cash
   advances `... SDM ...`), because round ATM amounts give many equal sums (452-472).
   On time-out every candidate row is `Unidentified`.
* **Weak spots**, both in lookalike 4: the word `CREDIT` in a debit
  (`CREDIT CARD REPAYMENT INSURANCE`) makes it a deposit, and a merchant refund printed
  with the merchant's name only is a purchase unless the subset-sum finds it. Here the
  mis-flagged debit makes the difference odd, the search finds nothing, and the replay
  reads 2 of 12 rows with the wrong sign.

### Several statements in one file
Every `Statement` word is a statement boundary (a `New Statement` marker in the word
stream). At each boundary the previous statement's deposits are settled with its own
opening and closing, and the period (so the year) is re-read. Balances are worked
backwards from each statement's own closing. If the last row is dated before the first,
statements are reversed (`Statement Number desc`, 1200-1222): a bundle printed newest
statement first.

### What is dropped
Everything outside the heading-to-Total/Page blocks: the summary, the cardholder name
lines, the `Total for card` lines, the page furniture. Dated lines with no figure (above).
A statement with no rows is not handled: with no month word in the word stream the row
loop's `sLoopToDate` never finds a date (it would run off the end of the data).

---

## 2. What the QVF writes for this type

From `sEndofData` (no running balance path) and the final transform:

| QVF column | How it is filled for BNZ Visa |
|---|---|
| File Name, Doc Reference Bank Statement, Doc Reference Bank Voucher | from the upload form |
| Row ID / Statement ID Final, Statement Row ID Final | file number - row number; row 1 is a made-up **Opening Balance** row |
| Bank | `BNZ` |
| Account Name, Account Number | the cardholder name and the 19-character card number from the details block (see the Advantage quirk) |
| Other Party Account Name, Other Party Account Number, Transaction Time | always empty |
| Date | the row date with the year rule, DD/MM/YYYY |
| Year, Tax Year | `Year(Date)`; Tax Year = year - 1 for Jan-Mar (7517) |
| Description as per bank statement | the details words, joined by single spaces (the FX line included) |
| Transaction Type | Deposit / Withdrawal / Unidentified (keywords + subset-sum) |
| Transaction Code, Code Description, Transaction Category | keyword lookup in the team's "Transaction Codes" workbook; defaults 200/400 and "To be done"; 199/399 "Transfers in/out", "Inter-account transfers" when the details contain another loaded statement's account number (7589-7596) |
| Amount | absolute value (`Fabs`), the sign is in Transaction Type |
| Balance | worked BACKWARDS from the statement's closing balance, in date order (rows are re-sorted by date within each statement, so cardholder sections are interleaved), owed = negative; `Unidentified` when any row of the file is Unidentified |
| Balance Check, Balance Pass | forward running sum from the opening row; Pass when it meets Balance |
| Sort Number | row number in the final table |

---

## 3. The lookalikes (`sets/visa2/bnz_visa_*.pdf`)

All: A4, Helvetica 8, page 1 = masthead, address, the period sentence, the terms URL and
details block, the summary strip and the payment box, then cardholder sections (name line,
heading `Date | Transaction details | Debit Amount $ | Credit Amount $`, rows, `Total for
card ending nnnn`), foot `Page n of N` / `Continued over...`, synthetic banner. Every figure
is printed `$1,234.56`. No running balance.

| # | What it tests | QVF replay |
|---|---|---|
| 1 | one card, one page, full payment, two interest lines | 14/14 right |
| 2 | two cardholders (two sections, two `Total for card` subtotals), section 2 on page 2, USD/AUD purchases with the foreign line above the NZ$ figure + an overseas fee row each, BNZ Cash Reward, Purchase Return/Refund, part payment, Advantage card | 34/34 right (account number shows the Advantage quirk) |
| 3 | period 13 Dec 2024 - 12 Jan 2025 (year rule), wrapped rows, BNZ ATM cash advances in round amounts + fee rows, interest on purchases and on cash advances | 20/20 right |
| 4 | overpaid: closes IN CREDIT (`$66.59 CR`, then `Please note`), merchant refund with no deposit word, a debit containing `CREDIT` | 10/12, 2 wrong sign |
| 5 | three consecutive statements in one PDF, balances chain | 31/31 right |
| 6 | no transactions, $0.00 to $0.00 | (the real script would not finish; replay reads 0 rows) |
| 7 | over the credit limit (`Over Limit/Overdue`), two cardholders, three pages, heading repeated per page and per section, FX on both cards, part payment | 72/72 right |
| 8 | opens IN CREDIT (`$35.xx CR`), no payment, BNZ Cash Reward | 11/11 right |
| 9 | a dated line with no figure inside the table | 13/13 right, the line dropped as "missing amount" |

Assumption flagged: Qlik's `=` string test is taken as case-sensitive (the script uses
`MixMatch` where it wants case-insensitive), so the upper-case banner
`... NOT A REAL STATEMENT` is not a "Statement" word.
