# Format card: Westpac - Credit Card (PDF)

Type key `westpac_cc`. QVF section `///$tab Westpac Credit Card`, script.qvs lines 6966-7478.
The form value is `vStatementType = 'Westpac - Credit Card'` with a `.pdf` file.

## What the statement is

A monthly credit card statement. Page 1 has a title ending in `STATEMENT`, the
cardholder's name and postal address, a details box (closing balance, account
number, statement period, credit limit, due date, minimum payment) and a summary
(opening balance, payments and credits, purchases and debits). Then the transaction
list, grouped: a `General Payments & Charges` section first (payments, interest,
fees), then one section per card, each headed by the cardholder's name and the masked
card number `**** **** **** 1234`. There is **no running balance**. Each row prints
**no year**: the year comes from the statement period.

## How the QVF reads it

One word per row, in reading order. No x positions. Everything is found from
neighbouring words.

### Landmarks it searches for (exact words, case matters)

| Landmark | Lines | Used for |
|---|---|---|
| `Statement` `Period` | 6996-7001 | One per statement, so it becomes a `New Statement` marker. |
| `Statement Period` ... `Credit` or `Purchases` | 7003-7027, 7258-7266 | The period: word +1 is the start, word +3 the end, both `dd/mm/yyyy` (word +2 is the separator, e.g. `-`). The window closes at the next `Credit` (e.g. "Credit Limit") or `Purchases`. |
| `Closing` `Balance` ... next `Account` or `Current` | 7029-7071 | The closing balance: the words in between. A `Closing Balance` straight after `Information` or `payment` is ignored (7038): the payment slip repeats it under "Payment Information". `Overdue` is also a landmark (7038), so a closing balance followed by an overdue line is not taken. |
| `Account` `Number` ... `Statement` / `Payments` / `Page` | 7073-7110 | The account number (the masked card number). |
| `STATEMENT` (upper case) ... `Westpac` `Cards` | 7112-7134 | The cardholder name: the words after the title word `STATEMENT`, stopping at the first word holding a digit or at `Flat` / `Unit` / `Apartment` (7128): the start of the postal address. `Westpac Cards` (a contact line) closes the window. (`ite` + 8 words is a second, unexplained start, 7118.) |
| `AMOUNT` `$` | 7143 | Table start, on every page that carries rows. |
| `Ways`, `Continued`, `Cheque`, `Minimum`; `Interest` `rate`; `Westpac` `New` (Zealand Limited); `Account` `number` | 7143 | Table end: "Continued over page", the footer, the interest-rate box, "Ways to pay", the payment slip. Starts and ends are paired as start + the next landmark (7138-7149). |

Pairing constraints that follow from these rules, so the lookalikes respect them:

- One `Closing Balance` per statement, followed in reading order by `Account` (here the
  `Account Number` line), with no other `Account`, `Current` or `Overdue` word
  before it in that statement. Otherwise the pairs shift, and a later statement's
  closing balance comes out empty.
- Upper-case `STATEMENT` once per statement (continuation pages must not repeat it),
  and a `Westpac Cards` line after the address.

### Table columns, in order

| # | Column | Printed as | QVF |
|---|---|---|---|
| 1 | Transaction date | `dd Mon` (2 words) | The column ends at the month word (7364-7370). |
| 2 | Date processed | `dd Mon` | **Skipped**: `vPosition + 2` (7368). |
| 3 | Transaction details | free text | Ends when the next word is a plain decimal with exactly two decimals (digits and one dot only: **no `$`, no thousands comma**), and either the word after it is `CR` / `New Statement`, or the third word is `CR` / `New Statement` / a month name (7376). |
| 4 | Amount | `45.67`, a payment or refund `500.00 CR` (`CR` a separate word) | The row ends when the word two ahead is a month name or the next word is `New Statement` (7352). |

Because the details test needs a bare decimal, transaction amounts of $1,000 or more
must be printed **without** a thousands separator (`1250.00`). A comma would make
the details swallow the amount. The summary figures do carry `$` and commas.

### Money in and money out

- Row amounts (7408-7432): a string holding `CR` has its last three characters
  (` CR`) cut and stays **positive** (a payment or refund, money in for the holder).
  Everything else is made **negative** (a purchase, interest or a fee). Spaces are
  removed.
- Closing balance (`sBalances`, 136-187): `$` stripped; `CR` (as ` CR`, `,CR` or
  `CR`) cut and positive (the bank owes the holder). Otherwise negative (the holder
  owes), except `0.00`.

### The year (7434-7450)

If the period **starts** in Oct, Nov or Dec and the row's month is Oct, Nov or Dec,
the row takes the start year. Every other row takes the **end** year. This handles
a December-January statement. It does **not** handle a January statement that lists
a purchase made on 30-31 December and processed in January: that row gets the
following December's year (lookalike `wpcc_7`).

### Cleaning inside the transaction list (lines it drops)

- **Masked card numbers and cardholder lines** (7152-7219, 7245-7253): the word after
  a `****` is dropped (the last four digits). Then, ten times forwards and ten times
  backwards, any word with no digit right after or right before a `****` is
  dropped (not `CR`, not `New Statement`), and so is any `*1234`-style word. Then
  the `****` words go. This removes the card sub-heading, the name on either side of
  the masked number, and up to ten words of it. The `CR` exception keeps a
  payment's `CR` that ends the row before a sub-heading.
- **Foreign currency fee lines** (7221-7243): the word after `Foreign Currency Fee`
  is flagged, then the next three, then everything up to five words before any
  flagged word. The net effect is a fixed window: two words before `Foreign`
  (the currency and the foreign amount) to four words after `Fee`. The lookalike
  line is `USD 30.00 Foreign Currency Fee $0.92 included in amount`.
- **A second details line swallowed into the amount** (7410-7416): if the amount
  string holds `Foreign`, it is cut to the first figure. Side effect: a foreign
  *refund* written this way would lose its `CR` and flip sign.
- **`General Payments & Charges`** (7251): `General`, `Payments`, `Charges` are
  dropped when they come straight after a `New Statement` marker. This matters
  because the statement number of statement 2+ is read off the word just before its
  first date (7298), which must be the marker itself. (This assumes the connector
  drops the `&`.)
- Everything outside the start/end pairs: the header, the summary, the interest
  rate box, "Ways to pay", the payment slip and the footers.

### Several statements in one file

- Each `Statement Period` is a `New Statement` marker, merged into the word stream
  (7162-7169).
- On a marker (7292-7337), the script seeks the next date. The statement number is
  the marker's own count (7298). The period, account number and name are re-read.
  The `vPreviousBalance` set at 7310-7320 is leftover: it is never used.
- The filter at 7167 peeks the **table being appended to**. If the peek sees rows
  appended in the same load, only the first marker passes. This needs checking on
  the server. A single statement with **no transactions** leaves the date search
  with nothing to find (`sLoopToDate` loops until it meets a month name, 261-299).

### Opening and closing balance, balances

- Only the **closing** balance is read. The printed opening balance and the summary
  totals are not read at all.
- `sEndofData` (no type, 1194-1470): rows are sorted by statement number and date,
  the closing balance is put on the last row, and balances are rebuilt **upward**
  from it. An `Opening Balance` row is added with the computed opening.
- `sBalanceCheck` (1474-1590) starts from that computed opening, so for this type
  `Balance Pass` can never fail. A missed or doubled row cannot be caught.

## Output columns the QVF fills for this type

| Output column | How |
|---|---|
| File Name, Doc Reference Bank Statement, Doc Reference Bank Voucher | From the upload form. |
| Sort Number, Row ID | As for every PDF type. Row 1 of each statement is the synthetic `Opening Balance` row. |
| Bank | `'Westpac'`. |
| Account Name | The cardholder name after `STATEMENT` (stops at the first digit word). An upper-case `FLAT 2, ...` address is **not** a stop word, so the name runs on (`wpcc_4`). |
| Account Number | The words after `Account Number` (the masked card number). |
| Other Party Account Name / Number, Transaction Time | Empty. |
| Date | The transaction date plus the inferred year, `DD/MM/YYYY`. The processed date is dropped. |
| Year, Tax Year | As for every PDF type. |
| Description as per bank statement | The details. The foreign currency fee line is removed. The cardholder/card sub-heading is removed. |
| Transaction Type | `Withdrawal` / `Deposit` from the sign. |
| Transaction Code, Code Description, Transaction Category | Keyword workbook lookup, defaults 200/400 "To be done". Inter-account matching rarely fires (the account number is a masked card). |
| Amount | Absolute value. |
| Balance | **Computed** running balance (none is printed). |
| Balance Check / Balance Pass | Vacuous for this type (see above). |
| Order | Sorted by date within each statement, so the card sections are merged into one date order. |

## Lookalikes (`gen/make_westpac_cc.py` writes into `sets/kbwp/`)

Every file is checked by `qvf_walk()`, which re-does the QVF's walk on the PDF's
words (landmarks, closing balance, account number, name, card and FX cleaning, rows,
year rule). All 7 pass. Three **deliberate** divergences are reported as expected:
the `FLAT` name run-on (wpcc_4), the never-ending date search (wpcc_6) and the
December dates a year late (wpcc_7).

| Case | What it tests |
|---|---|
| wpcc_1 | One page, one card, a payment, purchases and a refund. |
| wpcc_2 | Three pages, a primary and an additional card (two sub-headings), overseas purchases with the foreign-currency-fee line, "Continued over page", an annual fee, two payments, amounts over $1,000 printed without a separator. |
| wpcc_3 | Period 16/12/2023 - 15/01/2024 (the Oct-Dec year rule), interest charged rows. The summary prints "Interest Charged" apart from "Purchases & Debits". |
| wpcc_4 | The payment overshoots, so the closing balance prints `$82.27 CR`. Refunds. A payment over $1,000. An upper-case `FLAT` address. |
| wpcc_5 | Three consecutive monthly statements concatenated, each restarting "Page 1 of N", balances chaining. |
| wpcc_6 | No transactions in the period, and a `$25.00 CR` balance carried unchanged. |
| wpcc_7 | A January statement with purchases dated 30-31 December, processed 3-4 January. |

Truth convention: date = the transaction date (the first date). A purchase is a debit
and a payment or refund a credit, from the holder's side. Opening and closing are the
amounts owed, negated. Rows are in printed order (section by section).

## Measured on the new tool (2026-10-05, `score_auto.R`, cold = trained)

7 statements: auto_right 0, AUTO_WRONG 0, check_right 4 (wpcc_1, 2, 4, 5), check_wrong 2
(wpcc_3, 7), unread 1 (wpcc_6). Every figure is read right on wpcc_1/2/4/5. They are held
back only because the section title `General Payments & Charges` is taken as the heading
of both date columns (`R/auto_read_pdf.R:639-643`), so "TRANS DATE" vs "DATE PROCESSED"
is never seen. With that line removed (a probe), wpcc_1, 2 and 4 prove, and wpcc_5 proves
end to end. Then three things remain: the year of 30-31 Dec rows on a January statement
(`R/parse_pdf_table.R:738`), a summary printing interest apart from purchases
(`R/auto_read.R:1838-1880`), and the no-transaction statement. The full diagnosis is in
the area report.
