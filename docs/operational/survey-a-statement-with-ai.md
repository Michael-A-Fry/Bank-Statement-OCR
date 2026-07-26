# Survey a statement with an AI assistant (no personal information)

**What this is for.** When a statement won't convert — or converts but looks
wrong — the fix almost always needs the same handful of facts about how that
bank lays its page out. This page holds one ready-made prompt you paste into
Copilot (or any AI assistant) **with the statement attached**. It returns a
structured survey of the layout that contains **no client information**, so you
can paste it into a chat, an email or a ticket and get a template or a fix built
from it.

Run it over a stack of statements and you build a picture of every layout your
team actually receives, without any of them leaving the room.

**Why it exists.** Some problems can only be solved with real examples — how
distinctive a form's wording is, how a scanner mangles a particular typeface,
what a bank calls its closing balance. We can't fix those from one sample and we
can't fix them from a redacted picture. We *can* fix them from twenty-five
honest descriptions of the page.

---

## How to use it

1. Open Copilot and **attach the statement** (PDF, CSV or Excel — the prompt
   handles all three).
2. Paste the whole prompt below, unchanged, and send.
3. Read the reply, run the 30-second check at the bottom of this page, then send
   the reply on.

Do this once per statement. Numbering the files (`survey-01`, `survey-02`, …) as
you go makes a batch much easier to work through afterwards.

**If Statement Studio already had a go at the file**, paste what it said as well
— the red message on the Convert page, or the **Diagnostics** table on the
result page. Its wording (`date_parse`, `row_count`, `balance_break`,
`reconciliation_mismatch`, `amount_parse`, `row_parse`, `date_out_of_range`,
`redaction_unverified`, `scanned_no_ocr`) points straight at the part of the
survey that matters, and saves a round trip.

---

## The prompt

Copy everything between the lines.

~~~~
You are helping me describe the LAYOUT of a bank statement so that a rules-based
parser can be configured to read it. I have attached the document.

Produce a written survey of how the document is laid out. Use the exact section
headings and order given below, so that surveys of different statements can be
compared side by side.

=== RULE 1: NO PERSONAL INFORMATION ===

Never reproduce anything specific to the customer or their money. Specifically,
do NOT output:
  - any person's or business's name, signature, address, phone, email
  - account numbers, card numbers, IRD/tax numbers, client or member numbers
  - real transaction descriptions (merchant names, payee names, references)
  - real money amounts, real balances, or real dates

Where you would have used one of those, use a placeholder of the SAME SHAPE
instead, so the shape is preserved and the value is not:
  a name          -> [NAME]
  an address      -> [ADDRESS]
  an account no.  -> keep the punctuation, replace every digit with 9
                     (00-1234-5678901-00  ->  99-9999-9999999-99)
  a description   -> [DESCRIPTION] , or describe the kind
                     ("a merchant name, usually shouty capitals, up to ~30 chars")
  a money amount  -> an invented amount that is punctuated EXACTLY like the real
                     one (1,234.56  or  1.234,56  or  (45.00)  or  45.00-)
  a date          -> an invented date written EXACTLY like the real one
                     (07/04/2025  or  7 Apr 25  or  2025-04-07)

=== RULE 2: THE BANK'S OWN WORDING IS NOT PERSONAL INFORMATION ===

Anything the bank PRINTS ON EVERY COPY of this statement is exactly what I need,
and it is not personal information. That means: the bank's name and the product
name, column headings, section titles, the words "Opening balance" / "Closing
balance" / "Total", footer text, page-numbering wording, small print.

Reproduce those EXACTLY, character for character, including capital letters,
punctuation and spacing. Do not tidy them up, translate them, correct their
spelling or shorten them. A parser matches this text literally, so "OPENING
BALANCE", "Opening Balance" and "Opening balance" are three different things.

=== RULE 3: NEVER GUESS ===

If you cannot tell, write UNKNOWN and say what you would need in order to tell.
If a section does not apply to this document, write "n/a".
Do not invent a plausible answer. A wrong answer here is worse than no answer,
because it will be used to configure a parser that then reads the wrong numbers
without complaining.

=== THE SURVEY ===

1. DOCUMENT IDENTITY
   1.1 Bank / provider, exactly as printed on the page.
   1.2 Product or account type as printed ("Everyday", "Streamline", "Visa
       Platinum", "KiwiSaver annual statement", ...).
   1.3 What kind of document is it? One of:
       TRANSACTION STATEMENT (a table of transactions)
       SUMMARY / FORM (labelled figures, no transaction table — e.g. a tax
       certificate, a KiwiSaver annual statement)
       MIXED (both)
   1.4 File type: PDF / CSV / Excel / other.
   1.5 If PDF: is the text selectable, or is the page a scanned image?
       Say DIGITAL TEXT, SCANNED IMAGE, or MIXED (and which pages are which).
       If you cannot select text at all, say SCANNED IMAGE.
   1.6 Number of pages. Page size and orientation (A4 portrait, A4 landscape,
       Letter, ...).
   1.7 Currency, and how it is shown ($ / NZ$ / NZD / no symbol at all).
   1.8 Language, and any non-English characters that appear anywhere.

2. FINGERPRINT PHRASES  (how the parser will recognise this bank)
   List 3 to 8 short phrases that:
     - are printed on EVERY statement this bank issues, not just this one
     - would NOT appear on another bank's statement
     - are on page 1 if possible
   Quote each one exactly. Prefer the masthead, the product name, a distinctive
   column heading, and any bank-specific footer or small print.
   Then, for each phrase, say whether you think a DIFFERENT bank could plausibly
   print the same words. Say so honestly — a phrase like "Date Description
   Amount Balance" appears on almost every statement in the world and is useless
   as a fingerprint.

3. THE TRANSACTION TABLE   (skip if the document is a SUMMARY / FORM)
   3.1 The column headings, in left-to-right order, exactly as printed. If the
       table has no headings, say so and describe the columns instead.
   3.2 Do the headings repeat at the top of every page? Yes / No / only on some.
   3.3 Reproduce THREE consecutive transaction rows, with every value replaced
       per RULE 1, but with the SPACING PRESERVED so the columns still line up.
       Put them in a code block, with the heading row above them, like this:

       ```
       Date       Description                    Withdrawal   Deposit    Balance
       07 Apr     [DESCRIPTION]                       45.00              1,234.56
       07 Apr     [DESCRIPTION]                                120.00    1,354.56
       08 Apr     [DESCRIPTION]                       12.34              1,342.22
       ```

       Keep the column positions as close to the original as you can — the
       alignment is the most useful single thing in this whole survey.
   3.4 Is each column left-, right- or centre-aligned? (Amounts are usually
       right-aligned; getting this wrong splits columns in the wrong place.)
   3.5 Roughly how many transaction rows per page?

4. DATES
   4.1 How is a transaction date written? Give the pattern, and one invented
       example written the same way.
       Use these codes: %d day, %m month number, %b month name short (Apr),
       %B month name long (April), %Y 4-digit year, %y 2-digit year.
       e.g. "07/04/2025" is %d/%m/%Y ; "7 Apr 25" is %d %b %y.
   4.2 Does the transaction date include the YEAR? If not, where on the page is
       the year printed (statement period line, header, footer)? Quote that
       line's wording exactly, with invented dates.
   4.3 Is there MORE THAN ONE date column (e.g. transaction date and processed
       date)? Which one is the one that matters?
   4.4 Do all rows use the same date format, or does it change anywhere in the
       document?
   4.5 Is the date repeated on every row, or printed once for a group of rows
       that share it?

5. AMOUNTS
   5.1 Which of these five shapes does this statement use?
       (a) SIGNED         one amount column; money out has a minus sign
       (b) DEBIT/CREDIT   two separate columns (Withdrawal/Deposit,
                          Debit/Credit, Money out/Money in, Paid out/Paid in)
       (c) DR/CR SUFFIX   one amount column with DR or CR after the number
       (d) TYPE COLUMN    a separate short code column (D/C, DR/CR) says which
       (e) UNSIGNED       every amount is positive and the direction is implied
                          — typical of credit cards, where a bare amount is a
                          charge and "CR" marks a payment
   5.2 Quote the exact headings of whichever columns carry money.
   5.3 Thousands separator and decimal mark: 1,234.56 or 1.234,56 or 1 234.56
       or none. Give an invented example punctuated exactly like the real one.
   5.4 How is a negative shown? -45.00 / (45.00) / 45.00- / 45.00 DR / a red
       colour only / never negative.
   5.5 Is a currency symbol printed on each amount, or only in the heading?
   5.6 Are there any foreign-currency transactions? If so, how are the original
       amount, the rate and any conversion fee shown — on the same row, or on
       extra lines underneath?

6. BALANCES
   6.1 Is there a running balance column on every row? Yes / No / on some rows.
   6.2 Quote the EXACT wording used for the opening balance, and say where it
       appears (its own row in the table / a summary box / the header).
   6.3 Quote the EXACT wording used for the closing balance, and where.
   6.4 Any other balance-ish labels printed anywhere: "Balance brought forward",
       "Balance carried forward", "Total", "Available balance", "Credit limit",
       "Minimum payment", "Closing balance at 30 April". Quote each exactly.
   6.5 How is an overdrawn / negative balance shown? (-, OD, DR, brackets)

7. STATEMENT-LEVEL INFORMATION  (say WHERE it is, never WHAT it says)
   For each of the following, say whether it appears, and where on the page —
   top-left of page 1, a boxed panel top-right, the footer of every page, and
   so on. Do NOT reproduce the values.
   7.1 Account name          7.2 Account number
   7.3 Statement period      7.4 Statement or issue date
   7.5 Page numbering, and the exact wording ("Page 1 of 4", "1/4", "- 1 -")
   For 7.3, quote the LABEL and the connecting words exactly, with invented
   dates: e.g. "Statement period: 01 April 2025 to 30 April 2025", or
   "For the period 1 Apr 25 - 30 Apr 25".

8. LINES THAT ARE NOT TRANSACTIONS
   List every line inside or around the table that carries a money amount but is
   NOT a transaction, quoting its exact wording. These are the lines a parser
   most often mistakes for transactions. Look for:
     - opening / closing balance rows
     - "Balance brought forward" / "carried forward" at page breaks
     - sub-totals, section totals, "Total withdrawals", "Total deposits"
     - interest or fee summary boxes
     - section headings that group transactions by date, type or card
     - repeated page headers and footers
     - marketing or small-print blocks that mention amounts

9. AWKWARD BITS  (be blunt — this section is the most valuable one)
   9.1 Do any transaction descriptions wrap onto a second or third line? If so,
       do the continuation lines have anything else on them (a date, an amount),
       or are they text only?
   9.2 Does any single transaction span a page break?
   9.3 Are there rows with a blank or missing amount? Blank date? Blank
       description?
   9.4 Does the document contain MORE THAN ONE account or statement (several
       accounts bundled into one PDF)? If so, how does a new one start — a new
       page, a heading, a repeated masthead? Quote the wording that marks it.
   9.5 Is anything blacked out, blanked out or replaced with XXXX / #### ?
       Say where and how it looks. Do not attempt to read underneath it.
   9.6 Are there watermarks, stamps, "COPY"/"DUPLICATE" overlays, or a coloured
       background behind the table?
   9.7 If the page is scanned: is it straight or tilted? Speckled? Faint? Are
       any digits genuinely hard to read even for you — and which ones (a 3/8,
       5/6 or 0/O confusion is the usual one)?
   9.8 Does the table have ruled lines / boxes around cells, or is it held
       together by whitespace alone?
   9.9 Anything else about this document that you think would trip up a program
       reading it by rules.

10. WHAT WOULD BREAK A RULES-BASED PARSER
    In plain sentences, name the THREE things about this document most likely to
    make an automated reader get the wrong answer WITHOUT NOTICING. Be specific
    and be pessimistic. "The date has no year, so a statement spanning New Year
    would be read into the wrong year" is the kind of answer I want.

11. MACHINE-READABLE SUMMARY
    Finish with this exact block, filled in. Use null for anything you do not
    know. Do not add or rename keys. Keep the values short.

```yaml
survey:
  bank: ""                    # as printed
  product: ""                 # as printed
  doc_kind: ""                # statement | form | mixed
  file_type: ""               # pdf | csv | excel | other
  pdf_text: ""                # digital | scanned | mixed | n/a
  pages: 0
  page_size: ""               # A4 portrait | A4 landscape | Letter | ...
  currency: ""                # NZD | AUD | GBP | ...
  fingerprint_candidates: []  # exact phrases, most distinctive first
  fingerprint_risk: ""        # low | medium | high  (could another bank print these?)
  columns_in_order: []        # headings exactly as printed, left to right
  date_format: ""             # e.g. "%d/%m/%Y"
  date_has_year: null         # true | false
  year_source: ""             # where the year comes from when the row has none
  second_date_column: ""      # heading, or ""
  amount_style: ""            # signed | debit_credit_cols | dr_cr_suffix | type_dc | unsigned
  amount_columns: []          # headings of the money columns
  decimal_mark: ""            # dot | comma
  thousands_mark: ""          # comma | dot | space | none
  negative_style: ""          # minus | brackets | trailing_minus | dr_suffix | none
  running_balance: null       # true | false
  opening_balance_label: ""   # exact wording
  closing_balance_label: ""   # exact wording
  period_label: ""            # exact wording of the statement-period line
  summary_labels: []          # exact wording of every non-transaction money line
  wrapped_descriptions: null  # true | false
  multiple_statements: null   # true | false
  redactions_present: null    # true | false
  rows_per_page: 0
  top_risks: []               # the three from section 10, one short line each
```

=== BEFORE YOU ANSWER ===

Re-read your draft and check every one of these:
  - No real name, address, account number, description, amount or date anywhere,
    including inside the example rows and inside the YAML block.
  - Every phrase I asked you to quote exactly IS exact — same capitals, same
    punctuation, same spacing.
  - Every UNKNOWN is a genuine "I can't tell", not a guess you softened.
  - The YAML block parses and uses only the keys listed above.
~~~~

---

## What a good answer looks like

A fictional example, so you can tell a useful reply from a vague one. This is the
shape and the level of detail to expect:

> **2. FINGERPRINT PHRASES**
> 1. `"Riverbank Streamline Account"` — masthead, page 1. No other bank would
>    print this. **Distinctive: yes.**
> 2. `"Your transaction summary"` — above the table, every page. Another bank
>    could plausibly print this. **Distinctive: weak.**
> 3. `"Riverbank Limited. Registered in New Zealand."` — footer, every page.
>    **Distinctive: yes.**
>
> **5. AMOUNTS**
> 5.1 (b) DEBIT/CREDIT — two separate columns.
> 5.2 Headings exactly as printed: `Money out` and `Money in`.
> 5.3 Comma thousands, dot decimal: `1,234.56`.
> 5.4 Negatives never appear; direction is carried by which column is filled.
>
> **8. LINES THAT ARE NOT TRANSACTIONS**
> - `"BALANCE BROUGHT FORWARD"` — first row of the table on pages 2+.
> - `"Total money out"` / `"Total money in"` — last two rows, page 4 only.
> - `"Closing balance as at 30 April 2025"` — boxed, below the table.
>
> **10. WHAT WOULD BREAK A RULES-BASED PARSER**
> 1. The transaction date has no year (`07 Apr`), and the year only appears in
>    the period line at the top of page 1 — a statement crossing 31 December
>    would be read into the wrong year on half its rows.
> 2. `"BALANCE BROUGHT FORWARD"` sits inside the table with a figure in the
>    balance column, so a parser that keeps any row with a number will count it
>    as a transaction and double the opening balance.
> 3. Descriptions wrap to a second line that carries no date and no amount, so a
>    parser that requires a date on every row will silently drop the second half
>    of long descriptions.

If what comes back is thinner than that — vague phrases, "various amounts",
sections skipped — reply with:

> Sections 8, 9 and 10 are the ones I actually need. Answer them again in more
> detail, quoting the wording exactly, and don't skip anything you're unsure
> about — mark it UNKNOWN instead.

---

## Before you send the reply on

Thirty seconds, every time:

1. **Search the reply for a digit.** Every number left in it should be an
   invented example, a page count, a row count or a coordinate. If a real amount
   or a real account number survived, delete it.
2. **Search for a capital letter run** that could be a merchant or a person.
   Descriptions should all read `[DESCRIPTION]`.
3. **Check the dates.** They should be obviously invented, not the statement's
   real ones.

The assistant is told to do all of this. It is still your reply, and you are the
one sending it, so glance at it.

---

## What comes back, and what it turns into

| Survey section | What it configures |
|---|---|
| 2 — fingerprint phrases | `fingerprint:` in the template — how the bank is recognised |
| 3 — the table | the columns, and for a PDF the column bands you'd otherwise drag by hand |
| 4 — dates | `date_format` (and whether the year has to be inferred) |
| 5 — amounts | `amount_sign`, `decimal_mark`, negative handling |
| 6 — balances | reconciliation, and the **label dictionary** — new wordings for opening/closing balance |
| 7 — statement info | the header values shown on the result page |
| 8 — non-transaction lines | the summary-line wordings, so totals aren't counted as transactions |
| 9 + 10 — awkward bits | the edge-case register, and whichever engine change the batch turns out to need |

Sections 1–8 are usually enough to build a working template. Sections **9 and
10 are the ones worth collecting in bulk**: twenty-five statements' worth of
"what would break this" is what tells us which engine improvements are actually
worth making, instead of guessing from one sample.

Related: [Teach it a new bank layout](adding-a-bank-template.md) ·
[When something looks wrong](when-something-goes-wrong.md) ·
[Edge-case register](../context/edge-cases.md)
