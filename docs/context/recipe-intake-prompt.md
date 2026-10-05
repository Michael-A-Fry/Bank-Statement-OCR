# Copilot prompt: describe a statement design so a recipe can be written

**How to use.** Copy everything between the two lines of `=` into Copilot and
attach ONE statement (PDF, Excel or CSV). Copilot answers with a block that
starts `STATEMENT DESIGN` and ends `END OF DESIGN`. Paste that block to Claude
and say: "Write a recipe from this." If any answer is `UNSURE`, run the prompt
again on a second statement of the same design and paste both blocks.

Each numbered answer maps to one field of a recipe file (`recipes/*.yaml`,
read by `R/recipes.R`); the field is named in brackets so the mapping is exact.

==============================================================================
You will describe the LAYOUT of the attached bank statement so that a
developer can write a reader for this statement design. The developer must not
learn anything about the customer. Follow every rule below exactly. Do not
summarise, do not explain, do not add sections. Output only the block at the end.

PART A -- PRIVACY RULES. Apply to EVERY answer, including the sample.
A1. NEVER output: a person's or business's name; an address; a phone number;
    an email; an IRD or GST number; any account, card, customer, member,
    loan, facility or statement number; a branch name tied to the customer;
    any merchant, payee, other party, particulars, code or reference value.
A2. NEVER output a real amount or balance. When an answer needs a figure,
    invent one that has EXACTLY the same printed shape: same number of digits
    before the decimal point, same thousands separator, same decimal places,
    same currency symbol and its position, same minus sign and its position,
    same brackets, same trailing letters (CR, DR, OD), same spacing.
      Real "1,234.56 DR"  ->  write "8,765.43 DR"
      Real "-$12.40"      ->  write "-$56.78"
      Real "(300.00)"     ->  write "(700.00)"
A3. NEVER output a real date value. Keep the FORMAT, change the day/month/year.
      Real "03 Feb"       ->  write "17 Feb"
      Real "03/02/2026"   ->  write "17/05/2026"
A4. Replace other values with these placeholders, keeping their length roughly:
      person or business name  -> PERSON A, PERSON B, BUSINESS A
      merchant / payee         -> MERCHANT A, MERCHANT B
      reference / particulars  -> REF 1001, PART 2002, CODE 3003
      account / card number    -> same shape with 0 or X, e.g.
                                  00-0000-0000000-00 or XXXX XXXX XXXX 0000
A5. KEEP WORD FOR WORD (same spelling, same capital letters, same punctuation)
    the bank's own fixed wording: titles, box headings, field labels, column
    headings, summary labels, section titles, footer phrases, and words such as
    "Opening balance", "Totals at end of page", "Balance brought forward",
    "No transactions for this period". These are the same on every customer's
    statement and are exactly what the reader searches for. Do not correct,
    reword, abbreviate or translate them.
A6. If you are not certain whether something is fixed bank wording or customer
    information, treat it as customer information and replace it.

PART B -- HOW TO ANSWER
B1. Answer every numbered item. If the statement does not have the thing, write
    NONE. If you cannot tell, write UNSURE and one short reason.
B2. Where an item gives CHOICES, write exactly one of the choices, spelled as
    shown, and nothing else on that line.
B3. Where an item asks for EXACT TEXT, copy it from the statement character for
    character and put each phrase in double quotes.
B4. Where an item asks for a LIST, give one quoted phrase per line, in the
    order the statement prints them (top to bottom, left to right).
B5. Look at EVERY page before answering, not just page 1.

PART C -- THE BLOCK TO OUTPUT (copy this structure exactly)

STATEMENT DESIGN

1. BANK [bank]
   CHOICES: anz | asb | bnz | westpac | kiwibank | tsb | cooperative | sbs |
   heartland | rabobank | amex | other: <bank name as printed>

2. PRODUCT [title]
   The product as the statement names it, e.g. "Visa credit card",
   "Freedom account", "Home loan". EXACT TEXT if printed, otherwise describe in
   3 words or fewer.

3. FILE KIND [kind]
   CHOICES: pdf-text | pdf-scanned | excel-xlsx | excel-xls | csv
   (pdf-text = you can select/copy the words; pdf-scanned = the pages are images)
   Number of pages:

4. RECOGNISE BY -- MUST APPEAR [recognise: all]
   LIST of 2 to 4 phrases that (a) are printed on EVERY statement of this
   design, (b) are fixed bank wording, (c) are at least two words long,
   (d) contain no digits, dates or customer details. Prefer, in this order:
   the table's longest column heading, a box or section heading, the
   statement title. EXACT TEXT.

5. RECOGNISE BY -- MUST NOT APPEAR [recognise: none]
   LIST of phrases that this bank prints on a DIFFERENT product's statement but
   NOT on this one (e.g. a credit card's "Minimum payment due" when this is an
   everyday account). Only phrases you can see are absent here. EXACT TEXT.
   NONE if you do not know.

6. EACH STATEMENT STARTS WITH [statement_starts]
   EXACT TEXT of one phrase printed once at the top of the first page of each
   statement (a box heading or title), and not printed on the other pages.
   Then: Does this file hold more than one statement?  CHOICES: yes | no
   Does "Page 1 of N" (or similar) restart for each statement?
   CHOICES: yes | no | no page numbers

7. STATEMENT PERIOD [period: label, open_start]
   a) EXACT TEXT of the words printed immediately before the period's dates,
      e.g. "Statement period". If the dates come first and the label after,
      say so.
   b) The period as printed, with fake dates (rule A3), e.g.
      "Statement period 17 Feb 2026 to 16 Mar 2026".
   c) Word that joins the two dates. CHOICES: to | - | through | until | other: "<text>"
   d) Does any statement of this design print a word instead of a start date,
      e.g. "START - 30 Sep 2019"? EXACT TEXT of that word, or NONE.
   e) Statement date / issue date: EXACT TEXT of its label, and its format
      with a fake value. NONE if not printed.

8. OPENING BALANCE
   a) EXACT TEXT of the label.  b) Where. CHOICES: summary box | first row of
   the table | both | not printed.  c) A fake value in the printed shape (A2).

9. CLOSING BALANCE
   a) EXACT TEXT of the label (e.g. "Closing balance", "New balance",
   "Current balance", "Balance carried forward").  b) Where. CHOICES: summary
   box | last row of the table | both | not printed.  c) A fake value (A2).

10. PRINTED TOTALS
   LIST each total the statement prints, as: "EXACT LABEL" | where (summary
   box / end of each page / end of statement / end of each section) | which
   money it totals (money out / money in / both / net). If a row of labels is
   printed with the figures on the line BELOW the labels, write
   LABELS ABOVE FIGURES. NONE if no totals.

11. TABLE HEADING LINE [table: header]
   LIST every column heading on the transaction table's heading line, LEFT TO
   RIGHT, EXACT TEXT. One item per heading as printed. If one heading stands
   over several printed columns, give it once and add "(spans N columns)". If a
   heading is printed on two lines, give the full heading on one line
   ("Date" over "Processed" -> "Date Processed").
   Then: Is the heading line printed again on every page with transactions?
   CHOICES: yes | first page only | other: <say>
   Then: EXACT TEXT of any title printed directly ABOVE the heading line
   (e.g. "General Payments & Charges"), or NONE.

12. COLUMNS [table: columns]
   One line per heading from item 11, in the same order, in this form:
   "HEADING" -> ROLE | FORMAT | ALIGN | FILLED
   ROLE CHOICES:
     date        the transaction's date (only ONE column may be date)
     second-date another date (e.g. processed / value date)
     description the words saying what the transaction is
     debit       money OUT of the account, in its own column
     credit      money IN to the account, in its own column
     amount      ONE column for both in and out (sign, CR/DR or brackets)
     balance     the balance after each row
     type        a word or code per row (e.g. D/C, DR/CR, AP, BP)
     other-text  any other words (reference, particulars, code, card number)
     other-money any other figure (foreign currency amount, fee)
   FORMAT: for dates use exactly one of the date codes in item 13; for money
   write a fake value in the printed shape (A2); for words write "words".
   ALIGN CHOICES: left | right | centre
   FILLED CHOICES: every row | some rows | only money-out rows |
   only money-in rows

13. DATE FORMAT [dates: format, year]
   The format of the transaction date column, as EXACTLY ONE of these codes
   (%d = day 01-31, %m = month 01-12, %b = Jan..Dec, %B = January..December,
   %y = 2-digit year, %Y = 4-digit year):
     %d/%m/%Y   %d/%m/%y   %Y-%m-%d   %d-%m-%Y   %d-%m-%y   %d.%m.%Y
     %d.%m.%y   %Y/%m/%d   %m/%d/%Y   %d %b %Y   %d %B %Y   %d %b %y
     %d-%b-%Y   %b %d, %Y  %B %d, %Y  %d %b      %d %B      %b %d
     %B %d      %d/%m      %d-%b      %d-%b-%y
   If none fits, write OTHER and show a fake example.
   Does the row date print a year?  CHOICES: yes | no
   Is the date printed on every row, or only on the first row of each day?
   CHOICES: every row | first row of each day

14. MONEY IN AND OUT [money: style, negative, positive]
   a) CHOICES:
      separate columns  (money out and money in each have a column)
      one signed column (one amount column; out shown by minus, DR or brackets)
      type column       (one amount column; a separate column of words/codes
                         says in or out)
   b) How a money-OUT figure is written (fake value): ____
   c) How a money-IN figure is written (fake value): ____
   d) Letters printed after a figure to mean negative/overdrawn/debit.
      LIST from: "OD" | "DR" | "-" (trailing minus) | NONE
   e) Letters printed after a figure to mean positive/credit.
      LIST from: "CR" | NONE
   f) If type column: LIST every value it shows and what each means, e.g.
      "D" = money out, "C" = money in. EXACT TEXT for the values.
   g) Transaction codes printed at the start of descriptions (e.g. AP, BP, DC,
      EP): LIST each code with its meaning if the statement prints a legend.

15. RUNNING BALANCE
   CHOICES: every row | some rows | never
   How an overdrawn balance is shown (fake value): ____ or NONE

16. ROW ORDER [order]
   CHOICES: oldest_first | newest_first

17. LINES INSIDE THE TABLE THAT ARE NOT TRANSACTIONS [table: skip]
   LIST, EXACT TEXT of how each such line starts, e.g.
   "Balance brought forward", "Opening balance", interest-rate lines,
   "Rate change", cardholder or card headings (names replaced, A4),
   "Total for card ending", foreign-currency detail lines
   ("Foreign currency amount ..."), "- processed on:". NONE if none.

18. LINES THAT END THE TABLE [table: ends_at]
   LIST, EXACT TEXT of the start of each line that comes straight after the
   last transaction on a page or at the end of the statement, e.g.
   "Totals at end of page", "Balance carried forward", "Closing balance",
   a footer phrase. NONE if the table just stops.

19. NO TRANSACTIONS [table: no_rows]
   EXACT TEXT a statement of this design prints when it has no transactions,
   or NONE / UNSURE.

20. ROWS ON MORE THAN ONE LINE
   CHOICES: never | sometimes | often
   What the extra lines hold. CHOICES (one or more): more description |
   reference/particulars | foreign currency detail | processed date | other: <say>
   On a multi-line row, which line carries the money figure?
   CHOICES: first line | last line | varies

21. SECTIONS
   Does one statement hold several sections (cardholders, cards, accounts,
   "Payments" vs "Purchases")? CHOICES: no | yes
   If yes: EXACT TEXT of how a section starts (names replaced) and how it ends,
   and does each section restart its dates? CHOICES: yes | no

22. SPREADSHEETS ONLY (excel / csv; otherwise NONE)
   a) Sheet names (EXACT TEXT).  b) Rows above the column-header row: their
   labels only (EXACT TEXT), values replaced.  c) Column headers, LEFT TO
   RIGHT, EXACT TEXT.  d) How a negative or overdrawn figure is shown:
   CHOICES: minus | OD in the text | OD only in the cell's number format |
   brackets.  e) Cells that hold "." or "-" instead of a number?
   CHOICES: yes | no.  f) Rows after the transactions (pending, unstatemented)?
   EXACT TEXT of their heading, or NONE.

23. ANYTHING ELSE a reader could trip on, one line each, or NONE.

SAMPLE
Reproduce as plain text, keeping the columns roughly lined up as printed:
- the line directly above the heading line (if any),
- the heading line,
- 8 to 12 rows chosen so that, if the statement has them, there is at least
  one of each: money out, money in, a row on more than one line, an overdrawn
  balance, a non-transaction line from item 17, a line from item 18,
- the summary box lines for opening, closing and totals (items 8-10).
Apply PART A to every value. Keep every piece of fixed bank wording exactly.

SELF-CHECK (do this before answering, and fix anything that fails)
- No real name, number, amount, date value or merchant anywhere (PART A).
- Every EXACT TEXT phrase is copied character for character.
- Every CHOICES line holds exactly one listed choice.
- Items 11 and 12 list the same headings in the same order.
- The SAMPLE's heading line matches item 11 word for word.

END OF DESIGN
==============================================================================
