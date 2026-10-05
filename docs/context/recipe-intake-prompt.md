# Copilot prompt: describe a statement design so a recipe can be written

Paste everything between the lines into Copilot together with ONE statement
(PDF, Excel or CSV). Copy Copilot's answer (the STATEMENT DESIGN block) to
Claude and ask: "Write a recipe from this." Repeat with a second statement of
the same design if Copilot was unsure of anything, so Claude can see what varies.

---------------------------------------------------------------------------
You are describing the LAYOUT of the attached bank statement so a developer
can write a reader for this statement design. The developer must NOT see any
personal or account information. Follow the privacy rules exactly.

PRIVACY RULES (strict)
- Never output a person's or business's name, an address, phone, email, IRD
  or GST number, account number, card number, customer/member/loan number,
  or a real merchant, payee, reference, particulars or code value.
- Never output a real amount or balance. Where an example is needed, invent a
  figure with EXACTLY the same printed shape (same separators, decimals,
  currency sign, minus sign, brackets, CR/DR/OD suffix, spacing).
  e.g. "1,234.56 DR" -> "8,765.43 DR"; "-$12.40" -> "-$56.78".
- Replace names/payees with MERCHANT A, MERCHANT B, PERSON A; references with
  REF 1001; account numbers with the same shape of zeros/X
  (e.g. 00-0000-0000000-00, XXXX-XXXX-XXXX-0000).
- Dates: keep the FORMAT, change the values (e.g. "03 Feb" -> "17 Feb").
- KEEP, word for word: the bank's own fixed wording -- titles, headings,
  labels, column headings, footer phrases, summary labels, section titles.
  These are the same on everyone's statement and are what the reader looks for.
- If unsure whether something is personal, replace it.

OUTPUT exactly this block, filling every line. Write "none" or "unsure" rather
than guessing.

STATEMENT DESIGN
1. Bank and product: (e.g. ANZ, Visa credit card / everyday account / home loan)
2. File: PDF with selectable text | scanned PDF | Excel (.xls/.xlsx) | CSV; pages:
3. Recognise it by -- fixed words printed on EVERY statement of this design
   (title, box headings, footer phrases), exactly as printed:
4. Several statements in one file? How does each one start (exact words)?
   Does page numbering restart ("Page 1 of N")?
5. Statement period: exact label words and date format, as a pattern
   (e.g. "Statement period DD Mon YYYY to DD Mon YYYY"). Any "START - date"?
   Is there also a statement date / issue date? Its label and format:
6. Opening balance: exact label; where (summary box / first table row / both);
   number format pattern:
7. Closing balance: exact label; where; format. (e.g. "Closing balance",
   "New balance", "Current balance", "Balance carried forward")
8. Printed totals: exact labels and where (e.g. "Totals at end of page",
   "Total debits", a summary strip). If labels sit on one line with figures
   on the line below, say so:
9. Transaction table header: the heading words exactly, LEFT TO RIGHT,
   noting any heading that spans several columns. Repeated on every page?
   Any section title printed just above it (e.g. "General Payments & Charges")?
10. Columns, left to right, one line each:
    heading | what it holds | format pattern | left/right aligned | always filled?
    (e.g. "Date | transaction date | DD Mon (no year) | left | yes")
11. Money in vs out: separate columns? one amount column with a sign / CR /
    DR / brackets? a type column with words or codes (list them, e.g. D/C,
    DR/CR, Paid/Recd; transaction codes like AP BP DC DD EP with meanings)?
12. Running balance: printed on every row / some rows / never. Overdrawn shown
    how (OD, DR, minus, brackets)?
13. Row order: oldest first or newest first?
14. Year on each row's date? If not, where does the year come from?
15. Rows that are NOT transactions, inside or around the table, exact wording:
    (e.g. "Balance brought forward", "Totals at end of page", interest
    breakdown lines, "No transactions for this period", rate change lines,
    cardholder/card headings, "Total for card ending ...", foreign currency
    detail lines, "- processed on: DD Mon YY")
16. Rows that take more than one line? What do the extra lines hold?
    Two dates per row (transaction + processed)? Which is which?
17. Several cardholders / accounts / sections on one statement? How each
    section starts and ends (exact words, names replaced):
18. Where the table ends on each page and at the end (exact words):
19. Excel/CSV only: sheet names; preamble rows above the header (labels
    only); exact column headers; how a negative/overdrawn value is shown
    (minus, "OD" in the cell text or only in the cell format); any blank or
    "." cells; pending/unstatemented rows after the table?
20. Anything unusual a reader could trip on:

SAMPLE (faked values, real layout): reproduce, as plain text keeping the
spacing roughly as printed, the table header row, then 6-10 rows chosen to
show every kind of row (money out, money in, a wrapped row, a non-transaction
line, a page-total or carried-forward line, an overdrawn balance if any).
Apply the privacy rules to every value.
---------------------------------------------------------------------------
