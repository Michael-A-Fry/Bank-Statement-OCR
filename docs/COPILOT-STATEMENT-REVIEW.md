# Reviewing a statement safely (for Copilot) - no PII, structure only

Use this when a statement doesn't convert cleanly and you want a **safe-to-share
description of its layout** to pass back for a template or engine fix - **without
ever sending personal data**.

## The golden rule
> **Never include PII or personal details.** No names, account numbers, card
> numbers, addresses, merchant names, transaction descriptions, real amounts, or
> real dates. Describe only the **shape, layout, format and position** of things.
> A real value like `Countdown 47.20 on 17 Sep` must become a **shape**, e.g.
> `<merchant text> <amount 9.99> on <date "99 Xxx">`.

## Preferred path - let the engine do it (guaranteed safe)
Run the built-in audit; it masks every value to its shape automatically (letters
→ `x`/`X`, digits → `9`) so the output is safe by construction:

```sh
Rscript scripts/audit-statement.R  path/to/statement.pdf
```

It writes `statement.audit.md` - read it, confirm it looks safe, then share **that
file**. It already contains the format, page layout, detected template, date/
amount styles, redaction map, KPI statuses, masked row shapes, and a masked
page-1 word layout. This is the best thing to send back.

## Copilot path - when you can only use Copilot on the PDF
Paste the prompt below into Copilot along with the PDF. It asks Copilot to produce
the same kind of PII-free structural summary.

```text
You are helping describe a bank statement PDF so its LAYOUT can be supported by a
template-based parser. You MUST NOT reveal any personal or financial data.

HARD RULES:
- Do NOT output any real name, account/card number, address, merchant/payee,
  transaction description, real amount, or real date.
- Replace every real value with its SHAPE: letters -> x/X (by case), digits -> 9.
  Examples: "Countdown" -> "Xxxxxxxxx"; "1,234.56" -> "9,999.99"; "17 Sep" ->
  "99 Xxx"; "12-3456-7890123-00" -> "99-9999-9999999-99".
- If unsure whether something is sensitive, mask it.

Describe ONLY:
1. Document kind: bank transaction statement, or a form/summary (e.g. IRD/
   KiwiSaver - labelled values, not a transaction table)?
2. Pages, and roughly how many transaction rows.
3. The columns, left to right, with the FIELD each holds (date / description /
   withdrawal / deposit / amount / balance / type / reference) and their rough
   order. Note if there are TWO dates per row (transaction + processed/value).
4. Date format as a shape ("99 Xxx", "99/99/9999", "99 Xxxxxxxx 9999"), and
   whether a year is shown on each row or only in the statement period.
5. How amounts show direction: a minus sign? separate withdrawal/deposit columns?
   a DR/CR suffix? a D/C column? or unsigned (a plain number is a charge, only a
   payment marked, e.g. "9.99 CR")?
6. Any redactions/black boxes: where (which column, top/middle/bottom), and
   whether rows near them look complete otherwise.
7. Anything unusual vs a plain statement: multiple accounts/periods in one file,
   summary/closing-balance rows inside the amount column, running balance resets,
   sub-headers, carried-forward lines, foreign-currency lines, footated totals.
8. Whether it looks like a NEW format the parser might not handle, and what a
   template would need (which columns, which amount style, which date format).

Output as short bullet points. Shapes only. No real values anywhere.
```

## Sending it back
Share the resulting `*.audit.md` (or Copilot's bullet points). Because it is
shapes-only, it is safe to paste. From it, a template can be built or the engine
adjusted - column positions, date/amount handling, redaction behaviour - with no
personal data ever leaving your environment.
