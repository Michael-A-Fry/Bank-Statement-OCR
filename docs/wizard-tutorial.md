# Building a template from scratch - the complete walkthrough

**Who this is for:** the analyst who looks after this tool. No coding. If you can
draw a box with your mouse and pick an item from a dropdown, you can add any
bank. This guide walks the exact steps, and - more importantly - teaches you
**every way one statement differs from another** so nothing is ever a surprise.

There is a **worked example** you can follow along with:
`samples/raw/tutorial/sample_everyday_statement.pdf` (a synthetic Kowhai Bank NZ
statement, built to contain the tricky bits on purpose). Open it side-by-side.

---

## 1. What a "template" actually is

The engine has **zero** knowledge of any bank baked in. A template is a short
description that says, for one bank's statement layout:

> "The date is in *this* column, the amount is in *that* column, amounts are
> shown *this* way, and here's a phrase that proves it's this bank."

That's it. Add a bank = add one template. Nothing in the engine changes. This is
why one analyst can look after the whole thing.

Both kinds of template are built the same way: **Add a template** tab → upload
the file → **Open the toolkit**. The toolkit pre-fills everything it can detect;
what you do next depends on the file:
- **Delimited / Excel** (`.csv`, `.tsv`, `.xlsx`) - each field maps to a
  **column name**; you check the pickers against the sample rows shown on the
  left.
- **PDF** - each column is a **box on the page**; you check (or redraw) the
  proposed boxes.

---

## 2. First, read the statement's "shape" (the 9 ways they differ)

Before touching the wizard, look at your statement and answer these. Every
question maps to one setting. This checklist is the whole game - once you can
answer it, the wizard is 2 minutes.

| # | Question | Why it matters / what it sets |
|---|----------|-------------------------------|
| 1 | **What file is it?** CSV/TSV, Excel, PDF with selectable text, or a scanned/photographed PDF? | Picks the wizard. Scanned PDFs are read by OCR automatically - no extra step, but check the numbers. |
| 2 | **How are amounts shown?** | The single most important setting - see §3. One signed column? Two columns (Withdrawals/Deposits)? A `DR`/`CR` suffix? A separate Type column? |
| 3 | **How are dates written?** | See §4. Full date, or day+month only (year comes from the period)? Day-first or month-first? 2-digit year? |
| 4 | **Which columns exist?** | Some statements have no running balance. NZ statements often add Particulars / Code / Reference. Map what's there, leave the rest blank. |
| 5 | **Is there a preamble?** | Lines *before* the table (logo text, address, "Statement from…"). Delimited files sometimes have header junk above the real header row. |
| 6 | **Multi-line rows?** | A transaction whose detail spills onto a second line (e.g. `4835******1234 Orig date 06/05/2026`). Handled automatically - the second line has no date, so it's ignored. Don't map it. |
| 7 | **Redactions (black boxes)?** | Never a problem: the engine never reads text under a redaction overlay and never guesses the value. It marks the field `[REDACTED]`. Nothing for you to do. |
| 8 | **One account or several?** | A "combined" statement lists several accounts/products in one period. It parses, but running balances won't be continuous across accounts - the tool flags this so you review per account. |
| 9 | **How many statements in the file?** | If someone merged *several* statements (different periods) into one PDF, the tool flags it up front and asks you to split into one statement per file. Don't try to template a bundle. |

---

## 3. The ways AMOUNTS differ (pick one - this is the big one)

Statements encode "money in vs money out" in five common ways. Pick the matching
`amount style` in the wizard:

1. **One signed column** - `signed`.
   `-45.00` is money out, `45.00` is money in. Most CSV exports.
2. **Two columns: Withdrawals and Deposits** - `debit_credit_cols`.
   Each row fills one or the other. Map **both** the debit (withdrawals) and
   credit (deposits) columns. *This is the worked example.*
3. **A `DR` / `CR` suffix** - `dr_cr_suffix`.
   `123.45 DR` is out, `123.45 CR` is in. Every amount carries a suffix.
4. **A Type column holding D/C** - `type_dc`.
   A separate column says `D` or `C` (or `DR`/`CR`) and the amount is unsigned.
   Map the type column too.
5. **Unsigned amounts (credit cards)** - `unsigned`.
   A plain number like `45.00` is a **charge**, and only a **payment** is marked,
   e.g. `500.00 CR`. Charges come out negative (money out), the `CR` payment
   positive. If your card's *closing balance* is the amount owed (it rises with
   charges) and you want it to reconcile, set `unsigned_default: credit` so
   charges are positive instead - the wizard preview shows which way ties out.

> If the balance goes the wrong way (deposits look like withdrawals), you picked
> the wrong style. Change it and Preview again - no other change needed.

---

## 4. The ways DATES differ

Set the **date format** to match what you see. Common ones:

| On the statement | Setting | Note |
|------------------|---------|------|
| `21/04/2026` | day/month/year | NZ/UK default (day first) |
| `04/21/2026` | month/day/year | US |
| `2026-04-21` or `2026/04/21` | year/month/day | ISO |
| `1 April 2025` | day month-name year | full month name |
| **`21 Apr`** (no year!) | **day month-name, no year** | **The year is taken from the statement period automatically.** *This is the worked example.* |
| `21/04/26` | day/month/2-digit-year | 2-digit years are handled |

The **year-less** case is the sneaky one and you don't have to do anything
special: if the date has no year, the engine reads the statement's own period
("from 1 May 2026 to 31 May 2026") and attaches the right year - including when a
statement straddles New Year (Dec → Jan), where each date gets the year that
keeps it inside the period.

---

## 5. Step-by-step: a DELIMITED (CSV / Excel) template

1. **Add a template** tab → **upload** your sample `.csv`/`.tsv` → **Open the
   toolkit**. (**Excel?** Most `.xlsx` exports convert as-is via the generic
   Excel template; a custom Excel layout can't be drafted in the toolkit yet -
   save the sheet as CSV and set that up instead.)
2. The toolkit **auto-detects** the delimiter, date format, amount style and
   column mapping, and shows the first rows of your file on the left the whole
   time.
3. **Check the field pickers** point at the right columns (description,
   reference, balance). Set a picker to `(none)` if the statement doesn't have
   that column. The full field-by-field mapping (particulars, code, type,
   other party, …) is editable on the **Advanced** tab.
4. Fix the **date format** and **amount style** if the auto-guess is wrong (§3-4).
5. The **fingerprint** (which header names must all be present, §7) is drafted
   automatically from your sample; fine-tune it on the **Advanced** tab.
6. Watch the **preview** at the bottom - the real parsed rows. Eyeball them.
7. **Save template** - it writes `templates_user/<id>.yaml` for you.

---

## 6. Step-by-step: a PDF template (draw the boxes)

Follow along with the worked example PDF.

1. **Add a template** tab → **upload** the PDF → **Open the toolkit**. Column
   boxes are proposed from the page automatically; pick the **page** with the
   transaction table (often page 2; page 1 is usually a summary).
2. Check each proposed column, and fix any that are off:
   a. Choose the field in **"Column the box is…"** (start with **date**).
   b. **Drag a box** left-to-right across that column on the page image.
   c. Click **"Assign box → column"**. A proposed column that isn't really
      there? Pick it and click **"Remove this column"**.
   d. Repeat for **description**, then the amount column(s), then **balance**.
      - If amounts are **two columns**, set the style to Withdrawals/Deposits and
        draw a box for **debit** (withdrawals) *and* one for **credit**
        (deposits).
3. Set **date format** (§4) and **amount style** (§3) on the **Simple** tab.
4. The **fingerprint phrase(s)** - text that proves it's this bank (§7) - are
   drafted from the page automatically; fine-tune them on the **Advanced** tab.
5. Watch the **preview** - the engine keeps only rows whose **date box
   reads as a real date**, so headings, "balance brought forward", notes and the
   multi-line detail lines are dropped automatically. You don't box them out.
6. **Save template**.

**Tips that come straight from real statements:**
- Make each box **span the whole column width**, a little into the whitespace on
  both sides - not tight to the digits.
- Amounts are usually **right-aligned**; put the right edge of your box past the
  longest number.
- If a **long description** gets cut off, widen the description box (but stop
  before it touches the amount column).
- Boxes are **x-position only** (full page height) - you're defining columns, not
  rows. Rows are found automatically by the date.

---

## 7. The fingerprint - how detection knows it's this bank

You never type the bank name at conversion time; the engine detects it. The
**fingerprint** is the proof:
- **Delimited:** the set of header column names that must all be present.
- **PDF:** a phrase (or few) that must appear on the page.

Pick something **distinctive**. `Date` alone is on every statement; `Transaction
details` + `Withdrawals` + `Deposits` together is specific. If two templates
could match, the engine refuses to guess and tells you - so err towards specific.

---

## 8. Trust it when it reconciles

After Preview, look at the **Checks**:
- `running_balance_continuity` **pass** with 0 discontinuities means each row's
  balance = previous balance ± amount - i.e. **you mapped the columns right**.
- If `balance_reconciliation` passes too, opening + all transactions = closing.
  That's the gold standard: the numbers are provably correct.

The worked example reconciles: opening `1,250.00` + 12 transactions = `2,716.50`,
the closing balance the statement itself prints.

If a check **fails**, the **Diagnostics** panel tells you *where, why, and how to
fix it* - usually "wrong amount style" or "wrong date format". Fix and re-Preview.

---

## 9. Make it permanent (a golden test)

Once a template is right, lock it in so a future change can't silently break it:
1. Save a small real (or synthetic) sample under `samples/raw/<bank>/`.
2. Generate the golden CSV from the engine's own parse and eyeball it.
3. Save it to `tests/testthat/expected/<id>.csv`.
4. Add `tests/testthat/test-<id>.R` (copy an existing one).
5. Run `Rscript tests/run_tests.R` - your bank must pass and no other may break.

See `tests/HOWTO-add-template-test.md`. The tutorial sample itself
(`test-tutorial_everyday_pdf.R`) is a worked example of exactly this.

---

## 10. Troubleshooting (symptom → cause → fix)

| Symptom | Likely cause | Fix |
|---------|--------------|-----|
| "No template matched" | Fingerprint too strict, or wrong page | Loosen/repick the fingerprint phrase; check you're on the table page |
| Deposits show as withdrawals (or vice versa) | Wrong amount style | Switch signed ↔ debit/credit ↔ DR/CR (§3) |
| Dates all blank / wrong | Wrong date format | Match day-first vs month-first; for year-less dates confirm the period is detected (§4) |
| A column is empty | Box misplaced or wrong column mapped | Redraw the box over the right column; widen it |
| Description cut off | Box too narrow | Widen the description box (stop before the amount column) |
| Rows missing | Their date box didn't read as a date | Widen/move the date box; check the date format |
| Flagged "multiple statements" | Several statement periods in one file | Split into one statement per file and re-run |
| Flagged "combined statement" | One statement, several accounts | Expected; review per account - balances aren't continuous across accounts |
| `[REDACTED]` in a cell | A black box covers the value | Correct and intentional - the engine never reads under a redaction |

---

That's the whole job. Nine questions, two wizards, one Preview. When it
reconciles, it's right.
