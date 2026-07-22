# When something looks wrong

The tool never returns a silent wrong answer — a run that isn't clean always tells
you **where**, **why**, **how bad**, and **who fixes it**. This page helps you act on
that.

---

## Who fixes what
Every diagnostic carries a **"Who fixes this"** label. There are only five:

| Label | Meaning | What you do |
|---|---|---|
| **You — adjust the template** | A column / date / amount setting is off for this bank. | Open **Add a template**, tweak the box / date-format / amount-style, preview until it reconciles, Save. **~99% of issues.** |
| **You — fix the file** | The *input* is the problem, not the template. | Split a merged bundle into one statement per file; re-export as CSV/Excel; or rescan a blurry page at higher quality. |
| **You — review the data** | Not an error — something to eyeball (e.g. a combined multi-account statement, mixed currencies). | Sanity-check the rows; the numbers are extracted, they just need a human glance. |
| **No action** | Informational (redactions kept as `[REDACTED]`, OCR was used, etc.). | Nothing. |
| **Engine gap — escalate** | A genuinely new situation the engine doesn't handle yet. **Rare.** | Report it (see below) to whoever maintains the engine. |

**Rule of thumb:** *template / input / review → you can handle it, no code.*
*escalate → not your job* — it means the engine needs a generic improvement, which
then helps every bank. If you ever see mostly `escalate` labels on everyday
statements, that's the signal to get engine help.

---

## What the statuses mean
| You see | It means | Do |
|---|---|---|
| **UNSUPPORTED** | No template matched this layout. | Teach it once in the wizard — [adding-a-bank-template.md](adding-a-bank-template.md). The message shows the closest match and which columns were missing. |
| **NEEDS_REVIEW** | Parsed, but a check failed. | Open **Checks**; compare against the source. If the template is wrong, fix it and re-convert. |
| **FAILED** | The file couldn't be read. | The message says why (empty, wrong type, password-protected). |
| A check says **`na`** | That data isn't in the file. | Expected for balance-less CSV exports — not an error. |
| Trust **low** | A reconciliation check failed. | Don't rely on the output until it's resolved. |

---

## Reporting a problem safely (no personal data)
If you need help with a statement, **never send the statement or any personal
data** — no names, account/card numbers, addresses, merchants, descriptions, real
amounts, or real dates. Describe only the **shape and layout**.

Turn every real value into its shape: letters → `x`/`X` (by case), digits → `9`.
For example `Countdown 47.20 on 17 Sep` becomes `Xxxxxxxxx 99.99 on 99 Xxx`.

What's genuinely useful to whoever maintains the engine:
- What kind of document it is (transaction statement, or a form/summary).
- The columns left to right and what each holds (date / description / withdrawal /
  deposit / amount / balance / type / reference), and whether there are two dates per
  row.
- The date format as a shape (`99/99/9999`, `99 Xxx`), and whether the year is on
  each row or only in the statement period.
- How direction is shown (a minus sign? separate in/out columns? a `DR`/`CR` suffix?
  a `D`/`C` column?).
- Any redactions / black boxes: which column, roughly where.
- Anything unusual vs a plain statement (multiple accounts in one file, summary rows
  inside the amount column, running-balance resets, foreign-currency lines).

> The tool can generate this shapes-only summary for you automatically (it masks
> every value to its shape). If you need it, ask whoever maintains the engine to run
> the built-in audit on the file — it produces a safe `*.audit.md` describing the
> layout with no real values in it, which is the ideal thing to share.
