# Who fixes what — a lone analyst's decision guide

Every diagnostic the tool shows now carries a **"Who fixes this"** label, so you
never have to wonder whether to draw a box or call a developer. There are only
five answers:

| Label | Meaning | What you do |
|-------|---------|-------------|
| **You — adjust the template (wizard)** | A column/date/amount setting is off for this bank. | Open **Add a template** → the wizard, tweak the box/date-format/amount-style, Preview until it reconciles, Save. **~99% of issues.** |
| **You — fix the file (split / re-export / rescan)** | The *input* is the problem, not the template. | Split a merged bundle into one statement per file; re-export as CSV/Excel; or rescan a blurry page at higher DPI. |
| **You — review the data** | Not an error — a situation to eyeball (e.g. a combined multi-account statement, mixed currencies). | Sanity-check the rows; the numbers are extracted, they just need a human glance. |
| **No action** | Informational (redactions kept as `[REDACTED]`, OCR was used, etc.). | Nothing. |
| **Developer — engine gap (escalate)** | A genuinely new situation the engine doesn't handle yet. **Rare.** | Send the statement (and this diagnostic) to whoever maintains the engine. |

## The rule of thumb
- **"template" or "input" or "review" → you can handle it.** No code, ever.
- **"escalate" → not your job.** It means the *engine* needs a generic
  improvement — infrequent, and once done it helps every bank. You are not
  expected to touch code.

## Why this de-risks maintenance
Adding a bank is a **template** (drawing boxes in the wizard) — that's the daily,
no-code work. The handful of engine changes made during setup (handling 2-digit
years, summary lines, scanned PDFs, …) were **one-time generic maturation**, not
per-bank code — they now apply to every statement forever. So the split is:

- **You (analyst), often:** templates + reviewing the "Who fixes this" column.
- **A developer, rarely:** an `escalate` diagnostic on a genuinely novel format.

If you ever see mostly `escalate` labels on everyday statements, *that's* the
signal to get engine help — otherwise, it's boxes and dropdowns.
