# Converting a statement

The everyday job: turn a bank/credit-card statement into clean, downloadable data.

---

## Steps
1. Open the app (`http://<server-name>:8100`) and go to the **Convert** tab.
2. **Browse** and pick the statement file (CSV, TSV, Excel `.xlsx`, or PDF). Or
   click **try the sample** to see the whole flow first.
3. Leave **Bank** on **auto-detect** (only pick a bank yourself if the auto-match is
   wrong).
4. Click **Convert**.
5. Read the result:
   - **Verdict** — a plain-English summary of how it went.
   - **Trust** — high / medium / low (see below).
   - **Analysis** — money in/out and balance over time.
   - **Transactions** — a preview of the exact table you'll download.
   - **Checks** — the reconciliation results (balance, running balance, count,
     completeness…). A failed check is always flagged, never quietly fixed.
6. **Download** the **Excel**, **CSV**, or **JSON**.

---

## Reading the trust level
| Trust | Meaning | What to do |
|---|---|---|
| **high** | Balance-proven: opening balance + every transaction = the printed closing balance. | Ship it. |
| **medium** | Clean, but the file had no running balance to prove against (common for CSV exports). | Fine — glance at the preview. |
| **low** / a failed check | A reconciliation check didn't pass. | Open the source and compare before relying on it; see [when-something-goes-wrong.md](when-something-goes-wrong.md). |

---

## What each download contains
- **Excel (`.xlsx`)** — `Transactions` (the clean table), `Summary` (statement
  header), `Checks` (the KPIs + trust), `Provenance` (which source row each line came
  from).
- **CSV** — just the `Transactions` table, for loading into any tool.
- **JSON** — everything (header, transactions, checks, trust, provenance).

---

## Good habits
- Prefer a bank's **CSV/Excel export** over a PDF when it offers one — it's exact.
- On PDFs, glance at the **trust** score and the **coverage** note before relying on
  the numbers.
- If the app says **UNSUPPORTED**, the file is a layout it hasn't seen — teach it
  once in the wizard: [adding-a-bank-template.md](adding-a-bank-template.md).

Your privacy is preserved throughout: nobody sees anyone else's upload or result,
and the tool never un-redacts a statement — it only reads what is visible.
