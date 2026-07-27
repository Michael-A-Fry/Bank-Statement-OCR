# Converting a statement

The everyday job: turn a statement into clean, checked, downloadable data.

## Do it

1. Open `http://<server-name>:8100` and go to **Convert**.
2. **Browse** and pick the file — `.csv`, `.tsv`, `.tdv`, `.xlsx` or `.pdf`.
   Up to **200 MB**. Pick several at once to do a whole case folder in one go;
   the button then reads *Convert N files*.
3. Leave **Bank** on **Detect automatically**. Only set it if the tool read the
   statement as the wrong bank.
4. Click **Convert**. A scanned PDF takes tens of seconds — it is being OCR'd.
5. Read the verdict, then **Download** Excel, CSV or JSON.

Everything past the verdict is behind one link, **Show me how it read this** —
the page picture, the analysis, the checks, the diagnostics, the field coverage
and the dashboard decision. It opens itself whenever a check has failed or the
run needs review, and once you click it either way it stays that way for the rest
of your session.

## The verdict

There are four outcomes, and the headline says which in plain English.

| Outcome | What it means | Do |
|---|---|---|
| **Converted successfully** | Matched a template, parsed, every applicable check passed. | Download it. |
| **Converted — please double-check it** | You have the data, but a check failed or could not run. | Open **Show me how it read this** → **Checks**. Compare the flagged rows against the statement. |
| **No template for this statement yet** | Nothing recognised this layout. | [Add a template](adding-a-bank-template.md). If the card instead offers you a choice between templates that fit equally well, pick one and it converts — and tell the maintainer, because two templates fitting one statement is a duplicate to retire. |
| **Could not read this file** | The file itself is the problem — empty, wrong type, password-protected, a scan with no OCR available. | The message says which. |

You should never see `needs_review`, `unsupported` or a check's internal name on
screen; the engine's codes stay in the logs. If a raw code does appear, that is a
bug — report it.

## The confidence level

| | Meaning |
|---|---|
| **high** | Opening balance + every transaction = the closing balance. Completeness is proven arithmetically. |
| **medium** | Clean, but something could not be *proved* — usually there is no balance to prove it against. |
| **low** | A check failed. Do not rely on the figures until you know why. |

**A PDF or an Excel file can never reach "high", however perfect it is.** High
needs a count of physical source lines to prove no row was lost, and only a
delimited file (CSV/TSV) has one (`R/reconcile.R`, `.kpi_no_unparsed_rows`). On
a PDF, "medium" is the ceiling and is the normal, healthy result — read it as
"clean, and here is the one thing nobody could prove", not as a warning.

## Reading the Checks table

Three columns: the check, the result, and the detail. **The detail column is the
one that carries the figures — read it, not just the result word.**

A result is one of three things:

- **passed** — the check ran and passed.
- **a problem** — the check ran and failed. The detail names the rows.
- **could not be checked** — the check **did not run**. The detail says why: no
  opening balance printed, no running-balance column, no statement period, no
  independent source-line count. This is not an error and not a pass; it is an
  absence of proof, and it is why the confidence level is capped at medium.

Two checks pass on less than their name suggests, and their detail is the only
place that says so:

- **Row count** — with no printed count on the statement (most statements) it
  degrades to "at least one row was parsed". The detail says *no stated count*
  when that happened.
- **Row dates could be read** — passes when *at least one* date parsed. The
  detail gives the real figure, e.g. "1 of 500 row date(s) read". A statement
  where nearly every date is missing can still show this check as passed.

## What you download

| | Contains |
|---|---|
| **Excel `.xlsx`** | Six sheets: `Transactions`, `Summary` (the statement header), `Checks` (every check + the confidence level), `Provenance` (which source line or page position each row came from), `Diagnostics`, `Metadata`. |
| **CSV** | The `Transactions` table only. |
| **JSON** | Everything: build stamp, header, transactions, extras, checks, trust, diagnostics, provenance, metadata. |

## The dashboard line

Under the result, one sentence says whether the conversion reached the Qlik
dashboards:

- *Sent to the dashboards* — it was clean, and read by a shipped, tested template.
- *Held back from the dashboards - it was read by a template built here* — it
  converted fine and your download is complete; only the dashboards are gated.
  To lift that, the template has to be promoted
  ([maintaining-the-engine.md](maintaining-the-engine.md) §3).
- *Held back* for a failed check — fix the template or the file and convert
  again. Converting the same file again with nothing changed reproduces the same
  result exactly; the engine is deterministic.

Marking a result **wrong** in the feedback box withdraws its rows from the
dashboards immediately — they move to the held-back feed, and the line above
changes to say so. Fixing the template and converting again puts the corrected
figures back.

## Batch runs

Select several files and Convert. You get one summary line and a table, one row
per file. **Click a row** to open that file's full result — the same verdict,
checks, transactions and downloads as converting it alone.

A file the tool cannot read at all is reported in the batch table as *no
template yet*, not as unreadable. Open the row to see the real reason.

## Good habits

- Prefer a bank's **CSV/Excel export** over its PDF when one exists. It is exact,
  and it is the only path that can prove completeness.
- On a PDF, glance at **Show me how it read this** → the page picture. Green rows
  were kept; amber dashed rows were skipped but look like transactions. A page
  with a lot of amber is a template with a band in the wrong place.
- Nobody sees anyone else's upload or result. The tool never un-redacts: it reads
  only what is visible and never derives a hidden value.

Something looks wrong → [when-something-goes-wrong.md](when-something-goes-wrong.md).
