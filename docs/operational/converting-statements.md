# Converting a statement

The everyday job: turn a statement into clean, checked, downloadable data.

## Do it

1. Open `http://<server-name>:8100` and go to **Convert**.
2. **Browse** and pick the file — `.csv`, `.tsv`, `.tdv`, `.xlsx` or `.pdf`.
   Up to **200 MB**. Pick several at once to do a whole case folder in one go;
   the button then reads *Convert N files*.
3. **Type your QID** and click **Use this QID**. Six letters or numbers — your
   staff ID, e.g. `AB1234`. It is what the audit trail records as who ran this
   conversion, so **Convert does nothing until it is filled in**. You are asked
   once per session; after that the page just says *Recording as AB1234*, with a
   **change** link beside it for when somebody else takes the keyboard.

   If there is no QID box at all, the server already knows who you are and the
   tool does not ask.
4. Leave **Bank** on **Detect automatically**. Only set it if the tool read the
   statement as the wrong bank.
5. Click **Convert**. A scanned PDF takes tens of seconds — it is being OCR'd.
6. Read the verdict, then **Download** Excel, CSV or JSON.

Everything the tool *proved* stays on the page under the verdict: whether the
figures reached the dashboards, the summary cards, the strip of ticks, your
transactions — and, in one disclosure headed **Checks & detail (for review)**,
the **Checks**, the **Diagnostics** and the **Field coverage**. That disclosure
opens itself whenever a check has failed or the run needs review, so on the runs
that need reading it is already open.

One link goes further in, **Show me how it read this**, and it holds three
things and no more: **See it on the page** (the statement with the columns drawn
on it — PDF only), the charts, and the template it used. It appears only on a run
that produced transactions, it opens itself on a flagged run too, and once you
click it either way it stays that way for the rest of your session.

## The verdict

There are four outcomes. The **Result** column of the batch table prints them in
exactly these words:

| Outcome | What it means | Do |
|---|---|---|
| **Converted successfully** | Matched a template, parsed, and every check that ran, passed. | Download it. |
| **Converted - please double-check it** | You have the data, and the tool wants a second pair of eyes on it. Usually a check **failed**; it can also mean the file looks like several statements bundled together, or that another template nearly fitted it too. | Read **Checks**, then **Diagnostics**, in **Checks & detail (for review)** under your transactions — on a result like this it is already open. Compare the flagged rows against the statement. |
| **No template for this statement yet** | Nothing recognised this layout. | [Add a template](adding-a-bank-template.md). If the card instead reads **More than one template fits - pick which one**, two templates fit equally well: pick one and it converts — and tell the maintainer, because that is a duplicate to retire. |
| **Could not read this file** | The file itself is the problem — empty, wrong type, password-protected, a scan with no OCR available. | The message says which. |

**Converting one file at a time, the clean result is worded differently**, and it
is the one you will see most. Instead of *Converted successfully* the card counts
the rows and states the confidence level:

> **Converted — 7 transactions read · confidence: medium**

Same outcome, same row of the table above; it just says more, because on a single
file there is room to. The other three read as printed above on both screens (the
double-check headline picks up the same ` · confidence:` suffix on the card).

**A check that *could not be checked* does not change the headline.** A clean PDF
routinely has two or three checks with nothing to run against, and it still comes
back as a clean conversion — because nothing about it failed. What carries the
missing proof is the **confidence level**, which is capped at *medium* for exactly
that reason, and the checks table, which names each absence and why. An absence of
proof is never dressed up as a problem, and never dressed up as a pass either.

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
delimited file (CSV/TSV) has one — the check *No row failed to read* is the one
that reports it. On a PDF, "medium" is the ceiling and is the normal, healthy
result — read it as "clean, and here is the one thing nobody could prove", not as
a warning. The card says so itself under a medium PDF verdict, so you do not have
to remember it.

## The strip of ticks under the figures

Between the summary cards and your transactions is a row of coloured pills — the
checks a forensic reviewer would ask about, on the page whether they passed or
not, so a clean run *shows* its proof instead of merely not complaining. There is
a key printed under them:

> ✓ = checked and passed · ✗ = a problem · – = could not be checked (why, in
> Checks below)

Five are always there when they apply: *Opening + transactions = closing
balance*, *No row failed to read*, *Money in / money out is the right way round*,
*Each running balance follows from the last*, *Row dates could be read*. **Any
other check that failed is added to the end**, so the strip can never be
all-clear while something failed. A grey dash is not a pass and not a fault — it
is the check that had nothing to run against, and the Checks table below says
which.

These three marks mean what the tool proved. **They are not the buttons you rate
the conversion with** — that control is words only (*Correct* · *Minor issues* ·
*Wrong*), deliberately, so one mark never stands for both what the tool found and
what you think.

## Reading the Checks table

Five columns: **Check**, **Result**, **Expected**, **Read**, **Detail**. Expected
and Read are the two numbers the result is a verdict on, and **the Detail column
is the one that carries the sentence — read it, not just the result word.**

The Result column says one of four things, and the fourth is the one people miss:

- **OK** — the check ran and passed.
- **Problem** — the check ran and failed. The detail names the rows.
- **could not be checked** — the check **did not run**. The detail says why: no
  opening balance printed, no running-balance column, no statement period, no
  independent source-line count. This is not an error and not a pass; it is an
  absence of proof, and it is why the confidence level is capped at medium.
- **for information** — the row is a **count**, not a verdict. Two checks are
  like this: *Redactions found* (how many rows carried a redaction) and *Scan /
  OCR read quality* (how many pages were machine-read, and the worst page's
  confidence). There is nothing for them to pass or fail, so they are never
  dressed as a pass — and never as a failure to run either, which is what they
  used to read as on a statement that really had been OCR'd.

There is no fifth thing, and in particular there is no silent one: every check
that exists for your statement is in this table with one of those four words
beside it.

**On a run that produced no transactions there is no table**, and that is not the
same as an empty one. *Could not read this file* and *No template for this
statement yet* both mean nothing was extracted, so there was nothing to check —
the Checks heading carries a sentence in place of the table saying exactly that,
and Field coverage says the same. A blank table would be indistinguishable from
one that failed to draw; a sentence is not.

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

## Rating the conversion

*Was this conversion correct?* sits under the result on any run that produced
figures. Three choices — **Correct**, **Minor issues**, **Wrong** — in words, not
marks, and **nothing is pre-selected**: it is the one question only you can
answer, so the tool will not answer it for you. Submit with none chosen and it
says so rather than recording anything.

Marking a result **Wrong** does more than log an opinion: it withdraws that run's
rows from the dashboards immediately — they move to the held-back feed, the
dashboard line above changes to say so, and the screen tells you how many rows
were withdrawn. Fixing the template and converting again puts the corrected
figures back.

## Batch runs

Select several files and Convert. You get one summary line and a table, one row
per file. **Click a row** to open that file's full result — the same verdict,
checks, transactions and downloads as converting it alone.

Six columns: **File**, **Result**, **Bank**, **Rows**, **Confidence**, **What to
check**. It opens worst-first — the files that need work are already at the top
and already grouped by what went wrong — and clicking **Result** re-sorts by
severity, not alphabetically. **Confidence** is the same word the single-file
card prints, so on a thirty-file case you can tell the *high* files from the
merely-uncomplaining *medium* ones without opening any of them.

## Good habits

- Prefer a bank's **CSV/Excel export** over its PDF when one exists. It is exact,
  and it is the only path that can prove completeness.
- On a PDF, glance at **Show me how it read this** → **See it on the page**.
  Green rows were kept; amber dashed rows were skipped but look like
  transactions. A page with a lot of amber is a template with a band in the wrong
  place.
- Nobody sees anyone else's upload or result. The tool never un-redacts: it reads
  only what is visible and never derives a hidden value.

Something looks wrong → [when-something-goes-wrong.md](when-something-goes-wrong.md).
