# Statement Studio — statement & document conversion engine

A pure-**R** engine that turns a bank, card or financial statement into clean,
structured, downloadable data (**Excel + CSV + JSON**) with reconciliation checks
and a confidence level. Built for forensic accounting: high-fidelity extraction,
verbatim descriptions, honoured redactions, no silent data loss.

**No Python, no machine learning.** Deterministic behaviour and declarative
per-bank templates — an analyst adds a new bank by pointing and clicking, not by
writing code.

## Documentation

- **[Operational](docs/operational/README.md)** — how to *do* things: set it up,
  convert statements, add a bank, fix what looks wrong, run it, back it up,
  update it, admin, and wire up Qlik. One page per task.
- **[Context](docs/context/README.md)** — how it works and why: the charter, the
  data contract, the engine parameters, the edge-case register, the findings
  register, the roadmap.

**New here?** [first-time-setup.md](docs/operational/first-time-setup.md) — two
double-clicks, then set the admin password and open the firewall port.

**Inheriting it?** [charter.md](docs/context/charter.md) (one page: what this
tool must always do and must never do), then
[how-it-fits-together.md](docs/context/how-it-fits-together.md) (one page: upload
to dashboard, which module owns each step), then
[maintaining-the-engine.md](docs/operational/maintaining-the-engine.md) (running
the suite on the server, what an update overwrites, promoting a template). Then
back up the irreplaceable folders:
[backup-and-restore.md](docs/operational/backup-and-restore.md).

## What it does today

- **Reads CSV, TSV, Excel (`.xlsx`) and PDF.** Digital PDFs are read from their
  text layer; scanned pages are OCR'd and flagged as such.
- **13 shipped, golden-tested templates** in `templates/` — 7 delimited (ANZ
  everyday, ANZ credit card, ASB, BNZ, Kiwibank, Westpac, plus a cross-bank
  Xero-standard import), 5 PDF (`anz_everyday_pdf`, `anz_investmentfunds_pdf`,
  `asb_everyday_pdf`, `tutorial_everyday_pdf`, `westpac_everyday_pdf`) and one
  generic Excel template. Adding another bank on any path is a YAML template, not
  new code. A key-value mode for forms and IRD-style documents ships too.
- **A point-and-click toolkit** to teach it a new layout, including a visual PDF
  editor where you drag boxes over the columns. It writes the template; no YAML
  by hand.
- **Ten reconciliation checks** — balance reconciliation, running-balance
  continuity, money-in/out direction, row count, dates in period, dates readable,
  rows that failed to read, redaction count, OCR read quality and the redaction
  scan — each shown as a plain sentence with a high/medium/low confidence level.
- **Honours incoming redactions.** The tool never un-redacts, never estimates a
  hidden value, and never back-calculates one from a balance.
- **Never silently wrong.** Any non-clean run reports *where / why / how bad /
  who fixes it*. A check that could not run says so; it never shows a green tick.
- **Governed analytics.** A clean conversion from a shipped template feeds the
  Qlik dashboards automatically, and every conversion is told on screen whether
  it was published or held back, and why. Marking a result *wrong* withdraws it.
- **A full automated test suite** guards every guarantee, and the runner fails on
  a skipped test because a skip proves nothing. `Rscript tests/run_tests.R`
  prints the current totals; the last-measured baseline is kept in one place —
  [maintaining-the-engine.md](docs/operational/maintaining-the-engine.md) — so a
  noticeably lower total is a real signal rather than a stale number here.

## The app

Four tabs, all point-and-click:

- **About** — what the tool does and how to read the confidence signals.
- **Convert** — upload one statement or a whole case folder, click Convert, read
  the verdict, download.
  ([converting-statements.md](docs/operational/converting-statements.md))
- **Add a template** — upload a sample, confirm what the tool detected against a
  live preview, Save.
  ([adding-a-bank-template.md](docs/operational/adding-a-bank-template.md))
- **Admin** (reached with `?admin`, password-protected) — insights, templates,
  dictionaries, batch audit.
  ([admin-and-maintenance.md](docs/operational/admin-and-maintenance.md))

Outputs per statement: a six-sheet `.xlsx` (`Transactions`, `Summary`, `Checks`,
`Provenance`, `Diagnostics`, `Metadata`), a `.csv` of the transactions table, and
a `.json` holding everything including the build stamp and provenance.

## Forensic guarantees

1. Descriptions preserved **verbatim**, special characters intact.
2. The tool **never redacts** and never reveals — statements arrive redacted and
   it reads only what is visible.
3. **No silent drops** — completeness is proven by a check, or the check says
   plainly that it could not be proven.
4. **Reproducible** — same input + same template ⇒ identical output, stamped with
   the engine version and a hash of the template.
5. **Never crashes** — every error becomes a status with an actionable reason.
