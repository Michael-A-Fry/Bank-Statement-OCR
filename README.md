# Statement Studio — statement & document conversion engine

A pure-**R** engine that turns a bank, card or financial statement into clean,
structured, downloadable data (**Excel + CSV + JSON**) with reconciliation checks
and a confidence level. Built for forensic accounting: high-fidelity extraction,
verbatim descriptions, honoured redactions, no silent data loss.

**No Python, no machine learning.** Deterministic behaviour and declarative
per-bank templates — an analyst adds a new bank by pointing and clicking, not by
writing code.

## Documentation

Three pages read straight through, for whoever needs the whole thing at once:

- **[overview.md](docs/overview.md)** — what it is and what it is for: the
  problem, what it guarantees, what it deliberately does not, who does what, what
  it costs to run. No code in it.
- **[design.md](docs/design.md)** — the technical map, for a developer inheriting
  it cold: the shape, the path a statement takes, the template schema, the
  invariants, how to ship a change and how to put it back.
- **[story.md](docs/story.md)** — how it got this shape: every turn that made it
  something different, and why.

Then one page per job:

- **[For analysts](docs/for-analysts/README.md)** — the four pages a forensic
  accountant needs and nothing else: convert a statement, teach it a new bank,
  work out what to do when something looks wrong, describe a layout safely. This
  is the folder to hand to somebody new.
- **[Operational](docs/operational/README.md)** — how to *do* things: the four
  above, plus set it up, run it, back it up, update it, admin, and wire up Qlik.
- **[Context](docs/context/README.md)** — how it works and why: the charter, the
  data contract, the engine parameters, the edge-case register, the findings
  register, the roadmap.

**New here?** [first-time-setup.md](docs/operational/first-time-setup.md) — two
double-clicks, then set the admin password and open the firewall port.

**Turning it on for a unit today?**
[go-live-checklist.md](docs/operational/go-live-checklist.md) — the numbered
list for the day, every step with something you can see that says it worked, and
what "it is working" looks like an hour later. If the day goes wrong:
[rolling-back.md](docs/operational/rolling-back.md), which answers the question
the update page does not — what happens to the conversions the bad version
already sent to Qlik.

**Inheriting it?** [charter.md](docs/context/charter.md) (one page: what this
tool must always do and must never do), then
[design.md](docs/design.md) (the map, the reasoning and the traps, including
how to run it, ship a change to it and roll it back), then
[maintaining-the-engine.md](docs/operational/maintaining-the-engine.md) (running
the suite on the server, what an update overwrites, promoting a template). Then
back up the irreplaceable folders:
[backup-and-restore.md](docs/operational/backup-and-restore.md).

**Someone says a conversion is wrong?**
[investigating-a-wrong-conversion.md](docs/operational/investigating-a-wrong-conversion.md)
— from a run id or a feedback record to the original file, the same figure
reproduced from the command line, and template bug vs bad scan.

## What it does today

- **Reads CSV, TSV, Excel (`.xlsx`) and PDF.** Digital PDFs are read from their
  text layer; scanned pages are OCR'd and flagged as such.
- **13 shipped, golden-tested templates** in `templates/` — 7 delimited (ANZ
  everyday, ANZ credit card, ASB, BNZ, Kiwibank, Westpac, plus a cross-bank
  Xero-standard import), 5 PDF (`anz_everyday_pdf`, `anz_investmentfunds_pdf`,
  `asb_everyday_pdf`, `tutorial_everyday_pdf`, `westpac_everyday_pdf`) and one
  generic Excel template. Adding another bank on any path is a YAML template, not
  new code. A key-value mode for forms and IRD-style documents ships too.
- **A point-and-click toolkit** to teach it a new layout. It reads the file,
  proposes the whole template — columns, bands, date format, amount style, the
  bank's own name off the masthead — and shows a live preview of the real
  statement beside it. Reading that preview is the job; the visual PDF editor,
  where you drag a box over a column, is there for when it is wrong. It writes
  the template; no YAML by hand.
- **Ten reconciliation checks** — balance reconciliation, running-balance
  continuity, money-in/out direction, row count, dates in period, dates readable,
  rows that failed to read, redaction count, OCR read quality and the redaction
  scan — each shown as a plain sentence with a high/medium/low confidence level.
- **Honours incoming redactions.** The tool never un-redacts, never estimates a
  hidden value, and never back-calculates one from a balance.
- **Never silently wrong.** Any non-clean run reports *where / why / how bad /
  how to fix it*, with the fix-ownership triage carried in the downloaded
  workbook. A check that could not run says so; it never shows a green tick, and
  a check that is a count rather than a verdict is labelled as one.
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
  dictionaries, batch audit. Until `app.admin_password` is changed from the
  shipped placeholder it **refuses to open for anybody**, since that placeholder
  is printed in the example config and in these docs.
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
4. **Reproducible** — same input + same template + **same build** ⇒ byte-identical
   output. The engine version and a hash of the template are stamped *inside* each
   output, which is what makes the claim checkable — and is also why outputs from
   two different builds cannot match byte for byte even when every figure does.
   Comparing an old conversion against a re-run: read `engine_version` first, then
   the figures.
5. **Never crashes** — every error becomes a status with an actionable reason.
