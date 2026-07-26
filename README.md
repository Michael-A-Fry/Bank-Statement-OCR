# Statement Studio — statement & document conversion engine

A pure-**R** engine that turns a bank or credit-card statement into clean,
structured, downloadable data (**Excel + CSV + JSON**) with reconciliation checks
and a trust score. Built for forensic-accounting use: high-fidelity extraction,
verbatim descriptions, honoured redactions, and no silent data loss.

**No Python, no machine learning.** Deterministic behaviour and declarative per-bank
templates — a data analyst adds a new bank by pointing and clicking in a wizard, not
by writing code.

---

## 📖 Documentation

The docs are split in two:

- **[Operational](docs/operational/README.md)** — how to *do* things: set it up,
  update it, run it, convert statements, add a bank, admin, and wire up Qlik. Written
  for the data team, step by step, no code.
- **[Context](docs/context/README.md)** — background: what's built, how we got here,
  the launch audit, the architecture, and the research.

**New here?** Start with the [operational guide](docs/operational/README.md). The
whole setup is two double-clicks — [first-time-setup](docs/operational/first-time-setup.md).

**Inheriting it?** Read the [charter](docs/context/charter.md) (one page: what this
tool must always do, and must never do), then
[how-it-fits-together](docs/context/how-it-fits-together.md) (one page: the journey
from upload to dashboard, which module owns each step, and where each kind of change
goes), then the maintainer's runbook —
[maintaining-the-engine](docs/operational/maintaining-the-engine.md) — for how to run
the test suite on the server, what an update overwrites, and how to promote a
template to proven. Back up the irreplaceable folders:
[backup-and-restore](docs/operational/backup-and-restore.md).

---

## What it does today

- **Reads CSV, TSV, Excel (`.xlsx`) and PDF** statements. Digital PDFs are read from
  their text layer; scanned/image PDFs are read with OCR (each OCR'd page is flagged).
- **Six banks end-to-end** on the delimited (CSV/TSV) path — ANZ everyday, ANZ credit
  card, ASB, BNZ, Kiwibank, Westpac — plus a cross-bank Xero-standard import, each with
  a passing golden-file test.
- **PDF and Excel are shipped and tested, not merely built.** `templates/` ships proven
  PDF templates — `anz_everyday_pdf`, `anz_investmentfunds_pdf`, `asb_everyday_pdf`,
  `tutorial_everyday_pdf`, `westpac_everyday_pdf` — and a generic Excel template
  (`excel_generic_xlsx`); the PDF and Excel extraction paths are covered by golden
  round-trip tests, and a key-value (form/IRD) mode is built too. Adding another bank on
  any path is a YAML template, not new code.
- **Point-and-click wizard** to teach it a new bank — including a visual PDF editor
  where you draw boxes over the columns. It writes the template for you; no YAML by
  hand.
- **Reconciliation + trust score** — ten checks: balance reconciliation, running-balance
  continuity, money-in/out direction, transaction count, dates-in-period, dates readable,
  completeness, redaction summary, OCR read quality and the redaction scan — each shown
  as a plain sentence, not a code, with a high/medium/low trust level.
- **Honours incoming redactions** — the tool never un-redacts; it reads only what is
  visible and never estimates hidden values.
- **Never silently wrong** — any non-clean run reports *where / why / how bad /
  who fixes it*; every run is logged; the engine never returns a silent wrong answer.
- **Governed analytics** — a clean conversion from a proven template also feeds the
  Qlik dashboards automatically, and every conversion is told on screen whether it was
  published or held back, and why. Marking a result *wrong* withdraws it again.
- **A full automated test suite** guards every guarantee — over 2,200 assertions across
  500+ tests when last measured, **0 failures and 0 skips**. A skipped test proves
  nothing, so the runner fails on one. `Rscript tests/run_tests.R` prints the current
  totals, and the exact last-measured baseline is kept in ONE place —
  [maintaining-the-engine](docs/operational/maintaining-the-engine.md) — so a
  noticeably lower total is a real signal rather than a stale number here.

---

## The app

Four tabs, all point-and-click:
- **About** — what the tool does and how to read the trust signals.
- **Convert** — upload a statement, click Convert, read the verdict and checks,
  download the Excel/CSV/JSON. ([converting-statements](docs/operational/converting-statements.md))
- **Add a template** — upload a sample, confirm what the tool detected against a live
  preview, Save. ([adding-a-bank-template](docs/operational/adding-a-bank-template.md))
- **Admin** (password) — insights, template management, review queue, folder intake.
  ([admin-and-maintenance](docs/operational/admin-and-maintenance.md))

Outputs per statement:
- **`.xlsx`** — `Transactions`, `Summary`, `Checks`, `Provenance`, `Diagnostics` and
  `Metadata` sheets.
- **`.csv`** — the same `Transactions` table the workbook holds.
- **`.json`** — the full object (build stamp, header, transactions, extras, checks,
  trust, diagnostics, provenance, metadata).

---

## Forensic guarantees

1. Descriptions preserved **verbatim** (special characters intact).
2. The tool **never redacts** — statements arrive redacted and it reads only what is
   visible. Hidden values are never derived or estimated.
3. **No silent drops** — completeness is proven by a check.
4. **Reproducible** — same input + template ⇒ identical output; no manual edits.
5. **Never crashes** — every error becomes a status with an actionable reason.
