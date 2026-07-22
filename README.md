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

---

## What it does today

- **Reads CSV, TSV, Excel (`.xlsx`) and PDF** statements. Digital PDFs are read from
  their text layer; scanned/image PDFs are read with OCR (each OCR'd page is flagged).
- **Six banks end-to-end** on the delimited path — ANZ everyday, ANZ credit card,
  ASB, BNZ, Kiwibank, Westpac — plus a cross-bank Xero-standard import, each with a
  passing golden-file test. PDF and key-value (form/IRD) paths are built and grow by
  adding templates.
- **Point-and-click wizard** to teach it a new bank — including a visual PDF editor
  where you draw boxes over the columns. It writes the template for you; no YAML by
  hand.
- **Reconciliation + trust score** — balance reconciliation, running-balance
  continuity, transaction count, dates-in-period, completeness and a redaction
  summary, surfaced as plain checks with a high/medium/low trust level.
- **Honours incoming redactions** — the tool never un-redacts; it reads only what is
  visible and never estimates hidden values.
- **Never silently wrong** — any non-clean run reports *where / why / how bad /
  who fixes it*; every run is logged; the engine never returns a silent wrong answer.
- **A full automated test suite** guards every guarantee (272 tests, 0 failures).

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
- **`.xlsx`** — `Transactions`, `Summary`, `Checks`, `Provenance` sheets.
- **`.csv`** — the core `Transactions` table.
- **`.json`** — the full object (header, transactions, checks, trust, provenance).

---

## Forensic guarantees

1. Descriptions preserved **verbatim** (special characters intact).
2. The tool **never redacts** — statements arrive redacted and it reads only what is
   visible. Hidden values are never derived or estimated.
3. **No silent drops** — completeness is proven by a check.
4. **Reproducible** — same input + template ⇒ identical output; no manual edits.
5. **Never crashes** — every error becomes a status with an actionable reason.
