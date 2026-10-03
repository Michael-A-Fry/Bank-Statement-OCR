# Statement Studio — bank statement conversion engine

A pure-**R** engine that turns a bank or card statement into clean, structured,
downloadable data (**Excel + CSV + JSON**). It is built for forensic accounting:
descriptions kept verbatim, nothing hidden is ever read, no silent data loss, and
**no silently wrong figures**.

**Since 2.0.0 there are no templates.** You pick the **bank**, and the tool fills
it in from the statement itself. It then reads each statement from its
**content** (what is a date, what is a figure, what lines up with what) and
**proves** the reading with the statement's own arithmetic: the running balance,
opening plus movements equals closing, and the printed totals. A reading that
proves converts with no clicks. A reading it cannot prove is shown to a person
with the reason, and is fixed in two clicks. Each bank's layouts are **learned**
from the statements it has proved, so the next statement of that layout is
quicker and surer.

**No Python, no machine learning.** Learning here means keeping versioned
records of readings the arithmetic proved, each traceable to the statements that
proved it, and each one an admin can undo.

## Documentation

Three pages to read straight through, for whoever needs the whole thing at once:

- **[overview.md](docs/overview.md)**: what it is and what it is for. The
  problem, what it guarantees, what it deliberately does not do, who does what,
  and what it costs to run. No code in it.
- **[design.md](docs/design.md)**: the technical map, for a developer inheriting
  it cold. The shape, the path a statement takes, automatic reading (geometry
  proposes, arithmetic decides), banks and learned layouts, the invariants, and
  how to ship a change and put it back.
- **[story.md](docs/story.md)**: how it got this shape. Every turn that made it
  something different, and why.

Then one page per job:

- **[For analysts](docs/for-analysts/README.md)**: the four pages a forensic
  accountant needs and nothing else. Convert a statement, convert one from a bank
  the tool has not seen, work out what to do when something looks wrong, and
  describe a layout safely. This is the folder to hand to somebody new.
- **[Operational](docs/operational/README.md)**: how to *do* things. The four
  above, plus setting it up, running it, backing it up, updating it, admin, and
  wiring up Qlik.
- **[Context](docs/context/README.md)**: how it works and why. The charter, the
  automatic-reading specification, the data contract, the engine parameters, the
  edge-case register, the findings register and the roadmap.

**Updating a server from 1.x to 2.0.0?**
[release-2.0.0-hand-carry.md](docs/operational/release-2.0.0-hand-carry.md)
lists every file to update, add or delete. A plain copy-over is not enough for
this release.

**New here?** [first-time-setup.md](docs/operational/first-time-setup.md): two
double-clicks, then set the admin password and open the firewall port.

**Putting it on the server for the whole team?**
[deploy-on-the-qlik-server.md](docs/operational/deploy-on-the-qlik-server.md):
one page, start to finish. It covers the service account and its rights, the
port, the scheduled task that brings the app back after every reboot, the
firewall rule, and the address people type.

**Turning it on for a unit today?**
[go-live-checklist.md](docs/operational/go-live-checklist.md): the numbered
list for the day. If the day goes wrong, see
[rolling-back.md](docs/operational/rolling-back.md), including what happens to
the conversions a bad version already sent to Qlik.

**Inheriting it?** Read [charter.md](docs/context/charter.md) (one page: what
this tool must always do and must never do), then [design.md](docs/design.md),
then [maintaining-the-engine.md](docs/operational/maintaining-the-engine.md)
(running the suite on the server, and what an update overwrites). Then back up
the irreplaceable folders:
[backup-and-restore.md](docs/operational/backup-and-restore.md).

**Someone says a conversion is wrong?**
[investigating-a-wrong-conversion.md](docs/operational/investigating-a-wrong-conversion.md)
goes from a run id or a feedback record to the original file, reproduces the
same figure from the command line, and helps you tell a learned layout from a
reading fault from a bad scan.

## What it does today

- **Reads CSV, TSV, Excel (`.xlsx`) and PDF**, including scanned PDFs. Scans are
  read as pictures (OCR, about 2 seconds a page) and flagged as such.
- **Bank first, filled in for you.** The bank is taken from the account holder's
  own account number in the official Payments NZ branch register, then from the
  bank's legal name, website, phone number and brand words. The account number
  itself is never logged or stored.
- **Reads every statement from its content and proves it.** Each reading ends
  in one of four outcomes:
  - **Proven**: every running-balance step adds up to the cent, and no other
    reading of the columns does.
  - **Matches a learned layout**: no balance of its own, but the statement
    matches a layout already proven for that bank.
  - **Please check**: shown to a person, with the reason.
  - **Couldn't read**: also with the reason.
- **Please check** shows the found columns drawn on the page. A person sets a
  column's role from a dropdown and re-reads, and a fix that then proves is
  learned for that bank. A fix that does not prove, or a plain confirm, applies
  to that file only and waits for an admin.
- **Learns each bank's layouts.** A layout is provisional until three statements
  prove it or an admin confirms it. Every change is a new version, never an
  edit, and every output is stamped with the build and the learned state that
  produced it, so any conversion can be re-run and give the same answer.
- **Measured, with an answer key.** On the realistic synthetic dev set, 121 of
  128 text PDFs, 5 of 5 Excel files, 4 of 7 CSV files and 12 of 15 scans were
  read automatically and correctly. **None was read automatically and wrongly,
  on any set** (the 2.0.0 entry in [CHANGELOG.md](CHANGELOG.md)). The held-back
  acceptance sets have not been scored yet.
- **Governed analytics.** A statement feeds the Qlik dashboards when it was
  proven, matched a proven layout, or was confirmed by a person, and every
  conversion says on screen whether it was published or held back. Marking a
  result *wrong* withdraws it.
- **Tracking with no personal data.** Admin -> Automatic reading counts what
  was proven, what failed and why, and the spot-check answers, against the 95%
  target.
- **A full automated test suite** guards every guarantee, and the runner fails
  on a skipped test, because a skip proves nothing. `Rscript tests/run_tests.R`
  prints the current totals. The last measured baseline is kept in one place,
  [maintaining-the-engine.md](docs/operational/maintaining-the-engine.md).

## The app

Three tabs:

- **About**: what the tool does and how to read its outcomes.
- **Convert**: upload one statement or a whole case folder, check the bank,
  click Convert, read the outcome, and download. Please check, spot checks and
  the column editor (the last resort) are all here.
  ([converting-statements.md](docs/operational/converting-statements.md))
- **Admin** (reached with `?admin`, and password-protected): **Banks** (learned
  layouts, fixes waiting for an admin, train a bank), **Automatic reading** (the
  counts, and the spot-check rate), **Words** (the dictionaries) and **Health**.
  Until `app.admin_password` is changed from the shipped placeholder, Admin
  **refuses to open for anybody**.
  ([admin-and-maintenance.md](docs/operational/admin-and-maintenance.md))

Outputs per statement: a six-sheet `.xlsx` (`Transactions`, `Summary`, `Checks`,
`Provenance`, `Diagnostics`, `Metadata`), a `.csv` of the transactions table,
and a `.json` holding everything, including the build stamp and provenance.

## Forensic guarantees

1. Descriptions preserved **verbatim**, special characters intact.
2. The tool **never redacts and never reveals**. Statements arrive redacted and
   it reads only what is visible. An amount it cannot read but that the running
   balance proves is filled in, **marked as worked out**, and always sent to a
   person.
3. **Nothing automatic unless proven.** A statement converts with no person
   involved only when its own arithmetic proves the reading uniquely, or when it
   matches a layout that statements of the same design have already proved.
4. **No silent drops.** Every page with transaction-shaped lines must give rows,
   and every dated line must be used, before a reading can be proven.
5. **Reproducible.** The same input, the same build and the same learned state
   give byte-identical output. The engine version and the learned-state hash are
   stamped *inside* each output. That is what makes the claim checkable, and it
   is also why outputs from two different builds cannot match byte for byte even
   when every figure does.
6. **Never crashes.** Every error becomes a status with an actionable reason.
