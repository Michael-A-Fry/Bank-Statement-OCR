# Statement Studio — what it is, and what it is for

For anyone who needs to understand this without reading code: what problem it
solves, what it promises, what it deliberately does not promise, what it costs,
who does what, and what happens when it cannot read a file.

The rules it is measured against are in `docs/context/charter.md`. The cardinal
one is **never silently wrong**: a wrong figure that looks right is the worst
outcome this tool can produce, and a loud refusal is always better.

---

## The problem before it existed

Forensic accountants and financial-crime investigators work from bank statements.
The statements arrive as PDFs — some with a text layer, some scanned images — and
as CSV and Excel exports. Every bank prints a different layout, and the same bank
changes layout between years.

The tool this replaces was a Qlik app ("Statement Converter"). It worked, and it
had three structural problems:

1. **PDF reading depended on a licensed external connector** (Mole), which
   extracted every word of a page with its position. That was a paid dependency
   sitting under the whole capability.
2. **Every bank was hand-written script.** The connector gave word positions;
   per-bank Qlik script then walked those positions looking for anchor words
   ("Withdrawals", "No transactions for this period"), with exclusion flags for
   interest lines and so on. Adding a bank, or absorbing a layout change, meant
   someone writing more script. The logic was imperative, buried in a binary
   `.qvf`, and understood by very few people.
3. **Nothing proved the extraction was complete.** If a page's rows were split
   across two baselines, or a footer was folded into a transaction, or a column
   band sat three points too far left, the output looked exactly as it does when
   everything worked. Whatever could not be read that way was re-keyed by hand,
   which is slow and has no audit trail either.

That third one is the reason this project exists. Speed was never the hard part.
**Being able to say, and defend, that nothing was missed** was.

Version 1 replaced the hand-written script with declarative per-bank
**templates**. Version 2 (2.0.0, October 2026) removed templates altogether:
measured on 128 realistic statements, the shipped templates read none of them
perfectly, and templates drafted per file read a quarter. The tool now reads
every statement from its **content** and proves the reading with the statement's
own arithmetic.

---

## What it does

A person opens a browser, uploads a statement or a whole case folder, checks the
**bank** (filled in from the statement itself), and clicks Convert. For each
statement the tool:

1. **finds the columns from what is printed in them**: what is a date, what is a
   figure, what is words, what lines up with what, on every page;
2. **works out which column is which by the arithmetic.** It tries every
   assignment of money out, money in, signed amount and balance, and accepts a
   reading only when every running-balance step adds up to the cent and **no
   other reading does**. Without a running balance it uses opening plus
   movements equals closing, and the printed totals;
3. **gives one of four outcomes**: *Proven*; *Matches a learned layout* (no
   balance of its own, but the same design as statements of that bank that have
   already proved themselves); *Please check*, shown to a person with the reason
   and fixed in two clicks; or *Couldn't read*, with the reason;
4. **learns the bank's layout** from every proven statement, so the next one of
   that design is quicker and surer;
5. offers the data as Excel, CSV and JSON. Every conversion is logged, and
   proven conversions feed the Qlik dashboards automatically.

- **Formats read:** CSV, TSV, Excel (`.xlsx`) and PDF, digital or scanned. Scanned
  pages are read as pictures (OCR, about 2 seconds a page) and flagged as such.
- **Banks covered:** any bank whose statements carry their own arithmetic. No
  bank needs anything set up first. The bank is identified from the account
  holder's own account number in the official Payments NZ branch register
  (shipped with the tool), then from the bank's legal name, website, phone number
  and brand words. The account number itself is never logged or stored.
- **Measured on synthetic statements with an answer key**, before release: 121
  of 128 realistic text PDFs, 5 of 5 Excel exports, 4 of 7 CSV exports and 12 of
  15 scans read automatically and right. **None was read automatically and
  wrong, on any test set.** The rest went to a person, with the reason. The target
  is 95% of each kind, with none wrong; the held-back acceptance sets are still to
  be scored (the 2.0.0 entry in `CHANGELOG.md`).
- **Statements only.** The form and report modes were removed at 1.9.0.
- **Outputs per statement:** a six-sheet workbook (Transactions, Summary, Checks,
  Provenance, Diagnostics, Metadata), a CSV of the transactions, and a JSON
  holding everything, including the build stamp.
- **Speed:** seconds for a digital statement; a scanned page adds about 2
  seconds of OCR. A case folder is one click and one progress bar, and each
  conversion runs in its own process, so a colleague's long scan never freezes
  your page.

---

## What it guarantees

Each of these is a mechanism, not an aspiration, and each has automated tests
that fail the build if it stops being true.

1. **Descriptions come out exactly as printed.** The only thing done to a
   description is trimming outer whitespace. `O'Connor & Sons` survives intact.
2. **Nothing is automatic unless the arithmetic proved it.** A statement converts
   with nobody looking only when its own figures prove the reading, and no other
   reading of its columns fits; or when it matches a layout of its bank that
   statements of the same design have already proved. Everything else goes to a
   person, with the reason. On every test set measured before release, the number
   of statements read automatically and wrongly was **zero**.
3. **The same file, on the same build, against the same learned state, always
   produces the same bytes.** Every output carries the engine version and a hash
   of everything the tool had learned when it ran, so a figure can be reproduced
   years later and it can be proved what made it. Learned layouts are never edited,
   only added to, so a past state is never lost. Those two stamps are *inside* the output, which
   is the point — and the reason a re-run on a newer build cannot match the old
   bytes even when every figure is identical. Reproducing an old figure means
   comparing the figures, having first checked what has changed underneath.
4. **It never crashes.** `convert_statement()` wraps its whole body in a
   `tryCatch`, including everything that touches the file path, so every failure —
   right down to being handed something that is not a file name at all — comes
   back as a status with a reason a person can act on.
5. **A check that could not run never shows a green tick.** Every check that
   appears says one of **four** things: **OK**, **Problem**, **could not be
   checked**, or **for information** (a count, such as the OCR read quality, which
   cannot pass or fail). Marking a count as a count is what stops it ever reading
   as a pass it did not earn.

   There is no fifth state, and — the part that matters — no silent one. Every
   check that exists for a statement is on the screen with one of those four words
   beside it and its figures in the two columns next to it. Where no check exists
   — a file that could not be read, a statement nothing usable was read from —
   the table is replaced by a sentence saying nothing was extracted and so there was nothing to
   check, because a blank table cannot be told from one that failed to draw.
6. **An amount worked out rather than read is always marked, and always goes to
   a person.** When an amount cannot be read but the running balances either side
   prove what it must be, it is filled in, marked in the Flags column, and the
   statement goes to Please check.
7. **Only proven, layout-matched or person-confirmed conversions reach the
   dashboards**, and every conversion, published or held back, is told on screen
   which it was and why.

---

## What it does not guarantee

Stated plainly, because a promise nobody qualified is how a wrong figure gets
believed.

- **It cannot prove a statement that carries no arithmetic.** An export with no
  running balance and no totals has nothing to add up. The first ones from a
  design always go to a person; once a layout is proven by statements that do
  carry a balance, later ones of the same design convert on that layout. Those
  are counted separately and spot-checked twice as often, because the
  arithmetic cannot reach them.
- **It cannot count physical lines on a PDF or an Excel file.** The *No row failed
  to read* check needs an independent line count, which only a delimited file has.
  On a PDF completeness rests on the reader's own checks instead: every page with
  transaction-shaped lines gave rows, every dated line was used, and the balance
  chain held across pages.
- **"Row count" usually has nothing to check against.** Most statements do not
  print how many transactions they contain. When one does, the check compares. When
  one does not, it degrades to "at least one row was read" and says so in its own
  detail line.
- **It cannot detect a transaction the statement never printed.** If the bank
  omitted a row, or a page is missing from the PDF you were given, the balance
  check will usually catch it — and if the statement prints no balances at all,
  nothing can.
- **OCR is not guaranteed accurate.** Scanned pages are machine-read. The tool
  reports how many pages were OCR'd and the worst page's confidence, caps
  confidence for the whole run, and flags individual cells that scored low. It
  does not claim the digits are right. Measuring scanned digit accuracy against a
  hand-keyed statement is an open item; it is blocked on having one.
- **It does not categorise transactions.** No transaction type, no category, no
  tax year. The old Qlik tool derived those from keyword lookups; recreating that
  is planned work, deliberately downstream of extraction and out of the engine.
- **It is not the system of record.** It converts and feeds. The durable archive
  of statements is somewhere else.
- **A person's confirm is a person's word.** *This is right* on Please check
  converts a statement the arithmetic could not prove, on that person's say-so.
  It is refused when the arithmetic contradicts the reading, it is stamped as a
  person's on every dashboard row, and it never teaches the tool until an admin
  accepts it.
- **It is not yet measured on the server's own statements.** The figures above
  come from synthetic statements. On the server, tracking and spot checks run
  from the first day. About 300 clean spot checks are needed to say "under 1%
  wrong", and about 500 statements to say "at least 95% automatic".
- **It cannot tell you the statement itself is genuine.** It will report what a
  PDF's own header says produced it — naming an editor or converter is a fact
  about the file, not an accusation — and it stops there.

---

## What "it reconciles" actually means

The headline check is **opening balance + every transaction = closing balance**,
compared to the cent.

- If the statement prints both balances, that is the arithmetic, and it is the
  strongest completeness proof available.
- If it prints only one of them and carries a running-balance column, the missing
  one is **derived** from that column, and the check says on screen that it was
  derived. Deriving both is refused: that would just re-check the balance
  column's own endpoints and could hide a break in the middle.
- If it prints neither and has no running-balance column, the check reports
  "could not be checked" and names which anchor is missing. It does not guess.
- If both balances are printed but a single amount could not be read, the check
  **fails** rather than reporting "not applicable". The strongest available proof
  was there and one unreadable figure stopped it; that is a problem, not an
  absence.

A second check follows the running balance row by row and reports how many times
it does not follow from the one before, bridging blanks rather than skipping over
them so a break cannot hide inside a gap.

**When reconciliation fails**, four things happen, in this order:

1. The run is marked *needs review*. It is not marked failed — the workbook, CSV
   and JSON are all still produced, because the analyst still needs the data.
2. Confidence drops. A failure normally means low confidence; the one exception
   is that a failure of only the secondary running-balance or period checks, when
   the balance itself fully reconciles, is honestly rated medium rather than low.
3. **The rows are withheld from the Qlik dashboards**, and the screen says so and
   why. The way a withheld conversion gets published is a corrected reading on
   Please check (a fix that then proves), or a person's confirm, refused when the
   arithmetic contradicts it. Both leave an audit trail.
4. A diagnostic names the discrepancy, the rows involved, and **what to do about
   it**, in a sentence that says whose job it is by naming the action: set a
   column's role on Please check (the analyst), re-export or re-scan or split the
   file (whoever supplied it), or something neither of them can do (escalate). The
   screen shows the sentence; the downloaded workbook carries the same rows plus a
   one-word `fix_owner` triage for whoever maintains the tool.

---

## What happens when it cannot read something

There are three different outcomes, and they are deliberately different.

**It read the statement but could not prove it.** The outcome is *Please check*,
with the reason in a sentence that usually names a row and a page. The figures are
produced, nothing is published, and Please check shows the columns it found,
drawn on the page. One dropdown per column of figures and a Re-read button fix
most of these in two clicks. A fix that then proves is learned for that bank.

**It read nothing usable.** The outcome is *Couldn't read*, with the reason. If it
found columns, Please check still offers them. The last resort is drawing the
columns by hand, for that file only.

**The file cannot be read at all** — corrupt, encrypted, a scan with no OCR
available. The run is *failed*, with the reason.

In every case the uploaded file is kept byte-for-byte under `uploads/` so it can
be picked up and fixed later, and a "safe summary" of any statement can be
downloaded and shared: page sizes, counts and value *shapes* (`$9,999.99`,
`99 Xxxx 9999`) with no names, numbers or amounts in it. That is how a layout
problem gets discussed with someone who is not allowed to see the statement.

---

## Governance: what reaches the dashboards

While the feed is switched on, every conversion writes a manifest row, published
or not, so coverage is never silent. Rows reach the Qlik dashboard table only when
the conversion is `ok` **and** it was:

- **proven** by its own arithmetic; or
- a **match to a learned layout** of its bank that is already proven; or
- **confirmed by a person** on Please check (refused when the arithmetic
  contradicts the reading).

Each row carries which of the three it was, so a dashboard can tell a figure the
arithmetic proved from one a person vouched for. And the feed must be enabled and
its folder writable (a read-only share is reported, never recorded as a clean
publish). Since 2.0.0 the gate has no settings to loosen.

Withheld rows go to a separate `feed/review` folder, deliberately loadable, so the
team can see what is being held back and why. Every row carries its own gate
result, so the two can never be silently pooled into one total.

Marking a result **wrong** in the app withdraws that run's rows from the published
feed. A re-convert that flips the decision the other way purges the stale rows too.

---

## Who does what

**The forensic accountant** — the person the correctness and UX bar is set for.
Uploads a statement or a case folder, checks the bank, reads the outcome, and
downloads the data. When a statement did not prove itself, she looks at Please
check: confirms it, or sets the one column that is wrong. She is never asked a
question the tool could answer itself, and never one she could not.

**The admin** — one person, part-time. Sets the admin password (until they do,
the Admin tab refuses to open for anybody). On **Banks**: trains each bank on
every statement the unit has for it, confirms or retires learned layouts, and
accepts or discards a person's fix that the arithmetic could not prove. On
**Automatic reading**: watches the share read automatically against the 95%
target, and sets the spot-check rate. Edits the wording dictionaries in plain
English. Keeps the backups.

**The Qlik analyst** — consumes `feed/transactions/` with a folder connection and
a scheduled reload. Never touches the app.

**No figure is ever hand-edited.** There is no cell to type in. If a figure is
wrong, the fix is a corrected reading and a re-run, so the output stays
reproducible and provenance survives. The human overrides that exist are narrow
and self-declaring: a column's role set on Please check, a confirm, or columns
drawn by hand, and each is stamped on the output as a person's.

When somebody says a conversion is wrong, the maintainer's procedure — from a
run id to the original file, the same figure reproduced from the command line,
and learned layout versus reading fault versus bad scan — is
[`docs/operational/investigating-a-wrong-conversion.md`](operational/investigating-a-wrong-conversion.md).

---

## What it costs to run

**Licences: none.** That is the largest single change from the tool it replaces,
which needed a paid PDF connector under the whole PDF path. This reads PDFs with
open-source R packages.

**Infrastructure: one Windows box, offline.** No database, no server software, no
cloud, no internet. The install is one folder: it brings its own private copy of R
and its own packages, so whatever R the server already has is left untouched.
Setup is two double-clicks (build a bundle on a PC with internet, copy the folder
across, run it), then two one-time admin jobs — set the admin password, and open
one firewall port so people other than whoever is standing at the server can reach
it. Optionally register it as a scheduled task so it survives reboots.

**People: one analyst, part-time.** That is the design constraint, not an
aspiration — the charter says "one analyst can run and grow it, no engineer
required", and it is why a new bank needs nothing at all: its statements teach it.

**Disk:** small and bounded. Everything is one file per event, never a shared
append, which is why ten people can use it at once with no locking. Uploaded
statements are copied so a failed format can be picked up; those copies are real
client data, so they are deleted automatically after 90 days (configurable) while
the small record of what happened is kept forever. Run logs older than 90 days are
rolled into a yearly archive; nothing is lost.

**Scale designed for:** a department. Tens of users, hundreds of statements a
week, with per-user isolation — Shiny sessions are isolated, so nobody sees
another person's upload or result. The one deliberately shared resource is the
store of learned bank layouts, because that is a team asset rather than user data,
and it holds no client data: column roles, date and money styles, heading words.

**Several people converting at once is now genuinely parallel.** Each conversion
runs in its own short-lived process rather than inside the one that draws the
screen, so a colleague's scanned statement no longer freezes your page while it
reads. How many run at once is capped for the size of the box, and anyone past
the cap is queued **and told on screen that they are queuing** — the tool does
not do silent waiting any more than it does silent figures.

**The ongoing cost is looking at what the tool could not prove.** A new bank, or
a layout change at an existing one, costs a training run and a look at the
statements that needed one. A bank changing its print shows up on Admin -> Health
as a layout that started failing.

---

## Where it stands

Version 2.0.0: automatic reading, bank identity, learned layouts, tracking and
spot checks, with an automated suite that fails on a *skipped* test as well as a
failing one (because a skip proves nothing). The held-back acceptance sets (a
realistic holdout and 100 deliberately weird statements) are still to be scored,
and several release blockers are open; the 2.0.0 entry in `CHANGELOG.md` lists
them.

**No figure that moves is repeated on this page**, deliberately: a stale number
here reads as a regression signal. Each one lives in exactly one place, which is
where to read it.

| Figure | Read it in |
|---|---|
| the version | the `VERSION` file, and `build.engine_version` on every output |
| the test totals, and the last full run | [`operational/maintaining-the-engine.md`](operational/maintaining-the-engine.md) §1 |
| findings raised, fixed and open | [`context/findings-register.md`](context/findings-register.md), which states its own running total at the top |
| what is not yet done, in the words it was raised in | [`context/outstanding-work.md`](context/outstanding-work.md) |

The open findings that are blocked on **evidence nobody has** rather than on
effort — a spread of real forms, and one hand-keyed scanned statement to measure
OCR digit accuracy against — fail closed and loudly today, so neither can put a
wrong figure on screen.

What comes next is in [`context/roadmap.md`](context/roadmap.md): the
independent acceptance run, closing the release blockers, two engine faults
that could let a proven reading be wrong, and then the 95% target on each kind of
file.
