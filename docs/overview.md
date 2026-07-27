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

---

## What it does

A person opens a browser, uploads a statement or a whole case folder, and clicks
Convert. The tool works out which bank and layout it is, reads every transaction,
runs ten checks over the result, states in plain English how much of it is proven,
and offers the data as Excel, CSV and JSON. Every conversion is logged; clean
conversions from tested templates feed the Qlik dashboards automatically.

- **Formats read:** CSV, TSV, Excel (`.xlsx`) and PDF. Digital PDFs are read from
  their text layer; scanned pages are OCR'd and flagged as scanned.
- **Layouts covered today:** 13 shipped templates — 7 delimited (ANZ everyday and
  credit card, ASB, BNZ, Kiwibank, Westpac, plus a cross-bank Xero-standard
  import), 5 PDF and one generic Excel. Twelve take part in detection; the
  thirteenth is a tutorial specimen, deliberately excluded so a demo's wording can
  never match a real statement. On top of that there is a separate key-value mode
  for labelled-value documents such as KiwiSaver and IRD-style summaries.
- **Adding a layout is a template, not code.** An analyst uploads one example,
  confirms what the tool detected against a live preview of the real statement,
  and saves. It is YAML underneath, written by the tool.
- **Outputs per statement:** a six-sheet workbook (Transactions, Summary, Checks,
  Provenance, Diagnostics, Metadata), a CSV of the transactions, and a JSON
  holding everything including the build stamp.
- **Speed:** seconds. On the development box, an 11-page PDF with 311
  transactions converted in about 3.5 seconds; a two-page scan took about 9
  seconds including OCR. A case folder is one click and one progress bar.

---

## What it guarantees

Each of these is a mechanism, not an aspiration, and each has automated tests
that fail the build if it stops being true.

1. **Descriptions come out exactly as printed.** The only thing done to a
   description is trimming outer whitespace. `O'Connor & Sons` survives intact.
2. **Redactions are honoured absolutely.** Statements arrive already redacted;
   the tool never un-redacts, never estimates a hidden value, and never
   back-calculates one from a balance. A black box drawn over a PDF frequently
   leaves the original text still in the file — the tool treats the box as
   authoritative and does not read what is underneath. If the scan that detects
   those boxes could not finish on a page, the run **fails** and says so, rather
   than quietly falling back to the raw text.
3. **The same file plus the same template always produces the same bytes.** Every
   output carries the engine version and a content hash of the template that
   produced it, so a figure can be reproduced years later and it can be proved
   which build made it.
4. **It never crashes.** Every failure becomes a status with a reason a person
   can act on.
5. **A check that could not run never shows a green tick.** This is the one that
   matters most. Ten checks exist; each statement gets the ones that apply to it
   (three are raised only when there is something to say — the money-direction
   check, OCR read quality, and the redaction scan). Every check reports pass,
   fail, or "could not be checked" with the reason. There is no fourth state.
6. **Only clean, reconciled conversions from tested templates reach the
   dashboards** — and every conversion, published or held back, is told on screen
   which it was and why.

---

## What it does not guarantee

Stated plainly, because a promise nobody qualified is how a wrong figure gets
believed.

- **It cannot prove completeness on a PDF or an Excel file.** Completeness is
  proven by comparing the number of physical source lines against the number of
  parsed rows. Only a delimited file has an independent line count. For a PDF or
  a spreadsheet, that check honestly reports "could not be checked", and
  completeness rests on the balance reconciliation instead. The direct consequence
  is that **a PDF or Excel statement never reaches the top confidence level**, no
  matter how clean it is. Medium is the healthy ceiling for those formats. This is
  deliberate; making the check pass would be the lie.
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
- **A template is only as good as its example.** A shipped template has a golden
  test proving it reads a real statement of that bank correctly. A template built
  in the app has not been through that, which is exactly why its output is held
  back from the dashboards until someone promotes it.
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
   why. Nobody can wave it through: the gate takes no human input at all. The way
   a withheld conversion gets published is by fixing the template and converting
   again, which leaves an audit trail.
4. A diagnostic names the discrepancy, the rows involved, and **who fixes it** —
   the analyst in the template toolkit, whoever supplied the file, or a developer.

---

## What happens when it cannot read something

There are three different outcomes, and they are deliberately different.

**The layout is one it has never seen.** The run is *unsupported*, no figures are
produced, and nothing is published. The screen offers two routes: build a template
for it in the toolkit (upload the example, confirm the columns against a live
preview, save — from then on that layout converts automatically), or, if the
person would rather not, raise it with the team. Raising it writes a request
record holding only what they typed plus non-identifying context — file extension,
bank label, the options that were on screen. Never statement content.

**A template matched the wording but read no transactions.** This is a different
problem with the opposite fix: the template exists, its columns are in the wrong
place. The result names the template that matched and points at it, rather than
sending the analyst to build a third one that would have the same problem.

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
**all** of these hold:

- the conversion came out clean (`ok` — every check that could run, passed);
- its confidence meets the configured floor (ships as *medium*, because with
  *high* every PDF and Excel statement would be withheld);
- it was read by a **shipped, tested** template, not one built in the app;
- the feed is enabled and the folder could actually be written to (a read-only
  share is reported, never recorded as a clean publish).

Withheld rows go to a separate `feed/review` folder, deliberately loadable, so the
team can see what is being held back and why. Every row carries its own gate
result, so the two can never be silently pooled into one total.

Marking a result **wrong** in the app withdraws that run's rows from the published
feed. A re-convert that flips the decision the other way purges the stale rows too.

---

## Who does what

**The forensic accountant** — the person the correctness and UX bar is set for.
Uploads a statement or a case folder, reads the verdict, downloads the data. She
is never asked a question the tool could answer itself, and never asked one she
could not answer (which of two internal template variants a bank used, for
instance, is decided by the tool and flagged for review, not put to her).

**The analyst who adds a bank** — often the same person. Uploads one example of a
new layout, confirms the detected columns against the live preview, saves. No
YAML, no code, no developer. The tool then re-checks the statement against every
template and says in words whether it will be recognised on its own next time.

**The maintainer / admin** — one person, part-time. Sets the admin password (until
they do, the Admin tab refuses to open for anybody, because the placeholder is
printed in the example config). Watches Insights: which layouts keep failing, which
templates are drifting, which requests are outstanding. Promotes a good user
template into the shipped set — which is what lets its output reach the dashboards.
Edits the wording dictionary and the marker-word lexicon in plain English. Runs the
tidy-ups. Keeps the backups.

**The Qlik analyst** — consumes `feed/transactions/` with a folder connection and
a scheduled reload. Never touches the app.

**No figure is ever hand-edited.** There is no cell to type in. If a figure is
wrong, the fix is a template correction and a re-run, so the output stays
reproducible and provenance survives. The one human override that exists is
narrow and self-declaring: from the X-ray view an analyst can say "this skipped
line *is* a transaction", which adds the row **as the page printed it**, flags it
`forced`, and lowers the run's stated confidence.

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
required", and it is why a new bank is a YAML template rather than a code change.

**Disk:** small and bounded. Everything is one file per event, never a shared
append, which is also why ten people can use it at once with no locking. Uploaded
statements are copied so a failed format can be picked up; those copies are real
client data, so they are deleted automatically after 90 days (configurable) while
the small record of what happened is kept forever. Run logs older than 90 days are
rolled into a yearly archive; nothing is lost.

**Scale designed for:** a department. Tens of users, hundreds of statements a
week, with per-user isolation — Shiny sessions are isolated, so nobody sees
another person's upload or result. The one deliberately shared resource is the
template library, because that is a team asset rather than user data.

**The ongoing cost is templates, and it is meant to be.** Every new bank, and
every layout change at an existing bank, is one YAML template plus one test file.
That is the whole maintenance model.

---

## Where it stands

Version 1.2.0. 13 shipped templates, ten reconciliation checks, an automated suite
of several hundred tests that fails on a *skipped* test as well as a failing one
(because a skip proves nothing). A register of 109 findings from adversarial
review and real use, 104 of them fixed; the open ones are recorded with what they
are waiting for. Two of them are waiting on evidence rather than effort — a spread
of real forms, and one hand-keyed scanned statement to measure OCR digit accuracy
against — and both fail closed and loudly today.

The engine is done in the sense that matters: the next three things on the roadmap
are watching a real analyst build a template on a real statement, proving the
install on a real locked-down Windows box, and adding templates as real files
arrive. None of them adds an R module.
