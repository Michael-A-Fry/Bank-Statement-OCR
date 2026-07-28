# Changelog

The version record. Newest release first.

`VERSION` is stamped into every run log, every JSON output and the feed manifest,
so the heading below has to match it: that is what makes "which build produced
this figure?" answerable after the fact.

This file is deliberately terse. Each entry says what changed and, where a wrong
figure was possible, what stopped it. The evidence for every line is in
[`docs/context/findings-register.md`](docs/context/findings-register.md), keyed by
finding id.

---

## 1.3.0

The release where the pages about the tool were held to the same standard as the
figures in it — and where the toolkit stopped asking for things it can work out
for itself.

**The tool answers more of it**

- The toolkit reads the bank's own name off the statement — the masthead first,
  then the legal imprint — instead of the first bank word anywhere on the page.
  That had answered "ANZ" on an ASB statement, because ANZ was one of the payees.
  A bank nobody has taught it now gets its own name too.
- When the wrong template read a statement, the way to correct it is on the
  result above the fold, not behind the evidence toggle. A statement read end to
  end by the wrong template looks perfect on screen; it is the failure this tool
  exists to prevent.

**Nothing fails quietly — including the front door**

- `convert_statement()` now wraps everything that touches the file path. Handing
  it something that is not a filename comes back as a status with a reason. Two
  documents had claimed "it never crashes" before it was true of the whole
  function.
- Admin controls that changed something on disk and said nothing now say so, and
  a table with nothing in it says so rather than instructing the reader to use a
  picker with no options in it.

**The documentation became part of the product**

- Three read-through pages — what it is, how it is built, how it got this shape —
  are linked from the README and inside the guards, instead of sitting where the
  orphan test could not see them.
- Both doc guards were widened: no page can be orphaned from an index, and a path
  named anywhere in the tree — a document, an engine comment, a README beside the
  templates — has to resolve to a file that exists. Each caught a real break
  before it was fixed.
- Every claim in those pages was read back against the running code, and the ones
  that had stopped being true were corrected — a fourth result word the docs never
  mentioned, a count of checks that was one short, one screen with four names, and
  measurements nobody had re-taken.

**Two things a reader could not do, and now can**

- The analyst's guides describe the screen she actually has: the first thing the
  app asks her for, the four words a check can come back with, and the column that
  tells her whose problem it is. "Teach it a new bank" now follows the toolkit's
  own numbered steps and starts where the work really starts — reading the preview.
- The maintainer has a procedure for *somebody says a conversion is wrong*: run
  id, original file, the same figure reproduced from the command line, template
  bug or bad scan. And a design document that says how to run it, how to ship a
  change to it, and how to put it back.

**The pages were measured against the running app, not against each other**

- `N107` — the folder an accountant was handed, `docs/for-analysts/`, did not
  exist; her pages sat in a directory named for operators. That folder now
  exists, as the short index of her four pages, and the README and the
  operational index both route to it. A new guard fails the suite when any
  `docs/` path named in prose — **including a folder, which has no `.md` on the
  end and which every earlier scan therefore skipped** — points at nothing.
- A second new guard loads **every fenced settings block in the documentation
  through the real config loader** and fails if the Admin tab would open. A
  printed admin password is a password everybody has; the shipped placeholder is
  safe only because the app treats it as "nobody has set one yet", and a
  near-miss like `change-me` is a working password that reads like a placeholder.
- `design.md` §8 said adding a check makes "existing tests" fail and pointed at
  three. Re-run and counted: one ordinary new check took the suite to **27 failed
  and 1 error, in 17 tests across 10 files**, and the runner's reporter shows the
  first ten and says so in a line that is easy to miss. The section now carries
  the real number, the command that shows all of it, and the order to read it in
  — the six bank goldens first, because they are the only failures that are
  evidence about the check. Its recipe also gained the step that was missing: a
  new diagnostic category must be **raised**, not just declared, or the suite
  fails it as a dead row.
- `when-something-goes-wrong.md` was re-checked against the Diagnostics table as
  it renders. It had told the accountant that an *info* row's "How to fix" says
  *No action needed* — false for three of the five, including the one the page
  used as its example, whose sentence asks her to review per account when several
  accounts are mixed. It had also filed *"check those pages against the source
  PDF"* under "ask the supplier for a cleaner scan"; that sentence belongs to the
  one diagnostic that means **do not release this output**. The page now lists
  every phrase the *What* column can print, against what it means for her.
- `adding-a-bank-template.md` now describes what the drafter really does with
  currency: every name found on the figures goes into one pile and is saved only
  if there is exactly one, so a code does *not* outrank a symbol that disagrees
  with it. Six codes and four symbols are recognised, the mark has to sit before
  a figure printed with cents, and **NZD in that box is usually a fallback nobody
  checked** — which looks identical to a reading.
- A doc said `scripts/run_app.R` "prints the address". It prints the literal
  placeholder `http://<this-vm>:8100`, which nothing resolves. Reworded, in both
  places that said it, with what the console really shows.
- The root pages were re-read against the code: `R/jobs.R` runs every conversion
  in its own child process, so "no lock, no queue, nothing to tune" is no longer
  true — there is a queue, it is capped in threads as well as processes, and
  anyone past the cap is told they are queuing. The stale findings tally and an
  un-retaken speed measurement were replaced with the register's own figure and a
  fresh one.

**Suite:** grew again this release (725 tests at 1.2.0, 792 partway through this
one). The live totals are not repeated here, because 1.3.0 is still open and a
number frozen mid-release reads as a promise about today; the last full run is
recorded in
[`docs/operational/maintaining-the-engine.md`](docs/operational/maintaining-the-engine.md),
and the pass condition that does not move is *nothing failed, nothing errored,
nothing was skipped*. Register totals likewise live in
[`docs/context/findings-register.md`](docs/context/findings-register.md), which
states its own.

## 1.2.0

Eighteen findings, all but two raised from real use on real statements. Five put
wrong figures on screen; two of those were regressions introduced inside this
release and caught by reviewing it.

**Wrong figures**

- `N37` — a `CR` marker printed on the line below its amount was folded into the
  payee, so every credit-card payment and refund came out with the charge sign.
  The marker is now appended to the money cell it sits under.
- `N30` — a printed opening or closing balance could be kept as a transaction, at
  the balance's own material amount. The summary-line guard now reads both the
  description cell and the whole line, always.
- The first fix for `N30` then ate real transactions whose payee began with a
  month abbreviation (`MAYFAIR CLOSING BALANCE`). Narrowed.
- `N38` — a stray date range on the page could move the inferred year on every
  transaction. A span is no longer claimed unless it was actually found.
- Balances could be paired across two different dormant accounts in one bundle.

**The toolkit**

- `N29` — a duplicate output id aborted Shiny's bind pass for the whole modal, so
  on a PDF not one control the toolkit drew statically was live. Fixed, and the
  id collision is now a test.
- `N34`, `N35` — the preview says how much it read, and the first question the
  builder asks is one the person in front of it can answer.
- `N28` — the escape hatch is reachable from the screens that need it.
- A correction to a shipped template is saved as `<id>_custom` carrying a
  `refines:` line, and detection honours it. Before this, a correction tied with
  the template it was correcting and lost.

**Cases, bundles and periods**

- Batch conversion: select a whole case folder, one row per file, click a row for
  that file's full result.
- Multi-period reconciliation, and `N39`/`N40`: auto-split is no longer opt-in
  (one of thirteen templates ever opted in), and an unfinished seed template can
  no longer join detection.
- `N33` — a page footer is no longer folded into the transaction above it.

**Wording and screen**

- Every check label now claims only what its check proves. "Every row was read"
  became "No row failed to read"; "Row count matches the statement" became "Row
  count"; "Redactions found and honoured" became "Redactions found".
- A check that could not run says "could not be checked" rather than borrowing
  the field-coverage wording "not on this statement", which meant something else
  on the same screen.
- A tie between two templates converts and holds for review instead of demanding
  a pick from a screen that had no picker on it.
- Admin's `user_template_ids` reactive shadowed the engine function of the same
  name, so the template origin line printed an R error and Hide and Delete did
  nothing, silently. Removed.

**For the maintainer**

- Admin refuses to open at all while the admin password is the shipped
  placeholder.
- A safe, shapes-only audit any statement or folder can be described with, and
  the no-PII AI survey prompt
  ([`docs/operational/survey-a-statement-with-ai.md`](docs/operational/survey-a-statement-with-ai.md))
  that produces the same description without the file leaving the room.
- The docs were culled: the point-in-time audits, the research briefings and the
  superseded plans are gone from `docs/`, and git history keeps them. Two new
  tests fail the suite if a doc link dangles or a page is orphaned from its index.

**Suite:** 725 tests, 3,366 passing assertions · 0 errors · 0 skipped (from 2,880
at 1.1.0). Register: 109 findings, 104 fixed, 4 open, 1 not-a-defect.

**Known and deliberate**

- `app.R` is ~4,600 lines and not yet split. What makes it hard is ~3,800 lines of
  interleaved reactives in one scope; a de-risked plan is in the register,
  including the test-helper change that must go first (60 tests grep `app.R` by
  physical line offset).
- Two older findings wait on **evidence, not effort**: a spread of real forms to
  tune the form fingerprint against, and one hand-keyed golden scan to measure OCR
  digit accuracy. Both fail closed and loudly today.
- Two mechanisms answer "one file, several periods" and both are needed — the
  bundle split (several separately-issued documents) and the span merge (one
  document, several sections). The register records why deleting either is wrong.

## 1.1.0

**Correctness**

- A tie between two templates was broken by template id, alphabetically, so a
  hand-built `aaa_bank` beat a tested `westpac_everyday_pdf` on nothing but its
  name. Shipped templates now win an equal score.
- `%y` / `%Y` year truncation could read a four-digit-year export into 2020.
- `type_dc` sign inversion in drafted templates: a bank marking debits `DR` or
  `Debit` had every debit read as a credit.
- Delimited and Excel statement metadata is threaded into the header, so the
  balance, count and period checks can run on those formats at all.
- A re-convert that flips accepted → withheld now withdraws the previously
  accepted rows from the feed.

**Privacy and governance**

- Destructive Admin actions are authorised server-side; visibility is not
  authorisation in Shiny.
- Local metadata capture: levels, per-category switches, account numbers stored
  only as a one-way hash, never in the Qlik feed.

**Suite:** 2,880 passing · 0 failed · 0 skipped.

## 1.0.0

First production build: the engine, the shipped templates, the governed Qlik
feed, the audit trail and the offline installer, as described in
[`docs/context/charter.md`](docs/context/charter.md).
