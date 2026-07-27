# Changelog

What changed, in the order it matters. Newest release first.

Written for whoever inherits this: each entry says what it does for the person
using the tool, and — where it matters — what it stops going wrong. The finding
ids (`#3`, `N20`, …) point into [`docs/context/findings-register.md`](docs/context/findings-register.md),
which carries the evidence.

---

## 1.2.0

Eighteen findings, all but two raised from **real use on real statements**. Read
the first section: five of them put wrong figures on screen, and two of those
were regressions introduced during this same release and caught by reviewing it.

### Wrong figures — the ones to read

- **A credit was read as a debit, on every credit.** Credit-card statements print
  the amount on one line and its `CR` on the next. That marker is the *sign* of
  the amount above it, but the line carries no date and no digits, so it looked
  exactly like a wrapped description: the `CR` was folded into the payee, the
  amount cell never saw it, and every payment and refund came out with the charge
  sign. No error, no flag, a complete set of figures with the wrong sign — only a
  balance reconciliation could have noticed, and only if the statement prints
  balances. The marker is now appended to the money cell it sits under, so the
  amount is read as `150.00 CR` exactly as if it had been printed on one line.
  (`N37`)
- **A printed balance was kept as a transaction.** An opening or closing balance is
  not a transaction; when one was kept the output did not lose data, it *gained* a
  transaction that never happened, at the balance's own material amount — reported
  from use as *"Balance −9000 debit"*. The guard read the description cell and fell
  back to the whole line only when that cell was empty; a displaced band makes the
  cell non-empty but *wrong*, so the fallback never ran. It reads both, always.
  (`N30`)
- **The fix for that then dropped real transactions.** Reducing a line to its label
  stripped every number, marker and month-ish word *anywhere* on it — so any payee
  merely beginning with a month abbreviation was eaten. `MAYFAIR CLOSING BALANCE`
  became "closing balance"; `MARKET TOTAL PAYMENTS`, `JUNCTION TOTAL DEPOSITS` the
  same. Each was silently skipped, and bucketed non-actionable so the drop never
  reached the completeness measure. Only the *leading* date and *trailing* money
  are removed now — what is in the middle is what the line says. 23 cases are
  pinned in both directions.
- **A stray date range moved the year on every transaction.** A statement covering
  several periods is read as one span, and the first version widened the *dates*
  even when it had refused to pair the balances, on the theory that the earliest
  start and latest end are printed facts. What is printed is a date *range*, and
  statements print plenty that are not statement periods — a fixed-rate loan
  window, a tax year. The reader takes its year context from those two values, so
  a January statement beside a 2024–2026 loan term resolved every year-less date
  to **2024**, and the "all dates fall in the period" check read the same widened
  period and *passed*. Two outcomes now, not three: proven and everything moves
  together, or nothing moves and the reason is said out loud.
- **Balances could be paired across two different accounts.** Two dormant accounts
  both opening and closing on `0.00` chain perfectly, so the money proof believed
  it and stayed silent. If the sections name more than one account, it stops.

### The toolkit was not connected

- **A duplicate output id unbound the whole toolkit on PDFs.** `uiOutput("g_more_toggle")`
  was rendered twice. Two outputs sharing an id make the browser throw *"Duplicate
  binding for ID"*, and that aborts Shiny's entire bind pass for the modal — so on
  a PDF **not one** statically drawn control reached the server: the bank name, the
  date format, the amount style, the page number, "Assign it", "Remove it". Boxes
  drawn did nothing and the bands stayed at their drafted defaults, which is
  exactly the reported *"every second page has the default box assignments"* and
  *"I've drawn the box over the credit column and it reads nothing"*. Proven in the
  browser: typing `ANZ` into the bank box left the server holding the old value.
  A test now fails on any repeated output **or input** id. (`N29`)
- **The coordinate space is a contract, not a comment.** The parser was measured
  *innocent* here — across every real statement, no page differs in size from any
  other in its file, and a page rescaled 4.55× keeps an identical row count. The
  risk was on the drawing side, so the frame is now two callable, tested functions
  replacing four hand-rolled copies of the rule, one of which computed the ratio in
  the opposite direction.
- **The band editor draws on mouse release**, not during the drag, so small
  positional corrections are possible. (`N26`)
- **The save name is built from the bank *and* the kind of statement**, so two
  layouts from one bank no longer collide. It stops following the moment you type
  your own. (`N27`)
- **"None of these fit? Tell our team" is in front**, not inside the settings
  disclosure — it is the escape hatch for somebody already stuck, and the tool was
  making them hunt for it. (`N28`)
- **The form builder leads with the document.** Five steps, the concrete one first:
  draw a box round a value · what is this · repeat · check what comes out · save.
  The blank "type the values you want, one per line" box that used to open the
  screen is now optional and behind a disclosure. It also answers the question it
  used to ask: it reads what is in the box and the wording printed beside it, and
  decides whether to find the value *by its wording* (portable to the next
  document) or by *pinning the box*. (`N31`)

### A whole case, and statements that span several periods

- **Convert takes a folder.** The same picker takes one file or fifty: one row per
  file — status, bank, rows, and the one thing to check — sortable, worst-first,
  and clicking a row opens that file's *ordinary* result page. Nothing new happens
  per file; it is a loop over the same conversion, so a batch answer and a
  single-file answer cannot disagree. A file that cannot be read never stops the
  rest, and the QID is asked once per batch. (`N23`)
- **Several periods in one statement reconcile as one span** — earliest opening +
  every transaction = latest closing — but only when four things prove the pairing:
  the periods read as dates, they do not overlap, each section prints exactly one
  opening and one closing, and **each period closes on the figure the next one
  opens on**. A shared boundary day is not an overlap: banks routinely print the
  previous closing date as the next opening date, and rejecting that rejected the
  commonest real multi-period statement there is.
- **Several statements in one file are still split, not merged** — verified on a
  real 3-statement bundle, where the per-statement balances chain 95.22 → 37.88 →
  11.66 and each reconciles on its own anchors.

### Smaller, all from use

- **A page footer was being folded into the previous transaction's description**,
  so `"To 63A Rent Rent 63A Sar"` came out as `"To 63A Rent Rent 63A Sar Carried
  Forward to next page"`. No figure was wrong, which is exactly why nothing caught
  it — every check is about money and dates, none reads the description. (`N33`)
- **A year-less date format read nothing** when the statement printed no period the
  tool could parse and more than one year appeared on the page — which a credit
  card always does (payment due, card expiry, a copyright line). The labelled
  statement date has been in the dictionary since the beginning and **nothing read
  it**; it is now a year source, ahead of scanning the page for numbers. Reading a
  labelled fact is not a guess. (`N36`)
- **Converting is unmissable.** The status was a small panel in a screen corner, so
  people did not notice a run and clicked around during it. It is centred, large,
  and dims the page — one rule covering all five places the app reports progress.
  An ordinary warning still goes quietly to the corner. (`N32`)
- **The preview says how much it read.** It stops at the first few pages so the
  toolkit stays quick on a long statement, then announced that partial count as
  *"N transaction rows read"* — so the number an analyst checked her template
  against was not the number the conversion then gave her, with nothing saying why.
  It now reads *"N rows read from the first 3 of 11 pages"*, and only says that
  when the document really is longer than the preview. (`N34`)
- **The first question can be answered again.** *"A statement, or labelled
  values?"* is asked once on the page behind the toolkit — and not at all when the
  toolkit is opened from a result — so from inside it there was no way back, and
  Cancel threw the work away without answering it. **Not a transaction table?**
  now sits at the top of the toolkit and carries the same document across to the
  form builder, with nothing re-uploaded. The old wording pointed at a control
  *"above"*, which the modal is covering. (`N35`)
- **Choosing a bank narrows the template list**, the label is just *Template
  (optional)*, JSON is a link rather than a third equal button, a layout can carry
  the dates it applies between, and the dashboard is no longer mentioned to people
  who cannot see one.

### For the maintainer

- **Two duplicate input ids were live** — one button existed three times, two of
  which render together. Two buttons sharing an id keep independent click counters,
  so one sends a value identical to the current one and its handler never fires: a
  dead button, invisible from R and green in the suite.
- **A global CSS defect**: the table-header rule uses the `background` shorthand
  with `!important`, which resets `background-repeat`, so the sort arrow *tiled*
  across the header. Every table in the app was affected.
- **A test that passes with the guard removed is worse than no test.** The new
  summary-line invariant was mutation-tested and found *vacuous* — deleting the
  guard did not trip it — so it carries a case with teeth as well as the golden net.

**Suite:** 3,101 passing · 0 failed · 0 errors · 0 warnings · 0 skipped
(from 2,880 at 1.1.0). Register: 99 findings, 96 fixed, 3 open —
[`docs/context/findings-register.md`](docs/context/findings-register.md).

### Known and deliberate

- **`app.R` is 4,661 lines and still not split.** The case is now stronger, not
  weaker: the duplicate-id bug above was findable only by reading two distant
  regions of one file at once, which is the failure mode a split exists to prevent.
  The cheapest cut — moving the CSS to `www/` — is 265 lines and moves no concepts.
- **Three older findings wait on evidence, not effort** — a spread of real forms to
  tune the form fingerprint against, and one hand-keyed golden scan to measure OCR
  digit accuracy. Both fail closed and loudly today.
- **Two mechanisms now answer "one file, several periods"** — the span merge and the
  bundle split, mutually excluded by a page-marker count. One of them is the right
  answer and the other should go; that is the largest single simplification
  available in the engine.

---

## 1.1.0

### Correctness — figures that could have been wrong

These are the ones to read. Each was a route to a wrong number that nothing on
screen would have questioned.

- **A template's filename decided which figures reached the dashboards.** When two
  templates fitted a statement equally well, the winner was chosen by template id,
  alphabetically — so a hand-built `aaa_bank` beat a tested `westpac_everyday_pdf`
  on nothing but its name. Shipped templates now win an equal score; they have a
  golden test proving they read that bank correctly, and hand-built ones do not.
- **Reading 5 rows of a 500-row statement passed every check.** The row-count check
  requires only "more than zero" when a statement prints no stated count, and the
  completeness check returned "cannot be proved" for every PDF. A conversion could
  come back clean, feed the dashboards, and be missing 99% of the money. The tool
  now counts rows that *looked like transactions and could not be read* — the date
  wouldn't parse, or there was no amount in the money columns — and fails when more
  were missed than captured. Headings, notes, summary lines and wrapped text are
  excluded: most statements skip more rows than they keep, and that is normal.
  (`N20`)
- **"It matched, but there are no transactions" was an amber warning.** A template
  whose wording fits but whose columns are in the wrong place produced an empty
  workbook labelled *needs review*. It now reports plainly that the template read
  nothing, names it, and points at fixing that template rather than building
  another one.
- **A correction of a shipped template could never take effect.** Opening a shipped
  template to fix a moved column band saved it as `<id>_custom` with the same
  identifying phrase — so the two tied on every statement of that bank, and the
  tie-break preferred the tested one. The fix now records `refines: <id>`, and a
  correction outranks the single template it names. A hand-built *rival* still
  loses to the tested one. (`N22`)
- **Every conversion was about to become untraceable.** This deployment has no
  sign-in, so identity fell through to the account the server process runs as —
  the same for everyone. A QID is now required before the first conversion of a
  session, and the conversion is refused without one. Behind a real sign-in
  (Shiny host auth or an SSO header) nothing is asked at all.

### Privacy

- **The form path polices itself now.** A labelled value has no reconciliation to
  prove it — it is trusted because it was read — so an empty or contradictory read
  has to say so itself, and it did not. Every outcome reported `ok`: nothing found,
  the same label twice with different values, even a transaction statement fed to
  it. The count in the message was of fields the template *declares*, not values
  found, so a document where nothing was read still said "4 field(s) extracted".
  Zero values is now `unsupported` and names the template that matched but read
  nothing; conflicting values are `needs_review` with the conflicts named. The
  shipped ANZ sample turns out to print three of its labels twice with different
  values — it now says so instead of silently taking the first.
- **A negative kept its sign.** The money pattern allowed a minus only *inside* the
  currency symbol (`$-577.80`), so the two forms banks actually print — a minus
  before the symbol (`-$577.80`) and a trailing minus (`577.80-`) — matched without
  it, and a deduction was extracted as a positive. The ANZ KiwiSaver sample shipped
  in this repo prints `-$577.80` for tax and `-$489.22` for fees, so the one shipped
  form template was reading both with the wrong sign. The minus is also not always
  an ASCII hyphen: PDF typesetting emits U+2212 MINUS SIGN and financial documents
  use U+2013 EN DASH, and neither matched — found by generating test PDFs, since
  R's own `pdf()` device emits U+2212 for a plain "-", exactly as a typesetter does.

### Privacy

- **Client statement imagery no longer accumulates on disk.** Under memory pressure
  ImageMagick writes a page's raw pixels to a temporary file (~83 MB for one 300 dpi
  A4 page) and clears them only when the process exits — on a server running for
  months, never. Each piece of image work now gets a private folder that is deleted
  on the way out. (`N17`)
- **The screen no longer describes the tool's weaknesses.** The identity field
  explained itself with *"There is no sign-in on this server"* — a description of
  where the door is unlocked, shown to everyone who opens the page. A test now
  blocks that class of wording across the app, the label maps and every engine
  message.
- **The Admin tab is hidden unless the URL carries `?admin`.** Nobody converting a
  statement has the password, so a tab they cannot open was a reference to Admin on
  every screen and an invitation to try one. The maintainer bookmarks
  `http://your-server:8100/?admin`. This is not the lock — the password is, and
  every admin action is still checked server-side, so a hand-typed `?admin` reaches
  a login form and nothing else.
- **A QID is validated: six letters or numbers, stored uppercase.** Free text meant
  a typo or a name could be recorded against a conversion — a record nobody can
  follow back to a person, which is the same as having none.
- **An unguarded observer was found and closed.** Tightening the invariant test to
  read whole observer bodies (instead of a fixed nine lines) immediately caught one:
  the template-request queue was read off disk and its ids pushed into every
  session, admin or not, because a bare `observe()` is not suspended when its tab
  is hidden.
- **Admin is not named anywhere a user can read.** Nobody using the app has the
  password, so the About card advertising it, an error that said "tidy them up in
  Admin → Templates", and "ask your administrator" were all directions to a door
  that will not open.

### The screen

One rule now governs it, recorded in [`docs/context/charter.md`](docs/context/charter.md):

> **Never ask a question the tool can answer. Never ask a question the person in
> front of you can't.**

- **The result page shows the verdict, the download, the figures and the
  transactions — and nothing else by default.** Measured before: one result could
  show 19 blocks and 41 distinct sentences behind 19 controls, of which about five
  were questions an accountant could actually answer. Everything technical — the
  X-ray, the chart, every check, every diagnostic, the template controls — sits
  behind **"Show me how it read this"**. Nothing was deleted. **The click is
  remembered for the session**, so someone technical opens it once and never sees
  the button again; nobody is asked whether they are an advanced user.
- **The checks that matter are on screen, pass or fail.** Only FAILING checks were
  shown, which reads as "no news is good news" — the wrong way round for a tool
  whose output has to be defensible. The reason to trust a conversion is that the
  opening balance plus every transaction equals the closing balance the statement
  prints, and a *passing* proof said nothing at all. Four now sit under the figures:
  does it add up, was every row read, does the running balance follow, could every
  date be read. A dash means the check could not run on this statement — a fact
  worth seeing, not a pass.
- **The date format and the amount style stay in front.** They were briefly moved
  behind the toolkit's disclosure with everything else, on the theory that the
  drafter fills them in. It does - and it is wrong often enough that these are the
  two settings an analyst reports changing on almost every template she builds.
  Frequency beats how technical a control looks: hiding what is needed nearly every
  time puts a click on the *most* common path, not the rarest. The controls that
  exist only because of the amount style (what a bare number means, what the D/C
  indicator values are) sit beside it, so choosing "a D/C column" no longer shows
  nowhere to say what D means.
- **Setting up a bank is four steps stated at the top of the toolkit**: drag a box,
  say what it is, name the bank, save. One dropdown answers both "which column is
  this" and "pin this header value". Date format, amount style, the identifying
  phrase and the save name sit behind **"Show the settings for this statement"** —
  filled in from the file already, changed only if the preview looks wrong.
  Advanced and the raw YAML are unchanged, one click away.
- **A tie no longer stops a conversion.** Two templates fitting the same wording is
  a question only a maintainer can answer — an accountant cannot know which internal
  variant a bank printed with. The tested one is used, the run is held for review,
  the tie is named, and it is raised for whoever can retire the duplicate.
- **The "include templates built here" tick-box is gone.** Whether a colleague's
  template counts is a deployment setting (`app.user_templates_default`), not a
  per-conversion decision. It only ever produced the puzzle *"I built this template
  and it doesn't work"*. What reaches the Qlik dashboards is gated separately and
  is unchanged.
- **Verdict wording lost its hedging.** *"Medium confidence: it read cleanly and
  every check that could run passed, but one thing about this statement could not be
  fully proven — it is named below. Worth a quick eyeball against the statement."*
  is now *"Read cleanly. One thing could not be proven — it is named just below."*
  Engine detail — template ids, scores, missing phrases — moved off the verdict card
  into the evidence view, unchanged and still exact.

### For the maintainer

- **A no-PII prompt for describing an awkward statement.**
  [`docs/operational/survey-a-statement-with-ai.md`](docs/operational/survey-a-statement-with-ai.md)
  — attach a statement to Copilot, paste the prompt, and get back a structured
  description of the layout with no client detail in it: fingerprint candidates and
  how distinctive they really are, column order and alignment, date and amount
  styles, the exact wording of every balance and total line, and the three things
  most likely to make an automated reader wrong without noticing. A batch of these
  is what unblocks the remaining open findings.
- **Decisions are made on codes, not on English.** The completeness check used to
  decide by pattern-matching the display sentence *"the date didn't parse — usually
  the date format in the template is wrong"*. Rewording that sentence — which this
  codebase does constantly — would have switched the check off silently. The engine
  decides on `.PDF_ACTIONABLE_CODES`; the sentence is a lookup, free to reword.
- **Deletions.** Three exported detection helpers that each re-derived the same
  answer, one duplicated diagnostic category, one dead function, and the
  try-another-template rescue loop with its parameter, its field and its message.
  A sweep of all 344 functions in the engine and the app found exactly one with no
  caller, and no helper defined twice.
- **A one-page audit of the interface rule** and the reasoning behind each open
  finding is in [`docs/context/findings-register.md`](docs/context/findings-register.md)
  (84 findings, 81 fixed, 3 open).

### Known and deliberate

- **`app.R` is ~3,900 lines and has not been split.** A split was attempted and
  stopped: several tests read `app.R` as text, and a split that *passed* would have
  silently blinded the admin-authorisation guard — a test that no longer checks
  anything is worse than no test. Three small unblocking edits are documented in the
  findings register. This is the largest remaining maintainability risk.
- **`R/forms.R` + `R/extract_fields.R`** are a second template engine serving one
  shipped form template, kept for the IRD-document work they were built for.
- **Two open findings wait on evidence, not effort:** the form fingerprint gate
  needs more than one real form to tune against, and scanned-digit accuracy needs
  one hand-keyed golden scan to measure against. Both fail closed and loudly today.

**Suite:** 2,528 passing · 0 failed · 0 errors · 0 warnings · 0 skipped.

---

## 1.0.0

First production build. The engine, the templates, the governed Qlik feed, the
audit trail and the offline installer, as described in
[`docs/context/charter.md`](docs/context/charter.md).
