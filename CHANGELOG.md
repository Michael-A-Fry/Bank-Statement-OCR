# Changelog

What changed, in the order it matters. Newest release first.

Written for whoever inherits this: each entry says what it does for the person
using the tool, and — where it matters — what it stops going wrong. The finding
ids (`#3`, `N20`, …) point into [`docs/context/findings-register.md`](docs/context/findings-register.md),
which carries the evidence.

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
