# Bank Statement OCR Platform - Discovery Log

> Living requirements & decisions record. Seeded during scoping interviews and
> updated as decisions are confirmed. This file is the source of truth for
> *why* the platform is built the way it is.

_Last updated: 2026-07-20_

---

## 1. Purpose & background

Replace a brittle, per-statement custom-coded conversion process (built with
Qlik ODAG + Inphinity Mole, requiring exact-location/phrase/template code per
statement) with a maintainable, template-driven engine written in **pure R**
(no reticulate, no Python).

The platform is **"the meat in the sandwich"**: a bank/other statement comes in,
it is matched to a template (or flagged as unsupported), and it is converted
into clean, structured, downloadable data. It must outlive Qlik - the future
front-end/analytics tool is unknown, so the engine must be portable and
front-end agnostic.

### Hard constraints
- **Language:** R only. No `reticulate`, no Python, ever.
- **Attribution:** everything is authored as *Michael Fry*. No mention of AI,
  automated assistants, or any such tooling anywhere in the code, comments,
  docs, or any other artifact.
- **Environment:** cannot self-host a Shiny app for users outside the immediate
  team. Core capability must therefore be usable when plugged into whatever
  analytics tool is adopted.

---

## 2. Confirmed decisions - Bank 1 (Foundations)

### 2.1 Primary users & purpose
- Primary users are **forensic accountants**.
- They **download the structured data** and perform analysis in their own
  tools - they do **not** primarily analyse inside this application.
- Implication: the deliverable is **faithful, audit-grade extraction** -
  completeness (no silently dropped transactions), fidelity, and
  **provenance** (traceability of each row back to its source page/row) are
  paramount. In-app analytics are secondary.

### 2.2 Runtime & delivery (dual-purpose, engine-first)
- **Core = a headless, portable R engine** (an R package) producing generic,
  well-documented outputs that *any* analytics tool can ingest.
- A **Shiny app** is a valued front-end **for the internal team** (upload,
  template wizard, review/QA, download) - but it is a thin layer, not the
  product, because it cannot be published to external users.
- The engine must not depend on Shiny to function.

### 2.3 Inputs (the real-world mess)
- Document types: **bank statements, Visa/Mastercard (credit card) statements,
  and other bank-issued statements.**
- Formats: **PDF and Excel.**
- PDF text: a single PDF may contain **both selectable and non-selectable
  text** → OCR fallback is a **day-one** requirement, not a later phase.
- **Redactions** are a first-class concern:
  - can appear in **PDF and Excel**;
  - can be at the **start, middle, or end**, in **any volume**, in
    **inconsistent** places;
  - the engine must **tolerate and flag** them and **must never crash**
    because of them.
- High format variance: different banks, statement types, card schemes (Visa
  vs Mastercard), and **year-to-year layout variations** - all must be
  supported.

### 2.4 Auto-detection
- Today users **manually select** the statement type before upload.
- Target: **auto-detect** the bank/statement type, and **auto-detect
  unsupported formats** (return a clean "unsupported" status rather than
  failing hard).

### 2.5 Logging / audit (first-class subsystem)
- Every request must be logged: **who requested it, what they input, and the
  outcome/status of the output.**
- Purpose is twofold: **audit** *and* **operational failure-mode analysis**
  (understanding what is failing and why).

### 2.6 Output
- A **stable, documented data contract** (structured schema) that the current
  and any future analytics tool can read.
- Plus **generic file outputs** (e.g. CSV/Excel) for any tool, and consumable
  within the internal Shiny app.

---

## 3. Confirmed decisions - Bank 2 (OCR & parsing engine)

### 3.1 Redactions
- **Never derive or infer redacted values.** They were redacted deliberately;
  output is **as-shown only** (blank / `[REDACTED]` + `redaction_flag`).
- Typical physical form: **black boxes drawn on top of the text** (like a
  highlight/box struck through transactions that shouldn't be there) - but
  **this is not consistent across banks**; detection must be
  heuristic/pluggable and **per-template configurable**, not a single global
  assumption.
- **Critical forensic rule:** a black box *on top of* text frequently leaves
  the original text still present in the PDF text layer. The engine must treat
  a detected redaction overlay as authoritative and **must not extract text
  hidden beneath a redaction region** - otherwise it leaks the very data that
  was redacted.

### 3.2 Text extraction, sections & tables
- **Per page/region text-layer detection**: use `pdftools` (with x/y word
  positions) where text is selectable; fall back to `tesseract` + `magick`
  OCR only for non-selectable regions. OCR-derived values carry lower
  `confidence` + an `ocr_flag`.
- **Section awareness is central.** Statements have relatively consistent
  **start/end markers** (sections, words, phrases). Templates define sections
  by anchor markers, use them both to **classify statement type** and to
  **parse each section with its own rules**.
- **Extract tables as tables** where possible (column-band / positional
  detection), not just flat lines.
- **Environment:** CRAN package installation is available (incl. system libs
  for `pdftools`/`poppler` and `tesseract`), so OCR is viable day one.
- Seed section anchors and a starter template + test corpus from **publicly
  available specimen statements** of the major NZ banks (never real customer
  data).

### 3.3 Provenance / output layout
- Forensic accountants primarily download **clean raw transaction data**.
- **Provenance & traceability (source page, row, raw snippet, `sheet!cell`)
  live in a separate metadata sheet/page**, valuable for our debugging &
  audit - deliberately kept **out of** the core data the accountants consume.

### 3.4 Unified schema
- **One consistent core schema across every statement type** (bank account,
  Visa, Mastercard, other) - standardised key fields are essential for the
  accountants' work.
- Statement-type-specific extras (context, extra columns, header summaries
  like credit limit / min payment) are **separated from the core data** when
  easily produced.

### 3.5 Raw vs normalised & careful formatting
- Output preserves **both** `raw_*` (exactly as shown) **and** normalised
  fields.
- Normalisation applies to **numbers, signs and dates only**.
- **Descriptions are preserved verbatim** - no stripping of apostrophes,
  ampersands, or other special/Unicode characters (e.g. `O'Connor & Sons`
  must survive intact). Be deliberate about what is normalised vs preserved.

---

## 4. Confirmed decisions - Bank 3 (Template system & wizard)

### 4.0 North-star principle: RADICAL SIMPLICITY
The dominant, repeated requirement. Every design choice is judged against it:
- **No machine learning, no complex/clever algorithms, no ambiguity.**
  Deterministic behaviour only - "if it matches, it matches; if it doesn't,
  it doesn't."
- **Maintainable by a single data analyst with little-to-no software
  engineering skill** - adding a new bank/statement or an IRD-type document
  must be easy for that person.
- **Prove it on real data before committing.** Approaches (incl. YAML
  templates) are provisional until validated against real specimen statements;
  keep whatever actually works at a high hit-rate, drop what doesn't.

### 4.1 Template definition
- Plan of record: **declarative templates** (e.g. YAML) describing identity,
  fingerprint anchors, section markers, table column-bands, field formats,
  redaction settings, reconciliation rules - **no per-bank code**, with any
  genuinely odd behaviour handled via a small library of **named, reusable,
  parameterised transforms**.
- **Caveat (Michael):** not yet assumed sufficient. We must gather real
  statements and confirm the declarative approach matches a high proportion of
  them before locking it in. Do what works.

### 4.2 Matching / detection (dead simple, never wrong)
- Keep **user-selectable bank and/or bank+statement type** (as today).
- Produce a **trust / confidence score** telling the user whether the output
  can be relied on.
- Deterministic matching only - **never guess wrong**, even if that means more
  manual selection. Ambiguous / below-confidence → clear `unsupported` /
  `needs_review` status, never a silent wrong match.

### 4.3 Wizard
- Whatever is **simplest on our end and mistake-proof** - the tool must be
  100% foolproof, tested and audited, with maximum confidence.
- Real data required to design and test the wizard properly.
- Family model accepted (bank family → versioned/variant templates), kept as
  simple as the data allows.

### 4.4 Real-data corpus (explicit work item)
- Pull **as many public specimen statements as possible** from NZ banks -
  primary focus the **big six** (ANZ, BNZ, Westpac, Kiwibank, ASB,
  Co-operative), plus Excel + PDF statements from **other** banks (SBS, TSB,
  HSBC, etc.).
- Public specimens only - **never real customer data**. Used to validate
  methods and build the golden-file test set. **Fully tested before go-live.**

### 4.5 Build vs deployment reality
- **Development:** version-controlled, structured project with automated tests.
- **Deployment:** a **plain folder structure on an internal server** with **no
  version control** on that side, inside their environment.
- Therefore the **core is plain callable R functions in a folder**
  (e.g. `convert_statement(file, bank)` → structured data + output files) that
  any analytics tool can call and one analyst can run/maintain. Shiny app and
  any API are **optional convenience wrappers**, never required for the core to
  work.
- Must be simple to build on, maintain, add new statement types to, and later
  extend to non-bank documents (e.g. Inland Revenue).

---

## 5. Confirmed decisions - Bank 4 (Categorisation, reconciliation & trust)

### 5.1 Categorisation is OUT of the MVP
- Not a v1 must-have. Belongs to some downstream targets, not this engine's
  MVP.
- When built later, ingest the **existing keyword CSV lookup already
  maintained by the Auckland analysts** (Michael can supply it) rather than
  designing a new scheme. Leave a clean, unobtrusive hook only - build nothing
  now.

### 5.2 Reconciliation KPIs = the trust signal
- Bake in **as many reliable checks as possible**, surfaced as **clear
  single-line KPIs displayed alongside each statement** (e.g. balance check,
  transaction check, and others).
- The **definitive KPI set is to be derived from the real specimen statements**
  once pulled - only include checks that work **reliably across all banks**.
  Do not pre-guess the list; propose it from the data.
- Mismatches are **flagged, never auto-corrected**.

### 5.3 No manual correction
- **No manual transaction editing, ever.** "If it's wrong, it's wrong" - the
  fix is a template/engine correction + re-run, preserving provenance and
  reproducibility.

### 5.4 Actionable failure reporting
- Never crash. Every failure returns a **clear, actionable reason** - not just
  a status, but *why* it failed and *what it needs to work* (e.g. "statement
  type not supported", "no text layer on p.2 - OCR required", "reconciliation
  off by $X.XX", "redaction over transaction rows 5–9"). All logged for
  failure-mode analysis.

---

## 6. Confirmed decisions - Bank 5 (Consistency, maintainability & governance)

_All recommended defaults accepted._

### 6.1 Deliverable format
- Primary output: an **Excel workbook** - Sheet 1 core transactions (standard
  schema), Sheet 2 statement header/summary, Sheet 3 reconciliation KPIs +
  provenance/metadata, (later) Sheet 4 extras.
- **Plus** a plain **CSV** of the core table (tool-agnostic) and a **JSON**
  (future machine consumption).

### 6.2 Frozen data contract
- One **documented, versioned core schema**, identical for every
  bank/statement, so downstream tools never break. Working draft columns:
  `row_id, source_page, date_raw, date, description (verbatim), amount_raw,
  amount, debit, credit, balance, currency, direction, section, flags`.
- Final field list to be confirmed against the real specimens (guarantee the
  fields forensic accountants always need - running balance, unique row id,
  as-shown date string).

### 6.3 Quality gate ("foolproof")
- **Golden-file regression tests:** each specimen has an expected
  output; no change is accepted unless **all banks still pass and
  reconciliation passes**. A template is not "live" until it matches its
  golden file.

### 6.4 Maintainability
- Everything the analyst touches is **plain files** (templates, config), with
  **one documented entrypoint** and a plain-English "add a new statement in N
  steps" guide. No build tooling required on the deployment side.

### 6.5 Logging & security
- **File-based logging** (CSV/JSONL in a logs folder, no database): who
  requested, when, file name + hash, bank selected, status, reason,
  reconciliation result, template version.
- **No raw statement content in logs.** **100% local - no external/cloud
  calls** (rules out cloud OCR). Test fixtures use public specimens only.

### 6.6 Non-bank documents (side quest, post-MVP)
- Same engine + a `doc_type` dimension; IRD and similar docs become **more
  templates in the same folder** using the same section-anchor approach - a
  **config add-on, not a rebuild**. Explicitly **out of MVP scope**, but the
  frozen schema must not box it out.

---

## 7. Items still to confirm with real-world input
These were accepted "as designed" but need Michael's concrete answer before/at
build time; recorded as working assumptions until confirmed:
1. **Deployment server runtime** - is R installed on the release server, able
   to run scripts and install `pdftools` / `tesseract` etc.? (Gates the
   deployment design.) _Assumption: R available; design a self-contained
   folder regardless._
2. **Requester identity** for logs - does the calling tool pass a user/analyst
   id, or is it "whoever runs the script"? _Assumption: accept an optional
   `requested_by` argument, default to system user._
3. **Data-retention rule** - auto-purge uploaded files/outputs after N days?
   _Assumption: configurable retention, default keep-nothing-extra._
4. **#1 IRD document** to target first (post-MVP) so the schema stays
   future-proof. _Assumption: income-summary style doc._
5. **Existing categorisation CSV** structure + rough size (post-MVP) - Michael
   to supply the Auckland analysts' file.

---

## 8. Immediate next actions (post-discovery)
1. Pull as many **public specimen statements** as possible from NZ banks
   (big-six focus + others; PDF **and** Excel).
2. From that real data, **define the definitive reconciliation KPI set and
   finalise the core schema** - proposed back to Michael, not guessed.
3. Validate the **declarative-template approach** actually matches a high
   proportion of real statements before locking it in.
4. Scaffold the **plain-R, folder-deployable engine** with golden-file tests.
- Bank 4 - Categorisation, reconciliation & manual review.
- Bank 5 - Consistency, maintainability, governance, and non-bank docs (IRD
  etc.).

---

## 9. Interview progress
- [x] Bank 1 - Foundations
- [x] Bank 2 - OCR & parsing engine
- [x] Bank 3 - Template system & wizard
- [x] Bank 4 - Categorisation, reconciliation & review
- [x] Bank 5 - Consistency, maintainability & governance
- **Discovery complete.**

---

## 10. V1 build milestone (2026-07-20)
Engine built, adversarially audited and tested in pure R.
- **Delimited (CSV/TSV/TDV) path end-to-end for six banks** - ANZ everyday, ANZ
  credit card, ASB, BNZ, Kiwibank, Westpac - each with a golden-file test.
- **OCR integrated** - system Tesseract + poppler driven from R (no binding, no
  Python) as the no-text-layer fallback in the PDF reader; redactions stay
  honoured (OCR reads only visible pixels).
- PDF reader + forensic redaction guard; reconciliation KPIs + trust score;
  never-crash status model; per-run JSON logging.
- **Adversarial audit: 21 findings, all resolved.** Suite: 16 files / 76 tests
  / **292 assertions, 0 failures**.
- **Deferred (need real data, not code):** per-bank PDF transaction-table
  templates; `.xlsx` templates. Categorisation intentionally out of v1 scope.

See `README.md` and `docs/context/architecture/build-contract.md`.
- [ ] Bank 4 - Categorisation, reconciliation & review
- [ ] Bank 5 - Consistency, maintainability & governance

---

## 11. Feedback + deployment/integration requirements (2026-07-20)
Final requirements captured after the V1 engine milestone.

**Feedback on every conversion - BUILT.**
- Requirement: users must be able to submit feedback on every statement
  converted; at minimum it is added to the logs and flagged.
- Delivered: every conversion now carries a stable `run_id` (content hash +
  timestamp), written to the result and the run log. `submit_feedback(run_id,
  verdict, comment, ...)` appends to `logs/feedback.jsonl` with
  `flagged = verdict != "correct"`; `read_feedback()` reads it back for
  maintenance triage. The Shiny Convert tab shows a rating panel
  (Correct / Minor issues / Wrong + optional comment) after every conversion.
  Verdicts are the forensic user's ground truth - kept, never discarded.

**Concurrency, authorisation, Qlik - PLAN ONLY (design on record, no code).**
Written up in `docs/context/architecture/deployment-integration-plan.md`:
- **Concurrency:** the engine is already a stateless, re-entrant pure function
  of a file. Plan = a bounded pool of stateless workers (plumber REST) or a
  folder-inbox cron watcher; per-run output dirs (`out/<run_id>/`) so filenames
  never collide; append-only JSONL logs are atomic per-line on POSIX, with a
  documented SQLite upgrade path if load ever demands it. One config number caps
  concurrency; OCR optionally single-slotted.
- **Authorisation:** authorise at the corporate gateway (Windows/SAML/Qlik),
  verify AD group membership in the engine shell. Allowed groups are a
  **modifiable OR-list** in `config/auth.yaml` (default `RES_QLIKSENSE_PROD`),
  `fail_closed: true`. Interchangeable checks (LDAP / PowerShell / Qlik-ticket)
  behind one `user_in_allowed_group()` signature. The gateway user flows into
  the existing `requested_by` field, so audit + auth share one identity.
- **Qlik:** submit-in-Qlik / data-back-in-Qlik via a folder inbox↔outbox
  handshake (ODAG-native, recommended first) or a live REST call from the Qlik
  load script. The engine's existing outputs are already Qlik-ready; `run_id`
  ties Qlik reloads back to the run + feedback logs. **Limitation (deliberate):**
  template *creation* stays a visual, team activity in the internal Shiny
  wizards; Qlik is for *selecting a template + converting + giving feedback*.
  Adding a new bank = the analyst, in the wizard, once - then every Qlik user
  gets it for free. Keeps templates consistent for the whole team.

---

## 12. Label dictionary - the "hundreds of wordings" problem (2026-07-20)
Question raised: how do we handle the 100s of different labels/items that appear
in different places, may repeat, may not exist - "Opening balance" vs "starting
balance" vs "balance b/f" vs "balance:" - and does the YAML actually conform to
that level of customisation?

**Honest audit + fix.** Two variability problems, solved two ways:
- **Transaction tables (the core, ~95% of the deliverable): already conformed.**
  Rows have no per-row labels; they map by column header (delimited/excel) or
  x-band (pdf), once, in `columns:`. Names/places/repeat/absent are handled by
  column mapping + the keep-only-date-parseable-rows filter + `null` fields. No
  synonyms involved by design.
- **Single labelled values (opening/closing balance, period, account name, IRD
  fields): did NOT conform.** `extract_metadata.R` hardcoded the English label
  words in R; `extract_fields.R` allowed one label per field. Wording like
  "balance brought forward" was missed - exactly the "no hardcoded bs" violation.

**Built (option: build it fully):**
- `R/labels.R` - a declarative matcher: `any_of` synonyms, `value`
  (money/date/date_range/text/regex), `occurrence` (first/last/all → repeats),
  `where` (page1/last_page/int → places), `required` (exist/not), `on_conflict`
  (disagreeing matches flagged, never guessed). Value read from the label line or
  the next line if the label is a heading. Back-compat with bare-string/`label:`.
- `dictionaries/labels.yaml` - the shared base synonym dictionary; the single
  place a maintainer teaches new wording (a list of phrases, no code).
- `extract_metadata.R` de-hardcoded: opening/closing balance now come from the
  dictionary; period broadened to any two dates joined by a connective.
- `extract_fields.R` routed through the matcher; a field auto-inherits the base
  dictionary entry matching its name (or `dict:`), so "opening_balance"
  understands "balance brought forward" with zero extra config.
- Proven: "Balance brought forward" → opening, "New balance" → closing, with no
  code change. Suite: 28 files / 125 tests / 444 assertions, 0 failures.

Net: adding a bank's odd wording is one line in `dictionaries/labels.yaml`, not a
code change - which makes the engine *more* aligned with "no hardcoded bs,
maintainable by one analyst," not less.

---

## 13. Default vs user templates + guided setup (2026-07-20)
Goal set by Michael: "a 65-year-old non-technical accountant should be able to
custom-create a template for a new statement." Plus: who creates templates?
default vs user? pre-fill from a failed statement?

**Governance - two tiers, decided:**
- **Default templates** (`templates/`): curated by the team, golden-tested,
  loaded `origin: "default"`, must be valid (hard error). The wizards save here.
- **User templates** (`templates_user/`): created by accountants via Guided
  setup, loaded `origin: "user"`. A default ALWAYS wins an id clash (a user
  template can't shadow a blessed one); an invalid user template is skipped with
  a warning, never fatal, so one bad file can't break everyone. Origin is logged
  (`template_origin`) so the Admin panel can see user templates in use.

**Guided setup (the radical-ease flow):** when a statement is `unsupported`, the
file is ALREADY uploaded in Convert, so a "🪄 Set up this statement (guided)"
button appears. It calls `draft_template()` (auto-detect delimiter/date/amount/
columns for delimited; `suggest_pdf_columns()` for PDF), shows a live PREVIEW of
the extracted rows, asks at most two plain-language questions (dates? amounts?)
pre-answered, and Saves to `templates_user/`. Confirm-and-save, not build.

**Real-data win:** testing on real Westpac/ASB revealed the period is often given
as LABELLED dates ("Statement Opening date … / Closing date …") not "from X to
Y", so year-less dates ("15 Jun") had no year and parsed to 0 rows. Fixed
generically via the label dictionary (`statement_start`/`statement_end`) - Westpac
went from unsupported to 11 rows. Honest boundary recorded: guided auto-draft is
excellent for delimited + clean PDFs; complex/near-empty PDFs (e.g. ASB, one
transaction) still need the PDF wizard to fine-tune boxes.

Suite: 33 files / 145 tests / 522 assertions, 0 failures.
