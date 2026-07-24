# Launch audit - is it ready, what's missing, and the honest boundaries

A straight audit against the three things that matter: **usability, simplicity,
maintenance** - plus your specific questions on **drift** and **missing data**,
and a **go/no-go** at the end. No spin.

---

## 1. What it does today (verified, tested)
- **Reads** CSV/TSV, Excel, and PDF (text + scanned via OCR); redactions honoured.
- **Detects** the bank/layout from a swappable YAML template (zero bank code).
- **Extracts** a stable 16-column transaction table, verbatim descriptions,
  raw + normalised fields.
- **Reconciles** (balance, running balance, dates-in-period, count, completeness)
  and returns a trust level + a fail-loud diagnostic (where/why/how-to-fix).
- **Reports** field coverage ("what's present / empty / not on this statement").
- **Self-service templates:** Guided setup pre-fills a template from a failed
  file; wizards for power users; default vs user templates with precedence.
- **Admin insights:** status mix, unsupported-layout clusters ("build these
  next"), template usage, **drift**, flagged feedback, batch intake, log rollup.
- **Outputs:** multi-sheet Excel + CSV + JSON. **Logs:** one JSON file per run +
  per-feedback, concurrency-safe.
- **Test suite:** 58 files / 343 tests / 1,430 assertions, 0 failures.

## 2. Usability - B+
- **Great:** the live PREVIEW everywhere (Convert, guided, wizard, batch) - you
  always see what will be extracted before trusting it. Plain-language settings
  ("how are amounts shown?"). Guided setup for the common case is genuinely
  one-confirmation. Diagnostics tell you exactly what to fix.
- **Rough edges:** drawing/nudging PDF column boxes is still the hardest task for
  a non-technical user; complex PDFs sometimes need it. Mitigated by auto-draft +
  the coverage report flagging what's empty, but not zero-effort on every PDF.

## 3. Simplicity - A
- Engine is pure R, deterministic, no ML, no database, no build step.
- A bank = one YAML file. Config is YAML; logs are JSON; both open in Notepad.
- Deploy = unzip a folder. Maintain = edit a list of phrases / draw a box.
- Risk to watch: the app (`app.R`) is now large. It's still one file a person can
  read, but it's the piece most likely to intimidate. The *engine* stays small
  and clean, which is what matters for correctness.

## 4. Maintenance - A-
- One analyst can run it: add banks via wizard/guided, watch Admin, tidy logs.
- Every template should get a golden test (documented); the curated set has them.
- No external services to keep alive except the one R process (Option A) or the
  scheduled inbox job (Option B).

---

## 5. DRIFT - when a statement subtly changes, when is it caught?
**Answer: automatically, at the next conversion, through reconciliation - and
surfaced in Admin.**

- If a bank moves/renames a field so a value lands in the wrong column, the
  **balance stops reconciling** (or the running balance breaks). That run is
  logged `needs_review` with a `low` trust and a diagnostic naming the broken
  check. The user sees it immediately on that conversion.
- Across many runs, `template_drift()` (Admin → "Drift") flags a template whose
  **recent health dropped** vs its earlier baseline - i.e. it used to produce
  clean `ok` runs and now produces review/low-trust ones. That's your early
  warning that "template X is starting to fail - the bank changed something."
- **Caveat (important):** drift is only auto-caught when there's something to
  reconcile against (a balance, running balance, or stated count). If a field
  changes on a statement that has none of those, see §6 - the tool flags
  "completeness unverified" rather than silently passing.

## 6. MISSING DATA - when does it work, when does it flag?
Deterministic rules, no guessing:
- **A field is blank on some rows** (e.g. no reference on cash rows): fine -
  extracted as blank, shown as `partial` in the coverage report. No flag.
- **A field the template maps is blank on EVERY row**: coverage shows
  `present-but-empty` - the signal that a column/box is probably wrong. Visible,
  not fatal.
- **A whole optional column isn't on the statement** (no balance column): mapped
  as `null`; coverage shows `unmapped`. Fine by design.
- **A value is under a redaction**: emitted as `[REDACTED]`, flagged, never
  guessed.
- **A row can't be parsed** (bad date/amount): kept and flagged (`row_parse` /
  `amount_parse` diagnostic), never silently dropped for delimited input.
- **The engine can't prove completeness** (no balance / running balance / stated
  count - e.g. some Westpac PDFs): **completeness guard** fires. The run is never
  rated `high`, and a `completeness_unverified` diagnostic says: "can't confirm
  every transaction was captured - check the row count, or use a CSV/Excel
  export." This is the one place "missing data" could hide, and it is called out
  loudly instead of hidden.

---

## 7. Known boundaries (be honest with users on day one)
1. **CSV/Excel exports are the gold path.** They carry clean columns and usually a
   balance → full reconciliation → provably correct. Tell forensic users to
   prefer them when the bank offers them.
2. **Clean single-account PDFs** (tutorial-style, ANZ everyday) auto-draft and
   reconcile well.
3. **Complex PDFs** (multi-column with type codes, balance shown only at day
   boundaries, layout that varies between accounts - e.g. Westpac) are
   **best-effort**: auto-draft gets a starting point, a person confirms boxes in
   the wizard, and because balance is boundary-only the **completeness guard**
   does the protecting. Do NOT treat a PDF parse with no reconciliation as
   audit-final without eyeballing the row count.
4. **Merged multi-statement bundles** are detected and flagged, not parsed - split
   into one statement per file (the tool tells the user).
5. **OCR'd scans** can have character errors on low-quality images; flagged with
   confidence, reconciliation still guards the totals.

## 8. What's missing / nice-to-have (not blockers)
- Auto-draft that opens straight into the PDF wizard with boxes pre-drawn (today
  it hands you the draft + suggested boxes to confirm).
- Header-anchored PDF column detection for messier layouts (attempted; reverted
  as it regressed clean statements - needs more care).
- Golden tests for user-contributed templates (process documented; encourage it).
- AD-group check *inside* the app as a friendly gate (folder permission already
  enforces it; this is cosmetic).

## 9. Go / No-Go
**GO to launch - with a scoped message.** Ship it for:
- **CSV / Excel exports and clean PDFs** → trust it (reconciles).
- **Complex PDFs** → use it, but as *best-effort*: check the coverage report and
  the completeness warning, confirm the row count. Never treat an unreconciled
  parse as final.

The forensic-safety net (fail-loud diagnostics, trust levels, the completeness
guard, redaction handling) means the tool **won't silently give a wrong answer**
- its failure mode is "flagged for review", which is exactly right for auditors.
That's what makes it launchable tomorrow: not that it parses everything, but that
it's honest about what it couldn't.

**Launch checklist:** deploy per [`../operational/first-time-setup.md`](../operational/first-time-setup.md) →
convert 3–5 real statements you
know the answers to → confirm the reconciliation matches → point the AD group at
the folder → tell users "prefer CSV/Excel; on PDFs, check the coverage +
completeness note."
