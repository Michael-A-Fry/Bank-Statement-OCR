# Statement Studio — engine & platform audit

_Method: four independent domain reviews (engine, preprocessing, templates & builder, flow / concurrency / feed / deployment) cross-checked, each finding re-verified against source. Reviewed 2026-07-22._

## Executive summary

The numeric core is careful, forensic-minded work. Money is compared on a rounded-cent tolerance (`reconcile.R:39,59`), never `==` on floats; redactions are honoured (redacted amounts nulled at `parse.R:162`, hidden rows never fabricated); descriptions are verbatim (`normalise.R:174`); detection uses a sound per-candidate eligibility + unique-best rule with a deterministic radix tie-break (`detect.R:73-90`); xlsx output is made byte-reproducible; and there is a deliberate "completeness UNVERIFIED" guard plus an OCR trust cap (`reconcile.R:216-258`). The absence of a de-duplication step is correct, not a gap — dropping a "duplicate" would violate the no-silent-drops promise.

The risks that matter are a small number of **silently-wrong** paths where a plausible but incorrect number leaves the engine with nothing downstream to catch it:

1. **Date year truncation** (`%y` eats two digits, `%Y` accepts two, `as.Date` never full-consumes) turns a 4-digit-year export into year 2020 on a template that ships today (BNZ everyday CSV). Reproduced live. **P0.**
2. **`type_dc` sign inversion** in builder-drafted templates: the draft never captures `type_debit_value`, it defaults to a case-sensitive `"D"`, so a bank that marks debits `DR`/`Debit` has every debit read as a credit. **P0.**
3. **CSV/Excel reconciliation is a second-class citizen**: statement metadata is computed but never threaded into the header for delimited/excel, so `balance_reconciliation`, `transaction_count` and `dates_within_period` structurally cannot run — this is exactly the backstop that would catch (1) on a balance-less CSV. Flagged independently by three of the four domain reviews. **P1.**
4. **Governance/security**: destructive Admin actions are not authorized server-side (visibility ≠ authorization in Shiny), and a re-convert that flips accept→withhold leaves the previously-accepted rows live on the dashboard. **P1.**

**Launch readiness.** Not ready to ship the two P0s. Both are one-file, base-R fixes with no new dependency and should block launch. The template **builder** is a strong UX shell but is **not yet safe to hand a non-technical analyst unsupervised** — it can save a valid-but-silently-wrong template (type_dc sign, over-generic fingerprints). Multi-statement bundles are correctly *flagged-not-merged* today but the flag is under-powered. Everything else is hardening. Recommended gate: fix the two P0s and the admin-auth guard before launch; the CSV metadata gap and the feed integrity bug in the first patch.

---

## Resolution status (hardening pass, 2026-07-22)

**Every prioritised finding below is RESOLVED** on the `engine-hardening` branch —
each as one commit citing the finding id, each with a regression test (synthetic
fixtures, no PII), the full suite green throughout (1163 → 1286 tests). The
`split:` opt-in for auto-splitting bundles is the one **deferred** item (multi-statement
detection was hardened; auto-split is scoped in the research section below).

| Finding | Status | Commit |
|---|---|---|
| P0-1 date year truncation | ✅ resolved | `2170bf3` |
| P0-2 type_dc sign inversion | ✅ resolved | `cbabdcf` |
| P1-1 CSV/Excel metadata → header | ✅ resolved | `d184676` |
| P1-2 admin server-side authorization | ✅ resolved | `39f3115` |
| P1-3 feed stale rows on accept↔withhold | ✅ resolved | `9229c54` |
| P1-4 digital vector-box un-redaction leak | ✅ resolved | `9b0031b` |
| P2-1 OCR routing (word-box + text health) | ✅ resolved | `3db6f72` |
| P2-2 completeness vs multi-line records | ✅ resolved | `41ff708` |
| P2-3 continuity NA-gap bridge | ✅ resolved | `41ff708` |
| P2-4 multi-statement detection signals | ✅ resolved | `139cced` |
| P2-5 non-UTF-8 delimited transcode | ✅ resolved | `c66c7bd` |
| P2-6 filename_regex out of eligibility | ✅ resolved | `e6727bf` |
| P2-7 generic PDF fingerprints | ✅ resolved | `8a548cc` |
| P2-8 tutorial template out of detection | ✅ resolved | `e6727bf` |
| P2-9 effective_from/to soft signal | ✅ resolved | `e6727bf` |
| P2-10 excel_generic unsigned mis-sign | ✅ resolved | `a5770c2` |
| P2-11 dr_cr_suffix double-sign | ✅ resolved | `9c8d15f` |
| P2-12 non-atomic feed writes | ✅ resolved | `ea5798c` |
| P3-a PDF copyright-year inference | ✅ resolved | `2c1b0ef` |
| P3-b money regex 2-decimals | ✅ resolved | `c8d47aa` |
| P3-c Excel serial-date round() | ✅ resolved | `c8d47aa` |
| P3-d feed manifest per-reconvert | ✅ resolved | `6da8a7f` |
| P3-e `.trust_ok` NA not caught | ✅ resolved | `6da8a7f` |
| P3-f feed local-time non-determinism | ✅ resolved | `6da8a7f` |
| P3-g slug-collision overwrite | ✅ resolved | `590f4c7` |
| P3-h drafted fingerprint pins all headers | ✅ resolved | `590f4c7` |
| P3-i narrow redacted cell blank | ✅ resolved | `2c1b0ef` |
| P3-j OCR confidence image mismatch | ✅ resolved | `2c1b0ef` |
| multi-statement `split:` auto-split | ⏸ deferred | — (detection hardened; see research below) |

Alongside the fixes, this pass also shipped the **local metadata-capture subsystem**
(`c2b5fbd`) — the on-box "ML goldmine" (see [metadata-capture.md](metadata-capture.md))
— and the **project charter** (`7f419c1`, [charter.md](charter.md)).

### Customisation & maintainability platform (this pass, continued)

Once the engine was stable, the follow-on work made every "little thing" the engine
checks for **customisable data, not code**, and pulled scattered magic numbers into
the open — directly serving "keep it simple and maintainable as it scales":

| Item | Status | What |
|---|---|---|
| Recognition **lexicon** (three-tier: template > lexicon > built-in) | ✅ shipped | `R/lexicon.R` + `dictionaries/lexicon.yaml`; every marker/synonym/regex externalised, admin-editable, hot-applied, empty-file = shipped behaviour. See [customisation.md](customisation.md). |
| Deterministic **learning loop** (ML *proposes*, human approves, engine acts) | ✅ scaffolded | `R/suggestions.R` + Admin panel; ranks unrecognised signals from the metadata corpus into lexicon suggestions. |
| **Engine parameters** consolidated | ✅ shipped | `R/params.R` — year window, money tolerance, OCR/redaction thresholds, oversized advisories, each named once with one shared `.plausible_year` / `.tolerant_date`. See [engine-parameters.md](engine-parameters.md). |
| **Template hints** in metadata (draft-a-template signal) | ✅ shipped | `R/column_profile.R` — PII-safe per-source-column profiles + the engine's suggested mapping, so an unmatched statement carries everything an AI/human needs to build a template. |

---

## Findings by priority

### P0 — silently-wrong forensic numbers

---

**P0-1 · Date parsing silently produces a plausible wrong year**
Area: engine / date normalisation · `R/normalise.R:28-38` (consumed at `R/parse.R:59-65`, `R/parse_pdf_table.R`) · CONFIRMED (reproduced) · Effort S · Impact H

`parse_date()` calls `as.Date(s, format=fmt)` with no check that the format consumed the whole cell and no bound on the resulting year. Reproduced against the real engine:

- `parse_date("13/08/2025", "%d/%m/%y")` → `2020-08-13` (year 2020, plausible, wrong)
- `parse_date("13/08/25", "%d/%m/%Y")` → `25-08-13` (year 25 AD)
- `parse_date("13/08/2025 extra", "%d/%m/%Y")` → parses the prefix, drops the junk

The shipped `templates/bnz_everyday_csv.yaml` declares `date: {format: "%d/%m/%y"}` with `balance: null`, and CSV inputs carry no statement period (`extract_metadata` reads `input$pages`, NULL for delimited). So if BNZ (or any `%y`-templated bank) ever exports 4-digit years, **every** date silently becomes 2020: `dates_within_period` is `na` (no period), `dates_readable` passes (2020-08-13 is a valid date), there is no balance to reconcile. **Why it matters:** a forensic date wrong by five years that looks right is the exact silently-wrong outcome the product forbids, and on a balance-less CSV there is zero downstream catch. The engine already defends the *period* parse against this same `as.Date` behaviour (`reconcile.R:94` rejects year < 1990) — the transaction-date path just lacks the same discipline.
**Recommendation:** make `parse_date` strict — after `as.Date`, reformat the parsed date back through the *same* `fmt` and require it to equal the normalised input (a full round-trip / full-consume check), and clamp the year to a sane window (reject < 1990, as `.rec_date` already does). A row that fails becomes `date=NA` flagged `date_unresolved`, surfacing rather than silently trusted. Base R only.

---

**P0-2 · `type_dc` builder drafts flip every debit to a credit when the marker is not `"D"`**
Area: templates / builder · `R/draft.R:82-92` + `R/normalise.R:157-166` + `R/templates.R:91-92` · CONFIRMED · Effort S · Impact H

`detect_amount_style()` returns `"type_dc"` whenever a column is entirely in `{D,C,DR,CR}` (`wizard_detect.R:93-96`). `.draft_delimited` then stamps `amount_sign: type_dc` and maps the `type` column, but **never sets `type_debit_value`**. In `parse_amount` the debit token defaults to `"D"` with an exact, case-sensitive comparison (`normalise.R:163-164`: `tv == debit_val`). If a bank marks debits `DR`/`Debit`/`d`, no row is ever classified as a debit, so **every transaction is read as a credit (positive)**. `validate_template` only requires `columns.type` for `type_dc` (`templates.R:91-92`), not `type_debit_value`, so the broken template passes validation and saves; the Simple builder tab never surfaces the marker.

This is correctly scoped: the two shipped `type_dc` templates set the value by hand (`xero_standard_csv.yaml: type_debit_value: debit`, `anz_creditcard_csv.yaml: type_debit_value: "D"`) — which proves the authors know it is load-bearing. The exposure is the **builder** path, which a non-technical analyst uses unsupervised. **Why it matters:** every debit becomes a credit on a whole statement while the numbers still look right; a `type_dc` CSV without a running balance (common for card/loan exports) produces inverted cash flow with no red flag.
**Recommendation:** in `.draft_delimited`, infer `type_debit_value` from the sample (the token co-occurring with debits) and always emit it; add a Simple-tab control for "which marker means money out"; make `validate_template` require `type_debit_value` for `type_dc`; and make the comparison case-insensitive/trimmed. Never silently default to `"D"`.

---

### P1

**P1-1 · CSV/Excel statement metadata is never wired into the header — reconciliation KPIs and feed columns are structurally dead for the most common inputs**
Area: engine / reconcile / feed · `R/parse.R:175-186` (hardcodes NA) vs `R/parse_pdf_table.R:712-727` (injects it); root cause `R/extract_metadata.R:18` + `R/read_input.R:117-123` · CONFIRMED · corroborated by 3 of 4 reviews · Effort M · Impact H

The PDF parser folds extracted metadata (`opening_balance`, `closing_balance`, `period_start/end`, `stated_count`, `account_number`) into `parsed$header`. The delimited/excel path hardcodes all of them to `NA` and omits `stated_count` entirely. Compounding it, `extract_metadata` reads `input$pages`, which `read_input` leaves NULL for delimited (text is in `input$lines`) and excel (data in `input$table`) — so `meta` is blank for those formats regardless. `convert.R:35` computes `meta` but `reconcile` reads `parsed$header`, so it is never used. Net effect for every CSV/TSV/TXT/XLSX statement: `balance_reconciliation`, `transaction_count` and `dates_within_period` can never fire, and the feed's `period_start`/`period_end`/`account_number` columns (documented as populated) are always blank. **Why it matters:** it creates a two-tier safety net — PDF statements get the wrong-period / wrong-count / balance checks; CSV/Excel silently do not. This is the very backstop that would catch P0-1 on a balance-less CSV. It is not "wrong output" itself, but it removes the strongest completeness proofs for the highest-volume input type and drifts from the documented feed schema.
**Recommendation:** thread the already-computed `meta` into the header for all formats (pass `meta` into `parse_statement`, or enrich `result$header` from `result$metadata` in `convert.R` before `reconcile`), and make `extract_metadata` fall back to `input$lines` (CSV) and the Excel preamble rows so period/label/count anchors can actually be found.

---

**P1-2 · Admin destructive actions are not authorized server-side**
Area: security / admin · `app.R` — `adm_tpl_delete` (866), `adm_tpl_save` (905), `adm_dict_save` (926), `adm_rollup` (2734); guard `admin_ok()` used only at 751/922 · CONFIRMED · Effort S · Impact H

The admin password only drives `output$admin_authed`, a reactive gating `conditionalPanel` *visibility* (`app.R:747-748`, `suspendWhenHidden=FALSE`). The observers behind delete-user-template, save/overwrite-user-template, overwrite-the-shared-dictionary and rollup-logs never re-check `admin_ok()`. In Shiny any connected client can send those `input` ids straight over the websocket without the panel being visible and without entering the password. `adm_dict_save` overwrites the org-wide `dictionaries/labels.yaml` that `extract_metadata` uses for balance/period/count label matching — so an unauthenticated user can silently change reconciliation behaviour for everyone; `adm_rollup` archives-and-unlinks the per-run JSON audit originals. **Why it matters:** the admin password is the documented barrier over the shared template library, the dictionary that feeds reconciliation, and the audit trail — all bypassable. Visibility is not authorization.
**Recommendation:** add `req(isTRUE(admin_ok()))` (or an early `return()`) at the top of every admin-side observer where the state mutation happens, ideally via a single wrapper so a future observer cannot be added without the guard. Also (P3) refuse to arm the admin surface while the password is still the `"changeme"` default (`R/config.R:12`, compared at `app.R:750-751`).

---

**P1-3 · A re-convert that flips accept→withhold leaves the previously-accepted rows live on the dashboard**
Area: feed / governance gate · `R/feed.R:91-98` (writes one folder, never unlinks the other) · CONFIRMED · Effort S · Impact H

`write_feed` writes `feed/transactions/<key>.csv` on accept and `feed/review/<key>.csv` on withhold, content-hash keyed so a re-convert overwrites its *own* folder — but it never deletes the file in the *other* folder. If a statement was accepted once and later re-converted to a withheld result (realistic when an admin edits/removes the matching template, raises `feed.min_trust`, or changes the allowlist between runs), the manifest correctly flips to `withheld:…` while the previously-accepted `transactions/<key>.csv` is left in place. Qlik loads `transactions/*.csv`, so rows the gate now rejects stay on the dashboard. **Why it matters:** this is exactly the "unvetted output becomes org data" failure the gate exists to prevent, and it is silent — the QA manifest says withheld while the dashboard still shows the accepted rows.
**Recommendation:** before writing, `unlink` the same key from the opposite folder(s) so exactly one of `transactions/<key>.csv` / `review/<key>.csv` exists per statement at any time; do it under the same `safe()` wrapper.

---

**P1-4 · Digital PDFs with a vector black-box over live text are read and emitted (un-redaction leak)**
Area: redaction · `R/read_pdf.R:54-91` (detection hook) and 281 (`dark_rects` only inside the OCR branch) · PLAUSIBLE · Effort M/L · Impact H

For a digital (text-layer) PDF, redaction is honoured only via baked-in text markers, caller/template-supplied rects, or — **on OCR pages only** — auto-detected rasterised dark boxes. There is no detection of a filled black rectangle drawn as a vector graphic over text that is still present in the text layer: `pdftools` returns the underlying text, `apply_redaction_guard` finds no marker and no rect, and the hidden value is emitted verbatim. The header comment at `read_pdf.R:62-74` documents this as unimplemented future work and notes the content-stream (`re`/`f`) parser and the unconditional rasteriser are both absent (the environment lacks the rasteriser/OCR). **Why it matters:** "black box over selectable text" is one of the most common real-world redaction mistakes; emitting the hidden value violates the never-un-redact guarantee and is a silent forensic disclosure. Marked PLAUSIBLE because it is an acknowledged, environment-limited gap rather than a live regression.
**Recommendation:** add a vector-fill-rectangle detector for digital pages (parse the content stream for near-black `re`/`f` rectangles large enough to hide text, or rasterise every page with `pdf_render_page` and run `detect_dark_regions` unconditionally) feeding the *same* `rects` structure into `apply_redaction_guard`. Until then, surface a loud per-page warning when a page contains large filled rectangles so a reviewer knows the guarantee cannot be proven for that page.

---

### P2

**P2-1 · OCR routing keys off a single page-wide character count**
Area: digital-vs-OCR decision · `R/ocr.R:125-129` (`page_needs_ocr`, `min_chars=20`), `R/read_pdf.R:256-291` · PLAUSIBLE · Effort M · Impact M
Three related failure modes share one thin signal. (a) A scanned transaction page that also carries a thin digital layer (Bates stamp, footer, watermark) easily exceeds 20 non-space chars, so OCR never runs and the image-only rows are dropped with no flag — the exact case the code comment at `read_pdf.R:224-229` says it wants to defend against. (b) A broken-CID / no-ToUnicode font extracts text of the right length but garbled content, treated as a good text page. (c) On a page where `pdf_data` returned word boxes but `pdf_text` returned empty, `pages[p]` is `""`, so `page_needs_ocr` is TRUE and OCR overwrites good digital boxes at `read_pdf.R:289` — also breaking "a digital PDF must never be OCR'd". **Recommendation:** route on transaction-region coverage / dark-ink-with-no-word-boxes and gate the trigger on word-box presence, not a flat char count; add a cheap text-health ratio (alphanumeric vs control/PUA/replacement) to force OCR on corrupt text. Silent row loss on a scanned page is the worst of the three.

**P2-2 · `no_unparsed_rows` falsely fails on a legitimate multi-line quoted CSV record**
Area: reconcile · `R/reconcile.R:132-149`; source `R/read_delimited.R:88` · CONFIRMED · Effort S · Impact M
Completeness compares `source_line_count` = non-empty *physical* data lines (`read_delimited.R:88`) against `n` = parsed *logical* records, but `.split_records` deliberately coalesces a quoted field with an embedded newline into one record spanning several physical lines. So a statement with even one legitimate embedded-newline field computes `lost > 0` and fails `no_unparsed_rows` → trust drops to low, run marked needs_review, for a complete parse. **Why it matters:** crying wolf on every valid multi-line statement trains analysts to ignore the one completeness KPI that proves no rows were dropped. **Recommendation:** compare against the logical-record count — have `read_delimited` return `length(records)`, or subtract the extra physical lines accounted for by multi-line `source_spans` (`sum(lengths(source_spans)) - length(source_spans)`) — and keep a separate explicit signal for genuinely unbalanced-quote records so a real merge still fails loudly.

**P2-3 · A blank/redacted middle balance cell creates a blind window in running-balance continuity**
Area: reconcile · `R/reconcile.R:55-71` · CONFIRMED (by inspection) · Effort S · Impact M
The continuity loop skips any `i` where `balance[i]`, `balance[i-1]` or `amount[i]` is NA (line 58). A single NA middle balance skips *both* adjacent pairs, so a real balance break absorbed inside that gap is never seen (e.g. balances 100, NA, 130 with amounts 0/+20/+5 — the 130 should be ~125 — reports "pass, 0 discontinuities"). When amounts are also redacted, `balance_reconciliation` is `na` too, leaving continuity as the only integrity check with this hole. **Recommendation:** bridge across the gap — carry the last known-good balance plus the cumulative sum of intervening amounts and check `balance[i]` against that; if any intervening amount is NA, flag the bridge as genuinely unknown rather than silently passing.

**P2-4 · `detect_multiple_statements` only fires on distinct inline period ranges**
Area: templates / multi-statement · `R/extract_metadata.R:115-129` · CONFIRMED · Effort M · Impact M
`likely_multiple` is TRUE only when `n_periods > 1`, where `n_periods` counts distinct *inline* period-range strings (plus a labelled opening/closing-date fallback that collapses to one). Several same-format statements concatenated in one file are NOT flagged when periods are labelled pairs, year-less, or not a recognised inline range — the rows of N statements merge into one parse, endpoints are taken from whichever statement's labels win, and `balance_reconciliation` may still "pass" via derived endpoints: a merged, wrong result that looks complete. **Why it matters:** this is the exact bundle case the tool is meant to refuse. **Recommendation:** add deterministic boundary signals (fingerprint/header block re-occurrence — `page1_markers` already counts "Page 1 of N" repeats; running-balance discontinuity coinciding with a fresh opening-balance line; account-number change as supporting-only) and flag on ANY strong signal, defaulting to flag-and-split-to-needs_review. (See the multi-statement research section.)

**P2-5 · Delimited reader tags bytes UTF-8 without transcoding — Windows-1252/Latin-1 corrupts verbatim descriptions**
Area: encoding · `R/util.R:84-89` (`safe_readlines`) · CONFIRMED · Effort S · Impact M
`readLines(path, encoding="UTF-8")` only *tags* strings as UTF-8; it does not transcode. Bank CSV exports are frequently Windows-1252/Latin-1 (a £, é, or non-breaking space in a payee name), which then become mojibake or invalid-UTF-8 flowing verbatim into description/amount cells. Only the UTF-8 BOM is stripped; a UTF-16 BOM file is garbled entirely. **Why it matters:** the verbatim-description guarantee breaks when a £ silently becomes garbage, and European-formatted amounts can corrupt too. **Recommendation:** BOM-sniff (UTF-8/UTF-16), else validate UTF-8 and fall back to a declared/detected 8-bit codepage, then `iconv` to UTF-8; a template-level `encoding:` field keeps it deterministic. Base R (`iconv`, `readBin`) suffices.

**P2-6 · `filename_regex` crosses the `min_score` eligibility threshold**
Area: detection · `R/detect.R:39-43` · CONFIRMED (latent) · Effort S · Impact M
The `filename_regex` bonus (`score <- score + 1`) is folded into the same score later tested against `min_score` (`eligible <- scores >= mins`) and used for tie-break/margin. A template with a low `min_score` and a `filename_regex` can thus become eligible and win on the file *name* with zero header/page evidence, and identical file *content* under a different name detects differently — weakening "same input ⇒ identical output". The bonus is implemented only in the delimited branch, so the key silently does nothing for excel/pdf. Latent today (all shipped templates set `filename_regex: null`) but the builder/Advanced-YAML can introduce it. **Recommendation:** keep the filename signal out of the eligibility decision — let it only break a genuine tie among already-eligible candidates, or drop the key.

**P2-7 · Auto-drafted PDF fingerprints are single generic words (or fall back to `"Balance"`)**
Area: templates / builder · `R/wizard_auto.R:17-29`, `R/draft.R:121,133` · CONFIRMED · Effort M · Impact M
`header_phrases()` returns up to 3 single alphabetic words from `.HDR_KEYS` (date, balance, amount, …), and `.draft_pdf` falls back to `fp <- "Balance"` (min_score 1) when nothing is found. A saved user PDF template can therefore have a fingerprint like `["date","balance","amount"]` or just `["Balance"]` — phrases on essentially every statement. Since such a template only wins when no more-specific template out-scores it (the "unseen format" case), the effect is to convert a correct "unsupported" verdict into a silently-wrong match for future unseen PDFs. The shipped templates use hand-authored multi-word phrases ("Transaction type and details"); the auto-drafter cannot. **Recommendation:** prefer multi-word header phrases / branded strings, reject single-word-only fingerprints at save time, never fall back to bare `"Balance"`, and require ≥2 phrases with a minimum total length for user PDF templates.

**P2-8 · The tutorial sample template participates in real detection**
Area: templates · `templates/tutorial_everyday_pdf.yaml` loaded by `load_templates` · CONFIRMED · Effort S · Impact M
`tutorial_everyday_pdf.yaml` is a synthetic worked example but lives in `templates/` and loads as a curated default, so it takes part in production detection. Its fingerprint `["Transaction details","Withdrawals","Deposits"]` (min_score 3) is generic NZ-statement wording; a real statement using those words can match (or tie) and be parsed with the sample's hardcoded x-bands and reported as bank "Kowhai Bank NZ (sample)". **Recommendation:** move sample/tutorial templates out of `templates/` (e.g. `docs/samples/`, excluded from `load_template_set`) or ship them `hidden: true`, and add a load-time guard that excludes sample-marked templates from detection.

**P2-9 · `effective_from` / `effective_to` are declared on every template but never consumed**
Area: schema · `templates/*.yaml`; no reader in `R/` · CONFIRMED · Effort S · Impact L
Every shipped delimited template carries `effective_from`/`effective_to`, they round-trip through the Advanced-YAML editor, but a repo-wide search finds no consumer — detection, validation and parsing all ignore them. A maintainer will reasonably assume these date-scope a template; a statement outside the range still matches. **Why it matters:** a load-bearing-looking key that is inert invites misplaced trust that an outdated-format template won't match newer statements. **Recommendation:** either implement the range as a *soft* detection signal (out-of-range = score penalty / needs_review note, never a hard silent filter) or remove the keys.

**P2-10 · `excel_generic_xlsx` is a loose catch-all that mis-signs unsigned workbooks**
Area: templates · `templates/excel_generic_xlsx.yaml` · CONFIRMED · Effort S · Impact M
It matches any workbook with 3 of `{Date, Description, Amount, Balance}` and assumes `amount_sign: signed`. A workbook whose Amount column holds unsigned magnitudes reads as all-positive (all money-in); because Balance is only 1 of 4 fingerprint fields, a matching workbook can lack a balance column entirely, so continuity can't catch the inverted signs and it passes as ok. **Recommendation:** run `detect_amount_style()` on the sampled Excel table before falling back to `signed`, or tighten `excel_generic` to require a balance column and downgrade a signed Excel with no balance and no negatives to needs_review.

**P2-11 · `dr_cr_suffix` double-applies sign when a value carries both accounting-negative and a DR/CR suffix**
Area: engine · `R/normalise.R:128-134` with `.num_one:57-90` · CONFIRMED (reproduced) · Effort S · Impact M (narrow)
`dr_cr_suffix` strips the suffix then calls `.num`, but `.num_one` independently makes parentheses/minus negative, then the suffix sign multiplies again. Reproduced: `parse_amount("(500.00) DR")$value` → `+500` and `parse_amount("-500.00 DR")$value` → `+500`, when both markers indicate a debit and the answer should be `-500`. Narrow (needs a bank printing both markers on one figure) but a silently sign-flipped amount when it hits. **Recommendation:** in the `dr_cr_suffix` branch, read the magnitude unsigned (strip parentheses/minus before `.num`) and let only the suffix set the sign.

**P2-12 · Feed CSVs are written non-atomically**
Area: concurrency / feed · `R/feed.R:92-94,111-113` · PLAUSIBLE · Effort S · Impact M
Both the transactions/review CSV and the manifest are written straight to their final path with `write.csv` (no temp-then-rename). The scheduled Qlik reload reads the folder on a timer, so a reload landing mid-write reads a truncated CSV over SMB; two simultaneous converts of the same content race on the identical hash-keyed path. **Recommendation:** write to a unique temp file in the same directory then `file.rename` over the destination, so every reader sees a complete old-or-new file and the same-statement race becomes last-writer-wins on whole files.

---

### P3 (grouped)

- **PDF copyright-year inference stamps a wrong, unflagged year** — `R/parse_pdf_table.R:317-322,413`. With no parseable period and year-less table dates, a single distinct 4-digit year anywhere in page text (e.g. a footer "© 2019") is applied to every row and `year_resolved` becomes TRUE, bypassing the `date_unresolved` net. CONFIRMED (narrow). Restrict the scan to the period region, or flag `date_year_inferred` when the year came only from free page text.
- **Money regex requires exactly 2 decimals** — `R/labels.R:30`. A printed whole-dollar opening/closing balance ("Opening Balance $1,234") is not matched, so `balance_reconciliation` degrades to `na` with no signal a printed balance went unrecognised. CONFIRMED. Make the cents group optional while keeping the `(?![0-9])` guard; consider requiring a currency symbol/thousands grouping when cents are absent.
- **Excel serial-date `round()` uses banker's rounding** — `R/read_input.R:70`. A datetime serial with a `.5` fraction (noon) can round to the following day. PLAUSIBLE (narrow). Use `floor()` — the integer part is the date.
- **Feed manifest accumulates one row per re-convert** — `R/feed.R:111-113`. `transactions/<hash>.csv` is idempotent but the manifest is keyed by timestamped `run_id`, so Qlik's coverage table counts a re-run statement N times. CONFIRMED. Key the manifest by content hash (latest wins) or document Runs as per-attempt with a `max(converted_ts)` pattern.
- **`.trust_ok` `%||%` does not catch NA** — `R/feed.R:19,35`. An `NA` trust level makes `if(NA)` throw inside the gate; the `safe()` wrapper at `app.R:1547` swallows it, dropping both the transactions file and the manifest row. PLAUSIBLE/latent (trust level is always concrete today). Coalesce NA to the lowest trust (fail-closed to withheld).
- **Feed timestamps are local-time, non-deterministic** — `R/convert.R:16-17`, `R/feed.R:55`. `run_id` and `converted_ts` use `Sys.time()` with no fixed zone, so the feed is not reproducible across hosts/re-runs while the workbook/CSV/JSON are. CONFIRMED. Stamp UTC with an explicit zone and document `run_id` as an intentional per-run handle.
- **`save_user_template` slug collision silently overwrites** — `R/templates.R:237-239`. Two distinct ids that sanitise to the same slug ("ANZ Go!" / "ANZ-Go" → `ANZ_Go`) overwrite; the `_custom` guard only protects against default ids. CONFIRMED. Refuse or auto-uniquify when the target file already holds a different original id.
- **Auto-drafted delimited fingerprint pins ALL headers** — `R/draft.R:90-91` (`min_score = length(h)`). A one-column change in a later export drops the match to "unsupported", pushing analysts to re-draft near-duplicates. CONFIRMED. Draft a distinctive subset with `min_score` below the full count and surface it in the builder.
- **Narrow redacted cell can render blank instead of `[REDACTED]`** — `R/detect_redaction.R:96-121` (fixed `x_step=34`). A mapped column narrower than the stride may get no synthetic token, so a redacted cell reads empty. PLAUSIBLE. Make the stride adaptive to the template's column bands.
- **OCR page-mean vs per-word confidence come from different rendered images** — `R/ocr.R:98-120`. The page-mean fed to `ocr_min_confidence` is from `use_img` (upscaled) while per-word conf is from `box_img`; the two can diverge. PLAUSIBLE. Derive both from the same box-image TSV.

---

## Template builder & structure — fit for purpose?

**Verdict: the builder is a strong UX shell that is NOT yet safe to hand a non-technical analyst unsupervised, because it can emit valid-but-silently-wrong templates. The schema is well-factored and expressive for the mainstream NZ CSV/PDF/Excel range, with a few footguns and one dead key.**

What is genuinely solid and must not regress: draft-from-file → confirm plain-English choices → live preview (which reuses `display_transactions`, so preview == output) → Save → auto re-convert; `validate_template` is enforced on every save and on Advanced-YAML apply, so structurally *invalid* templates cannot be saved; the PDF band editor auto-switches `amount_sign` to `debit_credit_cols` when a debit/credit band is drawn; `unsigned_default` is surfaced; `metadata_regions` is a real capability that feeds `reconcile`. The failure modes are **semantic, not structural**:

Must-change list (to be analyst-safe):
1. **Infer and expose `type_debit_value`** and require it for `type_dc` (P0-2). This is the single highest-impact builder gap: the one path where the draft picks a sign style whose correctness depends on a value it never infers, the Simple UI never shows, and validation never requires.
2. **Reject or repair over-generic fingerprints** (P2-7): the auto-drafter can only produce single generic words / a `"Balance"` fallback for PDFs, so analyst-built PDF templates are systematically looser than curated ones and erode the "unsupported" safety net. The Simple tab has no fingerprint control at all.
3. **Draft a distinctive subset fingerprint** for delimited, not all-columns (P3), so a one-column export change doesn't drop the match and drive near-duplicate proliferation.
4. **Exclude sample templates from detection** (P2-8).

Schema footguns beyond the above: `effective_from`/`effective_to` are the only outright dead keys (P2-9); `filename_regex` is score-affecting, delimited-only, and content-nondeterministic (P2-6); `amount_sign` lives at the top level for delimited but under `table:` for pdf, forcing every helper to branch (a factoring smell, workable); and there is no schema notion of a repeating statement block (see next section). With the four must-changes, the builder is fit for purpose. The schema is fit for purpose for single-statement NZ layouts; the main missing *capability* is repeat/split.

---

## Multi-statement (same format) support — research verdict

**Feasible: yes. Reliable: yes, deterministically, without guessing hidden values. Worth it: build flag-and-split-assist first, full auto-split second and only behind an explicit opt-in.**

The building blocks already exist; today the tool only *flags* via `detect_multiple_statements` (→ needs_review) and never splits, and the flag is under-powered (P2-4). A wrongly-placed boundary in a forensic context is itself a silently-wrong outcome, so the default for an un-opted template must remain: detect the bundle, refuse the merged parse, route to needs_review with the boundary evidence shown.

**Deterministic boundary signals** (combine; require ≥1 strong; all observable in source text/coordinates, none require un-redacting or estimating a hidden value):
- STRONG — the identifying fingerprint / header block re-occurs mid-document (partly captured already by `page1_markers` counting "Page 1 of N" repeats).
- STRONG — a second, *different* statement-period range (the current signal, but hardened for labelled-pair and year-less cases).
- STRONG — a running-balance discontinuity that coincides with a fresh "opening balance" line (`reconcile` already computes per-adjacent-row continuity; a break at a row carrying a new opening balance is a boundary, not a data error).
- SUPPORTING (never alone) — an account-number change between row clusters (`extract_metadata` already notes multi-account is unreliable on its own because transfers name other accounts).

**Template-schema representation sketch** (opt-in `split:` block):

```
split:
  mode: repeat                 # one template, N statements in the file
  boundary:                    # first STRONG hit starts a new statement
    fingerprint_repeat: true   # re-occurrence of the page/header fingerprint block
    on_period_reset: true      # a new distinct period range
    on_new_opening_balance: true   # 'opening balance' line after a continuity break
    on_account_change: supporting  # never a boundary on its own
  per_statement_metadata: true # re-derive opening/closing/period/account per segment
  min_rows_per_segment: 1
```

The parser segments the row stream at boundaries, then runs the existing parse + reconcile **per segment**, emitting one result set per statement plus a bundle summary.

**What must change to support full auto-split:** per-statement reconciliation replaces the single `balance_reconciliation` (each segment gets its own opening/closing anchors); period/account metadata becomes a list, not scalars (the Metadata sheet and run-log schema currently assume scalars); trust rolls up as `min` over segments and a mis-placed boundary must degrade to needs_review, never a confident merge; output writers need a statement-index column or per-statement sheets; and completeness becomes "every source row assigned to exactly one segment AND every segment reconciles". No new R package is needed — this is base-R string/coordinate work on structures `read_pdf`/`read_delimited` already produce.

**Sequencing:** harden the *flag* first (fix P2-4 so bundles are reliably detected across labelled-pair/year-less/fingerprint-repeat cases) — this is immediately worth it and low-risk. Build auto-split next, only behind the `split:` opt-in with per-segment reconciliation and roll-up trust.

---

## Bold refactors (optional)

- **Route digital-vs-OCR on region coverage, not a scalar char count.** P2-1 is three findings stemming from one thin signal (`page_needs_ocr` char count). Replacing it with a transaction-region-coverage / dark-ink-without-word-boxes decision, gated on word-box presence, closes footer-defeat, CID-garble, and the good-page mis-fire at once, and is the highest-leverage change in the input domain.
- **One metadata pipeline for all formats.** The PDF path enriches the header from `extract_metadata`; delimited/excel do not (P1-1). Unify so every format threads period/opening/closing/stated-count into `parsed$header` through one code path, and make `extract_metadata` format-aware over `input$lines`/`input$table`. This also removes the feed's always-blank-column drift and the PDF-only two-tier safety net in one move.
- **A single admin-observer wrapper that enforces `req(admin_ok())`.** Beyond fixing P1-2, wrapping every admin mutation in one guarded helper makes it structurally impossible to add a new unguarded admin action later.
- **Atomic-write + gate-reconciled feed writer.** Fold P1-3 (unlink opposite folder), P2-12 (temp-then-rename) and the manifest-keying decision (P3) into one `write_feed` rewrite so the feed folder always reflects the current gate verdict as complete files.
- **Normalise `amount_sign` placement.** Lifting the pdf `table.amount_sign` / `table.columns` to the same top-level shape as delimited would delete the `is_pdf` branching threaded through `template_overview`, `.template_shape`, and the builder.

---

## Recommended sequence

**Do first — must-fix before launch (all small, base-R, no new dependency):**
1. **P0-1** strict `parse_date` round-trip + year bound (`normalise.R`). The sharpest silently-wrong path; ships on BNZ today.
2. **P0-2** infer + require + expose `type_debit_value`, make the compare case-insensitive (`draft.R`, `normalise.R`, `templates.R`). Whole-statement sign inversion from the builder.
3. **P1-2** add `req(admin_ok())` to `adm_tpl_delete`/`adm_tpl_save`/`adm_dict_save`/`adm_rollup`; block the default `"changeme"` password.
4. **P1-3** unlink the opposite feed folder on re-convert.

**Quick-wins to batch in the same pass (each S effort, high signal):**
5. **P2-2** count logical records for `no_unparsed_rows` (stops crying wolf on the one completeness KPI).
6. **P2-3** bridge the continuity NA-gap.
7. **P2-8 / P2-9 / P2-6** remove the tutorial template from detection; make `effective_*` a soft signal or delete it; take `filename_regex` out of eligibility.
8. **P2-5** transcode delimited input to UTF-8.

**First patch after launch:**
9. **P1-1** thread metadata into the delimited/excel header (M) — closes the CSV/Excel reconciliation gap and the backstop for P0-1.
10. **P1-4 / P2-1** digital vector-box redaction detection and coverage-based OCR routing (the two larger input-domain items).
11. **P2-4** harden multi-statement detection, then scope the `split:` opt-in.

**Later / hygiene:** the remaining P2s (excel generic tightening, atomic feed writes, dr_cr double-sign) and the P3 list.

---

## Future directions (appended after the hardening pass, 2026-07-22)

Now that every prioritised finding is resolved and the local metadata-capture
subsystem is in place, these are the forward bets — ordered roughly by value ÷
effort. None are committed work; they are the map.

### Near-term, low-risk
- **`split:` opt-in for statement bundles.** Detection now *flags* a bundle
  reliably (period repeat, `Page 1 of N` repeat, balance-block repeat → needs_review;
  P2-4). The next step is an opt-in `split:` template block that auto-segments a
  proven bundle at those boundaries, reconciles each segment independently, and
  rolls up a per-segment trust — never merging, never guessing a boundary. Default
  stays flag-and-refuse for un-opted templates.
- **Simple-tab fingerprint control (PDF).** The save-time gate now rejects a
  generic PDF fingerprint (P2-7), but the Simple tab has no control to *fix* one —
  a non-technical analyst hits the gate and must drop to Advanced YAML. Add a
  plain-English "distinctive phrase(s) on this statement" picker.
- **Deployment password hardening.** Server-side admin authorization is now
  enforced (P1-2), but the *default* password is still `changeme`. A first-run
  check that refuses to serve the Admin tab until the default is changed (or a
  real password / `BSO_ADMIN_PASSWORD` is set) closes the deployment gap.
- **Broaden preamble-wording recognition (CSV/Excel).** P1-1 wired preamble
  metadata through, but the extractor is conservative: real ASB-style wording
  ("Ledger Balance", "Avail Bal", "From date 20141220") and ISO-basic dates aren't
  yet recognised. Extend the label dictionary + a `%Y%m%d` date shape so those
  statements gain a working `balance_reconciliation` / `dates_within_period`.
- **Template `encoding:` key.** `safe_readlines` already auto-detects (BOM + UTF-8
  validity → Windows-1252) and accepts an `encoding` override; surfacing it as a
  validated template key would make an exotic codepage fully deterministic.

### The local-ML assist (using the metadata goldmine)
The metadata-capture subsystem is the substrate. Kept **local, forever, PII-safe**,
it accumulates a labelled corpus of *how every conversion went*. On top of it,
entirely on-box (no cloud, no new runtime dependency beyond an optional local
model):
- **Template-drift detection.** Cluster `layout.signature` + `field_fill` +
  KPI-outcome vectors over time; when a proven template's parse-quality
  distribution shifts (a bank changed its export), surface "this template may need
  updating" *before* an analyst hits a bad conversion.
- **Unseen-layout clustering.** Group `unsupported` conversions by layout
  signature so the biggest gaps bubble up ("14 files this month share a layout with
  no template") — a prioritised template-building queue, already partly served by
  the batch-audit gap report.
- **Draft-quality scoring.** Learn from the corpus which drafted templates went on
  to reconcile cleanly vs. which needed rework, and warn at save time when a draft
  looks like the ones that historically failed.
- **Reconciliation-anomaly hints.** With balance anchors + net amounts captured,
  flag a statement whose totals sit far outside the account's own history — a
  possible mis-parse or a genuine anomaly worth a second look.
All of the above are *assistive and non-authoritative*: they recommend, they never
silently change a figure. The deterministic engine remains the source of truth;
the model only ever points a human at something.

### Transaction categorisation (planned downstream — kept out of the extraction core)
Categorisation is a **downstream** capability, never part of the never-guess
extraction core, and always visibly derived (never presented as extracted truth):
- **Step 1 — recreate the existing maintained-keyword logic.** Port the current
  maintained keyword→category list as a deterministic, auditable rule pass over the
  (already-extracted) descriptions. Same inputs ⇒ same categories; every assignment
  traceable to the keyword that produced it; unmatched ⇒ uncategorised, never
  guessed.
- **Step 2 — something better.** Only once step 1 is trusted: a local, explainable
  model that proposes categories with a confidence and the evidence, for a human to
  accept — again downstream, again non-authoritative, again local-only.

### Blue-sky (my own list)
- **Diff two conversions of the same account.** Given last month's and this month's
  statement, show the reconciled delta (new payees, changed standing amounts) —
  forensic gold, and trivial once metadata anchors exist.
- **A "confidence contract" export.** Alongside the workbook, emit a machine-readable
  attestation: which KPIs proved what, which cells were OCR'd/redacted/inferred, and
  the exact template + version — so a downstream system (or a court bundle) can
  consume the *evidence*, not just the numbers.
- **Template synthesis from two examples.** Let an analyst drop two statements of the
  same bank and have the tool propose the stable column bands / date format / amount
  style from their intersection — draft-by-example rather than draft-by-drawing.
- **Redaction self-audit page.** A one-click report over `logs/metadata` showing
  every conversion where a redaction was detected, un-verifiable (scan incomplete),
  or a year/direction was inferred — the forensic reviewer's standing worklist.
- **Deterministic OCR "second reader".** Run two OCR engines (or two preprocessings)
  and only trust a cell where they agree; disagreement → flagged, never silently
  picked. Extends the never-silently-wrong contract into the pixel domain.
