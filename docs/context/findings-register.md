# Findings register

Every verified finding from the full-codebase review, with its status. This is the
working backlog: fix, tick, and append anything new to the bottom.

**How it was produced.** Ten independent review lenses over the whole codebase produced
113 findings; the 63 rated critical/high went through an adversarial verification pass
(62 verdicts returned: 47 CONFIRMED, 15 OVERSTATED/downgraded, 0 rejected outright), and a
completeness critic added 8 more. Severities below are POST-verification. The remaining
51 medium/low findings were not individually verified and are not listed here.

Status values: `open` · `fixed` · `not-a-defect` · `wont-fix` (with a reason).

_Last updated: 2026-07-26._


## Correctness

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 3 | CRITICAL | CONFIRMED | S | **fixed** | asb_everyday_pdf is a catch-all: any unsupported bank's PDF is silently parsed as ASB and accepted into the Qlik feed with wrong figures | templates/asb_everyday_pdf.yaml:10-12 (`min_score: 2` over `["Debit/Withdrawal", "Deposit", "Balance"]`) + R/detect.R:22-27 (substring `grepl(fixed=TR |
| 5 | HIGH | CONFIRMED | S | **fixed** | One blank or unreadable amount silently switches off balance reconciliation — and the reason shown is false | R/reconcile.R:37 requires `!any(is.na(tx$amount))`; the else branch at :49-51 reports status "na" with detail "no opening/closing balance and no runni |
| 20 | HIGH | CONFIRMED | M | **fixed** | PDF continuation merge silently deletes verbatim description words | R/parse_pdf_table.R:586-594: the merge folds only the descriptive bands (descr_cols at :559) into the previous transaction and uses the row's full raw |
| 28 | HIGH | CONFIRMED | M | **fixed** | 'Proven' means only 'the YAML sits in templates/' — and at least one template there has no test at all | R/feed.R:78-83 defines proven as `tid %in% names(load_templates(config$paths$templates, strict = FALSE))` — directory membership, nothing else. `asb_e |
| 34 | HIGH | CONFIRMED | M | **fixed** | The X-ray and row-coverage diagnostics contradict the reader whenever keep_dateless_rows is on — and the whole feature has zero tests | R/parse_pdf_table.R:501-506 (.is_txn has a fourth branch: `keep_dateless && .real_amount(r) && .has_amount(r)`); R/inspect.R:132-133 (natural_keep com |
| 48 | HIGH | CONFIRMED | S | **fixed** | Every PDF template the toolkit drafts carries a fingerprint phrase that can never match — whitespace-normalised at draft time, literal-matched at detect time | R/wizard_auto.R:48 collapses runs of whitespace before storing a phrase (`lines <- gsub("\\s+", " ", lines[nzchar(lines)])`); R/detect.R:24-25 matches |
| 9 | MEDIUM | CONFIRMED | S | open | "Something else" (form / IRD) templates skip the generic-fingerprint guard that protects statement templates, so one loose phrase can hijack every unsupported statement | R/templates.R:42-48 rejects a PDF statement template whose `page_contains_all` is only generic words, precisely so a template cannot "match unseen PDF |
| 15 | MEDIUM | CONFIRMED | S | open | Form (mode:fields) conversions are logged as "unsupported" — the audit trail and the Admin gap queue both lie | R/forms.R:165-181 (convert_document runs convert_statement first, which already wrote the log, then returns the form result); R/convert.R:193-217 writ |
| 16 | MEDIUM | CONFIRMED | S | open | Nothing pins which template bytes produced a figure — version: is a decorative 1 everywhere and no content hash is recorded | All 13 shipped templates carry `version: 1` (templates/*.yaml:5); R/draft.R:130,180,197 hard-code version = 1 on every draft; no code path anywhere in |
| 19 | MEDIUM | CONFIRMED | M | open | No retraction path: a conversion the analyst flags as WRONG stays on the Qlik dashboards forever | R/feedback.R:13-32 (submit_feedback writes a log record and nothing else); R/feed.R:102-105 is the ONLY code in the repo that removes a feed file, and |
| 21 | MEDIUM | OVERSTATED | S | **fixed** | date_year_inferred writes a guessed year into the emitted date and gates nothing | R/parse_pdf_table.R:330-336 — when no period parses, the year is taken from a regex scan of all page text (`\b(?:19/20)[0-9]{2}\b`) and used whenever  |
| 50 | MEDIUM | OVERSTATED | M | **fixed** | Row loss in the PDF path is invisible to every on-screen check except balance reconciliation | R/parse_pdf_table.R:616-617 drops any visual row failing `.is_txn` and returns `source_line_count = NA_integer_` (:780). R/reconcile.R:170 therefore s |
| 1 | HIGH | CONFIRMED | S | **fixed** | PDF templates with amount_sign: type_dc silently invert every debit into a credit | R/parse_pdf_table.R:647 (`parse_amount(amt_raw, style, list(decimal=dec, unsigned_default=udef))` — never passes `type`/`type_debit_value`/`type_credi |

## Security/Privacy

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 6 | HIGH | CONFIRMED | M | open | Every converted statement is silently retained forever in uploads/, with no purge, no retention policy and no disclosure to the user | app.R:1643 calls `record_upload()` on every real Convert; R/uploads.R:22 `safe(file.copy(path, file.path(ud, name)))` writes a byte-identical copy of  |
| 36 | HIGH | CONFIRMED | S | **fixed** | A failed vector-redaction scan changes nothing: hidden text is emitted and the run still says "Converted successfully" | R/read_pdf.R:314-330: when `detect_occluded_words` returns ok=FALSE (pdftools/magick missing, or the page will not rasterise — R/detect_redaction.R:10 |
| 40 | HIGH | CONFIRMED | S | **fixed** | The toolkit fingerprint can bake a customer's name into a saved template file | R/wizard_auto.R:33-37 `.fp_is_customer` only rejects a line that STARTS with a personal title, or a bare 1-4 word all-alpha line with no branding word |
| 47 | HIGH | OVERSTATED | M | open | The forensic audit trail's "who converted this" is either constant for the whole department or freely typed by the user | scripts/run_app.R:36 (`shiny::runApp(app_dir, host = "0.0.0.0", ...)` — ONE server process, browsers connect from other machines per docs/operational/ |
| 26 | MEDIUM | CONFIRMED | S | **fixed** | The admin-authorisation invariant test matches only `observeEvent(input$adm_`, and every admin surface outside that pattern is in fact ungated — real statement filenames reach any browser | tests/testthat/test-admin_auth.R:12 greps `observeEvent\(input\$adm_` and checks a 4-line window for `req(admin_ok())`. Outside that pattern and all m |
| 32 | MEDIUM | CONFIRMED | S | open | uploads/ retains every real client statement forever — no retention policy, no purge, no admin delete | R/uploads.R:20-21 copies each uploaded statement into uploads/<id>/ on every Convert (app.R:1644-1651, record=TRUE by default); R/retention.R:10-33 ro |
| 37 | MEDIUM | OVERSTATED | S | **fixed** | Ungated download handlers read files chosen by an unsanitised, client-supplied id — cross-user statement access and path traversal | R/uploads.R:79-83 `upload_file_path()` builds `file.path(.uploads_dir(dir), id)` with zero sanitisation; R/inbox.R:35-38 `failed_file_path()` does the |
| 51 | MEDIUM | OVERSTATED | S | **fixed** | Admin authorization is enforced only on write handlers; the read surface — including filenames of failed statements — is ungated and partly pushed to every session | tests/testthat/test-admin_auth.R only greps `observeEvent(input$adm_` and the `adm_data()` loader, so the invariant it proves is narrower than it read |
| 60 | MEDIUM | CONFIRMED | S | open | A typo in config.yaml silently reverts every setting to shipped defaults, including the admin password and the Qlik feed location | R/config.R:123 `fromfile <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)` and :124 `if (is.list(fromfile)) …` — an unparseable file is di |
| 22 | LOW | OVERSTATED | S | open | Withheld conversions write full transaction rows, including account_number, into the folder Qlik is connected to | R/feed.R:112-127 builds the stamped rows — `account_number = h$account_number` at R/feed.R:121 — and R/feed.R:124 sends them to `rev_dir` (feed/review |
| 7 | MEDIUM | CONFIRMED | M | open | Nothing prunes the two places that hold raw client statements — uploads/ and the per-conversion temp dirs grow forever | `R/uploads.R:24` copies every uploaded statement into `uploads/<id>/` and keeps it (`uploads.R:5` — "the saved files hold real statements"). `R/retent |

## Deployment

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 33 | HIGH | CONFIRMED | S | **fixed** | Shiny's default 5 MB upload cap is never raised — scanned PDFs, the dominant input, are rejected at the door on day 1 | `grep -rn maxRequestSize` over the whole repo returns nothing (exit 1). `scripts/run_app.R:36` calls `shiny::runApp(app_dir, host="0.0.0.0", port=port |
| 35 | HIGH | CONFIRMED | S | open | Every update silently wipes the admin-edited label dictionary and recognition lexicon | scripts/bundle-offline.R:56-57 ships `dictionaries` in every bundle; app.R:964-967 and app.R:1006-1012 write admin edits in place to `dictionaries/lab |
| 54 | HIGH | CONFIRMED | S | **fixed** | A scanned PDF on a box without OCR tools reports "No template for this statement yet" plus a nonsense diagnostic, and sends Beth into a toolkit where she can never succeed | R/read_pdf.R:230 gates OCR on `ocr_available()`, which is R/ocr.R:12-13 `nzchar(Sys.which("tesseract")) && nzchar(Sys.which("pdftoppm"))` — both absen |
| 8 | MEDIUM | CONFIRMED | S | open | The bundle build prints DONE and exits 0 even when the R installer and OCR prereqs are missing — deepening the known proxied-build failure | `scripts/bundle-offline.R:102-108` defines `grab()` to `cat` a message and return FALSE on any download error; the three call sites (`:115-116` R, `:1 |
| 30 | MEDIUM | OVERSTATED | S | open | Both first-run repair remedies in first-time-setup.md are no-ops — the .installed marker is written unconditionally | RUN-ME.bat:43 `if not exist "%BUNDLE%\.installed" call :firstRun`; RUN-ME.bat:68-73 runs install-on-pc.R then `type nul > "%BUNDLE%\.installed"` with  |
| 39 | MEDIUM | CONFIRMED | M | open | No backup or restore procedure exists for the analyst-created templates, the edited dictionaries, or the metadata corpus | The only backup anywhere is `RUN-ME.bat:79-93`, which copies `config/config.yaml` to `%LOCALAPPDATA%\StatementStudio` — on the same machine. `grep -rn |
| 44 | MEDIUM | CONFIRMED | S | open | Nobody watches the feed: it can stop filling entirely and nothing in the app, the logs or Admin would say so | app.R:1653 `if (record) safe(write_feed(res, CONFIG))` discards the gate result that write_feed carefully returns (R/feed.R:150); every write inside i |
| 53 | MEDIUM | CONFIRMED | S | open | The documented Task Scheduler recipe kills the app after 3 days and never restarts it if R dies | `docs/operational/running-and-keeping-it-up.md:17-29` gives a five-step Create Task recipe: General → Triggers (At startup) → Actions (RUN-ME.bat) → O |
| 2 | HIGH | CONFIRMED | S | open | An upgrade silently overwrites the Admin-edited label dictionary and recognition lexicon — the same statement can convert differently after an update, with no notice | `scripts/bundle-offline.R:56-59` ships `dictionaries` inside the dist folder. Those exact files are live, server-owned, Admin-edited state: `app.R:965 |
| 14 | MEDIUM | OVERSTATED | S | part-fixed | make-bundle.bat reports success even when the R / Poppler / Tesseract downloads failed | scripts/bundle-offline.R:102-108 — `grab()` catches every download error, prints a one-line 'skipped', and returns FALSE; :115-116 both R-installer at |

## Analytics/Qlik

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 17 | HIGH | CONFIRMED | S | open | The governed Qlik feed re-parses the output CSV with type inference, corrupting references, codes and descriptions | R/feed.R:111 `utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)` — no `colClasses = "character"`, so type.convert runs per column. T |
| 38 | HIGH | CONFIRMED | M | open | A feed write that never lands is invisible everywhere — and the documented UNC-share + scheduled-task setup makes that the likely default | `R/feed.R:22-29` writes via temp-then-`file.rename`, returning FALSE on any failure. The manifest call at `R/feed.R:148` discards that return entirely |
| 56 | HIGH | CONFIRMED | S | **fixed** | The same over-broad ASB fingerprint permanently withholds every real ANZ PDF statement from the governed feed | On the real statement samples/_private_staging/anz_0382025_jun.pdf: detection scores `anz_everyday_pdf` 3 vs `asb_everyday_pdf` 2, so `det$margin == 1 |
| 4 | MEDIUM | OVERSTATED | S | open | The feed's default trust floor (medium) admits exactly the runs the engine says it could not prove | R/config.R:43 and config/config.example.yaml:46 `min_trust: medium`; R/feed.R:35-43 `.trust_ok` accepts high/medium. Trust is capped at medium — never |
| 12 | MEDIUM | OVERSTATED | S | open | feed/review rows are byte-schema-identical to accepted rows with nothing marking them withheld — Qlik will auto-concatenate them into the dashboard | R/feed.R:114-128 builds ONE `stamped` frame and writes it to tx_dir or rev_dir with no discriminating column; the ctx block (feed.R:114-122) has no ga |
| 58 | MEDIUM | CONFIRMED | M | open | The feed's field set changes per template, but the documented Qlik load is one wildcard LOAD assuming a fixed schema | Live in the repo: feed/transactions/136c69b731ca8698.csv has 26 columns ending `currency,flags`; feed/transactions/bbf83b15b965ddbd.csv has 28, ending |

## Coverage/Data

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 11 | HIGH | CONFIRMED | M | **fixed** | No production PDF template has a golden extraction test; asb_everyday_pdf has no test at all and the statements they were built from are gitignored | tests/testthat/expected/ holds 7 golden CSVs: 6 delimited + tutorial_everyday_pdf.csv. `asb_everyday_pdf` appears in no test file. `anz_everyday_pdf`  |
| 27 | HIGH | CONFIRMED | L | open | The scanned/OCR path is not production-ready on real bank scans, and no test or metric would reveal it | Run against the three real scans in samples/_private_staging/scans/: westpac_scan.pdf -> `ok`, reconciles (6297.94 + 430.69 = 6728.63), 11 rows, 18 s. |
| 42 | MEDIUM | CONFIRMED | S | open | The shipped ASB CSV template is calibrated to a 2014 export and cannot read the current ASB export already sitting in the repo's own sample corpus | templates/asb_everyday_csv.yaml:16 pins `date: {source: Date, format: "%Y/%m/%d"}`, matching samples/raw/asb/asb_transaction_export_01.csv (dates `201 |
| 55 | MEDIUM | CONFIRMED | M | open | app.R is 2,938 lines the test suite never loads — including the entire no-code 'add a bank' mapping | tests/run_tests.R:21-23 sources only `R/*.R`; app.R, ui_content.R and ui_labels.R are never sourced. The only two tests that mention app.R (tests/test |
| 57 | LOW | OVERSTATED | S | open | Admin bulk 'convert & save' and the CLI never write the feed — statements vanish from Qlik AND from the coverage manifest | app.R:1066-1070 calls convert_statement() directly inside the Admin bulk-audit loop and never calls write_feed; run.R:29-34 (the CLI) likewise; write_ |

## UX/Adoption

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 13 | HIGH | CONFIRMED | S | **fixed** | Shiny's default 5 MB upload cap is never raised, so the dominant input — scanned PDFs — is rejected outright | `options(shiny.maxRequestSize=…)` / `shinyOptions` appear nowhere in the repo (verified across app.R, run.R, scripts/run_app.R, RUN-ME.bat, .Rprofile  |
| 18 | HIGH | CONFIRMED | S | open | A template Beth builds is switched OFF for auto-detection by default — the app never says so, and the save message tells her to do something that cannot work | R/config.R:18 (`user_templates_default = FALSE`; no config/config.yaml exists, so this default is live); app.R:365 the only opt-in is `checkboxInput(" |
| 24 | MEDIUM | CONFIRMED | S | open | The save-and-reconvert step forces the template by id, so the one moment the app could prove the new template auto-detects is the one moment it bypasses detection | app.R:2851-2856 — after `save_user_template` succeeds the app calls `run_conversion(gp, gn, record = FALSE, force_tpl = saved_id)`; R/convert.R:47-53  |
| 25 | MEDIUM | CONFIRMED | M | open | When the drafted PDF fingerprint is too generic, Save is refused and the only place to fix it is a raw YAML textarea | R/templates.R:44-47 makes a generic-or-empty `page_contains_all` a hard validation failure, and `save_user_template` (R/templates.R:276-277) stops on  |

## Performance/Scale

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 49 | HIGH | OVERSTATED | M | open | One blocking R process: a single scanned statement freezes the whole department for minutes, with no queue and no backpressure | scripts/run_app.R:36 (`shiny::runApp(app_dir, host="0.0.0.0", port=port)` — one process for everyone, launched by RUN-ME.bat:49); repo-wide grep finds |
| 52 | HIGH | CONFIRMED | L | open | One bare R process serves the whole department — a single OCR conversion freezes every other user's browser, and the architecture doc's justification no longer applies | `scripts/run_app.R:36` is a bare `shiny::runApp(host="0.0.0.0")` — one R process, one thread, all sessions. `docs/operational/running-and-keeping-it-u |
| 59 | HIGH | CONFIRMED | S | open | Three always-on observers re-scan unbounded on-disk state on every session connect and after every conversion, with no admin gate | app.R:1022-1025 `observe({ toks <- adm_suggestions()$… })` forces `lexicon_suggestions(LOGDIR)` (R/suggestions.R:17-23), which `fromJSON(simplifyVecto |
| 29 | MEDIUM | CONFIRMED | S | open | Every OCR'd page leaks two full-page images of the client statement into the R temp dir, never deleted | R/ocr.R:85-86 cleans only the pdftoppm renders (`prefix <- tempfile("ocrpg_")`; `on.exit(unlink(Sys.glob(paste0(prefix, "*"))))`). The two preprocessi |

## Docs/Handover

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 41 | HIGH | CONFIRMED | M | **fixed** | Three of the five 'proven' PDF templates have no fixture and no parse test, while README and the HOWTO state that every template ships with one | `templates/` holds 13 templates; `tests/testthat/expected/` holds 7 goldens. anz_everyday_pdf, asb_everyday_pdf and westpac_everyday_pdf are never par |
| 45 | HIGH | CONFIRMED | S | open | Nothing anywhere records which build of the engine produced a figure | No VERSION file, CHANGELOG or git tag (`git tag` empty; `ls` shows none). No engine-version field in the run record (logs/runs/*.json fields: ts, run_ |
| 23 | MEDIUM | CONFIRMED | S | open | Nothing tells the analyst to open the Windows Firewall port — "everyone opens a browser" fails for everyone except the person standing at the server | `scripts/run_app.R:36` binds `host="0.0.0.0"` on port 8100. `docs/operational/first-time-setup.md:58-61` and `running-and-keeping-it-up.md:12` both sa |

## Product/Capability

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 31 | HIGH | CONFIRMED | M | open | No department-scale convert path: bulk convert is admin-only, throws its outputs into a temp dir, and never writes the Qlik feed | app.R:353 — Convert's fileInput has no multiple=TRUE (one file per click); app.R:728 — the only multi-file input is inside the password-gated Admin ta |
| 61 | HIGH | CONFIRMED | S | open | The bundle auto-split subsystem is shipped, tested and completely dark — no template opts in and there is no way to opt one in without hand-editing YAML | R/split.R (247 lines) + tests/testthat/test-split.R; grep for a `split:` key across templates/, templates_user/, templates_seed/, fields_templates/ re |
| 10 | MEDIUM | OVERSTATED | S | **fixed** | A new bank whose name is not in a hardcoded 25-word brand list is classified as a customer name and dropped from the auto-drafted fingerprint | R/wizard_auto.R:24-27 defines `.FP_BRAND_RX`, a literal regex of ~25 words including the NZ bank names (kiwibank/westpac/anz/asb/bnz/tsb/sbs/rabobank/ |
| 46 | MEDIUM | CONFIRMED | S | open | The PDF's own document metadata (producer, creation/modification dates, encryption) is never read — the cheapest genuinely forensic capability left unbuilt | R/read_pdf.R:198-208 calls only pdftools::pdf_text, pdf_data and pdf_pagesize; pdftools::pdf_info is never called anywhere in the repo. Verified live  |
| 43 | LOW | OVERSTATED | M | open | The gate is machine-only and one-way: a needs_review conversion the analyst verifies as correct can never reach the dashboards | R/feed.R:52-53 (require_status_ok rejects anything that is not status=='ok') and R/feed.R:54-55 (trust floor); R/feedback.R has no feed interaction; n |
| 62 | LOW | OVERSTATED | S | open | Downstream categorisation cannot start: it is blocked on a taxonomy nobody has requested, and the legacy-parity gap it closes is absent from the roadmap | docs/context/architecture/legacy-qlik-mapping.md:60 ("Extraction parity is essentially met... The gap is the enrichment layer") and :81-85 ("The descr |

## New findings (append below)

_Anything discovered after the review lands here, newest at the bottom._

### Found while fixing (2026-07-26)

| # | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N1 | high | **fixed** | An invalid template was dropped at load with only an R `warning()` nobody surfaces — so tightening fingerprint validation would have made an analyst's saved template vanish after an update with no explanation. Failing closed is right; failing closed *silently* is not. `load_templates` now returns its skip reasons. | R/templates.R load_templates |
| N2 | medium | **fixed** | Two further admin download handlers were ungated beyond the ones the review named — found only because the widened invariant test was written before the fix. | app.R adm_ba_report / adm_ba_csv / adm_audit_dl |
| N3 | medium | **fixed** | The backfilled PDF goldens were generated but no test consumed them, so #11/#28/#41 looked closed while the parse was still unguarded. Now wired via `expect_statement_ok` plus a guard test that every proven template has a golden. | tests/testthat/test-pdf_template_goldens.R |
| N4 | low | open | `templates/asb_everyday_csv.yaml` still pins a single `%Y/%m/%d`, so the current ASB export in the repo (`asb_transaction_export_03.csv`, `13/10/2025`) reads every date as NA. Needs the candidate-format-list change (#42). | templates/asb_everyday_csv.yaml:16 |
