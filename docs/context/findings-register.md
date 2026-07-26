# Findings register

Every verified finding from the full-codebase review, with its status. This is the
working backlog: fix, tick, and append anything new to the bottom.

**How it was produced.** Ten independent review lenses over the whole codebase produced
113 findings; the 63 rated critical/high went through an adversarial verification pass
(62 verdicts returned: 47 CONFIRMED, 15 OVERSTATED/downgraded, 0 rejected outright), and a
completeness critic added 8 more. Severities below are POST-verification. The remaining
51 medium/low findings were not individually verified and are not listed here.

Status values: `open` · `fixed` · `not-a-defect` · `wont-fix` (with a reason).

**Where it stands: 93 findings, 81 fixed, 12 open — N30 is CRITICAL and is the next thing fixed.** N20 and N21 are the live ones and are described in full at the bottom; N20 is the
only open finding that CAN produce a wrong figure quietly, so it is the next thing
to fix. The other three are waiting on evidence (more real forms, a hand-keyed
golden scan) rather than on effort.

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
| 9 | MEDIUM | CONFIRMED | S | **fixed** | "Something else" (form / IRD) templates skip the generic-fingerprint guard that protects statement templates, so one loose phrase can hijack every unsupported statement | R/templates.R:42-48 rejects a PDF statement template whose `page_contains_all` is only generic words, precisely so a template cannot "match unseen PDF |
| 15 | MEDIUM | CONFIRMED | S | **fixed** | Form (mode:fields) conversions are logged as "unsupported" — the audit trail and the Admin gap queue both lie | R/forms.R:165-181 (convert_document runs convert_statement first, which already wrote the log, then returns the form result); R/convert.R:193-217 writ |
| 16 | MEDIUM | CONFIRMED | S | **fixed** | Nothing pins which template bytes produced a figure — version: is a decorative 1 everywhere and no content hash is recorded | All 13 shipped templates carry `version: 1` (templates/*.yaml:5); R/draft.R:130,180,197 hard-code version = 1 on every draft; no code path anywhere in |
| 19 | MEDIUM | CONFIRMED | M | **fixed** | No retraction path: a conversion the analyst flags as WRONG stays on the Qlik dashboards forever | R/feedback.R:13-32 (submit_feedback writes a log record and nothing else); R/feed.R:102-105 is the ONLY code in the repo that removes a feed file, and |
| 21 | MEDIUM | OVERSTATED | S | **fixed** | date_year_inferred writes a guessed year into the emitted date and gates nothing | R/parse_pdf_table.R:330-336 — when no period parses, the year is taken from a regex scan of all page text (`\b(?:19/20)[0-9]{2}\b`) and used whenever  |
| 50 | MEDIUM | OVERSTATED | M | **fixed** | Row loss in the PDF path is invisible to every on-screen check except balance reconciliation | R/parse_pdf_table.R:616-617 drops any visual row failing `.is_txn` and returns `source_line_count = NA_integer_` (:780). R/reconcile.R:170 therefore s |
| 1 | HIGH | CONFIRMED | S | **fixed** | PDF templates with amount_sign: type_dc silently invert every debit into a credit | R/parse_pdf_table.R:647 (`parse_amount(amt_raw, style, list(decimal=dec, unsigned_default=udef))` — never passes `type`/`type_debit_value`/`type_credi |

## Security/Privacy

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 6 | HIGH | CONFIRMED | M | **fixed** | Every converted statement is silently retained forever in uploads/, with no purge, no retention policy and no disclosure to the user | app.R:1643 calls `record_upload()` on every real Convert; R/uploads.R:22 `safe(file.copy(path, file.path(ud, name)))` writes a byte-identical copy of  |
| 36 | HIGH | CONFIRMED | S | **fixed** | A failed vector-redaction scan changes nothing: hidden text is emitted and the run still says "Converted successfully" | R/read_pdf.R:314-330: when `detect_occluded_words` returns ok=FALSE (pdftools/magick missing, or the page will not rasterise — R/detect_redaction.R:10 |
| 40 | HIGH | CONFIRMED | S | **fixed** | The toolkit fingerprint can bake a customer's name into a saved template file | R/wizard_auto.R:33-37 `.fp_is_customer` only rejects a line that STARTS with a personal title, or a bare 1-4 word all-alpha line with no branding word |
| 47 | HIGH | OVERSTATED | M | **fixed** | The forensic audit trail's "who converted this" is either constant for the whole department or freely typed by the user | scripts/run_app.R:36 (`shiny::runApp(app_dir, host = "0.0.0.0", ...)` — ONE server process, browsers connect from other machines per docs/operational/ |
| 26 | MEDIUM | CONFIRMED | S | **fixed** | The admin-authorisation invariant test matches only `observeEvent(input$adm_`, and every admin surface outside that pattern is in fact ungated — real statement filenames reach any browser | tests/testthat/test-admin_auth.R:12 greps `observeEvent\(input\$adm_` and checks a 4-line window for `req(admin_ok())`. Outside that pattern and all m |
| 32 | MEDIUM | CONFIRMED | S | **fixed** | uploads/ retains every real client statement forever — no retention policy, no purge, no admin delete | R/uploads.R:20-21 copies each uploaded statement into uploads/<id>/ on every Convert (app.R:1644-1651, record=TRUE by default); R/retention.R:10-33 ro |
| 37 | MEDIUM | OVERSTATED | S | **fixed** | Ungated download handlers read files chosen by an unsanitised, client-supplied id — cross-user statement access and path traversal | R/uploads.R:79-83 `upload_file_path()` builds `file.path(.uploads_dir(dir), id)` with zero sanitisation; R/inbox.R:35-38 `failed_file_path()` does the |
| 51 | MEDIUM | OVERSTATED | S | **fixed** | Admin authorization is enforced only on write handlers; the read surface — including filenames of failed statements — is ungated and partly pushed to every session | tests/testthat/test-admin_auth.R only greps `observeEvent(input$adm_` and the `adm_data()` loader, so the invariant it proves is narrower than it read |
| 60 | MEDIUM | CONFIRMED | S | **fixed** | A typo in config.yaml silently reverts every setting to shipped defaults, including the admin password and the Qlik feed location | R/config.R:123 `fromfile <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)` and :124 `if (is.list(fromfile)) …` — an unparseable file is di |
| 22 | LOW | OVERSTATED | S | **fixed** | Withheld conversions write full transaction rows, including account_number, into the folder Qlik is connected to | R/feed.R:112-127 builds the stamped rows — `account_number = h$account_number` at R/feed.R:121 — and R/feed.R:124 sends them to `rev_dir` (feed/review |
| 7 | MEDIUM | CONFIRMED | M | **fixed** | Nothing prunes the two places that hold raw client statements — uploads/ and the per-conversion temp dirs grow forever | `R/uploads.R:24` copies every uploaded statement into `uploads/<id>/` and keeps it (`uploads.R:5` — "the saved files hold real statements"). `R/retent |

## Deployment

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 33 | HIGH | CONFIRMED | S | **fixed** | Shiny's default 5 MB upload cap is never raised — scanned PDFs, the dominant input, are rejected at the door on day 1 | `grep -rn maxRequestSize` over the whole repo returns nothing (exit 1). `scripts/run_app.R:36` calls `shiny::runApp(app_dir, host="0.0.0.0", port=port |
| 35 | HIGH | CONFIRMED | S | **fixed** | Every update silently wipes the admin-edited label dictionary and recognition lexicon | scripts/bundle-offline.R:56-57 ships `dictionaries` in every bundle; app.R:964-967 and app.R:1006-1012 write admin edits in place to `dictionaries/lab |
| 54 | HIGH | CONFIRMED | S | **fixed** | A scanned PDF on a box without OCR tools reports "No template for this statement yet" plus a nonsense diagnostic, and sends Beth into a toolkit where she can never succeed | R/read_pdf.R:230 gates OCR on `ocr_available()`, which is R/ocr.R:12-13 `nzchar(Sys.which("tesseract")) && nzchar(Sys.which("pdftoppm"))` — both absen |
| 8 | MEDIUM | CONFIRMED | S | **fixed** | The bundle build prints DONE and exits 0 even when the R installer and OCR prereqs are missing — deepening the known proxied-build failure | `scripts/bundle-offline.R:102-108` defines `grab()` to `cat` a message and return FALSE on any download error; the three call sites (`:115-116` R, `:1 |
| 30 | MEDIUM | OVERSTATED | S | **fixed** | Both first-run repair remedies in first-time-setup.md are no-ops — the .installed marker is written unconditionally | RUN-ME.bat:43 `if not exist "%BUNDLE%\.installed" call :firstRun`; RUN-ME.bat:68-73 runs install-on-pc.R then `type nul > "%BUNDLE%\.installed"` with  |
| 39 | MEDIUM | CONFIRMED | M | **fixed** | No backup or restore procedure exists for the analyst-created templates, the edited dictionaries, or the metadata corpus | The only backup anywhere is `RUN-ME.bat:79-93`, which copies `config/config.yaml` to `%LOCALAPPDATA%\StatementStudio` — on the same machine. `grep -rn |
| 44 | MEDIUM | CONFIRMED | S | **fixed** | Nobody watches the feed: it can stop filling entirely and nothing in the app, the logs or Admin would say so | app.R:1653 `if (record) safe(write_feed(res, CONFIG))` discards the gate result that write_feed carefully returns (R/feed.R:150); every write inside i |
| 53 | MEDIUM | CONFIRMED | S | **fixed** | The documented Task Scheduler recipe kills the app after 3 days and never restarts it if R dies | `docs/operational/running-and-keeping-it-up.md:17-29` gives a five-step Create Task recipe: General → Triggers (At startup) → Actions (RUN-ME.bat) → O |
| 2 | HIGH | CONFIRMED | S | **fixed** | An upgrade silently overwrites the Admin-edited label dictionary and recognition lexicon — the same statement can convert differently after an update, with no notice | `scripts/bundle-offline.R:56-59` ships `dictionaries` inside the dist folder. Those exact files are live, server-owned, Admin-edited state: `app.R:965 |
| 14 | MEDIUM | OVERSTATED | S | **fixed** | make-bundle.bat reports success even when the R / Poppler / Tesseract downloads failed | scripts/bundle-offline.R:102-108 — `grab()` catches every download error, prints a one-line 'skipped', and returns FALSE; :115-116 both R-installer at |

## Analytics/Qlik

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 17 | HIGH | CONFIRMED | S | **fixed** | The governed Qlik feed re-parses the output CSV with type inference, corrupting references, codes and descriptions | R/feed.R:111 `utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)` — no `colClasses = "character"`, so type.convert runs per column. T |
| 38 | HIGH | CONFIRMED | M | **fixed** | A feed write that never lands is invisible everywhere — and the documented UNC-share + scheduled-task setup makes that the likely default | `R/feed.R:22-29` writes via temp-then-`file.rename`, returning FALSE on any failure. The manifest call at `R/feed.R:148` discards that return entirely |
| 56 | HIGH | CONFIRMED | S | **fixed** | The same over-broad ASB fingerprint permanently withholds every real ANZ PDF statement from the governed feed | On the real statement samples/_private_staging/anz_0382025_jun.pdf: detection scores `anz_everyday_pdf` 3 vs `asb_everyday_pdf` 2, so `det$margin == 1 |
| 4 | MEDIUM | OVERSTATED | S | **fixed** | The feed's default trust floor (medium) admits exactly the runs the engine says it could not prove | R/config.R:43 and config/config.example.yaml:46 `min_trust: medium`; R/feed.R:35-43 `.trust_ok` accepts high/medium. Trust is capped at medium — never |
| 12 | MEDIUM | OVERSTATED | S | **fixed** | feed/review rows are byte-schema-identical to accepted rows with nothing marking them withheld — Qlik will auto-concatenate them into the dashboard | R/feed.R:114-128 builds ONE `stamped` frame and writes it to tx_dir or rev_dir with no discriminating column; the ctx block (feed.R:114-122) has no ga |
| 58 | MEDIUM | CONFIRMED | M | **fixed** | The feed's field set changes per template, but the documented Qlik load is one wildcard LOAD assuming a fixed schema | Live in the repo: feed/transactions/136c69b731ca8698.csv has 26 columns ending `currency,flags`; feed/transactions/bbf83b15b965ddbd.csv has 28, ending |

## Coverage/Data

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 11 | HIGH | CONFIRMED | M | **fixed** | No production PDF template has a golden extraction test; asb_everyday_pdf has no test at all and the statements they were built from are gitignored | tests/testthat/expected/ holds 7 golden CSVs: 6 delimited + tutorial_everyday_pdf.csv. `asb_everyday_pdf` appears in no test file. `anz_everyday_pdf`  |
| 27 | HIGH | CONFIRMED | L | **fixed** | The scanned/OCR path is not production-ready on real bank scans, and no test or metric would reveal it | Run against the three real scans in samples/_private_staging/scans/: westpac_scan.pdf -> `ok`, reconciles (6297.94 + 430.69 = 6728.63), 11 rows, 18 s. |
| 42 | MEDIUM | CONFIRMED | S | **fixed** | The shipped ASB CSV template is calibrated to a 2014 export and cannot read the current ASB export already sitting in the repo's own sample corpus | templates/asb_everyday_csv.yaml:16 pins `date: {source: Date, format: "%Y/%m/%d"}`, matching samples/raw/asb/asb_transaction_export_01.csv (dates `201 |
| 55 | MEDIUM | CONFIRMED | M | **fixed** | app.R is 2,938 lines the test suite never loads — including the entire no-code 'add a bank' mapping | tests/run_tests.R:21-23 sources only `R/*.R`; app.R, ui_content.R and ui_labels.R are never sourced. The only two tests that mention app.R (tests/test |
| 57 | LOW | OVERSTATED | S | **fixed** | Admin bulk 'convert & save' and the CLI never write the feed — statements vanish from Qlik AND from the coverage manifest | app.R:1066-1070 calls convert_statement() directly inside the Admin bulk-audit loop and never calls write_feed; run.R:29-34 (the CLI) likewise; write_ |

## UX/Adoption

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 13 | HIGH | CONFIRMED | S | **fixed** | Shiny's default 5 MB upload cap is never raised, so the dominant input — scanned PDFs — is rejected outright | `options(shiny.maxRequestSize=…)` / `shinyOptions` appear nowhere in the repo (verified across app.R, run.R, scripts/run_app.R, RUN-ME.bat, .Rprofile  |
| 18 | HIGH | CONFIRMED | S | **fixed** | A template Beth builds is switched OFF for auto-detection by default — the app never says so, and the save message tells her to do something that cannot work | R/config.R:18 (`user_templates_default = FALSE`; no config/config.yaml exists, so this default is live); app.R:365 the only opt-in is `checkboxInput(" |
| 24 | MEDIUM | CONFIRMED | S | **fixed** | The save-and-reconvert step forces the template by id, so the one moment the app could prove the new template auto-detects is the one moment it bypasses detection | app.R:2851-2856 — after `save_user_template` succeeds the app calls `run_conversion(gp, gn, record = FALSE, force_tpl = saved_id)`; R/convert.R:47-53  |
| 25 | MEDIUM | CONFIRMED | M | **fixed** | When the drafted PDF fingerprint is too generic, Save is refused and the only place to fix it is a raw YAML textarea | R/templates.R:44-47 makes a generic-or-empty `page_contains_all` a hard validation failure, and `save_user_template` (R/templates.R:276-277) stops on  |

## Performance/Scale

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 49 | HIGH | OVERSTATED | M | **fixed** | One blocking R process: a single scanned statement freezes the whole department for minutes, with no queue and no backpressure | scripts/run_app.R:36 (`shiny::runApp(app_dir, host="0.0.0.0", port=port)` — one process for everyone, launched by RUN-ME.bat:49); repo-wide grep finds |
| 52 | HIGH | CONFIRMED | L | **fixed** | One bare R process serves the whole department — a single OCR conversion freezes every other user's browser, and the architecture doc's justification no longer applies | `scripts/run_app.R:36` is a bare `shiny::runApp(host="0.0.0.0")` — one R process, one thread, all sessions. `docs/operational/running-and-keeping-it-u |
| 59 | HIGH | CONFIRMED | S | **fixed** | Three always-on observers re-scan unbounded on-disk state on every session connect and after every conversion, with no admin gate | app.R:1022-1025 `observe({ toks <- adm_suggestions()$… })` forces `lexicon_suggestions(LOGDIR)` (R/suggestions.R:17-23), which `fromJSON(simplifyVecto |
| 29 | MEDIUM | CONFIRMED | S | **fixed** | Every OCR'd page leaks two full-page images of the client statement into the R temp dir, never deleted | R/ocr.R:85-86 cleans only the pdftoppm renders (`prefix <- tempfile("ocrpg_")`; `on.exit(unlink(Sys.glob(paste0(prefix, "*"))))`). The two preprocessi |

## Docs/Handover

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 41 | HIGH | CONFIRMED | M | **fixed** | Three of the five 'proven' PDF templates have no fixture and no parse test, while README and the HOWTO state that every template ships with one | `templates/` holds 13 templates; `tests/testthat/expected/` holds 7 goldens. anz_everyday_pdf, asb_everyday_pdf and westpac_everyday_pdf are never par |
| 45 | HIGH | CONFIRMED | S | **fixed** | Nothing anywhere records which build of the engine produced a figure | No VERSION file, CHANGELOG or git tag (`git tag` empty; `ls` shows none). No engine-version field in the run record (logs/runs/*.json fields: ts, run_ |
| 23 | MEDIUM | CONFIRMED | S | **fixed** | Nothing tells the analyst to open the Windows Firewall port — "everyone opens a browser" fails for everyone except the person standing at the server | `scripts/run_app.R:36` binds `host="0.0.0.0"` on port 8100. `docs/operational/first-time-setup.md:58-61` and `running-and-keeping-it-up.md:12` both sa |

## Product/Capability

| ID | Severity | Verified | Effort | Status | Finding | Evidence |
|---|---|---|---|---|---|---|
| 31 | HIGH | CONFIRMED | M | **fixed** | No department-scale convert path: bulk convert is admin-only, throws its outputs into a temp dir, and never writes the Qlik feed | app.R:353 — Convert's fileInput has no multiple=TRUE (one file per click); app.R:728 — the only multi-file input is inside the password-gated Admin ta |
| 61 | HIGH | CONFIRMED | S | **fixed** | The bundle auto-split subsystem is shipped, tested and completely dark — no template opts in and there is no way to opt one in without hand-editing YAML | R/split.R (247 lines) + tests/testthat/test-split.R; grep for a `split:` key across templates/, templates_user/, templates_seed/, fields_templates/ re |
| 10 | MEDIUM | OVERSTATED | S | **fixed** | A new bank whose name is not in a hardcoded 25-word brand list is classified as a customer name and dropped from the auto-drafted fingerprint | R/wizard_auto.R:24-27 defines `.FP_BRAND_RX`, a literal regex of ~25 words including the NZ bank names (kiwibank/westpac/anz/asb/bnz/tsb/sbs/rabobank/ |
| 46 | MEDIUM | CONFIRMED | S | **fixed** | The PDF's own document metadata (producer, creation/modification dates, encryption) is never read — the cheapest genuinely forensic capability left unbuilt | R/read_pdf.R:198-208 calls only pdftools::pdf_text, pdf_data and pdf_pagesize; pdftools::pdf_info is never called anywhere in the repo. Verified live  |
| 43 | LOW | OVERSTATED | M | **fixed** | The gate is machine-only and one-way: a needs_review conversion the analyst verifies as correct can never reach the dashboards | R/feed.R:52-53 (require_status_ok rejects anything that is not status=='ok') and R/feed.R:54-55 (trust floor); R/feedback.R has no feed interaction; n |
| 62 | LOW | OVERSTATED | S | **fixed** | Downstream categorisation cannot start: it is blocked on a taxonomy nobody has requested, and the legacy-parity gap it closes is absent from the roadmap | docs/context/architecture/legacy-qlik-mapping.md:60 ("Extraction parity is essentially met... The gap is the enrichment layer") and :81-85 ("The descr |

## New findings (append below)

_Anything discovered after the review lands here, newest at the bottom._

### Found while fixing (2026-07-26)

| # | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N1 | high | **fixed** | An invalid template was dropped at load with only an R `warning()` nobody surfaces — so tightening fingerprint validation would have made an analyst's saved template vanish after an update with no explanation. Failing closed is right; failing closed *silently* is not. `load_templates` now returns its skip reasons. | R/templates.R load_templates |
| N2 | medium | **fixed** | Two further admin download handlers were ungated beyond the ones the review named — found only because the widened invariant test was written before the fix. | app.R adm_ba_report / adm_ba_csv / adm_audit_dl |
| N3 | medium | **fixed** | The backfilled PDF goldens were generated but no test consumed them, so #11/#28/#41 looked closed while the parse was still unguarded. Now wired via `expect_statement_ok` plus a guard test that every proven template has a golden. | tests/testthat/test-pdf_template_goldens.R |
| N4 | low | **fixed** | `templates/asb_everyday_csv.yaml` still pins a single `%Y/%m/%d`, so the current ASB export in the repo (`asb_transaction_export_03.csv`, `13/10/2025`) reads every date as NA. Needs the candidate-format-list change (#42). | templates/asb_everyday_csv.yaml:16 |

### Found while fixing, round 2 (2026-07-26)

| # | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N5 | high | **fixed** | `.atomic_write_csv` treated a WARNED-about write as success — the exact read-only-share case it exists for, because `safe()` catches errors but not warnings. | R/feed.R |
| N6 | high | **fixed** | `record_upload()` could merge two uploads into ONE record folder (same file, same second), silently losing the first one's lifecycle history. | R/uploads.R |
| N7 | high | **fixed** | `save_metadata_config()` would WIPE a config.yaml it could not parse — an unreadable file was treated as an empty one, so saving the Admin toggle destroyed every other setting. | R/config.R |
| N8 | high | **fixed** | The `scanned_no_ocr` diagnostic added earlier could never fire in production: the field was produced but not threaded to the consumer. | R/read_pdf.R / R/diagnose.R |
| N9 | high | **fixed** | The REAL cause of #27: OCR reads a tightly-set date column as one glued token ("20Apr"). `as.Date` parses it correctly but `.date_canon` did not normalise the digit/month-name boundary, so the round-trip guard rejected it and the row was dropped. Measured: ANZ scan 41 → 300 rows; Westpac unchanged (strict no-op). | R/normalise.R |
| N10 | high | **fixed** | Multi-format date support would have been silently narrowed back to one format by simply opening the toolkit and pressing Save. | app.R `.datefmt_unchanged` |
| N11 | medium | **fixed** | `validate_lexicon()` ERRORED on an unknown category instead of reporting it — an admin typo crashed the validator whose job is a readable message. | R/lexicon.R |
| N12 | medium | **fixed** | The drafted-fingerprint brand vocabulary was documented as dictionary-driven but inert — the category was never registered, so `lex()` rejected it and the built-in list silently won. | R/lexicon.R |
| N13 | medium | **fixed** | Trailing all-delimiter padding rows were emitted as flagged "transactions" (24 phantom rows on a real ASB export). | R/read_delimited.R |
| N14 | medium | **fixed** | RUN-ME.bat's failure path exited 0, so a scheduled task recorded a clean run for a launch that never started; install-offline.R likewise reported success when packages failed. | RUN-ME.bat / scripts/install-offline.R |
| N15 | medium | open | A single distinctive-but-ubiquitous phrase still passes the form fingerprint gate (any two-word phrase counts as distinctive). Narrower than the original hole but not closed. | R/forms.R / `.fp_specific` |
| N16 | medium | open | Recovering the ANZ scan's rows exposes residual OCR digit errors, so it still fails reconciliation — correctly, and loudly. Improving scanned digit accuracy is the next real coverage step. | measured: 300 rows, reconciliation fails |
| N17 | low | **fixed** | ImageMagick can spill a disk-backed pixel cache (~83 MB) of a statement page under memory pressure, and only clears it when the PROCESS exits — never, on a server up for months. `with_image_scratch()` now gives each piece of image work a private folder and deletes it. | R/util.R + R/ocr.R, R/detect_redaction.R, R/read_input.R |
| N18 | low | **fixed** | The local `feed/transactions/136c69b731ca8698.csv` is a stale pre-fix artifact in the old 26-column schema. | gone; `feed/` is local-only and git-ignored |
| N19 | low | open | The real ANZ bundle reaches trust MEDIUM (not HIGH) after the split opt-in — a correction to the original #61 measurement. | samples/_private_staging/anz_multiple.pdf |

| N20 | high | open | The zero-row re-read is a CLIFF, not a measure. A template that reads 5 rows of a 500-row statement is exactly as wrong as one that reads 0, and passes every check: `transaction_count` only requires `n > 0` when the statement prints no stated count. | R/convert.R `empty_first`, R/reconcile.R:179 |
| N22 | high | **fixed** | The toolkit could not repair a shipped template: the fix saved as `<id>_custom` with the same fingerprint, tied on every statement, and lost the tie-break to the very template it was correcting. It now saves `refines: <id>` and detection ranks a correction one step above the single template it names. | app.R `g_save` / R/detect.R |
| N21 | medium | open | When a conversion is obviously wrong, the route to "this isn't right, build the right one" is not offered on the result that shows it. | the Convert result page |

### N20 - reading 5 of 500 is the same defect as reading 0

Raised by the owner: *"What about when one reads 5 and other 500?"*

The re-read added in this session triggers on `nrow(transactions) == 0`. That was
the reported symptom, not the defect. The defect is **a template that reads a
small fraction of what is on the page**, and 5-of-500 is the dangerous version,
because 0 rows is obviously broken while 5 rows looks like a statement.

It currently passes everything: `.kpi_transaction_count` (R/reconcile.R:179)
requires only `n > 0` when the statement prints no stated count, and balance
reconciliation is skipped for the same reason a thin parse happens. So the run
can come back **ok**, feed the dashboards, and be missing 99% of the money.

**The tool already knows.** `row_coverage()` (R/row_coverage.R) counts, for a PDF,
how many visual rows were seen and how many were kept, with a reason per rejected
row. Delimited files carry `source_line_count`. Nothing new has to be computed -
the number is measured and then not used to judge anything.

**The fix is a deletion, not an addition.** Replace the `== 0` cliff with the
measure that already exists, and drop the hidden re-read entirely:

  * A parse that keeps a small share of the rows it saw is `needs_review`, and the
    verdict says so in her words: *"read 5 rows, but there are about 500 on this
    statement"*. That is a question an accountant CAN answer - she is holding the
    statement - so it is the right question to put to her.
  * `PARAM_DETECT_MAX_REREADS`, the re-read loop, `rescued_from`, its message and
    its diagnostic all go. One honest number replaces a hidden mechanism, which
    is also the more defensible design: nothing is quietly swapped behind her.

Net: **fewer concepts, and it covers the case the loop never did.**

### N21 - the fix path belongs on the result that shows the problem

Raised by the owner: *"if it reads 5, is there a super quick not right, create new,
pathway?"*

The pathway exists (`cv_teach_go` opens the toolkit seeded from this statement),
but it is offered when a layout is UNRECOGNISED - not when a recognised template
produced an obviously wrong answer, which is the case N20 exposes. On a thin
parse the prominent action should be *"That's not right - set up the right one"*,
seeded from this statement and this template, so the analyst is one click from
the band editor with the wrong bands already on screen. Depends on N20 landing
first: without the measure there is nothing to hang the offer on.

| N23 | high | open | **Batch conversion.** Confirmed as a real working pattern: a case arrives as a folder of 10-50 statements, banks anywhere from all-same to all-different. Agreed shape: a second upload mode on Convert; one row per file (status, bank, rows, the failing check), sortable so every failure of the same kind can be fixed together; click a row to open that file's normal result; a file that cannot be read never blocks the rest - push through and report at the end. Today batch intake exists only in Admin, which nobody using the app can open. | Convert |
| N24 | medium | open | Surface `effective_from` / `effective_to` in the toolkit. The same bank and product in 2020 vs 2024 is a real variant; the schema already supports a date range but nothing shows it, so the only route is two rival templates that tie forever. Owner: "same would be good but plan for both." | app.R toolkit |
| N25 | low | open | JSON download is unused (CSV and Excel only). Demote it to a small link rather than a third equal button. | Convert |

| N26 | medium | open | The band editor commits the box WHILE you drag, so it re-detects the column on every mouse move and small positional corrections are almost impossible. It should draw on mouse RELEASE. Reported from real use; given that a mis-drawn band is the commonest cause of a wrong amount column, this is a correctness problem wearing a usability costume. | app.R, the PDF band editor brush |

| N27 | medium | open | The save name is generated from the BANK alone, so every layout from one bank drafts the same name and the second one collides or gets a "_2" suffix. It should be built from bank AND the kind of statement (`g_bank` + `g_type`), which the toolkit already asks for and already stores as `statement_type`. Directly worsens the near-duplicate problem: templates that cannot be told apart by name are exactly the ones that tie in detection. | app.R `g_id` / `g_bank` / `g_type` |

| N28 | high | open | "None of these fit? Tell our team" (`g_req_detail` / `g_req_send`, app.R:3286-3290) sits INSIDE the settings disclosure on the Simple tab, so to a user it appears only on Advanced. It is the escape hatch for somebody already stuck, and the tool then makes them hunt for it. Worse, picking "None of these" in the date or amount dropdown pops a notification saying "use the Tell our team box below" - and the box it names is not visible. It belongs in front, on Simple, always. | app.R:3286 |

| N29 | **high** | open | Two symptoms reported from real use, probably ONE cause: (a) every second page shows the DEFAULT box positions rather than the ones drawn; (b) a credit column with a box drawn over it reads nothing, while the debit column beside it reads fine. | app.R band editor + R/parse_pdf_table.R |

### N29 - bands on the wrong page, and a credit column that reads nothing

Reported together, and the leading hypothesis explains both at once.

The PARSER does not match bands against a page's raw coordinates. Every page's
words are first scaled to a REFERENCE page size (`.scale_words_to_ref`,
R/parse_pdf_table.R:81, `ref_width` defaulting to A4 at :349-359), so a band is
stored in reference coordinates, not in the coordinates of the page you drew it on.

**If the band EDITOR draws boxes at raw template coordinates over a page rendered
at that page's own size, then any page whose width differs from `ref_width` shows
every box displaced** - which looks exactly like "the defaults came back", because
a displaced box is indistinguishable from an untouched one. Alternating pages
differing in size is common: a letterhead page, a landscape insert, or a scan
where every second sheet was fed at a slightly different scale.

The same displacement explains the credit column. It is usually the NARROWEST
money band, so it is the first to fall outside its words when the scale is even
slightly off, while the wider debit band beside it still catches them. That is
precisely the reported shape: "picking up debits next to it but no credits."

**Check this first, it is cheap:** `row_coverage()` already reports `ref_width`,
`ref_height` and `any_page_rescaled`, and gives per-page kept counts. Run it on
the offending statement. If `any_page_rescaled` is TRUE, or the per-page kept
counts alternate high/low, the hypothesis is confirmed and the fix is to make the
editor draw in the same reference frame the parser matches in - one frame, defined
once, used by both.

If `any_page_rescaled` is FALSE the hypothesis is wrong and the credit miss is a
separate band/alignment problem: check whether right-aligned credit amounts extend
past the drawn band's right edge, since a word is assigned by its x position.

Filed HIGH: a mis-drawn band is the reported commonest cause of a wrong amount
column, and a *silently* mis-placed one is worse than a visibly wrong one.

| N30 | **critical** | open | Opening/closing balances are kept as TRANSACTIONS, with the balance figure landing in the debit column ("Balance -9000 debit"). Invents large wrong transactions rather than losing data. Same root cause as N29, plus a second failure that turns a displaced band into a phantom row. | R/parse_pdf_table.R:184-186 |

### N30 - a balance read as a nine-thousand-dollar debit

The owner's diagnosis is right, and it is worse than one bug: N29's displacement
also DISABLES the guard that exists to stop exactly this.

`.pdf_is_summary` (R/parse_pdf_table.R:184-186) decides whether a row is a summary
line - an opening/closing balance, a carried-forward, a total - and it reads the
DESCRIPTION band, falling back to the whole raw line only when that band is EMPTY:

    d <- tolower(trimws(description %||% ""))
    if (!nzchar(d)) d <- tolower(trimws(raw %||% ""))

With bands displaced, the description band does not come back empty - it comes
back with the WRONG words (a date fragment, part of a number, a neighbouring
column). Non-empty and wrong. So the raw fallback never runs, "Opening balance" is
never seen even though it is sitting right there in `raw`, and the row is kept.

The chain, end to end:
  1. bands displaced on a page whose width differs from ref_width  (N29)
  2. -> description band captures the wrong words, but is not empty
  3. -> .pdf_is_summary never falls back to raw, so the label is missed
  4. -> the row passes .is_txn and is kept as a transaction
  5. -> the balance figure sits in the displaced debit band: "-9000 debit"
  6. -> balance_reconciliation would normally catch a phantom of that size...
  7. -> ...except on a multi-period statement reconciliation is already failing
        or "na", so the last net is down too

Every layer of defence fails from one displacement, and the output gains
transactions that never happened, at material amounts.

**TWO SEPARATE FIXES, and the second is worth doing whatever N29 turns out to be:**

  A. Fix the displacement (N29) - one coordinate frame for editor and parser.

  B. HARDEN THE GUARD INDEPENDENTLY. Check `raw` ALWAYS, not only when the
     description band is empty. A line that says "Closing balance" IS a summary
     line whatever the description band happened to catch; there is no case where
     reading a wrongly-captured fragment INSTEAD of the full line is better. That
     is a one-line change and it would have contained this bug even with the bands
     displaced. Defence in depth: A stops it happening, B stops it mattering.

Also worth a test that no kept transaction's description or raw matches
.PDF_SUMMARY_LABELS - an invariant that would have failed loudly the first time
this happened.

| N31 | **high** | open | The form builder ("Something else") opens with the ABSTRACT step and buries the concrete one. First thing on screen is a blank box: "The values to pull out - one per line" (app.R:608) - it asks you to already know the answer. The obvious, teachable step, drawing a box on the page, is at the bottom under "optional" (app.R:635). Needs reordering, not rewording. | app.R:596-640 |

### N31 - the form builder asks the hard question first

Its order today, top to bottom:

    598  "Name each value you want pulled out and the wording printed next to it"
    605  a distinctive phrase, one per line          (abstract)
    608  THE VALUES TO PULL OUT - ONE PER LINE       (abstract, and blank)
    616  "Value printed away from its wording?"      (an edge case, before the norm)
    635  "Draw a box to place a value (OPTIONAL)"    (the concrete step, last, and
                                                     labelled optional)

An accountant opening this has a document in front of her and no idea what a
"value to pull out" is called. The first thing asked is the one thing she cannot
answer without already having done the task; the one thing she CAN do - point at a
number on the page - is at the bottom, marked optional, after an edge case about
values printed away from their label.

This is the same mistake as the date format and the escape hatch, in its strongest
form: what a person does FIRST is placed LAST, and the abstract step that only
makes sense afterwards is placed first.

WHAT IT SHOULD BE. The page leads, exactly as the statement toolkit now does:
  1. Here is your document. Draw a box round a value you want.
  2. What is this? (a name, in her words - the tool offers what it read nearby)
  3. Repeat for the two or three that matter.
  4. Preview: here is what will come out.
  5. Name it and save.
The typed list becomes what it should always have been - a shortcut for somebody
who already knows the field names, behind the settings disclosure, not the front
door. "Value printed away from its wording" stops being a section: it is simply
what happens when the box you drew is not beside its label, and the tool can say so.

Filed high. The form path is the one an accountant meets with no template to fall
back on and no reconciliation to catch a mistake, so the flow has to carry her.

### What is holding the last three open

None of them is blocked on effort or on a decision. Each is blocked on **evidence
we do not have**, and the charter's rule — never silently wrong — says a change we
cannot measure is worse than the known, loud, honest failure it replaces.

**N15 — the form fingerprint gate.** Tightening "how distinctive must a phrase be
before a form template may claim a PDF?" needs a spread of real forms to tune
against. There is exactly **one** shipped fields template
(`fields_templates/anz_kiwisaver_fields.yaml`). A threshold fitted to a sample of
one either stays where it is or starts rejecting legitimate templates nobody has
written yet — and the failure mode of over-tightening (an analyst's correct
template silently refuses to match) is the same class of harm as the hole itself.
*Unblocks when:* three or four more real forms are in the repo. The fix is then
half an hour in `.fp_specific`. Section 2 of
[survey-a-statement-with-ai.md](../operational/survey-a-statement-with-ai.md)
collects exactly this — candidate phrases plus an honest judgement of whether
another provider could print the same words — with no client detail in it, so a
stack of real forms can be surveyed without any of them leaving the building.

**N16 — residual OCR digit errors on recovered scans.** We now recover 300 rows
from the ANZ scan that used to yield 41 (N9). Some digits are still misread, so
reconciliation fails — **correctly and loudly**: the run is withheld from Qlik,
marked needs-review, and no wrong figure reaches a dashboard. Improving this means
changing preprocessing (thresholding, DPI, per-cell re-OCR), and there is no way
to tell an improvement from a regression without **ground truth**: the three scans
in `samples/_private_staging/scans/` have no verified correct values attached. A
blind tweak could raise the row count while lowering digit accuracy, which turns a
loud failure into a quiet wrong answer — the one outcome the charter forbids.
*Unblocks when:* one scan is keyed in by hand as a golden. Then character accuracy
is measurable and preprocessing can be tuned against a number. Section 9.7 of
[survey-a-statement-with-ai.md](../operational/survey-a-statement-with-ai.md)
narrows the search first — it asks which digits are hard to read and why (tilt,
speckle, faintness, a 3/8 or 5/6 typeface confusion), so the golden gets keyed
for the scan that actually represents the problem.

**N19 — trust MEDIUM, not HIGH, on the split ANZ bundle.** This is a **correction
to a measurement**, not a defect: MEDIUM is the right answer for a statement whose
running balance does not span the whole bundle. Nothing to build; it is recorded
so the earlier #61 note is not read as a promise of HIGH.
