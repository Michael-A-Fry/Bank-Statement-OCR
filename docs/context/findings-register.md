# Findings register

Every verified finding from the full-codebase review, with its status. This is the
working backlog: fix, tick, and append anything new to the bottom.

**How it was produced.** Ten independent review lenses over the whole codebase produced
113 findings; the 63 rated critical/high went through an adversarial verification pass
(62 verdicts returned: 47 CONFIRMED, 15 OVERSTATED/downgraded, 0 rejected outright), and a
completeness critic added 8 more. Severities below are POST-verification. The remaining
51 medium/low findings were not individually verified and are not listed here.
N48 onwards came from later sweeps that drove the running app and followed the written
documentation, rather than reading code; every one was reproduced before it was fixed and
re-proved afterwards by an agent that had not made the change.

Status values: `open` · `fixed` · `not-a-defect` · `wont-fix` (with a reason).

**Where it stands: 222 findings, 216 fixed, 3 open, 1 wont-fix, 2 not-a-defect.** The three open are **N15**, **N16** and **N151**. N15 and N16 are blocked on **evidence we do not have** rather than on effort: each fails closed and loudly today, so neither can put a wrong figure on screen, and what would unblock them - three or four more real forms, and one scan keyed in by hand as a golden - is set out at the bottom of this file. N151 is open in the harmless direction and is pinned by a test that says so; see the end of this file. N47 is wont-fix with reasons, at the end.

N48-N79 came from a later sweep that asked a different question - not *is this code correct?*
but *does the code do what it says, and does anything on a screen tell a lie?* That sweep
found the worst defect in the register (N48, a wrong bank on a reconciled workbook) and the
one that most undercut the product's central claim (N51, the auto-drafter reading nothing on
two statements this repo ships proven templates for). Neither was reachable by reading code
against its own comments; both needed the code run against a real statement and the result
read as a stranger would read it. That is worth remembering when choosing the next sweep.

_Last updated: 2026-08-24._


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
| N19 | low | **not-a-defect** | The real ANZ bundle reaches trust MEDIUM (not HIGH) after the split opt-in - a correction to the original #61 measurement, not a task. Trust on a split bundle is the WEAKEST segment's by design, so medium is the correct answer, not a shortfall. Re-measured after the 1.2.0 engine changes: still medium, still correct. Recorded so the earlier claim is not quoted again; nothing to build. | samples/_private_staging/anz_multiple.pdf |

| N20 | high | **fixed** | The zero-row re-read is a CLIFF, not a measure. A template that reads 5 rows of a 500-row statement is exactly as wrong as one that reads 0, and passes every check: `transaction_count` only requires `n > 0` when the statement prints no stated count. | R/convert.R `empty_first`, R/reconcile.R:179 |
| N22 | high | **fixed** | The toolkit could not repair a shipped template: the fix saved as `<id>_custom` with the same fingerprint, tied on every statement, and lost the tie-break to the very template it was correcting. It now saves `refines: <id>` and detection ranks a correction one step above the single template it names. | app.R `g_save` / R/detect.R |
| N21 | medium | **fixed** | When a conversion is obviously wrong, the route to "this isn't right, build the right one" is not offered on the result that shows it. | the Convert result page |

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

| N23 | high | **fixed** | **Batch conversion.** Confirmed as a real working pattern: a case arrives as a folder of 10-50 statements, banks anywhere from all-same to all-different. Agreed shape: a second upload mode on Convert; one row per file (status, bank, rows, the failing check), sortable so every failure of the same kind can be fixed together; click a row to open that file's normal result; a file that cannot be read never blocks the rest - push through and report at the end. Today batch intake exists only in Admin, which nobody using the app can open. | Convert |
| N24 | medium | **fixed** | Surface `effective_from` / `effective_to` in the toolkit. The same bank and product in 2020 vs 2024 is a real variant; the schema already supports a date range but nothing shows it, so the only route is two rival templates that tie forever. Owner: "same would be good but plan for both." | app.R toolkit |
| N25 | low | **fixed** | JSON download is unused (CSV and Excel only). Demote it to a small link rather than a third equal button. | Convert |

| N26 | medium | **fixed** | The band editor commits the box WHILE you drag, so it re-detects the column on every mouse move and small positional corrections are almost impossible. It should draw on mouse RELEASE. Reported from real use; given that a mis-drawn band is the commonest cause of a wrong amount column, this is a correctness problem wearing a usability costume. | app.R, the PDF band editor brush |

| N27 | medium | **fixed** | The save name is generated from the BANK alone, so every layout from one bank drafts the same name and the second one collides or gets a "_2" suffix. It should be built from bank AND the kind of statement (`g_bank` + `g_type`), which the toolkit already asks for and already stores as `statement_type`. Directly worsens the near-duplicate problem: templates that cannot be told apart by name are exactly the ones that tie in detection. | app.R `g_id` / `g_bank` / `g_type` |

| N28 | high | **fixed** | "None of these fit? Tell our team" (`g_req_detail` / `g_req_send`, app.R:3286-3290) sits INSIDE the settings disclosure on the Simple tab, so to a user it appears only on Advanced. It is the escape hatch for somebody already stuck, and the tool then makes them hunt for it. Worse, picking "None of these" in the date or amount dropdown pops a notification saying "use the Tell our team box below" - and the box it names is not visible. It belongs in front, on Simple, always. | app.R:3286 |

| N29 | **high** | **fixed** | Two symptoms reported from real use, probably ONE cause: (a) every second page shows the DEFAULT box positions rather than the ones drawn; (b) a credit column with a box drawn over it reads nothing, while the debit column beside it reads fine. | app.R band editor + R/parse_pdf_table.R |

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

| N30 | **critical** | **fixed** | Opening/closing balances are kept as TRANSACTIONS, with the balance figure landing in the debit column ("Balance -9000 debit"). Invents large wrong transactions rather than losing data. Same root cause as N29, plus a second failure that turns a displaced band into a phantom row. | R/parse_pdf_table.R:184-186 |

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

| N31 | **high** | **fixed** | The form builder ("Something else") opens with the ABSTRACT step and buries the concrete one. First thing on screen is a blank box: "The values to pull out - one per line" (app.R:608) - it asks you to already know the answer. The obvious, teachable step, drawing a box on the page, is at the bottom under "optional" (app.R:635). Needs reordering, not rewording. | app.R:596-640 |

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

| N32 | medium | **fixed** | The converting/loading status is a small panel in a screen corner (Shiny's default `withProgress` placement), so people do not notice a conversion is running and click around while it works. It should be large and centred - unmissable - because a scanned statement can take tens of seconds and the page otherwise looks idle. | app.R:2086 (and 1472, 1478), CSS at app.R:187 |

### N32 - the user cannot tell it is working

`withProgress(message = "Converting statement...")` at app.R:2086 renders in
Shiny's default position: a small panel in a corner. On a scanned statement the
conversion can run for tens of seconds while the page looks completely idle, so
people click Convert again, switch tabs, or assume it has hung.

This is the tool failing to answer a question it definitely knows the answer to:
it knows exactly whether it is working and how far through it is.

The pieces already exist and are not being used for this: there is a dim+spinner
treatment for recalculating outputs (CSS at app.R:215-230), and withProgress
already reports real stages ("Reading the file and detecting its format...",
"Running checks and writing outputs..."). What is missing is placement and size.

WHAT IT SHOULD BE: a large, centred, unmissable overlay for the duration of a
conversion, carrying the stage text that is already being produced. It must cover
all three withProgress sites (single convert at :2086, and the two batch/audit ones
at :1472 and :1478) - a batch of 30 files is the case where "is it doing anything?"
bites hardest. Style it once in the design tokens rather than three times.

| N33 | high | **fixed** | A page footer was folded into the PREVIOUS transaction's description, silently rewriting text the tool promises verbatim. Found by the N30 agent while testing, not reported from use. | R/parse_pdf_table.R `.is_footer_noise` |

### N33 - the statement's own wording, rewritten

`doc.pdf` prints `Carried Forward to next page` at the foot of a page. That is a
date-less, money-less text line, which is exactly the shape of a WRAPPED
DESCRIPTION, so the continuation merge folded it into the last real transaction
above it:

    To 63A Rent Rent 63A Sar                -> To 63A Rent Rent 63A Sar Carried Forward to next page
    2025 Wellington Golden Takeaway         -> 2025 Wellington Golden Takeaway Carried Forward to next page

Measured on the real file: 2 of 79 descriptions on `doc.pdf`, 0 after the fix,
with the row count unchanged at 79 - nothing was lost, but two rows said something
the statement never said.

**No figure was wrong, which is exactly why nothing caught it.** Every check this
tool has is about money and dates; none of them reads the description. A forensic
reviewer quoting that line in a report would be quoting the tool, not the bank.

THE TRAP, worth stating because the obvious fix is the wrong one: a bare
`Carried forward` IS a real summary line, matched as a WHOLE label by
`.pdf_is_summary`. Loosening that anchor so it also catches the footer is
precisely what would start eating `Total Payments to ACME Ltd`. So the footer
rule stays separate and REQUIRES the page words (`to next page`, `from previous
page`); the summary rule is untouched. Both directions are pinned by test.

| N34 | medium | **fixed** | The X-ray at template-creation time reports a DIFFERENT row count from the conversion that follows it, with nothing saying why - so the number people check the template against is not the number they get. Either make them the same or say "showing the first X rows". | app.R X-ray / R/inspect.R |
| N35 | medium | **fixed** | "Statement or other" is asked once and then cannot be revisited: having picked one, there is no way back. The stickiness is right for other things; here it strands anyone who picks wrong on their first go. | app.R, the toolkit's first question |
| N36 | **high** | **fixed** | A year-less date format ("October 12") reads NOTHING when the statement prints no period the tool can parse AND more than one distinct year appears on the page. Every date comes back blank. It fails CLOSED (rows are kept, flagged `date_unresolved`), so no figure is wrong - but the screen never says the real reason, which is that no YEAR could be found, not that the dates could not be read. | R/parse_pdf_table.R:544-569 |
| N37 | **critical** | **fixed** | On a credit-card statement that prints CR on the line BELOW the amount, the CR is captured into the DESCRIPTION instead of the amount, so every credit is emitted as a debit. Silently wrong figures, in the sign - the cardinal failure. | R/parse_pdf_table.R, the row grouping / continuation merge |

### N36 - the dates are not unreadable, the YEAR is missing

Reproduced deterministically. A month-first year-less statement ("March 30",
"April 10", "September 5") with `date_format: "%B %d"`:

| the page prints | rows kept | dates |
|---|---|---|
| a period the tool can parse | 3 | all three correct, no flags |
| no parseable period | 3 | all `NA`, flagged `date_unresolved` |

So the format is fine and the parser is fine. The year is the whole problem, and
it has exactly two sources today (`R/parse_pdf_table.R:544-569`): the parsed
statement period, or - only if that fails - **one single distinct 4-digit year in
the entire page text**. A credit-card statement routinely prints several (payment
due date, card expiry, a copyright line), so the unambiguity test fails and no
year is attached at all.

Refusing to guess is RIGHT and must stay. Two things are wrong around it:

1. **The screen blames the wrong thing.** `date_parse` says "dates couldn't be
   read". They were read perfectly; what is missing is a year. The tool knows
   exactly which of those two happened and says the less useful one - the
   interface rule, broken in the direction that sends the analyst to fix a date
   format that was never the problem.
2. **A third deterministic source is available and unused.** A labelled statement
   date ("Statement date", "Closing date", "Issue date") is a fact printed on the
   page, read through the same label dictionary as everything else. It should be
   tried BEFORE the desperate "one year in the whole text" scan - reading the
   document's own structure, not counting digits.

### N37 - a credit read as a debit

The reported shape: the amount prints on one line and its `CR` marker on the line
BELOW it. The row grouper puts the marker on the following visual row, so the
amount cell never sees its own CR and `.num` has nothing to make it positive -
every credit comes out as a debit. Meanwhile the stray CR lands in whatever band
covers it, which is the description.

This is the cardinal failure: no error, no flag, a full set of figures with the
wrong SIGN on every credit. Balance reconciliation is the only thing that would
notice, and only if the statement prints balances to reconcile against.

| N38 | high | **fixed** | On a BUNDLE the screen said "3 distinct statement periods found, read as one span 20 Apr to 19 Jun" - and no span was made. split.R had cut the file into 3 statements and reconciled each separately. The tool was misreporting how its own figures were produced. Introduced by the multi-period work in this same release. | R/extract_metadata.R detect_multiple_statements |

### N38 - a span that never happened

`period_start` / `period_end` always hold something. On a bundle they are simply
the FIRST period's dates, because `.period_span` is deliberately skipped when
more than one "Page 1 of N" is printed - `R/split.R` owns those files and cuts
them into separate statements, each reconciled on its own anchors.

Naming those two dates as "read as one span" therefore told a reader that three
statements had been read end to end, when the figures in front of her came from
three INDEPENDENT reconciliations. Nothing was wrong with the figures; the
sentence describing them was false.

Found by tracing whether tonight's `n_periods` change had broken the split (it
had not - the KPIs are still tagged `[statement 1..3]`). `period_merged` now
records whether a merge was actually made, and the span is only named when it was.

### The two mechanisms for "one file, several periods"

The simplicity review called these redundant - "two answers to the same shape of
problem, mutually excluded by a marker count" - and recommended deleting one.
**That is wrong, and the measurement above is why.** They take structurally
different inputs:

* **`split.R`** - several separately-issued statement DOCUMENTS concatenated.
  Each prints its own "Page 1 of N" and its own opening and closing, so each can
  be reconciled on its OWN anchors. Merging them would throw those anchors away
  and replace three independent proofs with one weaker one.
* **`.period_span`** - ONE document with several period sections. There are no
  document boundaries to cut on, so splitting is not available at all; the only
  way to make the checks work is to read it as one long span.

Delete either and the other cannot cover its cases. What WAS worth revisiting -
and has now been done, see `N39` - is the `split:` opt-in, which only 1 of 13
shipped templates declared even though the commit gate inside `split_bundle()` is
what actually makes splitting safe. That gap is closed; the two mechanisms stay.

| N39 | high | **fixed** | Auto-split was OPT-IN and only 1 of 13 shipped templates opted in, so a bundle read by any of the other twelve - or by any template an analyst builds - got no split at all and fell through to the merged parse. Measured on a two-statement Westpac bundle: needs_review, trust LOW, balance_reconciliation AND running_balance_continuity both failing - the exact symptom auto-split exists to remove. The opt-in was never what made splitting safe; the commit gate in split_bundle() is. | R/split.R `.split_spec` |

| N40 | medium | **fixed** | `templates_seed/` ships ten starting points whose bands are placeholders marked `# TODO draw` - and YAML drops comments, so once one is copied into `templates_user/` NOTHING the loader can see says it is unfinished. Six of the ten validate as-is, so a half-drawn template would join detection with placeholder coordinates. It fails loudly rather than silently, but the person is then debugging a template they never finished drawing. A `draft: true` key (the same shape as the existing `sample: true`) now makes that state visible: the loader refuses it and says what to do. | R/templates.R load_templates, templates_seed/*.yaml |

| N41 | medium | **fixed** | The batch table's Result column sorted BEST-first on the first click. `orderData` points it at a hidden severity key and DataTables' default first click is ascending, so the click meant to gather the failures put the clean files on top. The initial order was right, and the test only asserted the initial order, so the inversion was invisible. | app.R cv_batch `orderSequence` |
| N42 | medium | **fixed** | The toolkit's "this template's saved validity window is not a date" banner returned EARLY, so it kept claiming the boxes were empty after the user had picked dates - and hid the backwards-window warning for exactly the templates the branch was added for, meaning the one mistake a pair of date pickers still allows went unnamed until Save. | app.R g_eff_msg |
| N43 | low | **fixed** | A template whose validity window is the four letters `NA` (how yaml round-trips an absent value) opened under a red banner telling the user to fix something correct. `NA` and blank both mean ALWAYS. | app.R `.eff_date` |
| N44 | low | **fixed** | `.eff_stored_ok` detected an unshowable window by CATCHING `.eff_date`'s exception, so making that helper honour its documented "a string or NULL, full stop" contract silently removed the detection. A guard resting on another function throwing is one tidy-up away from disappearing; it now asks the question directly (`.eff_shows`). | app.R |
| N45 | low | **fixed** | `row_coverage`'s all-clear headline could read "Every candidate row was kept" while a mapped band had read no words anywhere - a dead band produces no rows to skip, so nothing was skipped. The withdrawn verdict was wrong to order a redraw (no threshold separates a misplaced band from a column with no entries), but silence is the opposite error: the fact is now stated and the judgement left to whoever can see the statement. | R/row_coverage.R |

| N46 | low | **fixed** | The design system is half-real. `www/app.css` declares the tokens, but app.R still carries **37 distinct hex colours across 87 uses and 125 inline `style=` attributes**, several of them near-misses for the tokens they should be using: `#b00020` x19 vs `--bad:#b3261e`, `#137333` x12 vs `--ok:#0f7a37`, `#c77700`/`#a15c00` x13 vs `--warn:#b7791f`. Nothing is wrong on screen; the colours are close enough that nobody notices, which is exactly why it will not self-correct. The test that looks like it guards this (`expect_false(grepl("tags$style(", joined))`) forbids style TAGS while 125 style ATTRIBUTES sit untouched - it passes and proves nothing. | app.R |
| N47 | low | **wont-fix** | Comment density where the recent work landed: `R/batch.R` is 93 comment lines to 67 code lines (1.39) and `R/row_coverage.R` 71 to 105 (0.68, down from 0.83 after the measurements moved into this register). The charter asks for comments "at the density of the code around them"; `R/schema.R`, a stable comprehensible file, is 0.08. Where an explanation is longer than the function it explains, the decision is usually the thing to simplify. Not all of it is fat - the surviving comments record WHY something surprising is the way it is, which is the valuable kind. | R/batch.R, R/row_coverage.R |

### The truth-and-docs sweep (N48-N79)

Ten agents read the whole repo against one question - *does the code do what it says, and
does anything on a customer-facing screen tell a lie?* - and two more drove the app and
followed the documentation as a maintainer on call and a forensic accountant. Everything
below was reproduced before it was fixed and re-proved after.

| ID | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N48 | **critical** | **fixed** | On an ambiguous match the engine converted with the template that scored HIGHEST - which need not be either of the two the screen named, and need not have matched at all. A statement whose tie was between two Kowhai templates was read by the ANZ template, reconciled to the cent, and stamped `bank: ANZ` on the workbook and the Qlik feed. The screen said "2 templates fit equally well" and named neither of the two it used. A wrong figure, in the field a court filing quotes first. | R/convert.R:258-270; `det$template_id` used instead of `det$tied[1]` |
| N49 | high | **fixed** | The balance check told the reviewer "no opening or closing balance was found, and no running-balance column to derive one from" on a statement whose running-balance column sat on the same screen, passing its own check. The refusal was right; the reason was false. | R/reconcile.R:114-125 |
| N50 | high | **fixed** | A file the tool could not read at all - random bytes named `.pdf`, an empty file, prose - was reported as "we don't have a template for this layout yet", which invites the analyst into the 2-minute template flow with nothing to draw a band on. | R/convert.R:70-113 `.unreadable_reason` |
| N51 | high | **fixed** | The auto-drafter read **0 rows** on `asb.pdf` and `anz_single.pdf` - two statements this repo ships proven hand-written templates for. Six general faults, none file-specific: the month was never inside the date band (`28 Apr` read as `28`, so `%d %b` parsed nothing and every row dropped - which is also why redrawing by hand never helped); money columns were measured over the whole page, so a balance-summary block invented columns; duplicate column roles collapsed silently; the headings printed on the statement were never read; the money pattern was pinned to `$`, so a GBP statement showed no money at all; ordinal dates were unknown. The drafted template now produces **byte-identical output to the proven template** on six statements, and reads rows on all 48 PDFs in the corpus. | R/wizard_auto.R `suggest_pdf_columns`, `.WA_MONEY`, `.WA_DATE` |
| N52 | high | **fixed** | The drafter guessed the bank from the first keyword found anywhere in the document, so an ASB statement mentioning ANZ once in a payee line drafted as ANZ - and the toolkit pre-filled that into the save form. | R/wizard_auto.R `.sniff_bank` |
| N53 | high | **fixed** | The confidence level was computed on every run and shown on **no clean run**, while four documents keyed their advice on it. It is on the card now, and says when medium is the format's normal ceiling rather than something to chase. | app.R cv_card, `.medium_is_the_ceiling` |
| N54 | high | **fixed** | Two of the three Analysis charts failed on every statement with `invalid RGB specification`: `#fff` passed to a function that requires 6 hex digits. The page had shipped with one working chart out of three. | app.R chart palette |
| N55 | high | **fixed** | Admin - Templates: a reactive named `user_template_ids` shadowed the engine function of the same name, so the origin line printed an R error object to the screen and both Hide and Delete were dead. | app.R |
| N56 | high | **fixed** | "More than one template fits - pick which one" was shown with no picker anywhere on the page. | app.R |
| N57 | high | **fixed** | The bundled sample statement - the first thing a new user is invited to click - could never convert: its template carried `sample: true`, which the loader drops. | templates/, R/templates.R |
| N58 | medium | **fixed** | `DIAG_PLAIN["amount_parse"]` served two opposite meanings, so a *direction* warning ("money in/out may be the wrong way round") rendered as "amounts couldn't be read". Now its own category, with the seam test asserting both ways. | R/diagnose.R:70,175; ui_labels.R |
| N59 | medium | **fixed** | `convert_document`'s `tryCatch` started 39 lines into the body, so the front door threw on inputs Shiny cannot produce but a caller can. Twelve hostile inputs now all return `status = "failed"` with a message; none throws. | R/convert.R:122-131 |
| N60 | medium | **fixed** | `validate_template` accepted a validity window that starts after it ends. | R/templates.R:201-216 |
| N61 | medium | **fixed** | Both irreversible Admin actions - purge uploads, delete template - fired on one click with no confirmation. They now state the count and what survives, and name the exact file. | app.R |
| N62 | medium | **fixed** | Three Admin downloads returned HTTP 500 when there was nothing to download (`req(FALSE)` inside the handler), and one wrote a file literally named `.audit.md`. Every download now returns a file or a readable one-line reason. | app.R |
| N63 | medium | **fixed** | "Try it on a sample statement" bypassed the QID gate that every other route enforces, so a run could be written with no record of who ran it. | app.R `.identity_ok` |
| N64 | medium | **fixed** | The feedback question was pre-answered "Correct", so the default reading of every conversion was an endorsement nobody gave. | app.R |
| N65 | medium | **fixed** | `output$cv_rematch` was never rendered, so on a clean result there was no on-screen route to say "wrong bank" - and a code comment claimed the opposite. | app.R |
| N66 | medium | **fixed** | X-ray: unticking the last remaining layer turned all six back on. | app.R |
| N67 | medium | **fixed** | Raw engine codes, template ids and internal metrics on customer-facing surfaces: `balance_reconciliation` in a sentence, "closest anz_everyday_pdf score 2/3", schema field names in Field coverage, engine column names in the X-ray legend. | R/detect.R:187-196, R/reconcile.R, app.R |
| N68 | medium | **fixed** | One screen, four names - "X-ray", "the inspect view", "Look inside", "See it on the page". Now one name everywhere a user can read it. | app.R, ui_content.R, R/reconcile.R, R/diagnose.R, docs/ |
| N69 | medium | **fixed** | The proof strip drew tick / cross / dash glyphs with no key anywhere on the page. | app.R cv_proof |
| N70 | medium | **fixed** | The QID is the first thing the app asks and appeared in none of the analyst's pages. | docs/for-analysts/ |
| N71 | medium | **fixed** | `when-something-goes-wrong.md` navigated by a fix-owner column that is maintainer-only and not on the analyst's screen at all. | docs/for-analysts/ |
| N72 | medium | **fixed** | `adding-a-bank-template.md` described seven steps against the app's five, and the two it added were the two hardest - both of which the app does for her. | docs/for-analysts/ |
| N73 | medium | **fixed** | No document said how to run the tool, how to ship a change, or how to roll one back; and there was no procedure for the one call that will actually come - "a user says this conversion is wrong". | docs/operational/ (new) |
| N74 | low | **fixed** | Admin said things that were not true: the gaps list included a file that converts cleanly; placeholder rows read as data; an empty suggestion picker carried instructions for using it. | app.R |
| N75 | low | **fixed** | Silent successes across Admin and the toolkit - actioned requests, lexicon reloads, template deletes and the toolkit's Assign/Remove all completed with no message, the last of them writing its confirmation to an invisible tab. | app.R |
| N76 | low | **fixed** | Invariant 17 (no non-ASCII in an R name) had no enforcement. A test now walks `getParseData()` over app.R and all 44 `R/*.R`. | tests/testthat/test-app-adoption.R |
| N77 | low | **fixed** | Cosmetics with the same root - text assembled without reading it back: "Read as: X statement statement"; the page box accepting page 99 of a 3-page PDF; the X-ray legend listing layers it had not drawn; notifications stacking rather than replacing; the Admin tab node present in the DOM without `?admin`. | app.R |
| N78 | low | **fixed** | Form templates are outranked by statement templates while the save message said detection is automatic. | app.R wording |
| N79 | low | **fixed** | The fingerprint dropdown offered single generic words that the same screen then refused to save. | app.R |

### The drive audit (N80-N104)

Two people worked the tool over rather than reading it: one drove every screen in a browser,
one followed the written documentation as a maintainer on call and then as a forensic
accountant. Every item was reproduced on a real statement before it was fixed.

**A caveat on this block.** N48-N79 were re-driven cold by an agent that had not made the
changes. N80-N104 were not - the independent re-drive was killed part-way by an
infrastructure restart. They are fixed, each with a regression test that fails when the fix
is reverted, and the suite is green at 4,006 - but the second pair of eyes did not land.
Treat them as verified by test, not by walkthrough.

| ID | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N80 | **critical** | **fixed** | The proof strip - the first quality signal on the page, above the fold - was pinned to five checks. `dates_within_period` and `transaction_count` can FAIL and were permanently off it, and nothing ever added a failing check back. A statement printing "9 transactions" from which 7 were read drew four chips, none red, under a key promising a symbol it would never draw. | app.R:3086-3093 `.PROOF_CHECKS` |
| N81 | **critical** | **fixed** | The toolkit put a green tick on a draft that read a tenth of the page: the verdict branched on `n > 0`, never on n against what was on the statement. Measured on `asb.pdf` (1 row kept, page 2 yielding nothing) and on a draft that silently dropped every date-less continuation line - which then reconciled with 11 balance discontinuities on the very next screen. `row_coverage()` already held the answer at that moment. | app.R:5115 |
| N82 | **critical** | **fixed** | A row printing its own 4-digit year was flagged `date_year_inferred`, on every row, whenever the statement's PERIOD text could not be parsed. The flag caps trust, drops the run to needs-review and withholds it from the dashboards - so the loudest warning the tool owns was spent on dates that were verbatim. 19 of 19 rows on a real statement. | R/parse_pdf_table.R:1064 |
| N83 | **critical** | **fixed** | One comma in a sentence made prose "a statement layout we don't know yet". The guard fired only when NO line held ANY separator character, so a note to self, an email body and meeting minutes were all invited into the template flow - the exact outcome the guard's own comment said it existed to prevent. The test covering it used the one prose string with no separator in it. | R/convert.R:105 |
| N84 | high | **fixed** | The settings block a maintainer copy-pastes printed `admin_password: change-me`; the code checks for `changeme`. Copy it verbatim and the guard concluded a password HAD been set - Admin opened, with its password printed in the documentation. The same page contradicted itself 40 lines later. | docs/operational/running-and-keeping-it-up.md:96 |
| N85 | high | **fixed** | "Period:" on the card was the min/max of the TRANSACTION dates, not the statement's printed period. By construction every transaction falls inside it, so "34 date(s) outside period" could only ever look like a bug - training an analyst to dismiss the one check that catches a mis-parsed year or a swapped day/month. The statement's own period appeared nowhere on screen. | app.R:3309-3313 |
| N86 | medium | **fixed** | Raw template ids on the customer-facing verdict card: the tie message named five, and the matched-but-empty message reached the screen as "badbands_pdf matches the wording...". | R/convert.R:334, :349-353 |
| N87 | medium | **fixed** | The transactions tables mapped column HEADERS to English and passed the VALUES verbatim, so the Flags column printed `ocr_low_conf` and `date_year_inferred` - on the screen the analyst's own page tells her to report as a bug. 11 emitable tokens, 3 of them with English anywhere. | app.R:3597, :5094; ui_labels.R |
| N88 | medium | **fixed** | The form result card: a FIELD column of engine schema names redundant with the LABEL beside it, MATCHED/REQUIRED/FLAGGED as `true`/`false`, and a chip reading "Read as: anz_kiwisaver_fields". The earlier fix for "Read as: NA NA statement" had swapped one wrong answer for a charter breach. | app.R:3473-3478, :3262 |
| N89 | medium | **fixed** | Three alarm levels for one run: the headline called the failing checks "secondary and commonly flag"; What-to-check listed only the dates item; the top diagnostic, severity HIGH, said the file was several statements bundled together; and the button offered the template toolkit. What-to-check omitted the highest-severity item on the page, and the CTA contradicted the tool's own diagnosis. | app.R |
| N90 | medium | **fixed** | The template toolkit was offered on a file that could not be read at all - nothing to draw a band on, on the same card that said so. | app.R |
| N91 | medium | **fixed** | A "Checks" heading over no rows and a "Field coverage" heading over no rows, on the screens with the least to go on - and a blank table cannot be told from a rendering failure. Plus "Was this conversion correct?" offered on a run that produced nothing. | app.R |
| N92 | medium | **fixed** | The batch table graded a file more confidently than the single-file card did - "Converted successfully" and "what to check: -" for a run the card called medium confidence with two unproven checks - and carried no confidence column at all, so a 30-file batch had to be opened row by row. | app.R |
| N93 | medium | **fixed** | A pound-sterling statement was drafted as NZD, with the pound printed in the cell beside a currency column reading NZD. All three drafters stamped the literal "NZD"; currency is now read off the money the drafter actually matched. | R/draft.R, R/wizard_auto.R |
| N94 | medium | **fixed** | "could not be checked" printed beside two matching numbers (Expected 79, Read 79) - identical in shape to the passing case, so the numbers carried no information about which, and a check that did not run read as a pass. | R/reconcile.R |
| N95 | medium | **fixed** | `run.R` printed no run id, so step 4 of the incident procedure - open `logs/runs/<run_id>.json` for the re-run - could not be followed against a folder of 291 files. | run.R |
| N96 | medium | **fixed** | "The output is byte-identical" is false after any engine update: `engine_version` is stamped inside the JSON. The procedure offered exactly one cause for a mismatch and pointed the maintainer at a template that was innocent. | docs/operational/investigating-a-wrong-conversion.md |
| N97 | low | **fixed** | The operational pages gave only `RUN-ME.bat`, with the working non-Windows command buried in a design document; and both said `config.yaml` is "created on first run" when the batch file copies it - which is why a clean start announced "Admin is CLOSED" with no file to edit. | docs/operational/ |
| N98 | low | **fixed** | The analyst's page said "these are the exact words on the screen" and then quoted a headline the app no longer builds; handed her an R symbol 14 lines after telling her a raw internal name on screen is a bug; and carried a stale sentence accusing correct behaviour of being wrong. | docs/operational/converting-statements.md |
| N99 | low | **fixed** | "A clean run shows a single row saying so" - it does not; real statements essentially never reach that branch. A clean CSV shows one medium-severity row and a clean 311-row PDF shows two info rows. | docs/, R/diagnose.R:433 |
| N100 | low | **fixed** | Tick and cross meant "checked and passed" / "a problem" in the proof-strip key and the analyst's OWN verdict in the feedback control twenty lines below. | app.R |
| N101 | low | **fixed** | Two comments in app.R stated a measured fact that a fix in the same release had made false ("draft_template reads 0 rows on asb.pdf and anz_single.pdf"). | app.R:434-435, :4514 |
| N102 | low | **fixed** | Two console warnings per toolkit open, and a comment asserting the one warning suppressed there was the only one. | app.R:3925-3929 |
| N103 | low | **fixed** | The add-a-check recipe was accurate but did not warn that minimal fixtures trip any new check - so the three tests that fail name date-year inference, not the check just added. | docs/design.md |
| N104 | low | **fixed** | Two tests asserted that a customer-facing message CONTAINS a raw template id - the one thing the charter forbids on that surface - so the suite defended the breach. They now assert the friendly name is present and the id absent. | tests/testthat/test-detect.R:410, :432 |

### The accountant's second pass (N105-N116)

Found by driving real statements with the analyst documentation open, after the drive audit
was fixed. The first two are the worst kind of defect this tool can have short of a wrong
figure: **it cries wolf on a clean statement.** Every one of these was reproduced on a
statement in this repo.

_All twelve are **fixed** and were re-verified by two agents that did not make the changes:
the perfect conversion is trusted and reaches the dashboards (actionable rows 12 -> 9, the
three deltas being exactly the statement's own Totals lines); the Admin vocabulary now takes
effect for `period`, `record` and `method`; and `docs/for-analysts/` exists as a signpost that
lands the analyst on her four pages.
**N111 is the exception** - both verifiers reported on the
rest of this block and neither reached it, so it is left open rather than assumed._

| ID | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N105 | **critical** | **fixed** | The tool quarantined a **perfect** conversion. On `anz_0382025_feb.pdf` the card read, in amber, confidence LOW: *"12 row(s) look like transactions but could not be read, against 7 that were - so more of it was missed than captured. The template is almost certainly reading the wrong part of the page"*, and the run was held back from the dashboards. The statement prints `Totals at end of period $2,625.15 $1,126.11 $19,231.00 OD`; the app's tiles read MONEY IN $1,126.11, MONEY OUT -$2,625.15 - **exact agreement, to the cent, both directions.** The 12 "missed transactions" are a page heading, four UPCOMING automatic payments, an other-accounts balance, three of the statement's own Totals lines, the ANZ footer and an interest-rate table. `.pdf_is_summary()` recognises "Opening balance" but not "Totals at end of page". The calibration this rests on (R/reconcile.R:340-345: *"healthy conversions sit at 4-36% actionable skips, broken ones at 100-1300%"*) is falsified by a shipped sample sitting at 171% while being perfect. | R/parse_pdf_table.R `.pdf_is_summary`; R/reconcile.R:340-345 |
| N106 | **critical** | **fixed** | The documented Admin escape hatch silently cannot work. `.PDF_TRAIL_MONEY` strips a trailing overdraft marker with a pattern whose `od\b` matches **inside the word "period"**, so the label becomes `totals at end of peri`. R/parse_pdf_table.R:289 promises *"An analyst can drop the wording in the Admin vocabulary; no code change."* Tested with the phrases exactly as printed: "totals at end of page" works, "totals at end of period" does not. She types what is on the page, saves, re-converts, and nothing happens, with no feedback anywhere saying why. Every label ending `od` / `dr` / `cr` is affected - period, record, method. | R/parse_pdf_table.R:249, :289 |
| N107 | high | **fixed** | `docs/for-analysts/` - the directory the analyst is handed - **does not exist**. Her three pages sit in `docs/operational/`, a directory named for operators. | docs/ |
| N108 | medium | **fixed** | On a needs_review run the engine headline calls the failing checks *"secondary and commonly flag on combined/multi-account statements"* directly above a HIGH diagnostic saying the file is several statements bundled together and must be split. The What-to-check list and the button were fixed in the drive audit; this sentence was not. | R/reconcile.R `.reconcile_trust` |
| N109 | medium | **fixed** | The form result says *"3 label(s) appear more than once with different values; the first of each was taken - check them against the document"* and the NEEDS A LOOK column beside it is empty on every row. The tool knows which three and will not say. | app.R |
| N110 | medium | **fixed** | Every failing check is listed twice on one screen, and one of the two names it in words that state the opposite of what happened. | app.R / R/reconcile.R |
| N111 | medium | **fixed** | A figure in a column headed EXPECTED that the statement never printed anywhere - so the reviewer cannot check the checker. | R/reconcile.R |
| N112 | low | **fixed** | "Everything past the verdict is behind one link" - it is two on a clean run, and the dashboard line is two clicks deep. | app.R |
| N113 | low | **fixed** | A heading that promises something that is not under it. | app.R |
| N114 | low | **fixed** | `app.R:3452 cur_symbol()` holds raw non-ASCII glyph literals. The charter's ASCII rule covers string literals, and this round's drafter change means a GBP statement now really reaches that code - so on the C-locale box the suite runs on, these are a live risk rather than a style nit. | app.R:3452 |
| N115 | low | **fixed** | `design.md` section 8 now warns that adding a check breaks existing tests, but a maintainer following it got **27 failed + 1 error**, not the 3 the page implies - and the reporter caps output at 10, so most are invisible. | docs/design.md |
| N116 | low | **fixed** | `scripts/run_app.R` prints the literal placeholder `http://<this-vm>:8100` rather than a resolvable address, so a document saying it "prints the address" is generous. | scripts/run_app.R |

### Making it work for a squad, and what that broke (N117-N134)

The concurrency round: every conversion now runs in its own process, so five to ten analysts
can convert at once. Two verifiers then drove it and followed the documentation. N117-N119
are the price of the change; the rest were already there and only surfaced under real use.

_All fixed. N118-N132 were closed by the following round and re-verified by two agents that
did not make the changes: the bundle period, the split-file proof strip and the locale sweep
were each driven on real statements, and both verifiers independently measured the same suite._

| ID | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N117 | high | **fixed** | The concurrency work disarmed the only guard on the batch seam and turned the suite red. `test-batch.R` grepped `app.R` for one exact call spelling; the call moved into the child process, the grep found nothing, and the guard did not fail loudly - it *errored* on `src[integer(0)]`, which reads as a broken test rather than a broken seam. The guard now asks the invariant of every caller wherever it lives, so moving the call cannot silently disarm it again. | tests/testthat/test-batch.R:240 |
| N118 | high | **fixed** | **A backwards date published to the governed feed.** On an auto-split bundle the card reads "Statement period: 20 Apr 2026 to 19 Feb 2026" - it ends two months before it starts. The header takes statement 1's start and statement 3's end in FILE order, not date order; the split table on the same screen shows the true span is 20 Dec 2025 to 19 Jun 2026. It reaches Qlik: `feed/runs/83634521b5....csv` carries `"20 Apr 2026","19 Feb 2026","accepted"`. Nothing on screen or in Checks notices. | R/split.R / R/extract_metadata.R |
| N119 | high | **fixed** | The proof strip does not render **at all** on a split file. `.proof_pick` matches KPI names exactly, but a split run's names carry a ` [statement 1]` suffix, so nothing matches and the whole strip - and its key - is dropped. Verified twice on screen: 824 rows, "Sent to the dashboards", `#cv_proof` innerText length **0**. All three split files in the corpus do it. `plain_check` and `.medium_is_the_ceiling` are split-aware; this is not. | app.R:3438, :3447 |
| N120 | high | **fixed** | Two run records for one statement carry the **same** `engine_version` and the **same** `template_sha256` and **opposite verdicts** (`needs_review`/low, then `ok`/medium) - because this round changed the behaviour without moving VERSION. The incident procedure's headline test says "any difference is a finding", so it now cries wolf on exactly the statements this round fixed, and every record already written under 1.3.0 is indistinguishable from a new one. | VERSION, logs/runs/ |
| N121 | high | **fixed** | A non-ASCII literal that **eats bytes**. `R/labels.R:109` holds an en-dash inside a character class. Under `LC_ALL=C` - the locale the suite runs in and the stated deployment locale - `"Account name: EUR1,234.00"` comes back with the euro sign's leading byte eaten and `validUTF8()` FALSE: the value is silently no longer verbatim. With a UTF-8-marked input it throws instead. `app.R` forces a UTF-8 locale; `run.R` and `tests/run_tests.R` do not - so the CLI the incident procedure tells a maintainer to reproduce with runs without the guard the app has. Five other `R/` files still carry non-ASCII literals. | R/labels.R:109 |
| N122 | medium | **fixed** | N83 is only half fixed: `R/convert.R:139` strips blank lines **before** the shape test, so a note to self with blank lines between its sentences is still "No template for this statement yet". The guard's own comment says an email's greeting and sign-off are "scattered through prose" - and the caller removes what scattered them. Multi-line minutes are correctly refused, so partial, not absent. | R/convert.R:139 |
| N123 | medium | **fixed** | The save confirmation prints a raw template id **twice** - `Saved "sample_bank_statement_pdf" ... it matched yours ("sample_bank_statement_pdf") on its own* - while the card two inches to the left correctly says "Read as: SAMPLE BANK statement". | R/util.R:256, :259; app.R:5873 |
| N124 | medium | **fixed** | N91 finished two of three headings: on a failed run "Diagnostics - where / why / how to fix" still sits over blank space, while Checks and Field coverage both explain themselves. | app.R:4055 |
| N125 | medium | **fixed** | The **first navigation instruction a new analyst is given** is wrong. Last round moved Checks, Diagnostics, Field coverage and the dashboard line out from behind the "Show me how it read this" toggle; three sentences still send her there, including the one on the page her own README tells her to start with. `when-something-goes-wrong.md` was edited that round and line 9 was left. | docs/operational/converting-statements.md:24, :38; when-something-goes-wrong.md:9 |
| N126 | medium | **fixed** | Admin's Uploads table lists **every** upload, newest first - which is what the incident procedure promises - under the heading "Uploads - new formats to pick up" and the help text "Statements the tool couldn't read, that nobody has set up yet". A maintainer at step 1, looking for a successful conversion, is told by the screen not to look there. | app.R:638-639 vs R/uploads.R:62-87 |
| N127 | low | **fixed** | Field coverage says "not on this statement" about columns the statement **prints** (`TYPE`, `NAME OF OTHER PARTY`, `TRANSACTION PARTICULARS` on westpac.pdf) - the template folds them into `description`. The note cell beside it says the true thing, so the row disagrees with itself. Pre-existing. | app.R, COVERAGE_PLAIN |
| N128 | low | **fixed** | `design.md` section 8 step 4 says the `.KPI_DIAGNOSIS` entry is "in the same file" as `DIAG_PLAIN` (`ui_labels.R`); it is in `R/diagnose.R:151`. This is the step the page itself flags as the one that catches people out. | docs/design.md |
| N129 | low | **fixed** | "Six kinds ... see the five kinds above" - six is correct and the page lists six; the back-reference says five. | docs/operational/when-something-goes-wrong.md |
| N130 | low | **fixed** | `design.md` still states "A non-ASCII character inside a string literal is fine", and its invariant table says a non-ASCII string VALUE "still passes". Both are false for the UI files already, and the rule is widening this round. | docs/design.md:28-29, :353 |
| N131 | low | **fixed** | The maintainer's crash log carries the child's **locale string** where the reason should be: `convert job job00023 (stopped): [1] "C.UTF-8"`. A top-level `Sys.setlocale()` in the generated child is auto-printed as line 1 of every log, and for a job killed before any other output that is the whole tail. | R/jobs.R:280 |
| N132 | low | **fixed** | `maintaining-the-engine.md` records a build baseline this tree cannot produce (71 files / 855 tests / 4,213 assertions against an actual 71 / 864 / 4,283), on the page that tells a maintainer a non-zero failure count means stop. | docs/operational/maintaining-the-engine.md |
| N133 | - | **not-a-defect** | Recorded so the next reviewer does not have to rediscover it: the completeness guard was deliberately **loosened** this round (`no_unparsed_rows` now needs `actionable >= 3 && actionable >= n && actionable*4 > visual_rows`). A worked case of 10 kept / 11 actionable / 60 visual now returns "could not be checked" where it previously returned "failed". The comment at R/reconcile.R:352-410 gives the honest reason - no partial read exists in this repo to calibrate against - and it fails toward "unproven" rather than "passed", which is the right direction. | R/reconcile.R:352-410 |
| N134 | high | **fixed** | Found while proving the concurrency work, and the reason it is recorded rather than folded silently into the feature: the conversion child inherited the **service's** locale rather than the app's, so a description with an accented character came back **mangled in the delivered CSV** (`Caf<U+00E9> Kr<U+00F6>ne` against `Cafe Krone` correctly rendered). Byte-comparison against an in-process run showed the first differing byte at position 186. It would only ever have appeared on a real customer's statement. Also fixed there: reaping a job killed the R child but left `tesseract` orphaned - three at a core each, 14 minutes after the tabs closed. | R/jobs.R:262, :206 |

### The go-live sweep (N137-N144)

The last sweep before production asked a different question again: not *is this correct*, but
*what only breaks on the day, on a machine nobody has tested, in front of people who cannot
fix it.* It found the worst defect in this register.

| ID | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N137 | **critical** | **fixed** | **The offline bundle carried real bank statements out of the building.** The bundle is the deliverable: a folder copied to a USB stick and walked to an air-gapped police server. Its payload list included `samples`, and `file.copy(recursive = TRUE)` takes a directory WHOLE - including `samples/_private_staging/`, which holds **80 real statements, 21 MB**, git-ignored and mode 0600, under a README that promises they are *"never shared or distributed, never included in any package"*. Proved end to end: a naive copy carries all 80; after the prune, 0 survive and only the README remains. The prune is not trusted either - the builder enumerates what is left and **stops the build** if anything is standing. | scripts/bundle-offline.R |
| N138 | high | **fixed** | The same builder discarded the return value of `unlink(dist/config/config.yaml)`. A locked or read-only file meant the bundle shipped **the build PC's admin password**, and copying that bundle over a live install would have replaced the server's real settings including the feed folder. Now verified and fatal. | scripts/bundle-offline.R |
| N139 | **critical** | **fixed** | **A failed feed write said "Sent to the dashboards."** The three post-verdict failures (write failed, no rows, a stale row left on the dashboard) overrode `ok` and `why` but never `line` - and `line` is what the screen prints in bold, first. So a run the dashboards never received was headlined as delivered, with the truth in grey underneath. Driven live with the feed folder made genuinely immutable. It is the charter's cardinal failure inverted - loudly wrong - on the one line that says where the figures went, on the screen the go-live checklist tells the operator to trust. The test that covered it asserted `ok` and `why` and never once looked at `line`. | ui_labels.R `plain_feed` |
| N140 | high | **fixed** | `VERSION` never shipped in the bundle, so `engine_version()` on the server returned **"unknown"** - stamped into every run record and every workbook. The whole incident procedure rests on comparing `engine_version` between two runs, so it would have been unanswerable for ever, for every run made before anyone noticed. VERSION is now on the *essential* list, which fails the build rather than shipping without it. | scripts/bundle-offline.R |
| N141 | high | **fixed** | The PC installer wrote garbage over the operator's PATH and Poppler never reached it. When the registry read returned no `Path` line - a fresh service account, or any failed query - the code produced `cur = NA`, and `nzchar(NA)` is TRUE, so it set PATH to a literal `NA;...`. The Tesseract block then repeated it and **overwrote the Poppler entry**, so a server that shipped Poppler could not read a single scanned page. `R/ocr.R` refuses a scan unless both binaries resolve, so this decided whether scanned statements worked at all. | scripts/install-offline.R |
| N142 | high | **fixed** | A bundle could be built **without its own installer**, silently: the copy was guarded by `if (file.exists(...))` and nothing checked afterwards. RUN-ME.bat then runs `install-on-pc.R` unconditionally on the air-gapped server. Worse, the old failure blamed *"some R packages could not be installed - most often the bundle was built under a different R x.y"*, sending the operator to rebuild for a reason that was never true. Now fatal at build time. The manifest states **all seven** prerequisites where it used to state two. | scripts/bundle-offline.R, RUN-ME.bat |
| N143 | medium | **fixed** | `RUN-ME.bat` threw the app's exit code away - the normal path ended on `goto :eof` after `pause`, so **every** run reported success. A Task Scheduler entry set to restart on failure would never restart, and an app dying at startup stayed dead silently. | RUN-ME.bat |
| N144 | medium | **fixed** | **The Qlik dashboard was already splitting its totals in two, in production data.** Template extras were appended to each transactions row, so a card statement carrying `fx_amount` produced a 32-column file among 30-column ones - measured on the real feed folder as **17 files at 30 columns and 2 at 32**. Qlik's wildcard `LOAD *` concatenates only on an exact field-set match, so those two statements' figures were landing in a second table and the unit's totals were short by them, with nothing on any screen saying so. The extras now travel in their own folder keyed by run_id + row_id; the transactions field set is fixed for ever. The doc that promised extras were safe has been corrected - it contradicted itself two pages later, where it named this exact symptom as a fault to look for. | R/feed.R, docs/operational/connecting-qlik.md |

**A near-miss worth recording, because it argues for the suite.** Fixing N144 I moved a
block of code between an `if` and its `else`, so the `else` silently re-attached to my new
`if`: every ordinary feed write - the ones with no extras at all - was recorded as
`write_failed` while its file sat there correctly written. A successful write reported as a
failure, which is the same class of lie as N139 in the other direction. It never reached a
commit: the suite went from 0 failures to 32 in one run and named the seam. Written down
because the lesson is not "be careful with braces", it is that a dangling `else` is a
structure that can absorb a change silently, and the only reason this was caught in seconds
rather than in production was 900-odd tests that actually exercise the write path.

### The empty-band verdict - the measurements that withdrew it

Kept here rather than in `R/row_coverage.R`, where they had grown to thirty lines
of comment justifying a three-line change. All on real statements in
`samples/_private_staging`, each with its shipped, proven template:

* **False positive on ONE word.** `anz_single.pdf` / `anz_everyday_pdf`: all five
  bands correct, 311 of 311 rows kept, exactly **1** word of 4,251 unclaimed - a
  page-corner mark at x=29, y=1. Map one further column over a strip this statement
  happens not to print in, and that single word made the report order a maintainer
  to redraw a band that was correct. It also SUPPRESSED the true message ("14 rows
  were skipped for an unreadable date or a missing amount"), being first in the chain.
* **False negative on ONE word.** `anz_0382025_feb.pdf` with the credit band
  genuinely moved off its column: 54 words unclaimed, rows lost - and no verdict at
  all, because one stray word landed inside the misplaced band.
* **No threshold separates them, in either unit.** A REAL fault (credit band
  misplaced on `anz_single.pdf`) leaves **5.13%** of the region's words unclaimed;
  the CORRECT shipped `westpac_everyday_pdf` on `westpac_B.pdf` leaves **9.44%**.
  By count: that real fault on the 2-page ANZ leaves 54, the correct Westpac 124.
  The fault is under the healthy baseline on both scales.
* **Nor does the shape of the hole.** 79% of correct-Westpac's 124 unclaimed words
  sit in one 20pt strip, against 62% for the genuine ANZ fault.

Structural, and therefore untunable: a word is unclaimed precisely when it is
outside EVERY band, so it can never be evidence about WHICH band is wrong. The
counts remain as counts; the verdict does not come back.

### The app.R split - a concrete plan, deliberately not started

`server <- function` is ~3,800 of app.R's ~4,600 lines: 128 `output$`, 68
`observeEvent`, 33 `reactive()` and 30 `reactiveVal` in ONE lexical scope where
every name is visible to every other. Moving the CSS out made the file 250 lines
shorter and removed **zero** reactive-graph complexity - CSS was the cheapest
250 lines in it to lose. The argument for splitting is unchanged.

**The blocker is the test suite, and it has a two-line fix.**
`tests/testthat/test-app-ui.R` greps app.R's SOURCE TEXT: `.ui_src()` reads the
one file, `.ui_fun()` lifts a function by regex, `.ui_block()` takes a fixed
n-line window hand-tuned per test. 38 `grepl` assertions and 10 `.ui_fun` lifts
across 60 `test_that` blocks are pinned to physical layout. Fix that FIRST, in its
own commit, before anything moves:

```r
.ui_src <- function() {
  fs <- c(file.path(engine_root(), "app.R"),
          list.files(file.path(engine_root(), "server"), "\\.R$", full.names = TRUE))
  skip_if_not(file.exists(fs[1]))
  unlist(lapply(fs, readLines, warn = FALSE))
}
```

**Then split with `source(..., local = TRUE)` INSIDE `server()`** - not Shiny
modules. One environment, zero namespacing, no input-id changes, no changes to
the 60 tests. Modules would mean rewriting every id and every cross-reference,
which is a different and much larger job with its own silent-failure modes.

| file | from app.R | approx lines |
|---|---|---|
| `server/admin.R` | the Admin tab's server half | ~740 |
| `server/toolkit.R` | the template toolkit + form builder | ~1,290 |
| `server/xray.R` | the X-ray / inspect view | ~510 |
| `server/convert.R` | Convert, batch, the result page | ~970 |

Leaves app.R at ~1,100: the `ui`, the shared `reactiveVal`s, and four `source()`
lines. Add `server/` to `scripts/bundle-offline.R` in the SAME commit - `www/` has
just shown how quietly that breaks (nothing fails; the install is simply wrong).

**Why it was not done here:** it is a ~1,300-line move through a reactive scope,
verified by a suite that pins physical layout, at the end of a long session in
which two agents and one reviewer each caught a real error in work that looked
finished. The plan above is the de-risked version; it wants a session of its own.

### N39 - the opt-in was the wrong lock

`split_bundle()` already refuses unless an INDEPENDENT structural signal confirms
the segment count AND every segment parses and reconciles. The `split:` block was a
second lock on a door that gate already holds shut - and it was the one keeping out
the templates that needed it.

Measured before changing anything. Every PDF in the corpus, shipped templates,
opt-in versus defaulted-on:

* **No existing outcome changes.** Nothing gains a split it should not have
  (`anz_multi_B.pdf`, 5 page-1 markers, is still refused by the commit gate);
  nothing loses one.
* That measurement is INERT on its own, because every bundle in the corpus already
  matches the one template that opted in. So the case that matters was built:
  a Westpac statement concatenated with itself, read by `westpac_everyday_pdf`,
  which declares nothing.

| | outcome |
|---|---|
| opt-in (as it was) | `needs_review`, trust **low**, `balance_reconciliation` and `running_balance_continuity` both **fail** |
| defaulted on | 2 statements, reconciled separately, per-statement checks, **status ok**, trust medium, **no failing checks** |

`split: false` remains as an explicit opt-OUT for a template that must never be
cut. `templates/anz_everyday_pdf.yaml` had its block removed: it restated the
defaults exactly, so it said nothing and implied ANZ was special.

### What is holding the last two open

Neither is blocked on effort or on a decision. Each is blocked on **evidence we do
not have**, and the charter's rule — never silently wrong — says a change we cannot
measure is worse than the known, loud, honest failure it replaces.

**Exactly what is needed, and whether an AI survey can supply it.** The short
answer is that N15 can be answered entirely by
[`docs/operational/survey-a-statement-with-ai.md`](../operational/survey-a-statement-with-ai.md),
and N16 cannot be answered by it at all — for a reason worth stating plainly.

**N15 — the form fingerprint gate. YES, the survey answers this.**
What the gate has to decide is whether a phrase is distinctive enough to identify
one form TYPE. That is a property of a CORPUS, not of a document: "Tax
certificate" looks distinctive until you have seen it on eleven unrelated forms.
So the missing evidence is a phrase inventory across many forms, not any one form.

* **What to collect:** the survey run over **8–12 different form types**, ideally
  from **3 or more issuers** (IRD certificates, KiwiSaver annual statements,
  account/interest summaries, insurance or fund letters). More issuers matters
  more than more documents — two forms from one issuer share house style and will
  understate the collision rate.
* **Which answers do the work:** `fingerprint_candidates`, `fingerprint_risk`,
  `form_code`, and section 3F's `form_labels` / `form_repeated_labels`.
* **What it becomes:** every candidate phrase from every form, cross-matched
  against every other form and against the statement surveys already collected.
  A phrase that appears in exactly one document is a fingerprint; a phrase that
  appears in two is not. That measured collision rate is what `.fp_specific`
  should be tuned against — instead of today's "any two-word phrase counts".
* **No client data is involved.** These are the printed words of the form itself,
  identical on every copy ever issued, and Rule 2 of the prompt already says so.

**N16 — scanned digit accuracy. NO, the survey cannot answer this.**
The evidence needed is GROUND TRUTH: what the digits on a scanned page actually
are, to compare against what OCR read. On a real statement those digits are the
client's money. Any output carrying them is client data by definition, so no
no-PII survey can ever contain it. This is not a limitation of the prompt; it is
what the finding is about.

Three routes, and they are not equivalent:

1. **Hand-key one real scan, in-house.** Someone types the date, description and
   amount of every row of one scanned statement into a CSV that stays in
   `samples/_private_staging/` (git-ignored, never leaves the building). ~20–40
   minutes for a few hundred rows. This is the only route that measures the
   degradations of the scanners actually in use, so it is the one that says
   whether any tuning is real.
2. **A synthetic scan harness — buildable with no data at all.** Render a
   statement with known values, rasterise it at realistic DPI, degrade it
   (blur / noise / skew / JPEG), OCR it, and compare against the truth we started
   from. Repeatable, zero PII, and it is what tells you WHERE digits break —
   8↔B, 5↔S, 0↔O, a lost decimal point, a thousands separator read as a full
   stop. It cannot tell you whether those are the failures your real scans have.
3. **A genuinely scanned public specimen** with published figures. Cheapest, but
   there is no guarantee one exists that resembles the statements in use.

**Recommended order: 2, then 1.** The harness makes the failure modes visible and
tunable without waiting for anyone; the single hand-keyed scan then confirms the
tuning against reality. Doing 1 alone gives one data point with no way to
generalise; doing 2 alone risks tuning for degradations that never occur here.

**Where the survey still helps N16:** it cannot supply the truth, but section 1.5
and the scan questions describe the *character* of the degradation — DPI, whether
the type is tightly set, whether digits touch, whether the page looks faxed. That
is what decides which degradations the harness should model, so a handful of scan
surveys is worth collecting before building it.

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

## Last two, found by driving Admin after the round

| ID | Severity | Status | Finding | Evidence |
|---|---|---|---|---|
| N135 | medium | **fixed** | **An Admin action acted on a template nobody chose.** The template picker is rebuilt whenever the set changes (save, hide, delete) and was rebuilt with no `selected`, so selectize fell back to the first option. Two consequences: the confirmation for what you just did was wiped by the picker's own observer - which Delete worked around with a toast and Hide and Save did not, so they completed in silence - and the *next* click acted on whatever the picker had jumped to. Measured live: "Only USER templates can be hidden" about a shipped template the operator never picked. Fixed at the root by keeping the selection when it still exists; a deleted id cannot, and falling back is correct there. Hiding also now reports itself in a way that survives the redraw, by name rather than by id. Adding the fix tripped the suite's own admin guard - the observer reads an admin input now, so it re-verifies the session like every other privileged handler - which is that guard working. | app.R picker rebuild, `input$adm_tpl_hide` |
| N136 | medium | **fixed** | `config/config.example.yaml` documented a control that was deliberately deleted: *"Whether Convert's 'Include user-created templates' box starts TICKED"*. There is no such box, and `docs/operational/README.md` sends admins to that file as the annotated example. An admin could set the value false expecting analysts to tick a box per conversion, and instead silently stop every user-built template being used, with nothing on screen to turn back on. A stale comment in `app.R` described the same removed tick-box. | config/config.example.yaml, app.R |

## Closing notes on the last three

**N46 (design system) - fixed, and not the way it first looked.** The obvious tidy-up
was to replace the raw hex in `app.R` with `var(--token)`. That would have **broken the
X-ray**: those colours are handed to base-R graphics (`rect`, `text`, `border`), which
cannot read a CSS variable. The rule that is actually true is narrower - a colour with a
token must be spelled the same in both languages - so `app.R` now declares `PALETTE` and
a test asserts it agrees with `app.css` value for value. Colour uses in `app.R` went from
**99 to 56**, and the three near-misses (`#b00020`, `#137333`, `#c77700`) are gone.
Inline `style=` attributes were left alone: they are not the defect, and rewriting 138 of
them is a refactor with real risk to the screen and no correctness gain.

A warning worth keeping. The first attempt used `perl -pi -e 's/"#b00020"/PALETTE$bad/g'`,
and perl read `$bad` as one of its own variables and interpolated it to nothing - so every
site became a bare `PALETTE`, including `if (ok) PALETTE else PALETTE`. Success and failure
would have rendered the same colour and every X-ray layer would have collapsed into one.
**4,499 assertions still passed.** The only thing that caught it was the guard written in
the same sitting, which is the argument for writing the test before trusting the sweep.

**N47 (comment density) - wont-fix, deliberately.** `R/batch.R` is still 93 comment lines
to 67 of code (1.39) and `R/row_coverage.R` 71 to 105 (0.68), against `R/schema.R` at 0.10.
The finding's own last sentence is the reason: the surviving comments record WHY something
surprising is the way it is, and this file is the evidence - the note above about perl
eating `$bad` is exactly the kind of thing that only survives in a comment. Trimming to a
ratio would delete the codebase's best feature to satisfy a number nobody reads. Reopen it
if a specific comment is found to be *wrong*, which is a different and much worse problem.

**N111 - fixed.** `dates_within_period` printed a date RANGE under **Expected** and a
COUNT of offending rows under **Actual** - "Expected 2025-08-13..2025-09-01, Actual 34" -
two different kinds of thing under two headings that promise a comparison. Actual is now
the span the rows really cover, which is comparable to the period; the count was never
lost, it is the detail sentence and the discrepancy.

## The report paradigm (N145-N151) - new capability, its own risks

Not a sweep: these are the defects that were **designed out** while building the
multi-table document extractor (1.5.0), recorded here because each is a way this
feature could silently produce a wrong figure, and the reason it cannot is a
specific decision that a later change could undo.

**N145 (silent truncation) - closed by design.** A template drawn on a copy of a
report whose schedule ran to page 5 reads the same schedule on a copy that runs to
page 7 and **stops at 5**. Every row that does come out is correct, so nothing on
screen looks wrong. `doc_locate_table()` follows a table past its declared end while
the pages after it keep repeating its header, and the extension is reported in the
result and on screen. Test: *"a table longer on THIS document than on the example
is followed, and it is recorded"*.

**N146 (a column deleted by an overlap) - closed at save time.** A word can land in
only one column band and the earlier band wins, so two bands that overlap do not
duplicate a value - they **delete it** from the second column, and every row still
looks complete. `validate_document_template()` refuses overlapping bands before the
template is written, naming both columns.

**N147 (a band drawn slightly too narrow) - closed by measurement.** A right-aligned
money column whose band stops four points short loses every figure in it, and the
remaining columns still read perfectly. Every word inside a table's boundary that no
column claimed is recorded and counted on the result screen (`unclaimed_words`), and
the column's fill rate comes back 0 and flagged. Test: *"a column band drawn too
narrow shows up as unclaimed words, not as a clean read"*.

**N148 (two rows read as one) - closed by learning the pitch.** A fixed row tolerance
is right for one page shape only; across thirty tables in one report the line pitch
runs from about 10pt to 24pt. Too large **merges two rows into one**, which is the
silent corruption the project exists to prevent. The tolerance is learned per table
from the page's own line spacing, floored at the proven statement tolerance and
capped at 14 so a widely-leaded title block cannot swallow the value printed under
its label.

**N149 (a stale position that still reads something) - closed by reporting it.**
Reading a table from where it sat on the example, on a document where it has moved,
does not fail: it quietly picks up the title line and the header row as if they were
data. So a table is located by its **heading wording** first and its coordinates
last, which of the two was used is reported per table, and a position-only match
sends the whole conversion to *needs review*. Test: *"with no heading to go on it
falls back to position, and says THAT"* asserts the three junk rows explicitly.

**N150 (a report reaching the dashboards) - closed in one line.** There is no
reconciliation behind a report, so nothing here can tell a right figure from a wrong
one. `write_feed()` refuses `kind = "tables"` outright, beside the same refusal for
forms - enforced in code rather than left to the fact that no screen offers it.

**N151 (a text-only table losing its heading row) - open, and deliberately the
harmless direction.** Auto-detection calls the first line a header when it carries no
figures and the lines below do. A table of names and addresses satisfies neither
half, so its heading line is proposed as a first row of data. That row is visible in
the preview and *Header lines* fixes it in one click; the opposite guess would delete
a real row with nothing on screen to notice. Pinned by *"a table with no figures
anywhere keeps its heading line as a row, visibly"*. It would take font weight or
size from `pdf_data(font_info = TRUE)` to do better, which is not available on every
pdftools build this ships against.

## What sixteen awkward documents found (N152-N160)

Not review and not a sweep: sixteen fixtures were built for the shapes a printed
report actually uses (`tests/testthat/helper-doc-hard.R`) and run against the
report extractor. **Nine of them broke it.** Every finding below is a
measurement, and every one is now a test in `tests/testthat/test-doc_hard.R` that
records the number it started from.

They were measured under WebR -- R 4.6.0 compiled to WebAssembly, run from Node
with this repository mounted -- because the machine doing the work had no R and
no way to install one. See [`../../tools/webr/README.md`](../../tools/webr/README.md).
The report extractor is base-R only by design, which is what made that possible.

**N152 (rows merged by an uneven row height) - fixed. The worst of them.** A
table with 10pt and 26pt rows alternating has a median gap of 26; six tenths of
that swallowed every 10pt gap. Six rows came back as **four**, with `R2 R3` in one
cell and `20.00 30.00` in another. Two rows merged is the silent corruption this
project exists to prevent, there is no reconciliation here to catch it, and
nothing about the output looks wrong. The tolerance now follows the **lower
quartile** of the gaps -- the tightest spacing the table actually uses -- so it is
right for every row rather than for the average one. On an evenly-set table the
two numbers are identical, so nothing else moved.

**N153 (a followed table swallowed the tables under it) - fixed.** A schedule
followed onto page 5 returned **93 rows instead of 83**: a fee table and a contact
table read as ten more transactions. An open-ended window now stops where the
rows stop -- a gap much bigger than the ones this table has been using AND a line
filling fewer of its columns -- and where it stopped is reported. Both halves are
needed: the gap alone would cut a schedule at the whitespace before its own total
row, which is N156.

**N154 (two schedules joined into one) - fixed.** Two different accounts'
schedules, consecutive pages, identical headers, identical geometry: read as
**one table of twelve rows** under the first account's name. Nothing measurable
separated them except that page two carried its own heading. That is now the
deciding test -- a genuine continuation page opens with the repeated column names
and nothing above them.

**N155 (a continuation header that said "(continued)") - fixed.** An 18-row
schedule came back as two tables of eight and ten, because the header on page 2
was not identical to page 1's. A repeated header now matches on prefix -- and
still refuses the same column names in a **different order**, which is a
different table that happens to share a vocabulary.

**N156 (the total row left out) - fixed.** A schedule sets its total apart with a
blank line, an underline and a row with two cells filled. Every one of those ends
a run of tabular lines, so the row somebody actually came for was the row left
behind. A run now reaches down past the break for up to three lines that still
land in at least two of its own bands, skipping printed rules. The same fix
recovers a total indented past the first column.

**N157 (a bordered table proposed no columns) - fixed.** `+--------+-------+`
rules drawn as characters span the full width and merge every column band into
one, so a bordered table looked like nothing at all: **zero tables proposed**.
Printed rules are now transparent -- they neither start a run, nor end one, nor
contribute a band, nor become a row.

**N158 (a condensed table read as one cell per row) - fixed.** 2pt gutters with a
`|` glyph in them, narrower than any word space. A lone vertical bar is a border,
not a value: dropped at the one point both the proposer and the reader come
through, which separates the columns on real whitespace AND stops a cell coming
out as `| 1,240.55`.

**N159 (a two-column table was not a table) - fixed.** A valuation of item and
amount, five rows: **proposed as nothing**. Two columns are now allowed on a
longer run -- four consecutive rows in the same two bands. The front matter of an
ordinary report still proposes no tables, which is what the three-cell rule was
protecting.

**N160 (a wrapped description ended its table) - fixed, and caused by N152's
fix.** Once the tighter tolerance correctly made a wrapped line its own line, the
one-cell line broke the run and the table was **not proposed at all**. A wrap --
indented past where the rows start, within a line of the row above, filling one
column -- is folded into that row. Worth recording as a pair: the first fix was
right and made the second one necessary, which is the argument for building the
fixtures before changing the rule.

**Still limitations, and asserted as tests.** Two tables printed side by side
read as one of six columns (the ink is identical to a genuine six-column table);
a spanning header cell becomes the proposed table name; a table with no figures
anywhere keeps its heading line as a row (N151). Each has a test asserting the
CURRENT behaviour, because a limitation nobody wrote down cannot be told apart
from a bug nobody noticed.

