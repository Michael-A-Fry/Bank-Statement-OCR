# convert.R -- orchestrator. NEVER throws: every failure becomes a `failed`
# result with an actionable message. Logs exactly one line per run.

# ---- build provenance: making determinism PROVABLE ---------------------------
# The charter promises "same input + same template = same answer", but nothing in
# the outputs said WHICH build produced a figure, so the promise could never be
# CHECKED after the fact: rebuild the engine, nudge a template, and the numbers
# change with no trace. Two stamps close that:
#   * engine_version() -- the repo-root VERSION file (one line, e.g. "1.0.0").
#   * template_sha256  -- a content hash of the template AS LOADED, so an edited
#     YAML is visibly a different template even at the same declared `version:`.
# Both are stamped into the run log, the JSON output and the feed manifest.

# engine_version() -- the build stamp, read ONCE per session (the file cannot
# change under a running air-gapped install). Missing/unreadable -> "unknown":
# an honest "we don't know" beats a guessed number, and it never errors.
.VERSION_CACHE <- new.env(parent = emptyenv())
engine_version <- function() {
  if (!is.null(.VERSION_CACHE$v)) return(.VERSION_CACHE$v)
  # ENGINE_ROOT is how the scripts/tests point at the install; "." covers the
  # app + RUN-ME.bat, which always start in the repo root.
  cand <- unique(c(file.path(Sys.getenv("ENGINE_ROOT", "."), "VERSION"), "VERSION"))
  v <- NA_character_
  for (p in cand) {
    if (!file.exists(p)) next
    ln <- trimws(safe(safe_readlines(p), character(0)))
    ln <- ln[!is.na(ln) & nzchar(ln)]
    if (length(ln)) { v <- ln[1]; break }
  }
  .VERSION_CACHE$v <- if (is.na(v)) "unknown" else v
  .VERSION_CACHE$v
}

# .text_sha256(x) -- content hash of a string (same fallback order as
# file_sha256). NA when no hashing package is installed, never an error.
.text_sha256 <- function(x) {
  x <- paste(as.character(x), collapse = "\n")
  if (requireNamespace("openssl", quietly = TRUE))
    return(paste0(openssl::sha256(charToRaw(x))))
  if (requireNamespace("digest", quietly = TRUE))
    return(digest::digest(x, algo = "sha256", serialize = FALSE))
  NA_character_
}

# template_sha256(template) -- hash the template's canonical YAML (the load-time
# `origin` marker stripped by template_yaml(), so it round-trips). Deterministic
# for identical content, and different the moment a band or format is edited.
template_sha256 <- function(template) {
  if (is.null(template)) return(NA_character_)
  safe(.text_sha256(template_yaml(template)), NA_character_)
}

# log_run(logdir, result) -- write THE run-log record: exactly ONE file per run,
# named by run_id, holding the FINAL outcome.
#
# WHY this is a separate function rather than inline in convert_statement: the
# front door (convert_document) may fall back to FORM extraction after the
# statement pipeline returns "unsupported". When convert_statement wrote the log
# itself, a successfully converted IRD/KiwiSaver form was recorded as an
# unsupported statement -- and counted in Admin's "build these next" queue, which
# reads exactly that field. Now convert_statement BUILDS the record
# (result$run_log) and only writes it when it is the whole story; convert_document
# writes it once, after the final outcome is known. No record is ever rewritten.
log_run <- function(logdir, result) {
  rec <- result$run_log
  if (is.null(rec) || !length(rec)) return(invisible(NULL))
  safe(write_log_record(logdir, "runs", rec$run_id %||% result$run_id %||% "unknown", rec))
}

# .unreadable_reason(input) -- one sentence when the READER got nothing usable out
# of the file, NULL when it did. This is the line between "could not be read" and
# "no template for this layout yet", and the two need opposite answers: the first
# is the sender's problem (re-export, rescan, unlock), the second is a template to
# build. Getting it wrong wastes the analyst's time on a file that can never
# convert, so each test below is a FACT about the bytes, never a judgement:
#   pdf         -- read_pdf sets `pages` to NULL only when pdftools could not read
#                  the document at all. A SCAN reads fine and comes back with empty
#                  pages plus scanned_no_ocr, and keeps its own (correct, louder)
#                  diagnostic -- so this must test the reader's verdict, not the
#                  emptiness of the text.
#   delimited   -- a delimited file is a table only if its lines SEPARATE. The
#                  separators tested are the ones the installed templates declare
#                  (plus the four any reader falls back to), so teaching the tool a
#                  bank that separates some other way is still a YAML edit and can
#                  never turn that bank's export into "unreadable". A header-only
#                  export DOES separate, so it still reaches "matched the wording,
#                  read no transactions" rather than being called unreadable.
#   excel       -- no sheet, or a sheet with no columns, is nothing to read.
.unreadable_reason <- function(input, templates = NULL) {
  kind <- input$kind %||% NA_character_
  if (identical(kind, "pdf")) {
    if (is.null(input$pages) || !length(input$pages))
      return(paste("no text could be read from this PDF - it is damaged, encrypted,",
                   "or not a PDF at all"))
    return(NULL)
  }
  if (identical(kind, "delimited")) {
    lines <- input$lines %||% character(0)
    lines <- lines[!is.na(lines) & nzchar(trimws(lines))]
    if (!length(lines)) return("this file is empty - there is nothing in it to read")
    seps <- unlist(lapply(templates %||% list(),
                          function(t) as.character(unlist(t$delimiter %||% character(0)))))
    seps <- unique(c(seps[!is.na(seps) & nzchar(seps)], ",", "\t", ";", "|"))
    # fixed = TRUE: these are separator strings, and "|" is a regex alternation.
    if (!any(vapply(seps, function(s) any(grepl(s, lines, fixed = TRUE)), logical(1))))
      return(paste("no rows of separated values were found in this file - it holds",
                   "text, but not a table"))
    return(NULL)
  }
  if (identical(kind, "excel")) {
    tbl <- input$table
    if (is.null(tbl) || !ncol(tbl))
      return("no worksheet with any data could be read from this workbook")
    return(NULL)
  }
  NULL
}

# convert_statement(...) -> result (build-contract sections 6, 7).
# `log = FALSE` builds the run record on the result (result$run_log) WITHOUT
# writing it, so a caller that may still change the outcome (convert_document's
# form fallback) can write exactly one, final record itself.
convert_statement <- function(path, bank = NULL, statement_type = NULL,
                              outdir = "out", templates_dir = "templates",
                              user_templates_dir = "templates_user",
                              requested_by = NULL,
                              formats = c("xlsx", "csv", "json"),
                              logdir = "logs", redaction_rects = NULL,
                              force_template = NULL, force_rows = NULL,
                              log = TRUE) {
  # NOTHING THAT TOUCHES `path` HAPPENS OUTSIDE THE FUNNEL. docs/design.md and
  # docs/overview.md both state "never throws at the front door" as a fact, but the
  # tryCatch used to open below this preamble -- so basename(path) sat outside it
  # and convert_document(1L) threw "a character vector argument expected" before a
  # single guard ran. Shiny always hands over a character datapath, so nothing was
  # breaking; the danger was that the next maintainer to add a line here would
  # believe it was protected. What is left above the tryCatch cannot throw for any
  # input: `safe()` covers the hash and the config, and the run id is built from
  # values that are already safe. Everything else moved inside.
  base <- "input"
  cfg <- safe(load_config(), .config_defaults())      # for the metadata-capture level
  t0 <- Sys.time()                                    # elapsed timing (metadata only)
  # run_id: an intentional per-RUN handle for this conversion (feedback and logs
  # point back to it). Content hash (first 10 chars) + a UTC timestamp -- UTC and
  # an explicit zone so the id is reproducible across hosts/timezones, matching
  # the deterministic workbook/CSV/JSON outputs (only the timestamp part varies
  # per attempt, by design).
  sha <- safe(file_sha256(path), NA_character_)
  # (content hash + UTC second + a short random suffix). The suffix matters: without
  # it, converting the SAME statement twice inside one second produced ONE run_id, and
  # the second run overwrote the first's audit record -- a forensic tool must not lose
  # a record of work it did.
  run_id <- paste0(substr(if (is.na(sha)) "na" else sha, 1, 10), "-",
                   format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"), "-",
                   paste0(sample(c(0:9, letters[1:6]), 4, replace = TRUE), collapse = ""))
  result <- new_result(status = "failed", template_id = NA_character_,
                       messages = character(0))
  detected_template <- NA_character_
  template_version <- NA
  row_count <- 0L
  kpi_fail_count <- 0L
  trust_level <- NA_character_
  # layout/detection signals for the admin reports (clustering unsupported files)
  closest_template <- NA_character_
  detect_detail <- NA_character_
  layout_sig <- NA_character_
  layout_hint <- NA_character_
  template_origin <- NA_character_
  tmpl_sha <- NA_character_        # build provenance: WHICH template content ran

  outcome <- tryCatch({
    base <- tools::file_path_sans_ext(basename(path %||% "input"))
    templates <- load_template_set(templates_dir, user_templates_dir)
    input <- read_input(path, redaction_rects = redaction_rects)
    # THE FILE ITSELF COULD NOT BE READ. That is not the same answer as "we have
    # never seen this layout", and it must never be given the same one: a corrupt
    # PDF and a note-to-self .txt both came back as "No template read this
    # statement - set one up and it converts every time", sending somebody off to
    # spend two minutes teaching the tool a file that has nothing in it to teach,
    # while the server console printed "Couldn't read xref table". Raised through
    # the funnel above, which is what turns it into a `failed` result with the
    # `unreadable` diagnostic and the wording that already exists for it.
    if (!is.null(why <- .unreadable_reason(input, templates))) stop(why, call. = FALSE)
    meta <- extract_metadata(input)
    multi <- detect_multiple_statements(input, meta)
    lsig <- layout_signature(input)
    layout_sig <- lsig$signature
    layout_hint <- lsig$hint

    # force_template: the user picked an exact template on Convert -> skip
    # detection and use it directly (still runs full reconciliation, so a wrong
    # forced pick still surfaces as needs_review, never silently trusted).
    if (!is.null(force_template) && nzchar(force_template) && !is.null(templates[[force_template]])) {
      det <- list(template_id = force_template, matched = TRUE, score = NA_real_,
                  margin = Inf, runner_up = NA_character_,
                  candidates = data.frame(id = force_template, score = NA_real_,
                                          stringsAsFactors = FALSE),
                  eligible_ids = force_template, tied = character(0),
                  detail = "template chosen by the user")
    } else {
      det <- detect_statement(input, templates, hint_bank = bank,
                              hint_type = statement_type)
    }
    closest_template <- det$template_id
    detect_detail <- det$detail
    parsed <- recon <- template <- NULL   # set in the matched branch; NULL otherwise

    # A TIE IS NOT A DEAD END.
    # Two templates fitting the same wording is a question only a maintainer can
    # answer -- an accountant cannot know which internal variant a bank used to
    # print her statement. Stopping her to ask was asking a question the person in
    # front of us cannot answer (charter: the interface rule), and it left her with
    # no spreadsheet over a problem that was never hers.
    #
    # So: take the best candidate (a tested template over a hand-built one, then
    # deterministic), convert, and HOLD THE RUN FOR REVIEW with the tie named. She
    # gets her data; the duplicate templates go to whoever can retire one; and
    # nothing slips into the dashboards, because needs_review never reaches them.
    # This is not a guess -- every figure still goes through full reconciliation,
    # so the wrong variant shows up as a failing check rather than a clean answer.
    ambiguous <- !isTRUE(det$matched) && length(det$tied) >= 2
    if (!isTRUE(det$matched) && !ambiguous) {
      result$status <- "unsupported"
      result$template_id <- if (is.na(det$template_id)) NA_character_ else det$template_id
      # det$detail names the closest template, its score and the phrases it wanted
      # ("closest anz_everyday_pdf score 2/3 (missing ...)"). That is real evidence
      # and it is kept -- build_diagnostics puts it in the unknown_format diagnostic,
      # behind "Show me how it read this". It is not put on the verdict card,
      # because a template id and a fraction are not something the person holding
      # the statement can act on. Charter: the interface rule.
      result$messages <- status_message("unsupported",
        "we don't have a template for this layout yet")
      result$candidates <- det$candidates
      result$detect <- list(margin = NA_real_, runner_up = det$runner_up,
                            thin = FALSE, ambiguous = FALSE, tied = character(0))
      result$trust <- list(level = "low", score = 0, reasons = det$detail)
      result$metadata <- c(meta, list(multiple = multi))
      result$diagnostics <- build_diagnostics("unsupported", det = det,
        metadata = list(multi = multi, pages = meta$pages_actual, max_page_pt = meta$max_page_pt,
                        # A scan we could not machine-read looks identical to an
                        # unknown layout unless we say so -- carry the reason through.
                        scanned_no_ocr = input$meta$scanned_no_ocr %||% 0L,
                        ocr_tools = input$meta$ocr_tools_available %||% TRUE))
    } else {
      # .read_with(tid) -- read the statement with ONE template, all the way to a
      # reconciled result. Opt-in auto-split (see split.R): if the template declares
      # a `split:` block AND the upload looks like a bundle, segment it and reconcile
      # each statement on its own. Returns NULL unless the boundaries are
      # independently confirmed -- so a template that hasn't opted in, or a bundle
      # whose boundaries can't be trusted, falls straight through to the normal
      # single parse, which (because multi$likely_multiple) routes to needs_review:
      # the safe flag-and-refuse default is unchanged.
      .read_with <- function(tid) {
        tpl <- templates[[tid]]
        sb <- if (isTRUE(multi$likely_multiple)) safe(split_bundle(input, tpl, meta), NULL) else NULL
        if (!is.null(sb))
          return(list(template = tpl, parsed = sb$parsed, recon = sb$recon,
                      sb = sb, did_split = TRUE))
        p <- parse_statement(input, tpl, force_rows = force_rows, meta = meta)
        list(template = tpl, parsed = p, recon = reconcile(p, tpl),
             sb = NULL, did_split = FALSE)
      }

      # WHICH template actually reads it. On a tie, `det$template_id` is the top
      # scorer over ALL templates, and `det$tied` is the templates that met their
      # OWN min_score and share the top ELIGIBLE score -- two different sets. A
      # template can out-score both tied ones and still be ineligible (it wanted 3
      # phrases and found 2), and reading with THAT one stamped its bank on the
      # workbook, the CSV and the JSON while the screen named the other two. The
      # figures reconciled cleanly, so nothing looked wrong: a wrong figure that
      # LOOKS right, which is the one outcome this tool must never produce.
      # det$tied is already ordered by the principled order promised above -- a
      # shipped/tested template over a hand-built one, then the filename hint, then
      # id -- so its first entry IS the best candidate, deterministically.
      used_id <- if (ambiguous) det$tied[1] else det$template_id
      att <- .read_with(used_id)
      template <- att$template
      parsed <- att$parsed; recon <- att$recon; sb <- att$sb; did_split <- att$did_split
      detected_template <- template$id
      template_version <- template$version %||% NA
      template_origin <- template$origin %||% "default"
      tmpl_sha <- template_sha256(template)
      row_count <- nrow(parsed$transactions)
      kpi_fail_count <- sum(recon$kpis$status == "fail")
      trust_level <- recon$trust$level

      # A THIN detection margin (won over the runner-up by only 1 fingerprint
      # phrase) means a near-duplicate template nearly matched too -- exactly the
      # "matched but maybe the wrong variant" case. Treat it as needs_review so the
      # analyst confirms the template, even when every KPI passes.
      thin_match <- is.finite(det$margin) && det$margin <= 1 && !is.na(det$runner_up)
      # A CONFIRMED auto-split has handled the bundle (every statement parsed +
      # reconciled on its own), so being multiple is no longer a reason to force
      # review, nor to raise the "split this into one file" diagnostic -- the
      # rolled-up trust / KPI outcomes decide, like any other run. One fact, used by
      # both the status gate and the diagnostics; raw `multi` still records that this
      # upload WAS a bundle for the metadata + run log.
      multi_resolved <- if (did_split) utils::modifyList(multi, list(likely_multiple = FALSE)) else multi
      # NO TRANSACTIONS IS NOT "REVIEW THIS" -- there is nothing to review. It used
      # to come out as needs_review ("parsed 0 row(s) but review needed"), an amber
      # warning attached to an empty workbook, which reads as a quality niggle
      # rather than "this did not work". It is the same outcome as having no
      # template at all: the wording matched, the layout did not. So say that, and
      # route it where a new/fixed template gets built.
      status <- if (row_count == 0L) {
        "unsupported"
      } else if (kpi_fail_count > 0 || identical(recon$trust$level, "low") ||
                 isTRUE(multi_resolved$likely_multiple) || thin_match || ambiguous) {
        "needs_review"
      } else {
        "ok"
      }
      diag <- build_diagnostics(status, parsed = parsed, recon = recon,
        metadata = list(multi = multi_resolved, pages = meta$pages_actual, max_page_pt = meta$max_page_pt,
                        template = template, pdf_doc = input$meta$pdf_doc,
                        # A template that matched but read nothing is a DIFFERENT
                        # problem from an unknown layout, and has a different fix:
                        # the template exists, its columns are in the wrong place.
                        matched_empty = if (row_count == 0L) template$id else NULL,
                        tied = if (ambiguous) det$tied else NULL,
                        # which of the tied templates produced these figures
                        tied_used = if (ambiguous) template$id else NULL))
      outputs <- write_outputs(parsed, recon, outdir, base, formats,
        diagnostics = diag, metadata = meta,
        build = list(engine_version = engine_version(), template_id = template$id,
                     template_version = as.character(template_version),
                     template_sha256 = tmpl_sha))

      msg <- if (status == "ok") {
        status_message("ok", sprintf("matched %s, %d row(s), trust %s",
                                     template$id, row_count, recon$trust$level))
      } else if (row_count == 0L) {
        status_message("unsupported",
          sprintf("%s matches the wording on this statement but read no transactions from it",
                  template$id),
          "the columns are most likely in the wrong place - open this statement in the template toolkit and check where they sit")
      } else {
        status_message("needs_review",
          sprintf("parsed %d row(s) but review needed", row_count),
          paste(recon$trust$reasons, collapse = "; "))
      }
      # Named, not hidden -- but stated as a fact about the TOOL, not as a task for
      # the person reading it. She has her data; somebody else retires the duplicate.
      # It names the one that WAS used and the one(s) that were not: listing the tie
      # without saying which of them produced these figures leaves the reader unable
      # to check the answer against the right template.
      if (ambiguous) {
        others <- setdiff(det$tied, template$id)
        msg <- c(status_message("needs_review",
          sprintf("%s templates fit this statement equally well; it was read with %s",
                  length(det$tied), template$id),
          sprintf("worth a check against the alternative%s: %s",
                  if (length(others) == 1L) "" else "s", paste(others, collapse = ", "))), msg)
      }

      result$status <- status
      result$template_id <- template$id
      result$trust <- recon$trust
      result$kpis <- recon$kpis
      result$header <- parsed$header
      result$outputs <- outputs
      # The governed feed's ONLY source of transaction values. It used to re-read
      # the output CSV, and utils::read.csv's type inference silently mangled it:
      # a leading-zero code "000001" became 1 and a reference "0012345" became
      # 12345, so the dashboard showed figures the workbook never contained. The
      # apostrophe guard .spreadsheet_safe() adds purely so Excel won't EXECUTE a
      # description starting "=" survived into the feed too. This is the same
      # table the workbook/CSV show, taken BEFORE that display-only guard, so the
      # feed row and the workbook row hold byte-identical values.
      result$feed_rows <- display_transactions(parsed$transactions, parsed$extras)
      result$diagnostics <- diag
      result$coverage <- field_coverage(parsed, template)
      result$metadata <- c(meta, list(multiple = multi,
        split = if (did_split) list(n_statements = sb$n_statements, on = sb$on,
                                    statements = sb$statements) else NULL))
      result$messages <- msg
      # Candidate templates + margin, for the "matched but maybe wrong" panel.
      result$candidates <- det$candidates
      result$detect <- list(margin = det$margin, runner_up = det$runner_up,
                            thin = thin_match, ambiguous = ambiguous,
                            tied = if (ambiguous) det$tied else character(0))
    }
    # LOCAL-ONLY metadata capture (the ML goldmine). Built here where every
    # artifact is in scope; written after the run log below. NEVER enters the feed.
    result$metadata_capture <- safe(capture_metadata(list(
      run_id = run_id, ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      requested_by = requested_by %||% current_user(), sha = sha,
      input = input, parsed = parsed, recon = recon, det = det, meta = meta,
      multi = multi, template = template, status = result$status,
      # already computed above -- reuse instead of recomputing inside capture.
      layout_sig = lsig, coverage = result$coverage,
      elapsed_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000), cfg), NULL)
    result
  }, error = function(e) {
    r <- new_result(status = "failed")
    # NOT "...and matches a template". Everything that lands here failed BEFORE any
    # template was in play, so pointing at templates sends the reader to build one
    # for a file the tool could not even open. Same cure as the `unreadable`
    # diagnostic (R/diagnose.R), so the message and the diagnostic say one thing.
    r$messages <- status_message("failed", conditionMessage(e),
                                 "check the file opens, is the type it claims to be, and is not password-protected or damaged")
    r$diagnostics <- build_diagnostics("failed", messages = r$messages)
    r
  })

  result <- outcome
  result$run_id <- run_id

  # ---- run log: one file per run (concurrency-safe, no shared append) ----
  # requested_by defaults to the OS-authenticated user, so every conversion is
  # attributed to a real person without any login prompt.
  # BUILT here, WRITTEN by log_run() -- see log_run() for why the write is
  # separable (the form fallback in convert_document may still change `status`).
  result$run_log <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run_id = run_id,
    kind = "statement",
    requested_by = requested_by %||% current_user(),
    # safe(): the run log is written AFTER the funnel closes, so this is one of the
    # few lines left that a hostile `path` could still throw from.
    source_file = safe(basename(path %||% NA_character_), NA_character_),
    source_sha256 = sha,
    bank_hint = bank %||% NA_character_,
    detected_template = if (result$status %in% c("ok", "needs_review")) result$template_id else NA_character_,
    template_origin = template_origin,
    closest_template = closest_template,
    detect_detail = detect_detail,
    layout_signature = layout_sig,
    layout_hint = layout_hint,
    template_version = template_version,
    # build provenance -- WHICH engine build and WHICH template content ran, so
    # "same input + same template = same answer" can be checked, not just claimed.
    engine_version = engine_version(),
    template_sha256 = tmpl_sha,
    status = result$status,
    trust_level = result$trust$level %||% NA_character_,
    row_count = row_count,
    n_fields = NA_integer_,          # forms only (see convert_document)
    kpi_fail_count = kpi_fail_count,
    pages = result$metadata$pages_actual %||% NA_integer_,
    period_start = result$metadata$period_start %||% NA_character_,
    period_end = result$metadata$period_end %||% NA_character_,
    n_accounts = result$metadata$n_accounts %||% NA_integer_,
    multiple_statements = isTRUE(result$metadata$multiple$likely_multiple),
    message = paste(result$messages, collapse = " | ")
  )
  if (isTRUE(log)) log_run(logdir, result)

  # ---- metadata capture: LOCAL ONLY, kept forever (logs/metadata/), never fed ----
  # This describes the STATEMENT ATTEMPT (how the file read, what the columns
  # looked like) and stays per-attempt even when convert_document later succeeds
  # with a form template -- the statement pipeline really did find no match, and
  # that is the signal the capture exists to keep. The RUN record is the single
  # per-run OUTCOME; this is not a second copy of it.
  safe(write_metadata_record(logdir, run_id, result$metadata_capture))
  result$metadata_capture <- NULL   # transient -- not part of the returned result

  result
}
