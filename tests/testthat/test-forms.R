# Tests for the mode:fields orchestration (R/forms.R): load, detect, convert.

test_that("load_fields_templates returns only mode:fields templates", {
  ft <- load_fields_templates(fields_templates_dir(), NULL)
  expect_true("anz_kiwisaver_fields" %in% names(ft))
  expect_true(all(vapply(ft, is_fields_template, logical(1))))
})

test_that("detect_form matches on identifying phrases, else unsupported", {
  ft <- load_fields_templates(fields_templates_dir(), NULL)
  hit <- list(pages = "ANZ KiwiSaver Scheme\nOpening balance $1.00\nClosing balance $2.00")
  d <- detect_form(hit, ft)
  expect_true(d$matched)
  expect_equal(d$template_id, "anz_kiwisaver_fields")

  miss <- list(pages = "A completely unrelated document with no identifying phrases")
  expect_false(detect_form(miss, ft)$matched)

  expect_false(detect_form(hit, list())$matched)   # no templates installed
})

test_that("detect_form reports UNMATCHED on an equal-specificity tie (never guesses)", {
  mk <- function(id) list(id = id, mode = "fields", format = "pdf",
    fingerprint = list(page_contains_all = c("IR3", "Tax year")),
    fields = list(a = "A"))
  ft <- list(ird_a = mk("ird_a"), ird_b = mk("ird_b"))
  page <- list(pages = "IR3 return for the 2025 Tax year")
  d <- detect_form(page, ft)
  expect_false(d$matched)                       # two equally-specific -> ask, don't guess
  expect_true(grepl("equally", d$detail))
  # ...and it does NOT tell anybody to "pick a bank": no form screen has a bank
  # control, so that was an instruction nobody could follow.
  expect_false(grepl("pick a bank", d$detail, fixed = TRUE))
  # ...and convert_form must therefore NOT silently extract with a guessed template
  od <- tempfile(); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  # (no real file needed: detect happens on text; use the sample dir with a tie)
  # a UNIQUE best still matches
  ft2 <- list(ird_a = mk("ird_a"),
              ird_b = list(id = "ird_b", mode = "fields", format = "pdf",
                fingerprint = list(page_contains_all = "IR3"), fields = list(a = "A")))
  expect_true(detect_form(page, ft2)$matched)
  expect_equal(detect_form(page, ft2)$template_id, "ird_a")  # more specific wins
})

test_that("extract + write_form_outputs produce the labelled values", {
  ft <- load_fields_templates(fields_templates_dir(), NULL)
  input <- list(pages = paste(
    "ANZ KiwiSaver Scheme",
    "Opening balance   $51,904.55",
    "Closing balance   $61,060.94",
    "Government contribution   $521.43", sep = "\n"))
  f <- extract_fields(input, ft[["anz_kiwisaver_fields"]])
  expect_equal(f$value[f$field == "opening_balance"], "$51,904.55")
  expect_equal(f$value[f$field == "government_contribution"], "$521.43")

  od <- tempfile(); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  paths <- write_form_outputs(f, od, "ks", c("csv", "json"))
  expect_true(all(file.exists(paths)))
  csv <- utils::read.csv(paths[grepl("\\.csv$", paths)], stringsAsFactors = FALSE)
  expect_true("value" %in% names(csv))
})

test_that("validate_fields_template flags a non-fields or empty template", {
  ok <- list(id = "x", mode = "fields", fields = list(a = "A"),
             fingerprint = list(page_contains_all = c("Kowhai KiwiSaver Scheme",
                                                      "Annual member statement")))
  expect_length(validate_fields_template(ok), 0)
  expect_true(length(validate_fields_template(list(id = "x", mode = "fields"))) > 0)
  expect_true(length(validate_fields_template(list(id = "x", fields = list(a = "A")))) > 0)
})

# ---------------------------------------------------------------------------
# F1-9: a form template's fingerprint faces the same distinctiveness gate as a
# statement template's. detect_form is reached for EVERY statement no transaction
# template recognised, and there is no reconciliation behind it -- so a loose
# fingerprint turns a correct "unsupported" verdict into a confident extraction.
# ---------------------------------------------------------------------------

test_that("a form template with no fingerprint, or a generic one, is invalid", {
  base <- list(id = "loose", mode = "fields", format = "pdf", fields = list(a = "A"))
  no_fp <- validate_fields_template(base)
  expect_true(length(no_fp) > 0)
  expect_match(paste(no_fp, collapse = " "), "fingerprint", fixed = TRUE)

  generic <- base; generic$fingerprint <- list(page_contains_all = c("Balance", "Date"))
  expect_true(length(validate_fields_template(generic)) > 0)   # every word is universal

  # ...and a real, distinctive one passes.
  good <- base; good$fingerprint <- list(page_contains_all = c("Balance",
                                                              "Kowhai Bank KiwiSaver"))
  expect_length(validate_fields_template(good), 0)
})

test_that("a form template that names a PERSON is rejected (no PII fingerprints)", {
  t <- list(id = "pii", mode = "fields", format = "pdf", fields = list(a = "A"),
            fingerprint = list(page_contains_all = c("Mr John Smith", "Annual statement")))
  probs <- validate_fields_template(t)
  expect_true(any(grepl("PERSON", probs)))
})

test_that("load_fields_templates skips an invalid template and SAYS which (never silent)", {
  d <- tempfile("ft_"); dir.create(d)
  yaml::write_yaml(list(id = "good_form", mode = "fields", format = "pdf",
    fingerprint = list(page_contains_all = c("Kowhai Bank KiwiSaver",
                                             "Annual member statement")),
    fields = list(opening_balance = "Opening balance")), file.path(d, "good.yaml"))
  yaml::write_yaml(list(id = "loose_form", mode = "fields", format = "pdf",
    fingerprint = list(page_contains_all = c("Balance")),
    fields = list(opening_balance = "Opening balance")), file.path(d, "loose.yaml"))

  ft <- load_fields_templates(d, NULL)
  expect_identical(names(ft), "good_form")          # the loose one cannot match anything
  errs <- attr(ft, "load_errors")
  expect_length(errs, 1)
  expect_match(errs, "loose_form")                  # named, so it can be fixed

  # the loose template can no longer hijack an unrecognised bank statement.
  page <- list(pages = "Kowhai Bank statement\nBalance 100.00\nDate 01/01/2026")
  expect_false(detect_form(page, ft)$matched)
})

test_that("the shipped form template still passes the gate", {
  ft <- load_fields_templates(fields_templates_dir(), NULL)
  expect_length(attr(ft, "load_errors"), 0)
  expect_true("anz_kiwisaver_fields" %in% names(ft))
})

test_that("convert_document routes a statement vs a form through one front door", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fdir <- fields_templates_dir()
  tdir <- templates_dir()
  od <- tempfile(); ld <- tempfile("dl_")   # own logdir: never write into the repo
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)

  # a form PDF: no transaction template matches -> falls back to form extraction
  fpdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(fpdf))
  fr <- convert_document(fpdf, outdir = od, templates_dir = tdir,
                         user_templates_dir = NULL, fields_dir = fdir, logdir = ld)
  expect_equal(fr$kind, "form")
  expect_true(fr$status %in% c("ok", "needs_review"))
  expect_true(fr$n_fields >= 4)

  # a document that matches nothing at all stays an unsupported statement
  bad <- tempfile(fileext = ".csv"); writeLines(c("a,b", "1,2"), bad)
  r2 <- convert_document(bad, outdir = od, templates_dir = tdir,
                         user_templates_dir = NULL, fields_dir = fdir, logdir = ld)
  expect_equal(r2$kind, "statement")
  expect_equal(r2$status, "unsupported")
})

# ---------------------------------------------------------------------------
# F1-15: ONE run record per run, holding the FINAL outcome. convert_statement
# used to write "unsupported" before the form fallback ran, so a form that
# converted perfectly was on record as a failure -- and counted in Admin's
# "build these next" queue, which reads exactly that field.
# ---------------------------------------------------------------------------

test_that("a converted form is logged ONCE, as a form, not as an unsupported statement", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fpdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(fpdf))
  od <- tempfile("fo_"); ld <- tempfile("fl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)

  fr <- convert_document(fpdf, outdir = od, templates_dir = templates_dir(),
                         user_templates_dir = NULL,
                         fields_dir = fields_templates_dir(),
                         logdir = ld)
  expect_equal(fr$kind, "form")
  expect_true(fr$status %in% c("ok", "needs_review"))

  rec <- read_runs(ld)
  expect_equal(nrow(rec), 1L)                              # exactly one record
  expect_identical(rec$status, "needs_review")             # the FINAL outcome
  expect_identical(rec$kind, "form")
  expect_identical(rec$detected_template, "anz_kiwisaver_fields")
  expect_true(rec$n_fields >= 4)
  # ...and it therefore does NOT show up as a format nobody has built yet.
  expect_equal(nrow(unsupported_clusters(read_runs(ld))), 0L)
})

test_that("a document nothing can read is still logged once, as unsupported", {
  od <- tempfile("fo_"); ld <- tempfile("fl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  bad <- tempfile(fileext = ".csv"); writeLines(c("a,b", "1,2"), bad)
  r <- convert_document(bad, outdir = od, templates_dir = templates_dir(),
                        user_templates_dir = NULL,
                        fields_dir = fields_templates_dir(),
                        logdir = ld)
  expect_equal(r$status, "unsupported")
  rec <- read_runs(ld)
  expect_equal(nrow(rec), 1L)
  expect_identical(rec$status, "unsupported")
  expect_identical(rec$kind, "statement")
  expect_true(nrow(unsupported_clusters(read_runs(ld))) >= 1)   # this one IS a gap
})

test_that("convert_statement still writes its own record when called directly", {
  # the Admin bulk-convert and the CLI call convert_statement, not the front door.
  fx <- fixture("samples/raw/bnz/bnz_transaction_export_01.csv")
  skip_if_not(file.exists(fx))
  od <- tempfile("so_"); ld <- tempfile("sl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  res <- convert_statement(fx, outdir = od, templates_dir = templates_dir(), logdir = ld)
  expect_true(file.exists(file.path(ld, "runs", paste0(res$run_id, ".json"))))
})

test_that("convert_form on the sample PDF extracts fields (skips if absent)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile(); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  res <- convert_form(pdf, fields_dir = fields_templates_dir(),
                      outdir = od, formats = c("csv"))
  # This guide document prints three of its labels twice with different values,
  # so the honest outcome is needs_review with the conflicts named.
  expect_equal(res$status, "needs_review")
  expect_gt(res$n_conflicts, 0L)
  expect_equal(res$template_id, "anz_kiwisaver_fields")
  expect_true(res$n_fields >= 4)
  expect_true(any(file.exists(res$outputs)))
})

# --- A FORM HAS NO RECONCILIATION, SO IT MUST POLICE ITSELF -------------------
# Found by throwing generated PDFs at the form path: every outcome - nothing
# found, contradictory values, a transaction statement - reported status "ok".
# n_fields counted the fields the template DECLARES, not the ones found, so a
# document where nothing was read still said "4 field(s) extracted".
#
# A labelled value is trusted because it was read. There is no balance to prove
# it, so an empty or contradictory read has to say so itself.

.fields_tpl <- function() {
  d <- tempfile("ft_"); dir.create(d)
  writeLines(c("id: hazard_fields", "bank: Southern Trust", "statement_type: summary",
    "format: pdf", "mode: fields", "version: 1", "fingerprint:",
    '  page_contains_all: ["SOUTHERN TRUST", "Account summary"]', "fields:",
    '  opening_balance: {any_of: ["Opening balance"], value: money}',
    '  closing_balance: {any_of: ["Closing balance"], value: money}',
    "currency: NZD"), file.path(d, "t.yaml"))
  d
}
.hazard_pdf <- function(lines) {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.27, height = 11.69)
  graphics::plot.new(); graphics::par(mar = c(0,0,0,0))
  graphics::plot.window(c(0,100), c(0,100))
  for (i in seq_along(lines))
    graphics::text(3, 97 - i*3.2, lines[i], adj = 0, cex = 0.72, family = "mono")
  invisible(grDevices::dev.off()); f
}
.run <- function(lines) {
  out <- tempfile("fo_"); dir.create(out)
  convert_form(.hazard_pdf(lines), fields_dir = .fields_tpl(), user_fields_dir = NULL,
               outdir = out, formats = "csv")
}

test_that("finding none of the values is not 'ok'", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  # labels present only as column headers, values in rows below - the tool must
  # NOT reach into another line for a value, and must not call that a clean read
  r <- .run(c("SOUTHERN TRUST  Account summary", "",
              "Period   Opening balance   Closing balance",
              "2024        $1,400.00         $1,850.00"))
  expect_identical(r$n_values, 0L)
  expect_identical(r$status, "unsupported")
  expect_true(any(grepl("found none of its", as.character(r$messages), fixed = TRUE)))
})

test_that("the same label twice with different values is never a clean read", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  r <- .run(c("SOUTHERN TRUST  Account summary", "",
              "Fund A", "Opening balance   $1,000.00", "Closing balance   $1,100.00", "",
              "Fund B", "Opening balance   $2,000.00", "Closing balance   $2,300.00"))
  expect_gt(r$n_conflicts, 0L)
  expect_identical(r$status, "needs_review")
  expect_true(any(grepl("more than once with different values",
                        as.character(r$messages), fixed = TRUE)))
})

test_that("a clean form says how many of how many it found", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  r <- .run(c("SOUTHERN TRUST  Account summary", "",
              "Opening balance   $1,250.00", "Closing balance   $1,724.50"))
  expect_identical(r$status, "ok")
  expect_identical(r$n_values, 2L)
  expect_true(any(grepl("2 of 2 value", as.character(r$messages), fixed = TRUE)))
})

test_that("a typographic minus keeps its sign", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  # The R pdf() device emits U+2212 MINUS SIGN for a plain "-", exactly as a real
  # typesetter does. Only the ASCII hyphen used to match, so a deduction printed
  # this way came out POSITIVE.
  r <- .run(c("SOUTHERN TRUST  Account summary", "",
              "Opening balance   -$1,250.00", "Closing balance   $1,724.50"))
  v <- setNames(r$fields$value, r$fields$field)
  expect_true(grepl("1,250.00", v[["opening_balance"]], fixed = TRUE))
  expect_false(identical(v[["opening_balance"]], "$1,250.00"),
               info = "the minus was dropped")
})

# ---------------------------------------------------------------------------
# G2: THE ONLY QUALITY SIGNAL A FORM HAS MUST REACH THE FILE.
# `conflict` is produced by extract_fields, counted by convert_form and DECIDES
# the run's status - and the downloaded CSV selected a fixed seven columns that
# did not include it. Measured on the shipped sample: three values were labels
# printed more than once with different figures; the file showed all three as
# matched TRUE, flagged FALSE, with no conflict column at all.
# ---------------------------------------------------------------------------

test_that("the downloaded form file carries the conflict, the count and the page", {
  input <- list(pages = c(
    "Kowhai Scheme\nOpening balance   $1,000.00",
    "Opening balance   $2,000.00\nClosing balance   $9,000.00"))
  f <- extract_fields(input, list(fields = list(
    opening_balance = list(any_of = "Opening balance", value = "money"),
    closing_balance = list(any_of = "Closing balance", value = "money"))),
    dict = list())

  od <- tempfile("g2_"); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  paths <- write_form_outputs(f, od, "ks", c("csv", "json"))
  csv <- utils::read.csv(paths[grepl("\\.csv$", paths)], stringsAsFactors = FALSE)
  for (col in c("conflict", "n", "other_values", "page", "found_by"))
    expect_true(col %in% names(csv), info = paste(col, "is missing from the file"))
  ob <- csv[csv$field == "opening_balance", ]
  expect_true(ob$conflict)
  expect_identical(ob$n, 2L)
  expect_identical(ob$other_values, "$2,000.00")   # visibly, not just a boolean
  expect_identical(csv$page[csv$field == "closing_balance"], 2L)

  # ...and the JSON, built from the same frame, drops none of it either.
  js <- jsonlite::fromJSON(paths[grepl("\\.json$", paths)])
  expect_true("conflict" %in% names(js$detail))
  expect_true("other_values" %in% names(js$detail))
})

test_that("the shipped sample's three conflicts reach its downloaded file", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile("g2s_"); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  res <- convert_form(pdf, fields_dir = fields_templates_dir(), outdir = od,
                      formats = "csv")
  csv <- utils::read.csv(res$outputs[grepl("\\.csv$", res$outputs)],
                         stringsAsFactors = FALSE)
  expect_equal(sum(csv$conflict %in% TRUE), res$n_conflicts)
  expect_gt(res$n_conflicts, 0L)
  # every conflicting row names the reading it did NOT take
  expect_true(all(nzchar(csv$other_values[csv$conflict %in% TRUE])))
})

# ---------------------------------------------------------------------------
# G1/G8: THE RUN RECORD FOR A FORM OR A REPORT.
# It used to be the failed statement pass's verdict plus two literal zeros.
# ---------------------------------------------------------------------------

test_that("a form run records ITS OWN numbers, its template hash and its files", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile("g8o_"); ld <- tempfile("g8l_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  r <- convert_document(pdf, outdir = od, templates_dir = templates_dir(),
                        user_templates_dir = NULL,
                        fields_dir = fields_templates_dir(), logdir = ld)
  expect_identical(r$kind, "form")
  rec <- r$run_log

  # G1: WHICH TEMPLATE CONTENT ran. Hardcoded NA on both other routes before.
  expect_false(is.na(rec$template_sha256))
  expect_identical(rec$template_sha256, template_sha256(
    load_fields_templates(fields_templates_dir(), NULL)[["anz_kiwisaver_fields"]]))

  # G8 rule 1: a measure that does not apply is NA, never a literal zero - a
  # zero counted every form run as a clean zero-failure conversion in Admin.
  expect_true(is.na(rec$row_count))
  expect_true(is.na(rec$kpi_fail_count))

  # G8 rule 2: the failed statement attempt's fields are CLEARED, not carried
  # over. Measured before this: a form run reported period_start "1 April 2025"
  # scraped off the form, and a layout_hint of page words.
  expect_true(is.na(rec$closest_template))
  expect_true(is.na(rec$detect_detail))
  expect_true(is.na(rec$period_start))
  expect_true(is.na(rec$period_end))
  expect_true(is.na(rec$layout_hint))
  # ...but the layout SIGNATURE stays: it is a hash, it names nobody, and it is
  # what clusters layouts.
  expect_false(is.na(rec$layout_signature))

  # G8 rule 3: the numbers that DECIDED the status have names of their own.
  expect_identical(rec$n_values, as.integer(r$n_values))
  expect_identical(rec$n_conflicts, as.integer(r$n_conflicts))
  expect_identical(rec$required_missing, as.integer(r$required_missing))

  # ...and the record names the files, so a file and a record tie together.
  expect_true(nzchar(rec$output_files))
  for (p in r$outputs)
    expect_true(grepl(basename(p), rec$output_files, fixed = TRUE))

  # and all of that is what actually landed on disk.
  on_disk <- read_runs(ld)
  expect_equal(nrow(on_disk), 1L)
  expect_identical(on_disk$n_conflicts, as.integer(r$n_conflicts))
})

# ---------------------------------------------------------------------------
# L1: A FORM OR REPORT TEMPLATE CAN BE FORCED, ALL THE WAY THROUGH THE FRONT
# DOOR. convert_form() and convert_tables() have always taken a template_id and
# nothing ever passed one, while convert_document's only override turned into
# kind "statement" outright - so forcing a form template produced an unsupported
# STATEMENT.
# ---------------------------------------------------------------------------

test_that("a form template forced at the front door reads the document as a form", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile("l1o_"); ld <- tempfile("l1l_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  r <- convert_document(pdf, outdir = od, templates_dir = templates_dir(),
                        user_templates_dir = NULL,
                        fields_dir = fields_templates_dir(), logdir = ld,
                        template_id = "anz_kiwisaver_fields")
  expect_identical(r$kind, "form")
  expect_true(r$status %in% c("ok", "needs_review"))
  expect_identical(r$template_id, "anz_kiwisaver_fields")
  expect_true(isTRUE(r$forced_template))
  # the OLD argument name is the same argument, so nothing that calls it breaks
  r2 <- convert_document(pdf, outdir = od, templates_dir = templates_dir(),
                         user_templates_dir = NULL,
                         fields_dir = fields_templates_dir(), logdir = ld,
                         force_template = "anz_kiwisaver_fields")
  expect_identical(r2$kind, "form")
})

test_that("an id that is in no library is SAID, and the document is still read", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile("l1u_"); ld <- tempfile("l1ul_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  r <- convert_document(pdf, outdir = od, templates_dir = templates_dir(),
                        user_templates_dir = NULL,
                        fields_dir = fields_templates_dir(), logdir = ld,
                        template_id = "no_such_template_anywhere")
  expect_identical(r$kind, "form")            # read the usual way
  expect_true(any(grepl("not in the library", as.character(r$messages),
                        fixed = TRUE)))       # and never silently
})

test_that("convert_form reads with the template it was given, and refuses one it has not got", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  d <- tempfile("tie_"); dir.create(d)
  for (id in c("aa_trust", "bb_trust"))
    writeLines(c(paste0("id: ", id), "bank: Southern Trust",
      "statement_type: summary", "format: pdf", "mode: fields", "version: 1",
      "fingerprint:", '  page_contains_all: ["SOUTHERN TRUST", "Account summary"]',
      "fields:", '  opening_balance: {any_of: ["Opening balance"], value: money}'),
      file.path(d, paste0(id, ".yaml")))
  p <- .hazard_pdf(c("SOUTHERN TRUST  Account summary", "",
                     "Opening balance   $1,250.00"))
  out <- tempfile("tio_"); dir.create(out)
  on.exit(unlink(c(d, out), recursive = TRUE), add = TRUE)

  # L-detect: TWO TEMPLATES THAT BOTH FIT IS NOT AN ANSWER OF "NO". It used to
  # come back unsupported on a document the library reads perfectly, naming
  # neither of them.
  r <- convert_form(p, fields_dir = d, user_fields_dir = NULL, outdir = out,
                    formats = "csv")
  expect_identical(r$status, "needs_review")
  expect_true(isTRUE(r$ambiguous))
  expect_true(any(grepl("equally well", as.character(r$messages), fixed = TRUE)))
  expect_identical(r$fields$value[r$fields$field == "opening_balance"], "$1,250.00")

  # ...and being told which one to use settles it outright.
  r2 <- convert_form(p, fields_dir = d, user_fields_dir = NULL, outdir = out,
                     formats = "csv", template_id = "bb_trust")
  expect_identical(r2$status, "ok")
  expect_identical(r2$template_id, "bb_trust")

  # An id that is not in the library is REFUSED, never quietly answered by a
  # different template.
  r3 <- convert_form(p, fields_dir = d, user_fields_dir = NULL, outdir = out,
                     formats = "csv", template_id = "not_installed")
  expect_identical(r3$status, "unsupported")
  expect_true(any(grepl("not in the library", as.character(r3$messages),
                        fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# L-detect: the form detector had the report detector's three faults.
# ---------------------------------------------------------------------------

test_that("a fingerprint phrase matches whatever case and spacing it was typed in", {
  # Reported: "I put a phrase printed on it, bang smack on front page, still used
  # another template." A phrase typed in title case against a page printed in
  # capitals could never match, and nothing on screen said why.
  mk <- function(ph) list(t = list(id = "t", mode = "fields", format = "pdf",
    fingerprint = list(page_contains_all = ph), fields = list(a = "A")))
  page <- list(pages = "QUARTERLY PORTFOLIO REPORT\nOpening balance $1.00")
  for (ph in c("QUARTERLY PORTFOLIO REPORT", "Quarterly Portfolio Report",
               "Quarterly  Portfolio Report", "quarterly portfolio report"))
    expect_true(detect_form(page, mk(ph))$matched,
                info = paste("phrase typed as:", ph))
})

test_that("a near miss names the closest template and the wording it wanted", {
  ft <- load_fields_templates(fields_templates_dir(), NULL)
  d <- detect_form(list(pages = "ANZ KiwiSaver Scheme and nothing else"), ft)
  expect_false(d$matched)
  # the maintainer's line names the template and the score...
  expect_match(d$detail, "closest")
  expect_match(d$detail, "anz_kiwisaver_fields")
  # ...and the person's line names neither an id nor a fraction.
  expect_false(is.null(d$detail_plain))
  expect_false(grepl("anz_kiwisaver_fields", d$detail_plain, fixed = TRUE))
  # the same return shape the other two detectors have, so one screen can read
  # all three
  for (k in c("template_id", "matched", "score", "candidates", "eligible_ids",
              "tied", "margin", "runner_up", "detail", "detail_plain"))
    expect_true(k %in% names(d), info = k)
  expect_true(is.data.frame(d$candidates))
})

# ---------------------------------------------------------------------------
# K9: A RUN LOG THAT CANNOT BE WRITTEN IS NOT SWALLOWED.
# log_run wrapped the write in safe() and both callers wrapped it again. On a
# full disk or a vanished network path every conversion still succeeded and
# produced a complete workbook with no audit record, and nobody was told.
# ---------------------------------------------------------------------------

test_that("a conversion that could not be recorded says so, and still converts", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile("k9o_")
  # a log folder that can never be created: its parent is a FILE
  blocker <- tempfile("k9f_"); writeLines("not a directory", blocker)
  on.exit(unlink(c(od, blocker), recursive = TRUE), add = TRUE)

  r <- suppressWarnings(convert_document(pdf, outdir = od,
    templates_dir = templates_dir(), user_templates_dir = NULL,
    fields_dir = fields_templates_dir(), logdir = file.path(blocker, "logs")))

  expect_identical(r$kind, "form")            # the work still happened
  expect_true(length(r$outputs) > 0L)         # she still gets her file
  expect_true(nzchar(r$log_error %||% ""))    # ...and the reason is carried
  expect_true(any(grepl("not recorded in the audit log",
                        as.character(r$messages), fixed = TRUE)))
})

test_that("a conversion that WAS recorded says nothing about the log and names the file", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile("k9ok_"); ld <- tempfile("k9okl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  r <- convert_document(pdf, outdir = od, templates_dir = templates_dir(),
                        user_templates_dir = NULL,
                        fields_dir = fields_templates_dir(), logdir = ld)
  expect_null(r$log_error)
  expect_true(file.exists(r$log_path))
  expect_false(any(grepl("audit log", as.character(r$messages), fixed = TRUE)))
  # exactly ONE record - the retry that finds the reason must never write a
  # second copy of a run.
  expect_equal(length(list.files(file.path(ld, "runs"))), 1L)
})

# ---------------------------------------------------------------------------
# G9: the PII gate guarded the fingerprint only, and a label is captured off the
# page exactly the same way and saved into the same file.
# ---------------------------------------------------------------------------

test_that("a form template whose LABEL names a person is refused, like a fingerprint", {
  base <- list(id = "pii_label", mode = "fields", format = "pdf",
               fingerprint = list(page_contains_all = c("Kowhai Bank KiwiSaver",
                                                        "Annual member statement")))
  bad <- base
  bad$fields <- list(holder = list(any_of = "Prepared for Mr John Smith",
                                   value = "text"))
  probs <- validate_fields_template(bad)
  expect_true(any(grepl("PERSON", probs)))
  # ...through save_fields_template too, which is where the file gets written and
  # then copied off the box.
  d <- tempfile("pii_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(save_fields_template(bad, d), "PERSON")

  # a printed label is untouched
  ok <- base; ok$fields <- list(opening_balance = "Opening balance")
  expect_length(validate_fields_template(ok), 0)
  expect_true(file.exists(save_fields_template(ok, d)))
})

test_that("an answer she can SEE beats a template selection she cannot", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile("j1o_"); ld <- tempfile("j1l_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  # Browser-proven: the "it picked the wrong bank?" panel keeps its selection when
  # it is hidden, so ticking "Something else" left a BANK template chosen
  # underneath - and it won, in silence, turning the document into an unsupported
  # statement on a control she could no longer see.
  r <- convert_document(pdf, outdir = od, templates_dir = templates_dir(),
                        user_templates_dir = NULL,
                        fields_dir = fields_templates_dir(), logdir = ld,
                        kind = "other", force_template = "anz_everyday_pdf")
  expect_identical(r$kind, "form")
  expect_true(any(grepl("different kind of document", as.character(r$messages),
                        fixed = TRUE)))
})
