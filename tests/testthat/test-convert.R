# test-convert.R -- the orchestrator's own contracts (R/convert.R):
#   * WHICH template a tie is actually read with, and what the workbook is stamped
#     with as a result;
#   * a file the reader could not read is reported as unreadable, never as a
#     layout nobody has taught the tool yet;
#   * the front door really does never throw, for any argument at all.

# .tie_dir(...) -- three templates over the shipped ANZ export, arranged so the
# GLOBAL top scorer is NOT one of the tied candidates:
#   aaa_toplayout_csv  wants 3 phrases, finds 2, min_score 3 -> highest score,
#                      but INELIGIBLE (it never met its own threshold)
#   b_/c_tiedlayout_csv want 2 phrases, find 2, min_score 2 -> both eligible, tied
# detect_statement then returns template_id = aaa_... (the closest, for reporting)
# and tied = c(b_..., c_...). Those are different sets, and reading with the first
# one stamped a bank on the workbook that no screen ever named.
.tie_dir <- function() {
  d <- tempfile("convtie_"); dir.create(d)
  writeLines(c(
    "id: aaa_toplayout_csv", "bank: TOPBANK", "statement_type: everyday",
    "format: delimited", "version: 1", "min_score: 3", "fingerprint:",
    "  header_contains_all: [Type, Details, NotOnThisPage]",
    'delimiter: ","', "columns:",
    '  date: {source: Date, format: "%d/%m/%Y"}',
    "  amount: {source: Amount}", "  description: {source: Details}",
    "amount_sign: signed", "currency: NZD"), file.path(d, "a.yaml"))
  for (v in c("b", "c")) writeLines(c(
    sprintf("id: %s_tiedlayout_csv", v), sprintf("bank: %sBANK", toupper(v)),
    "statement_type: everyday", "format: delimited", "version: 1", "min_score: 2",
    "fingerprint:", "  header_contains_all: [Particulars, Reference]",
    'delimiter: ","', "columns:",
    '  date: {source: Date, format: "%d/%m/%Y"}',
    "  amount: {source: Amount}", "  description: {source: Details}",
    "amount_sign: signed", "currency: NZD"), file.path(d, paste0(v, ".yaml")))
  d
}

# THE CARDINAL ONE. An ambiguous match used to convert with det$template_id -- the
# top scorer over ALL templates -- while the screen listed det$tied, the templates
# that met their own min_score. When those differ, the workbook, the CSV and the
# JSON were stamped with a bank the reviewer was never shown, and the balance
# reconciled cleanly, so nothing looked wrong. A wrong figure that LOOKS right.
test_that("a tie is read with one of the templates the screen names", {
  d <- .tie_dir(); out <- tempfile("convtieout_"); dir.create(out)
  src <- fixture("samples/raw/anz/anz_transaction_export_01.csv")

  det <- detect_statement(read_input(src), load_templates(d, strict = FALSE))
  expect_false(det$matched)
  expect_setequal(det$tied, c("b_tiedlayout_csv", "c_tiedlayout_csv"))
  expect_false(det$template_id %in% det$tied)      # the shape that made this possible

  res <- convert_statement(src, outdir = out, templates_dir = d, logdir = out)
  expect_identical(res$status, "needs_review")
  expect_true(res$template_id %in% res$detect$tied)
  # ...and the bank stamped on the outputs is that template's bank, not the
  # ineligible top scorer's
  expect_identical(res$header$bank, "BBANK")
  expect_false(identical(res$header$bank, "TOPBANK"))
  # deterministic: same input + same templates -> same template chosen
  res2 <- convert_statement(src, outdir = out, templates_dir = d, logdir = out)
  expect_identical(res2$template_id, res$template_id)
})

test_that("the tie message names the template that was USED, then the alternatives", {
  d <- .tie_dir(); out <- tempfile("convtiemsg_"); dir.create(out)
  res <- convert_statement(fixture("samples/raw/anz/anz_transaction_export_01.csv"),
                           outdir = out, templates_dir = d, logdir = out)
  m <- paste(as.character(res$messages), collapse = " ")
  # IN WORDS. This message renders on the verdict card, beside "Read as: BBANK
  # everyday statement" -- five raw ids used to print there, on the one card the
  # charter's interface rule is strictest about.
  expect_match(m, "it was read as BBANK everyday statement", fixed = TRUE)
  expect_match(m, "CBANK everyday statement", fixed = TRUE)  # the alternative is still named
  expect_false(grepl("_csv", m, fixed = TRUE))               # and no template id at all
  # the diagnostic (the maintainer's evidence trail) still carries the ids
  cats <- as.character(res$diagnostics$category)
  expect_true("ambiguous_template" %in% cats)
  expect_match(res$diagnostics$detail[cats == "ambiguous_template"][1],
               "read with b_tiedlayout_csv", fixed = TRUE)
})

# Two set-ups of the SAME layout is the common tie, and they share a display name
# -- so "worth a check against the alternative: BBANK everyday statement" would
# repeat what she was just told, against a template she cannot tell apart.
test_that("a tie between duplicates of one layout does not name a twin", {
  d <- tempfile("convdup_"); dir.create(d); out <- tempfile("convdupout_"); dir.create(out)
  for (v in c("b", "c")) writeLines(c(
    sprintf("id: %s_duplayout_csv", v), "bank: BBANK", "statement_type: everyday",
    "format: delimited", "version: 1", "min_score: 2",
    "fingerprint:", "  header_contains_all: [Particulars, Reference]",
    'delimiter: ","', "columns:",
    '  date: {source: Date, format: "%d/%m/%Y"}',
    "  amount: {source: Amount}", "  description: {source: Details}",
    "amount_sign: signed", "currency: NZD"), file.path(d, paste0(v, ".yaml")))
  res <- convert_statement(fixture("samples/raw/anz/anz_transaction_export_01.csv"),
                           outdir = out, templates_dir = d, logdir = out)
  m <- paste(as.character(res$messages), collapse = " ")
  expect_match(m, "the same layout is set up more than once", fixed = TRUE)
  expect_false(grepl("worth a check against the alternative", m, fixed = TRUE))
  expect_false(grepl("_csv", m, fixed = TRUE))
})

test_that("no engine code reaches the verdict card on a clean or an empty read", {
  out <- tempfile("convname_"); dir.create(out)
  ok <- convert_statement(fixture("samples/raw/anz/anz_transaction_export_01.csv"),
                          outdir = out, templates_dir = templates_dir(), logdir = out)
  expect_match(paste(ok$messages, collapse = " "), "matched ANZ everyday statement", fixed = TRUE)
  expect_false(grepl("anz_everyday_csv", paste(ok$messages, collapse = " "), fixed = TRUE))
  # ...and the template that matched the wording and read nothing says so by name
  hdr <- tempfile("asbempty_", fileext = ".csv")
  writeLines(c("Created date / time : 24 December 2014 / 19:38:14",
               "From date 20141220", "To date 20141224",
               "Date,Unique Id,Tran Type,Cheque Number,Payee,Memo,Amount"), hdr)
  e <- convert_statement(hdr, outdir = out, templates_dir = templates_dir(), logdir = out)
  expect_identical(e$status, "unsupported")
  expect_match(paste(e$messages, collapse = " "),
               "ASB everyday statement matches the wording", fixed = TRUE)
  expect_false(grepl("asb_everyday_csv", paste(e$messages, collapse = " "), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# UNREADABLE IS NOT UNSUPPORTED. Both used to come out as "No template for this
# statement yet", which sends somebody to spend two minutes teaching the tool a
# file that has nothing in it to teach.
test_that("a file the reader cannot read is reported as unreadable, not as a new layout", {
  out <- tempfile("convbad_"); dir.create(out)
  junk <- tempfile("junk_", fileext = ".pdf")
  writeBin(as.raw(rep(c(0x25, 0x7d, 0x00, 0xff), 1000)), junk)   # 4000 bytes of noise
  # PROSE THAT HOLDS A SEPARATOR. The old fixture was one sentence with no
  # separator in it at all, so it passed over a guard that only asked whether a
  # separator character appeared ANYWHERE -- and every one of these came back as
  # "we don't have a template for this layout yet", the answer this guard exists
  # to prevent. Real notes have commas in them; the fixture has to as well.
  prose <- vapply(list(
    "This is just a note to myself.",
    "Use the transaction export, not the PDF.",
    c("Hi Beth,", "", "Please find attached the statement for June.",
      "Let me know if you need anything else.", "", "Regards,", "Tim"),
    c("Meeting minutes - 12 June", "Present: Tim, Beth, Sam",
      "1. Budget discussed; no decision taken", "2. Next meeting Friday"),
    "Nothing here; nothing at all.",
    "See the shared drive | folder for the export."
  ), function(txt) { p <- tempfile("prose_", fileext = ".txt"); writeLines(txt, p); p },
  character(1))
  empty <- tempfile("empty_", fileext = ".csv"); file.create(empty)

  for (p in c(junk, prose, empty)) {
    r <- convert_statement(p, outdir = out, templates_dir = templates_dir(), logdir = out)
    expect_identical(r$status, "failed", info = basename(p))
    expect_true("unreadable" %in% as.character(r$diagnostics$category), info = basename(p))
    expect_false("unknown_format" %in% as.character(r$diagnostics$category), info = basename(p))
    # and the cure does not send the reader off to build a template
    expect_false(grepl("matches a template", paste(r$messages, collapse = " "), fixed = TRUE),
                 info = basename(p))
  }
})

test_that("a statement that IS readable still converts exactly as before", {
  out <- tempfile("convgood_"); dir.create(out)
  r <- convert_statement(fixture("samples/raw/kiwibank/kiwibank_transaction_01.csv"),
                         outdir = out, templates_dir = templates_dir(), logdir = out)
  expect_identical(r$status, "ok")
  expect_equal(nrow(r$feed_rows), 4L)
  # a header-only export is "matched the wording, read nothing" -- NOT unreadable:
  # it separates into fields, so there is a table there, it is simply empty.
  hdr <- tempfile("hdronly_", fileext = ".csv")
  writeLines("Type,Details,Particulars,Code,Reference,Amount,Date,ForeignCurrencyAmount,ConversionCharge", hdr)
  h <- convert_statement(hdr, outdir = out, templates_dir = templates_dir(), logdir = out)
  expect_identical(h$status, "unsupported")
  expect_false("unreadable" %in% as.character(h$diagnostics$category))
})

# "Holds no table" is judged against the separators the INSTALLED TEMPLATES
# declare, so teaching the tool a bank that separates some other way stays a YAML
# edit (charter: a new bank is a template, never new code) and can never turn that
# bank's export into "could not be read".
test_that("what counts as separated is read off the templates, not hard-coded", {
  spaced <- tempfile("spaced_", fileext = ".txt")
  writeLines(c("Date Amount Details", "01/04/2025 -4.50 COFFEE"), spaced)
  input <- read_input(spaced)
  shipped <- load_template_set(templates_dir(), NULL)
  expect_false(is.null(.unreadable_reason(input, shipped)))       # nothing separates it
  with_spacer <- c(list(spacer = list(id = "spacer", delimiter = " ")), shipped)
  expect_null(.unreadable_reason(input, with_spacer))             # ...until a template says so
  # the four fallbacks every reader has are always accepted
  for (sep in c(",", "\t", ";", "|")) {
    p <- tempfile("sep_", fileext = ".csv")
    writeLines(paste("Date", "Amount", "Details", sep = sep), p)
    expect_null(.unreadable_reason(read_input(p), shipped), info = sep)
  }
})

# What makes a file a table is a REPEATED SHAPE, not a separator character. These
# are the files a character test cannot tell apart from a statement.
test_that("a table is a repeated shape, read the way the reader will read it", {
  shipped <- load_template_set(templates_dir(), NULL)
  wr <- function(txt) { p <- tempfile("shape_", fileext = ".csv"); writeLines(txt, p); p }
  # the 6-line preamble is not a table; the header and its rows below it are
  expect_null(.unreadable_reason(
    read_input(fixture("samples/raw/asb/asb_transaction_export_01.csv")), shipped))
  # the same export with no transactions in the period: nothing repeats, but a
  # template names the header line it is looking straight at
  expect_null(.unreadable_reason(read_input(wr(c(
    "Created date / time : 24 December 2014 / 19:38:14",
    "Bank 12; Branch 3456; Account 7890123-45-00",
    "From date 20141220",
    "Date,Unique Id,Tran Type,Cheque Number,Payee,Memo,Amount"))), shipped))
  # A QUOTED comma is part of a payee, not a field boundary. Counted blind, this
  # file reads 3 then 4 then 3 fields -- no shape at all -- and a real ASB export
  # ("Acme, Inc.") would have been called unreadable.
  expect_null(.unreadable_reason(read_input(wr(c(
    "Date,Payee,Amount",
    '2014/12/23,"Acme, Inc.",5678.90',
    "2014/12/24,Bob Ltd,-3.80"))), shipped))
})

# ---------------------------------------------------------------------------
# THE RUN ID IS THE HANDLE THE INCIDENT PROCEDURE IS BUILT ON.
# docs/operational/investigating-a-wrong-conversion.md sends the maintainer to
# logs/runs/<run_id>.json for the re-run. The CLI printed status, template,
# trust, checks, outputs and message -- and no run id, so there was no way to
# know which of the hundreds of records it had just written.
test_that("the CLI prints a run id that names the record it just wrote", {
  root <- engine_root()
  out <- tempfile("cliout_"); dir.create(out)
  rc <- file.path(R.home("bin"), "Rscript")
  txt <- suppressWarnings(system2(rc, c(shQuote(file.path(root, "run.R")),
    shQuote(file.path(root, "samples/raw/anz/anz_transaction_export_01.csv")), '""',
    shQuote(out)), stdout = TRUE, stderr = FALSE))
  skip_if(!length(txt), "Rscript produced no output")
  line <- grep("^run id:", txt, value = TRUE)
  expect_length(line, 1L)
  id <- trimws(sub("^run id:", "", line))
  expect_true(nzchar(id))
  # the whole point: that id opens the record the procedure asks for
  rec <- file.path(root, "logs", "runs", paste0(id, ".json"))
  expect_true(file.exists(rec))
  # a test must not leave the install's own run history behind. The id names
  # every record this run wrote, which is exactly what makes that possible.
  unlink(c(rec, file.path(root, "logs", "metadata", paste0(id, ".json"))))
})

# ---------------------------------------------------------------------------
# "Never throws at the front door" is stated as a FACT in docs/design.md and
# docs/overview.md. It was not one: the tryCatch opened below the preamble, so
# basename(path) sat outside it and convert_document(1L) threw before any guard
# ran. Unreachable from Shiny (a datapath is always character) -- which is exactly
# why it needed a test rather than a reader's trust.
test_that("the front door never throws, whatever it is handed", {
  out <- tempfile("convhostile_"); dir.create(out)
  adir <- tempfile("adir_"); dir.create(adir)
  zero <- tempfile("zero_", fileext = ".csv"); file.create(zero)
  hostile <- list(1L, 3.14, NULL, NA, NA_character_, c("a.csv", "b.csv"),
                  list("a.csv"), adir, zero,
                  file.path(tempdir(), "nope.csv"), "", TRUE)
  for (i in seq_along(hostile)) {
    r <- convert_document(hostile[[i]], outdir = out, templates_dir = templates_dir(),
                          fields_dir = fields_templates_dir(),
                          logdir = out)
    expect_identical(r$status, "failed", info = paste("input", i))
    expect_true(nzchar(paste(r$messages, collapse = " ")), info = paste("input", i))
  }
})

# ---------------------------------------------------------------------------
# N83, THE OTHER HALF: THE PROSE GUARD WAS DEFEATED BY A BLANK LINE.
#
# .delimited_tabular decides on ADJACENCY -- two records that split the same way
# NEXT TO each other. Its own comment says an email's matching lines are
# "SCATTERED through prose" -- and .unreadable_reason stripped the blank lines
# before calling it, which is exactly what scattered them. So
#     Hi Beth,  /  <blank>  /  Use the transaction export, not the PDF.
# arrived as two adjacent 2-field records, i.e. a table, and a note to self came
# back as "we don't have a template for this layout yet" plus an offer to build
# one -- the answer this whole guard exists to prevent.
#
# The fix is at the caller: the shape test now sees the file's real line
# structure. The emptiness test and the header_regex scan keep the stripped view,
# because neither of them cares where a blank line was.
# ---------------------------------------------------------------------------
test_that("blank lines still scatter prose - a note to self is not a table", {
  shipped <- load_template_set(templates_dir(), NULL)
  wr <- function(txt) { p <- tempfile("blank_", fileext = ".txt"); writeLines(txt, p); p }
  # the note that was read as a statement layout
  note <- c("Hi Beth,", "", "Use the transaction export, not the PDF.", "",
            "Thanks", "Michael")
  expect_false(is.null(.unreadable_reason(read_input(wr(note)), shipped)))
  # a sign-off separated from a greeting by a paragraph break, same shape
  expect_false(is.null(.unreadable_reason(read_input(wr(
    c("Morning Beth,", "", "Statement attached.", "", "Regards,", "Tim"))), shipped)))
  # ...and end to end it is UNREADABLE, not a layout nobody has built yet
  r <- convert_statement(wr(note), outdir = tempfile("blankout_"),
                         templates_dir = templates_dir(), logdir = tempfile("blanklog_"))
  expect_identical(r$status, "failed")
  expect_true("unreadable" %in% as.character(r$diagnostics$category))
  expect_false("unknown_format" %in% as.character(r$diagnostics$category))
})

test_that("the case the earlier round protected is still a table", {
  # THE LINE THIS FIX MUST NOT CROSS. Two adjacent records that split the same way
  # ARE a table, however narrow -- test-forms.R depends on this one coming back
  # `unsupported` (a layout with no template), never `failed`.
  shipped <- load_template_set(templates_dir(), NULL)
  wr <- function(txt) { p <- tempfile("tab_", fileext = ".csv"); writeLines(txt, p); p }
  expect_null(.unreadable_reason(read_input(wr(c("a,b", "1,2"))), shipped))
  # a real export with a blank line in the middle of its rows is still a table:
  # the run either side of the gap is what proves it
  expect_null(.unreadable_reason(read_input(wr(
    c("Date,Payee,Amount", "2026-01-01,A,1.00", "", "2026-01-02,B,2.00",
      "2026-01-03,C,3.00"))), shipped))
  # a header-only export with a trailing newline keeps its one-record fallback
  expect_null(.unreadable_reason(read_input(wr(
    c("Date,Unique Id,Tran Type,Payee,Memo,Amount", ""))), shipped))
  # the shipped preamble export is unaffected
  expect_null(.unreadable_reason(
    read_input(fixture("samples/raw/asb/asb_transaction_export_01.csv")), shipped))
})

test_that("every delimited sample in the corpus is still read as a table", {
  # The blast radius, measured rather than argued: whatever the shape test now
  # sees, no real bank export may become "could not be read".
  shipped <- load_template_set(templates_dir(), NULL)
  files <- list.files(file.path(engine_root(), "samples", "raw"),
                      pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
  skip_if(length(files) == 0)
  bad <- Filter(function(f) !is.null(.unreadable_reason(read_input(f), shipped)), files)
  expect_identical(basename(bad), character(0),
                   info = paste("newly unreadable:", paste(basename(bad), collapse = ", ")))
})

# ---------------------------------------------------------------------------
# N1xx: THE SAVE CONFIRMATION SPOKE THE CARD'S LANGUAGE IN IDS.
# The verdict card two inches to the left says "Read as: SAMPLE BANK statement";
# the save confirmation said, twice, sample_bank_statement_pdf. An id is a
# maintainer's handle -- an accountant cannot check a figure against it, and the
# charter's interface rule forbids putting an engine code in front of her.
# ---------------------------------------------------------------------------
test_that("recognition_summary names templates in words when it can", {
  tset <- list(
    sample_bank_statement_pdf = list(id = "sample_bank_statement_pdf", bank = "SAMPLE BANK"),
    sample_bank_twin_pdf      = list(id = "sample_bank_twin_pdf", bank = "SAMPLE BANK"),
    anz_everyday_pdf          = list(id = "anz_everyday_pdf", bank = "ANZ",
                                     statement_type = "everyday"))
  sid <- "sample_bank_statement_pdf"
  # it is the SAME wording the card uses, which is the whole point
  expect_identical(template_display_name(tset[[sid]]), "SAMPLE BANK statement")

  ok <- recognition_summary(list(matched = TRUE, template_id = sid), sid, tset)
  expect_match(ok$detail, "SAMPLE BANK statement", fixed = TRUE)
  expect_false(grepl(sid, ok$detail, fixed = TRUE))

  lost <- recognition_summary(list(matched = TRUE, template_id = "anz_everyday_pdf"), sid, tset)
  expect_match(lost$headline, "ANZ everyday statement", fixed = TRUE)
  expect_false(grepl("anz_everyday_pdf", lost$headline, fixed = TRUE))

  tie <- recognition_summary(list(matched = FALSE, tied = c(sid, "sample_bank_twin_pdf")),
                             sid, tset)
  expect_false(grepl("sample_bank_twin_pdf", paste(tie$headline, tie$detail), fixed = TRUE))
  expect_match(tie$headline, "SAMPLE BANK statement", fixed = TRUE)
})

test_that("recognition_summary falls back to the id rather than naming nothing", {
  # No template set to look in -> there is no name to be had, and a confirmation
  # that named NOTHING would be worse than one naming a handle. This is also what
  # keeps every existing caller working unchanged.
  r <- recognition_summary(list(matched = TRUE, template_id = "beth_bank_pdf"), "beth_bank_pdf")
  expect_match(r$detail, "beth_bank_pdf", fixed = TRUE)
  # a set that simply does not hold the id behaves the same way
  r2 <- recognition_summary(list(matched = TRUE, template_id = "beth_bank_pdf"),
                            "beth_bank_pdf", list(other = list(id = "other", bank = "X")))
  expect_match(r2$detail, "beth_bank_pdf", fixed = TRUE)
})
