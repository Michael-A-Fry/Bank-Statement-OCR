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
  expect_match(m, "it was read with b_tiedlayout_csv", fixed = TRUE)
  expect_match(m, "c_tiedlayout_csv", fixed = TRUE)          # the alternative is still named
  # the diagnostic says which one produced these figures too
  cats <- as.character(res$diagnostics$category)
  expect_true("ambiguous_template" %in% cats)
  expect_match(res$diagnostics$detail[cats == "ambiguous_template"][1],
               "read with b_tiedlayout_csv", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# UNREADABLE IS NOT UNSUPPORTED. Both used to come out as "No template for this
# statement yet", which sends somebody to spend two minutes teaching the tool a
# file that has nothing in it to teach.
test_that("a file the reader cannot read is reported as unreadable, not as a new layout", {
  out <- tempfile("convbad_"); dir.create(out)
  junk <- tempfile("junk_", fileext = ".pdf")
  writeBin(as.raw(rep(c(0x25, 0x7d, 0x00, 0xff), 1000)), junk)   # 4000 bytes of noise
  prose <- tempfile("prose_", fileext = ".txt")
  writeLines("This is just a note to myself.", prose)
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
                          fields_dir = file.path(engine_root(), "fields_templates"),
                          logdir = out)
    expect_identical(r$status, "failed", info = paste("input", i))
    expect_true(nzchar(paste(r$messages, collapse = " ")), info = paste("input", i))
  }
})
