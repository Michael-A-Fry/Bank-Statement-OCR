# test-convert.R -- the front door's own contracts (R/convert.R):
#   * bank first: the bank is pre-filled from the statement, a confident
#     disagreement with the person's pick blocks learning and says so;
#   * the reader's outcome decides the status (proven -> ok, check ->
#     needs_review, unread -> unsupported), and every output is stamped with what
#     produced it -- never with an account number;
#   * learning only from what the arithmetic proved; a person's fix that proves
#     teaches at once, one that does not applies to the file only;
#   * a file the reader could not read is reported as unreadable;
#   * the front door really does never throw, for any argument at all.

test_that("a statement the arithmetic proves converts ok, stamped, and is learned", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  acct <- nz_test_account()
  r <- cv(proven_csv(acct))
  expect_identical(r$status, "ok")
  expect_identical(r$outcome, "proven")
  expect_identical(r$feed_basis, "proven")
  expect_identical(r$bank$bank, "bnz")                  # pre-filled from the statement
  expect_identical(r$bank$confidence, "high")
  expect_equal(nrow(r$feed_rows), 5L)
  # the stamp, on the result, the run log and the JSON
  expect_identical(r$run_log$engine_version, engine_version())
  expect_identical(r$run_log$outcome, "proven")
  expect_identical(r$run_log$proof_kind, "chain")
  expect_identical(r$run_log$institution, "bnz")
  expect_identical(r$run_log$bank_code, "02")
  expect_identical(r$run_log$layouts_state, "empty")    # nothing learned before it
  expect_identical(r$run_log$learn_action, "created")
  js <- jsonlite::fromJSON(r$outputs[["json"]])
  expect_identical(js$build$outcome, "proven")
  expect_identical(js$build$institution, "bnz")
  # a provisional layout was started, and the next conversion is stamped with it
  expect_length(layouts_load(file.path(d, "layouts"), "bnz"), 1L)
  r2 <- cv(proven_csv(acct))
  expect_false(identical(r2$run_log$layouts_state, "empty"))
  expect_identical(r2$run_log$learn_action, "none")      # the same statement counts once
  # never the account number: not in the run log, the tracking, or a layout
  body <- strsplit(acct, "-")[[1]][3]
  for (sub in c("logs", "tracking", "layouts"))
    expect_false(grepl(body, every_file_text(file.path(d, sub)), fixed = TRUE), info = sub)
})

test_that("a zero printed in money out is written 0.00, never -0.00", {
  cv <- convert_sandbox()
  r <- cv(proven_csv())
  csv <- utils::read.csv(r$outputs[["csv"]], colClasses = "character")
  fee <- csv[csv$description == "Account fee", ]
  expect_false(grepl("^-", fee$amount))
  expect_equal(as.numeric(fee$amount), 0)
  js <- paste(readLines(r$outputs[["json"]]), collapse = "\n")
  expect_false(grepl("-0[,}\\s]|-0[.]0", js, perl = TRUE))
})

test_that("a statement nothing proves goes to a person, with the reader's reason", {
  cv <- convert_sandbox()
  r <- cv(unproven_csv(), bank = "ANZ")
  expect_identical(r$status, "needs_review")
  expect_identical(r$outcome, "check")
  expect_identical(r$feed_basis, "none")
  expect_true(nzchar(r$reason))
  expect_match(paste(r$messages, collapse = " "), sub("[.]$", "", r$reason), fixed = TRUE)
  expect_true("not_proven" %in% r$diagnostics$category)
  expect_identical(r$run_log$learn_action, "none")      # nothing proven, nothing learned
  expect_length(r$fix_held, 0L)
})

test_that("a person confirming a reading converts it for that file only", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  r <- cv(unproven_csv(), bank = "ANZ", confirm = TRUE)
  expect_identical(r$status, "ok")
  expect_identical(r$feed_basis, "person")
  expect_identical(r$run_log$proof_kind, "person")
  expect_identical(r$run_log$learn_action, "none")      # never learned on one person's word
  expect_length(layouts_load(file.path(d, "layouts"), "anz"), 0L)
  # ...it is held for an admin instead
  held <- fixes_pending(file.path(d, "layouts"))
  expect_equal(nrow(held), 1L)
  expect_identical(held$kind, "confirm")
  ev <- jsonlite::fromJSON(readLines(list.files(file.path(d, "tracking"), full.names = TRUE)[1])[1])
  expect_identical(ev$event, "confirm")
  expect_identical(ev$proof_kind, "person")
  # an admin accepting it makes it a proven layout of the bank
  ok <- fix_accept(held$id, file.path(d, "layouts"), by = "admin")
  expect_true(ok$ok)
  expect_length(layouts_load(file.path(d, "layouts"), "anz"), 1L)
  expect_equal(nrow(fixes_pending(file.path(d, "layouts"))), 0L)
})

test_that("confirming cannot make a statement whose balance does not add up ok", {
  cv <- convert_sandbox()
  bad <- write_statement_csv(c("Date,Details,Amount,Balance",
    "14/04/2025,Salary,2500.00,3500.00", "15/04/2025,Rent,-1200.00,2300.00",
    "17/04/2025,Coffee,-4.50,2200.00", "18/04/2025,Bread,-3.00,2197.00"))
  r <- cv(bad, bank = "ANZ", confirm = TRUE)
  expect_false(identical(r$status, "ok"))
  expect_identical(r$feed_basis, "none")
  expect_match(r$messages[1], "cannot be confirmed", fixed = TRUE)
  expect_length(r$fix_held, 0L)
})

test_that("a picked bank the statement contradicts blocks learning, and says so", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  p <- proven_csv()
  r <- cv(p, bank = "ANZ")
  expect_identical(r$status, "ok")                      # the figures are still proven
  expect_identical(r$run_log$learn_action, "none")
  expect_true(r$bank$block_learning)
  expect_true("bank_check" %in% r$diagnostics$category)
  expect_match(paste(r$messages, collapse = " "), "Nothing will be learned", fixed = TRUE)
  expect_length(layouts_load(file.path(d, "layouts")), 0L)
  # once the person confirms the pick, it is learned under the bank they chose
  r2 <- cv(p, bank = "ANZ", bank_confirmed = TRUE)
  expect_identical(r2$run_log$learn_action, "created")
  expect_length(layouts_load(file.path(d, "layouts"), "anz"), 1L)
})

test_that("a person's roles that then prove are learned straight away", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  p <- ambiguous_csv()
  r0 <- cv(p, bank = "Rimu Bank")
  expect_identical(r0$outcome, "check")                 # two readings both add up
  expect_setequal(r0$columns$field[r0$columns$kind == "money"], c("debit", "credit", "balance"))
  r1 <- cv(p, bank = "Rimu Bank", overrides = list(roles = c(debit = "debit", credit = "credit")))
  expect_identical(r1$status, "ok")
  expect_identical(r1$outcome, "proven")
  expect_identical(r1$run_log$learn_action, "corrected")
  ly <- layouts_load(file.path(d, "layouts"), "Rimu Bank")
  expect_length(ly, 1L)
  expect_identical(ly[[1]]$layout$status, "proven")
  expect_identical(ly[[1]]$layout$origin, "corrected")
  # ...so the next statement of that design converts on its own
  r2 <- cv(ambiguous_csv(), bank = "Rimu Bank")
  expect_identical(r2$status, "ok")
  expect_false(is.na(r2$run_log$layout))
})

test_that("a fix that breaks the arithmetic is neither proven, learned nor held", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  r <- cv(proven_csv(), overrides = list(roles = c(debit = "credit", credit = "debit")))
  expect_false(identical(r$status, "ok"))
  expect_identical(r$feed_basis, "none")
  expect_identical(r$run_log$learn_action, "none")
  expect_length(r$fix_held, 0L)
  expect_length(layouts_load(file.path(d, "layouts")), 0L)
  expect_equal(nrow(fixes_pending(file.path(d, "layouts"))), 0L)
})

test_that("a fix that cannot be read is refused in words and changes nothing", {
  cv <- convert_sandbox()
  r <- cv(proven_csv(), overrides = list(roles = c(description = "debit")))
  expect_identical(r$status, "ok")                      # the statement's own reading stands
  expect_true("fix_not_applied" %in% r$diagnostics$category)
  expect_match(r$messages[1], "The fix was not applied", fixed = TRUE)
  for (bad in list(c(debit = "balance", credit = "balance"), c(debit = "spending"), c("credit")))
    expect_true(!is.null(.override_roles(list(template = list(auto = list(roles = c("debit", "credit", "balance")))), bad)$error))
})

test_that("each statement of a bundle is read and proven on its own", {
  cv <- convert_sandbox()
  r <- cv(fixture("tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf"), bank = "ANZ")
  expect_identical(r$status, "ok")
  expect_equal(r$metadata$split$n_statements, 2L)
  expect_setequal(unique(r$feed_rows$statement_index), 1:2)
  expect_identical(vapply(r$reading, `[[`, "", "outcome"), c("proven", "proven"))
  expect_identical(r$run_log$statements, 2L)
})

test_that("spot checks are off by default and picked deterministically when on", {
  cv <- convert_sandbox()
  expect_false(cv(proven_csv())$spot_check)
  expect_true(.spot_pick(strrep("0", 64), 1))
  expect_false(.spot_pick(strrep("f", 64), 0.5))
  expect_false(.spot_pick(NA_character_, 1))
  d <- tempfile("spot_"); dir.create(d)
  res <- list(stamp = list(layouts_state = "empty", outcome = "proven", proof_kind = "chain",
                           kind = "delimited", institution = "bnz", layout = "bnz_1@2"))
  expect_true(isTRUE(spot_check_record(res, "right", d)))
  line <- jsonlite::fromJSON(readLines(list.files(d, full.names = TRUE)[1])[1])
  expect_identical(line$event, "spot_check")
  expect_identical(line$spot_check, "right")
  expect_identical(line$layout_id, "bnz_1")
})

test_that("the run log keeps the reason without quoting the statement", {
  expect_identical(.log_scrub('The dated line "22 Feb TOTARA 12-3456-7890123-00" on page 1.'),
                   'The dated line "..." on page 1.')
  expect_identical(.log_scrub("account 0123456 moved 743.63"), "account # moved 743.63")
})

# ---- found by the independent review of the engine --------------------------

# A statement whose preamble names ANZ's legal entity: identified with MEDIUM
# confidence, which asks rather than blocks in bank_pick().
anz_named_csv <- function(balance = TRUE) write_statement_csv(c(
  "ANZ Bank New Zealand Limited",
  if (balance) c("Date,Details,Amount,Balance", "13/04/2025,Opening balance,,1000.00",
                 "14/04/2025,Salary,2500.00,3500.00", "15/04/2025,Rent,-1200.00,2300.00",
                 "17/04/2025,Coffee,-4.50,2295.50")
  else c("Date,Details,Amount", "14/05/2025,Salary,2600.00", "15/05/2025,Rent,-1250.00",
         "19/05/2025,Coffee,-5.50", "21/05/2025,Power,-130.00")))

test_that("a statement that names another bank teaches nothing until the pick is kept", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  p <- anz_named_csv()
  r <- cv(p, bank = "Westpac")
  expect_identical(r$bank$confidence, "medium")
  expect_identical(r$status, "ok")                      # proven figures still convert
  expect_identical(r$run_log$learn_action, "none")      # ...but never into Westpac's layouts
  expect_true(r$bank$block_learning)
  expect_length(layouts_load(file.path(d, "layouts")), 0L)
  expect_identical(cv(p, bank = "Westpac", bank_confirmed = TRUE)$run_log$learn_action, "created")
})

test_that("a statement with nothing to add up is not converted on another bank's layout", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  shape <- function() write_statement_csv(c("Date,Details,Amount", "14/04/2025,Salary,2500.00",
                                            "15/04/2025,Rent,-1200.00", "17/04/2025,Coffee,-4.50"))
  cv(shape(), bank = "Westpac", confirm = TRUE)          # a person vouches; an admin accepts
  expect_true(fix_accept(fixes_pending(file.path(d, "layouts"))$id[1], file.path(d, "layouts"))$ok)
  expect_identical(cv(shape(), bank = "Westpac")$outcome, "layout_match")
  r <- cv(anz_named_csv(balance = FALSE), bank = "Westpac")
  expect_identical(r$status, "needs_review")            # Westpac's layout, ANZ's statement
  expect_identical(r$feed_basis, "none")
  expect_match(r$reason, "confirm the bank first", fixed = TRUE)
  r2 <- cv(anz_named_csv(balance = FALSE), bank = "Westpac", bank_confirmed = TRUE)
  expect_identical(r2$outcome, "layout_match")
})

test_that("a confirm sent with a fix that could not be applied is refused", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  r <- cv(unproven_csv(), bank = "ANZ", confirm = TRUE, overrides = list(roles = c(nosuch = "debit")))
  expect_identical(r$status, "needs_review")
  expect_identical(r$feed_basis, "none")
  expect_false(.feed_gate(r)$accept)
  expect_match(r$messages[1], "cannot be confirmed", fixed = TRUE)
  expect_equal(nrow(fixes_pending(file.path(d, "layouts"))), 0L)
})

test_that("a fix for one statement of a bundle leaves the others alone", {
  cv <- convert_sandbox()
  b <- fixture("tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf")
  swap <- list(roles = c(debit = "credit", credit = "debit"))
  r <- cv(b, bank = "ANZ", overrides = c(swap, list(statement = 2L)))
  expect_identical(r$reading[[1]]$outcome, "proven")    # untouched
  expect_null(r$reading[[1]]$fix)
  expect_identical(r$reading[[2]]$fix$kind, "roles")
  expect_false(identical(r$status, "ok"))
  # a fix naming no statement goes only to statements that did not convert; here
  # none, and that is said rather than dropped
  r2 <- cv(b, bank = "ANZ", overrides = swap)
  expect_identical(r2$status, "ok")
  expect_match(r2$messages[1], "reached none of this file's statements", fixed = TRUE)
  r3 <- cv(b, bank = "ANZ", overrides = c(swap, list(statement = 9L)))
  expect_match(r3$messages[1], "does not hold", fixed = TRUE)
  expect_true(all(vapply(r3$reading, function(x) identical(x$outcome, "proven"), NA)))
})

test_that("one bundle counts once towards a layout", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  r <- cv(fixture("tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf"), bank = "ANZ")
  expect_identical(r$run_log$learn_action, "created,none")
  ly <- layouts_load(file.path(d, "layouts"), "anz")
  expect_length(ly, 1L)
  expect_length(ly[[1]]$layout$proved_by, 1L)
})

test_that("a file that cannot be read is still stamped and tracked", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  junk <- tempfile("junk_", fileext = ".pdf"); writeBin(as.raw(1:200), junk)
  r <- cv(junk, bank = "ANZ")
  expect_identical(r$status, "failed")
  expect_identical(r$run_log$layouts_state, "empty")
  expect_identical(r$run_log$institution, "anz")
  ev <- jsonlite::fromJSON(readLines(list.files(file.path(d, "tracking"), full.names = TRUE)[1])[1])
  expect_identical(ev$outcome, "unread")
  expect_identical(ev$institution, "anz")
})

test_that("a workbook's preamble never reaches the run log or the metadata", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  acct <- nz_test_account()
  x <- tempfile("wb_", fileext = ".xlsx")
  rows <- rbind(c(paste("BNZ account", acct, "- MS A EXAMPLE"), NA, NA, NA), NA,
                c("Txn Date", "Narrative", "Amt (NZD)", "Running Bal"),
                c("13/04/2025", "Opening balance", NA, "1000.00"), c("14/04/2025", "Salary", "2500.00", "3500.00"),
                c("15/04/2025", "Rent", "-1200.00", "2300.00"), c("17/04/2025", "Coffee", "-4.50", "2295.50"))
  openxlsx::write.xlsx(as.data.frame(rows), x, colNames = FALSE)
  r <- cv(x)
  expect_identical(r$status, "ok")
  logs <- every_file_text(file.path(d, "logs"))
  expect_false(grepl(strsplit(acct, "-")[[1]][3], logs, fixed = TRUE))
  expect_false(grepl("EXAMPLE", logs, ignore.case = TRUE))
  expect_match(r$run_log$layout_hint, "narrative", fixed = TRUE)
})

test_that("a bank given as an account number is not used", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  acct <- nz_test_account()
  r <- cv(proven_csv(acct), bank = acct)
  expect_identical(r$bank$bank, "bnz")                  # taken from the statement instead
  expect_true(is.na(r$run_log$bank_hint))
  expect_match(paste(r$messages, collapse = " "), "long number", fixed = TRUE)
  body <- strsplit(acct, "-")[[1]][3]
  for (sub in c("logs", "tracking", "layouts"))
    expect_false(grepl(body, paste(c(every_file_text(file.path(d, sub)), list.files(file.path(d, sub), recursive = TRUE)),
                                   collapse = "\n"), fixed = TRUE), info = sub)
})

test_that("the front door never throws, for any argument at all", {
  for (p in list(1L, NULL, NA, c("a", "b"), "/no/such/file.pdf", list(1))) {
    r <- convert_statement(p, outdir = tempfile(), logdir = tempfile(), layouts_dir = tempfile(), tracking_dir = NA)
    expect_identical(r$status, "failed")
    expect_true(nzchar(r$run_id))
  }
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
    r <- convert_statement(p, outdir = out, logdir = out, layouts_dir = file.path(out, "ly"), tracking_dir = NA)
    expect_identical(r$status, "failed", info = basename(p))
    expect_true("unreadable" %in% as.character(r$diagnostics$category), info = basename(p))
    expect_false("unknown_format" %in% as.character(r$diagnostics$category), info = basename(p))
    # and the cure does not send the reader off to build a template
    expect_false(grepl("matches a template", paste(r$messages, collapse = " "), fixed = TRUE),
                 info = basename(p))
  }
})

test_that("a statement that IS readable still converts", {
  out <- tempfile("convgood_"); dir.create(out)
  r <- convert_statement(fixture("samples/raw/kiwibank/kiwibank_transaction_01.csv"),
                         outdir = out, logdir = out, layouts_dir = file.path(out, "ly"), tracking_dir = NA)
  expect_identical(r$status, "ok")
  expect_equal(nrow(r$feed_rows), 4L)
  # a header-only export is "matched the wording, read nothing" -- NOT unreadable:
  # it separates into fields, so there is a table there, it is simply empty.
  hdr <- tempfile("hdronly_", fileext = ".csv")
  writeLines("Type,Details,Particulars,Code,Reference,Amount,Date,ForeignCurrencyAmount,ConversionCharge", hdr)
  h <- convert_statement(hdr, outdir = out, logdir = out, layouts_dir = file.path(out, "ly"), tracking_dir = NA)
  expect_identical(h$status, "unsupported")
  expect_false("unreadable" %in% as.character(h$diagnostics$category))
})

# "Holds no table" is judged against the four separators the reader itself tries.
test_that("the separators the reader tries are the ones that make a table", {
  spaced <- tempfile("spaced_", fileext = ".txt")
  writeLines(c("Date Amount Details", "01/04/2025 -4.50 COFFEE"), spaced)
  expect_false(is.null(.unreadable_reason(read_input(spaced))))   # nothing separates it
  for (sep in c(",", "\t", ";", "|")) {
    p <- tempfile("sep_", fileext = ".csv")
    writeLines(paste("Date", "Amount", "Details", sep = sep), p)
    expect_null(.unreadable_reason(read_input(p)), info = sep)
  }
})

# What makes a file a table is a REPEATED SHAPE, not a separator character. These
# are the files a character test cannot tell apart from a statement.
test_that("a table is a repeated shape, read the way the reader will read it", {
  wr <- function(txt) { p <- tempfile("shape_", fileext = ".csv"); writeLines(txt, p); p }
  # the 6-line preamble is not a table; the header and its rows below it are
  expect_null(.unreadable_reason(
    read_input(fixture("samples/raw/asb/asb_transaction_export_01.csv"))))
  # the same export with no transactions in the period: nothing repeats, but the
  # last line is a header wide enough to be one
  expect_null(.unreadable_reason(read_input(wr(c(
    "Created date / time : 24 December 2014 / 19:38:14",
    "Bank 12; Branch 3456; Account 7890123-45-00",
    "From date 20141220",
    "Date,Unique Id,Tran Type,Cheque Number,Payee,Memo,Amount")))))
  # A QUOTED comma is part of a payee, not a field boundary. Counted blind, this
  # file reads 3 then 4 then 3 fields -- no shape at all -- and a real ASB export
  # ("Acme, Inc.") would have been called unreadable.
  expect_null(.unreadable_reason(read_input(wr(c(
    "Date,Payee,Amount",
    '2014/12/23,"Acme, Inc.",5678.90',
    "2014/12/24,Bob Ltd,-3.80")))))
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
  # A throwaway config: the CLI must not learn into, or track into, the install.
  cfg <- file.path(out, "config.yaml")
  writeLines(c("paths:", sprintf("  layouts: %s", file.path(out, "layouts")),
               sprintf("  tracking: %s", file.path(out, "tracking"))), cfg)
  rc <- file.path(R.home("bin"), "Rscript")
  txt <- suppressWarnings(system2(rc, c(shQuote(file.path(root, "run.R")),
    shQuote(file.path(root, "samples/raw/anz/anz_transaction_export_01.csv")), '""',
    shQuote(out)), stdout = TRUE, stderr = FALSE, env = paste0("BSO_CONFIG=", cfg)))
  skip_if(!length(txt), "Rscript produced no output")
  line <- grep("^run id:", txt, value = TRUE)
  expect_length(line, 1L)
  id <- trimws(sub("^run id:", "", line))
  expect_true(nzchar(id))
  expect_length(grep("^reading:", txt), 1L)
  # the whole point: that id opens the record the procedure asks for
  rec <- file.path(root, "logs", "runs", paste0(id, ".json"))
  expect_true(file.exists(rec))
  # a test must not leave the install's own run history behind. The id names
  # every record this run wrote, which is exactly what makes that possible.
  unlink(c(rec, file.path(root, "logs", "metadata", paste0(id, ".json"))))
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
  wr <- function(txt) { p <- tempfile("blank_", fileext = ".txt"); writeLines(txt, p); p }
  # the note that was read as a statement layout
  note <- c("Hi Beth,", "", "Use the transaction export, not the PDF.", "",
            "Thanks", "Michael")
  expect_false(is.null(.unreadable_reason(read_input(wr(note)))))
  # a sign-off separated from a greeting by a paragraph break, same shape
  expect_false(is.null(.unreadable_reason(read_input(wr(
    c("Morning Beth,", "", "Statement attached.", "", "Regards,", "Tim"))))))
  # ...and end to end it is UNREADABLE, not a layout nobody has built yet
  r <- convert_statement(wr(note), outdir = tempfile("blankout_"),
                         logdir = tempfile("blanklog_"), layouts_dir = tempfile("blankly_"), tracking_dir = NA)
  expect_identical(r$status, "failed")
  expect_true("unreadable" %in% as.character(r$diagnostics$category))
  expect_false("unknown_format" %in% as.character(r$diagnostics$category))
})

test_that("the case the earlier round protected is still a table", {
  # THE LINE THIS FIX MUST NOT CROSS. Two adjacent records that split the same way
  # ARE a table, however narrow -- test-forms.R depends on this one coming back
  # `unsupported` (a layout with no template), never `failed`.
  wr <- function(txt) { p <- tempfile("tab_", fileext = ".csv"); writeLines(txt, p); p }
  expect_null(.unreadable_reason(read_input(wr(c("a,b", "1,2")))))
  # a real export with a blank line in the middle of its rows is still a table:
  # the run either side of the gap is what proves it
  expect_null(.unreadable_reason(read_input(wr(
    c("Date,Payee,Amount", "2026-01-01,A,1.00", "", "2026-01-02,B,2.00",
      "2026-01-03,C,3.00")))))
  # a header-only export with a trailing newline keeps its one-record fallback
  expect_null(.unreadable_reason(read_input(wr(
    c("Date,Unique Id,Tran Type,Payee,Memo,Amount", "")))))
  # the shipped preamble export is unaffected
  expect_null(.unreadable_reason(
    read_input(fixture("samples/raw/asb/asb_transaction_export_01.csv"))))
})

test_that("every delimited sample in the corpus is still read as a table", {
  # The blast radius, measured rather than argued: whatever the shape test now
  # sees, no real bank export may become "could not be read".
  files <- list.files(file.path(engine_root(), "samples", "raw"),
                      pattern = "\\.(csv|tsv|txt)$", recursive = TRUE, full.names = TRUE)
  skip_if(length(files) == 0)
  bad <- Filter(function(f) !is.null(.unreadable_reason(read_input(f))), files)
  expect_identical(basename(bad), character(0),
                   info = paste("newly unreadable:", paste(basename(bad), collapse = ", ")))
})

