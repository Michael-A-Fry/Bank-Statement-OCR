# The fifteen polish decisions agreed with the owner (plain reasons, next steps,
# Read as, Next file to check, Accept all that add up, remembered banks, drop,
# what happens now, Undo, hover help, the week, the note). The browser tour
# (tools/ui/check.mjs) presses them; these hold the words and the rules.

.pl_labels <- function() {
  f <- file.path(engine_root(), "ui_labels.R"); skip_if_not(file.exists(f))
  e <- new.env(parent = globalenv()); sys.source(f, envir = e); e
}
.pl_app <- function() {
  f <- file.path(engine_root(), "app.R"); skip_if_not(file.exists(f))
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("every check has a plain problem sentence, and none says Failed", {
  L <- .pl_labels()
  expect_setequal(names(L$READING_PROBLEM_PLAIN), names(L$READING_CHECK_PLAIN))
  expect_setequal(names(L$CHECK_PROBLEM_PLAIN), names(L$CHECK_PLAIN))
  all <- c(L$READING_PROBLEM_PLAIN, L$CHECK_PROBLEM_PLAIN)
  expect_false(any(grepl("Failed", all)))
  expect_true(all(lengths(strsplit(all, "\\s+")) <= 10L))
  expect_identical(L$plain_failing_check("reading:unique"), "Two columns could be money out - tell us which")
  expect_identical(L$plain_failing_check("reading:balance_chain"), "The running balance does not add up")
  expect_identical(L$plain_check_problem("balance_reconciliation [statement 2]"), "The balance does not add up (statement 2)")
})

test_that("Couldn't read always says what to do next", {
  L <- .pl_labels()
  expect_match(L$unread_next("scan", ""), "original PDF from the bank")
  expect_match(L$unread_next("pdf", "damaged, encrypted, or not a PDF"), "password")
  expect_match(L$unread_next(NA, ""), "PDF, CSV or Excel")
  expect_match(L$unread_next("delimited", "", has_cols = TRUE), "Check the columns")
  expect_true(nzchar(L$unread_next("delimited", "no rows")))
  src <- .pl_app()
  expect_match(src, "unread_next(res_i$stamp$kind", fixed = TRUE)
  expect_match(src, 'div(class = "plan-next", nxt)', fixed = TRUE)
})

test_that("the table says Read as, drops the your-choice chip, and names what Check opens", {
  src <- .pl_app()
  expect_match(src, 'tags$th("Read as")', fixed = TRUE)
  expect_false(grepl('tags$th("Layout")', src, fixed = TRUE))
  expect_false(grepl('"your choice"', src, fixed = TRUE))
  expect_false(grepl('"Please check \\u2192"', src, fixed = TRUE))
  expect_match(src, 'sprintf("Check %s row%s"', fixed = TRUE)
})

test_that("hover help: one sentence for each outcome word and for a tick or cross", {
  L <- .pl_labels()
  expect_setequal(names(L$OUTCOME_HELP), c("ok", "warn", "bad"))
  expect_true(all(grepl("^(Done|Needs you|Couldn't read): .+\\.$", unlist(L$OUTCOME_HELP))))
  expect_match(L$TICK_HELP, "^Tick: "); expect_match(L$CROSS_HELP, "^Cross: ")
  src <- .pl_app()
  expect_match(src, "title = OUTCOME_HELP[[o$cls]]", fixed = TRUE)
  expect_match(src, "pass = TICK_HELP, fail = CROSS_HELP", fixed = TRUE)
})

test_that("a case has Next file to check, Accept all that add up, and a what-happens-now line", {
  src <- .pl_app()
  expect_match(src, 'actionButton("cv_next_check", "Next file to check', fixed = TRUE)
  expect_match(src, 'sprintf("Accept all that add up (%d)"', fixed = TRUE)
  expect_match(src, 'p(class = "plan-now", .case_now(', fixed = TRUE)
  # Accept all is the engine's own confirm, per file: never a bypass of the proof
  expect_match(src, "run_batch(f, as.character(b$chosen", fixed = TRUE)
  expect_match(src, "if (isTRUE(confirm)) a$confirm <- TRUE", fixed = TRUE)
  # ...and only for a file held ONLY because its design is new
  expect_match(src, 'identical(as.character(r$status %||% "")[1], "needs_review") && isTRUE(.new_design(r))', fixed = TRUE)
})

test_that("It's right and Set aside wait ten seconds with an Undo, and Set aside takes a note", {
  src <- .pl_app()
  expect_match(src, "UNDO_SECONDS <- 10L", fixed = TRUE)
  expect_match(src, '.ck_hold("confirm", res, ov = cv_ov())', fixed = TRUE)
  expect_match(src, '.ck_hold("aside", res)', fixed = TRUE)
  expect_match(src, 'actionButton("cv_ck_undo_pending", "Undo"', fixed = TRUE)
  expect_match(src, 'id = "cv_ck_aside_note"', fixed = TRUE)
  # a note never keeps a long number (it could be an account)
  expect_match(src, 'gsub("[0-9][0-9 -]{4,}[0-9]", "[number]"', fixed = TRUE)
})

test_that("files or a folder can be dropped on the page, into the ordinary picker", {
  src <- .pl_app()
  expect_match(src, "webkitGetAsEntry", fixed = TRUE)
  expect_match(src, "inp.files = box.files; $(inp).trigger('change')", fixed = TRUE)
})

test_that("the bank is remembered by a salted mark and a name pattern, never the number", {
  d <- tempfile("bm_")
  salt <- bank_memory_salt(d)
  expect_true(nchar(salt) >= 32L)
  expect_identical(bank_memory_salt(d), salt)          # made once
  expect_identical(bank_name_pattern("ANZ_March_2024.pdf"), bank_name_pattern("anz april 2025.PDF"))
  expect_false(grepl("[0-9]", bank_name_pattern("Kiwi 38-9000-0123456-00.csv")))
  expect_true(is.na(bank_name_pattern("2024-03.pdf")))
  old <- options(statement_studio.account_salt = salt); on.exit(options(old))
  mk <- .bi_mark("12", "3456", "0123456", "00")
  expect_match(mk, "^[0-9a-f]{24}$")
  expect_false(grepl("3456|123456", mk))
  options(statement_studio.account_salt = "")
  expect_true(is.na(.bi_mark("12", "3456", "0123456", "00")))   # no salt, no mark
  expect_true(is.na(.bi_mark("12", "XXXX", "0123456", "00")))
  bank_memory_note(d, "Kiwi statement 12.csv", mk, "kiwibank")
  raw <- paste(readLines(file.path(d, "bank_memory.json")), collapse = "")
  expect_false(grepl("0123456", raw))
  expect_identical(bank_memory_recall(d, "something.csv", mk)$bank, "kiwibank")
  expect_identical(bank_memory_recall(d, "kiwi Statement 99.csv")$bank, "kiwibank")
  expect_null(bank_memory_recall(d, "other.csv"))
})

test_that("Admin's week is one line, and a set-aside note reaches the admin", {
  now <- as.POSIXct("2026-10-10 12:00:00", tz = "UTC")
  up <- data.frame(ts_utc = c("2026-10-09T10:00:00Z", "2026-10-08T10:00:00Z", "2026-10-07T10:00:00Z", "2026-09-01T10:00:00Z"),
                   status = c("ok", "ok", "set_aside", "ok"), checked = c(FALSE, TRUE, FALSE, FALSE),
                   stringsAsFactors = FALSE)
  expect_identical(week_summary(up, now),
                   "This week: 3 statements - 1 done on their own, 1 checked by a person, 1 set aside.")
  expect_identical(week_summary(data.frame(), now), "This week: no statements yet.")
  d <- tempfile("up_"); dir.create(d)
  f <- tempfile(fileext = ".csv"); writeLines("a,b", f)
  id <- record_upload(f, status = "needs_review", dir = d)
  set_upload_status(id, "set_aside", detail = "Set aside on Please check for an admin to look at. Note: asked for the PDF", dir = d)
  u <- read_uploads(d)
  expect_identical(u$note[u$id == id], "asked for the PDF")
  na <- needs_attention(NULL, NULL, d, 30)
  expect_identical(na$set_aside$note, "asked for the PDF")
  expect_match(na$week, "^This week: 1 statement - 1 set aside\\.$")
})
