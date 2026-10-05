# test-tracking.R -- automatic-reading tracking (R/tracking.R, spec section 8).
# The one promise that matters most: no personal data can be written, by design
# (an allowlist of typed fields), not by care at each call site.

.tk_dir <- function() { d <- tempfile("tracking"); dir.create(d); d }
.tk_text <- function(d) paste(unlist(lapply(sort(list.files(d, full.names = TRUE)), readLines, warn = FALSE)), collapse = "\n")
.tk_lines <- function(d) lapply(unlist(lapply(sort(list.files(d, full.names = TRUE)), readLines, warn = FALSE)),
                                jsonlite::fromJSON, simplifyVector = FALSE)

.tk_convert <- function(ts, outcome = "proven", kind = "pdf", ...) {
  utils::modifyList(list(ts = ts, event = "convert", engine_version = "2.1.0", state_id = "e138cf5d6157",
         bank_code = "01", institution = "anz", layout_id = "anz_1", layout_version = 3L,
         kind = kind, pages = 2L, rows = 33L, outcome = outcome, proof_kind = "chain",
         checks_failed = character(0), repairs_tried = character(0), candidates = 1L,
         secs = 0.61234, derived = 0L), list(...))
}

test_that("one event is one JSON line, in the month's file, fields in allowlist order", {
  d <- .tk_dir()
  ok <- expect_silent(track_record(.tk_convert("2026-10-03T01:02:03Z"), d))
  expect_true(isTRUE(ok))
  expect_identical(list.files(d), "tracking-2026-10.jsonl")
  rec <- .tk_lines(d)[[1]]
  expect_identical(names(rec)[1:3], c("ts", "event", "engine_version"))
  expect_identical(rec$rows, 33L)
  expect_equal(rec$secs, 0.612)
  expect_identical(rec$checks_failed, list())
  # a second event appends, it does not replace
  track_record(list(event = "spot_check", ts = "2026-10-04T00:00:00Z", spot_check = "right"), d)
  expect_length(.tk_lines(d), 2)
})

test_that("ts is filled in when absent, and files rotate by month", {
  d <- .tk_dir()
  track_record(list(event = "learn", learn_action = "created"), d)
  rec <- .tk_lines(d)[[1]]
  expect_match(rec$ts, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
  expect_identical(list.files(d), sprintf("tracking-%s.jsonl", substr(rec$ts, 1, 7)))
  d2 <- .tk_dir()
  track_record(.tk_convert("2026-09-30T23:59:59Z"), d2)
  track_record(.tk_convert("2026-10-01T00:00:00Z"), d2)
  expect_identical(list.files(d2), c("tracking-2026-09.jsonl", "tracking-2026-10.jsonl"))
})

test_that("nothing off the allowlist is ever written: unknown fields, bad enums, free text", {
  d <- .tk_dir()
  pii <- list(description = "PAK N SAVE PETONE", amount = -123.45, date = "2026-09-14",
              account_number = "01-0428-1833424-000", file_name = "smith_john_sept.pdf",
              name = "John Smith")
  f <- c(.tk_convert("2026-10-03T01:02:03Z"), pii)
  f$outcome <- "maybe"                          # not an outcome
  f$institution <- "John Smith"                 # not an id
  f$bank_code <- "01-0428"                      # not a bank code
  f$layout_id <- "smith_john_sept.pdf"          # not an id
  f$rows <- 2.5                                 # not a whole number
  f$pages <- -1L                                # out of range
  f$state_id <- "Smith"                         # not a hash
  expect_warning(track_record(f, d))
  txt <- .tk_text(d)
  for (bad in c("PAK N SAVE", "123.45", "2026-09-14", "1833424", "smith", "Smith", "maybe", "2.5"))
    expect_false(grepl(bad, txt, fixed = TRUE), info = bad)
  rec <- .tk_lines(d)[[1]]
  expect_false(any(c("description", "amount", "date", "account_number", "file_name", "name",
                     "outcome", "institution", "bank_code", "layout_id", "rows", "pages",
                     "state_id") %in% names(rec)))
  expect_identical(rec$event, "convert")       # the rest of the event is still counted
  expect_identical(rec$kind, "pdf")
})

test_that("a warning names the field and the rule, never the refused value", {
  d <- .tk_dir()
  w <- character(0)
  withCallingHandlers(track_record(list(event = "convert", institution = "Jane Doe 0211234567",
                                        notes = "Jane Doe"), d),
                      warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_length(w, 2)
  expect_false(any(grepl("Jane|0211234567", w)))
  expect_true(any(grepl("institution", w)))
  expect_true(any(grepl("notes", w)))
})

test_that("list fields keep only allowed items and stay lists", {
  d <- .tk_dir()
  expect_warning(track_record(list(event = "convert", ts = "2026-10-03T00:00:00Z",
                                   checks_failed = c("balance_chain", "Mrs Smith owes", "unique"),
                                   repairs_tried = c("wider_cells", "Bad Value!")), d))
  rec <- .tk_lines(d)[[1]]
  expect_identical(unlist(rec$checks_failed), c("balance_chain", "unique"))
  expect_identical(unlist(rec$repairs_tried), "wider_cells")
  d2 <- .tk_dir()
  track_record(list(event = "convert", ts = "2026-10-03T00:00:00Z", checks_failed = "unique"), d2)
  expect_true(grepl("\"checks_failed\":[\"unique\"]", .tk_text(d2), fixed = TRUE))
})

test_that("a correction records roles and box movement only", {
  d <- .tk_dir()
  moves <- list(list(role_from = "credit", role_to = "debit", dx_left = 12.345, dx_right = -3),
                list(role_from = "John Smith", role_to = "debit", dx_left = 1, dx_right = 1),
                list(role_from = "text1", role_to = "description", dx_left = "far", dx_right = 0))
  expect_warning(track_record(list(event = "correction", ts = "2026-10-03T00:00:00Z", correction = moves), d))
  rec <- .tk_lines(d)[[1]]
  expect_length(rec$correction, 1)
  expect_identical(rec$correction[[1]], list(role_from = "credit", role_to = "debit", dx_left = 12.3, dx_right = -3L))
  expect_false(grepl("John", .tk_text(d)))
})

test_that("no valid event means nothing is written; failures return FALSE, never throw", {
  d <- .tk_dir()
  r <- suppressWarnings(track_record(list(rows = 3L), d))
  expect_false(isTRUE(r))
  r <- suppressWarnings(track_record(list(event = "party"), d))
  expect_false(isTRUE(r))
  expect_false(isTRUE(suppressWarnings(track_record("convert", d))))
  expect_false(isTRUE(suppressWarnings(track_record(NULL, d))))
  expect_length(list.files(d), 0)
  blocker <- tempfile("notadir"); writeLines("x", blocker)
  r <- suppressWarnings(track_record(list(event = "convert"), file.path(blocker, "sub")))
  expect_false(isTRUE(r))
  expect_true(nzchar(attr(r, "reason")))
})

test_that("the summary counts outcomes, kinds, banks, layouts, checks and spot checks", {
  d <- .tk_dir()
  track_record(.tk_convert("2026-09-01T00:00:00Z"), d)
  track_record(.tk_convert("2026-10-01T00:00:00Z"), d)
  track_record(.tk_convert("2026-10-02T00:00:00Z", outcome = "layout_match", proof_kind = "layout"), d)
  track_record(.tk_convert("2026-10-02T01:00:00Z", outcome = "check", kind = "scan",
                           checks_failed = c("balance_chain", "unique"), repairs_tried = "wider_cells",
                           derived = 1L), d)
  track_record(.tk_convert("2026-10-02T02:00:00Z", outcome = "unread", kind = "delimited",
                           institution = "asb", layout_id = NULL, layout_version = NULL,
                           checks_failed = "balance_chain"), d)
  track_record(list(event = "spot_check", ts = "2026-10-03T00:00:00Z", spot_check = "right"), d)
  track_record(list(event = "spot_check", ts = "2026-10-03T00:00:01Z", spot_check = "wrong"), d)
  track_record(list(event = "learn", ts = "2026-10-03T00:00:02Z", learn_action = "promoted",
                    layout_id = "anz_1", layout_version = 3L), d)
  track_record(list(event = "correction", ts = "2026-10-03T00:00:03Z",
                    correction = list(list(role_from = "credit", role_to = "debit", dx_left = 0, dx_right = 4))), d)
  s <- track_summary(d)
  expect_identical(s$statements, 5L)
  expect_identical(s$outcomes, c(proven = 2L, layout_match = 1L, check = 1L, unread = 1L))
  expect_identical(s$automatic, 3L)
  expect_equal(s$automatic_rate, 0.6)
  pdf <- s$by_kind[s$by_kind$kind == "pdf", ]
  expect_identical(pdf$statements, 3L); expect_equal(pdf$automatic_rate, 1)
  expect_equal(s$by_kind$automatic_rate[s$by_kind$kind == "scan"], 0)
  expect_true(is.na(s$by_kind$automatic_rate[s$by_kind$kind == "excel"]))
  expect_identical(s$banks, c(anz = 4L, asb = 1L))
  expect_identical(s$layouts, c(`anz_1@3` = 4L))
  expect_identical(s$checks_failed, c(balance_chain = 2L, unique = 1L))
  expect_identical(s$repairs_tried, c(wider_cells = 1L))
  expect_identical(s$with_derived, 1L)
  expect_identical(s$spot_checks, c(right = 1L, wrong = 1L, cant_tell = 0L, total = 2L))
  expect_identical(s$learn_actions, c(promoted = 1L))
  expect_identical(s$corrections, 1L)
  expect_identical(s$roles_changed, c(`credit -> debit` = 1L))
  expect_identical(s$events[["convert"]], 5L)
  expect_identical(s$first, "2026-09-01T00:00:00Z")
  expect_identical(s$files, 2L)
  # since: a date, a string or a time; the September file is not even opened
  s2 <- track_summary(d, since = "2026-10-01")
  expect_identical(s2$statements, 4L)
  expect_identical(s2$files, 1L)
  expect_identical(track_summary(d, since = as.Date("2026-10-02"))$statements, 3L)
  s3 <- track_summary(d, since = "not a date")
  expect_identical(s3$statements, 5L)
  expect_length(s3$notes, 1)
  # nothing at all
  e <- track_summary(file.path(d, "none"))
  expect_identical(e$statements, 0L)
  expect_true(is.na(e$automatic_rate))
})

# N224: Admin's "a person on Please check" counted 0 however many readings people
# confirmed, because a confirm is its own event and the proof kinds were counted
# over convert events only.
test_that("a person's confirm counts under 'a person', without counting the statement twice", {
  d <- .tk_dir()
  # sent to Please check (the reader's own proof kind), then confirmed by a person
  track_record(.tk_convert("2026-10-02T00:00:00Z", outcome = "check", proof_kind = "chain"), d)
  track_record(.tk_convert("2026-10-02T00:05:00Z", outcome = "check", event = "confirm", proof_kind = "person"), d)
  track_record(.tk_convert("2026-10-02T01:00:00Z", outcome = "check", proof_kind = "totals"), d)
  track_record(.tk_convert("2026-10-02T01:05:00Z", outcome = "check", event = "confirm", proof_kind = "person"), d)
  track_record(.tk_convert("2026-10-02T02:00:00Z"), d)
  s <- track_summary(d)
  expect_identical(s$statements, 3L)                     # a confirm is not another statement
  expect_identical(s$proof_kinds[["person"]], 2L)
  expect_identical(s$proof_kinds[["chain"]], 2L)
  expect_identical(s$proof_kinds[["totals"]], 1L)
  expect_identical(s$events[["confirm"]], 2L)
  # ...and the carry-off summary says the same
  out <- tempfile(fileext = ".json")
  expect_true(isTRUE(track_export(d, out)))
  expect_identical(jsonlite::fromJSON(out)$proof_kinds$person, 2L)
})

test_that("torn or hand-edited lines are skipped or cleaned, never counted as written", {
  d <- .tk_dir()
  track_record(.tk_convert("2026-10-01T00:00:00Z"), d)
  f <- file.path(d, "tracking-2026-10.jsonl")
  cat("{\"ts\":\"2026-10-01T00:00:01Z\",\"event\":\"conv", file = f, append = TRUE)   # torn
  cat("\n{\"ts\":\"2026-10-01T00:00:02Z\",\"event\":\"convert\",\"outcome\":\"proven\",\"kind\":\"pdf\",\"institution\":\"Mr Bloggs\",\"description\":\"RENT\"}\n",
      file = f, append = TRUE)
  s <- track_summary(d)
  expect_identical(s$statements, 2L)
  expect_identical(s$unreadable_lines, 1L)
  expect_false("Mr Bloggs" %in% names(s$banks))
})

test_that("the carry-off export is counts and codes only", {
  d <- .tk_dir()
  suppressWarnings(track_record(c(.tk_convert("2026-10-01T00:00:00Z"), list(description = "PAK N SAVE")), d))
  track_record(.tk_convert("2026-10-01T00:00:01Z", outcome = "check", checks_failed = "unique"), d)
  out <- file.path(tempfile("exp"), "summary.json")
  r <- track_export(d, out)
  expect_true(isTRUE(r))
  j <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_identical(j$statements, 2L)
  expect_identical(j$outcomes$proven, 1L)
  expect_identical(j$checks_failed$unique, 1L)
  expect_length(j$by_kind, 4)
  txt <- paste(readLines(out), collapse = "\n")
  expect_false(grepl("PAK N SAVE", txt, fixed = TRUE))
  expect_false(any(grepl("[.]part$", list.files(dirname(out)))))
  expect_false(isTRUE(track_export(d, "")))
  # re-exporting replaces the file
  track_record(.tk_convert("2026-10-01T00:00:02Z"), d)
  expect_true(isTRUE(track_export(d, out)))
  expect_identical(jsonlite::fromJSON(out)$statements, 3L)
})

test_that("a reading's convert event passes the allowlist untouched", {
  rd <- list(outcome = "check", template = list(signature = list(kind = "scan")),
             transactions = data.frame(date = c("2026-09-01", "2026-09-02"), amount = c(-5, 10),
                                       description = c("A", "B")),
             proof = list(kind = "chain", pages_used = c(1L, 2L, 2L), derived = 1L),
             checks = data.frame(check = c("balance_chain", "no_derived_amounts", "printed_totals"),
                                 ok = c(TRUE, FALSE, NA), why = "x"),
             candidates = data.frame(source = c("content", "layout:anz_1@3", "repair:wider_cells"),
                                     passed = FALSE, why = "x"),
             matched_layout = "anz_1@3", secs = 1.5)
  f <- track_reading_fields(rd, bank = list(institution = "anz", bank_code = "01", confidence = "high"),
                            state_id = "e138cf5d6157")
  d <- .tk_dir()
  expect_silent(track_record(f, d))
  rec <- .tk_lines(d)[[1]]
  expect_identical(rec$kind, "scan")
  expect_identical(rec$pages, 2L)
  expect_identical(rec$rows, 2L)
  expect_identical(rec$layout_id, "anz_1"); expect_identical(rec$layout_version, 3L)
  expect_identical(unlist(rec$checks_failed), "no_derived_amounts")
  expect_identical(unlist(rec$repairs_tried), "wider_cells")
  expect_identical(rec$candidates, 3L)
  expect_identical(rec$institution, "anz"); expect_identical(rec$bank_code, "01")
  # a custom bank name is tracked as its slug
  expect_identical(track_reading_fields(NULL, bank = "Smith Credit Union")$institution, "smith_credit_union")
})

test_that("account-number, amount, date and name shapes are refused in every typed field", {
  d <- .tk_dir()
  base <- list(event = "convert", ts = "2026-10-01T00:00:00Z")
  refuse <- list(
    institution = "acct_0104281833424", institution = "0104281833424000", institution = "john_smith 01",
    layout_id = "anz_0104281833424", layout_id = "0104281833424000", layout_id = "smith_john_sept.pdf",
    layout_id = "anz", engine_version = "123.45", engine_version = "JohnSmith", engine_version = "smithjohn.pdf",
    state_id = "0104281833424000", state_id = "123456", state_id = "01-0428-1833424-00",
    ts = "2026-09-14", ts = "2026-02-30T00:00:00Z", ts = "2999-01-01T00:00:00Z",
    pages = 1833424L, rows = 1833424L, candidates = 4500L, secs = 1833424.5, layout_version = 1833424L,
    bank_code = "0104", repairs_tried = "john_smith", checks_failed = "0104281833424")
  for (i in seq_along(refuse)) {
    f <- c(base, refuse[i])
    w <- character(0)
    withCallingHandlers(track_record(f, d), warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
    expect_true(length(w) >= 1L, info = names(refuse)[i])
    expect_false(any(grepl("1833424|smith|Smith|123[.]45|2026-09-14|2999", w)), info = names(refuse)[i])
  }
  txt <- .tk_text(d)
  for (bad in c("1833424", "0104", "smith", "Smith", "123.45", "2026-09-14", "2999", "2026-02-30", "4500", "123456"))
    expect_false(grepl(bad, txt, fixed = TRUE), info = bad)
  # an unknown field whose NAME was built from data is not echoed with its digits
  w <- character(0)
  withCallingHandlers(track_record(c(base, list(acct_0104281833424 = 1)), d),
                      warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("1833424", w)))
  # the shapes the tool itself writes all pass
  ok <- list(event = "convert", ts = "2026-10-01T00:00:00Z", engine_version = "1.23.1", state_id = "9de4fff208fe",
             institution = "the_co_operative_bank", layout_id = "the_co_operative_bank_12", layout_version = 4L,
             bank_code = "02", repairs_tried = TRACK_REPAIRS, checks_failed = TRACK_CHECKS)
  d2 <- .tk_dir()
  expect_silent(track_record(ok, d2))
  rec <- .tk_lines(d2)[[1]]
  expect_identical(sort(names(rec)), sort(names(ok)))
  expect_silent(track_record(list(event = "convert", state_id = "empty", engine_version = "unknown"), d2))
})

test_that("a correction names roles only from the column-role vocabulary", {
  d <- .tk_dir()
  moves <- list(list(role_from = "credit", role_to = "debit", dx_left = 1, dx_right = 1),
                list(role_from = "text2", role_to = "particulars", dx_left = 1, dx_right = 1),
                list(role_from = "smith", role_to = "debit", dx_left = 1, dx_right = 1),
                list(role_from = "credit", role_to = "debit", dx_left = 1833424, dx_right = 1))
  expect_warning(track_record(list(event = "correction", correction = moves), d))
  rec <- .tk_lines(d)[[1]]
  expect_length(rec$correction, 2)
  expect_false(grepl("smith|1833424", .tk_text(d)))
})

test_that("the check and repair lists match what the reader can report", {
  # The automatic reader's files, and the recipe reader's, which reports the same
  # checks of a recipe's table and one of its own (written as `check = "..."`).
  src <- unlist(lapply(list.files(fixture("R"), "^(auto_read.*|recipes)[.]R$", full.names = TRUE), readLines, warn = FALSE))
  skip_if(!length(src))
  checks <- unique(c(regmatches(src, regexpr('(?<=add\\(")[a-z_]+(?=")', src, perl = TRUE)),
                     regmatches(src, regexpr('(?<=check = ")[a-z_]+(?=")', src, perl = TRUE))))
  expect_true("statements_join" %in% checks)
  repairs <- unique(regmatches(src, regexpr('(?<="repair:)[a-z_]+(?=")', src, perl = TRUE)))
  expect_true(length(checks) > 5)
  expect_true(all(checks %in% TRACK_CHECKS), info = paste(setdiff(checks, TRACK_CHECKS), collapse = ", "))
  expect_true(all(repairs %in% TRACK_REPAIRS), info = paste(setdiff(repairs, TRACK_REPAIRS), collapse = ", "))
})

test_that("many processes appending at once: every event is one whole line", {
  skip_on_os("windows")                       # forks; the lock is the same there
  d <- .tk_dir()
  invisible(parallel::mclapply(1:6, function(w) for (i in 1:40)
    track_record(list(event = "convert", ts = "2026-10-01T00:00:00Z", outcome = "proven", kind = "pdf", rows = i), d),
    mc.cores = 6))
  s <- track_summary(d)
  expect_identical(s$statements, 240L)
  expect_identical(s$unreadable_lines, 0L)
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE), "tracking-2026-10.jsonl")   # no lock left behind
})

test_that("a line torn by a killed writer costs that line only, not the next event", {
  d <- .tk_dir()
  track_record(.tk_convert("2026-10-01T00:00:00Z"), d)
  cat("{\"ts\":\"2026-10-01T00:00:01Z\",\"ev", file = file.path(d, "tracking-2026-10.jsonl"), append = TRUE)
  track_record(.tk_convert("2026-10-01T00:00:02Z", outcome = "check"), d)
  s <- track_summary(d)
  expect_identical(s$statements, 2L)
  expect_identical(s$unreadable_lines, 1L)
  # a stale lock left by a dead process does not stop recording
  dir.create(file.path(d, "tracking-2026-10.jsonl.lock"))
  expect_true(isTRUE(track_record(.tk_convert("2026-10-01T00:00:03Z"), d)))
  expect_identical(track_summary(d)$statements, 3L)
})

test_that("the summary of an empty, missing or damaged store has every field", {
  d <- .tk_dir()
  file.create(file.path(d, "tracking-2026-09.jsonl"))                     # empty
  full <- names(track_summary(d))
  expect_identical(track_summary(d)$statements, 0L)
  expect_identical(names(track_summary(file.path(d, "none"))), full)
  # binary junk, nuls and bytes that are not UTF-8 in one month do not lose another month
  writeBin(as.raw(c(0:255, 0, 0, 10, 0, 10)), file.path(d, "tracking-2026-08.jsonl"))
  track_record(.tk_convert("2026-10-01T00:00:00Z"), d)
  s <- track_summary(d)
  expect_identical(names(s), full)
  expect_identical(s$statements, 1L)
  expect_true(s$unreadable_lines >= 1L)
  expect_identical(s$files, 3L)
})

test_that("a file's timing is one anonymous record, never counted as a statement", {
  d <- tempfile("trk_"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  track_record(list(event = "convert", kind = "pdf", outcome = "proven", rows = 5L), d)
  track_record(list(event = "timing", kind = "pdf", pages = 150L, statements = 50L, ocr_pages = 0L, secs = 35.2,
                    secs_read = 2.1, secs_bank = 5.8, secs_reading = 23.4, secs_files = 0.7,
                    source_file = "Smith J statement.pdf"), d)
  ln <- unlist(lapply(list.files(d, full.names = TRUE, recursive = TRUE), readLines))
  tim <- jsonlite::fromJSON(ln[grepl('"timing"', ln)])
  expect_identical(tim$secs_reading, 23.4)
  expect_null(tim$source_file)                        # a name never reaches the record
  s <- track_summary(d)
  expect_identical(as.integer(s$statements), 1L)
})
