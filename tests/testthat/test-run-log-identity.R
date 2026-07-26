# The forensic audit trail: WHO ran a conversion, and whether a record can ever be
# lost or overwritten. (#47)
#
# Two separate failures were in here:
#   1. Attribution was SELF-DECLARED. The typed name won over the detected identity
#      and only one string was stored, so a run log could not distinguish "Beth
#      typed her name" from "the machine established it" -- while the screen said
#      "Detected as X from your sign-in" about a value that, in the shipped
#      one-server model, was just the account the SERVER runs as.
#   2. run_id is (content hash + whole second) and the record file was opened "w",
#      so two conversions of the same statement in the same second left ONE record.

test_that("identity_fields keeps the claim and the machine fact apart", {
  f <- identity_fields(attested = "Beth", detected = "DOMAIN\\jsmith", source = "sso")
  expect_identical(f$attested_by, "Beth")
  expect_identical(f$detected_identity, "DOMAIN\\jsmith")   # NOT overwritten by the claim
  expect_identical(f$identity_source, "sso")
})

test_that("identity_fields records 'nobody attested' honestly instead of inventing one", {
  f <- identity_fields(attested = "   ", detected = "SVC_STATEMENTS", source = "os")
  expect_true(is.na(f$attested_by))            # blank stays blank, never back-filled
  expect_identical(f$detected_identity, "SVC_STATEMENTS")
  expect_identical(f$identity_source, "os")    # "os" = the server's own account
  g <- identity_fields()
  expect_true(is.na(g$attested_by)); expect_true(is.na(g$detected_identity))
  expect_identical(g$identity_source, "none")
  # an unrecognised source is never passed through as if it meant something
  expect_identical(identity_fields("a", "b", source = "trust me")$identity_source, "none")
})

test_that("two runs with the same run_id leave TWO audit records, not one", {
  ld <- tempfile("log_")
  p1 <- write_log_record(ld, "runs", "abc0123456-20260101120000",
                         list(run_id = "abc0123456-20260101120000", requested_by = "first"))
  p2 <- write_log_record(ld, "runs", "abc0123456-20260101120000",
                         list(run_id = "abc0123456-20260101120000", requested_by = "second"))
  expect_false(identical(p1, p2))
  expect_equal(length(list.files(file.path(ld, "runs"), pattern = "\\.json$")), 2L)
  recs <- read_log_records(ld, "runs")
  expect_setequal(recs$requested_by, c("first", "second"))
})

test_that("amend_log_record completes a record with the identity split", {
  ld <- tempfile("log2_")
  write_log_record(ld, "runs", "r1", list(run_id = "r1", requested_by = "Beth", status = "ok"))
  ok <- amend_log_record(ld, "runs", "r1",
                         identity_fields("Beth", "SVC_STATEMENTS", "os"))
  expect_true(ok)
  rec <- jsonlite::fromJSON(file.path(ld, "runs", "r1.json"))
  expect_identical(rec$status, "ok")                 # nothing else disturbed
  expect_identical(rec$attested_by, "Beth")
  expect_identical(rec$detected_identity, "SVC_STATEMENTS")
  expect_identical(rec$identity_source, "os")
})

test_that("amend_log_record fails CLOSED on a missing or ambiguous run id", {
  ld <- tempfile("log3_")
  write_log_record(ld, "runs", "r1", list(run_id = "r1"))
  expect_false(amend_log_record(ld, "runs", "nosuchrun", list(attested_by = "X")))
  # a same-second clash makes the id ambiguous: stamping one person's name onto
  # another person's conversion is exactly the silently-wrong outcome we forbid,
  # so it refuses rather than guessing which record is which.
  write_log_record(ld, "runs", "r1", list(run_id = "r1"))
  expect_false(amend_log_record(ld, "runs", "r1", list(attested_by = "X")))
  rec <- jsonlite::fromJSON(file.path(ld, "runs", "r1.json"))
  expect_null(rec$attested_by)
})

# #24 -- after a save the app re-converted with the template FORCED by id, which
# short-circuits detection, so the one moment it could prove the new template
# auto-detects was the one moment it never tested it. recognition_summary is the
# pure half of the real check (app.R runs detect_statement and passes the result).
test_that("recognition_summary says plainly when the new template WILL be found", {
  r <- recognition_summary(list(matched = TRUE, template_id = "beth_bank_pdf"), "beth_bank_pdf")
  expect_true(isTRUE(r$ok))
  expect_match(r$headline, "recognised automatically")
  expect_match(r$detail, "beth_bank_pdf", fixed = TRUE)
})

test_that("recognition_summary warns when another template still wins", {
  r <- recognition_summary(list(matched = TRUE, template_id = "anz_everyday_pdf"), "beth_bank_pdf")
  expect_false(isTRUE(r$ok))
  expect_match(r$headline, "anz_everyday_pdf", fixed = TRUE)
  expect_match(r$detail, "more specific")
})

test_that("recognition_summary warns when nothing matches, and says why", {
  r <- recognition_summary(list(matched = FALSE, template_id = NA_character_,
                                detail = "no template met its min_score"), "beth_bank_pdf")
  expect_false(isTRUE(r$ok))
  expect_match(r$headline, "NOT recognised")
  expect_match(r$detail, "no template met its min_score", fixed = TRUE)
})

test_that("recognition_summary never implies a pass when the check could not run", {
  r <- recognition_summary(NULL, "beth_bank_pdf")
  expect_true(is.na(r$ok))                     # NOT TRUE, and not FALSE either
  expect_match(r$headline, "could not check")
})

# #25 -- the toolkit's plain-English fingerprint box. A phrase is matched against
# the page text character for character, so a trailing space or a stray blank line
# is the difference between a template that recognises the bank and one that never
# matches -- and that is not something a non-technical user can debug.
test_that("fingerprint_phrases cleans what was typed without changing it", {
  expect_identical(fingerprint_phrases("Everyday Account Statement"),
                   "Everyday Account Statement")
  expect_identical(fingerprint_phrases("  Bank of Somewhere  \n\n Statement of Account \n"),
                   c("Bank of Somewhere", "Statement of Account"))
  # order is kept (the first phrase is the most distinctive one the drafter found)
  expect_identical(fingerprint_phrases("B\nA"), c("B", "A"))
  # a phrase added twice is not two conditions
  expect_identical(fingerprint_phrases("A\nA\n A "), "A")
  # nothing typed is nothing claimed -- never a phantom empty phrase
  expect_identical(fingerprint_phrases(""), character(0))
  expect_identical(fingerprint_phrases("   \n  \n"), character(0))
  expect_identical(fingerprint_phrases(NULL), character(0))
  expect_identical(fingerprint_phrases(NA), character(0))
})
