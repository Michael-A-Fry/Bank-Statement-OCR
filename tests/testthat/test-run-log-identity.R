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

# The record says WHAT produced the answer -- engine build, learned state, layout,
# outcome, proof -- so any past conversion can be re-run and gives the same
# answer; and it never holds the account number, only the bank.
test_that("every run record is stamped with what produced it, and no account number", {
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  acct <- nz_test_account()
  r <- cv(proven_csv(acct))
  rec <- jsonlite::fromJSON(file.path(d, "logs", "runs", paste0(r$run_id, ".json")))
  for (f in c("engine_version", "reader_version", "layouts_state", "outcome", "proof_kind",
              "institution", "bank_code", "learn_action", "feed_basis"))
    expect_true(f %in% names(rec), info = f)
  expect_identical(rec$requested_by, "tester")
  expect_false(grepl(strsplit(acct, "-")[[1]][3],
                     paste(readLines(file.path(d, "logs", "runs", paste0(r$run_id, ".json"))), collapse = ""),
                     fixed = TRUE))
  # a failed run is recorded too, with its stamp
  f <- cv(file.path(tempdir(), "no_such_statement.csv"))
  rec2 <- jsonlite::fromJSON(file.path(d, "logs", "runs", paste0(f$run_id, ".json")))
  expect_identical(rec2$status, "failed")
  expect_identical(rec2$engine_version, engine_version())
})
