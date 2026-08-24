# shim.R -- a minimal testthat, so the repo's own test files can be RUN inside
# WebR (which has no testthat and no way to fetch one).
#
# It is deliberately strict about the things that matter: expect_equal uses
# all.equal, expect_identical uses identical, and a failure records the call so
# the report names the assertion, not just the test.

.TT <- new.env(parent = emptyenv())
.TT$pass <- 0L; .TT$fail <- character(0); .TT$skip <- character(0); .TT$cur <- ""

.tt_ok <- function() .TT$pass <- .TT$pass + 1L
.tt_bad <- function(msg) {
  .TT$fail <- c(.TT$fail, sprintf("%s\n      %s", .TT$cur, msg))
  invisible(FALSE)
}
.tt_lab <- function(x) {
  s <- paste(utils::capture.output(utils::str(x, give.attr = FALSE, vec.len = 6)),
             collapse = " ")
  substr(gsub("\\s+", " ", s), 1, 220)
}

test_that <- function(desc, code) {
  .TT$cur <- desc
  r <- tryCatch({ force(code); TRUE }, error = function(e) e)
  if (inherits(r, "error")) {
    m <- conditionMessage(r)
    if (grepl("^__SKIP__", m)) .TT$skip <- c(.TT$skip, paste0(desc, " (", sub("^__SKIP__ ?", "", m), ")"))
    else if (grepl("there is no package called|namespace .yaml|could not find function \"(read_yaml|write_yaml|as\\.yaml)\"|loadNamespace",
                   m)) .TT$skip <- c(.TT$skip, paste0(desc, " (needs a package WebR has not got: ", m, ")"))
    else .tt_bad(paste("ERROR:", m))
  }
  invisible(NULL)
}
context <- function(...) invisible(NULL)
skip <- function(msg = "") stop("__SKIP__ ", msg, call. = FALSE)
skip_if_not <- function(cond, msg = "condition not met") if (!isTRUE(cond)) skip(msg)
skip_if <- function(cond, msg = "skipped") if (isTRUE(cond)) skip(msg)

expect_true <- function(x, ...) if (isTRUE(x)) .tt_ok() else .tt_bad(paste("expect_true got", .tt_lab(x)))
expect_false <- function(x, ...) if (isFALSE(x)) .tt_ok() else .tt_bad(paste("expect_false got", .tt_lab(x)))
expect_null <- function(x, ...) if (is.null(x)) .tt_ok() else .tt_bad(paste("expect_null got", .tt_lab(x)))
expect_identical <- function(a, b, ...) {
  if (identical(a, b)) .tt_ok()
  else .tt_bad(sprintf("expect_identical\n        got : %s\n        want: %s", .tt_lab(a), .tt_lab(b)))
}
expect_equal <- function(a, b, ..., tolerance = 1e-8) {
  r <- isTRUE(all.equal(a, b, tolerance = tolerance, check.attributes = FALSE))
  if (r) .tt_ok()
  else .tt_bad(sprintf("expect_equal\n        got : %s\n        want: %s", .tt_lab(a), .tt_lab(b)))
}
expect_length <- function(x, n) if (length(x) == n) .tt_ok() else
  .tt_bad(sprintf("expect_length %d, got %d", n, length(x)))
expect_gt <- function(a, b) if (isTRUE(a > b)) .tt_ok() else .tt_bad(sprintf("expect_gt: %s > %s is FALSE", .tt_lab(a), .tt_lab(b)))
expect_gte <- function(a, b) if (isTRUE(a >= b)) .tt_ok() else .tt_bad(sprintf("expect_gte: %s >= %s is FALSE", .tt_lab(a), .tt_lab(b)))
expect_lt <- function(a, b) if (isTRUE(a < b)) .tt_ok() else .tt_bad(sprintf("expect_lt: %s < %s is FALSE", .tt_lab(a), .tt_lab(b)))
expect_lte <- function(a, b) if (isTRUE(a <= b)) .tt_ok() else .tt_bad(sprintf("expect_lte failed"))
expect_match <- function(x, pattern, fixed = FALSE, ignore.case = FALSE, ...) {
  if (!length(x)) return(.tt_bad(sprintf("expect_match on nothing (pattern %s)", pattern)))
  if (any(grepl(pattern, x, fixed = fixed, ignore.case = ignore.case))) .tt_ok()
  else .tt_bad(sprintf("expect_match: %s not found in %s", pattern, .tt_lab(x)))
}
expect_no_match <- function(x, pattern, fixed = FALSE, ...)
  if (!any(grepl(pattern, x, fixed = fixed))) .tt_ok() else .tt_bad("expect_no_match failed")
expect_error <- function(expr, regexp = NULL, ...) {
  r <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (is.null(r)) return(.tt_bad("expect_error: no error raised"))
  if (!is.null(regexp) && !grepl(regexp, conditionMessage(r)))
    return(.tt_bad(sprintf("expect_error: message %s does not match %s",
                           .tt_lab(conditionMessage(r)), regexp)))
  .tt_ok()
}
expect_warning <- function(expr, ...) { withCallingHandlers(force(expr), warning = function(w) invokeRestart("muffleWarning")); .tt_ok() }
expect_silent <- function(expr) { force(expr); .tt_ok() }

.tt_report <- function(label) {
  cat(sprintf("\n== %s ==\n  PASS %d   FAIL %d   SKIP %d\n",
              label, .TT$pass, length(.TT$fail), length(.TT$skip)))
  if (length(.TT$skip)) {
    cat("  -- skipped --\n")
    for (s in .TT$skip) cat("   .", substr(s, 1, 150), "\n")
  }
  if (length(.TT$fail)) {
    cat("  -- FAILURES --\n")
    for (f in .TT$fail) cat("   x ", f, "\n", sep = "")
  }
  invisible(length(.TT$fail))
}
.tt_reset <- function() { .TT$pass <- 0L; .TT$fail <- character(0); .TT$skip <- character(0) }
