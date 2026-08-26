# Template validation (build-contract sections 6, 7, 11.5). amount_sign
# prerequisites must be caught at LOAD with a clear per-id message rather than
# crashing (or silently mis-signing) inside parse_statement.

.min_tmpl <- function(...) {
  t <- list(
    id = "x", bank = "B", statement_type = "e", format = "delimited",
    version = 1, min_score = 1,
    # A DISTINCTIVE header set: "Date"/"Amount" alone appear on every statement
    # ever exported, so validate_template now rejects them as a fingerprint (that
    # looseness is what let one template swallow other banks' files). These tests
    # are about saving / hiding / origin, so they carry a realistic fingerprint.
    fingerprint = list(header_contains_all = c("Date", "Amount", "Particulars")),
    delimiter = ",",
    columns = list(date = list(source = "Date"), amount = list(source = "Amount"),
                   description = list(source = "Amount")),
    amount_sign = "signed", currency = "NZD")
  ov <- list(...)
  for (k in names(ov)) t[[k]] <- ov[[k]]
  t
}

test_that("shipped templates all validate cleanly", {
  templates <- load_templates(templates_dir())
  expect_gte(length(templates), 6L)
})

test_that("debit_credit_cols without debit/credit columns is rejected at load", {
  t <- .min_tmpl(amount_sign = "debit_credit_cols")
  probs <- validate_template(t)
  expect_true(any(grepl("columns.debit", probs)))
  expect_true(any(grepl("columns.credit", probs)))
})

test_that("type_dc without a type column is rejected at load", {
  t <- .min_tmpl(amount_sign = "type_dc")
  probs <- validate_template(t)
  expect_true(any(grepl("columns.type", probs)))
})

test_that("valid debit_credit_cols / type_dc templates pass", {
  # debit_credit_cols has NO single 'amount' column -- it supplies debit + credit.
  # (Do not add an 'amount' key here: that masked the bug where validate_template
  # wrongly demanded columns.amount for this style, breaking guided-setup save.)
  ok1 <- .min_tmpl(amount_sign = "debit_credit_cols",
    columns = list(date = list(source = "Date"), description = list(source = "Details"),
      debit = list(source = "Dr"), credit = list(source = "Cr")))
  expect_length(validate_template(ok1), 0L)
  ok2 <- .min_tmpl(amount_sign = "type_dc", type_debit_value = "D",
    columns = list(date = list(source = "Date"), amount = list(source = "Amount"),
      description = list(source = "Amount"), type = list(source = "T")))
  expect_length(validate_template(ok2), 0L)
})

test_that("a debit_credit_cols draft round-trips through save (guided-setup save bug)", {
  # The tool's OWN draft of a Debit/Credit CSV must validate + save -- otherwise
  # guided setup dead-ends with 'Couldn't save'.
  f <- fixture("tests/testthat/fixtures/debit_credit_cols.csv")
  skip_if_not(file.exists(f))
  d <- draft_template(f, bank = "TestBank")
  expect_equal(d$amount_sign, "debit_credit_cols")
  expect_null(d$columns$amount)                       # the draft has no amount column
  expect_length(validate_template(d), 0L)             # ...and that is valid
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_silent(save_user_template(d, dir))           # save must not throw
  expect_true(file.exists(file.path(dir, paste0(d$id, ".yaml"))))
})

test_that("extras with a missing source column is rejected", {
  t <- .min_tmpl(extras = list(card = list(note = "no source here")))
  expect_true(any(grepl("extras.card", validate_template(t))))
})

test_that("load_templates hard-errors listing the bad template id", {
  dir <- file.path(tempdir(), paste0("badtmpl_", as.integer(runif(1, 1, 1e6))))
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  yaml::write_yaml(.min_tmpl(id = "broken", amount_sign = "type_dc"),
                   file.path(dir, "broken.yaml"))
  expect_error(load_templates(dir), "broken")
})

test_that("template_overview summarises every template with tested/user origin", {
  ov <- template_overview(load_template_set(templates_dir(), "does_not_exist"))
  expect_true(all(c("id","bank","type","format","amount_sign","date_format","origin","version")
                  %in% names(ov)))
  expect_true(nrow(ov) >= 10)
  expect_true(all(ov$origin == "tested"))                 # shipped defaults
  # a pdf template reports its table amount_sign / date_format, not NA
  anz <- ov[ov$id == "anz_everyday_pdf", ]
  expect_equal(anz$amount_sign, "debit_credit_cols")
  expect_equal(anz$date_format, "%d %b")
})

test_that("template_yaml round-trips back to a valid template", {
  t <- load_template_set(templates_dir(), "does_not_exist")[["westpac_everyday_pdf"]]
  back <- yaml::yaml.load(template_yaml(t))
  expect_length(validate_template(back), 0)
  expect_null(back$origin)                                 # origin stripped for edit
})

# A template whose validity window CLOSES BEFORE IT OPENS can never apply to any
# statement. It loaded, validated and detected like any other, and the only sign
# of it was that every statement it should have caught came back "no template for
# this layout yet" -- a silent no-op, which is the one thing this engine may not
# be. It is now rejected with a sentence an analyst can act on.
test_that("an effective window that closes before it opens is refused", {
  bad <- .min_tmpl(effective_from = "2030-01-01", effective_to = "2020-01-01")
  probs <- validate_template(bad)
  expect_length(probs, 1L)
  expect_match(probs, "effective_from")
  expect_match(probs, "could never apply")
  expect_match(probs, "swap")                          # says what to DO about it

  # every legitimate shape is untouched
  expect_length(validate_template(.min_tmpl(effective_from = "2014-01-01",
                                            effective_to = "2026-01-01")), 0L)
  expect_length(validate_template(.min_tmpl(effective_from = "2014-01-01")), 0L)
  expect_length(validate_template(.min_tmpl(effective_to = "2026-01-01")), 0L)
  expect_length(validate_template(.min_tmpl(effective_from = "2020-01-01",
                                            effective_to = "2020-01-01")), 0L)
  # a bound the tolerant parser cannot read is left alone rather than guessed at
  # (the soft window check in diagnose.R skips those for the same reason)
  expect_length(validate_template(.min_tmpl(effective_from = "some time in 2030",
                                            effective_to = "2020-01-01")), 0L)
  # ...and the shipped set still validates
  expect_gte(length(load_templates(templates_dir())), 6L)
})

# The rename half of this used to live here and NOWHERE else -- no screen offered
# a rename, so the only caller rename_user_template() ever had was this test. It
# is gone, and with it the risk of a template id changing under the run log, the
# feed manifest and the audit records that point back to it by that id.
test_that("user templates can be listed and deleted (management)", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  t <- .min_tmpl(); t$id <- "mybank_csv"
  save_user_template(t, dir)
  expect_true("mybank_csv" %in% user_template_ids(dir))

  expect_true(delete_user_template("mybank_csv", dir))
  expect_false("mybank_csv" %in% user_template_ids(dir))
  expect_false(delete_user_template("does_not_exist", dir))
  # ...and the engine no longer offers a rename at all
  expect_false(exists("rename_user_template", mode = "function"))
})

test_that("a user template can be hidden and un-hidden without deleting it", {
  udir <- tempfile(); on.exit(unlink(udir, recursive = TRUE), add = TRUE)
  t <- .min_tmpl(); t$id <- "parkme_csv"
  save_user_template(t, udir)

  # visible by default, and part of the active set used for detection/conversion
  expect_true("parkme_csv" %in% user_template_ids(udir))
  expect_true("parkme_csv" %in% names(load_template_set(templates_dir(), udir)))

  expect_true(set_user_template_hidden("parkme_csv", TRUE, udir))
  # gone from the active set (won't take part in detection) but still on disk
  expect_false("parkme_csv" %in% names(load_template_set(templates_dir(), udir)))
  expect_true("parkme_csv" %in% user_template_ids(udir))
  # ...and still visible to the management view
  active_ids <- names(load_template_set(templates_dir(), udir, include_hidden = TRUE))
  expect_true("parkme_csv" %in% active_ids)
  ov <- template_overview(load_template_set(templates_dir(), udir, include_hidden = TRUE))
  expect_equal(ov$hidden[ov$id == "parkme_csv"], "hidden")

  expect_false(set_user_template_hidden("parkme_csv", FALSE, udir))   # un-hide
  expect_true("parkme_csv" %in% names(load_template_set(templates_dir(), udir)))
})

test_that("shipped templates cannot be hidden", {
  udir <- tempfile(); on.exit(unlink(udir, recursive = TRUE), add = TRUE)
  expect_error(set_user_template_hidden("anz_everyday_pdf", TRUE, udir), "user-created")
})

test_that("duplicate_template_groups clusters same-layout user templates", {
  base <- .min_tmpl()
  a <- base; a$id <- "acme_v1"; a$bank <- "Acme"
  b <- base; b$id <- "acme_v2"; b$bank <- "Acme (branch)"    # same shape, different id/label
  c <- base; c$id <- "other"; c$columns$amount <- list(source = "Value")   # different column mapping
  tset <- list(acme_v1 = a, acme_v2 = b, other = c)
  for (id in names(tset)) tset[[id]]$origin <- "user"
  groups <- duplicate_template_groups(tset)
  expect_length(groups, 1L)                          # only the two acme variants group
  expect_setequal(groups[[1]], c("acme_v1", "acme_v2"))

  # shipped templates are excluded (user_only), and a lone template never groups
  none <- duplicate_template_groups(list(x = `$<-`(a, "origin", "default")))
  expect_length(none, 0L)
})

# ---------------------------------------------------------------------------
# columns.date.format may be a LIST of candidate formats (#42).
#
# THE BUG THIS CLOSES. templates/asb_everyday_csv.yaml pinned "%Y/%m/%d". ASB's
# own committed sample samples/raw/asb/asb_transaction_export_03.csv writes
# 13/10/2025, so it matched ASB confidently (7/6) and then returned EVERY date as
# NA with dates_readable = fail -- the "a bank tweaked its export" brittleness,
# with a green suite.
#
# THE RULE. All-or-nothing: a candidate wins only if it reads EVERY non-empty
# value in the column. A per-row fallback would be silently wrong -- "03/04" reads
# as 3 April under one candidate and 4 March under another, on the same statement.
# ---------------------------------------------------------------------------

test_that("resolve_date_format picks the ONE format that reads the whole column", {
  fmts <- c("%Y/%m/%d", "%d/%m/%Y")
  expect_identical(resolve_date_format(c("2014/12/20", "2014/12/21"), fmts), "%Y/%m/%d")
  expect_identical(resolve_date_format(c("13/10/2025", "17/10/2025"), fmts), "%d/%m/%Y")
  # blanks are not evidence either way and must not veto a candidate
  expect_identical(resolve_date_format(c("13/10/2025", "", NA, "  "), fmts), "%d/%m/%Y")
  # a single declared format resolves to itself, whatever the data says: the
  # ordinary case must behave exactly as it did before lists were allowed.
  expect_identical(resolve_date_format(c("nonsense"), "%d/%m/%Y"), "%d/%m/%Y")
  # nothing to judge on -> the first declared candidate (deterministic)
  expect_identical(resolve_date_format(character(0), fmts), "%Y/%m/%d")
  expect_identical(resolve_date_format(c("", NA), fmts), "%Y/%m/%d")
})

test_that("resolve_date_format refuses a MIXED column rather than guessing", {
  # The forensic case: one format reads rows 1-2, the other reads row 3, neither
  # reads all three. Reading them under different formats would put two dates in
  # the file under a day/month reading and one under month/day -- plausible and
  # wrong. Fail closed instead: NA -> every date NA -> dates_readable fails.
  mixed <- c("2014/12/20", "2014/12/21", "13/10/2025")
  expect_true(is.na(resolve_date_format(mixed, c("%Y/%m/%d", "%d/%m/%Y"))))
  # ...and no declared candidate at all is also NA, never a silent default
  expect_true(is.na(resolve_date_format(c("13/10/2025"), character(0))))
})

test_that("a format LIST is allowed for delimited but REJECTED for pdf", {
  ok <- .min_tmpl()
  ok$columns$date <- list(source = "Date", format = c("%Y/%m/%d", "%d/%m/%Y"))
  expect_length(validate_template(ok), 0L)

  # A pdf template's table.date_format goes straight to as.Date(), which RECYCLES
  # a format vector element-wise -- row 1 read one way, row 2 the other. That is a
  # whole column of plausible wrong dates, so it must be refused at load.
  pdf_t <- list(id = "p", bank = "B", statement_type = "e", format = "pdf",
    version = 1, min_score = 1, currency = "NZD",
    fingerprint = list(page_contains_all = list("Transaction type and details")),
    table = list(date_format = c("%d %b", "%d/%m/%Y"), amount_sign = "signed",
      columns = list(date = list(x_min = 0, x_max = 80),
                     description = list(x_min = 80, x_max = 300),
                     amount = list(x_min = 300, x_max = 400))))
  expect_true(any(grepl("must be a single date format", validate_template(pdf_t))))

  # a garbage entry anywhere in the list is still rejected (not just position 1)
  bad <- .min_tmpl()
  bad$columns$date <- list(source = "Date", format = c("%Y/%m/%d", "not a format"))
  expect_true(any(grepl("is not a date format", validate_template(bad))))
})

test_that("a multi-format template still shows ONE date_format per overview row", {
  # the overview and the duplicate-layout signature need a scalar per template
  expect_identical(.one_date_format(c("%Y/%m/%d", "%d/%m/%Y")), "%Y/%m/%d | %d/%m/%Y")
  ov <- template_overview(load_template_set(templates_dir(), "does_not_exist"))
  asb <- ov[ov$id == "asb_everyday_csv", ]
  expect_equal(nrow(asb), 1L)
  expect_identical(asb$date_format, "%Y/%m/%d | %d/%m/%Y")
})

# ---------------------------------------------------------------------------
# The fingerprint distinctiveness helpers are SHARED, not private (#9).
# R/forms.R reuses .fp_fingerprint_problems so a form template passes the SAME
# generic-fingerprint gate as a statement template. If either helper is ever moved
# into a local scope or renamed, forms.R silently falls back to a weaker inline
# rule -- so pin their visibility and signature here.
# ---------------------------------------------------------------------------
test_that("the fingerprint distinctiveness helpers are visible at top level", {
  expect_true(exists(".fp_fingerprint_problems", mode = "function"))
  expect_true(exists(".fp_specific", mode = "function"))
  expect_identical(names(formals(.fp_fingerprint_problems)), c("ph", "min_score", "fmt"))
  # and they still judge: a bare generic word cannot carry a pdf fingerprint
  expect_true(length(.fp_fingerprint_problems(c("Balance"), 1, "pdf")) > 0)
  expect_length(.fp_fingerprint_problems(c("Transaction type and details"), 1, "pdf"), 0L)
  expect_identical(.fp_specific(c("Balance", "Transaction type and details")), c(FALSE, TRUE))
})

test_that("resolve_delimiter is all-or-nothing and leaves single-delimiter templates alone", {
  one <- .min_tmpl()                                    # delimiter: ","
  # the ordinary case short-circuits: the header is never even consulted
  expect_identical(resolve_delimiter("anything at all", one), ",")
  expect_identical(resolve_delimiter(NA_character_, one), ",")

  multi <- .min_tmpl(delimiter = c(",", "\t"))
  expect_identical(resolve_delimiter("Date\tAmount\tParticulars", multi), "\t")
  expect_identical(resolve_delimiter("Date,Amount,Particulars", multi), ",")
  # a header carrying only SOME of the fingerprinted columns is not a fit under
  # either candidate -> first declared, never a partial split
  expect_identical(resolve_delimiter("Date\tAmount", multi), ",")
  # no header to judge on -> first declared (deterministic)
  expect_identical(resolve_delimiter("", multi), ",")

  # a missing delimiter key still means comma, as it always has
  expect_identical(resolve_delimiter("Date,Amount", .min_tmpl(delimiter = NULL)), ",")
})

test_that("an empty delimiter entry is rejected at load", {
  # an empty separator would split every line into single characters
  expect_true(any(grepl("non-empty separator",
                        validate_template(.min_tmpl(delimiter = c(",", ""))))))
  expect_length(validate_template(.min_tmpl(delimiter = c(",", "\t"))), 0L)
})

# ---- an unfinished seed can never join detection -----------------------------
# templates_seed/ ships starting points whose bands are placeholders marked
# "# TODO draw". YAML drops comments, so once a seed is copied into
# templates_user/ nothing the loader can see says it is unfinished - and six of
# the ten shipped seeds validate as-is, so a half-drawn one would take part in
# detection with placeholder coordinates.
test_that("every shipped seed template is marked draft", {
  dir <- seed_templates_dir()
  skip_if_not(dir.exists(dir))
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  expect_gt(length(files), 0)
  undrafted <- Filter(function(f) !isTRUE(yaml::read_yaml(f)$draft), files)
  expect_identical(basename(undrafted), character(0))
})

test_that("a draft template is refused with a reason that says what to do", {
  d <- tempfile(); dir.create(d)
  t <- list(id = "seed_x", bank = "SEED", statement_type = "everyday",
            format = "delimited", version = 1, currency = "NZD",
            amount_sign = "signed", date_format = "%d/%m/%Y",
            min_score = 1, draft = TRUE,
            # a specific column name, so the control case below is refused by the
            # DRAFT marker alone and not by the generic-fingerprint guard
            fingerprint = list(header_contains_all = list("Date", "Seedbank Ref")),
            columns = list(date = list(source = "Date"),
                           description = list(source = "Description"),
                           amount = list(source = "Amount")))
  yaml::write_yaml(t, file.path(d, "seed_x.yaml"))
  loaded <- suppressWarnings(load_templates(d, origin = "user", strict = FALSE))
  expect_null(loaded[["seed_x"]])                       # refused, not loaded
  why <- paste(attr(loaded, "load_errors"), collapse = " ")
  expect_match(why, "unfinished seed")
  expect_match(why, "draw its bands")                   # ...and what to do about it
  # and the SAME template without the marker loads normally, so the refusal is
  # the marker's doing and not a validation failure hiding behind it
  t$draft <- NULL
  yaml::write_yaml(t, file.path(d, "seed_x.yaml"))
  ok <- suppressWarnings(load_templates(d, origin = "user", strict = FALSE))
  expect_false(is.null(ok[["seed_x"]]))
})

# ---------------------------------------------------------------------------
# ONE LIBRARY, THREE KINDS.
#
# Reported: "even in the admin, other templates and other things are not even
# seen." Admin's list read the statement set and nothing else, so a form or a
# report template - the whole other half of the app - could not be found, read,
# checked, hidden or deleted on the one screen that manages layouts.
# ---------------------------------------------------------------------------

.lib_stmt <- function(id = "s1") list(id = id, bank = "ANZ", statement_type = "everyday",
  format = "pdf", version = 1,
  table = list(columns = list(date = list(x_min = 1, x_max = 2),
                              amount = list(x_min = 3, x_max = 4))))
.lib_fields <- function(id = "f1") list(id = id, bank = "IRD", statement_type = "summary",
  mode = "fields", format = "pdf", version = 2,
  fields = list(a = "A", b = "B", c = "C"))
.lib_doc <- function(id = "d1") list(id = id, bank = "ACME", statement_type = "report",
  mode = "document", format = "pdf", version = 3,
  tables = list(x = list(), y = list()), pairs = list(p = list()))

test_that("template_kind reads the paradigm off the template itself", {
  expect_identical(template_kind(.lib_stmt()), "statement")
  expect_identical(template_kind(.lib_fields()), "fields")
  expect_identical(template_kind(.lib_doc()), "document")
  # a template written before the other two modes existed declares no mode at all
  expect_identical(template_kind(list(id = "old")), "statement")
})

test_that("the library lists all three kinds, statements first", {
  ov <- library_overview(list(.lib_stmt()), list(.lib_fields()), list(.lib_doc()))
  expect_equal(nrow(ov), 3L)
  expect_identical(names(ov)[1], "kind")
  expect_identical(ov$kind, unname(.TEMPLATE_KIND_LABEL[c("statement", "fields", "document")]))
  expect_identical(ov$id, c("s1", "f1", "d1"))
  # "Other" is the word Convert and Add-a-template both use, so the distinction
  # is not given a third name on a third screen
  expect_true(all(grepl("^Other ", ov$kind[2:3])))
})

test_that("the library says what each template actually reads", {
  ov <- library_overview(list(.lib_stmt()), list(.lib_fields()), list(.lib_doc()))
  expect_identical(ov$reads[ov$id == "s1"], "2 columns")
  expect_identical(ov$reads[ov$id == "f1"], "3 values")
  expect_identical(ov$reads[ov$id == "d1"], "2 tables + 1 value")
  # a report template with neither says so rather than showing "0"
  d <- .lib_doc(); d$tables <- list(); d$pairs <- list()
  expect_identical(library_overview(documents = list(d))$reads, "nothing yet")
})

test_that("the library names a report and a form without calling them statements", {
  expect_identical(template_library_name(.lib_doc()), "ACME report")
  expect_identical(template_library_name(.lib_fields()), "IRD summary")
  # ...and still says "statement" where that is the truth
  expect_identical(template_library_name(.lib_stmt()), "ANZ everyday statement")
})

test_that("the library carries origin and hidden for every kind", {
  d <- .lib_doc(); d$origin <- "user"; d$hidden <- TRUE
  f <- .lib_fields(); f$origin <- "default"
  ov <- library_overview(fields = list(f), documents = list(d))
  expect_identical(ov$origin[ov$id == "d1"], "user")
  expect_identical(ov$hidden[ov$id == "d1"], "hidden")
  expect_identical(ov$origin[ov$id == "f1"], "tested")
  expect_identical(ov$hidden[ov$id == "f1"], "")
})

test_that("an empty library is an empty frame with the right columns, not an error", {
  ov <- library_overview()
  expect_equal(nrow(ov), 0L)
  expect_identical(names(ov),
    c("kind", "name", "id", "reads", "origin", "hidden", "version"))
})

# ---------------------------------------------------------------------------
# H11: the duplicate grouper, once there is more than one kind of template
# ---------------------------------------------------------------------------

test_that("a report template is not a duplicate of every other report template", {
  # H11. .template_shape() built its signature out of the STATEMENT keys only --
  # format, amount_sign, date_format and the transaction column mapping -- none
  # of which a report or a form has. So every one of them collapsed to the same
  # string and duplicate_template_groups() put the whole library in ONE group.
  # Three unrelated templates already tripped it; at forty it is one enormous
  # false alarm on the screen a maintainer opens to prune.
  a <- .lib_doc("rep_a"); a$origin <- "user"
  a$fingerprint <- list(page_contains_all = list("Consolidated position report"))
  a$tables <- list(holdings = list(name = "Holdings", columns = list(
    list(name = "Code", x_min = 30, x_max = 100),
    list(name = "Units", x_min = 100, x_max = 200))))
  b <- .lib_doc("rep_b"); b$origin <- "user"
  b$fingerprint <- list(page_contains_all = list("Annual fee schedule"))
  b$tables <- list(fees = list(name = "Fees charged", columns = list(
    list(name = "Fee", x_min = 40, x_max = 240),
    list(name = "Amount", x_min = 240, x_max = 500))))
  f <- .lib_fields("form_c"); f$origin <- "user"
  f$fields <- list(opening = list(any_of = list("Opening balance")))
  g <- .lib_fields("form_d"); g$origin <- "user"
  g$fields <- list(tax = list(any_of = list("Tax deducted")))

  sig <- vapply(list(a, b, f, g), .template_shape, character(1))
  expect_equal(length(unique(sig)), 4L)
  expect_length(duplicate_template_groups(list(rep_a = a, rep_b = b,
                                               form_c = f, form_d = g)), 0L)
})

test_that("two drafts of ONE report, and of one form, are still found", {
  # The grouper has to keep doing its job: a signature that never collides is as
  # useless as one that always does.
  a <- .lib_doc("rep_a"); a$origin <- "user"; a$bank <- "Northwind"
  a$fingerprint <- list(page_contains_all = list("Consolidated position report"))
  a$tables <- list(holdings = list(name = "Holdings", columns = list(
    list(name = "Code", x_min = 30, x_max = 100))))
  b <- a; b$id <- "rep_b"; b$bank <- "Northwind Trustee Services"
  # ...and the table KEYS may differ while the titles and bands agree, because a
  # key is a maintainer's handle and the title is what is printed on the page.
  names(b$tables) <- "hold"
  f <- .lib_fields("f1"); f$origin <- "user"
  f$fields <- list(opening = list(any_of = list("Opening balance", "Balance b/f")))
  g <- f; g$id <- "f2"; g$bank <- "IRD NZ"
  names(g$fields) <- "opening_balance"

  grp <- duplicate_template_groups(list(rep_a = a, rep_b = b, f1 = f, f2 = g))
  expect_length(grp, 2L)
  expect_true(any(vapply(grp, function(z) setequal(z, c("rep_a", "rep_b")), logical(1))))
  expect_true(any(vapply(grp, function(z) setequal(z, c("f1", "f2")), logical(1))))
})

test_that("a report whose TABLES differ is not a duplicate of one whose bands do", {
  a <- .lib_doc("rep_a"); a$origin <- "user"
  a$fingerprint <- list(page_contains_all = list("Position report"))
  a$tables <- list(h = list(name = "Holdings", columns = list(
    list(name = "Code", x_min = 30, x_max = 100))))
  b <- a; b$id <- "rep_b"
  b$tables$h$columns[[1]]$x_max <- 140          # the same table drawn differently
  expect_false(identical(.template_shape(a), .template_shape(b)))
  c2 <- a; c2$id <- "rep_c"
  c2$tables <- list(h = list(name = "Fees charged", columns = a$tables$h$columns))
  expect_false(identical(.template_shape(a), .template_shape(c2)))
})

test_that("the PII gate is reachable for any text a template captured off the page", {
  # G9b. .fp_has_pii() guarded exactly ONE field, the fingerprint phrase.
  # Measured on one string: "Prepared for Mr John Smith" was REFUSED as a
  # fingerprint phrase and accepted without comment as a table TITLE and as a
  # value's label wording -- and the builder's first gesture is "drag a box round
  # the table's title", off the page, word for word, into a file that gets copied
  # off the box. So the same rule is available to every validator, with one
  # signature, and it is pinned here because a validator in another file calls it
  # by name.
  expect_true(exists(".fp_pii_problems", mode = "function"))
  expect_identical(names(formals(.fp_pii_problems)), c("x", "what"))

  p <- .fp_pii_problems("Prepared for Mr John Smith", "table title")
  expect_length(p, 1L)
  expect_match(p, "table title")
  expect_match(p, "names a person")
  expect_false(grepl("[0-9]", p))                     # no ids, no scores

  # ...and it says nothing about the wordings a real template is made of
  expect_length(.fp_pii_problems(c("Holdings at period end", "Opening balance")), 0L)
  expect_length(.fp_pii_problems(NULL), 0L)
  expect_length(.fp_pii_problems(character(0)), 0L)
  expect_length(.fp_pii_problems("Master Account"), 0L)   # a title with no name after it
})
