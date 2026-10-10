# test-recipes-admin.R -- the Admin page engine for recipes (R/recipes_admin.R).
# Every change is a new version file in a sandboxed server folder; nothing is
# edited, and nothing touches the repo's recipes/ or templates/.

rc_pdf <- function(..., width = 600, shift = NULL) {
  pages <- list(...)
  words <- lapply(seq_along(pages), function(p) {
    lines <- pages[[p]]
    rows <- lapply(seq_along(lines), function(i) {
      m <- gregexpr("\\S+", lines[i])[[1]]
      if (m[1] < 0) return(NULL)
      tx <- regmatches(lines[i], list(m))[[1]]
      data.frame(width = nchar(tx) * 5, height = 8, x = 20 + (as.numeric(m) - 1) * 5,
                 y = 20 + i * 12, space = TRUE, text = tx, stringsAsFactors = FALSE)
    })
    w <- do.call(rbind, rows)
    if (!is.null(shift) && length(shift) >= p) w$x <- w$x + shift[p]
    w
  })
  list(kind = "pdf", path = "", sha256 = NA_character_,
       pages = vapply(pages, paste, "", collapse = "\n"), words = words,
       page_width = rep(width, length(pages)), page_height = rep(800, length(pages)),
       page_ocr = rep(FALSE, length(pages)), meta = list())
}

# One table line: the date at column 1, the details at 10, money out ending at 62,
# money in at 76, the balance at 90 (a trailing OD or CR prints after it).
rc_line <- function(date = "", desc = "", dr = "", cr = "", bal = "", mark = "") {
  s <- rep(" ", 110)
  put <- function(s, txt, at) { ch <- strsplit(txt, "")[[1]]; if (length(ch)) s[at:(at + length(ch) - 1L)] <- ch; s }
  s <- put(s, date, 1); s <- put(s, desc, 10)
  if (nzchar(dr)) s <- put(s, dr, 63 - nchar(dr))
  if (nzchar(cr)) s <- put(s, cr, 77 - nchar(cr))
  if (nzchar(bal)) s <- put(s, bal, 91 - nchar(bal))
  if (nzchar(mark)) s <- put(s, mark, 92)
  sub("\\s+$", "", paste(s, collapse = ""))
}

rc_box <- function(period = "01 Feb 2026 to 28 Feb 2026", open = "1,000.00", close = "3,344.91",
                   glance = "Account at a glance") c(
  "ANZ                                                         Account statement", "",
  paste0(glance, "                         Statement date 28 Feb 2026"),
  "Account name       SAMPLE TRADING LIMITED",
  "Account number     01-0021-2348037-00",
  paste("Statement period  ", period),
  paste("Opening balance            ", open),
  paste("Closing balance            ", close), "")
rc_head <- rc_line("Date", "Transaction type and details", "Withdrawals", "Deposits", "Balance")
rc_rows <- list(
  rc_line("03 Feb", "EP    RIVERSIDE DAIRY", dr = "12.40", bal = "987.60"),
  rc_line("05 Feb", "DC    SALARY MATAI HOLDINGS", cr = "3,120.00", bal = "4,107.60"),
  rc_line("09 Feb", "DD    CITY COUNCIL RATES", dr = "268.15", bal = "3,839.45"),
  rc_line("14 Feb", "AP    TRANSFER TO SAVINGS", dr = "400.00", bal = "3,439.45"),
  rc_line("21 Feb", "VT    HARBOUR FUEL", dr = "96.72", bal = "3,342.73"),
  rc_line("26 Feb", "      CREDIT INTEREST PAID", cr = "2.18", bal = "3,344.91"))
rc_want <- c(-12.40, 3120.00, -268.15, -400.00, -96.72, 2.18)
rc_dates <- c("2026-02-03", "2026-02-05", "2026-02-09", "2026-02-14", "2026-02-21", "2026-02-26")
rc_total <- function(dr, cr, what = "period") rc_line("", paste("      Totals at end of", what), dr = dr, cr = cr)

# The one-page statement, and the same statement over two pages, its heading
# printed again on the second, a page total and a balance brought forward.
rc_one <- function(rows = rc_rows, box = rc_box(), extra = character(0))
  c(box, rc_head, unlist(rows), rc_total("777.27", "3,122.18"), extra, "", "Page 1 of 1")
rc_two_pages <- function() list(
  c(rc_box(), rc_head, unlist(rc_rows[1:3]), rc_total("280.55", "3,120.00", "page"), "", "Page 1 of 2"),
  c("ANZ", "", rc_head, rc_line("", "      Balance brought forward from previous page", bal = "3,839.45"),
    unlist(rc_rows[4:6]), rc_total("777.27", "3,122.18"), "", "Page 2 of 2"))
rc_write <- function(dir, name, lines) writeLines(lines, file.path(dir, name))
rc_tmpdir <- function() { d <- tempfile("recipes_"); dir.create(d); d }
rc_valid_yaml <- function(id = "kauri_test", version = 1, status = "proven", extra = character(0)) c(
  sprintf("recipe: %s", id), "format: 1", sprintf("version: %s", version), "bank: anz", "kind: pdf",
  sprintf("status: %s", status),
  "recognise: {all: [\"Account at a glance\", \"Transaction type and details\"]}",
  "statement_starts: \"Account at a glance\"",
  "period: {label: \"Statement period\"}",
  "table:",
  "  header: [\"Date\", \"Transaction type and details\", \"Withdrawals\", \"Deposits\", \"Balance\"]",
  "  columns:",
  "    date: {under: \"Date\"}",
  "    description: {under: \"Transaction type and details\"}",
  "    debit: {under: \"Withdrawals\"}",
  "    credit: {under: \"Deposits\"}",
  "    balance: {under: \"Balance\"}",
  "  ends_at: [\"Totals at end of page\", \"Totals at end of period\"]",
  "  no_rows: [\"No transactions for this period\"]",
  "dates: {format: \"%d %b\", year: period}",
  "money: {style: debit_credit_cols, negative: [\"OD\"]}", extra)

# ---- the loader ------------------------------------------------------------------------

ra_dirs <- function() {
  d <- tempfile("rca_"); dir.create(file.path(d, "shipped"), recursive = TRUE)
  rc_write(file.path(d, "shipped"), "kauri_test.yaml", rc_valid_yaml("kauri_test"))
  list(shipped = file.path(d, "shipped"), server = file.path(d, "server"))
}
ra_top <- function(dirs, id) recipes_load(c(dirs$shipped, dirs$server))
ra_live <- function(dirs) vapply(recipes_load(c(dirs$shipped, dirs$server)), `[[`, "", "ref")
ra_track <- function(d, ...) track_record(c(list(event = "convert", engine_version = "3.0.0"), list(...)), d)

test_that("tracking records which recipe read a statement, and which was only tried", {
  f <- track_reading_fields(list(matched_recipe = "kauri_test@2", outcome = "proven", transactions = data.frame()))
  expect_identical(f$recipe_id, "kauri_test"); expect_identical(f$recipe_version, 2L); expect_identical(f$recipe_used, "read")
  f <- track_reading_fields(list(recipe_tried = list(recipe = "kauri_test@1"), outcome = "check", transactions = data.frame()))
  expect_identical(f$recipe_used, "tried")
  d <- tempfile("trk_"); ra_track(d, recipe_id = "kauri_test", recipe_version = 1L, recipe_used = "read", outcome = "proven")
  expect_identical(.track_read(d)$records[[1]]$recipe_id, "kauri_test")
  # an id carrying an account number has nowhere to go
  expect_warning(ra_track(d, recipe_id = "acct_12345678", outcome = "proven"))
  expect_null(.track_read(d)$records[[2]]$recipe_id)
})

test_that("the overview has one row per recipe, with its use in the last 30 days", {
  dirs <- ra_dirs(); trk <- tempfile("trk_")
  ra_track(trk, recipe_id = "kauri_test", recipe_version = 1L, recipe_used = "read", outcome = "proven")
  ra_track(trk, recipe_id = "kauri_test", recipe_version = 1L, recipe_used = "read", outcome = "check")
  ra_track(trk, recipe_id = "kauri_test", recipe_version = 1L, recipe_used = "tried", outcome = "check")
  ra_track(trk, outcome = "proven")
  ov <- recipes_overview(dirs, trk)
  expect_identical(nrow(ov), 1L)
  expect_identical(ov$id, "kauri_test"); expect_identical(ov$bank, "ANZ"); expect_identical(ov$status, "proven")
  expect_true(ov$enabled); expect_identical(ov$origin, "shipped")
  expect_identical(c(ov$read, ov$needed_help, ov$tried_not_proven), c(3L, 2L, 1L))
})

test_that("off and on are new versions; the shipped file is never touched", {
  dirs <- ra_dirs(); before <- readLines(file.path(dirs$shipped, "kauri_test.yaml"))
  r <- recipe_set_enabled("kauri_test", FALSE, dirs)
  expect_true(r$ok); expect_identical(r$ref, "kauri_test@2")
  expect_identical(ra_live(dirs), character(0))
  ov <- recipes_overview(dirs, tempfile())
  expect_false(ov$enabled); expect_identical(ov$status, "retired"); expect_identical(ov$origin, "shipped, changed here")
  expect_match(recipe_set_enabled("kauri_test", FALSE, dirs)$why, "already off")
  r <- recipe_set_enabled("kauri_test", TRUE, dirs)
  expect_true(r$ok); expect_identical(ra_live(dirs), "kauri_test@3")
  expect_identical(recipes_overview(dirs, tempfile())$status, "proven")
  expect_identical(readLines(file.path(dirs$shipped, "kauri_test.yaml")), before)
  expect_setequal(list.files(dirs$server, "[.]yaml$"), c("kauri_test@v2.yaml", "kauri_test@v3.yaml"))
})

test_that("a change in plain fields is a new version that still reads the statement", {
  dirs <- ra_dirs(); inp <- rc_pdf(rc_one())
  r <- recipe_update("kauri_test", list(title = "ANZ everyday", recognise_add = "Statement date",
                                        ends_add = "Your available credit"), dirs, check = list(inp))
  expect_true(r$ok); expect_identical(r$ref, "kauri_test@2")
  rc <- recipes_load(c(dirs$shipped, dirs$server))[[1]]
  expect_identical(rc$title, "ANZ everyday")
  expect_true("Statement date" %in% rc$all); expect_true("Your available credit" %in% rc$ends_at)
  expect_identical(recipe_read(inp, rc)$outcome, "proven")
  # remove a word again
  expect_true(recipe_update("kauri_test", list(recognise_remove = "statement DATE"), dirs)$ok)
  expect_false("Statement date" %in% recipes_load(c(dirs$shipped, dirs$server))[[1]]$all)
})

test_that("a change that is not a recipe, or that breaks a statement, is refused in a sentence", {
  dirs <- ra_dirs(); inp <- rc_pdf(rc_one())
  r <- recipe_update("kauri_test", list(columns = c("date", "description", "money out", "money in")), dirs)
  expect_false(r$ok); expect_match(r$why, "^That change was not saved: There are 5 heading words but 4 columns")
  r <- recipe_update("kauri_test", list(columns = c("date", "description", "money out", "money in", "bogus")), dirs)
  expect_false(r$ok); expect_match(r$why, "\"bogus\" is not a column")
  r <- recipe_update("kauri_test", list(colour = "red"), dirs)
  expect_false(r$ok); expect_match(r$why, "\"colour\" is not something")
  r <- recipe_update("kauri_test", list(recognise_remove = c("Account at a glance", "Transaction type and details")), dirs)
  expect_false(r$ok); expect_match(r$why, "recognise")
  # swapping money out and money in makes the statement stop adding up: blocked
  r <- recipe_update("kauri_test", list(columns = c("date", "description", "money in", "money out", "balance")), dirs, check = list(inp))
  expect_false(r$ok); expect_match(r$why, "1 would no longer add up")
  expect_false(dir.exists(dirs$server) && length(list.files(dirs$server, "[.]yaml$")))
})

test_that("Test reads a statement with an unsaved change and says plainly whether it adds up", {
  dirs <- ra_dirs(); inp <- rc_pdf(rc_one())
  t <- recipe_test("kauri_test", inp, dirs)
  expect_identical(t$outcome, "proven"); expect_identical(t$rows, 6L); expect_match(t$why, "^It adds up: 6 transactions")
  t <- recipe_test("kauri_test", inp, dirs, changes = list(columns = c("date", "description", "money in", "money out", "balance")))
  expect_identical(t$outcome, "check"); expect_match(t$why, "^It does not add up")
  t <- recipe_test(yaml::yaml.load(paste(rc_valid_yaml("draft_x", status = "draft"), collapse = "\n")), inp)
  expect_identical(t$outcome, "proven")
  t <- recipe_test("kauri_test", inp, dirs, changes = list(date_format = "squiggle"))
  expect_identical(t$outcome, "check"); expect_match(t$why, "not a date")
  expect_false(dir.exists(dirs$server))
  # a date given as printed is turned into its format
  y <- .rca_apply(yaml::yaml.load(paste(rc_valid_yaml(), collapse = "\n")), list(date_format = "03/02/2026"))
  expect_identical(y$dates$year, "printed"); expect_match(y$dates$format, "%Y")
})

test_that("undo goes back a version by writing it again; accept and retire act on drafts", {
  dirs <- ra_dirs()
  expect_false(recipe_undo("kauri_test", dirs)$ok)
  recipe_update("kauri_test", list(title = "Changed"), dirs)
  r <- recipe_undo("kauri_test", dirs)
  expect_true(r$ok); expect_identical(r$ref, "kauri_test@3")
  expect_identical(recipes_load(c(dirs$shipped, dirs$server))[[1]]$title, "kauri_test")
  rc_write(dirs$server, "anz_draft_1@v1.yaml", rc_valid_yaml("anz_draft_1", status = "draft"))
  expect_false(recipe_accept("kauri_test", dirs)$ok)
  r <- recipe_accept("anz_draft_1", dirs); expect_true(r$ok)
  ov <- recipes_overview(dirs, tempfile())
  expect_identical(ov$status[ov$id == "anz_draft_1"], "proven")
  r <- recipe_retire("anz_draft_1", dirs); expect_true(r$ok)
  expect_false("anz_draft_1" %in% recipes_overview(dirs, tempfile(), hidden = FALSE)$id)
  expect_true("anz_draft_1" %in% recipes_overview(dirs, tempfile())$id)
  expect_match(recipe_set_enabled("nope", TRUE, dirs)$why, "no recipe called")
})

test_that("merge keeps one recipe of a design with both recipes' words; refuses different tables", {
  dirs <- ra_dirs(); inp <- rc_pdf(rc_one())
  dir.create(dirs$server)
  rc_write(dirs$server, "anz_draft_1@v1.yaml", sub("recognise: {all: [\"Account at a glance\", ",
    "recognise: {all: [\"Statement date\", \"Account at a glance\", ", rc_valid_yaml("anz_draft_1", status = "draft"), fixed = TRUE))
  na <- needs_attention(dirs, tempfile(), tempfile())
  expect_identical(na$counts[["merges"]], 1L); expect_identical(na$counts[["drafts"]], 1L)
  r <- recipe_merge("anz_draft_1", "kauri_test", dirs, check = list(inp))
  expect_true(r$ok); expect_identical(r$kept, "kauri_test"); expect_identical(r$retired, "anz_draft_1")
  live <- recipes_load(c(dirs$shipped, dirs$server))
  expect_identical(vapply(live, `[[`, "", "id"), "kauri_test")
  expect_true("Statement date" %in% live[[1]]$all)
  expect_identical(needs_attention(dirs, tempfile(), tempfile())$counts[["merges"]], 0L)
  # different tables: refused
  rc_write(dirs$server, "other_one@v1.yaml", sub("%d %b", "%d %b %Y", sub("year: period", "year: printed", rc_valid_yaml("other_one"), fixed = TRUE), fixed = TRUE))
  r <- recipe_merge("kauri_test", "other_one", dirs)
  expect_false(r$ok); expect_match(r$why, "read their tables differently")
})

test_that("a new draft from a statement and its answers; needs attention lists what to look at", {
  dirs <- ra_dirs(); unlink(file.path(dirs$shipped, "kauri_test.yaml"))
  inp <- rc_pdf(rc_one())
  expect_false(recipe_from_statement(inp, list(), dirs)$ok)
  r <- recipe_from_statement(inp, list(bank = "ANZ"), dirs)
  expect_true(r$ok); expect_identical(r$ref, "anz_draft_1@1")
  t <- recipe_test("anz_draft_1", inp, dirs); expect_identical(t$outcome, "proven")
  # a retired draft's id is never reused
  recipe_retire("anz_draft_1", dirs)
  expect_identical(recipe_from_statement(inp, list(bank = "ANZ"), dirs)$ref, "anz_draft_2@1")
  up <- tempfile("up_"); src <- tempfile(fileext = ".pdf"); writeLines("x", src)
  id <- record_upload(src, "s.pdf", dir = up); set_upload_status(id, "set_aside", dir = up)
  record_upload(src, "t.pdf", dir = up)
  trk <- tempfile("trk_")
  ra_track(trk, recipe_id = "anz_draft_2", recipe_version = 1L, recipe_used = "tried", outcome = "check")
  na <- needs_attention(dirs, trk, up)
  expect_identical(unname(na$counts), c(1L, 1L, 1L, 0L))
  expect_identical(na$set_aside$id, id); expect_identical(na$failing$id, "anz_draft_2")
})
