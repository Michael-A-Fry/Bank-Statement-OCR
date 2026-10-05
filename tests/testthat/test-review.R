# test-review.R -- Admin -> Review (R/review.R and its screen in app.R).
#
# The product owner's words: "how do I go into admin, see what's been run and not
# worked and actually SEE it to fix it, accept it or reject it, at the moment I can
# confirm the selected layout but can't actually see it". These hold the three
# lists to that: each finds the file it shows from what the server already keeps,
# says so plainly when the file is gone, and reads it again without learning or
# writing anything.

# A statement drawn here, with no balance and no totals: nothing on it proves
# which column is which, so it always goes to Please check. Synthetic: no person,
# no account.
.rv_statement <- function(path, rows = list(c("02/04/2025", "Salary", "2,500.00"), c("05/04/2025", "Rent", "-1,200.00"),
                                            c("09/04/2025", "Coffee", "-4.50"), c("14/04/2025", "Groceries", "-85.20"))) {
  grDevices::pdf(path, width = 8.27, height = 11.69)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 595), ylim = c(842, 0), xaxs = "i", yaxs = "i")
  graphics::text(40, 60, "Everyday Account Statement", adj = 0, font = 2, cex = 1.4)
  graphics::text(40, 90, "Statement period 1 April 2025 to 30 April 2025", adj = 0)
  graphics::text(40, 140, "Date", adj = 0, font = 2); graphics::text(130, 140, "Description", adj = 0, font = 2)
  graphics::text(520, 140, "Amount", adj = 1, font = 2)
  y <- 140
  for (r in rows) { y <- y + 22
    graphics::text(40, y, r[1], adj = 0); graphics::text(130, y, r[2], adj = 0); graphics::text(520, y, r[3], adj = 1) }
  invisible(grDevices::dev.off())
  path
}
.rv_runs <- function(...) {
  rows <- list(...)
  do.call(rbind, lapply(rows, function(r) {
    base <- list(ts = NA_character_, run_id = NA_character_, kind = "statement", source_file = NA_character_,
                 source_sha256 = NA_character_, institution = NA_character_, bank_hint = NA_character_,
                 status = NA_character_, outcome = NA_character_, reason = NA_character_, message = NA_character_,
                 person_fix = NA_character_, proof_kind = NA_character_, fix_held = NA_character_)
    as.data.frame(utils::modifyList(base, r), stringsAsFactors = FALSE)
  }))
}

test_that("Needs a look: the newest run of each file that did not prove, newest first, with a plain reason", {
  runs <- .rv_runs(
    list(ts = "2026-10-01T09:00:00+0000", run_id = "r1", source_file = "a.pdf", source_sha256 = "aaa",
         status = "needs_review", outcome = "check", reason = "Nothing on the statement proves which column is which."),
    # ...and a.pdf was set right on Please check later: it leaves the list
    list(ts = "2026-10-01T09:05:00+0000", run_id = "r2", source_file = "a.pdf", source_sha256 = "aaa",
         status = "ok", outcome = "proven"),
    list(ts = "2026-10-02T10:00:00+0000", run_id = "r3", source_file = "b.csv", source_sha256 = "bbb",
         status = "unsupported", outcome = "unread", reason = "No row of the file has a date with a figure beside it, so no transaction table was found."),
    list(ts = "2026-10-03T08:00:00+0000", run_id = "r4", source_file = "c.pdf", source_sha256 = "ccc",
         status = "failed", message = "failed: the file is damaged; check the file opens"),
    # confirmed on Please check: converted, and waiting as a held fix instead
    list(ts = "2026-10-03T09:00:00+0000", run_id = "r5", source_file = "d.pdf", source_sha256 = "ddd",
         status = "ok", outcome = "check", proof_kind = "person"),
    # not a statement conversion at all
    list(ts = "2026-10-04T09:00:00+0000", run_id = "r6", kind = "wizard", source_file = "e.pdf", status = "failed"))
  d <- review_needs_look(runs)
  expect_identical(d$file, c("c.pdf", "b.csv"))
  expect_identical(d$run_id, c("r4", "r3"))
  # the failed one says why in the words of its message, without the code or the advice
  expect_identical(d$reason[1], "The file is damaged.")
  expect_match(d$reason[2], "^No row of the file has a date")
  # the same file under two names is one file; a run with no hash is its own
  two <- .rv_runs(list(ts = "2026-10-01T09:00:00+0000", run_id = "x1", source_file = "one.pdf", source_sha256 = "s",
                       status = "needs_review", reason = "r"),
                  list(ts = "2026-10-01T10:00:00+0000", run_id = "x2", source_file = "copy of one.pdf", source_sha256 = "s",
                       status = "needs_review", reason = "r"),
                  list(ts = "2026-10-01T11:00:00+0000", run_id = "x3", source_file = "nohash.pdf",
                       status = "unsupported", reason = "r"))
  expect_identical(review_needs_look(two)$run_id, c("x3", "x2"))
  expect_identical(nrow(review_needs_look(data.frame())), 0L)
  expect_identical(review_reason(NA, NA), "No reason was recorded.")
})

test_that("a run's file is found where it is kept, and when it is not, the screen is told why", {
  up <- tempfile("up_"); dir.create(up)
  f <- tempfile(fileext = ".pdf"); .rv_statement(f)
  sha <- file_sha256(f)
  id <- record_upload(f, name = "april.pdf", status = "needs_review", run_id = "run-1", dir = up)
  u <- read_uploads(up)
  expect_identical(u$sha256, sha)                                   # the record now carries its hash
  k <- review_kept(sha, "april.pdf", "run-9", u, up, NULL, 90)
  expect_identical(k$upload_id, id)
  expect_true(file.exists(k$path))
  # by the run the upload was recorded under, when there is no hash to go by
  expect_identical(review_kept(NA, "april.pdf", "run-1", u, up, NULL, 90)$upload_id, id)
  # never converted on Convert: not kept, and said so
  n <- review_kept("0000", "other.pdf", "run-2", u, up, NULL, 90)
  expect_true(is.na(n$path))
  expect_identical(n$why, "This file was not kept. Only files converted on Convert are kept.")
  # deleted by the retention purge: said so, with the period
  Sys.setFileTime(file.path(up, id, "april.pdf"), Sys.time() - 200 * 86400)
  purge_uploads(up, keep_days = 90)
  g <- review_kept(sha, "april.pdf", "run-1", read_uploads(up), up, NULL, 90)
  expect_true(is.na(g$path))
  expect_identical(g$why, "This file is no longer kept. Kept files are deleted after 90 days.")
  # the folder intake's original, by name -- only when it is the same bytes
  root <- tempfile("intake_"); dir.create(file.path(root, "failed"), recursive = TRUE)
  file.copy(f, file.path(root, "failed", "april.pdf"))
  expect_identical(review_kept(sha, "april.pdf", NA, NULL, up, root, 90)$path, file.path(root, "failed", "april.pdf"))
  expect_true(is.na(review_kept("not-the-same", "april.pdf", NA, NULL, up, root, 90)$path))
  # a name from the log can never walk out of the intake folders
  file.copy(f, file.path(root, "secret.pdf"))
  expect_true(is.na(review_kept(NA, "../secret.pdf", NA, NULL, up, root, 90)$path))
})

test_that("a layout's example is the newest kept upload it read; with none, the screen says why", {
  up <- tempfile("up_"); dir.create(up)
  f <- tempfile(fileext = ".pdf"); .rv_statement(f)
  old <- record_upload(f, name = "march.pdf", status = "ok", template = "anz_1@1", dir = up)
  Sys.sleep(1.1)
  new <- record_upload(f, name = "april.pdf", status = "ok", template = "anz_1@3", dir = up)
  record_upload(f, name = "other.pdf", status = "ok", template = "anz_2@1", dir = up)
  u <- read_uploads(up)
  ex <- review_layout_example("anz_1", u, up, 90)
  expect_identical(ex$upload_id, new)
  expect_identical(basename(ex$path), "april.pdf")
  nothing <- review_layout_example("westpac_1", u, up, 90)
  expect_true(is.na(nothing$path))
  expect_match(nothing$why, "learned from files that were not kept")
  # the files it read were deleted by the retention purge
  for (i in c(old, new)) Sys.setFileTime(list.files(file.path(up, i), full.names = TRUE), Sys.time() - 200 * 86400)
  purge_uploads(up, keep_days = 90)
  gone <- review_layout_example("anz_1", read_uploads(up), up, 90)
  expect_true(is.na(gone$path))
  expect_match(gone$why, "no longer kept. Kept files are deleted after 90 days")
})

test_that("a held fix is tied to the file it was held from: by the run log, else by its bank and time", {
  fix <- list(id = "tsb_0123456789ab", bank = "tsb", held = "2026-10-05T03:10:38Z")
  runs <- .rv_runs(
    list(ts = "2026-10-05T03:10:20+0000", run_id = "a", institution = "tsb", status = "needs_review"),
    list(ts = "2026-10-05T03:10:40+0000", run_id = "b", institution = "tsb", status = "ok", proof_kind = "person",
         fix_held = "tsb_0123456789ab"),
    list(ts = "2026-10-05T03:10:41+0000", run_id = "c", institution = "anz", status = "ok", proof_kind = "person"))
  expect_identical(runs$run_id[review_fix_run(fix, runs)], "b")
  # a fix held before the run log named it: the person's run for that bank, just after
  runs$fix_held <- NA_character_
  expect_identical(runs$run_id[review_fix_run(fix, runs)], "b")
  # nothing of that bank near that time: not guessed
  expect_true(is.na(review_fix_run(list(id = "x_1", bank = "kiwibank", held = fix$held), runs)))
  expect_true(is.na(review_fix_run(fix, data.frame())))
})

test_that("a confirm held for an admin names its fix in the run log, and Review finds its file", {
  skip_if_not_installed("pdftools")
  d <- tempfile("rvcv_"); dir.create(d)
  f <- .rv_statement(file.path(d, "april.pdf"))
  L <- file.path(d, "layouts"); LG <- file.path(d, "logs")
  r <- convert_statement(f, bank = "tsb", outdir = file.path(d, "o"), logdir = LG, layouts_dir = L,
                         tracking_dir = NA, formats = "csv", requested_by = "tester", confirm = TRUE)
  expect_identical(r$status, "ok")
  expect_length(r$fix_held, 1L)
  runs <- read_log_records(LG, "runs")
  expect_identical(runs$fix_held, r$fix_held)
  # nothing from the statement in it: a bank and a hash
  expect_match(runs$fix_held, "^tsb_[0-9a-f]{12}$")
  rec <- review_fix_record(r$fix_held, L)
  expect_identical(rec$bank, "tsb")
  expect_identical(runs$run_id[review_fix_run(rec, runs)], r$run_id)
  up <- file.path(d, "uploads")
  id <- record_upload(f, name = "april.pdf", status = r$status, run_id = r$run_id, dir = up)
  expect_identical(review_kept(runs$source_sha256, runs$source_file, runs$run_id, read_uploads(up), up, NULL, 90)$upload_id, id)
  # a run that held nothing says so
  r2 <- convert_statement(f, bank = "tsb", outdir = file.path(d, "o2"), logdir = file.path(d, "logs2"),
                          layouts_dir = L, tracking_dir = NA, formats = "csv", requested_by = "tester")
  expect_true(is.na(read_log_records(file.path(d, "logs2"), "runs")$fix_held))
  expect_null(review_fix_record("tsb_nothere", L))
  expect_null(review_fix_record("../x", L))
})

test_that("review_read reads a kept file again for its picture, and learns, logs and writes nothing", {
  skip_if_not_installed("pdftools")
  d <- tempfile("rvrd_"); dir.create(d)
  f <- .rv_statement(file.path(d, "april.pdf"))
  before <- list.files(d, recursive = TRUE, all.files = TRUE)
  rd <- review_read(f, bank = "TSB")
  expect_true(rd$ok)
  expect_identical(rd$pages, 1L)
  expect_length(rd$units, 1L)
  expect_identical(rd$units[[1]]$outcome, "check")
  cl <- review_columns(rd)
  expect_true(all(c("date", "description", "amount") %in% cl$field))
  expect_true(all(c("page", "x_min", "x_max", "ink_min", "ink_max", "kind", "heading") %in% names(cl)))
  expect_identical(review_first_page(rd, TRUE), 1L)
  # only what a picture needs: no transaction, no figure
  for (u in rd$units)
    expect_setequal(names(u), c("outcome", "why", "pages", "columns", "matched_layout"))
  expect_false(any(grepl("2,500|2500", unlist(lapply(rd$units, function(u) unlist(u))))))
  # with a person's roles, read on their own
  rr <- review_read(f, bank = "TSB", roles = "amount")
  expect_identical(review_columns(rr)$field[review_columns(rr)$kind == "money"], "amount")
  # nothing was written beside the file (no layout, no log, no output)
  expect_identical(list.files(d, recursive = TRUE, all.files = TRUE), before)
  # a file that cannot be read says so, and never throws
  bad <- file.path(d, "not.pdf"); writeLines("not a pdf", bad)
  b <- review_read(bad)
  expect_false(b$ok); expect_true(nzchar(b$why))
  expect_identical(review_first_page(list(units = list())), 1L)
  expect_null(review_columns(NULL))
  # ...and it is a job a child process can run (task "review", R/jobs.R)
  j <- job_run_task("review", f, list(outdir = d, bank = "TSB"))
  expect_identical(review_columns(j)$field, cl$field)
})

# ---- the screen (app.R) -----------------------------------------------------------
.rv_app <- function() {
  p <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(p))
  readLines(p, warn = FALSE)
}

test_that("Admin has a Review tab beside Banks, with the three lists and a page beside each", {
  src <- .rv_app(); joined <- paste(src, collapse = "\n")
  banks <- grep('^\\s*"Banks",\\s*$', src); review <- grep('^\\s*"Review",\\s*$', src)
  auto <- grep('^\\s*"Automatic reading",\\s*$', src)
  expect_length(review, 1L)
  expect_true(banks[1] < review && review < auto[1])
  ui <- .src_block(src, '^\\s*"Review",\\s*$', 30L)
  for (h in c('h4\\("Needs a look"\\)', 'h4\\("Layouts"\\)', 'h4\\("Held fixes"\\)'))
    expect_match(ui, h)
  for (o in c("adm_rv_look", "adm_rv_look_view", "adm_rv_layouts", "adm_rv_lay_view", "adm_rv_fixes", "adm_rv_fix_view"))
    expect_match(ui, sprintf('"%s"', o), info = o)
  # Needs a look: file name, bank, date and the plain reason
  look <- .src_block(src, "output\\$adm_rv_look <- renderDT", 20L)
  expect_match(look, 'heads <- c\\("File", "Bank", "When", "Why"\\)')
  expect_match(look, "review_needs_look|rv_look_rows\\(\\)")
  # each pane: the page with its columns, or the plain reason it cannot be shown
  view <- .src_block(src, "\\.rv_view <- function", 15L)
  expect_match(view, 'plotOutput\\(paste0\\(px, "_plot"\\)')
  expect_match(view, "rv-gone")
  # Please check, from Needs a look
  expect_match(.src_block(src, "observeEvent\\(input\\$adm_rv_look_open, \\{", 8L),
               "\\.reread_on_convert\\(w\\$path, w\\$name, upload_id = .*bank = w\\$bank\\)")
})

test_that("Review draws a page the way Please check draws it -- the same helpers, not a copy", {
  src <- .rv_app(); joined <- paste(src, collapse = "\n")
  # ONE drawing of the column bands, called by both screens
  expect_length(grep("\\.draw_page_columns <- function", src), 1L)
  expect_match(.src_block(src, "output\\$cv_ck_plot <- renderPlot", 8L), "\\.draw_page_columns\\(r, res\\$reading\\[\\[s\\]\\]\\$columns\\)")
  rv <- .src_block(src, "\\.rv_outputs <- function", 40L)
  expect_match(rv, "\\.draw_page_columns\\(r, review_columns\\(pn\\$got\\(\\)\\)\\)")
  expect_match(.src_block(src, "\\.rv_pane <- function", 10L), "render_page_view\\(w\\$path, pn\\$page\\(\\), 100\\)")
  # the band and its ink are drawn in one place only (the column editor draws boxes)
  expect_length(grep("lwd = 1\\.6, lty = 2", src), 1L)
  # a spreadsheet has no page: its columns by their headings, from one helper
  expect_match(.src_block(src, "output\\$cv_ck_table <- renderUI", 8L), "\\.ck_columns_table\\(cols\\)")
  expect_match(rv, "\\.ck_columns_table\\(cl\\)")
  # the file is read in its own process, never in the app's
  expect_match(.src_block(src, "\\.rv_open <- function", 25L), 'pn\\$slot\\$start\\("review"')
  expect_false(grepl("review_read\\(", joined))
})

test_that("the page picture draws on a real page, with and without columns", {
  skip_if_not_installed("pdftools")
  src <- .rv_app()
  lab <- new.env(parent = globalenv()); sys.source(file.path(engine_root(), "ui_labels.R"), envir = lab)
  env <- new.env(parent = lab)
  env$PALETTE <- eval(parse(text = grep("^PALETTE <- list\\(", src, value = TRUE)[1])[[1]])
  for (nm in c(".col_label", ".ck_col_colour", ".draw_page_columns", ".page_plot_height")) {
    i <- grep(sprintf("^\\Q%s\\E <- function", nm), src, perl = TRUE)
    expect_length(i, 1L)
    for (j in seq(i, i + 40L)) {
      f <- tryCatch(eval(parse(text = paste(src[i:j], collapse = "\n"))[[1]], envir = env), error = function(e) NULL)
      if (is.function(f)) { assign(nm, f, envir = env); break }
    }
  }
  pdf_f <- .rv_statement(tempfile(fileext = ".pdf"))
  r <- render_page_view(pdf_f, 1L, 50)
  cols <- review_columns(review_read(pdf_f, bank = "TSB"))
  png_f <- tempfile(fileext = ".png")
  grDevices::png(png_f, width = 400, height = 560)
  expect_silent(env$.draw_page_columns(r, cols))
  expect_silent(env$.draw_page_columns(r, NULL))                       # still reading: the page alone
  expect_silent(env$.draw_page_columns(r, transform(cols, page = 2L))) # none on this page
  grDevices::dev.off()
  expect_gt(file.size(png_f), 1000)
  expect_identical(env$.page_plot_height(600, NULL), 600)
  expect_equal(env$.page_plot_height(595, r), round(595 * r$h / r$w))
})

test_that("every Review action is checked server-side and goes through the screen's one way of doing it", {
  src <- .rv_app()
  for (id in c("adm_rv_look_rows_selected", "adm_rv_look_open", "adm_rv_layouts_rows_selected",
               "adm_rv_fixes_rows_selected")) {
    blk <- .src_block(src, sprintf("observeEvent\\(input\\$%s, \\{", id), 3L)
    expect_match(blk, "^\\s*observeEvent\\([^{]+\\{\\s+req\\(admin_ok\\(\\)\\)", info = id)
  }
  # Confirm, Retire, Rename, Accept, Discard: the helpers Banks uses, each gated
  for (h in c("adm_rv_lay_confirm, \\.layout_change_ui\\(", "adm_rv_lay_retire, \\.layout_change_ui\\(",
              "adm_rv_lay_rename, \\.layout_rename_ui\\(", "adm_rv_fix_accept, \\.fix_act\\(",
              "adm_rv_fix_discard, \\.fix_act\\("))
    expect_length(grep(paste0("observeEvent\\(input\\$", h), src), 1L)
  expect_match(.src_block(src, "\\.layout_change_ui <- function", 6L), "req\\(admin_ok\\(\\)\\)")
  expect_match(.src_block(src, "\\.fix_act <- function", 6L), "req\\(admin_ok\\(\\)\\)")
  # Review acts on what is ON SCREEN, not on a table row a redraw may have moved
  expect_match(.src_block(src, "observeEvent\\(input\\$adm_rv_lay_confirm", 3L), "\\.rv_layout_on_screen")
  expect_match(.src_block(src, "observeEvent\\(input\\$adm_rv_fix_accept", 3L), "id = isolate\\(rv_fix\\$want\\(\\)\\)\\$fix_id")
  # accepting a fix files its file under the new layout, so the layout has an example
  acc <- .src_block(src, "\\.fix_accept_now <- function", 8L)
  expect_match(acc, "fix_accept\\(id, LAYOUTS_DIR")
  expect_match(acc, "set_upload_status\\(src\\$upload_id, .*template = r\\$ref")
  # every output of the tab that shows anything is admin-only
  outs <- grep('output(\\$adm_rv_[a-z_]+|\\[\\[paste0\\(px, "_[a-z]+"\\)\\]\\]) <- render(DT|UI|Plot)\\(\\{', src)
  expect_gte(length(outs), 14L)
  for (i in outs) expect_match(paste(src[i:(i + 1L)], collapse = " "), "req\\(admin_ok\\(\\)\\)", info = src[i])
})
