# test-layouts.R -- the learned-layout store (R/layouts.R, spec Appendix A3).
# Readings here are built by hand in the reader's shape, so these tests pin the
# store's rules without depending on how well the reader reads any one file.

.lt_sig <- function(kind = "pdf", roles = c("date", "description", "debit", "credit", "balance"),
                    date_format = "%d %b", money_style = "debit_credit_cols",
                    tokens = c("balance", "date", "deposits", "details", "withdrawals"),
                    rel_x = c(0.05, 0.5, 0.72, 0.85, 1)) {
  list(kind = kind, roles = roles, date_format = date_format, money_style = money_style,
       sign_markers = character(0), balance_freq = "every", newest_first = FALSE,
       heading_tokens = tokens, producer = "Test producer", rel_x = rel_x, extras = character(0))
}

.lt_reading <- function(outcome = "proven", ...) {
  sig <- .lt_sig(...)
  tpl <- list(id = "auto_0123456789ab", bank = NA_character_, statement_type = NA_character_,
              format = "pdf", version = 1L, currency = "NZD",
              table = list(row_tol = 3L, date_format = sig$date_format, amount_sign = "debit_credit_cols",
                           columns = list(date = list(x_min = 40, x_max = 80),
                                          description = list(x_min = 80, x_max = 330),
                                          debit = list(x_min = 330, x_max = 395),
                                          credit = list(x_min = 395, x_max = 472),
                                          balance = list(x_min = 472, x_max = 545)),
                           columns_by_page = list(list(date = list(x_min = 41, x_max = 81)), NULL)),
              auto = list(roles = c("debit", "credit", "balance"), conv = "S", liab = FALSE,
                          dir = "old", engine = "1.0.0", outcome = outcome),
              signature = sig, boxes = list(data.frame(field = "date", ink_min = 44, ink_max = 69)))
  list(outcome = outcome, why = "test", template = tpl,
       proof = list(kind = "chain", derived = 0L), matched_layout = NULL)
}

.lt_sha <- function(i) paste(rep(sprintf("%02x", i %% 256), 32), collapse = "")
# A made-up NZ account number per account (none of these is a real account).
.lt_acct <- function(i) sprintf("12-3456-%07d-00", 1000000L + i)
.lt_dir <- function() { d <- tempfile("layouts"); dir.create(d); d }
.lt_files <- function(d) sort(list.files(d, recursive = TRUE))

test_that("an empty store loads as nothing and has a fixed state id", {
  d <- .lt_dir()
  expect_length(layouts_load(d), 0)
  expect_length(layouts_load(file.path(d, "not-there")), 0)
  expect_identical(layouts_state_id(d), "empty")
  expect_identical(layouts_state_id(file.path(d, "not-there")), "empty")
  expect_equal(nrow(layouts_banks(d)), 0)
})

test_that("three proven statements make a provisional layout proven, one version file per change", {
  d <- .lt_dir()
  a1 <- layout_learn(.lt_reading(), "anz", .lt_sha(1), d, accounts = .lt_acct(1))
  expect_identical(a1$action, "created")
  expect_identical(a1$ref, "anz_1@1")
  expect_identical(a1$status, "provisional")
  v1 <- file.path(d, "anz", "anz_1@v1.yaml")
  expect_true(file.exists(v1))
  before <- readLines(v1)

  a2 <- layout_learn(.lt_reading(), "ANZ", .lt_sha(2), d, accounts = .lt_acct(2))   # same bank, other spelling
  expect_identical(a2$action, "evidence_added")
  expect_identical(a2$ref, "anz_1@2")

  again <- layout_learn(.lt_reading(), "anz", .lt_sha(2), d)    # the same statement twice
  expect_identical(again$action, "none")
  expect_match(again$why, "already counts")

  a3 <- layout_learn(.lt_reading(), "anz", .lt_sha(3), d, accounts = .lt_acct(1))
  expect_identical(a3$action, "promoted")
  expect_identical(a3$status, "proven")

  a4 <- layout_learn(.lt_reading(), "anz", .lt_sha(4), d)
  expect_identical(a4$action, "none")
  expect_identical(a4$ref, "anz_1@3")

  expect_identical(.lt_files(d), c("anz/anz_1@v1.yaml", "anz/anz_1@v2.yaml", "anz/anz_1@v3.yaml"))
  expect_identical(readLines(v1), before)                       # never edited in place
  ly <- layouts_load(d)[["anz_1"]]
  expect_identical(ly$layout$version, 3L)
  expect_identical(ly$layout$proved_by, c(.lt_sha(1), .lt_sha(2), .lt_sha(3)))
  expect_identical(ly$layout$origin, "auto")
})

test_that("only a proven reading teaches, and a blocked bank pick teaches nothing", {
  d <- .lt_dir()
  for (oc in c("check", "layout_match", "unread"))
    expect_identical(layout_learn(.lt_reading(outcome = oc), "anz", .lt_sha(1), d)$action, "none")
  blocked <- list(bank = "anz", ask = TRUE, block_learning = TRUE, why = "x")
  expect_identical(layout_learn(.lt_reading(), blocked, .lt_sha(1), d)$action, "none")
  expect_identical(layout_learn(.lt_reading(), NA, .lt_sha(1), d)$action, "none")
  expect_identical(layout_learn(.lt_reading(), "", .lt_sha(1), d)$action, "none")
  expect_identical(layout_learn(.lt_reading(), "anz", "not-a-hash", d)$action, "none")
  r <- .lt_reading(); r$template$signature <- NULL
  expect_identical(layout_learn(r, "anz", .lt_sha(1), d)$action, "none")
  r <- .lt_reading(); r$proof$derived <- 1L                          # "proven" yet derived: refused
  expect_identical(layout_learn(r, "anz", .lt_sha(1), d)$action, "none")
  r <- .lt_reading(); r$checks <- data.frame(check = c("balance_chain", "unique"), ok = c(TRUE, FALSE), why = "x")
  expect_identical(layout_learn(r, "anz", .lt_sha(1), d)$action, "none")
  expect_identical(layout_learn(NULL, "anz", .lt_sha(1), d)$action, "none")
  expect_length(.lt_files(d), 0)
  # an unblocked pick is learned from, filed under its bank
  ok <- list(bank = "asb", ask = FALSE, block_learning = FALSE, why = "x")
  expect_identical(layout_learn(.lt_reading(), ok, .lt_sha(1), d)$action, "created")
  expect_true(file.exists(file.path(d, "asb", "asb_1@v1.yaml")))
})

test_that("a stored layout keeps the template, drops per-file geometry, and the reader can use it", {
  d <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)
  ly <- layouts_load(d)[["anz_1"]]
  expect_identical(ly$id, "anz_1")
  expect_identical(ly$bank, "anz")
  expect_identical(ly$format, "pdf")
  expect_equal(ly$table$columns$debit$x_min, 330)
  expect_null(ly$table$columns_by_page)
  expect_null(ly$boxes)
  expect_null(ly$signature)
  expect_null(ly$auto$outcome)
  lb <- ly$layout
  for (f in c("id", "bank", "status", "version", "created", "proved_by", "origin", "signature"))
    expect_true(f %in% names(lb), info = f)
  expect_match(lb$created, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
  expect_identical(lb$signature, .layout_sig_norm(.lt_sig()))
  # the reader's own view of it (R/auto_read.R)
  if (exists(".ar_layout_info", mode = "function")) {
    info <- .ar_layout_info(ly)
    expect_identical(info$ref, "anz_1@1")
    expect_false(info$proven)
    layout_confirm("anz_1", d)
    expect_true(.ar_layout_info(layouts_load(d)[["anz_1"]])$proven)
  }
})

test_that("layout_match: hard keys, the soft threshold, and pdf == scan", {
  d <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)
  L <- layouts_load(d)
  m <- layout_match(.lt_sig(), L)
  expect_identical(m$ref, "anz_1@1")
  expect_equal(m$score, 1)
  # a scan of the same design, OCR having misread one heading word
  m2 <- layout_match(.lt_sig(kind = "scan", tokens = c("balance", "date", "dep0sits", "details", "withdrawals"),
                             rel_x = c(0.06, 0.48, 0.72, 0.86, 1)), L)
  expect_identical(m2$id, "anz_1")
  expect_gte(m2$score, LAYOUT_MATCH_MIN)
  # a scan whose headings OCR could not read: missing words are not contrary words
  expect_identical(layout_match(.lt_sig(kind = "scan", tokens = character(0)), L)$id, "anz_1")
  # extra text columns meet by place, whatever the reading called them
  wide <- .lt_sig(roles = c("date", "description", "particulars", "code", "debit", "credit", "balance"),
                  rel_x = c(0.05, 0.3, 0.45, 0.55, 0.72, 0.85, 1))
  dw <- .lt_dir()
  r <- .lt_reading(); r$template$signature <- wide
  layout_learn(r, "asb", .lt_sha(1), dw)
  Lw <- layouts_load(dw)
  expect_identical(layout_match(modifyList(wide, list(kind = "scan", roles = c("date", "description", "text1", "text2", "debit", "credit", "balance"))), Lw)$id, "asb_1")
  expect_null(layout_match(modifyList(wide, list(roles = c("date", "text1", "description", "text2", "debit", "credit", "balance"))), Lw))
  # a description column whose longest line differs still matches
  expect_identical(layout_match(.lt_sig(rel_x = c(0.05, 0.42, 0.72, 0.85, 1)), L)$id, "anz_1")
  # hard keys: roles, money style, kind family
  expect_null(layout_match(.lt_sig(roles = c("date", "description", "amount", "balance"),
                                   rel_x = c(0.05, 0.5, 0.8, 1)), L))
  expect_null(layout_match(.lt_sig(money_style = "signed"), L))
  expect_null(layout_match(.lt_sig(kind = "delimited"), L))
  # another design sharing roles and date style: other words, columns elsewhere
  other <- .lt_sig(tokens = c("amount", "balance", "date", "narrative", "paid", "received"),
                   rel_x = c(0.1, 0.45, 0.66, 0.8, 1))
  expect_null(layout_match(other, L))
  expect_null(layout_match(NULL, L))
  expect_null(layout_match(.lt_sig(), list()))
  expect_null(layout_match(.lt_sig(), list("junk", NULL, list(a = 1))))
})

test_that("a different design becomes a second layout of the same bank", {
  d <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)
  r <- layout_learn(.lt_reading(tokens = c("amount", "balance", "date", "narrative", "paid", "received"),
                                rel_x = c(0.1, 0.45, 0.66, 0.8, 1)), "anz", .lt_sha(2), d)
  expect_identical(r$action, "created")
  expect_identical(r$id, "anz_2")
  expect_identical(names(layouts_load(d)), c("anz_1", "anz_2"))
  b <- layouts_banks(d)
  expect_identical(b$slug, "anz")
  expect_identical(b$layouts, 2L)
  expect_identical(b$provisional, 2L)
  expect_identical(b$statements, 2L)
})

test_that("ties break the same way every time: proven first, then more evidence, then lower number", {
  mk <- function(id, status, n) {
    ly <- list(id = id, table = list(), layout = list(id = id, bank = "anz", status = status, version = 1L,
               proved_by = vapply(seq_len(n), .lt_sha, ""), origin = "auto", signature = .lt_sig()))
    ly
  }
  L <- list(mk("anz_2", "provisional", 2), mk("anz_1", "provisional", 1), mk("anz_3", "proven", 1))
  expect_identical(layout_match(.lt_sig(), L)$id, "anz_3")
  expect_identical(layout_match(.lt_sig(), rev(L))$id, "anz_3")
  L2 <- L[1:2]
  expect_identical(layout_match(.lt_sig(), L2)$id, "anz_2")
  L3 <- list(mk("anz_5", "provisional", 1), mk("anz_4", "provisional", 1))
  expect_identical(layout_match(.lt_sig(), L3)$id, "anz_4")
  expect_identical(layout_match(.lt_sig(), rev(L3))$id, "anz_4")
  L4 <- c(L, list(mk("anz_9", "retired", 9)))
  expect_identical(layout_match(.lt_sig(), L4)$id, "anz_3")
})

test_that("confirm, correct, rename and retire each write a new version and keep every file", {
  d <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)

  c1 <- layout_confirm("anz_1", d, by = "Admin One")
  expect_true(c1$ok); expect_true(c1$changed); expect_identical(c1$ref, "anz_1@2")
  ly <- layouts_load(d)[["anz_1"]]
  expect_identical(ly$layout$status, "proven")
  expect_identical(ly$layout$origin, "confirmed")
  expect_identical(ly$layout$confirmed_by, "Admin One")
  c2 <- layout_confirm("anz_1", d)                               # nothing new to say
  expect_true(c2$ok); expect_false(c2$changed); expect_identical(c2$ref, "anz_1@2")

  n1 <- layout_rename("anz_1@2", "Everyday account", d)
  expect_true(n1$changed)
  expect_identical(layout_display_name(layouts_load(d)[["anz_1"]]), "ANZ layout 1: Everyday account")
  expect_false(layout_rename("anz_1", "   ", d)$ok)

  fixed <- .lt_reading()$template
  fixed$table$columns$debit$x_min <- 320
  k <- layout_correct("anz_1", fixed, d, by = "Analyst")
  expect_true(k$ok); expect_identical(k$ref, "anz_1@4")
  ly <- layouts_load(d)[["anz_1"]]
  expect_identical(ly$layout$origin, "corrected")
  expect_identical(ly$layout$status, "proven")
  expect_identical(ly$layout$proved_by, .lt_sha(1))
  expect_identical(ly$layout$name, "Everyday account")
  expect_equal(ly$table$columns$debit$x_min, 320)
  expect_null(ly$table$columns_by_page)

  r <- layout_retire("anz_1", d)
  expect_true(r$ok); expect_identical(r$ref, "anz_1@5")
  expect_length(layouts_load(d), 0)
  expect_identical(layouts_load(d, include_retired = TRUE)[["anz_1"]]$layout$status, "retired")
  expect_false(layout_retire("anz_1", d)$changed)
  expect_identical(.lt_files(d), sprintf("anz/anz_1@v%d.yaml", 1:5))

  # a retired layout never collects evidence: the same design starts afresh
  again <- layout_learn(.lt_reading(), "anz", .lt_sha(7), d)
  expect_identical(again$action, "created")
  expect_identical(again$id, "anz_2")
  # ...and confirming the retired one brings it back
  expect_true(layout_confirm("anz_1", d)$changed)
  expect_true("anz_1" %in% names(layouts_load(d)))
})

test_that("a corrected reading with no layout yet becomes a new proven layout", {
  d <- .lt_dir()
  k <- layout_correct(NULL, .lt_reading()$template, d, by = "Admin", bank = "kiwibank")
  expect_true(k$ok)
  expect_identical(k$ref, "kiwibank_1@1")
  ly <- layouts_load(d, "kiwibank")[["kiwibank_1"]]
  expect_identical(ly$layout$status, "proven")
  expect_identical(ly$layout$origin, "corrected")
  expect_false(layout_correct(NULL, .lt_reading()$template, d)$ok)    # no bank
  expect_false(layout_correct("kiwibank_1", list(), d)$ok)            # no table
})

test_that("admin changes on something that is not there say so and never throw", {
  d <- .lt_dir()
  for (f in list(function() layout_confirm("nope_1", d), function() layout_retire("nope_1", d),
                 function() layout_rename("nope_1", "x", d), function() layout_confirm(NULL, d),
                 function() layout_correct("nope_1", .lt_reading()$template, d))) {
    r <- f()
    expect_false(r$ok)
    expect_true(is.character(r$why) && nzchar(r$why))
  }
})

test_that("the state id follows every learned change and nothing else", {
  d <- .lt_dir()
  s0 <- layouts_state_id(d)
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)
  s1 <- layouts_state_id(d)
  expect_match(s1, "^[0-9a-f]{12}$")
  expect_false(identical(s0, s1))
  expect_identical(layouts_state_id(d), s1)                       # stable
  # backups, temp files, a lock, stray files: not layouts
  writeLines("x", file.path(d, "anz", "anz_1@v1.yaml.bak"))
  writeLines("x", file.path(d, "anz", "anz_1@v2.yaml.123.part"))
  dir.create(file.path(d, "anz", ".lock"))
  writeLines("x", file.path(d, "README.md"))
  expect_identical(layouts_state_id(d), s1)
  unlink(file.path(d, "anz", ".lock"), recursive = TRUE)
  # a copy elsewhere (other times, other folder) has the same id
  d2 <- .lt_dir()
  file.copy(file.path(d, "anz"), d2, recursive = TRUE)
  expect_identical(layouts_state_id(d2), s1)
  layout_rename("anz_1", "Go account", d)
  s2 <- layouts_state_id(d)
  expect_false(identical(s1, s2))
  layout_retire("anz_1", d)
  expect_false(identical(s2, layouts_state_id(d)))
})

test_that("a damaged file is skipped and named, never fatal", {
  d <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)
  layout_learn(.lt_reading(tokens = c("amount", "paid", "received"), rel_x = c(0.1, 0.45, 0.66, 0.8, 1)),
               "anz", .lt_sha(2), d)
  writeLines("layout: [unclosed", file.path(d, "anz", "anz_2@v2.yaml"))
  L <- layouts_load(d)
  expect_identical(names(L), "anz_1")                 # anz_2's latest is unreadable: not used at all
  expect_length(attr(L, "problems"), 1)
  # a file whose name and content disagree is not trusted either
  y <- yaml::read_yaml(file.path(d, "anz", "anz_1@v1.yaml"))
  yaml::write_yaml(y, file.path(d, "anz", "anz_1@v2.yaml"))
  L <- layouts_load(d)
  expect_length(L, 0)
  expect_length(attr(L, "problems"), 2)
})

test_that("display names come from the column roles, never from heading text", {
  ly <- list(layout = list(id = "anz_3", bank = "anz", status = "proven", version = 1L,
                           signature = .lt_sig()))
  expect_identical(layout_display_name(ly),
                   "ANZ layout 3: Date | Description | Money out | Money in | Balance")
  ly$layout$signature$roles <- c("date", "date2", "description", "text1", "amount", "balance")
  expect_identical(layout_display_name(ly, "My Bank"),
                   "My Bank layout 3: Date | Second date | Description | Text 1 | Amount | Balance")
  expect_type(layout_display_name(NULL), "character")
})

test_that("two writers racing for one version: exactly one file, never one replacing the other", {
  skip_on_os("windows")                       # forks; the same exclusivity holds there
  d <- .lt_dir()
  won <- parallel::mclapply(1:8, function(i) {
    ly <- list(table = list(), layout = list(id = "anz_1", bank = "anz", version = 1L, who = i))
    isTRUE(.layout_write(ly, d))
  }, mc.cores = 4)
  expect_identical(sum(unlist(won)), 1L)
  expect_identical(.lt_files(d), "anz/anz_1@v1.yaml")
  # and many learns at once, three designs: no version lost, every layout proven once
  d2 <- .lt_dir()
  designs <- list(list(), list(tokens = c("amount", "paid", "received"), rel_x = c(0.1, 0.45, 0.66, 0.8, 1)),
                  list(roles = c("date", "description", "amount", "balance"), money_style = "signed",
                       rel_x = c(0.1, 0.5, 0.8, 1)))
  jobs <- expand.grid(s = 1:5, d = 1:3)
  res <- parallel::mclapply(seq_len(nrow(jobs)), function(i)
    layout_learn(do.call(.lt_reading, designs[[jobs$d[i]]]), "anz", .lt_sha(i), d2,
                 accounts = .lt_acct(i))$action, mc.cores = 6)
  acts <- unlist(res)
  expect_identical(sum(acts == "created"), 3L)
  expect_identical(sum(acts == "promoted"), 3L)
  L <- layouts_load(d2)
  expect_length(L, 3)
  expect_true(all(vapply(L, function(l) l$layout$status == "proven" && length(l$layout$proved_by) == 3L, NA)))
  expect_length(attr(L, "problems"), 0)
})

test_that("a lock is released only by its holder", {
  d <- .lt_dir()
  rel1 <- .layout_lock(file.path(d, "anz"))
  expect_true(is.function(rel1))
  # the lock is taken over (as when stale) and a new holder takes it...
  unlink(file.path(d, "anz", ".lock"), recursive = TRUE)
  rel2 <- .layout_lock(file.path(d, "anz"))
  rel1()                                       # ...the first holder's release must not free it
  expect_true(dir.exists(file.path(d, "anz", ".lock")))
  rel2()
  expect_false(dir.exists(file.path(d, "anz", ".lock")))
})

test_that("a cut-short or hand-copied file is never used", {
  d <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)
  layout_learn(.lt_reading(), "anz", .lt_sha(2), d)
  f2 <- file.path(d, "anz", "anz_1@v2.yaml")
  full <- readLines(f2)
  # cut at every line: whatever still parses must not be taken for the layout
  for (n in seq(5, length(full) - 1L)) {
    writeLines(full[seq_len(n)], f2)
    L <- layouts_load(d)
    expect_length(L, 0)
    expect_length(attr(L, "problems"), 1)
  }
  writeLines(full, f2)
  expect_identical(layouts_load(d)[["anz_1"]]$layout$version, 2L)
  # a copy of an ANZ layout dropped in another bank's folder is not a layout
  dir.create(file.path(d, "asb"))
  file.copy(file.path(d, "anz", "anz_1@v1.yaml"), file.path(d, "asb", "anz_1@v3.yaml"))
  expect_identical(layouts_load(d)[["anz_1"]]$layout$version, 2L)
  expect_length(layouts_load(d, "asb"), 0)
})

test_that("a signature missing a field or malformed teaches nothing and never spawns layouts", {
  d <- .lt_dir()
  for (drop in c("kind", "roles", "money_style", "rel_x")) {
    r <- .lt_reading(); r$template$signature[[drop]] <- NULL
    a <- layout_learn(r, "anz", .lt_sha(1), d)
    expect_identical(a$action, "none", info = drop)
    expect_true(nzchar(a$why))
  }
  r <- .lt_reading(); r$template$signature$rel_x <- c(0.1, 0.5, 1)    # 3 positions, 5 columns
  expect_identical(layout_learn(r, "anz", .lt_sha(1), d)$action, "none")
  r <- .lt_reading(); r$template$signature$money_style <- "maybe"
  expect_identical(layout_learn(r, "anz", .lt_sha(1), d)$action, "none")
  expect_length(.lt_files(d), 0)
  # missing heading words (a scan OCR could not read) are unknown, not contrary:
  # learned, and matched again
  r <- .lt_reading(); r$template$signature$heading_tokens <- NULL
  expect_identical(layout_learn(r, "anz", .lt_sha(1), d)$action, "created")
  expect_identical(layout_learn(r, "anz", .lt_sha(2), d)$action, "evidence_added")
  # a signature with no positions at all still meets its layout on words and dates
  d2 <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d2)
  s <- .lt_sig(); s$rel_x <- NULL
  expect_identical(layout_match(s, layouts_load(d2))$id, "anz_1")
  # ...but with neither positions nor words, a shared date style is not enough
  s$heading_tokens <- NULL
  expect_null(layout_match(s, layouts_load(d2)))
})

test_that("one statement is evidence for one layout, however it is read again", {
  d <- .lt_dir()
  expect_identical(layout_learn(.lt_reading(), "anz", .lt_sha(1), d)$action, "created")
  # the same file, read differently after a reader change, matches nothing: no second layout
  other <- .lt_reading(tokens = c("amount", "paid", "received"), rel_x = c(0.1, 0.45, 0.66, 0.8, 1))
  a <- layout_learn(other, "anz", .lt_sha(1), d)
  expect_identical(a$action, "none")
  expect_match(a$why, "anz_1@1")
  expect_identical(.lt_files(d), "anz/anz_1@v1.yaml")
  # once the layout it proved is retired, the statement may teach afresh
  layout_retire("anz_1", d)
  expect_identical(layout_learn(other, "anz", .lt_sha(1), d)$action, "created")
})

test_that("versions after a retirement continue, and ids are never reused", {
  d <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d)
  layout_retire("anz_1", d)                                      # v2 retired
  expect_null(layout_match(.lt_sig(), layouts_load(d, include_retired = TRUE)))
  expect_identical(layout_learn(.lt_reading(), "anz", .lt_sha(2), d)$id, "anz_2")
  expect_identical(layout_confirm("ANZ_1", d)$ref, "anz_1@3")    # ids are not case-sensitive
  layout_retire("anz_2", d)
  # a retired id stays taken
  expect_identical(layout_learn(.lt_reading(tokens = "zzz", rel_x = c(0.1, 0.45, 0.66, 0.8, 1)),
                                "anz", .lt_sha(3), d)$id, "anz_3")
})

test_that("bank folders: odd names still give ids the store can list", {
  long <- paste0(strrep("a", 39), " bank")
  s <- .layout_slug(long)
  expect_false(grepl("_$", s))
  expect_identical(.layout_slug("CON"), "con_bank")
  expect_identical(.layout_slug("  "), NA_character_)
  d <- .lt_dir()
  for (b in c(long, "Con", "Bank 2")) {
    a <- layout_learn(.lt_reading(), b, .lt_sha(1), d)
    expect_identical(a$action, "created", info = b)
    expect_true(a$id %in% names(layouts_load(d, b)), info = b)
  }
})

test_that("a text column named from its heading meets the same layout as one called textN", {
  d <- .lt_dir()
  r <- .lt_reading(roles = c("date", "description", "type", "debit", "credit", "balance"),
                   rel_x = c(0.05, 0.4, 0.55, 0.72, 0.85, 1))
  layout_learn(r, "anz", .lt_sha(1), d)
  s <- .lt_sig(roles = c("date", "description", "text1", "debit", "credit", "balance"),
               rel_x = c(0.05, 0.4, 0.55, 0.72, 0.85, 1))
  expect_identical(layout_match(s, layouts_load(d))$id, "anz_1")
  # where "type" carries the sign it is a role of its own
  t1 <- .lt_sig(roles = c("date", "description", "type", "amount"), money_style = "type_dc", rel_x = c(0.25, 0.5, 0.75, 1))
  t2 <- modifyList(t1, list(roles = c("date", "description", "text1", "amount")))
  L <- list(list(layout = list(id = "x_1", status = "proven", version = 1L, signature = t1)))
  expect_identical(layout_match(t1, L)$id, "x_1")
  expect_null(layout_match(t2, L))
})

test_that("a CSV or Excel reading (columns at the top, no table:) is learned and kept", {
  d <- .lt_dir()
  sig <- .lt_sig(kind = "delimited", roles = c("date", "description", "amount", "balance"),
                 money_style = "signed", tokens = c("amount", "balance", "date", "memo"),
                 rel_x = c(0.25, 0.5, 0.75, 1))
  tpl <- list(id = "auto_x", format = "delimited", columns = list(date = list(source = "Date"),
              amount = list(source = "Amount")), amount_sign = "signed", signature = sig)
  rd <- list(outcome = "proven", template = tpl, proof = list(kind = "chain", derived = 0L))
  expect_identical(layout_learn(rd, "kiwibank", .lt_sha(1), d)$action, "created")
  ly <- layouts_load(d)[["kiwibank_1"]]
  expect_identical(ly$columns$date$source, "Date")
  expect_identical(layout_learn(rd, "kiwibank", .lt_sha(2), d)$action, "evidence_added")
  tpl$columns$amount$source <- "Value"
  expect_true(layout_correct("kiwibank_1", tpl, d)$ok)
  expect_identical(layouts_load(d)[["kiwibank_1"]]$columns$amount$source, "Value")
})

test_that("the state id does not depend on the order files were written", {
  a <- .lt_dir(); b <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), a)
  layout_learn(.lt_reading(), "asb", .lt_sha(2), a)
  # the same files, copied in the other order
  for (bk in c("asb", "anz")) file.copy(file.path(a, bk), b, recursive = TRUE)
  expect_identical(layouts_state_id(b), layouts_state_id(a))
  # Windows line endings: the same id
  f <- list.files(b, recursive = TRUE, full.names = TRUE)[1]
  txt <- readLines(f); con <- file(f, "wb"); writeBin(charToRaw(paste0(paste(txt, collapse = "\r\n"), "\r\n")), con); close(con)
  expect_identical(layouts_state_id(b), layouts_state_id(a))
  # any change to any file: a new id
  cat("# touched\n", file = f, append = TRUE)
  expect_false(identical(layouts_state_id(b), layouts_state_id(a)))
})

# N217: "proven after 3 proven statements (from at least 2 different accounts)"
# (spec section 6). A year of one account's statements proves only that account's
# print. The accounts are told apart by short salted marks kept in the layout file;
# an account number is never written anywhere.
test_that("a layout is proven only by statements of two different accounts, and keeps no number", {
  d <- .lt_dir()
  one <- c(.lt_acct(1), "38-9000-7654321-00")          # the holder's number and a payee's
  expect_identical(layout_learn(.lt_reading(), "anz", .lt_sha(1), d, accounts = one)$action, "created")
  expect_identical(layout_learn(.lt_reading(), "anz", .lt_sha(2), d, accounts = .lt_acct(1))$action, "evidence_added")
  # the same account printed with a three-digit suffix: still one account
  a3 <- layout_learn(.lt_reading(), "anz", .lt_sha(3), d, accounts = "12-3456-1000001-000")
  expect_identical(a3$action, "evidence_added")
  expect_identical(a3$status, "provisional")
  expect_match(a3$why, "all of its statements are from one account")
  # a statement on which no account number was found tells nothing about accounts
  a4 <- layout_learn(.lt_reading(), "anz", .lt_sha(4), d)
  expect_identical(a4$status, "provisional")
  # another account that only shares a payee with the first counts as the same one:
  # the safe way to be wrong, since the layout just waits for one more account
  a5 <- layout_learn(.lt_reading(), "anz", .lt_sha(5), d, accounts = c(.lt_acct(9), "38-9000-7654321-00"))
  expect_identical(a5$status, "provisional")
  # a statement of an account with nothing in common: proven
  a6 <- layout_learn(.lt_reading(), "anz", .lt_sha(6), d, accounts = .lt_acct(2))
  expect_identical(a6$action, "promoted")
  expect_match(a6$why, "6 different statements from 2 different accounts", fixed = TRUE)

  # what is on disk: marks and a salt, never a number (nor its long middle part)
  txt <- paste(unlist(lapply(list.files(d, recursive = TRUE, full.names = TRUE), readLines)), collapse = "\n")
  for (n in c("1000001", "1000002", "1000009", "7654321", "12-3456", "38-9000"))
    expect_false(grepl(n, txt, fixed = TRUE), info = n)
  acc <- layouts_load(d)[["anz_1"]]$layout$accounts
  expect_match(acc$salt, "^[0-9a-f]{16}$")
  expect_length(acc$groups, 2L)
  expect_true(all(grepl("^[0-9a-f]{4}( [0-9a-f]{4})*$", acc$groups)))
  # the marks are the layout's own: the same account gets other marks in another layout
  d2 <- .lt_dir()
  layout_learn(.lt_reading(), "anz", .lt_sha(1), d2, accounts = .lt_acct(2))
  other <- layouts_load(d2)[["anz_1"]]$layout$accounts
  expect_false(identical(other$salt, acc$salt))
  expect_identical(.layout_marks(.lt_acct(2), other$salt), other$groups)

  # admin changes keep what was counted
  layout_rename("anz_1", "Everyday", d)
  layout_retire("anz_1", d)
  expect_identical(layouts_load(d, include_retired = TRUE)[["anz_1"]]$layout$accounts, acc)
})

test_that("one account's statements need an admin's confirm; a layout from before accounts were counted catches up", {
  d <- .lt_dir()
  for (i in 1:4) layout_learn(.lt_reading(), "anz", .lt_sha(i), d, accounts = .lt_acct(1))
  expect_identical(layouts_load(d)[["anz_1"]]$layout$status, "provisional")
  expect_true(layout_confirm("anz_1", d)$changed)                  # "or an admin confirm"
  expect_identical(layouts_load(d)[["anz_1"]]$layout$status, "proven")
  # a provisional layout learned with no account numbers (as every one was before
  # accounts were counted): its old proofs count as statements, not as accounts
  d2 <- .lt_dir()
  for (i in 1:3) layout_learn(.lt_reading(), "anz", .lt_sha(i), d2)
  ly <- layouts_load(d2)[["anz_1"]]
  expect_identical(ly$layout$status, "provisional")
  expect_null(ly$layout$accounts)
  expect_match(layout_learn(.lt_reading(), "anz", .lt_sha(4), d2, accounts = .lt_acct(1))$why,
               "from one account", fixed = TRUE)
  expect_identical(layout_learn(.lt_reading(), "anz", .lt_sha(5), d2, accounts = .lt_acct(2))$action, "promoted")
})

test_that("layouts.R and tracking.R are ASCII-only", {
  for (f in c("R/layouts.R", "R/tracking.R")) {
    p <- fixture(f)
    skip_if_not(file.exists(p))
    bytes <- readBin(p, "raw", file.info(p)$size)
    expect_false(any(bytes > as.raw(127)), info = f)
  }
})
