# P1-2: every privileged Admin handler must RE-VERIFY the admin session on the
# server. The Admin controls are hidden until login, but a crafted client message
# can fire any input regardless of what's on screen, so a missing server-side
# check would let an unauthenticated client delete templates, overwrite the label
# dictionary, or archive logs. This is a static guard: it reads app.R and asserts
# the invariant, so a future admin observer added without the gate fails the build.

test_that("every admin action observer re-checks admin_ok() server-side (P1-2)", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  src <- readLines(app, warn = FALSE)
  starts <- grep("observeEvent\\(input\\$adm_", src)
  expect_true(length(starts) > 5)                 # sanity: we found the handlers
  unguarded <- character(0)
  for (i in starts) {
    if (grepl("input\\$adm_login", src[i])) next  # the login handler IS the gate
    window <- paste(src[i:min(i + 4L, length(src))], collapse = " ")
    if (!grepl("req\\(admin_ok\\(\\)\\)", window))
      unguarded <- c(unguarded, trimws(src[i]))
  }
  expect_identical(unguarded, character(0))
})

test_that("admin dashboard data is only loaded for an authenticated session (P1-2)", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  src <- paste(readLines(app, warn = FALSE), collapse = "\n")
  # the auto-loader for adm_data() must be gated, or a non-admin client could pull
  # run logs / feedback by marking a hidden output visible.
  expect_true(grepl("req\\(admin_ok\\(\\)\\);\\s*if \\(is.null\\(adm_data\\(\\)\\)\\) load_admin\\(\\)", src))
})

# The observeEvent guard above was blind to BARE observe() blocks. Those are never
# suspended by visibility, so an ungated one runs for every browser that connects,
# before any password -- which is how the failed-statement filenames were being
# pushed to anyone who could reach the port. Widen the invariant to cover them.
test_that("bare observe() blocks that touch admin data are gated too (#26/#51)", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  src <- readLines(app, warn = FALSE)
  starts <- grep("^\\s*observe\\(\\{\\s*$", src)
  expect_true(length(starts) > 2)                       # sanity: found the blocks
  # An admin-data observe is one whose body reads an admin surface.
  admin_touch <- "adm_|UPLOADS_DIR|inbox_state|read_uploads|LOGDIR"
  # Read the ACTUAL body by balancing braces, not a fixed window. A fixed window
  # cuts both ways: it bled into whatever followed a short observer (flagging a
  # block that touches nothing) and it stopped reading a long one after nine
  # lines (missing admin access below that). Neither is a check worth having.
  .obs_body <- function(i) {
    depth <- 0L; out <- character(0)
    for (k in i:length(src)) {
      out <- c(out, src[k])
      depth <- depth + lengths(regmatches(src[k], gregexpr("\\{", src[k])))[1] -
                       lengths(regmatches(src[k], gregexpr("\\}", src[k])))[1]
      if (k > i && depth <= 0L) break
    }
    paste(out, collapse = " ")
  }
  unguarded <- character(0)
  for (i in starts) {
    body <- .obs_body(i)
    if (!grepl(admin_touch, body)) next
    if (!grepl("req\\(admin_ok\\(\\)\\)", body))
      unguarded <- c(unguarded, sprintf("line %d", i))
  }
  expect_identical(unguarded, character(0))
})

# The gate itself. The shipped password is printed in the example config and the
# docs, so serving Admin behind it is serving Admin behind nothing; a bare
# identical() with no throttling makes a short secret walkable from a script; and
# with no sign-out the first person to log in on a shared machine leaves Admin open
# for whoever sits down next.
test_that("Admin is refused outright while the shipped password is still in force", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  src <- readLines(app, warn = FALSE)
  joined <- paste(src, collapse = "\n")
  # the refusal is decided from the config, once, at startup
  expect_match(joined, "ADMIN_PW_UNSET <- isTRUE\\(admin_password_is_default\\(CONFIG\\)\\)")
  # ...and the login handler checks it BEFORE comparing anything
  i <- grep("observeEvent\\(input\\$adm_login", src)[1]
  expect_false(is.na(i))
  body <- paste(src[i:min(i + 12L, length(src))], collapse = " ")
  expect_match(body, "if \\(ADMIN_PW_UNSET\\)")
  expect_true(regexpr("ADMIN_PW_UNSET", body, fixed = TRUE) <
              regexpr("identical(input$adm_pw", body, fixed = TRUE))
  # the screen says how to set one instead of showing a box that can never open
  expect_match(joined, "BSO_ADMIN_PASSWORD", fixed = TRUE)
  expect_match(joined, "Admin is closed - no admin password has been set", fixed = TRUE)
})

test_that("wrong admin passwords are throttled", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  joined <- paste(readLines(app, warn = FALSE), collapse = "\n")
  expect_match(joined, "adm_fails <- reactiveVal", fixed = TRUE)
  expect_match(joined, "adm_locked_until <- reactiveVal", fixed = TRUE)
  expect_match(joined, "\\.adm_lock_seconds <- function")
  expect_match(joined, "Too many wrong passwords", fixed = TRUE)
})

test_that("there is a way OUT of an admin session", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  src <- readLines(app, warn = FALSE)
  joined <- paste(src, collapse = "\n")
  expect_match(joined, 'actionButton\\("adm_signout"')
  i <- grep("observeEvent\\(input\\$adm_signout", src)[1]
  expect_false(is.na(i))
  body <- paste(src[i:min(i + 5L, length(src))], collapse = " ")
  expect_match(body, "admin_ok\\(FALSE\\)")
  expect_match(body, "adm_data\\(NULL\\)")   # nothing privileged left computed
})

test_that("admin download handlers re-check the session server-side (#37)", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  src <- readLines(app, warn = FALSE)
  starts <- grep("output\\$adm_[A-Za-z_]*\\s*<-\\s*downloadHandler", src)
  expect_true(length(starts) >= 2)
  unguarded <- character(0)
  for (i in starts) {
    window <- paste(src[i:min(i + 8L, length(src))], collapse = " ")
    if (!grepl("req\\(admin_ok\\(\\)\\)", window))
      unguarded <- c(unguarded, trimws(src[i]))
  }
  expect_identical(unguarded, character(0))
})

# ---------------------------------------------------------------------------
# The Admin tab is hidden unless the URL carries ?admin. Nobody converting a
# statement has the password, so an unopenable tab is a reference to Admin on
# every screen and an invitation to try one.
#
# It is NOT the security boundary and must never be allowed to become one. These
# assertions exist to make that impossible to forget: the tab is hidden, AND
# every admin action is still checked server-side, so a hand-typed ?admin reaches
# a login form exactly as before.
test_that("the Admin tab is hidden unless the URL asks for it", {
  src <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  joined <- paste(src, collapse = "\n")
  expect_match(joined, 'hideTab\\("main_tabs", "Admin"\\)')
  expect_match(joined, "parseQueryString\\(session\\$clientData\\$url_search")
  # hiding must be the DEFAULT: shown only when ?admin is present
  i <- grep('hideTab\\("main_tabs", "Admin"\\)', src)
  expect_length(i, 1L)
  expect_match(paste(src[(i - 1):i], collapse = " "), '!\\("admin" %in% names\\(q\\)\\)')
})

test_that("hiding the tab did not become the lock", {
  # The password is the boundary. If someone ever deletes the server-side checks
  # because "the tab is hidden anyway", this fails.
  src <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  expect_gt(length(grep("req\\(admin_ok\\(\\)\\)", src)), 5L)
  expect_gt(length(grep("admin_ok <- reactive|admin_ok\\(\\)", src)), 5L)
})

test_that("a QID must be six letters or numbers", {
  src <- paste(readLines(file.path(engine_root(), "app.R"), warn = FALSE), collapse = "\n")
  expect_match(src, 'QID_PATTERN <- "\\^\\[A-Za-z0-9\\]\\{6\\}\\$"')
  # free text was the old behaviour: a typo or a name recorded against a
  # conversion is a record nobody can follow back to a person
  expect_match(src, "if \\(!grepl\\(QID_PATTERN, v\\)\\)")
  expect_match(src, "cv_qid\\(toupper\\(v\\)\\)", info = "one identity per person in the log")
  for (v in c("AB1234", "abc123", "123456")) expect_true(grepl("^[A-Za-z0-9]{6}$", v))
  for (v in c("AB123", "AB12345", "AB-123", "", "Michael")) expect_false(grepl("^[A-Za-z0-9]{6}$", v))
})
