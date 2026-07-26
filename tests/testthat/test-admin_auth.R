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
  unguarded <- character(0)
  for (i in starts) {
    body <- paste(src[i:min(i + 8L, length(src))], collapse = " ")
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
