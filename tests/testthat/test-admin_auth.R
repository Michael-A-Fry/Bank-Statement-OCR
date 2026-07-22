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
