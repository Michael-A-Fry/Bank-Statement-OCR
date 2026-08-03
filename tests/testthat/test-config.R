# Central config loader: defaults, deep-merge of a partial file, resolved keys.

test_that("load_config returns complete defaults when no file is present", {
  old <- Sys.getenv("BSO_ADMIN_PASSWORD"); Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (nzchar(old)) Sys.setenv(BSO_ADMIN_PASSWORD = old))
  cfg <- load_config(path = file.path(tempdir(), "definitely_absent.yaml"))
  expect_equal(cfg$paths$templates, "templates")
  expect_equal(cfg$paths$user_templates, "templates_user")
  expect_equal(cfg$app$admin_password, "changeme")
  expect_true(isTRUE(cfg$feed$enabled))          # feed on by default
})

test_that("a partial config file deep-merges over the defaults", {
  old <- Sys.getenv("BSO_ADMIN_PASSWORD"); Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (nzchar(old)) Sys.setenv(BSO_ADMIN_PASSWORD = old))
  p <- file.path(tempdir(), "cfg_partial.yaml")
  writeLines(c("app:",
               "  admin_password: s3cret",
               "paths:",
               "  templates: proven_only"), p)
  cfg <- load_config(p)
  expect_equal(cfg$app$admin_password, "s3cret")            # overridden
  expect_equal(cfg$paths$templates, "proven_only")          # overridden
  expect_equal(cfg$paths$logs, "logs")                      # default preserved
  expect_equal(cfg$app$title, "Statement Studio")           # default preserved
  expect_equal(cfg$feed$min_trust, "medium")                # default feed gate preserved
})

test_that("the BSO_ADMIN_PASSWORD env var overrides the file", {
  p <- file.path(tempdir(), "cfg_pw.yaml")
  writeLines(c("app:", "  admin_password: fromfile"), p)
  Sys.setenv(BSO_ADMIN_PASSWORD = "fromenv")
  on.exit(Sys.unsetenv("BSO_ADMIN_PASSWORD"))
  expect_equal(load_config(p)$app$admin_password, "fromenv")
})

test_that("the bundled example config is valid YAML and parses", {
  ex <- fixture("config/config.example.yaml")
  skip_if_not(file.exists(ex))
  cfg <- load_config(ex)
  expect_equal(cfg$paths$templates, "templates")
  expect_equal(cfg$app$port, 8100)
})

# ---------------------------------------------------------------------------
# #18 -- a template built in the app must actually take part in detection.
# The product's central promise ("build it once, that bank converts automatically
# from then on") is FALSE whenever this default is off, because the only opt-in is
# a tick-box buried in a collapsed "It picked the wrong bank?" panel. Governance is
# not what this switch protects: what reaches Qlik is gated on template ORIGIN in
# R/feed.R, which this does not touch.
test_that("templates built in the app are included in detection by default (#18)", {
  old <- Sys.getenv("BSO_ADMIN_PASSWORD"); Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (nzchar(old)) Sys.setenv(BSO_ADMIN_PASSWORD = old))
  cfg <- load_config(path = file.path(tempdir(), "definitely_absent.yaml"))
  expect_true(isTRUE(cfg$app$user_templates_default))
  # ...and the Qlik gate is untouched by it: proven templates only, as before.
  expect_identical(unlist(cfg$feed$allowed_template_origins), "default")
})

test_that("the shipped default agrees with what the UI copy promises (#18)", {
  ui <- fixture("ui_content.R")
  skip_if_not(file.exists(ui))
  txt <- paste(readLines(ui, warn = FALSE), collapse = " ")
  promised <- grepl("converts automatically", txt) || grepl("just works", txt)
  cfg <- load_config(path = file.path(tempdir(), "definitely_absent.yaml"))
  if (promised) expect_true(isTRUE(cfg$app$user_templates_default))
  # the example file must not quietly contradict the built-in default either
  ex <- fixture("config/config.example.yaml")
  skip_if_not(file.exists(ex))
  expect_identical(isTRUE(load_config(ex)$app$user_templates_default),
                   isTRUE(cfg$app$user_templates_default))
})

# ---------------------------------------------------------------------------
# ADMIN GATE -- the shipped password is printed in the example file and the docs,
# so it is a "not set yet" marker, not a password. The app must be able to tell.
test_that("the shipped admin password is recognised as 'no password set'", {
  old <- Sys.getenv("BSO_ADMIN_PASSWORD"); Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (nzchar(old)) Sys.setenv(BSO_ADMIN_PASSWORD = old))
  expect_true(admin_password_is_default(load_config(file.path(tempdir(), "definitely_absent.yaml"))))
  expect_true(admin_password_is_default(list(app = list(admin_password = "changeme"))))
  expect_true(admin_password_is_default(list(app = list(admin_password = "  changeme  "))))
  expect_true(admin_password_is_default(list(app = list(admin_password = ""))))
  expect_true(admin_password_is_default(list(app = list())))     # nothing set at all
  expect_false(admin_password_is_default(list(app = list(admin_password = "a real one"))))
  # a fresh install (example config copied verbatim) is therefore CLOSED
  ex <- fixture("config/config.example.yaml")
  skip_if_not(file.exists(ex))
  expect_true(admin_password_is_default(load_config(ex)))
})

test_that("BSO_ADMIN_PASSWORD opens the gate without touching the file", {
  p <- file.path(tempdir(), "cfg_gate.yaml")
  writeLines(c("app:", "  admin_password: changeme"), p)
  Sys.setenv(BSO_ADMIN_PASSWORD = "set-by-the-env")
  on.exit(Sys.unsetenv("BSO_ADMIN_PASSWORD"))
  expect_false(admin_password_is_default(load_config(p)))
})

# ---------------------------------------------------------------------------
# #60 -- a config.yaml that does not parse used to be discarded in silence, taking
# the admin password back to the placeholder and the Qlik feed back to its default
# folder. Never fail into a weaker posture without saying so.
test_that("an unparseable config.yaml is reported, not silently ignored (#60)", {
  old <- Sys.getenv("BSO_ADMIN_PASSWORD"); Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (nzchar(old)) Sys.setenv(BSO_ADMIN_PASSWORD = old))
  p <- file.path(tempdir(), "cfg_broken.yaml")
  writeLines(c("app:", "  admin_password: s3cret", "  feed_dir: [1,"), p)
  cfg <- load_config(p, refresh = TRUE)
  err <- config_error(cfg)
  expect_false(is.null(err))
  expect_match(err, "could not be read")
  # the fall-back really is the WEAKER posture -- which is exactly why it must shout
  expect_equal(cfg$app$admin_password, "changeme")
  expect_true(admin_password_is_default(cfg))     # so the Admin tab refuses: closed
  expect_equal(cfg$feed$feed_dir, "feed")
})

test_that("a good config - or an empty one - reports no error", {
  p <- file.path(tempdir(), "cfg_fine.yaml"); writeLines(c("app:", "  port: 9001"), p)
  expect_null(config_error(load_config(p, refresh = TRUE)))
  e <- file.path(tempdir(), "cfg_comments.yaml"); writeLines("# all defaults, on purpose", e)
  expect_null(config_error(load_config(e, refresh = TRUE)))   # empty is a choice, not a fault
})

test_that("save_metadata_config refuses to overwrite a config.yaml it cannot read (#60)", {
  p <- file.path(tempdir(), "cfg_unreadable.yaml")
  writeLines(c("app:", "  admin_password: s3cret", "  feed_dir: [1,"), p)
  before <- readLines(p, warn = FALSE)
  ok <- save_metadata_config("standard", list(layout = TRUE), p)
  expect_false(isTRUE(ok))
  expect_match(attr(ok, "reason") %||% "", "could not be read")
  # the file is LEFT ALONE. Merging one toggle onto an empty list and writing it
  # back would have deleted the admin password and the feed folder outright.
  expect_identical(readLines(p, warn = FALSE), before)
})

# ---------------------------------------------------------------------------
# #6/#32 -- how long a copy of a real client statement is kept must be a setting,
# not a fact of the code.
test_that("upload retention is a config key with a finite default", {
  cfg <- load_config(path = file.path(tempdir(), "definitely_absent.yaml"))
  expect_equal(cfg$retention$uploads_keep_days, 90)
  p <- file.path(tempdir(), "cfg_ret.yaml")
  writeLines(c("retention:", "  uploads_keep_days: 7"), p)
  expect_equal(load_config(p)$retention$uploads_keep_days, 7)
})

# ---------------------------------------------------------------------------
# Every yes/no setting is read with isTRUE(), so anything that is not a real
# logical reads as FALSE -- and in YAML `'true'`, `"yes"` and `1` are a string or
# a number. Quoting a value is the most ordinary thing a person editing YAML in
# Notepad does, and it silently moved the deployment to the WEAKER reading:
# feed.require_status_ok off publishes unreconciled figures, feed.enabled off
# stops the dashboards gaining data at all. Neither said anything.
# ---------------------------------------------------------------------------

test_that("yes/no settings survive being quoted, and never fail to the weaker reading", {
  p <- file.path(tempdir(), "cfg_flags.yaml")
  writeLines(c("feed:", "  enabled: 'true'", "  require_status_ok: \"yes\"",
               "  include_review_feed: 1", "app:", "  user_templates_default: 'on'"), p)
  cfg <- load_config(p, refresh = TRUE)
  for (v in list(cfg$feed$enabled, cfg$feed$require_status_ok,
                 cfg$feed$include_review_feed, cfg$app$user_templates_default)) {
    expect_true(is.logical(v))
    expect_true(isTRUE(v))
  }
  expect_null(config_error(cfg))       # these are readable, so nothing to shout about
})

test_that("a real off is still off, in every spelling", {
  p <- file.path(tempdir(), "cfg_flags_off.yaml")
  writeLines(c("feed:", "  enabled: false", "  require_status_ok: 'no'",
               "  include_review_feed: 0"), p)
  cfg <- load_config(p, refresh = TRUE)
  expect_false(cfg$feed$enabled)
  expect_false(cfg$feed$require_status_ok)
  expect_false(cfg$feed$include_review_feed)
  expect_null(config_error(cfg))
})

test_that("a yes/no setting that is neither keeps the default AND says so", {
  p <- file.path(tempdir(), "cfg_flags_junk.yaml")
  writeLines(c("feed:", "  require_status_ok: maybe"), p)
  cfg <- load_config(p, refresh = TRUE)
  # the built-in default is TRUE, and the SAFE reading is the one that keeps the
  # gate on -- guessing "off" would publish unreconciled figures on a typo.
  expect_true(cfg$feed$require_status_ok)
  err <- config_error(cfg)
  expect_false(is.null(err))
  expect_match(err, "feed.require_status_ok")
  expect_match(err, "not yes or no")
})

test_that("a settings SECTION of the wrong shape does not stop the app starting", {
  p <- file.path(tempdir(), "cfg_shape.yaml")
  # one mis-indented line is enough to turn a whole block into a bare value. The
  # first cfg$app$admin_password then died with "$ operator is invalid for atomic
  # vectors" -- the app would not start at all, which on go-live morning is the
  # worst outcome available.
  for (body in list("app: hello", "feed: 3", c("app:", "  - 1", "  - 2"))) {
    writeLines(body, p)
    cfg <- expect_no_error(load_config(p, refresh = TRUE))
    expect_true(is.list(cfg$app)); expect_true(is.list(cfg$feed))
    expect_identical(cfg$app$admin_password, .DEFAULT_ADMIN_PASSWORD)
    expect_true(cfg$feed$require_status_ok)          # governance back at its default
    expect_match(config_error(cfg) %||% "", "not a group of settings")
  }
})
