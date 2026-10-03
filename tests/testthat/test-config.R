# Central config loader: defaults, deep-merge of a partial file, resolved keys.

test_that("load_config returns complete defaults when no file is present", {
  old <- Sys.getenv("BSO_ADMIN_PASSWORD"); Sys.unsetenv("BSO_ADMIN_PASSWORD")
  on.exit(if (nzchar(old)) Sys.setenv(BSO_ADMIN_PASSWORD = old))
  cfg <- load_config(path = file.path(tempdir(), "definitely_absent.yaml"))
  expect_equal(cfg$paths$layouts, "templates/layouts")
  expect_equal(cfg$paths$tracking, "logs/tracking")
  expect_equal(cfg$auto_reading$spot_check_rate, 0)   # spot checks off by default
  expect_null(cfg$paths$templates)                     # statement templates are gone
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
               "  layouts: D:/learned",
               "auto_reading:",
               "  spot_check_rate: 0.05"), p)
  cfg <- load_config(p)
  expect_equal(cfg$app$admin_password, "s3cret")            # overridden
  expect_equal(cfg$paths$layouts, "D:/learned")             # overridden
  expect_equal(cfg$auto_reading$spot_check_rate, 0.05)      # overridden
  expect_equal(cfg$paths$logs, "logs")                      # default preserved
  expect_equal(cfg$app$title, "Statement Studio")           # default preserved
  expect_true(isTRUE(cfg$feed$enabled))                     # default feed preserved
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
  expect_equal(cfg$paths$layouts, "templates/layouts")
  expect_equal(cfg$auto_reading$spot_check_rate, 0)
  expect_equal(cfg$app$port, 8100)
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
# feed.enabled off stops the dashboards gaining data at all, and nothing said so.
# ---------------------------------------------------------------------------

test_that("yes/no settings survive being quoted, and never fail to the weaker reading", {
  p <- file.path(tempdir(), "cfg_flags.yaml")
  writeLines(c("feed:", "  enabled: 'true'", "  include_review_feed: 1",
               "metadata:", "  retain_forever: \"yes\""), p)
  cfg <- load_config(p, refresh = TRUE)
  for (v in list(cfg$feed$enabled, cfg$feed$include_review_feed, cfg$metadata$retain_forever)) {
    expect_true(is.logical(v))
    expect_true(isTRUE(v))
  }
  expect_null(config_error(cfg))       # these are readable, so nothing to shout about
})

test_that("a real off is still off, in every spelling", {
  p <- file.path(tempdir(), "cfg_flags_off.yaml")
  writeLines(c("feed:", "  enabled: false", "  include_review_feed: 'no'"), p)
  cfg <- load_config(p, refresh = TRUE)
  expect_false(cfg$feed$enabled)
  expect_false(cfg$feed$include_review_feed)
  expect_null(config_error(cfg))
})

test_that("a yes/no setting that is neither keeps the default AND says so", {
  p <- file.path(tempdir(), "cfg_flags_junk.yaml")
  writeLines(c("feed:", "  enabled: maybe"), p)
  cfg <- load_config(p, refresh = TRUE)
  # the built-in default is in force, never a guess
  expect_true(cfg$feed$enabled)
  err <- config_error(cfg)
  expect_false(is.null(err))
  expect_match(err, "feed.enabled")
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
    expect_true(cfg$feed$enabled)                    # back at its default
    expect_match(config_error(cfg) %||% "", "not a group of settings")
  }
})

# ---------------------------------------------------------------------------
# A word setting that is not one of its words keeps the built-in default AND is
# reported, in the banner the startup warning and Admin already shout.
test_that("a misspelt word setting keeps the built-in default AND is reported", {
  p <- tempfile(fileext = ".yaml")
  writeLines(c("metadata:", "  level: ful"), p)
  cfg <- load_config(p)
  expect_identical(cfg$metadata$level, "full")
  err <- config_error(cfg)
  expect_false(is.null(err))
  expect_match(err, "metadata.level", fixed = TRUE)
  # ...and one only in the wrong CASE is simply read, and stays silent
  writeLines(c("metadata:", "  level: Standard"), p)
  cfg2 <- load_config(p, refresh = TRUE)
  expect_identical(cfg2$metadata$level, "standard")
  expect_null(config_error(cfg2))
})

test_that("bad yes/no values and bad word values are reported in the one sentence", {
  p <- tempfile(fileext = ".yaml")
  writeLines(c("feed:", "  enabled: 'sometimes'", "metadata:", "  level: meduim"), p)
  cfg <- load_config(p)
  err <- config_error(cfg)
  expect_match(err, "feed.enabled", fixed = TRUE)
  expect_match(err, "metadata.level", fixed = TRUE)
  expect_true(isTRUE(cfg$feed$enabled))          # both back to the built-in default
  expect_identical(cfg$metadata$level, "full")
})

# ---------------------------------------------------------------------------
# K4: a YAML save must never cost what it replaces: a backup, an atomic write,
# and a way back from one bad save (dictionaries, a person's held fix).
test_that("save_yaml_safely keeps the previous version and never leaves a part file", {
  d <- tempfile("sv_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- file.path(d, "acme.yaml")

  expect_true(isTRUE(save_yaml_safely(list(id = "acme", version = 1L), p)))
  expect_false(file.exists(paste0(p, ".bak")))       # nothing to back up on the first save
  expect_true(isTRUE(save_yaml_safely(list(id = "acme", version = 2L), p)))

  expect_equal(yaml::read_yaml(p)$version, 2L)
  expect_equal(yaml::read_yaml(paste0(p, ".bak"))$version, 1L)   # one save's worth of undo
  expect_equal(length(list.files(d, pattern = "part")), 0L)
})

# The backup and the temp file must be invisible to anything listing "*.yaml", or
# a half-written file could be read as the real one.
test_that("neither the backup nor the temp file can ever be listed as a YAML file", {
  d <- tempfile("sv2_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- file.path(d, "acme.yaml")
  save_yaml_safely(list(id = "acme"), p); save_yaml_safely(list(id = "acme"), p)
  file.create(file.path(d, "acme.yaml.999.part"))
  expect_identical(list.files(d, pattern = "\\.ya?ml$"), "acme.yaml")
})

test_that("a save that cannot be written changes nothing and says why in one sentence", {
  d <- tempfile("sv3_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- file.path(d, "acme.yaml")
  save_yaml_safely(list(id = "acme", version = 1L), p)
  r <- save_yaml_safely(list(id = "acme"), file.path("/proc", "no", "such", "acme.yaml"))
  expect_false(isTRUE(r))
  expect_match(attr(r, "reason"), "^[^A-Z]*[a-z]")        # a sentence, not a code
  expect_false(grepl("Error|error in", attr(r, "reason")))
  expect_equal(yaml::read_yaml(p)$version, 1L)            # the good file is untouched
})
