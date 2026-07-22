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

test_that("the committed example config is valid YAML and parses", {
  ex <- fixture("config/config.example.yaml")
  skip_if_not(file.exists(ex))
  cfg <- load_config(ex)
  expect_equal(cfg$paths$templates, "templates")
  expect_equal(cfg$app$port, 8100)
})
