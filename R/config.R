# config.R -- ONE place for all deployment settings, deep-merged over the built-in defaults so a
# partial or absent file still yields a complete, valid config. BSO_ADMIN_PASSWORD overrides.

# The PLACEHOLDER shipped in the example config. While it is in force the app REFUSES Admin.
.DEFAULT_ADMIN_PASSWORD <- "changeme"

.config_defaults <- function() list(
  app = list(
    title          = "Statement Studio",
    admin_password = .DEFAULT_ADMIN_PASSWORD,  # placeholder -> Admin stays CLOSED
    shiny_url      = "http://localhost:8100", # the URL Qlik's "Convert a statement" tile opens
    port           = 8100L,
    max_upload_mb  = 200,
    # TRUE, because the promise is "build a template once and that bank converts automatically".
    # Governance is not served by this switch: what reaches Qlik is gated on template ORIGIN.
    user_templates_default = TRUE
  ),
  # EVERY TEMPLATE LIVES UNDER templates/, one folder per kind. The separation is load bearing: curated
  # vs user is the Qlik governance gate, and one folder per mode stops a report template joining
  # statement detection. A folder name is the template's `mode:`.
  paths = list(
    templates      = "templates/statements",       # PROVEN / curated statements
    user_templates = "templates/statements_user",  # analyst drafts (Shiny only, NEVER Qlik)
    fields         = "templates/fields",
    user_fields    = "templates/fields_user",
    docs           = "templates/documents",
    user_docs      = "templates/documents_user",
    dictionary     = "dictionaries/labels.yaml",
    lexicon        = "dictionaries/lexicon.yaml",  # engine recognition vocabularies
    uploads        = "uploads",
    requests       = "requests",
    logs           = "logs"
  ),
  feed = list(
    # The analytics feed Qlik loads. Only reconciled conversions from PROVEN (curated) templates
    # reach the dashboard table, so unvetted output never becomes org data.
    enabled                  = TRUE,       # write the feed on each conversion
    feed_dir                 = "feed",
    require_status_ok        = TRUE,       # only clean conversions (status == ok)
    min_trust                = "medium",
    allowed_template_origins = list("default"),  # 'default' = curated/proven; add 'user' to include drafts
    template_allowlist       = list(),     # optional: restrict to specific template ids
    include_review_feed      = TRUE,       # also write withheld runs to feed/review (separate table)
    # One CSV per statement is written and nothing removed one, while Qlik loads the folder with a
    # wildcard, so at 50 conversions a day the reload walks 18,000 files after a year. 0 or less means
    # never archive.
    keep_days                = 400
  ),
  metadata = list(
    # Local-only structural and quality capture. It NEVER leaves this machine and NEVER enters the
    # governed feed: no raw statement CONTENT, and any account number stored as a hash.
    level    = "full",          # off | standard | full  -- how much detail to capture
    capture  = list(            # per-category switches (each applies within its level)
      layout         = TRUE,    # layout signature, format, column/page shape
      parse_quality  = TRUE,    # row/flag/coverage/fill stats, misses, value shapes
      detection      = TRUE,    # scores, margin, candidates, eligibility
      reconciliation = TRUE,    # KPI outcomes, trust, balance anchors, discontinuities
      multi_statement = TRUE,   # #statements / #periods / #accounts / boundary signals
      novelty        = TRUE,    # unmapped columns + unrecognised tokens (ML-feedback signal)
      template_hints = TRUE,    # per-column profiles + suggested mapping (draft-a-template signal)
      ocr            = TRUE,     # OCR pages + confidence stats
      redaction      = TRUE      # redaction counts + scan completeness
    ),
    retain_forever = TRUE       # exempt metadata from log rollup (never archived / deleted)
  ),
  retention = list(
    # Every converted statement is copied byte-for-byte into uploads/<id>/. Those are REAL client
    # statements, so purge_uploads() deletes the copied FILE and keeps record.json.
    uploads_keep_days = 90
  )
)

# admin_password_is_default() is TRUE while the shipped placeholder is in force, and the app uses it
# to REFUSE Admin. .modernise_template_paths() rewrites a settings file naming the OLD folders: the
# file WINS over the defaults, so such a config points at an empty folder and every template the team
# built disappears with nothing said.
.LEGACY_TEMPLATE_PATHS <- list(
  templates      = c("templates",             "templates/statements"),
  user_templates = c("templates_user",        "templates/statements_user"),
  fields         = c("fields_templates",      "templates/fields"),
  user_fields    = c("fields_templates_user", "templates/fields_user"),
  docs           = c("doc_templates",         "templates/documents"),
  user_docs      = c("doc_templates_user",    "templates/documents_user")
)
.modernise_template_paths <- function(cfg) {
  for (k in names(.LEGACY_TEMPLATE_PATHS)) {
    v <- cfg$paths[[k]]
    if (!length(v) || !is.character(v)) next
    old <- .LEGACY_TEMPLATE_PATHS[[k]][1]
    if (identical(gsub("\\\\", "/", trimws(v[1])), old))
      cfg$paths[[k]] <- .LEGACY_TEMPLATE_PATHS[[k]][2]
  }
  cfg
}

admin_password_is_default <- function(cfg = load_config()) {
  pw <- trimws(as.character(cfg$app$admin_password %||% .DEFAULT_ADMIN_PASSWORD)[1])
  is.na(pw) || !nzchar(pw) || identical(pw, .DEFAULT_ADMIN_PASSWORD)
}

# The YAML parse failure recorded by load_config(), or NULL. A broken config.yaml silently reverting
# to built-ins was invisible: one stray tab restored the placeholder password and redirected the feed.
config_error <- function(cfg) attr(cfg, "config_error", exact = TRUE)

# Yes/no settings deciding how much governance is in force, all read with isTRUE(). YAML makes this
# easy to trip over - `true` is TRUE but `'true'` is a string, and quoting feed.require_status_ok
# switched the status gate OFF. Anything unrecognisable keeps the BUILT-IN DEFAULT, never the weaker
# reading.
.FLAG_SETTINGS <- list(
  c("app", "user_templates_default"),
  c("feed", "enabled"), c("feed", "require_status_ok"),
  c("feed", "include_review_feed"),
  c("metadata", "retain_forever"))
.FLAG_TRUE  <- c("true", "yes", "on", "y", "t", "1")
.FLAG_FALSE <- c("false", "no", "off", "n", "f", "0")

.as_flag <- function(v) {
  if (is.logical(v) && length(v) == 1L && !is.na(v)) return(v)
  if (is.null(v) || length(v) != 1L) return(NA)
  w <- tolower(trimws(as.character(v)[1]))
  if (is.na(w)) return(NA)
  if (w %in% .FLAG_TRUE) return(TRUE)
  if (w %in% .FLAG_FALSE) return(FALSE)
  NA
}

# Returns cfg with every flag a real logical and every enum an allowed word, carrying attr
# "flag_error". It also repairs a SECTION of the wrong shape - one mis-indented line replaced a whole
# settings block with a bare value and the app would not start.
.coerce_flags <- function(cfg, defaults = .config_defaults()) {
  bad <- character(0)
  for (sec in unique(vapply(.FLAG_SETTINGS, `[[`, "", 1L))) {
    if (!is.null(cfg[[sec]]) && !is.list(cfg[[sec]])) {
      cfg[[sec]] <- defaults[[sec]]
      bad <- c(bad, sprintf("the whole '%s:' section (it is not a group of settings)", sec))
    }
  }
  for (k in .FLAG_SETTINGS) {
    if (!is.list(cfg[[k[1]]])) next
    v <- cfg[[k]]
    if (is.null(v) || (is.logical(v) && length(v) == 1L && !is.na(v))) next
    f <- .as_flag(v)
    if (is.na(f)) {
      cfg[[k]] <- defaults[[k]]
      bad <- c(bad, sprintf("%s is '%s', which is not yes or no", paste(k, collapse = "."),
                            paste(as.character(v), collapse = " ")))
    } else cfg[[k]] <- f
  }
  cfg <- .coerce_enums(cfg, defaults)
  bad <- c(bad, attr(cfg, "enum_error", exact = TRUE) %||% character(0))
  attr(cfg, "enum_error") <- NULL
  if (length(bad)) attr(cfg, "flag_error") <- paste(bad, collapse = "; ")
  cfg
}

# Settings that are one of a short list of words. BOTH fail closed and silently - a misspelt min_trust
# withholds every clean medium statement, a mis-cased origin makes every conversion return
# withheld:not_proven. Wrong CASE is simply read; not in the list keeps the default and is reported.
.ENUM_SETTINGS <- list(
  list(key = c("feed", "min_trust"),                values = c("high", "medium", "any")),
  list(key = c("feed", "allowed_template_origins"), values = c("default", "user"), many = TRUE),
  list(key = c("metadata", "level"),                values = c("off", "standard", "full")))

.coerce_enums <- function(cfg, defaults) {
  bad <- character(0)
  for (e in .ENUM_SETTINGS) {
    k <- e$key
    if (!is.list(cfg[[k[1]]])) next
    v <- cfg[[k]]
    if (is.null(v)) next
    w <- tolower(trimws(as.character(unlist(v))))
    if (!length(w) || anyNA(w) || !all(w %in% e$values)) {
      cfg[[k]] <- defaults[[k]]
      bad <- c(bad, sprintf("%s is '%s', which is not %s", paste(k, collapse = "."),
                            paste(as.character(unlist(v)), collapse = " "),
                            .or_list(e$values)))
      next
    }
    cfg[[k]] <- if (isTRUE(e$many) && is.list(v)) as.list(w) else w
  }
  if (length(bad)) attr(cfg, "enum_error") <- bad
  cfg
}

.or_list <- function(x) {
  if (length(x) < 2L) return(paste(x, collapse = ""))
  paste(paste(utils::head(x, -1L), collapse = ", "), "or", utils::tail(x, 1L))
}

.deep_merge <- function(base, over) {
  if (is.null(over)) return(base)
  if (!is.list(base) || !is.list(over)) return(over)
  for (k in names(over)) base[[k]] <- .deep_merge(base[[k]], over[[k]])
  base
}

save_metadata_config <- function(level, capture, path = .config_path()) {
  if (!is.null(path) && file.exists(path)) {
    parsed <- tryCatch(yaml::read_yaml(path), error = function(e) e)
    if (inherits(parsed, "error"))
      return(invisible(structure(FALSE, reason = sprintf(
        "%s could not be read (%s), so it was left untouched -- fix the file first, or saving this setting would wipe every other setting in it",
        path, conditionMessage(parsed)))))
    existing <- parsed
  } else existing <- list()
  if (!is.list(existing)) existing <- list()
  lvl <- if (tolower(level %||% "full") %in% metadata_levels()) tolower(level) else "full"
  existing$metadata <- list(level = lvl, capture = as.list(capture), retain_forever = TRUE)
  ok <- isTRUE(tryCatch({
    if (!is.null(path)) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(existing, path); TRUE
  }, error = function(e) FALSE))
  invisible(ok)
}

.config_path <- function() {
  p <- Sys.getenv("BSO_CONFIG", "")
  if (nzchar(p)) p else file.path("config", "config.yaml")
}

# load_config is called many times per conversion, and each call rebuilt the defaults and re-parsed
# the file. Cached on path + mtime + size. The env-secret override is applied fresh every call.
.CONFIG_CACHE <- new.env(parent = emptyenv())

load_config <- function(path = .config_path(), refresh = FALSE) {
  fi  <- if (!is.null(path) && file.exists(path)) file.info(path) else NULL
  key <- paste(path %||% "<none>",
               if (is.null(fi)) "-" else paste0(as.numeric(fi$mtime), "|", fi$size))
  cfg <- if (!refresh && exists(key, envir = .CONFIG_CACHE, inherits = FALSE)) {
    get(key, envir = .CONFIG_CACHE, inherits = FALSE)
  } else {
    c0 <- .config_defaults()
    if (!is.null(fi)) {
      # A config.yaml that does not parse is NOT the same as no config.yaml: it silently reverted the
      # admin password to the placeholder, a weaker posture chosen by a stray tab. Defaults still let
      # the app start, but the failure is RECORDED.
      fromfile <- tryCatch(yaml::read_yaml(path), error = function(e) e)
      if (inherits(fromfile, "error")) {
        attr(c0, "config_error") <- sprintf("%s could not be read: %s", path,
                                            conditionMessage(fromfile))
      } else if (is.list(fromfile)) {
        c0 <- .modernise_template_paths(.coerce_flags(.deep_merge(c0, fromfile)))
        fe <- attr(c0, "flag_error", exact = TRUE)
        if (!is.null(fe)) {
          attr(c0, "flag_error") <- NULL
          attr(c0, "config_error") <- sprintf(
            "%s could not be used in full, so the built-in default is in force for: %s",
            path, fe)
        }
      } else if (!is.null(fromfile)) {
        attr(c0, "config_error") <- sprintf(
          "%s is not a settings file (it holds no name: value settings)", path)
      }
    }
    if (length(ls(.CONFIG_CACHE)) >= 8L) rm(list = ls(.CONFIG_CACHE), envir = .CONFIG_CACHE)
    assign(key, c0, envir = .CONFIG_CACHE)
    c0
  }
  envpw <- Sys.getenv("BSO_ADMIN_PASSWORD", "")
  if (nzchar(envpw)) cfg$app$admin_password <- envpw
  cfg
}

# The old template layout, moved once. Rules, because this touches irreplaceable files: it MOVES and
# never deletes; an existing destination file is NEVER overwritten; an emptied folder keeps a
# MOVED.txt; and it is idempotent.

.TEMPLATE_LAYOUT_MOVES <- list(
  c("templates_user",        "templates/statements_user"),
  c("templates_seed",        "templates/statements_seed"),
  c("fields_templates",      "templates/fields"),
  c("fields_templates_user", "templates/fields_user"),
  c("doc_templates",         "templates/documents"),
  c("doc_templates_user",    "templates/documents_user")
)

migrate_template_layout <- function(root = ".") {
  said <- character(0)
  .yamls <- function(d) list.files(d, pattern = "\\.ya?ml$", full.names = TRUE)

  .move_files <- function(from, to, what) {
    src <- .yamls(from)
    if (!length(src)) return(invisible(NULL))
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    moved <- 0L; clashed <- character(0)
    for (f in src) {
      dest <- file.path(to, basename(f))
      if (file.exists(dest)) { clashed <- c(clashed, basename(f)); next }
      if (isTRUE(suppressWarnings(file.rename(f, dest)))) moved <- moved + 1L
      else clashed <- c(clashed, basename(f))
    }
    if (moved)
      said <<- c(said, sprintf("moved %d %s%s from %s to %s", moved, what,
                               if (moved == 1L) "" else "s", .rel(from), .rel(to)))
    if (length(clashed))
      said <<- c(said, sprintf(
        "LEFT IN PLACE in %s (a file of the same name is already in %s): %s",
        .rel(from), .rel(to), paste(clashed, collapse = ", ")))
    invisible(NULL)
  }

  # Relative to the app folder, the way the person reading logs\\startup.log sees it. Plain prefix
  # removal, not a regex - root is a real path and can hold anything a Windows folder name can.
  .rel <- function(p) {
    pre <- paste0(root, "/")
    p <- if (startsWith(p, pre)) substring(p, nchar(pre) + 1L) else p
    paste0(gsub("/", "\\\\", p), "\\")
  }

  .breadcrumb <- function(from, to) {
    if (!dir.exists(from) || length(.yamls(from))) return(invisible(NULL))
    note <- file.path(from, "MOVED.txt")
    if (file.exists(note)) return(invisible(NULL))
    try(writeLines(c(
      "This folder has moved.",
      "",
      paste0("Everything that was here is now in  ", .rel(to)),
      "",
      "Every template now lives under templates\\, one folder per kind, with a",
      "README.md in templates\\ that says which is which. This folder is empty and",
      "can be deleted; it is left behind only so that looking for it finds this",
      "note rather than nothing."), note), silent = TRUE)
    invisible(NULL)
  }

  flat <- file.path(root, "templates")
  if (dir.exists(flat) && length(.yamls(flat)))
    .move_files(flat, file.path(root, "templates", "statements"), "statement template")

  for (m in .TEMPLATE_LAYOUT_MOVES) {
    from <- file.path(root, m[1]); to <- file.path(root, m[2])
    if (!dir.exists(from)) next
    .move_files(from, to, "template")
    .breadcrumb(from, to)
  }
  said
}
