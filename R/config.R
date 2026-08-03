# config.R -- ONE place for all deployment settings. load_config() reads
# config/config.yaml (kept out of any distributed copy; copy config/config.example.yaml
# to create it) and deep-merges it over the built-in defaults, so a partial or
# absent file still yields a complete, valid config. The admin password also accepts
# an environment-variable override (BSO_ADMIN_PASSWORD) for sites that would rather
# not keep it in a file; the file is the default home otherwise.

# .DEFAULT_ADMIN_PASSWORD -- the PLACEHOLDER shipped in the example config. It is
# not a password, it is a "you have not set one yet" marker: while it is still in
# force the app REFUSES to serve the Admin tab at all (see admin_password_is_default
# below and the gate in app.R). Kept as a named constant so the default, the
# example file and the refusal can never drift apart.
.DEFAULT_ADMIN_PASSWORD <- "changeme"

# .config_defaults() -- every setting the app/engine reads, with safe defaults.
.config_defaults <- function() list(
  app = list(
    title          = "Statement Studio",
    admin_password = .DEFAULT_ADMIN_PASSWORD,  # placeholder -> Admin stays CLOSED
    shiny_url      = "http://localhost:8100", # the URL Qlik's "Convert a statement" tile opens
    port           = 8100L,
    # Biggest statement a user may upload, in MB. Shiny's own default is 5 MB, which
    # rejects the scanned PDFs this tool exists to read, so we set our own.
    max_upload_mb  = 200,
    # Whether Convert's "Include user-created templates" box starts TICKED.
    # TRUE, because the product's promise is "build a template once and that bank
    # converts automatically from then on" -- with this off, a template Beth builds
    # is excluded from auto-detection and the promise is false. Governance is NOT
    # served by this switch: what reaches Qlik is gated on template ORIGIN in
    # R/feed.R (feed.allowed_template_origins), which is unaffected by it. Set it
    # false only if your team wants to opt in to its own templates each time.
    user_templates_default = TRUE
  ),
  paths = list(
    templates      = "templates",         # PROVEN / curated templates
    user_templates = "templates_user",    # analyst drafts (Shiny only, NEVER Qlik)
    fields         = "fields_templates",
    user_fields    = "fields_templates_user",
    dictionary     = "dictionaries/labels.yaml",
    lexicon        = "dictionaries/lexicon.yaml",  # engine recognition vocabularies
    uploads        = "uploads",
    requests       = "requests",
    logs           = "logs"
  ),
  feed = list(
    # The analytics feed Qlik loads for dashboards. Accountants convert in the Shiny
    # app; each result is written here (gated) as a side-effect, and a Qlik folder
    # connection + scheduled reload turns it into org-wide dashboards. Only
    # reconciled conversions from PROVEN (curated) templates reach the dashboard
    # table -- the governance gate -- so unvetted output never becomes org data.
    enabled                  = TRUE,       # write the feed on each conversion
    feed_dir                 = "feed",
    require_status_ok        = TRUE,       # only clean conversions (status == ok)
    # high | medium | any. 'medium' (default) accepts every CLEAN conversion; 'high'
    # accepts ONLY balance-proven ones (opening + txns = printed closing) -- stricter,
    # but a clean statement with no running balance is 'medium' and would be withheld.
    min_trust                = "medium",
    allowed_template_origins = list("default"),  # 'default' = curated/proven; add 'user' to include drafts
    template_allowlist       = list(),     # optional: restrict to specific template ids
    include_review_feed      = TRUE        # also write withheld runs to feed/review (separate table)
  ),
  metadata = list(
    # LOCAL-ONLY structural + quality capture about every conversion -- the raw
    # material for future on-box analysis / a local ML assist. Written to
    # logs/metadata/<run_id>.json (one file per run, never a shared append), it
    # NEVER leaves this machine and NEVER enters the governed Qlik feed. No raw
    # statement CONTENT is stored -- only structure, counts and quality signals;
    # any account number is stored ONLY as a hash. See docs/context/metadata-capture.md
    # for the per-level PII notes.
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
    # Every converted statement is copied byte-for-byte into uploads/<id>/ so a
    # format that failed can be picked up and fixed. Those copies are REAL client
    # statements, so they must not sit there forever by accident. After this many
    # days the tidy-up (purge_uploads, R/retention.R) deletes the copied FILE and
    # keeps the small record.json, so the audit of "this was uploaded, this is what
    # happened" survives while the client data does not. 0 (or a negative number)
    # means keep the copies indefinitely -- a deliberate choice, and the Convert
    # page then says so out loud.
    uploads_keep_days = 90
  )
)

# admin_password_is_default(cfg) -- TRUE while the shipped placeholder (or a blank)
# is still the admin password. The app uses this to REFUSE the Admin tab rather
# than serve template deletion, the shared dictionary and the analytics-feed
# settings behind a password that is printed in the example file and the docs.
admin_password_is_default <- function(cfg = load_config()) {
  pw <- trimws(as.character(cfg$app$admin_password %||% .DEFAULT_ADMIN_PASSWORD)[1])
  is.na(pw) || !nzchar(pw) || identical(pw, .DEFAULT_ADMIN_PASSWORD)
}

# config_error(cfg) -- the YAML parse failure recorded by load_config(), or NULL.
# A broken config.yaml silently reverting to built-ins used to be invisible; one
# stray tab restored the placeholder admin password and redirected the Qlik feed
# with nothing said. Callers surface this (startup warning + Admin banner).
config_error <- function(cfg) attr(cfg, "config_error", exact = TRUE)

# ---- yes/no settings that decide how much governance is in force ------------
# Every one of these is read with isTRUE(), so ANYTHING that is not a real logical
# reads as FALSE. YAML makes that easy to trip over: `true`, `yes`, `on` and `True`
# all parse to TRUE, but `'true'`, `"yes"` and `1` parse to a string or a number --
# and quoting a value is the most ordinary thing a person editing YAML in Notepad
# does. The result was silent and one-directional:
#
#   feed.require_status_ok: 'true'  -> the status gate switched OFF, so conversions
#                                      the tool itself flags as needing review were
#                                      written to feed/transactions marked accepted
#   feed.enabled: 'true'            -> the feed switched OFF, and the dashboards
#                                      simply stopped gaining data
#
# Neither said anything, and both were chosen by a quote character. So the words a
# person would reasonably write are accepted, and anything unrecognisable keeps the
# BUILT-IN DEFAULT (never the weaker reading) and is reported through config_error,
# which the startup warning and the Admin banner already shout about.
.FLAG_SETTINGS <- list(
  c("app", "user_templates_default"),
  c("feed", "enabled"), c("feed", "require_status_ok"),
  c("feed", "include_review_feed"),
  c("metadata", "retain_forever"))
.FLAG_TRUE  <- c("true", "yes", "on", "y", "t", "1")
.FLAG_FALSE <- c("false", "no", "off", "n", "f", "0")

# .as_flag(v) -- v as TRUE/FALSE, or NA when it is not a yes/no at all.
.as_flag <- function(v) {
  if (is.logical(v) && length(v) == 1L && !is.na(v)) return(v)
  if (is.null(v) || length(v) != 1L) return(NA)
  w <- tolower(trimws(as.character(v)[1]))
  if (is.na(w)) return(NA)
  if (w %in% .FLAG_TRUE) return(TRUE)
  if (w %in% .FLAG_FALSE) return(FALSE)
  NA
}

# .coerce_flags(cfg, defaults) -> cfg with every .FLAG_SETTINGS value a real
# logical, carrying attr "flag_error" naming anything that could not be read.
#
# It also repairs a SECTION of the wrong shape (`feed: 3`, `app: hello` -- a
# mis-indented line is enough to produce one). That replaced the whole settings
# block with a bare value, and the first `cfg$app$admin_password` then died with
# "$ operator is invalid for atomic vectors" -- the app simply would not start, on
# go-live morning, over one space. Defaults go back in and the banner says which
# section, which is the same fail-loud-but-keep-running contract as an unparseable
# file above.
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
  if (length(bad)) attr(cfg, "flag_error") <- paste(bad, collapse = "; ")
  cfg
}

# .deep_merge(base, over) -- override wins; sub-lists merge key-by-key, scalars
# replace. A NULL override leaves the base untouched (so a blank YAML key = default).
.deep_merge <- function(base, over) {
  if (is.null(over)) return(base)
  if (!is.list(base) || !is.list(over)) return(over)
  for (k in names(over)) base[[k]] <- .deep_merge(base[[k]], over[[k]])
  base
}

# save_metadata_config(level, capture, path) -- persist ONLY the metadata block to
# config.yaml (merging over whatever is already there), so the Admin toggle for the
# local capture survives a restart without disturbing the rest of the file. Returns
# TRUE on success. retain_forever stays TRUE -- metadata is never rolled up.
save_metadata_config <- function(level, capture, path = .config_path()) {
  # An UNREADABLE existing file must never be treated as an empty one: merging
  # onto list() and writing would silently delete every other setting in it (the
  # admin password, the Qlik feed_dir, the paths) to save one toggle. Refuse and
  # let the caller say so -- the file is still there to be fixed by hand.
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

# .config_path() -- BSO_CONFIG env wins; else config/config.yaml next to the app.
.config_path <- function() {
  p <- Sys.getenv("BSO_CONFIG", "")
  if (nzchar(p)) p else file.path("config", "config.yaml")
}

# load_config is called MANY times per conversion (once per lex() via .lexicon_path,
# plus convert/feed/metadata), and each call rebuilt the defaults + re-parsed
# config.yaml. Cache the file-merged config keyed by path + mtime + size, so it
# re-reads ONLY when the file actually changes (an admin save or a hand-edit) --
# self-invalidating, no writer needs to remember to clear it. The env-secret
# override is applied fresh on every call, never cached, so it can't go stale.
.CONFIG_CACHE <- new.env(parent = emptyenv())

# load_config(path) -> the complete, merged config list.
load_config <- function(path = .config_path(), refresh = FALSE) {
  fi  <- if (!is.null(path) && file.exists(path)) file.info(path) else NULL
  key <- paste(path %||% "<none>",
               if (is.null(fi)) "-" else paste0(as.numeric(fi$mtime), "|", fi$size))
  cfg <- if (!refresh && exists(key, envir = .CONFIG_CACHE, inherits = FALSE)) {
    get(key, envir = .CONFIG_CACHE, inherits = FALSE)
  } else {
    c0 <- .config_defaults()
    if (!is.null(fi)) {
      # A config.yaml that does not parse is NOT the same as no config.yaml. It
      # silently reverted the admin password to the shipped placeholder and the
      # Qlik feed_dir to "feed" -- a weaker posture, chosen by a stray tab, with
      # nothing said. Defaults are still used (the app must start), but the failure
      # is RECORDED on the result so every caller can shout about it.
      fromfile <- tryCatch(yaml::read_yaml(path), error = function(e) e)
      if (inherits(fromfile, "error")) {
        attr(c0, "config_error") <- sprintf("%s could not be read: %s", path,
                                            conditionMessage(fromfile))
      } else if (is.list(fromfile)) {
        c0 <- .coerce_flags(.deep_merge(c0, fromfile))
        fe <- attr(c0, "flag_error", exact = TRUE)
        if (!is.null(fe)) {
          attr(c0, "flag_error") <- NULL
          attr(c0, "config_error") <- sprintf(
            "%s could not be used in full, so the built-in default is in force for: %s",
            path, fe)
        }
      } else if (!is.null(fromfile)) {
        # An empty file (or comments only) parses to NULL and legitimately means
        # "all defaults" -- silent. Anything else that is not a settings map (a
        # bare string, a list of values) is a mistake worth saying out loud.
        attr(c0, "config_error") <- sprintf(
          "%s is not a settings file (it holds no name: value settings)", path)
      }
    }
    if (length(ls(.CONFIG_CACHE)) >= 8L) rm(list = ls(.CONFIG_CACHE), envir = .CONFIG_CACHE)
    assign(key, c0, envir = .CONFIG_CACHE)
    c0
  }
  # Env override for the one secret, so a site can keep it out of the file entirely.
  envpw <- Sys.getenv("BSO_ADMIN_PASSWORD", "")
  if (nzchar(envpw)) cfg$app$admin_password <- envpw
  cfg
}
