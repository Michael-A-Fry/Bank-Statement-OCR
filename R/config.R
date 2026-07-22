# config.R -- ONE place for all deployment settings. load_config() reads
# config/config.yaml (kept out of any distributed copy; copy config/config.example.yaml
# to create it) and deep-merges it over the built-in defaults, so a partial or
# absent file still yields a complete, valid config. The admin password also accepts
# an environment-variable override (BSO_ADMIN_PASSWORD) for sites that would rather
# not keep it in a file; the file is the default home otherwise.

# .config_defaults() -- every setting the app/engine reads, with safe defaults.
.config_defaults <- function() list(
  app = list(
    title          = "Statement Studio",
    admin_password = "changeme",          # the Admin-tab barrier (a simple gate)
    shiny_url      = "http://localhost:8100", # the URL Qlik's "Convert a statement" tile opens
    port           = 8100L,
    # Convert offers proven (curated) templates by default; a tick-box brings in
    # user-created ones (with a "not guaranteed tested" warning). Flip this to start
    # with that box ticked if your team mostly uses templates they built themselves.
    user_templates_default = FALSE
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
  )
)

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
  existing <- if (!is.null(path) && file.exists(path))
    tryCatch(yaml::read_yaml(path), error = function(e) list()) else list()
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
clear_config_cache <- function() rm(list = ls(.CONFIG_CACHE), envir = .CONFIG_CACHE)

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
      fromfile <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
      if (is.list(fromfile)) c0 <- .deep_merge(c0, fromfile)
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
