# config.R -- ONE place for all deployment settings. load_config() reads
# config/config.yaml (git-ignored on a deployment; copy config/config.example.yaml
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

# .config_path() -- BSO_CONFIG env wins; else config/config.yaml next to the app.
.config_path <- function() {
  p <- Sys.getenv("BSO_CONFIG", "")
  if (nzchar(p)) p else file.path("config", "config.yaml")
}

# load_config(path) -> the complete, merged config list.
load_config <- function(path = .config_path()) {
  cfg <- .config_defaults()
  if (!is.null(path) && file.exists(path)) {
    fromfile <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
    if (is.list(fromfile)) cfg <- .deep_merge(cfg, fromfile)
  }
  # Env override for the one secret, so a site can keep it out of the file entirely.
  envpw <- Sys.getenv("BSO_ADMIN_PASSWORD", "")
  if (nzchar(envpw)) cfg$app$admin_password <- envpw
  cfg
}
