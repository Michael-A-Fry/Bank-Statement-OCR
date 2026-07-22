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
    shiny_url      = "http://localhost:8100", # where Qlik's "no template yet" link points
    port           = 8100L
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
  qlik = list(
    # Qlik converts with PROVEN templates only: it reads paths$templates and NEVER
    # paths$user_templates. On a bank it has no proven template for, it points the
    # user at the Shiny app (app$shiny_url) instead of building one.
    proven_templates_dir = NULL,          # NULL -> use paths$templates
    queue_unsupported    = TRUE           # also file a miss into the Shiny pickup queue
  ),
  feed = list(                             # Mode A batch feed into Qlik dashboards
    feed_dir                 = "feed",
    require_status_ok        = TRUE,
    min_trust                = "high",     # high | medium | any
    allowed_template_origins = list("default"),
    template_allowlist       = list(),
    include_review_feed      = TRUE
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
  # Resolve the Qlik proven-templates default (blank in the file -> the curated dir).
  if (is.null(cfg$qlik$proven_templates_dir) || !nzchar(cfg$qlik$proven_templates_dir %||% ""))
    cfg$qlik$proven_templates_dir <- cfg$paths$templates
  cfg
}
