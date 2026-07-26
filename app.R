# app.R -- interactive GUI for the statement conversion engine.
#
# Two jobs, both point-and-click for a non-engineer analyst:
#   1. Convert -- upload a statement, convert it, review the checks, download.
#   2. Add a template -- upload a sample and open the template toolkit: the tool
#      pre-fills what it can detect, you confirm against a live preview and SAVE
#      a new bank template (it writes the YAML for you).
#
# Run locally:  R -e 'shiny::runApp(".", launch.browser = TRUE)'
# (from the repo root, so R/ and templates/ resolve.)

# Force a UTF-8 locale FIRST. On a host whose default locale is C/ASCII
# (ANSI_X3.4-1968), R cannot represent the unicode symbols used throughout the
# UI and renders them as mojibake ("<80><94>"), which makes the whole app look
# broken. Try the common UTF-8 locale names and stop at the first that takes.
suppressWarnings(for (.loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"))
  if (nzchar(Sys.setlocale("LC_CTYPE", .loc))) break)

suppressMessages({
  library(shiny)
  library(DT)
})

# Load the engine (all pure-R modules) into the session.
for (.f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(.f)

# All deployment settings live in ONE place: config/config.yaml (copy it from
# config/config.example.yaml). Any absent key falls back to the built-in default,
# so with no config file the app behaves exactly as before.
CONFIG <- load_config()
# A config.yaml that does not parse used to revert to built-in defaults SILENTLY --
# including the admin password (back to the shipped placeholder) and the Qlik
# feed_dir. The docs tell a non-technical analyst to edit that file in Notepad, so
# one stray tab could quietly weaken the deployment. Say it loudly at startup, and
# again on a banner in Admin (output$adm_cfg_banner) for whoever is looking at the
# screen rather than the console.
CONFIG_ERROR <- config_error(CONFIG)
if (!is.null(CONFIG_ERROR)) {
  warning(sprintf(paste("SETTINGS FILE NOT LOADED: %s\n",
                        "Statement Studio has started on its BUILT-IN DEFAULTS, which include the",
                        "placeholder admin password and the default Qlik feed folder. Fix the file",
                        "and restart."), CONFIG_ERROR), call. = FALSE, immediate. = TRUE)
}
# Admin stays CLOSED while the password is still the shipped placeholder (or blank).
# Read once at startup: the gate must not depend on a per-session value.
ADMIN_PW_UNSET <- isTRUE(admin_password_is_default(CONFIG))
if (ADMIN_PW_UNSET)
  message("Statement Studio: Admin is CLOSED - no admin password is set. ",
          "Set app.admin_password in config/config.yaml or the BSO_ADMIN_PASSWORD ",
          "environment variable, then restart.")
# Shiny's built-in upload ceiling is 5 MB, which REJECTS the input this tool exists
# for: a 300-dpi scan of a year's statement is routinely 10-40 MB, and even the
# sample scan shipped with the repo is 4.4 MB. The file picker refuses it with a bare
# red strip and nothing reaches the engine, so there is no log line either. Raise it
# here (and in scripts/run_app.R, which is how the server actually starts) from one
# config key so a site can tune it without touching code.
MAX_UPLOAD_MB <- suppressWarnings(as.numeric(CONFIG$app$max_upload_mb %||% 200))
# Do templates built HERE take part in detection? A deployment decision, read once
# -- never a tick-box on the Convert page. Whether a colleague's template counts
# is not a question to put to the person converting a statement, and the tick-box
# only ever produced the puzzle "I built this template and it does not work".
# Governance is unaffected: what reaches the dashboards is gated separately, on
# template origin (feed.allowed_template_origins).
USE_USER_TEMPLATES <- isTRUE(CONFIG$app$user_templates_default %||% TRUE)
if (!is.finite(MAX_UPLOAD_MB) || MAX_UPLOAD_MB <= 0) MAX_UPLOAD_MB <- 200
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)
TEMPLATES_DIR      <- CONFIG$paths$templates       # curated, team-maintained (proven) templates
USER_TEMPLATES_DIR <- CONFIG$paths$user_templates  # templates accountants create via guided setup
LOGDIR             <- CONFIG$paths$logs            # run log + feedback log live together, next to the app
UPLOADS_DIR        <- CONFIG$paths$uploads         # every uploaded statement + its lifecycle status (local-only)
REQUESTS_DIR       <- CONFIG$paths$requests        # "none of these fits -- tell our team" raises (local-only)
FIELDS_DIR         <- CONFIG$paths$fields          # curated mode:fields (IRD/form) templates
USER_FIELDS_DIR    <- CONFIG$paths$user_fields     # form templates built in the app
DICT_PATH          <- CONFIG$paths$dictionary      # the shared label dictionary
LEXICON_PATH       <- CONFIG$paths$lexicon %||% file.path("dictionaries", "lexicon.yaml")  # recognition vocabularies
# The bundled specimen statement (public sample, ships with the app) that "Try it
# on a sample" converts, so a brand-new user sees a full result without a file.
SAMPLE_STATEMENT <- file.path("samples", "raw", "tutorial", "sample_everyday_statement.pdf")

# How many days of run/feedback logs to keep before "Tidy up logs" archives them.
# One place, so the button label and both rollup calls can never disagree.
LOG_KEEP_DAYS <- 90L
# How long a COPY of an uploaded statement is kept under uploads/<id>/. Config-driven
# because it is a data-retention decision, not a code decision; the same number
# drives the purge, the Admin button label and the line shown under the file picker,
# so what the user is told and what happens can never disagree.
UPLOADS_KEEP_DAYS <- suppressWarnings(as.numeric(CONFIG$retention$uploads_keep_days %||% 90))
if (!is.finite(UPLOADS_KEEP_DAYS)) UPLOADS_KEEP_DAYS <- 90
UPLOADS_NOTE <- uploads_retention_note(UPLOADS_KEEP_DAYS)
# Startup tidy-up, once per process. Two things nothing else reclaimed:
#   * old copies of client statements under uploads/ (kept forever until now);
#   * per-session scratch folders under the temp dir. R only clears the temp dir
#     when the process exits, and this app is a service that runs for months, so
#     every conversion's outputs AND its copy of the source piled up for the life
#     of the server. Each session now unlinks its own (see run_conversion and
#     onSessionEnded); this sweep is the backstop for a browser that closed
#     abruptly, and it is re-run on each conversion (a fresh process's temp dir is
#     empty, so at startup it usually finds nothing -- that is fine, it is cheap).
safe(purge_uploads(UPLOADS_DIR, keep_days = UPLOADS_KEEP_DAYS))
safe(sweep_temp_dirs(keep_hours = 24))
# read_file_text(p) -- a file's contents as one string ("" if absent). Used by the
# Admin YAML editors to load the dictionary / lexicon into their text boxes.
read_file_text <- function(p) if (file.exists(p)) paste(readLines(p, warn = FALSE), collapse = "\n") else ""

# Plain-English label maps + the preview-column helpers live in ui_labels.R (the
# wording a non-technical user sees). Sourced here -- after the engine, before the
# UI -- so rewording copy is one small, obvious file, never buried in app.R.
source("ui_labels.R")

# About-page + tutorial HTML content lives in ui_content.R (readability).
source("ui_content.R")

# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$title("Statement Studio"),
    # One design system, all inline (the app must work offline / air-gapped: no CDN
    # fonts, scripts or icon packs). Two style tags: this one is layout and shape,
    # the one below carries the colour/size tokens and the elevated surfaces.
    tags$style(HTML("
     /* Layout and shape rules. THE COLOURS AND SIZES LIVE IN ONE PLACE ONLY --
        the single :root block in the second style tag below. There used to be a
        :root here too; being first, every value in it was overridden by that one,
        so editing a colour here changed nothing on screen. If you are changing a
        colour, a radius or a shadow, do it there. */
     body{font-family:'Segoe UI',system-ui,-apple-system,Roboto,'Helvetica Neue',Arial,sans-serif;
          color:var(--ink);font-size:14px}
     h1,h2,h3,h4,h5{font-weight:600;color:var(--ink)}
     h4{font-size:16.5px;margin-top:20px}
     hr{border-top-color:var(--line)}
     .ok{color:var(--ok);font-weight:600}.bad{color:var(--bad);font-weight:600}
     .muted{color:var(--muted)}
     .mono{font-family:Consolas,'Courier New',monospace;white-space:pre-wrap}
     .modal-lg{width:95%;max-width:1240px}
     .modal-content{border-radius:12px}
     /* header */
     .app-header{display:flex;align-items:baseline;flex-wrap:wrap;padding:14px 0 10px}
     .app-header>*{margin-right:12px}
     .app-mark{display:inline-block;width:13px;height:13px;border-radius:4px;background:var(--brand);align-self:center}
     .app-title{font-size:21px;font-weight:700;letter-spacing:.2px}
     .app-tagline{font-size:13px;color:var(--muted)}
     /* tabs: quiet underline style, active = product green */
     .nav-tabs{border-bottom:2px solid var(--line);margin-bottom:4px}
     .nav-tabs>li>a{border:none;border-bottom:3px solid transparent;border-radius:0;
       color:var(--muted);font-weight:600;padding:9px 14px;margin-right:2px}
     .nav-tabs>li>a:hover{background:var(--brand-tint);border:none;
       border-bottom:3px solid var(--brand-line);color:var(--brand)}
     .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{
       border:none;border-bottom:3px solid var(--brand);color:var(--brand);background:transparent}
     /* buttons. NB: Shiny action/download buttons always carry btn-default even
        when another btn-* class is added, so the btn-default skin must exclude
        those or it beats them on focus (a clicked Convert would turn pale). */
     .btn{border-radius:7px;font-weight:600}
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger){
       border-color:#cfd6d2;color:var(--ink)}
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger):hover,
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger):focus{
       background:var(--panel);border-color:#b9c2bd;color:var(--ink)}
     .btn-primary{background:var(--brand);border-color:var(--brand-dark);color:#fff}
     .btn-primary:hover,.btn-primary:focus,.btn-primary:active,.btn-primary:active:focus{
       background:var(--brand-dark);border-color:var(--brand-dark);color:#fff}
     .btn-warning{background:#f7c948;border-color:#dfa92e;color:#4a3200}
     .btn-warning:hover,.btn-warning:focus{background:#eeb63c;border-color:#c99518;color:#402b00}
     .btn-danger{background:#fff;border-color:#e3a4ae;color:var(--bad)}
     .btn-danger:hover,.btn-danger:focus{background:#fdecec;border-color:var(--bad);color:var(--bad)}
     /* panels + forms */
     .well{background:var(--panel);border:1px solid var(--line);border-radius:10px;box-shadow:none}
     .form-control{border-radius:7px;border-color:#cfd6d2;box-shadow:none}
     .form-control:focus{border-color:var(--brand);box-shadow:0 0 0 3px rgba(0,32,91,.15)}
     .help-block{color:var(--muted);font-size:12.5px}
     .progress-bar{background-color:var(--brand)}
     /* tables (DT) */
     table.dataTable{font-size:13px}
     table.dataTable thead th,table.dataTable thead td{
       background-color:var(--panel)!important;border-bottom:2px solid var(--line)!important;font-size:12.5px}
     table.dataTable tbody tr:hover{background:#f3f8f4}
     /* collapsed sections */
     details>summary{cursor:pointer}
     /* downloads box (appears in the sidebar once a conversion produced files) */
     .dl-box{background:var(--brand-tint);border:1px solid var(--brand-line);border-radius:10px;
       padding:10px 12px;margin:10px 0}
     .dl-box .btn{margin:3px 6px 3px 0}
     /* small status chips on the result headline */
     .chip{display:inline-block;padding:2px 10px;border-radius:999px;font-size:12px;font-weight:600;
       margin:2px 6px 0 0;background:#f2f4f3;border:1px solid var(--line);color:#4a555f}
     .chip-warn{background:var(--warn-bg);border-color:var(--warn-line);color:var(--warn-ink)}
     .shiny-notification{border-radius:10px;border:1px solid var(--line);
       box-shadow:0 6px 24px rgba(0,0,0,.14);font-size:13.5px}
     /* About hub: the journey entry - two doors and a quiet third */
     .hub{max-width:1020px}
     .hub-lead{font-size:15.5px;color:#3a4652;max-width:780px;line-height:1.55;margin:4px 0 18px}
     .hub-cards{display:flex;flex-wrap:wrap}
     a.hub-card{flex:1 1 260px;max-width:330px;margin:0 14px 14px 0;padding:16px 18px;
       border:1px solid var(--line);border-radius:12px;background:#fff;display:block;
       color:var(--ink);text-decoration:none}
     a.hub-card:hover,a.hub-card:focus{border-color:var(--brand-line);
       box-shadow:0 4px 14px rgba(0,32,91,.12);text-decoration:none;color:var(--ink)}
     a.hub-card-primary{background:var(--brand-tint);border-color:var(--brand-line)}
     a.hub-card-quiet{background:var(--panel)}
     .hub-card-kicker{font-size:11.5px;font-weight:700;letter-spacing:.6px;
       text-transform:uppercase;color:var(--brand);margin-bottom:4px}
     .hub-card-title{font-size:17px;font-weight:700;margin-bottom:6px}
     .hub-card-body{font-size:13px;color:var(--muted);line-height:1.5;margin-bottom:10px}
     .hub-card-go{font-size:13px;font-weight:700;color:var(--brand)}
    ")),
    # Enter in the Admin password box = click Enter (no mouse trip). The
    # trigger('change') first flushes the debounced text value, so a fast
    # type-then-Enter never submits a stale password.
    tags$script(HTML(
      "$(document).on('keyup', '#adm_pw', function(e){
         if (e.key === 'Enter') { $(this).trigger('change'); $('#adm_login').click(); }
       });")),
    # Loading feedback: a real animation, not just the grey-out. A busy pill shows
    # whenever Shiny is working (convert, X-ray render, any recompute); recalculating
    # outputs dim and float a spinner so a slow plot/table clearly says "loading".
    tags$style(HTML("
     @keyframes ss-spin{to{transform:rotate(360deg)}}
     @keyframes ss-fade{from{opacity:0}to{opacity:1}}
     #ss-busy{position:fixed;right:18px;bottom:18px;z-index:2000;display:none;
       align-items:center;gap:9px;background:#fff;border:1px solid var(--line);
       border-left:4px solid var(--brand);border-radius:10px;padding:9px 14px;
       box-shadow:0 6px 22px rgba(0,0,0,.14);font-size:13px;font-weight:600;color:var(--ink)}
     #ss-busy.on{display:flex;animation:ss-fade .15s ease}
     .ss-ring{width:16px;height:16px;border-radius:50%;border:2.5px solid var(--brand-line);
       border-top-color:var(--brand);animation:ss-spin .7s linear infinite}
     /* dim + spinner over any output that is recalculating */
     .shiny-bound-output.recalculating{opacity:.45;transition:opacity .1s}
     .shiny-plot-output.recalculating{position:relative}
     .shiny-plot-output.recalculating::after{content:'';position:absolute;top:50%;left:50%;
       width:34px;height:34px;margin:-17px 0 0 -17px;border-radius:50%;
       border:3px solid var(--brand-line);border-top-color:var(--brand);
       animation:ss-spin .7s linear infinite}
     /* prominent download bar at the top of a result */
     .dl-hero{display:flex;align-items:center;flex-wrap:wrap;gap:10px;
       background:var(--brand-tint);border:1px solid var(--brand-line);border-radius:10px;
       padding:12px 16px;margin:2px 0 14px}
     .dl-hero .dl-hero-label{font-weight:700;color:var(--brand);margin-right:4px}
     .dl-hero .btn{font-size:15px;padding:8px 18px}
    ")),
    tags$script(HTML(
      "(function(){var t=null;
        function pill(){var p=document.getElementById('ss-busy');
          if(!p){p=document.createElement('div');p.id='ss-busy';
            p.innerHTML='<span class=\"ss-ring\"></span><span>Working…</span>';
            document.body.appendChild(p);}return p;}
        $(document).on('shiny:busy',function(){clearTimeout(t);
          t=setTimeout(function(){pill().classList.add('on');},250);});
        $(document).on('shiny:idle',function(){clearTimeout(t);
          var p=document.getElementById('ss-busy');if(p)p.classList.remove('on');});
      })();")),
    # ---- Elevated design system (layered last so it wins the cascade). One
    # brand accent (NZ-Police navy); green/amber/red reserved for trust meaning.
    # All self-contained: no CDN fonts, scripts or icons. ------------------------
    tags$style(HTML("
     /* THE design tokens. One block, one place: every colour, radius and shadow
        the app uses is declared here. (Nothing above re-declares them.) */
     :root{
       /* Brand accent = a Qlik-family green, so arriving from Qlik Sense feels
          continuous. Deep enough that white button text stays legible. Green also
          reads as go/positive/money-in here, which coheres. The wordmark stays
          NZ-Police navy (below) so this is our tool in the Qlik world, not a clone. */
       --brand:#00793d; --brand-dark:#004a24; --brand-600:#00612f;
       --brand-tint:#e6f3ec; --brand-line:#bcdcc8;
       --ink:#15202b; --slate:#43515f; --muted:#68727d;
       --line:#e6eaee; --line-2:#d6dce2; --bg:#f3f5f4; --surface:#ffffff; --panel:#f8faf9;
       --ok:#0f7a37; --ok-bg:#e9f6ee; --ok-line:#bfe0c8;
       --bad:#b3261e; --bad-bg:#fdecec; --bad-line:#f1b6b6;
       --warn:#b7791f; --warn-ink:#8a5b00; --warn-bg:#fff7e8; --warn-line:#f0c979;
       --r:10px; --r-sm:8px; --r-lg:14px;
       --sh-1:0 1px 2px rgba(16,32,50,.05);
       --sh-2:0 1px 3px rgba(16,32,50,.06),0 6px 18px rgba(16,32,50,.07);
     }
     body{background:var(--bg);color:var(--ink);font-size:15px;line-height:1.5;-webkit-font-smoothing:antialiased}
     .container-fluid{max-width:1340px;margin:0 auto;padding:0 22px 40px}
     a{color:var(--brand)} a:hover,a:focus{color:var(--brand-dark)}
     h1,h2,h3,h4,h5{color:var(--ink);font-weight:700;letter-spacing:-.01em}
     h3{font-size:19px} h4{font-size:16px;margin-top:22px}
     p{line-height:1.55} .muted,.help-block{color:var(--muted)}
     hr{border-top-color:var(--line)}
     /* app bar: slim wordmark row, full-bleed with a hairline under it */
     .app-header{display:flex;align-items:center;gap:12px;margin:0 -22px;
       padding:13px 22px;background:var(--surface);border-bottom:1px solid var(--line);box-shadow:var(--sh-1)}
     .app-mark{width:22px;height:22px;border-radius:6px;align-self:center;
       background:linear-gradient(135deg,var(--brand),var(--brand-600));
       box-shadow:inset 0 0 0 1px rgba(255,255,255,.14)}
     .app-title{font-size:19px;font-weight:800;letter-spacing:-.02em;color:#00205b}
     .app-tagline{font-size:13px;color:var(--muted);align-self:center}
     /* main tab row = the app nav */
     #main_tabs.nav-tabs{margin:0 -22px 20px;padding:0 22px;background:var(--surface);
       border-bottom:1px solid var(--line);box-shadow:var(--sh-1);gap:2px}
     #main_tabs.nav-tabs>li>a{font-size:14.5px;font-weight:700;padding:12px 16px;color:var(--muted)}
     #main_tabs.nav-tabs>li.active>a{color:var(--brand)}
     .nav-tabs>li>a{font-weight:600;color:var(--muted)}
     /* surfaces */
     .well{background:var(--surface);border:1px solid var(--line);border-radius:var(--r-lg);box-shadow:var(--sh-1);padding:20px}
     /* inputs */
     .form-control{height:auto;padding:9px 12px;border:1px solid var(--line-2);border-radius:var(--r-sm);
       font-size:14.5px;color:var(--ink);background:var(--surface);box-shadow:none}
     .form-control:focus{border-color:var(--brand);box-shadow:0 0 0 3px rgba(0,121,61,.16)}
     select.form-control{padding-right:30px}
     .shiny-input-container>label,label.control-label{font-weight:700;font-size:13.5px;color:var(--slate);margin-bottom:6px}
     .selectize-input{border:1px solid var(--line-2)!important;border-radius:var(--r-sm)!important;padding:8px 12px!important;box-shadow:none!important}
     .selectize-input.focus{border-color:var(--brand)!important;box-shadow:0 0 0 3px rgba(0,121,61,.16)!important}
     .selectize-dropdown{border-radius:var(--r-sm);border-color:var(--line-2)}
     /* buttons: one navy accent; the old gold 'toolkit' buttons become navy-secondary */
     .btn{border-radius:var(--r-sm);font-weight:700;padding:9px 16px;font-size:14.5px;box-shadow:none;transition:background .12s,border-color .12s}
     .btn-primary{background:var(--brand);border:1px solid var(--brand);color:#fff;box-shadow:var(--sh-1)}
     .btn-primary:hover,.btn-primary:focus,.btn-primary:active,.btn-primary:active:focus{background:var(--brand-600);border-color:var(--brand-600);color:#fff}
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger){background:var(--surface);border:1px solid var(--line-2);color:var(--ink)}
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger):hover,
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger):focus{background:var(--brand-tint);border-color:var(--brand-line);color:var(--brand)}
     .btn-warning{background:var(--brand-tint);border:1px solid var(--brand-line);color:var(--brand)}
     .btn-warning:hover,.btn-warning:focus{background:#e2ebf7;border-color:var(--brand);color:var(--brand-dark)}
     .btn-danger{background:#fff;border:1px solid var(--bad-line);color:var(--bad)}
     .btn-danger:hover,.btn-danger:focus{background:var(--bad-bg);border-color:var(--bad);color:var(--bad)}
     /* advanced disclosure (Convert: 'It picked the wrong bank?') — obvious, one click */
     details.adv-bank{margin:4px 0 2px}
     details.adv-bank>summary{cursor:pointer;color:var(--brand);font-weight:700;font-size:13.5px;list-style:none;padding:4px 0}
     details.adv-bank>summary::-webkit-details-marker{display:none}
     details.adv-bank>summary::before{content:'\\25B8  '}
     details.adv-bank[open]>summary::before{content:'\\25BE  '}
     /* tables (DT) */
     table.dataTable{font-size:13.5px}
     table.dataTable thead th,table.dataTable thead td{background:var(--bg)!important;color:var(--slate)!important;
       text-transform:uppercase;font-size:11.5px;letter-spacing:.4px;font-weight:700;
       border-bottom:1px solid var(--line-2)!important;padding:10px 12px!important}
     table.dataTable tbody td{padding:9px 12px}
     table.dataTable tbody tr:hover{background:var(--brand-tint)!important}
     .dataTables_wrapper .dataTables_filter input,.dataTables_wrapper .dataTables_length select{
       border:1px solid var(--line-2);border-radius:var(--r-sm);padding:5px 8px}
     /* verdict banner: the result hero */
     .verdict{display:flex;gap:14px;align-items:flex-start;border:1px solid;border-left-width:5px;
       border-radius:var(--r-lg);padding:16px 20px;margin:2px 0 16px;box-shadow:var(--sh-1)}
     .verdict-ico{flex:0 0 auto;width:34px;height:34px;border-radius:50%;color:#fff;font-size:19px;font-weight:800;
       display:flex;align-items:center;justify-content:center;margin-top:1px}
     .verdict-title{font-size:21px;font-weight:800;letter-spacing:-.01em;margin:0 0 3px;line-height:1.2}
     .verdict-body{margin:0 0 6px;color:var(--slate);line-height:1.5}
     .verdict-high{background:var(--ok-bg);border-color:var(--ok-line);border-left-color:var(--ok)}
     .verdict-high .verdict-ico{background:var(--ok)}
     .verdict-medium{background:var(--warn-bg);border-color:var(--warn-line);border-left-color:var(--warn)}
     .verdict-medium .verdict-ico{background:var(--warn)}
     .verdict-low{background:var(--bad-bg);border-color:var(--bad-line);border-left-color:var(--bad)}
     .verdict-low .verdict-ico{background:var(--bad)}
     /* stat tiles */
     .stat-grid{display:flex;flex-wrap:wrap;gap:10px;margin:2px 0 10px}
     .stat{flex:1 1 130px;min-width:118px;background:var(--surface);border:1px solid var(--line);
       border-radius:var(--r);padding:12px 14px;box-shadow:var(--sh-1)}
     .stat-label{font-size:11.5px;color:var(--muted);text-transform:uppercase;letter-spacing:.5px;font-weight:700}
     .stat-value{font-size:22px;font-weight:800;letter-spacing:-.01em;margin-top:3px;color:var(--ink)}
     /* per-statement table for an auto-split bundle */
     .split-table{border-collapse:collapse;font-size:13px;margin:0 0 14px;max-width:900px}
     .split-table th,.split-table td{border:1px solid var(--line);padding:5px 10px;text-align:left}
     .split-table th{background:var(--brand-tint);color:var(--slate);font-weight:700}
     /* chips */
     .chip{background:#eef1f4;border:1px solid var(--line);color:var(--slate);border-radius:999px;padding:3px 11px;font-size:12px;font-weight:600}
     .chip-warn{background:var(--warn-bg);border-color:var(--warn-line);color:var(--warn-ink)}
     /* download bars */
     .dl-hero{background:var(--brand-tint);border:1px solid var(--brand-line);border-radius:var(--r-lg);padding:13px 18px}
     .dl-hero .btn{font-size:15px;padding:9px 18px}
     .dl-box{background:var(--brand-tint);border:1px solid var(--brand-line);border-radius:var(--r)}
     /* About hub cards */
     .hub-lead{font-size:17px;color:var(--slate);line-height:1.55;margin:6px 0 20px}
     a.hub-card{border:1px solid var(--line);border-radius:var(--r-lg);box-shadow:var(--sh-1);background:var(--surface);padding:18px 20px}
     a.hub-card:hover,a.hub-card:focus{box-shadow:var(--sh-2);border-color:var(--brand-line)}
     a.hub-card-primary{background:linear-gradient(180deg,#eef7f1,#e3f2e9);border-color:var(--brand-line)}
     a.hub-card-quiet{background:var(--bg)}
     .hub-card-kicker{color:var(--brand);letter-spacing:.7px;font-size:11.5px}
     .hub-card-title{font-size:18px} .hub-card-body{color:var(--slate);font-size:13.5px}
     .hub-card-go{color:var(--brand)}
    "))
  ),
  div(class = "app-header",
    span(class = "app-mark"),
    span(class = "app-title", "Statement Studio"),
    span(class = "app-tagline", "Statements and documents in — clean, checked data out.")),
  tabsetPanel(
    id = "main_tabs", selected = "Convert",
    # ---- About: the "what is this and why can I rely on it" page - one promise,
    # then the two doors (convert / teach) and a quiet third, then the proof story.
    # NB the app OPENS on Convert (selected, below); this is the page you come back
    # to, not the one you land on.
    tabPanel("About", br(),
      div(class = "hub",
        div(class = "hub-lead",
          "Turn any bank statement or financial document — PDF, CSV or Excel —",
          " into clean, checked data. It never guesses: every figure comes straight",
          " off your statement, and anything it can't verify is flagged with the reason."),
        div(class = "hub-cards",
          actionLink("ab_go_convert", class = "hub-card hub-card-primary", label = div(
            div(class = "hub-card-kicker", "Most days"),
            div(class = "hub-card-title", "Convert a statement"),
            div(class = "hub-card-body",
                "Upload your bank's export, click Convert. You get the verdict, the analysis, every transaction, and the download."),
            div(class = "hub-card-go", "Open Convert →"))),
          actionLink("ab_go_template", class = "hub-card", label = div(
            div(class = "hub-card-kicker", "New bank or document"),
            div(class = "hub-card-title", "Teach it a new layout"),
            div(class = "hub-card-body",
                "A new statement layout, or any other document - a form, a summary, a letter. The toolkit pre-fills what it can detect; you confirm against a live preview and save. About 2 minutes, no code."),
            div(class = "hub-card-go", "Open Add a template →"))),
          )),
      # NOTE: no Admin card here, and no Admin anywhere else a user can see.
      # Nobody who uses this app has the Admin password; advertising it is an
      # invitation to a locked door. Maintainer tasks are reached from the Admin
      # tab by the person who looks after the tool, and are never referred to in
      # the wording on Convert, Add a template or About.
      about_html()),
    # ---- Convert -------------------------------------------------------
    tabPanel(
      "Convert",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          fileInput("cv_file", "Statement file (.pdf / .csv / .tsv / .xlsx)",
                    accept = c(".pdf", ".csv", ".tsv", ".tdv", ".xlsx")),
          # Say what happens to the file BEFORE it is handed over. A copy is kept
          # on the server; the person uploading a client's bank statement is owed
          # that fact, the period, and the reason -- generated from the same
          # setting the purge uses, so it can never overstate or understate it.
          helpText(class = "muted", UPLOADS_NOTE),
          # WHO RAN THIS. Asked only when the tool genuinely cannot work it out.
          # Where the server sits behind a real sign-in (host or SSO) this renders
          # nothing at all -- asking for what the environment already established
          # is the question a tool should answer for itself. Where there is no
          # sign-in, it is asked ONCE per session and then collapses to one line,
          # because the alternative is an audit trail that names the server's own
          # account for the whole department.
          uiOutput("cv_whoami"),
          # THE BANK, IN FRONT. It sat inside "It picked the wrong bank?" on the
          # assumption detection usually gets it right. On real statements it does
          # not yet, which makes the override one of the most-used controls on the
          # page - so hiding it put a click on a common path. Auto-detect stays the
          # default, so nobody has to answer it; it is simply visible when they do.
          selectInput("cv_bank_quick", "Bank",
                      choices = c("Detect automatically" = ""), width = "100%"),
          actionButton("cv_go", "Convert", class = "btn-primary btn-lg btn-block"),
          helpText(sprintf("Up to %g MB.", MAX_UPLOAD_MB)),
          # Everything most people never need is one obvious click away, so the
          # default view is simply: file, name, Convert.
          tags$details(class = "adv-bank",
            tags$summary("It picked the wrong bank?"),
            # NO "include templates built here" tick-box. Whether a colleague's
            # template counts is not a per-conversion decision an accountant should
            # be making -- a template someone here built for this bank either works
            # for the team or it does not, and it is reviewed before it is trusted
            # for the dashboards (that gate is feed.allowed_template_origins, and it
            # is untouched by this). The tick-box only ever produced the puzzle "I
            # built this template and it does not work". It is now a deployment
            # setting: app.user_templates_default in config/config.yaml.
            div(style = "padding-top:10px", uiOutput("cv_bank_ui")))
        ),
        mainPanel(
          width = 8,
          # ---------------------------------------------------------------
          # THE INTERFACE RULE (charter), applied to the result page.
          #
          # ABOVE the fold: what a forensic accountant came here for -- did it
          # work, here is your download, here are your transactions. That is the
          # whole job, so it is the whole default view.
          #
          # BEHIND ONE CONTROL: evidence about how the tool did its job -- the
          # X-ray, the chart, the checks, the diagnostics, the template controls.
          # All of it still exists and none of it is more than one click away,
          # because some people want every bit of it. It simply stops being
          # pushed at the people who do not.
          #
          # That click is REMEMBERED for the session (cv_detail_open), so someone
          # technical opens it once and never sees the button again. Nobody is
          # ever asked "are you an advanced user?" -- a question the tool can
          # answer for itself by watching what they do.
          # ---------------------------------------------------------------
          uiOutput("cv_status"),
          uiOutput("cv_headline"),   # the verdict, in her words
          uiOutput("cv_downloads"),  # the payoff, right under it
          # Form / labelled-value PDF result (renders only when kind == "form").
          uiOutput("cv_form"),
          # Before any conversion, a clear empty state rather than bare headers.
          conditionalPanel("output.cv_has_result != true", uiOutput("cv_empty")),
          conditionalPanel("output.cv_has_result == true && output.cv_is_form != true",
            # Figures + transactions render only when the parse produced rows: an
            # unsupported or failed result must never show zero-money cards and an
            # empty graph under its honest verdict.
            conditionalPanel("output.cv_has_txns == true",
              uiOutput("cv_summary"),
              uiOutput("cv_proof"),    # did it add up - always, pass or fail
              uiOutput("cv_split"),    # a bundle: what each statement in it says
              h4("Your transactions"),
              DTOutput("cv_txns")),
            # cv_teach is the NEXT ACTION when a layout is new ("set it up once and
            # it converts every time"), not evidence about the run -- so it stays
            # above the fold. Burying the only thing left to do would be the same
            # mistake in the other direction.
            uiOutput("cv_teach"),
            # ONE control; everything technical sits behind it.
            uiOutput("cv_more_toggle"),
            conditionalPanel("output.cv_detail_open == true",
              conditionalPanel("output.cv_has_txns == true",
                h4("See it on the page"),
                div(
                conditionalPanel("output.ix_is_pdf == true",
                  p(class = "muted", "Green = rows kept. Amber dashed = skipped rows that look like transactions."),
                  fluidRow(
                    column(3, numericInput("ix_page", "Page", 1, min = 1, step = 1)),
                    column(9, br(),
                      checkboxGroupInput("ix_layers", "Show on the page (untick to hide a layer):",
                        choices = c("Columns" = "cols", "Kept transaction rows" = "kept",
                                    "Skipped rows" = "skipped", "Redactions" = "redact",
                                    "Balances / dates / account" = "meta",
                                    "Faint box on every word" = "words"),
                        selected = c("cols", "kept", "skipped", "redact", "meta", "words"),
                        inline = TRUE))),
                  plotOutput("ix_plot", height = "640px"),
                  uiOutput("ix_legend"),
                  h4("Rows skipped on this page - and why"),
                  helpText(HTML("A real transaction here usually means a one-line template fix - most often the <b>date format</b>. A genuine one-off: select it and add it, flagged <b>forced</b>.")),
                  DTOutput("ix_skipped"),
                  br(),
                  actionButton("ix_add_row", "This IS a transaction - add the selected row", class = "btn-warning"),
                  tags$hr(),
                  helpText("Can't share the statement? This uses only page sizes and counts."),
                  downloadButton("ix_coverage_dl", "Download shareable diagnostic (no statement contents)")),
                conditionalPanel("output.ix_is_pdf != true",
                  helpText("The X-ray view is for PDF statements. For CSV / Excel, the field coverage below shows which column feeds each field."))),
                h4("Analysis"),
            # Design-system tokens, not one-off greys: this panel is the last
            # surface on Convert that drew its own border.
            div(style = "border:1px solid var(--line);border-radius:var(--r);padding:10px 14px;margin:6px 0 14px",
              fluidRow(
                column(4, selectInput("an_view", "Show",
                  c("Money in vs out" = "inout", "Balance over time" = "balance",
                    "Running total of every transaction" = "cumnet"), width = "100%")),
                column(4, selectInput("an_group", "Group by",
                  c("Day" = "day", "Week" = "week", "Month" = "month"),
                  selected = "week", width = "100%")),
                column(4, radioButtons("an_unit", "Measure",
                  c("Dollars" = "amount", "Count" = "count"), inline = TRUE))),
              plotOutput("cv_trend", height = "270px"),
              uiOutput("cv_trend_note"))
              ),
              # "Wrong template?" -- a real question, but for someone who wants to
              # look under the bonnet, not for the person who just wanted a file.
              uiOutput("cv_candidates"),
              uiOutput("cv_detail"),
              uiOutput("cv_feed"))),   # ...and whether it reached the dashboards
          uiOutput("cv_feedback")
        )
      )
    ),
    # ---- Add a template (one toolkit for statements + a form builder) --
    tabPanel(
      "Add a template",
      br(),
      wellPanel(
        h4(style = "margin-top:0", "Teach the tool a new layout"),
        p(class = "muted", style = "max-width:820px",
          "Upload one example. The tool reads what it can, you confirm it against a live preview, and save. About two minutes."),
        fileInput("ts_file", "One example of the document (.csv / .tsv / .tdv / .pdf / .xlsx)",
                  accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
        radioButtons("ts_doctype", "What kind of document is this?",
          c("A bank or card statement - a table of transactions" = "statement",
            "Something else - labelled values (an IRD form, an account summary, a letter)" = "other"),
          selected = "statement"),
        conditionalPanel("input.ts_doctype == 'statement'",
          actionButton("ts_go", "Open the toolkit", class = "btn-primary btn-lg"),
          # The guide sits AFTER the action it explains, so the thing to click is
          # the most prominent thing on the page.
          p(class = "muted", style = "margin:12px 0 0",
            actionLink("ts_help", "New here? Read the 2-minute guide - the ways statements differ and what each setting means"))),
        conditionalPanel("input.ts_doctype == 'other'",
          div(style = "padding:10px 12px;background:#fffbe9;border:1px solid #f0c36d;border-radius:8px;margin-top:4px",
            strong("Heads up - an 'Other' document is read differently"),
            p(style = "margin:6px 0 0;color:#555",
              "It has no transaction table or running balance, so the completeness checks don't apply: the tool pulls the labelled values you name, and you eyeball each one. Set it up with the builder below.")))),
      # One flow: the toolkit above is THE way to add a statement template (its
      # Advanced tab covers field-by-field / YAML editing, so there is no separate
      # "build by hand" path). 'Other' documents (labelled values) are set up with
      # this builder, shown right here when "Something else" is picked above.
      conditionalPanel("input.ts_doctype == 'other'",
      br(),
      helpText("Name each value you want pulled out and the wording printed next to it on the page. Preview it against your document, then save."),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          textInput("fb_id", "Template name (saves as this)", "newpdf_fields"),
          textInput("fb_bank", "Which bank or issuer?", "NewIssuer"),
          textInput("fb_type", "Kind of document", "summary"),
          textAreaInput("fb_fp", "A distinctive phrase printed on this document (one per line)",
                        rows = 3, value = "KiwiSaver\nOpening balance"),
          helpText(class = "muted", "This is how the tool recognises this document next time. All of the phrases must appear on it."),
          textAreaInput("fb_fields", "The values to pull out - one per line", rows = 6,
                        value = paste("opening_balance = Opening balance; Balance brought forward | money",
                                      "closing_balance = Closing balance | money", sep = "\n")),
          helpText(class = "muted", HTML(paste0(
            "One line per value: <b>a name for it</b> = <b>the wording printed on the page</b> | <b>what kind of value</b>.<br>",
            "Extra wordings for the same value go after a semicolon. Kinds: money, date, date_range, text.<br>",
            "So <code>closing_balance = Closing balance; Balance at end | money</code> reads either wording as money."))),
          tags$hr(),
          strong("Value printed away from its wording?"),
          helpText("Draw a box around the value on the page, name it, and click Set - it is read from that spot instead."),
          fileInput("fb_sample", "A different PDF to draw on (optional)", accept = ".pdf"),
          fluidRow(
            column(6, textInput("fb_rf_field", "Name for this value", "")),
            column(6, selectInput("fb_rf_type", "What kind of value",
                                  c("money", "date", "date_range", "text")))),
          fluidRow(
            column(4, numericInput("fb_rf_page", "Page", 1, min = 1, step = 1)),
            column(8, br(),
                   actionButton("fb_rf_set", "Set value box", class = "btn-primary"),
                   actionButton("fb_rf_clear", "Clear boxes"))),
          tags$hr(),
          actionButton("fb_preview", "Preview on the document"),
          actionButton("fb_save", "Save template", class = "btn-primary"),
          br(), br(), uiOutput("fb_msg")),
        mainPanel(
          width = 8,
          conditionalPanel("output.fb_has_sample == true",
            h4("Draw a box to place a value (optional)"),
            plotOutput("fb_plot", brush = brushOpts("fb_brush", direction = "xy"), height = "540px"),
            tableOutput("fb_regions_tbl")),
          h4("Preview - what will be pulled out"), uiOutput("fb_prev_status"), DTOutput("fb_prev_tbl"),
          # The template file itself is the data analyst's view, not Beth's: it is
          # kept, one click away, instead of ending her flow with a wall of YAML.
          tags$details(style = "margin-top:14px",
            tags$summary(class = "muted", style = "cursor:pointer",
                         "The template file this will save (for your data analyst)"),
            div(class = "mono", verbatimTextOutput("fb_yaml")))))
    )
    ),
    # ---- Admin (insights + batch intake) ------------------------------
    tabPanel(
      "Admin",
      br(),
      # A broken settings file is announced to everyone who opens Admin, signed in
      # or not (without the parse detail, which only an admin sees) -- a silent
      # revert to built-in defaults is how the password and the feed folder change
      # behind the team's back.
      uiOutput("adm_cfg_banner"),
      # The sign-in box is rendered SERVER-side, because when no admin password has
      # been set there must be no box at all -- just the instructions for setting one.
      conditionalPanel("!output.admin_authed", uiOutput("adm_login_panel")),
      conditionalPanel("output.admin_authed",
      div(style = "text-align:right;margin-bottom:6px",
          actionButton("adm_signout", "Sign out of Admin", class = "btn-default btn-sm")),
      tabsetPanel(
        tabPanel(
          "Insights",
          br(),
          actionButton("adm_refresh", "Refresh from logs", class = "btn-primary"),
          helpText("A live picture from every conversion the team has run and every rating left."),
          h4("Uploads - new formats to pick up"),
          helpText("Statements the tool couldn't read, that nobody has set up yet. Pick one up: download its safe summary (no personal data) or open it in the toolkit."),
          fluidRow(
            column(8, DTOutput("adm_uploads")),
            column(4,
              selectInput("adm_up_pick", "Pick a saved upload", choices = NULL),
              downloadButton("adm_up_audit", "Download its safe summary (no personal data)"),
              br(), br(),
              actionButton("adm_up_wizard", "Set it up - open the toolkit",
                           class = "btn-warning"))),
          tags$hr(),
          h4("Format requests - raised by the team"),
          helpText("Layouts the team flagged as unsupported, in their own words (no personal data). Build the template, then mark it done."),
          fluidRow(
            column(9, DTOutput("adm_requests")),
            column(3,
              selectInput("adm_req_pick", "A request", choices = NULL),
              actionButton("adm_req_actioned", "Mark done", class = "btn-primary"),
              br(), br(),
              actionButton("adm_req_dismiss", "Dismiss"))),
          tags$hr(),
          h4("Folder intake - inbox / processed / failed"),
          helpText("Statements dropped into the inbox/ folder land here. Anything in failed/ is worth a look."),
          uiOutput("adm_inbox_counts"),
          fluidRow(
            column(8, h5("Failed - needs attention"), DTOutput("adm_inbox_failed")),
            column(4,
              selectInput("adm_inbox_pick", "A failed file", choices = NULL),
              actionButton("adm_inbox_wizard", "Open in the toolkit", class = "btn-warning"),
              br(), br(),
              downloadButton("adm_inbox_audit", "Download its safe summary (no personal data)"))),
          fluidRow(
            column(4, h5("Waiting in inbox"), DTOutput("adm_inbox_waiting")),
            column(4, h5("Processed"), DTOutput("adm_inbox_processed")),
            column(4, h5("Output folders (outbox)"), DTOutput("adm_inbox_outbox"))),
          tags$hr(),
          fluidRow(
            column(5, h4("Conversions by status"), plotOutput("adm_status_plot", height = "210px"),
                   DTOutput("adm_overview")),
            column(7, h4("Feedback flagged as wrong / minor issues"), DTOutput("adm_feedback"))),
          h4("Statements the tool can't read yet - the gaps to fill"),
          helpText("Each row is one layout the tool doesn't recognise yet (identical layouts are grouped). The biggest count is the one to build a template for first - it unblocks the most statements."),
          DTOutput("adm_gaps"),
          h4("Templates that started failing recently"),
          helpText("A statement's layout can change slightly - a field moves or gets renamed - and stop adding up. When that happens the tool flags the conversion for a check, and any template that's suddenly getting more of those shows here. Empty is good."),
          DTOutput("adm_drift"),
          h4("Template usage"),
          DTOutput("adm_usage"),
          br(),
          actionButton("adm_rollup", sprintf("Tidy up logs (archive runs older than %d days)", LOG_KEEP_DAYS)),
          uiOutput("adm_rollup_msg"),
          br(),
          # Retention of the SAVED STATEMENTS themselves -- real client data, and
          # until now nothing ever deleted it. Runs at startup too; this is the
          # "do it now" button, and it says what it will do before you press it.
          h4("Saved statements - retention"),
          helpText(UPLOADS_NOTE),
          actionButton("adm_purge_uploads",
                       if (UPLOADS_KEEP_DAYS > 0)
                         sprintf("Delete saved statements older than %d days now", as.integer(UPLOADS_KEEP_DAYS))
                       else "Delete old saved statements now (retention is set to 'keep indefinitely')",
                       class = "btn-danger"),
          uiOutput("adm_purge_msg")
        ),
        tabPanel(
          "Templates",
          br(),
          helpText(HTML("Every layout the tool can read: <b>tested</b> = shipped and checked, <b>user</b> = built here. Click a row to view and edit it.")),
          DTOutput("adm_tpl_overview"),
          br(),
          fluidRow(
            column(5,
              selectInput("adm_tpl_pick", "Preview / edit a template", choices = NULL),
              uiOutput("adm_tpl_origin"),
              actionButton("adm_tpl_dup", "Duplicate (new id)"),
              actionButton("adm_tpl_validate", "Check it's valid"),
              actionButton("adm_tpl_save", "Save as user template", class = "btn-primary"),
              actionButton("adm_tpl_hide", "Hide / un-hide (user template)"),
              actionButton("adm_tpl_delete", "Delete (user template)", class = "btn-danger"),
              br(), br(), uiOutput("adm_tpl_msg"),
              tags$details(
                tags$summary(class = "muted", style = "cursor:pointer;font-size:12.5px", "How these actions work"),
                helpText("Duplicate copies this template with a new id into the editor - tweak it and Save. Rename by changing the id, saving, then deleting the old one. Hide parks a user template out of detection without deleting it. Delete only removes USER templates; shipped 'tested' ones are read-only and win on an id clash, so a copy needs its own id."))),
            column(7,
              h4("Template YAML"),
              textAreaInput("adm_tpl_edit", NULL, value = "", width = "100%", height = "460px"))
          ),
          tags$hr(),
          h4("Near-duplicate user templates - consolidate the pile"),
          helpText("Templates that read a statement identically (same format, amounts, dates and columns) but were drafted more than once. Keep the best one and Hide or Delete the rest - pick any id above to act on it."),
          uiOutput("adm_tpl_dupes"),
          tags$hr(),
          h4("Label dictionary - the wordings the tool looks for"),
          helpText(HTML(paste0(
            "The usual reason a value comes up <b>blank</b> is that the statement prints a wording ",
            "the tool has not met - it says <i>\"Balance at start\"</i> where the tool knows ",
            "<i>\"Opening balance\"</i>. Add the exact wording and Save; it applies from the next ",
            "conversion."))),
          # The common case first, in plain English: pick the value, type the exact
          # wording, add it. It writes through dictionary_append() (R/labels.R),
          # which INSERTS one line and leaves the rest of the file -- comments and
          # all -- untouched. The whole-file editor is kept one click below for the
          # rare edit a wording cannot express (a pattern, a page rule, a new value).
          fluidRow(
            column(5,
              uiOutput("adm_dict_field_ui"),
              textInput("adm_dict_phrase", "The wording this statement prints",
                        placeholder = "balance at start"),
              actionButton("adm_dict_add", "Teach it this wording", class = "btn-primary"),
              br(), br(), uiOutput("adm_dict_add_msg")),
            column(7,
              helpText(HTML(paste0(
                "Type it as the statement prints it - case doesn't matter, and part of the ",
                "wording is enough (<i>\"balance at start\"</i> matches <i>\"Balance at start ",
                "of period\"</i>). It applies from the next conversion, everywhere the tool ",
                "looks for that value. A backup of the file is kept each time."))))),
          tags$details(style = "margin-top:6px",
            tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
              "Edit the whole dictionary file - for a pattern, a page rule, or a value that isn't listed yet"),
            div(style = "padding-top:10px",
              helpText(HTML(paste0(
                "Each value has an <code>any_of:</code> list of wordings; add a line under it, ",
                "indented the same as the lines already there, wording in quotes:<br>",
                "<span class='mono' style='font-size:12px'>opening_balance:<br>",
                "&nbsp;&nbsp;any_of:<br>",
                "&nbsp;&nbsp;&nbsp;&nbsp;- \"opening balance\"<br>",
                "&nbsp;&nbsp;&nbsp;&nbsp;- \"balance at start\"&nbsp;&nbsp; &lt;- your new line</span><br>",
                "Save refuses anything that isn't laid out properly, and keeps a backup, so it ",
                "is safe to try."))),
              fluidRow(
                column(5,
                  actionButton("adm_dict_reload", "Reload from file"),
                  actionButton("adm_dict_save", "Save dictionary", class = "btn-primary"),
                  br(), br(), uiOutput("adm_dict_msg")),
                column(7,
                  textAreaInput("adm_dict_edit", NULL, value = "", width = "100%", height = "360px")))))
        ),
        tabPanel(
          # NB the tab NAME is a navigation anchor: docs/, config.example.yaml and
          # dictionaries/lexicon.yaml all send the reader to "Admin -> Data capture".
          # Rename it only together with those.
          "Data capture",
          br(),
          h4("Local metadata capture - the raw material for on-box analysis"),
          helpText(HTML(paste0(
            "Every conversion can save a rich, structured record of <b>how it went</b> ",
            "(the layout it matched, how cleanly it parsed, detection scores, ",
            "reconciliation outcomes, OCR / redaction signals). It is stored on <b>this ",
            "machine only</b> under <code>logs/metadata/</code>, kept forever, and ",
            "<b>never enters the Qlik feed</b>. <b>No statement content is stored</b> - ",
            "only structure, counts and quality signals; any account number is stored ",
            "only as a one-way hash. Turn the detail up or down, or switch off ",
            "categories you don't want captured."))),
          fluidRow(
            column(5,
              radioButtons("adm_meta_level", "How much to capture",
                choices = c("Full - everything (recommended)" = "full",
                            "Standard - the essentials" = "standard",
                            "Off - capture nothing" = "off"),
                selected = CONFIG$metadata$level %||% "full"),
              checkboxGroupInput("adm_meta_cats", "Categories to capture",
                choices = c("Layout (shape / signature)" = "layout",
                            "Parse quality (rows / flags / misses / shapes)" = "parse_quality",
                            "Detection (scores / candidates)" = "detection",
                            "Reconciliation (KPIs / balances)" = "reconciliation",
                            "Multi-statement (# statements / periods / accounts)" = "multi_statement",
                            "Novelty (unmapped columns / unrecognised tokens)" = "novelty",
                            "Template hints (column profiles / suggested mapping)" = "template_hints",
                            "OCR (pages / confidence)" = "ocr",
                            "Redaction (counts / coverage)" = "redaction"),
                selected = names(Filter(isTRUE, CONFIG$metadata$capture %||% list()))),
              actionButton("adm_meta_save", "Save capture settings", class = "btn-primary"),
              br(), br(), uiOutput("adm_meta_msg")),
            column(7,
              tags$div(class = "muted", style = "font-size:12px",
                HTML(paste0(
                  "<b>What each level records (PII notes):</b><br>",
                  "<b>Off</b> - nothing beyond the normal run log.<br>",
                  "<b>Standard</b> - layout signature, format, detection score/match, ",
                  "row count, trust level, KPI pass/fail counts. No per-row detail.<br>",
                  "<b>Full</b> - adds flag histograms, per-field fill ratios, candidate ",
                  "scores, per-KPI outcomes, balance anchors and net amount, OCR / ",
                  "redaction detail, and timing.<br><br>",
                  "Balances and the statement period are financial metadata (not ",
                  "personal identifiers) and never leave this machine. Descriptions, ",
                  "payees and references are <b>never</b> stored. Account numbers are ",
                  "stored only as a hash, so the same account links across runs without ",
                  "the number being readable."))))
          ),
          tags$hr(),
          # The recognition vocabulary, fronted by the thing an admin actually comes
          # here to do: teach it one word. The whole-file editor is kept, one click
          # away, for the rare edit a word list cannot express (a pattern, a list of
          # date formats). Everything below writes through the SAME engine function,
          # so there is one way a word gets in.
          h4("Words the tool knows to look for"),
          helpText(HTML(paste0(
            "When a statement writes something the tool has never met - <code>cow</code> for money ",
            "out, say - it cannot tell which way that money went. Teach it the word here and every ",
            "conversion from then on knows it, everywhere. Nothing is ever added automatically: ",
            "you decide."))),
          fluidRow(
            column(5, selectInput("adm_word_kind", "This word means…",
              c("money OUT - a debit marker (D, DR, Paid...)"    = "debit_markers",
                "money IN - a credit marker (C, CR, Recd...)"    = "credit_markers",
                "the heading of a money-OUT column"            = "amount_style_debit_headers",
                "the heading of a money-IN column"             = "amount_style_credit_headers",
                "the balance is overdrawn"                     = "overdrawn_markers",
                "a value has been redacted / blacked out"      = "redaction_markers",
                "a word that appears in a table's heading row" = "header_keywords",
                "a bank or brand name, not a customer's name"  = "fingerprint_brand_words"))),
            column(4, textInput("adm_word_text", "The word, exactly as the statement prints it", "")),
            column(3, br(), actionButton("adm_word_add", "Teach it this word", class = "btn-primary"))),
          uiOutput("adm_word_msg"),
          br(),
          h4("Words your statements used that the tool didn't recognise"),
          helpText("Taken from the conversions run here, most frequent first - so the word worth teaching it is at the top. Pick one, say which way the money goes, and Approve."),
          uiOutput("adm_sugg_scope"),
          fluidRow(
            column(6,
              tableOutput("adm_sugg_tokens"),
              fluidRow(
                column(5, selectInput("adm_sugg_tok", "Word", choices = character(0))),
                column(4, radioButtons("adm_sugg_dir", "Means", inline = TRUE,
                                       choices = c("money out (debit)" = "debit_markers",
                                                   "money in (credit)" = "credit_markers"))),
                column(3, br(), actionButton("adm_sugg_approve", "Approve", class = "btn-primary"))),
              uiOutput("adm_sugg_msg")),
            column(6,
              strong("Columns in your statements that no template uses"),
              tableOutput("adm_sugg_cols"),
              helpText("A column that keeps turning up unused is usually a field worth mapping in that bank's template."))),
          br(),
          tags$details(
            tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                         "Edit the whole vocabulary file - for a pattern or a list of date formats"),
            div(style = "padding:8px 2px",
              helpText(HTML(paste0(
                "Beyond single words this file also holds the money / date / account <b>patterns</b> ",
                "and the date formats to try. Word lists ADD to the built-ins; a pattern REPLACES ",
                "one (and is refused if it won't work). Leave a category out to keep its default. ",
                "<b>Show built-in defaults</b> prints a complete, valid starting point to copy from."))),
              fluidRow(
                column(5,
                  actionButton("adm_lex_reload", "Reload from file"),
                  actionButton("adm_lex_defaults", "Show built-in defaults"),
                  actionButton("adm_lex_save", "Save vocabulary", class = "btn-primary"),
                  br(), br(), uiOutput("adm_lex_msg")),
                column(7,
                  textAreaInput("adm_lex_edit", NULL, value = "", width = "100%", height = "320px")))))
        ),
        tabPanel(
          "Batch & audit",
          br(),
          helpText(HTML("Drop in a pile of statements and get one picture: what converts, the gap layouts <b>biggest-first</b>, and <b>ready-to-edit draft templates</b> for them. Safe to share - only shapes and counts, never contents. Tick <b>convert &amp; save</b> to also produce real outputs and feed Insights.")),
          fluidRow(
            column(4,
              fileInput("adm_ba_files", "Statements (.csv / .tsv / .pdf / .xlsx)", multiple = TRUE,
                        accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
              checkboxInput("adm_ba_convert",
                            "Also convert & save outputs (writes files, logs runs, feeds Insights)",
                            value = FALSE),
              actionButton("adm_ba_run", "Run", class = "btn-primary"),
              br(), br(),
              downloadButton("adm_ba_report", "Safe audit report (.md)"),
              br(), br(),
              downloadButton("adm_ba_csv", "Converted report (.csv)"),
              br(), br(),
              helpText("Also available headless: Rscript scripts/bulk-audit.R <folder>")),
            column(8,
              uiOutput("adm_ba_summary"),
              h4("Gaps - layouts the tool can't read yet, biggest first"), DTOutput("adm_ba_clusters"),
              h4("Per file - shapes only, no personal data"), DTOutput("adm_ba_files_tbl"))),
          h4("Recommended draft templates (editable - copy into the Templates tab to save)"),
          uiOutput("adm_ba_recs"),
          tags$hr(),
          h4("Single statement - safe summary"),
          helpText("Upload one statement to download its shapes-only summary (no personal data) for sharing."),
          fileInput("adm_audit_one", "Statement", multiple = FALSE,
                    accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
          downloadButton("adm_audit_dl", "Download safe audit (.md)")
        )
      )
      )
    )
  )
)

# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Tutorial: the step-by-step "how to build a template" guide, reachable
  # from the Add-a-template tab and from inside the toolkit itself.
  show_tutorial <- function() showModal(modalDialog(
    title = "Building a template - the 2-minute guide", size = "l", easyClose = TRUE,
    tutorial_html(), footer = modalButton("Close")))
  # (Only from the tab, not from inside the toolkit modal: Shiny shows one modal
  # at a time, so opening the guide there would close the toolkit mid-edit.)
  observeEvent(input$ts_help, show_tutorial())

  # .clamp_page(v, n) -- keep a typed page number inside the document. All three
  # screens with a page box (the X-ray, the template toolkit, the form builder)
  # share it: typing a page the document doesn't have used to leave a blank panel
  # with nothing to explain it. n = NA means "we couldn't count the pages", in
  # which case the number is left exactly as typed.
  .clamp_page <- function(v, n) {
    p <- suppressWarnings(as.integer(v %||% 1L)); if (!isTRUE(is.finite(p))) p <- 1L
    max(1L, if (is.na(n)) p else min(p, as.integer(n)))
  }

  tpl_bump <- reactiveVal(0)   # bump to force a reload after a save
  # Active set: hidden user templates are excluded, so they take no part in
  # detection / conversion / the Convert picker.
  templates <- reactive({ tpl_bump(); load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR) })
  # Management set: EVERYTHING, including hidden, so Admin can preview and un-hide.
  all_templates <- reactive({ tpl_bump(); load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR, include_hidden = TRUE) })

  # ---- Admin password gate. Hidden outputs are suspended, so no admin data is
  # computed or sent to the browser until the password is entered. Set it in
  # config/config.yaml (app.admin_password); the BSO_ADMIN_PASSWORD env var
  # overrides it if present.
  #
  # Three rules, all fail-closed:
  #  1. NO PASSWORD SET -> Admin is refused outright. The shipped placeholder is
  #     printed in the example config and the docs, so serving template deletion,
  #     the shared dictionary and the analytics-feed settings behind it is the same
  #     as serving them behind nothing. The screen says how to set one.
  #  2. WRONG PASSWORD -> counted, and after a few tries the box goes quiet for a
  #     spell, so the one short secret can't just be walked through from a script.
  #  3. There is a way OUT. Without a sign-out, the first person to log in on a
  #     shared machine leaves Admin open for whoever sits down next.
  admin_ok <- reactiveVal(FALSE)
  output$admin_authed <- reactive(isTRUE(admin_ok()))
  outputOptions(output, "admin_authed", suspendWhenHidden = FALSE)

  # THE ADMIN TAB IS NOT FOR ANYONE USING THIS APP. Nobody converting a statement
  # has the password, so a tab they cannot open is a reference to Admin on every
  # screen and an invitation to try. It is hidden unless the URL carries ?admin -
  # the maintainer bookmarks that link (see docs/operational/admin-and-maintenance.md).
  #
  # This is NOT the security boundary and must never be mistaken for one: the
  # password is. Hiding the tab only stops it being advertised; every admin action
  # is still checked server-side with req(admin_ok()), so a hand-typed ?admin gets
  # a login form, exactly as before.
  observe({
    q <- parseQueryString(session$clientData$url_search %||% "")
    if (!("admin" %in% names(q))) hideTab("main_tabs", "Admin")
  })
  adm_fails <- reactiveVal(0L)          # consecutive wrong passwords this session
  adm_locked_until <- reactiveVal(0)    # epoch seconds; 0 = not locked
  # Backoff: free for the first 3 tries, then 5s, 10s, 20s, 40s ... capped at 5
  # minutes. Deliberately simple and per-session -- enough to make guessing a short
  # password by hand or by script pointless, with no timer, store or dependency.
  .adm_lock_seconds <- function(fails) if (fails < 3L) 0 else min(300, 5 * 2^(fails - 3L))
  .adm_lock_left <- function() max(0, ceiling(adm_locked_until() - as.numeric(Sys.time())))
  .adm_login_error <- function(msg) output$adm_login_msg <- renderUI(
    div(style = "color:#b00020;margin-top:6px", msg))
  observeEvent(input$adm_login, {
    if (ADMIN_PW_UNSET) {            # rule 1 -- no password set, no Admin, ever
      admin_ok(FALSE)
      .adm_login_error("Admin is closed on this install: no admin password has been set.")
      return()
    }
    wait <- .adm_lock_left()
    if (wait > 0) {                  # rule 2 -- still in the backoff window
      .adm_login_error(sprintf("Too many wrong passwords. Try again in %d second%s.",
                               wait, if (wait == 1) "" else "s"))
      return()
    }
    if (identical(input$adm_pw %||% "", CONFIG$app$admin_password %||% "")) {
      adm_fails(0L); adm_locked_until(0)
      admin_ok(TRUE); output$adm_login_msg <- renderUI(NULL)
    } else {
      n <- isolate(adm_fails()) + 1L
      adm_fails(n)
      secs <- .adm_lock_seconds(n)
      if (secs > 0) adm_locked_until(as.numeric(Sys.time()) + secs)
      .adm_login_error(if (secs > 0)
        sprintf("Wrong password (%d wrong tries). Try again in %d seconds.", n, as.integer(secs))
        else "Wrong password.")
    }
  })
  # Sign out: drop the session's admin flag AND the admin data it pulled, so
  # nothing privileged is left computed behind a hidden panel.
  observeEvent(input$adm_signout, {
    req(admin_ok())
    admin_ok(FALSE); adm_data(NULL)
    updateTextInput(session, "adm_pw", value = "")
    output$adm_login_msg <- renderUI(div(class = "muted", style = "margin-top:6px",
                                         "Signed out of Admin."))
  })
  # The sign-in panel -- or, when no password is set, the plain instructions for
  # setting one instead of a box that can never open.
  output$adm_login_panel <- renderUI({
    if (ADMIN_PW_UNSET) return(wellPanel(style = "max-width:640px",
      h4("Admin is closed - no admin password has been set"),
      p("Admin manages templates, the shared label dictionary and the analytics feed, so it stays shut until this install has its own password. The one in the example settings file is printed in the documentation, so it is not a password."),
      p(HTML(paste0("To open it, do <b>either</b> of these on the server and restart the app:",
        "<ul><li>put <code>admin_password: your-password</code> under <code>app:</code> in ",
        "<code>config/config.yaml</code> (copy <code>config/config.example.yaml</code> if it isn't there yet), or</li>",
        "<li>set the <code>BSO_ADMIN_PASSWORD</code> environment variable.</li></ul>"))),
      p(class = "muted", "Everything on the Convert and Add-a-template tabs works normally without this.")))
    wellPanel(style = "max-width:440px",
      h4("Admin - password required"),
      passwordInput("adm_pw", "Password"),
      actionButton("adm_login", "Enter", class = "btn-primary"),
      uiOutput("adm_login_msg"))
  })
  # Not suspended: it is a small static panel, and it must already be on screen the
  # moment the Admin tab is opened (it is the only thing that tells an operator the
  # tab is closed because no password has been set).
  outputOptions(output, "adm_login_panel", suspendWhenHidden = FALSE)
  # The settings-file banner. The parse detail can name a line of the file, so only
  # a signed-in admin sees it; everyone else is told plainly that something is wrong
  # and who to tell.
  output$adm_cfg_banner <- renderUI({
    if (is.null(CONFIG_ERROR)) return(NULL)
    div(style = "margin:0 0 10px;padding:10px 12px;border:1px solid var(--warn-line);background:var(--warn-bg);color:var(--warn-ink);border-radius:8px",
      strong("This install's settings file could not be read."),
      p(style = "margin:6px 0 0",
        "Statement Studio is running on built-in defaults, which include the placeholder admin password and the default analytics-feed folder. Anything set in config/config.yaml - the feed folder, the paths, the upload limit - is NOT in force. Tell whoever looks after the server."),
      if (isTRUE(admin_ok())) div(class = "mono", style = "margin-top:6px;font-size:12px", CONFIG_ERROR))
  })
  outputOptions(output, "adm_cfg_banner", suspendWhenHidden = FALSE)

  # The Convert picker offers PROVEN (curated) templates by default. Ticking
  # "Include user-created templates" adds the ones users built (with a not-tested
  # warning) -- so you can always reach your own templates, but the vetted set is
  # what's shown unless you ask for more.
  proven_templates <- reactive({ tpl_bump()
    tryCatch(load_templates(TEMPLATES_DIR, strict = FALSE), error = function(e) list()) })
  # The templates built HERE (visible ones only). Used to tell a user whose
  # statement came back "unsupported" that her own templates were switched off --
  # rather than leaving her to find a tick-box inside a collapsed panel.
  user_template_ids <- reactive({ tpl_bump()
    u <- tryCatch(load_templates(USER_TEMPLATES_DIR, origin = "user", strict = FALSE),
                  error = function(e) list())
    names(u)[!vapply(u, function(t) isTRUE(t$hidden), logical(1))] })
  cv_pick_templates <- reactive({
    if (USE_USER_TEMPLATES) templates() else proven_templates()
  })
  # Keep the visible picker's list in step with the templates actually loaded.
  observe({
    ts <- cv_pick_templates()
    b <- sort(unique(vapply(ts, function(t) t$bank %||% "", character(1))))
    b <- b[nzchar(b)]
    updateSelectInput(session, "cv_bank_quick",
                      choices = c("Detect automatically" = "", stats::setNames(b, b)),
                      selected = isolate(input$cv_bank_quick) %||% "")
  })

  output$cv_bank_ui <- renderUI({
    ts <- cv_pick_templates()
    banks <- sort(unique(vapply(ts, function(t) t$bank %||% "", character(1))))
    banks <- banks[nzchar(banks)]
    # Template picker: labelled "Bank · type - id" so you can force an EXACT
    # audited template, not just a bank, when you need to be specific.
    ov <- template_overview(ts)
    tpl_choices <- c("(auto-detect)" = "")
    if (nrow(ov)) tpl_choices <- c(tpl_choices,
      stats::setNames(ov$id, sprintf("%s · %s - %s", ov$bank, ov$type, ov$id)))
    tagList(
      selectInput("cv_bank", "Bank (optional)", c("(auto-detect)", banks)),
      selectInput("cv_template", "…or force an exact template (optional)", choices = tpl_choices))
  })


  # ---- Admin: template overview / preview / edit ----
  # The management view shows ALL templates, hidden ones included, so a parked
  # draft can be found and un-hidden.
  output$adm_tpl_overview <- renderDT(
    template_overview(all_templates()),
    options = list(pageLength = 25, dom = "tip"), rownames = FALSE, selection = "single")

  observe(updateSelectInput(session, "adm_tpl_pick", choices = sort(names(all_templates()))))

  # clicking a row selects it in the picker
  # Server-side admin gate. The Admin tab's controls are hidden until login, but
  # a crafted client message can still fire any input, so EVERY privileged handler
  # re-verifies the admin session here and fail-closes (silent no-op) without one.
  observeEvent(input$adm_tpl_overview_rows_selected, {
    req(admin_ok())
    ov <- template_overview(all_templates())
    i <- input$adm_tpl_overview_rows_selected
    if (length(i) && i <= nrow(ov)) updateSelectInput(session, "adm_tpl_pick", selected = ov$id[i])
  })

  observeEvent(input$adm_tpl_pick, {
    req(admin_ok())
    t <- all_templates()[[input$adm_tpl_pick]]; req(t)
    updateTextAreaInput(session, "adm_tpl_edit", value = template_yaml(t))
    output$adm_tpl_msg <- renderUI(NULL)
  })

  # Show whether the selected template is a read-only shipped one or a deletable
  # user one, so the analyst knows what Delete will do.
  output$adm_tpl_origin <- renderUI({
    id <- input$adm_tpl_pick; if (is.null(id) || !nzchar(id)) return(NULL)
    is_user <- id %in% user_template_ids(USER_TEMPLATES_DIR)
    hidden <- isTRUE(all_templates()[[id]]$hidden)
    tagList(
      span(class = "muted",
        if (is_user) "This is a USER template (yours) - editable, hideable & deletable."
        else "This is a shipped 'tested' template - read-only (Save makes a user copy)."),
      if (hidden) tagList(br(), span(class = "bad",
        "Hidden - it is NOT used for detection. Un-hide to bring it back.")))
  })
  # Hide / un-hide a USER template: parks it out of detection without deleting.
  observeEvent(input$adm_tpl_hide, {
    req(admin_ok())
    id <- input$adm_tpl_pick
    if (is.null(id) || !nzchar(id)) return()
    if (!(id %in% user_template_ids(USER_TEMPLATES_DIR))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be hidden; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    now_hidden <- isTRUE(all_templates()[[id]]$hidden)
    res <- safe(set_user_template_hidden(id, !now_hidden, USER_TEMPLATES_DIR), NULL)
    if (is.null(res)) { output$adm_tpl_msg <- .tpl_note("Couldn't change it.", ok = FALSE); return() }
    tpl_bump(isolate(tpl_bump()) + 1)
    output$adm_tpl_msg <- .tpl_note(if (isTRUE(res))
      sprintf("Hid <b>%s</b> - it won't be used for detection until you un-hide it.", id)
      else sprintf("Un-hid <b>%s</b> - it's active again.", id))
  })
  # Near-duplicate user templates, grouped by identical layout, so a heap of
  # variants can be consolidated (keep one, hide/delete the rest via the controls
  # above). Uses the management set so hidden variants show up too.
  output$adm_tpl_dupes <- renderUI({
    groups <- duplicate_template_groups(all_templates())
    if (!length(groups))
      return(helpText("No duplicate user templates - nothing to consolidate."))
    ov <- template_overview(all_templates())
    do.call(tagList, lapply(seq_along(groups), function(gi) {
      ids <- groups[[gi]]
      rows <- ov[ov$id %in% ids, , drop = FALSE]
      lab <- sprintf("%s · %s", rows$bank[1] %||% "?", rows$format[1] %||% "?")
      tags$div(style = "margin:6px 0;padding:6px 10px;border-left:3px solid #c77700;background:#fff8ef",
        strong(sprintf("Same layout (%d): %s", length(ids), lab)),
        tags$ul(lapply(seq_len(nrow(rows)), function(i) tags$li(
          sprintf("%s%s", rows$id[i], if (nzchar(rows$hidden[i])) " (hidden)" else "")))))
    }))
  })
  # Delete a USER template (never a shipped one), then refresh the picker.
  observeEvent(input$adm_tpl_delete, {
    req(admin_ok())
    id <- input$adm_tpl_pick
    if (is.null(id) || !nzchar(id)) return()
    if (!(id %in% user_template_ids(USER_TEMPLATES_DIR))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be deleted; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    ok <- safe(delete_user_template(id, USER_TEMPLATES_DIR), FALSE)
    if (isTRUE(ok)) {
      tpl_bump(isolate(tpl_bump()) + 1)
      output$adm_tpl_msg <- .tpl_note(sprintf("Deleted user template <b>%s</b>.", id))
    } else output$adm_tpl_msg <- .tpl_note("Couldn't delete it.", ok = FALSE)
  })

  # Duplicate the selected template with a fresh id, into the editor to tweak+save.
  observeEvent(input$adm_tpl_dup, {
    req(admin_ok())
    t <- tryCatch(yaml::yaml.load(input$adm_tpl_edit %||% ""), error = function(e) NULL)
    if (is.null(t) || is.null(t$id)) t <- templates()[[input$adm_tpl_pick]]
    req(t)
    ids <- names(templates())
    new_id <- paste0(t$id, "_copy"); k <- 2L
    while (new_id %in% ids) { new_id <- paste0(t$id, "_copy", k); k <- k + 1L }
    t$id <- new_id; t$origin <- NULL
    updateTextAreaInput(session, "adm_tpl_edit", value = yaml::as.yaml(t))
    output$adm_tpl_msg <- .tpl_note(sprintf("Duplicated as <b>%s</b> - edit it and click Save.", new_id))
  })

  .tpl_from_editor <- function() tryCatch(yaml::yaml.load(input$adm_tpl_edit), error = function(e) NULL)
  .tpl_note <- function(html, ok = TRUE)
    renderUI(div(style = sprintf("color:%s;font-size:12px", if (ok) "#137333" else "#b00020"), HTML(html)))

  observeEvent(input$adm_tpl_validate, {
    req(admin_ok())
    t <- .tpl_from_editor()
    if (is.null(t)) { output$adm_tpl_msg <- .tpl_note("That is not valid YAML.", FALSE); return() }
    probs <- validate_template(t)
    output$adm_tpl_msg <- if (!length(probs)) .tpl_note("Valid ✓")
      else .tpl_note(paste("Problems:<br>", paste(probs, collapse = "<br>")), FALSE)
  })

  observeEvent(input$adm_tpl_save, {
    req(admin_ok())
    t <- .tpl_from_editor()
    if (is.null(t)) { output$adm_tpl_msg <- .tpl_note("That is not valid YAML.", FALSE); return() }
    path <- tryCatch(save_user_template(t, USER_TEMPLATES_DIR), error = function(e) conditionMessage(e))
    if (is.character(path) && file.exists(path)) {
      tpl_bump(tpl_bump() + 1)
      shadowed <- !is.null(safe(load_templates(TEMPLATES_DIR), list())[[t$id %||% ""]])
      msg <- sprintf("Saved to %s.", path)
      if (shadowed) msg <- paste0(msg, "<br><b>Note:</b> a shipped 'tested' template with id '",
        t$id, "' takes precedence - rename the id for your edit to apply.")
      output$adm_tpl_msg <- .tpl_note(msg, !shadowed)
    } else output$adm_tpl_msg <- .tpl_note(paste("Could not save:", path), FALSE)
  })

  # ---- Admin: label dictionary edit (the fix for "check shows NA") ----
  # Two controls, ONE write path each way round: the plain-English "teach it this
  # wording" goes through dictionary_append() (R/labels.R), the whole-file editor
  # writes the text the admin typed. Both back the file up first.
  .load_dict_text <- function() read_file_text(DICT_PATH)
  dict_bump <- reactiveVal(0)
  # The values the dictionary already knows, offered by name so an admin never has
  # to invent a key. Read from the FILE, so the list can never drift from it.
  output$adm_dict_field_ui <- renderUI({
    req(admin_ok()); dict_bump()
    keys <- sort(names(safe(load_label_dict(DICT_PATH), list())))
    if (!length(keys)) return(helpText("No label dictionary file was found on this install."))
    selectInput("adm_dict_field", "This wording means…",
                stats::setNames(keys, tools::toTitleCase(gsub("_", " ", keys))))
  })
  observeEvent(input$adm_dict_add, {
    req(admin_ok())
    fld <- input$adm_dict_field %||% ""
    out <- dictionary_append(fld, input$adm_dict_phrase %||% "", path = DICT_PATH)
    if (isTRUE(out)) {
      updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text())
      dict_bump(isolate(dict_bump()) + 1)
      if (isTRUE(attr(out, "added"))) updateTextInput(session, "adm_dict_phrase", value = "")
    }
    output$adm_dict_add_msg <- renderUI(span(class = if (isTRUE(out)) "ok" else "bad",
      # The engine's own sentence, shown as it comes back -- so the screen can
      # never describe a refusal differently from the reason it was refused for.
      if (isTRUE(out) && isTRUE(attr(out, "added")))
        sprintf("Added. From the next conversion, \"%s\" is read as %s.",
                trimws(input$adm_dict_phrase %||% ""), gsub("_", " ", fld))
      else attr(out, "reason") %||% "Could not add that wording."))
  })
  observeEvent(admin_ok(), if (isTRUE(admin_ok()))
    updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text()))
  observeEvent(input$adm_dict_reload, { req(admin_ok())
    updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text()) })
  observeEvent(input$adm_dict_save, {
    req(admin_ok())
    txt <- input$adm_dict_edit %||% ""
    if (!isTRUE(tryCatch({ yaml::yaml.load(txt); TRUE }, error = function(e) FALSE))) {
      output$adm_dict_msg <- renderUI(div(style = "color:#b00020", "Not valid YAML - not saved."))
      return()
    }
    safe(file.copy(DICT_PATH, paste0(DICT_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({ writeLines(txt, DICT_PATH); TRUE }, error = function(e) FALSE))
    output$adm_dict_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved (backup at labels.yaml.bak). New wordings apply to the next conversion."
      else "Could not write the file - check folder permissions."))
  })

  # ---- Admin: local metadata-capture settings (level + category switches) ----
  observeEvent(input$adm_meta_save, {
    req(admin_ok())
    lvl <- input$adm_meta_level %||% "full"
    cats <- input$adm_meta_cats %||% character(0)
    all_cats <- c("layout", "parse_quality", "detection", "reconciliation",
                  "multi_statement", "novelty", "template_hints", "ocr", "redaction")
    capture <- stats::setNames(as.list(all_cats %in% cats), all_cats)
    okw <- save_metadata_config(lvl, capture)
    if (okw) { CONFIG$metadata$level <<- lvl; CONFIG$metadata$capture <<- capture }
    # A refusal carries its REASON (e.g. "the file is there but doesn't parse, so
    # writing this toggle would wipe every other setting in it"). Show it -- a bare
    # "check folder permissions" would send the admin hunting the wrong problem.
    why <- attr(okw, "reason")
    output$adm_meta_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved. Applies to the next conversion. Capture is local only and never enters the Qlik feed."
      else if (!is.null(why)) paste("Not saved:", why)
      else "Could not write config/config.yaml - check folder permissions."))
  })

  # ---- Admin: recognition-vocabulary (lexicon) editor ----
  .load_lex_text <- function() read_file_text(LEXICON_PATH)
  observeEvent(admin_ok(), if (isTRUE(admin_ok()))
    updateTextAreaInput(session, "adm_lex_edit", value = .load_lex_text()))
  observeEvent(input$adm_lex_reload, { req(admin_ok())
    updateTextAreaInput(session, "adm_lex_edit", value = .load_lex_text()) })
  observeEvent(input$adm_lex_defaults, { req(admin_ok())
    updateTextAreaInput(session, "adm_lex_edit", value = safe(lexicon_defaults_yaml(), "")) })
  observeEvent(input$adm_lex_save, {
    req(admin_ok())
    txt <- input$adm_lex_edit %||% ""
    parsed <- tryCatch(yaml::yaml.load(txt), error = function(e) e)
    if (inherits(parsed, "error")) {
      output$adm_lex_msg <- renderUI(div(style = "color:#b00020", "Not valid YAML - not saved.")); return()
    }
    probs <- validate_lexicon(parsed)
    if (length(probs)) {
      output$adm_lex_msg <- renderUI(div(style = "color:#b00020",
        HTML(paste("Not saved -", paste(probs, collapse = "; "))))); return()
    }
    safe(file.copy(LEXICON_PATH, paste0(LEXICON_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({
      dir.create(dirname(LEXICON_PATH), recursive = TRUE, showWarnings = FALSE)
      writeLines(txt, LEXICON_PATH); TRUE }, error = function(e) FALSE))
    if (okw) clear_lexicon_cache()   # the next conversion re-reads the vocabulary
    output$adm_lex_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved (backup at lexicon.yaml.bak). Applies to the next conversion, everywhere."
      else "Could not write the file - check folder permissions."))
  })

  # ---- Admin: teaching the engine a word -- typed in, or approved from what the
  # conversions actually met. Both write through the SAME engine function
  # (lexicon_append: union into the file, back it up, clear the cache), so there is
  # exactly one way a word gets into the vocabulary whichever control put it there.
  sugg_bump <- reactiveVal(0)
  # Teach it ONE word, in plain English. The common case ("this bank writes a word
  # we've never met") should not need YAML, and now it doesn't.
  observeEvent(input$adm_word_add, {
    req(admin_ok())
    w <- trimws(input$adm_word_text %||% "")
    if (!nzchar(w)) {
      output$adm_word_msg <- renderUI(span(class = "bad",
        "Type the word first - exactly as the statement prints it.")); return() }
    kind <- input$adm_word_kind %||% "debit_markers"
    out <- lexicon_append(kind, tolower(w), LEXICON_PATH)
    updateTextAreaInput(session, "adm_lex_edit", value = read_file_text(LEXICON_PATH))
    sugg_bump(isolate(sugg_bump()) + 1)
    ok <- isTRUE(out)
    if (ok) updateTextInput(session, "adm_word_text", value = "")
    output$adm_word_msg <- renderUI(span(class = if (ok) "ok" else "bad",
      # A refusal is reported in the engine's own words, so the screen and the
      # reason it was refused for can never say different things.
      if (ok) sprintf("Added \"%s\". Every conversion from now on knows it.", w)
      else attr(out, "reason") %||% "Could not write the vocabulary file - check folder permissions on the dictionaries folder."))
  })
  adm_suggestions <- reactive({ sugg_bump()
    safe(lexicon_suggestions(LOGDIR), list(indicator_tokens = data.frame(), unmapped_columns = data.frame())) })
  output$adm_sugg_tokens <- renderTable({ req(admin_ok()); adm_suggestions()$indicator_tokens })
  output$adm_sugg_cols   <- renderTable({ req(admin_ok()); adm_suggestions()$unmapped_columns })
  # The metadata corpus is kept forever and this read is BOUNDED (see
  # R/suggestions.R), so say which slice the ranking came from. A partial answer
  # that doesn't admit it is partial is the thing the charter forbids.
  output$adm_sugg_scope <- renderUI({
    req(admin_ok())
    s <- adm_suggestions()
    n <- s$scanned %||% NA_integer_; tot <- s$total %||% NA_integer_
    if (is.na(n) || is.na(tot)) return(NULL)
    p(class = "muted", style = "font-size:12px", if (n < tot)
      sprintf("Ranked from the %s newest conversion records of %s kept (the newest are what matter for 'what is it still missing?').",
              format(n, big.mark = ","), format(tot, big.mark = ","))
      else sprintf("Ranked from all %s conversion records kept.", format(tot, big.mark = ",")))
  })
  # ADMIN-ONLY, and it must be gated HERE. A bare observe() is never suspended by
  # visibility (unlike a render output), so without this it runs for every browser
  # that connects -- before any password -- and forces a full scan of the metadata
  # corpus on connect. conditionalPanel hides; it does not authorise.
  observe({
    req(admin_ok())
    toks <- adm_suggestions()$indicator_tokens$token %||% character(0)
    updateSelectInput(session, "adm_sugg_tok", choices = toks)
  })
  observeEvent(input$adm_sugg_approve, {
    req(admin_ok())
    tok <- input$adm_sugg_tok; cat_ <- input$adm_sugg_dir %||% "debit_markers"
    if (is.null(tok) || !nzchar(tok)) {
      output$adm_sugg_msg <- renderUI(div(class = "muted", "Pick a token first.")); return()
    }
    out <- lexicon_append(cat_, tolower(tok), LEXICON_PATH)
    updateTextAreaInput(session, "adm_lex_edit",
      value = read_file_text(LEXICON_PATH))
    sugg_bump(sugg_bump() + 1)
    ok <- isTRUE(out)
    output$adm_sugg_msg <- renderUI(div(style = sprintf("color:%s", if (ok) "#137333" else "#b00020"),
      if (ok) sprintf("Added '%s' to %s. The engine will use it on the next conversion.",
                      tolower(tok), sub("_markers$", "", cat_))
      else attr(out, "reason") %||% "Could not write the vocabulary file."))
  })

  # ---- Admin: bulk audit & gaps ----
  adm_ba <- reactiveVal(NULL)
  adm_ba_conv <- reactiveVal(NULL)   # converted-report rows, when "convert & save" was ticked
  observeEvent(input$adm_ba_run, {
    req(admin_ok())
    if (is.null(input$adm_ba_files)) {
      showNotification("Upload some statements first, then click Run.",
                       type = "warning", duration = 6)
      return()
    }
    fs <- input$adm_ba_files
    sess <- tempfile("ba_"); dir.create(sess, showWarnings = FALSE)  # guaranteed-unique per session/process (no bleed)
    paths <- vapply(seq_len(nrow(fs)), function(i) {
      d <- file.path(sess, fs$name[i]); file.copy(fs$datapath[i], d, overwrite = TRUE); d }, character(1))
    adm_ba_conv(NULL)
    withProgress(message = "Auditing statements (scanned pages are OCR'd)", value = NULL,
                 adm_ba(batch_audit(paths, templates = templates())))
    # Optional heavier pass: actually convert & log each file so the runs feed
    # Insights (this is what the old separate "Batch intake" tab did).
    if (isTRUE(input$adm_ba_convert)) {
      rows <- vector("list", length(paths))
      withProgress(message = "Converting & logging", value = 0, {
        for (i in seq_along(paths)) {
          incProgress(1 / length(paths), detail = fs$name[i])
          r <- tryCatch(convert_statement(paths[i], outdir = sess, templates_dir = TEMPLATES_DIR,
            user_templates_dir = USER_TEMPLATES_DIR, logdir = LOGDIR, requested_by = "batch"),
            error = function(e) NULL)
          csv <- if (!is.null(r)) r$outputs[grepl("\\.csv$", r$outputs)] else character(0)
          nrw <- if (length(csv) && file.exists(csv[1]))
            tryCatch(nrow(utils::read.csv(csv[1], check.names = FALSE)), error = function(e) NA_integer_) else NA_integer_
          rows[[i]] <- data.frame(file = fs$name[i], status = r$status %||% "failed",
            template = r$template_id %||% NA_character_, trust = r$trust$level %||% NA_character_,
            n_rows = nrw, stringsAsFactors = FALSE)
        }
      })
      adm_ba_conv(do.call(rbind, rows))
      load_admin()   # the batch just wrote logs; refresh Insights
    }
  })
  output$adm_ba_summary <- renderUI({
    b <- adm_ba(); if (is.null(b)) return(helpText("Upload statements and click Run."))
    g <- b$feature_gaps
    conv <- adm_ba_conv()
    none <- function(x) if (length(x)) paste(names(x), collapse = ", ") else "(none seen)"
    tagList(
      p(strong(sprintf("%d statements: ", g$total)),
        paste(sprintf("%s=%s", names(g$by_status), g$by_status), collapse = ", ")),
      p(sprintf("scanned %d · with redactions %d · multi-account %d · multi-period %d · unsupported %d across %d layouts",
        g$scanned, g$with_redactions, g$multi_account, g$multi_period, g$unsupported, g$distinct_gap_layouts)),
      p(class = "muted", sprintf("amount styles: %s | date formats: %s | banks: %s",
        none(g$amount_styles), none(g$date_formats), none(g$banks))),
      if (!is.null(conv)) div(style = "background:#eef;padding:6px 10px;border-radius:6px;margin-top:6px",
        sprintf("Converted & logged %d file(s): %d ok, %d need review, %d unsupported/failed - now in Insights.",
                nrow(conv), sum(conv$status == "ok"), sum(conv$status == "needs_review"),
                sum(conv$status %in% c("unsupported", "failed")))))
  })
  output$adm_ba_clusters <- renderDT({
    b <- adm_ba(); req(b); if (!nrow(b$clusters)) return(data.frame(note = "no gaps - everything parsed"))
    b$clusters[, c("count", "kind", "layout_hint", "signature")]
  }, options = list(pageLength = 10, dom = "tp"), rownames = FALSE)
  output$adm_ba_files_tbl <- renderDT({
    b <- adm_ba(); req(b)
    b$per_file[, c("idx", "kind", "status", "template", "bank", "n_rows", "redacted", "amount_style", "date_format", "trust")]
  }, options = list(pageLength = 15, dom = "tip"), rownames = FALSE)
  output$adm_ba_recs <- renderUI({
    b <- adm_ba(); if (is.null(b) || !length(b$recommendations))
      return(helpText("Run a bulk audit to see recommended draft templates."))
    do.call(tagList, lapply(b$recommendations, function(r) tagList(
      h5(sprintf("%d file(s), %s - draft id: %s", r$count, r$kind, r$draft_id %||% "?")),
      tags$pre(style = "font-size:11px;max-height:260px;overflow:auto;background:#f7f7f7;padding:8px", r$draft_yaml))))
  })
  output$adm_ba_report <- downloadHandler(
    filename = function() "bulk-audit.md",
    content = function(file) {
      req(admin_ok())        # admin-only export
      b <- adm_ba()
      if (is.null(b)) { showNotification("Run a bulk audit first - nothing to download yet.",
                                         type = "warning", duration = 6); req(FALSE) }
      writeLines(format_batch_audit(b), file) })
  output$adm_ba_csv <- downloadHandler(
    filename = function() "batch_converted.csv",
    content = function(file) {
      req(admin_ok())        # admin-only export
      conv <- adm_ba_conv()
      if (is.null(conv)) { showNotification("Tick 'Also convert & save' and run first - no converted report yet.",
                                            type = "warning", duration = 6); req(FALSE) }
      utils::write.csv(conv, file, row.names = FALSE) })
  output$adm_audit_dl <- downloadHandler(
    filename = function() "statement.audit.md",
    content = function(file) {
      req(admin_ok())        # admin-only export
      if (is.null(input$adm_audit_one)) { showNotification("Upload a statement to audit first.",
                                          type = "warning", duration = 6); req(FALSE) }
      writeLines(format_audit(statement_audit(input$adm_audit_one$datapath, templates = templates())), file) })

  # ---- Admin: uploads & pickups ----
  output$adm_uploads <- renderDT({
    cv_upload_id(); input$adm_refresh          # refresh after a convert or on demand
    u <- read_uploads(UPLOADS_DIR)
    if (!nrow(u)) return(data.frame(note = "no uploads yet"))
    # `purged` = the saved copy has passed its retention period and been deleted.
    # Shown, because a pickup row whose file is gone must not look actionable.
    u[, c("ts", "file_ext", "status", "template", "trust", "needs_pickup", "purged", "run_id")]
  }, options = list(pageLength = 8, dom = "tip"), rownames = FALSE)
  observe({
    req(admin_ok())          # upload ids identify real client statements
    cv_upload_id(); input$adm_refresh
    u <- read_uploads(UPLOADS_DIR)
    # Only offer uploads whose file is still on disk -- a purged one cannot be
    # audited or opened in the toolkit, so offering it would be a dead end.
    pick <- if (nrow(u)) u$id[u$needs_pickup & !u$purged] else character(0)
    updateSelectInput(session, "adm_up_pick", choices = pick)
  })
  output$adm_up_audit <- downloadHandler(
    filename = function() "upload.audit.md",
    content = function(file) {
      req(admin_ok())        # reads another analyst's saved statement off disk
      id <- input$adm_up_pick
      p <- if (!is.null(id) && nzchar(id)) upload_file_path(id, UPLOADS_DIR) else NA_character_
      writeLines(format_audit(statement_audit(need_file(p), templates = templates())), file)
    })

  # ---- Admin: format requests raised via the "tell our team" escape hatch ----
  req_bump <- reactiveVal(0)   # bump to refresh after a triage action
  output$adm_requests <- renderDT({
    req_bump(); input$adm_refresh
    q <- read_template_requests(REQUESTS_DIR)
    if (!nrow(q)) return(data.frame(note = "no format requests raised yet"))
    q[, c("ts", "requested_by", "status", "detail", "context")]
  }, options = list(pageLength = 6, dom = "tip"), rownames = FALSE)
  observe({
    # A bare observe() is NOT suspended when its tab is hidden, so without this
    # guard the template-request queue was read off disk and its ids pushed into
    # every session's select input, admin or not. Found by the invariant test once
    # it started reading whole observer bodies instead of a fixed nine lines.
    req(admin_ok())
    req_bump(); input$adm_refresh
    q <- read_template_requests(REQUESTS_DIR)
    open <- if (nrow(q)) q$id[q$status == "open"] else character(0)
    updateSelectInput(session, "adm_req_pick", choices = open)
  })
  observeEvent(input$adm_req_actioned, {
    req(admin_ok())
    id <- input$adm_req_pick; req(id, nzchar(id))
    if (isTRUE(set_request_status(id, "actioned", dir = REQUESTS_DIR))) req_bump(req_bump() + 1)
  })
  observeEvent(input$adm_req_dismiss, {
    req(admin_ok())
    id <- input$adm_req_pick; req(id, nzchar(id))
    if (isTRUE(set_request_status(id, "dismissed", dir = REQUESTS_DIR))) req_bump(req_bump() + 1)
  })

  # ---- Admin: folder-intake browser (inbox / processed / failed / outbox) ----
  inbox_state <- reactive({ input$adm_refresh; cv_upload_id(); inbox_status(".") })
  output$adm_inbox_counts <- renderUI({
    s <- inbox_state(); c <- s$counts
    p(class = "muted", HTML(sprintf(
      "Waiting: <b>%d</b> &nbsp;|&nbsp; Processed: <b>%d</b> &nbsp;|&nbsp; Failed: <b>%d</b> &nbsp;|&nbsp; Stuck: <b>%d</b> &nbsp;|&nbsp; Output folders: <b>%d</b>",
      c[["inbox"]], c[["processed"]], c[["failed"]], c[["stuck"]], c[["outbox"]])))
  })
  inbox_tbl <- function(which) renderDT({
    d <- inbox_state()$folders[[which]]
    if (!nrow(d)) return(data.frame(note = "empty"))
    d
  }, options = list(pageLength = 6, dom = "tip"), rownames = FALSE)
  output$adm_inbox_failed    <- inbox_tbl("failed")
  output$adm_inbox_waiting   <- inbox_tbl("inbox")
  output$adm_inbox_processed <- inbox_tbl("processed")
  output$adm_inbox_outbox    <- inbox_tbl("outbox")
  observe({
    # These are REAL filenames of failed client statements -- routinely a surname
    # and a case reference. They were being pushed to every connected browser on
    # connect, with no login and no user action.
    req(admin_ok())
    s <- inbox_state()
    updateSelectInput(session, "adm_inbox_pick",
      choices = if (nrow(s$folders$failed)) s$folders$failed$file else character(0))
  })
  observeEvent(input$adm_inbox_wizard, {
    req(admin_ok())
    nm <- input$adm_inbox_pick
    if (is.null(nm) || !nzchar(nm)) { showNotification("Pick a failed file first.", type = "warning"); return() }
    p <- failed_file_path(nm, ".")
    if (is.na(p)) { showNotification("That file is no longer in failed/.", type = "error"); return() }
    open_guided(p, nm)
  })
  output$adm_inbox_audit <- downloadHandler(
    filename = function() paste0(input$adm_inbox_pick %||% "file", ".audit.md"),
    content = function(file) {
      req(admin_ok())        # reads a failed client statement off disk
      nm <- input$adm_inbox_pick
      p <- if (!is.null(nm) && nzchar(nm)) failed_file_path(nm, ".") else NA_character_
      writeLines(format_audit(statement_audit(need_file(p), templates = templates())), file)
    })

  # ---- Add a template: build a PDF-form template from labels + placed boxes --
  # (Extraction/running of form PDFs now happens on the Convert tab - one door.)
  # parse_fields_spec -- turn the friendly "name = Label; Label2 | money" lines
  # into a fields{} block. Value type after "|" is optional (default text).
  parse_fields_spec <- function(text) {
    lines <- trimws(strsplit(text %||% "", "\n")[[1]])
    lines <- lines[nzchar(lines) & grepl("=", lines)]
    fields <- list()
    for (ln in lines) {
      name <- trimws(sub("=.*$", "", ln))
      rhs  <- trimws(sub("^[^=]*=", "", ln))
      vtype <- NULL
      if (grepl("\\|", rhs)) { vtype <- trimws(sub("^.*\\|", "", rhs)); rhs <- trimws(sub("\\|.*$", "", rhs)) }
      labels <- trimws(strsplit(rhs, ";")[[1]]); labels <- labels[nzchar(labels)]
      if (!nzchar(name) || !length(labels)) next
      spec <- list(any_of = as.list(labels))
      if (!is.null(vtype) && vtype %in% c("money", "date", "date_range", "text")) spec$value <- vtype
      fields[[name]] <- spec
    }
    fields
  }
  # Positional value boxes drawn on the sample: field -> list(page,x_min..y_max,value).
  fb_regions <- reactiveVal(list())
  # ONE upload for this whole flow. The page asks for the document at the top;
  # asking again inside the builder ("sample PDF to draw on") meant the first
  # picker did nothing at all on this path, and nothing said so. The document
  # uploaded at the top is used by default; the builder's own picker stays for the
  # case where a different PDF is handier to draw on.
  fb_doc <- reactive({
    if (!is.null(input$fb_sample)) return(input$fb_sample$datapath)
    f <- input$ts_file
    if (!is.null(f) && identical(tolower(tools::file_ext(f$name %||% "")), "pdf"))
      return(f$datapath)
    NULL
  })
  fb_n_pages <- reactive({
    p <- fb_doc(); if (is.null(p)) return(NA_integer_)
    i <- tryCatch(read_input(p), error = function(e) NULL); if (is.null(i)) return(NA_integer_)
    n <- length(i$pages %||% i$words %||% list())
    if (n >= 1L) as.integer(n) else NA_integer_
  })
  observe({
    n <- fb_n_pages(); if (is.na(n)) return()
    updateNumericInput(session, "fb_rf_page", max = n,
      label = if (n == 1L) "Page (this document is 1 page)" else sprintf("Page (1 to %d)", n))
  })
  output$fb_has_sample <- reactive({ !is.null(fb_doc()) })
  outputOptions(output, "fb_has_sample", suspendWhenHidden = FALSE)
  observeEvent(input$fb_rf_set, {
    nm <- gsub("[^A-Za-z0-9_]+", "_", trimws(input$fb_rf_field %||% ""))
    br <- input$fb_brush
    if (!nzchar(nm)) { showNotification("Name the field first.", type = "warning"); return() }
    if (is.null(br)) { showNotification("Draw a box on the page around the value first.", type = "warning"); return() }
    r <- fb_regions()
    r[[nm]] <- list(page = .clamp_page(input$fb_rf_page, fb_n_pages()),
                    x_min = round(br$xmin), x_max = round(br$xmax),
                    y_min = round(br$ymin), y_max = round(br$ymax),
                    value = input$fb_rf_type %||% "text")
    fb_regions(r)
    showNotification(sprintf("Placed the value box for '%s'.", nm), type = "message")
  })
  observeEvent(input$fb_rf_clear, fb_regions(list()))
  output$fb_regions_tbl <- renderTable({
    r <- fb_regions(); if (!length(r)) return(NULL)
    do.call(rbind, lapply(names(r), function(nm) data.frame(field = nm, page = r[[nm]]$page,
      box = sprintf("x %d-%d, y %d-%d", r[[nm]]$x_min, r[[nm]]$x_max, r[[nm]]$y_min, r[[nm]]$y_max),
      value = r[[nm]]$value, stringsAsFactors = FALSE)))
  })
  fb_render <- reactive({
    p <- fb_doc(); req(p)
    render_page_view(p, .clamp_page(input$fb_rf_page, fb_n_pages()), 100)
  })
  output$fb_plot <- renderPlot({
    r <- fb_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i", xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    for (nm in names(fb_regions())) { b <- fb_regions()[[nm]]
      if (isTRUE(b$page == r$pg)) {
        rect(b$x_min, b$y_max, b$x_max, b$y_min, border = "#a15c00", lwd = 2)
        text(b$x_min, b$y_min, nm, col = "#a15c00", font = 2, cex = 0.9, adj = c(0, 1))
      } }
  })
  fb_template <- reactive({
    phrases <- trimws(strsplit(input$fb_fp %||% "", "\n")[[1]]); phrases <- phrases[nzchar(phrases)]
    flds <- parse_fields_spec(input$fb_fields)
    # Merge the drawn value boxes: a positional field reads its value from the box,
    # regardless of where (or whether) a label appears.
    for (nm in names(fb_regions())) { b <- fb_regions()[[nm]]
      flds[[nm]] <- list(region = list(page = b$page, x_min = b$x_min, x_max = b$x_max,
                                       y_min = b$y_min, y_max = b$y_max), value = b$value)
    }
    list(id = gsub("[^A-Za-z0-9_]+", "_", input$fb_id %||% "newpdf_fields"),
         bank = input$fb_bank %||% "NewIssuer", statement_type = input$fb_type %||% "summary",
         format = "pdf", mode = "fields", version = 1,
         fingerprint = list(page_contains_all = as.list(phrases)),
         fields = flds, currency = "NZD")
  })
  output$fb_yaml <- renderText({ t <- fb_template(); t$origin <- NULL; yaml::as.yaml(t) })

  fb_preview <- reactiveVal(NULL)
  observeEvent(input$fb_preview, {
    p <- fb_doc()
    if (is.null(p)) { showNotification("Upload the document at the top of this page first (or a PDF to draw on, above).",
                                       type = "warning", duration = 6); return() }
    inp <- tryCatch(read_input(p), error = function(e) NULL)
    if (is.null(inp)) { showNotification("Couldn't read that file.", type = "error"); return() }
    f <- tryCatch(extract_fields(inp, fb_template()), error = function(e) NULL)
    fb_preview(f)
  })
  # Same verdict card as everywhere else, and it names the ones that came back
  # empty: a value that did not match is the whole reason to look at this preview.
  output$fb_prev_status <- renderUI({
    f <- fb_preview()
    if (is.null(f)) return(p(class = "muted",
      "Click \"Preview on the document\" to see what each wording pulls out."))
    got <- sum(f$matched %in% TRUE); miss <- nrow(f) - got
    missing <- if (miss > 0) paste(f$field[!(f$matched %in% TRUE)], collapse = ", ") else ""
    div(class = if (miss > 0) "verdict verdict-medium" else "verdict verdict-high",
        style = "margin:2px 0 12px",
      div(class = "verdict-ico", if (miss > 0) "!" else "✓"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", sprintf("%d of %d value%s found", got, nrow(f),
                                             if (nrow(f) == 1L) "" else "s")),
        p(class = "verdict-body", style = "margin:0", if (miss > 0)
            sprintf("Not found: %s. The wording on the page has to match what you typed - copy it exactly, or draw a box around the value instead.", missing)
          else "Check each value below against the document, then Save template.")))
  })
  output$fb_prev_tbl <- renderDT({
    f <- fb_preview(); req(!is.null(f))
    datatable(f[, intersect(c("field", "label", "value", "matched"), names(f))],
              rownames = FALSE, options = list(dom = "t", pageLength = 25))
  })
  observeEvent(input$fb_save, {
    t <- fb_template()
    probs <- validate_fields_template(t)
    if (length(probs)) {
      output$fb_msg <- renderUI(span(class = "bad", paste("Not valid:", paste(probs, collapse = "; ")))); return() }
    ok <- tryCatch({ save_fields_template(t, USER_FIELDS_DIR); TRUE }, error = function(e) FALSE)
    output$fb_msg <- renderUI(if (isTRUE(ok))
      span(class = "ok", sprintf("Saved '%s'. Now upload the document on the Convert tab - it's detected automatically.", t$id))
      else span(class = "bad", "Couldn't save - check the fields."))
  })

  # ---- X-ray, shown inline on the Convert tab (no separate upload/section).
  # Derived from the conversion result: read the converted file with its matched
  # template and lay out exactly what the engine selected on the page.
  ix_state <- reactive({
    res <- cv_res(); src <- cv_src(); if (is.null(res) || is.null(src)) return(NULL)
    tid <- (res$template_id %||% NA_character_)[1]
    if (is.na(tid) || !nzchar(tid)) return(NULL)
    tmpl <- tryCatch(templates()[[tid]], error = function(e) NULL); if (is.null(tmpl)) return(NULL)
    inp <- tryCatch(read_input(src$path), error = function(e) NULL); if (is.null(inp)) return(NULL)
    if (!identical(inp$kind, "pdf")) return(list(is_pdf = FALSE))
    # force_rows: user-confirmed rows are painted kept, so the X-ray matches what
    # the reader now emits after a force-include.
    layout <- tryCatch(inspect_pdf_layout(inp, tmpl, force_rows = cv_forced()), error = function(e) NULL)
    # the balance/period/account values are ALREADY in the conversion result -- reuse
    # them instead of re-scanning the whole document with extract_metadata.
    meta <- cv_res()$metadata %||% tryCatch(extract_metadata(inp), error = function(e) NULL)
    meta_loc <- NULL
    if (!is.null(meta)) {
      targets <- list(opening_balance = meta$opening_balance, closing_balance = meta$closing_balance,
                      period_start = meta$period_start, period_end = meta$period_end)
      if (length(meta$accounts)) targets$account <- meta$accounts[1]
      wbp <- inp$words %||% list()
      meta_loc <- lapply(seq_along(wbp), function(p)
        tryCatch(locate_values_on_page(wbp[[p]], targets), error = function(e) NULL))
    }
    list(is_pdf = TRUE, path = src$path, layout = layout, meta_loc = meta_loc)
  })
  # Answer "is this a PDF?" CHEAPLY (read_input is content-cached, ~1ms) instead of
  # pulling ix_state -- which would run the whole ~1.6s X-ray layout on EVERY PDF
  # convert just to drive the conditionalPanel, even with the X-ray tab closed.
  output$ix_is_pdf <- reactive({
    res <- cv_res(); src <- cv_src(); if (is.null(res) || is.null(src)) return(FALSE)
    isTRUE(identical(tryCatch(read_input(src$path)$kind, error = function(e) NULL), "pdf"))
  })
  outputOptions(output, "ix_is_pdf", suspendWhenHidden = FALSE)

  # How many pages the converted statement has. read_input is content-cached (~1ms),
  # so this is cheap enough to drive a label.
  ix_n_pages <- reactive({
    src <- cv_src(); if (is.null(src)) return(NA_integer_)
    inp <- tryCatch(read_input(src$path), error = function(e) NULL)
    if (is.null(inp) || !identical(inp$kind, "pdf")) return(NA_integer_)
    n <- length(inp$pages %||% inp$words %||% list())
    if (n >= 1L) as.integer(n) else NA_integer_
  })
  # ONE page number, clamped, shared by the picture, the legend, the skipped-row
  # table and "add this row" -- they each used to re-derive it, so they could
  # disagree about which page you were looking at.
  ix_page_now <- reactive(.clamp_page(input$ix_page, ix_n_pages()))
  # Say how many pages there are, on the control itself.
  observe({
    n <- ix_n_pages(); if (is.na(n)) return()
    updateNumericInput(session, "ix_page", max = n,
      label = if (n == 1L) "Page (this statement is 1 page)" else sprintf("Page (1 to %d)", n))
  })

  ix_pal <- function(bands) {
    if (!length(bands)) return(character(0))
    stats::setNames(grDevices::hcl(seq(5, 320, length.out = length(bands)), 75, 50), names(bands))
  }
  output$ix_legend <- renderUI({
    st <- ix_state(); req(st, st$is_pdf, !is.null(st$layout))
    pg <- as.character(ix_page_now())
    P <- st$layout$pages[[pg]]; req(P)
    pal <- ix_pal(P$bands)
    layers <- input$ix_layers %||% c("cols", "kept", "skipped", "redact", "meta", "words")
    sw <- function(col, lab) tags$div(style = "margin:2px 0",
      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;border:2px solid %s;margin-right:6px;vertical-align:middle", col)),
      tags$span(lab))
    has_ocr <- !is.null(P$words$ocr_conf) && any(!is.na(P$words$ocr_conf))
    # Only name the layers currently shown, so the legend tracks the tick-boxes.
    tagList(strong("Legend"),
      if ("cols" %in% layers) lapply(names(pal), function(nm) sw(pal[[nm]], nm)),
      if ("kept" %in% layers) sw("#137333", "transaction row (kept)"),
      if ("skipped" %in% layers) sw("#c77700", "skipped row that looks like a transaction"),
      if ("meta" %in% layers) sw("#a15c00", "balance / account details"),
      if ("redact" %in% layers) sw("#b00020", "redaction (not read)"),
      if (has_ocr) sw("#c77700", "shaded amber = machine-read word the tool is unsure about - double-check it"))
  })
  ix_render <- reactive({
    st <- ix_state(); req(st, st$is_pdf)
    render_page_view(st$path, ix_page_now(), 100)
  })
  output$ix_plot <- renderPlot({
    st <- ix_state(); req(st, st$is_pdf); r <- ix_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    lay <- st$layout; if (is.null(lay)) return(invisible())
    P <- lay$pages[[as.character(r$pg)]]; if (is.null(P)) return(invisible())
    layers <- input$ix_layers %||% c("cols", "kept", "skipped", "redact", "meta", "words")
    reg <- P$region; ytop <- reg$y_min %||% 0; ybot <- reg$y_max %||% r$h
    pal <- ix_pal(P$bands)
    if (!is.null(reg$x_min)) rect(reg$x_min, ybot, reg$x_max %||% r$w, ytop,
                                  border = "#666", lty = 2, lwd = 1.4)
    w <- P$words
    if ("words" %in% layers && nrow(w))
      rect(w$x, w$y, w$x + w$width, w$y + w$height, border = "#cfcfcf", lwd = 0.4)
    # Machine-read (OCR) words the engine itself is unsure about: shaded amber so
    # "double-check the numbers" points at exactly the doubtful words. Uses the SAME
    # floor as the engine's ocr_low_conf row flag (PARAM_OCR_CELL_MIN_CONF, params.R).
    if (!is.null(w$ocr_conf)) {
      lc <- w[!is.na(w$ocr_conf) & w$ocr_conf < PARAM_OCR_CELL_MIN_CONF, , drop = FALSE]
      if (nrow(lc)) rect(lc$x, lc$y, lc$x + lc$width, lc$y + lc$height,
                         border = "#c77700", col = "#c7770040", lwd = 1.2)
    }
    if ("cols" %in% layers) {
      sel <- w[!is.na(w$column), , drop = FALSE]
      if (nrow(sel)) rect(sel$x, sel$y, sel$x + sel$width, sel$y + sel$height,
                          border = pal[sel$column], lwd = 1.3)
    }
    if ("redact" %in% layers) {
      red <- w[w$redacted %in% TRUE, , drop = FALSE]
      if (nrow(red)) rect(red$x, red$y, red$x + red$width, red$y + red$height,
                          border = "#b00020", col = "#b0002022", lwd = 1)
    }
    if ("cols" %in% layers) for (nm in names(P$bands)) { b <- P$bands[[nm]]
      if (!is.null(b$x_min) && !is.null(b$x_max)) {
        rect(b$x_min, ybot, b$x_max, ytop, border = pal[[nm]], lwd = 2)
        text((b$x_min + b$x_max) / 2, ytop, nm, col = pal[[nm]], font = 2, cex = 0.9, pos = 3, offset = 0.2)
      } }
    if ("kept" %in% layers) {
      kr <- P$rows[P$rows$kept, , drop = FALSE]
      if (nrow(kr)) rect(kr$x0 - 1, kr$y0 - 1, kr$x1 + 1, kr$y1 + 1, border = "#137333", lwd = 1)
    }
    # Amber dashed: rows the engine skipped that LOOK like transactions (bad date
    # or missing amount) -- the "why aren't you seeing it" rows. Continuations,
    # summaries and headings are intentionally left unhighlighted (they're in the
    # table below with their reason) so the page isn't noisy.
    if ("skipped" %in% layers && !is.null(P$rows$reason)) {
      sk <- P$rows[!P$rows$kept &
        grepl("didn't parse|no amount", P$rows$reason %||% ""), , drop = FALSE]
      if (nrow(sk)) rect(sk$x0 - 1, sk$y0 - 1, sk$x1 + 1, sk$y1 + 1,
                         border = "#c77700", lty = 2, lwd = 1.6)
    }
    if ("meta" %in% layers && !is.null(st$meta_loc)) {
      ml <- st$meta_loc[[r$pg]]
      if (!is.null(ml)) { f <- ml[ml$found %in% TRUE, , drop = FALSE]
        if (nrow(f)) { rect(f$x0 - 2, f$y0 - 2, f$x1 + 2, f$y1 + 2, border = "#a15c00", lwd = 2)
          text(f$x1 + 3, (f$y0 + f$y1) / 2, f$field, col = "#a15c00", font = 2, cex = 0.8, adj = c(0, 0.5)) } }
    }
    # pinned header-value boxes (metadata_regions) the template defines for this page
    if ("meta" %in% layers) {
      mr <- P$meta_regions %||% list()
      for (nm in names(mr)) { b <- mr[[nm]]
        if (is.null(b$x_min) || is.null(b$x_max)) next
        y0 <- b$y_min %||% 0; y1 <- b$y_max %||% r$h
        rect(b$x_min, y0, b$x_max, y1, border = "#7b1fa2", lwd = 2, lty = 3)
        text(b$x_min, y0, nm, col = "#7b1fa2", font = 2, cex = 0.8, pos = 3, offset = 0.2)
      }
    }
  })
  # "Why aren't you seeing it": every row the engine skipped on this page, with the
  # plain-English reason. Kept rows are excluded (they're the transactions). The
  # actionable skips (bad date / missing amount) sort to the top.
  # The skipped rows for the CURRENT page, ordered likely-missed-transactions
  # first. ONE source of truth so the table's row numbers and the "add" action
  # below refer to exactly the same rows.
  ix_skipped_rows <- reactive({
    st <- ix_state(); if (is.null(st) || !isTRUE(st$is_pdf)) return(NULL)
    lay <- st$layout; if (is.null(lay)) return(NULL)
    pg <- ix_page_now()
    P <- lay$pages[[as.character(pg)]]
    if (is.null(P) || is.null(P$rows) || !nrow(P$rows) || is.null(P$rows$reason)) return(NULL)
    sk <- P$rows[!P$rows$kept & nzchar(P$rows$reason %||% ""), , drop = FALSE]
    if (!nrow(sk)) return(sk)
    actionable <- grepl("didn't parse|no amount", sk$reason)
    sk[order(!actionable), , drop = FALSE]           # likely-missed transactions first
  })
  output$ix_skipped <- renderDT({
    sk <- ix_skipped_rows()
    if (is.null(sk)) return(datatable(data.frame(message = "Nothing to show for this page yet."),
                                      rownames = FALSE, options = list(dom = "t")))
    if (!nrow(sk)) return(datatable(
      data.frame(message = "Every row on this page was either kept or is a heading / footer."),
      rownames = FALSE, options = list(dom = "t")))
    trunc <- function(s, n = 90) ifelse(nchar(s) > n, paste0(substr(s, 1, n), "…"), s)
    out <- data.frame(
      `what's on the row` = trunc(sk$raw %||% ""),
      `date cell` = sk$date %||% NA_character_,
      `why it was skipped` = sk$reason,
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(out, rownames = FALSE, selection = "single",
              options = list(pageLength = 10, dom = "tp", scrollX = TRUE))
  })
  # Shareable, PII-safe row-coverage diagnostic for the current statement: page
  # sizes vs the template reference, kept/skipped counts and reasons -- the numbers
  # needed to see why rows go missing WITHOUT the statement contents.
  output$ix_coverage_dl <- downloadHandler(
    filename = function() "row-coverage-diagnostic.md",
    content = function(file) {
      st <- ix_state(); src <- cv_src(); res <- cv_res()
      tid <- (res$template_id %||% NA_character_)[1]
      tmpl <- if (!is.na(tid) && nzchar(tid)) tryCatch(templates()[[tid]], error = function(e) NULL) else NULL
      if (is.null(src) || is.null(tmpl)) {
        showNotification("Convert a PDF statement first - nothing to diagnose yet.", type = "warning", duration = 6); req(FALSE) }
      inp <- tryCatch(read_input(src$path), error = function(e) NULL)
      if (is.null(inp)) { showNotification("Couldn't re-read the file for the diagnostic.", type = "error"); req(FALSE) }
      writeLines(format_row_coverage(row_coverage(inp, tmpl)), file)
    })
  # "This IS a transaction": select a skipped row and add it. We record its page +
  # y-band as a force_rows entry and re-run the conversion, so the row lands in the
  # output flagged `forced` (and malformed / date_unresolved if its amount or date
  # couldn't be read) -- captured, and honestly labelled as hand-added.
  observeEvent(input$ix_add_row, {
    sk <- ix_skipped_rows()
    if (is.null(sk) || !nrow(sk)) {
      showNotification("No skipped rows on this page to add.", type = "warning"); return() }
    sel <- input$ix_skipped_rows_selected
    if (is.null(sel) || !length(sel)) {
      showNotification("Click a row in the table below first, then add it.", type = "warning"); return() }
    row <- sk[sel[1], , drop = FALSE]
    band <- list(page = ix_page_now(), y_min = row$y0 - 1, y_max = row$y1 + 1)
    cur <- cv_forced(); cur[[length(cur) + 1L]] <- band; cv_forced(cur)
    src <- cv_src(); sess <- cv_dir()
    if (is.null(src) || is.null(sess)) {
      showNotification("Convert a statement first.", type = "warning"); return() }
    res <- convert_now(src$path, sess, forced_rows = cv_forced())
    cv_res(res)
    # Re-publish. Adding a row changes the figures the workbook and CSV hold, and
    # the feed is keyed by the statement's content hash, so this OVERWRITES that
    # statement's published rows rather than adding a second copy. Without it the
    # dashboards kept the pre-correction rows while the screen and the download
    # showed the corrected ones -- and the feed line above would have gone on
    # reporting a verdict from the previous run.
    publish_result(res, cv_recorded())
    showNotification("Added that row as a transaction (flagged 'forced') and re-checked the statement.",
                     type = "message", duration = 6)
  })
  # Remediate a stuck upload right here: load the saved file into the SAME guided
  # toolkit the Convert tab uses, so a failed/abandoned statement is a 2-second
  # pickup - identify it in the table (A), open it, teach the tool, save (B).
  observeEvent(input$adm_up_wizard, {
    req(admin_ok())
    id <- input$adm_up_pick
    if (is.null(id) || !nzchar(id)) {
      showNotification("Pick a saved upload first.", type = "warning"); return() }
    p <- upload_file_path(id, UPLOADS_DIR)
    if (is.na(p) || !file.exists(p)) {
      showNotification("That upload's file is no longer available.", type = "error"); return() }
    # Same setup surface as Convert; upload_id ties a successful Save back to this
    # pickup so it drops off the list.
    open_guided(p, basename(p), upload_id = id)
  })

  cv_res <- reactiveVal(NULL)
  cv_dir <- reactiveVal(NULL)
  cv_src <- reactiveVal(NULL)      # the uploaded file (path + name), for guided setup
  cv_fb_done <- reactiveVal(FALSE)
  cv_fb_rec  <- reactiveVal(NULL)              # the feedback record, incl. any feed retraction
  cv_upload_id <- reactiveVal(NA_character_)   # the tracked upload for this conversion
  cv_forced <- reactiveVal(list())             # user-confirmed "this IS a transaction" bands
  cv_feed_gate <- reactiveVal(NULL)            # what the governed feed did with it
  cv_recorded  <- reactiveVal(FALSE)           # ...and whether this run feeds at all
  # When the browser tab closes, take this session's scratch folder with it. The
  # folder holds a copy of the client's statement plus every output; the process
  # temp dir is only cleared when R exits, and this app is a long-running service.
  session$onSessionEnded(function() {
    d <- isolate(cv_dir())
    if (!is.null(d) && nzchar(d) && dir.exists(d)) try(unlink(d, recursive = TRUE), silent = TRUE)
  })

  # convert_now(src, sess, forced_rows) -- run the one front-door conversion with a
  # progress bar, reading bank / exact-template from the current inputs. Shared by
  # the Convert button and the X-ray "add this row" action so both paths behave
  # identically (the second just adds force_rows and reuses the same session dir).
  convert_now <- function(src, sess, forced_rows = NULL, force_tpl = NULL,
                          include_user = FALSE) {
    # The picker in front wins; the one inside "It picked the wrong bank?" is the
    # same choice for anyone who opens that panel. Blank in both means auto-detect.
    pick <- function(v) if (is.null(v) || !nzchar(v) || identical(v, "(auto-detect)")) NULL else v
    bank <- pick(input$cv_bank_quick) %||% pick(input$cv_bank)
    forced_tpl <- force_tpl %||%
      (if (!is.null(input$cv_template) && nzchar(input$cv_template)) input$cv_template else NULL)
    # The tick-box, an explicit force, or an explicit include_user brings in the
    # user-created set. include_user exists because updateCheckboxInput only
    # reaches the browser AFTER this observer finishes: right after a save we know
    # this is for the moment right after a template is saved, when the caller
    # already knows the new template must take part even if the deployment has
    # user-built templates switched off.
    use_user <- USE_USER_TEMPLATES || !is.null(force_tpl) || isTRUE(include_user)
    who <- who_now()
    withProgress(message = "Converting statement…", value = 0.2, {
      incProgress(0.2, detail = "Reading the file and detecting its format…")
      out <- tryCatch(
        convert_document(src, bank = bank, outdir = sess,
                         templates_dir = TEMPLATES_DIR,
                         user_templates_dir = if (use_user) USER_TEMPLATES_DIR else NULL,
                         fields_dir = FIELDS_DIR, user_fields_dir = USER_FIELDS_DIR,
                         requested_by = who, logdir = LOGDIR,
                         force_template = forced_tpl, force_rows = forced_rows),
        error = function(e) {
          safe(cat(sprintf("[%s] convert error (%s): %s\n", format(Sys.time()),
                           basename(src), conditionMessage(e)),
                   file = file.path(LOGDIR, "errors.log"), append = TRUE))
          list(status = "failed", messages = FRIENDLY_READ_ERROR)
        })
      incProgress(0.5, detail = "Running checks and writing outputs…")
      out
    })
  }

  # detected_identity_info() -- who the environment can actually ESTABLISH, without
  # anyone typing anything, AND how it established it. Order of trust:
  #   "host" -- the Shiny host's authenticated user (session$user: Shiny Server Pro,
  #             Posit Connect, RStudio auth). A real, per-person sign-in.
  #   "sso"  -- an identity a reverse proxy / SSO gateway forwards in a request
  #             header (oauth2-proxy, nginx auth_request, IIS/Windows-auth,
  #             Cloudflare Access). Also per-person.
  #   "os"   -- the OS account the SERVER PROCESS runs as. In the shipped
  #             one-server-for-the-team model this is the SAME for everybody and
  #             identifies NOBODY. It is kept (it is still a fact worth logging)
  #             but it must never be presented as the user's sign-in.
  #   "none" -- nothing at all.
  # Header values are only ever stored as a string in the audit log -- never
  # evaluated -- so there is no injection surface.
  .SSO_HEADERS <- c("HTTP_X_FORWARDED_USER", "HTTP_X_AUTH_REQUEST_USER",
    "HTTP_X_AUTH_REQUEST_EMAIL", "HTTP_X_FORWARDED_EMAIL", "HTTP_REMOTE_USER",
    "HTTP_X_REMOTE_USER", "HTTP_X_FORWARDED_PREFERRED_USERNAME", "HTTP_CF_ACCESS_AUTHENTICATED_USER_EMAIL")
  detected_identity_info <- function() {
    su <- session$user
    if (!is.null(su) && nzchar(trimws(su))) return(list(who = trimws(su), source = "host"))
    req <- session$request
    if (!is.null(req)) for (h in .SSO_HEADERS) {
      v <- tryCatch(req[[h]], error = function(e) NULL)
      if (!is.null(v) && nzchar(trimws(v))) return(list(who = trimws(v), source = "sso"))
    }
    cu <- current_user()
    if (!is.null(cu) && nzchar(cu) && !identical(cu, "unknown"))
      return(list(who = cu, source = "os"))
    list(who = NA_character_, source = "none")
  }
  detected_identity <- function() detected_identity_info()$who
  # Is the detected identity actually THIS PERSON (rather than the server's own
  # account)? Only a host/SSO sign-in is; that is the only case where pre-filling
  # the name box or saying "signed in as" is true.
  .identity_is_personal <- function(info) info$source %in% c("host", "sso")
  # who_now() -- what goes in the run log's requested_by: the name typed on
  # Convert, else whatever the environment could establish. NOTE this is the
  # ATTESTED-or-fallback value; it never stands alone in the log any more -- the
  # typed claim and the machine-detected identity are ALSO written as separate
  # fields (see stamp_identity below), so a reader can tell one from the other.
  # WHO RAN THIS, asked at most once a session and only when it cannot be
  # established. NA until given; conversion is blocked until then (see cv_go), so
  # no run is ever recorded against nobody.
  # A QID is six letters or digits. Checked here rather than accepted as free
  # text, because the audit trail is the point of asking: a typo, a name, or an
  # empty string recorded against a conversion is a record nobody can follow back
  # to a person, which is the same as having no record. Stored uppercase so the
  # same person is one identity in the log however they typed it.
  QID_PATTERN <- "^[A-Za-z0-9]{6}$"
  cv_qid <- reactiveVal(NA_character_)
  observeEvent(input$cv_qid_set, {
    v <- trimws(input$cv_qid %||% "")
    if (!grepl(QID_PATTERN, v)) {
      showNotification("A QID is six letters or numbers, e.g. AB1234.",
                       type = "warning", duration = 6)
    } else cv_qid(toupper(v))
  })
  observeEvent(input$cv_qid_change, cv_qid(NA_character_))
  output$cv_whoami <- renderUI({
    # A real per-person sign-in answers this already: ask nothing.
    if (.identity_is_personal(detected_identity_info())) return(NULL)
    q <- cv_qid()
    if (!is.na(q))
      return(div(class = "muted", style = "margin:-2px 0 12px",
                 sprintf("Recording as %s", q), " \u00b7 ",
                 actionLink("cv_qid_change", "change")))
    tagList(
      textInput("cv_qid", "Your QID", value = ""),
      # Say what it is FOR. Never say why it is needed: the old wording announced
      # that this server has no sign-in, which tells every person who opens the
      # page - including anyone who should not be on it - exactly where the door
      # is unlocked. What the tool cannot do is nobody's business but the
      # maintainer's, and it is in the docs where it belongs.
      helpText(style = "margin-top:-6px",
        "Recorded as who ran this. Asked once."),
      actionButton("cv_qid_set", "Use this QID", class = "btn-default"),
      tags$hr(style = "margin:14px 0"))
  })
  who_now <- function() {
    q <- cv_qid()
    if (!is.na(q) && nzchar(q)) return(q)
    detected_identity() %||% (session$user %||% current_user())
  }
  # stamp_identity(run_id) -- write BOTH facts onto the run record: what was typed
  # (a claim) and what the machine could establish (with its source). The engine
  # only knows one string; the UI knows both, so it completes the record here.
  # Never silent: if the record can't be completed (missing, or an ambiguous
  # same-second id) the user is TOLD, because an audit trail that quietly loses the
  # "who" is worse than one that says it did.
  stamp_identity <- function(run_id) {
    rid <- as.character(run_id %||% NA_character_)[1]
    # No run_id means the engine never got far enough to write a record at all
    # (the file could not be read). That failure is already on screen -- there is
    # nothing to amend and nothing extra to warn about.
    if (is.na(rid) || !nzchar(rid)) return(invisible(FALSE))
    info <- detected_identity_info()
    ok <- safe(amend_log_record(LOGDIR, "runs", rid,
      identity_fields(attested = cv_qid(), detected = info$who, source = info$source)), FALSE)
    if (!isTRUE(ok))
      showNotification(paste("This conversion ran, but its audit record could not be completed with who ran it.",
                             "Tell whoever looks after the tool before relying on this run."),
                       type = "warning", duration = 12)
    invisible(ok)
  }
  # run_conversion -- the whole convert-a-file flow (session dir, convert, state,
  # upload capture), shared by the Convert button and "Try it on a sample".
  # record = FALSE skips the Admin uploads capture (the bundled sample is not a
  # team statement to pick up).
  run_conversion <- function(srcpath, name, record = TRUE, force_tpl = NULL,
                             include_user = FALSE) {
    old <- isolate(cv_dir())
    sess <- tempfile("cv_")   # guaranteed-unique per session/process (no cross-user bleed)
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, name)
    file.copy(srcpath, src, overwrite = TRUE)
    # Reclaim the PREVIOUS conversion's scratch folder, AFTER the copy above (a
    # re-convert is often handed the file that lives in it). It holds the outputs
    # AND a copy of the client's statement, and nothing was ever removing it: on a
    # server that runs for months, every conversion by every user stayed on disk for
    # the life of the process. Once a new conversion starts the old outputs are
    # unreachable from the UI anyway.
    if (!is.null(old) && nzchar(old) && !identical(old, sess) && dir.exists(old))
      safe(unlink(old, recursive = TRUE))
    # ...and anything this process left behind more than a day ago, which is what a
    # browser closed abruptly (no onSessionEnded) leaves lying about. The folder
    # this conversion is using, and any file open in the toolkit, are excluded.
    safe(sweep_temp_dirs(keep_hours = 24,
                         exclude = c(sess, dirname(isolate(guided())$path %||% "."))))
    cv_forced(list())   # a new file -> forget any force-included rows from the last one
    who <- who_now()
    res <- convert_now(src, sess, forced_rows = NULL, force_tpl = force_tpl,
                       include_user = include_user)
    cv_res(res); cv_dir(sess); cv_src(list(path = src, name = name))
    cv_fb_done(FALSE); cv_fb_rec(NULL)   # reset the feedback panel for the new conversion
    # Complete the audit record with the attested vs detected identity split.
    stamp_identity(res$run_id %||% NA_character_)
    # Capture the upload + its outcome so a failed/abandoned new format is a
    # 2-second pickup in Admin -> Uploads (the file is saved for a safe re-audit).
    uid <- if (record) safe(record_upload(src, name = name, requested_by = who,
      status = res$status %||% "failed", run_id = res$run_id %||% NA_character_,
      template = res$template_id %||% NA_character_,
      trust = res$trust$level %||% NA_character_,
      detail = paste(res$messages, collapse = "; "), dir = UPLOADS_DIR), NA_character_)
    else NA_character_
    cv_upload_id(uid)
    publish_result(res, record)
  }

  # publish_result(res, record) -- write the governed feed for this conversion and
  # keep the gate's verdict for the screen. ONE place, so what reaches Qlik and
  # what the screen claims about Qlik always come from the same run.
  #
  # Feeding used to be a silent side-effect: the verdict went to the feed manifest
  # and the feed log, and the person who ran the conversion was never told. A clean
  # statement read by a template she built here is withheld ("not_proven") -- and
  # she had every reason to believe her figures were on the dashboard.
  # `record = FALSE` (the bundled sample, and the preview re-convert after saving a
  # template) neither feeds nor claims anything about the feed.
  publish_result <- function(res, record) {
    cv_recorded(isTRUE(record))
    cv_feed_gate(if (isTRUE(record)) safe(write_feed(res, CONFIG), NULL) else NULL)
  }

  observeEvent(input$cv_go, {
    if (is.null(input$cv_file)) {
      showNotification("Choose a statement file first - a PDF, CSV or Excel export from your bank.",
                       type = "warning", duration = 6)
      return()
    }
    # No sign-in and no QID means the run would be recorded against the account the
    # SERVER process runs as -- identical for the whole department, identifying
    # nobody. For a tool whose output is meant to be defensible, that is worse than
    # stopping, so it stops. Once per session, not once per statement.
    if (!.identity_is_personal(detected_identity_info()) && is.na(cv_qid())) {
      showNotification("Enter your QID first - it is what the audit trail records as who ran this conversion.",
                       type = "warning", duration = 8)
      return()
    }
    run_conversion(input$cv_file$datapath, input$cv_file$name)
  })

  # "Try it on a sample": convert the bundled specimen statement, so the very
  # first visit can show the whole payoff (verdict, analysis, downloads) without
  # the user needing a statement at hand.
  observeEvent(input$cv_try_sample, {
    if (!file.exists(SAMPLE_STATEMENT)) {
      showNotification("The bundled sample statement isn't on this install (samples/ folder missing).",
                       type = "warning", duration = 6)
      return()
    }
    run_conversion(SAMPLE_STATEMENT, basename(SAMPLE_STATEMENT), record = FALSE)
  })

  # A result exists once a conversion has run -- gates the whole result scaffold so
  # a first-time visitor never sees bare "Checks / Diagnostics" headers over empty
  # tables (which read as half-built).
  output$cv_has_result <- reactive({ !is.null(cv_res()) })
  outputOptions(output, "cv_has_result", suspendWhenHidden = FALSE)
  # A parse that produced rows: gates the analysis cards / graph / transactions,
  # so an unsupported or failed result shows its verdict + next step, not an
  # empty dashboard of zeros.
  output$cv_has_txns <- reactive({
    res <- cv_res()
    isTRUE((res$status %||% "") %in% c("ok", "needs_review")) &&
      !identical(res$kind, "form") && length(res$outputs %||% character(0)) > 0
  })
  outputOptions(output, "cv_has_txns", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # "Show me how it read this" -- the one control that separates the accountant's
  # view from the evidence view (charter: the interface rule).
  #
  # STICKY BY DESIGN. Once someone opens it, it stays open for every conversion
  # for the rest of their session. That is the whole answer to "some users are
  # more technical than others": we do not ask them which they are, and we do not
  # make them re-open it on every file. They tell us by clicking once, and the
  # tool remembers. Closing it again is equally sticky, so nobody is stuck with a
  # view they did not want.
  cv_detail_open <- reactiveVal(FALSE)
  observeEvent(input$cv_more, cv_detail_open(!isTRUE(cv_detail_open())))
  output$cv_detail_open <- reactive({ isTRUE(cv_detail_open()) })
  outputOptions(output, "cv_detail_open", suspendWhenHidden = FALSE)

  output$cv_more_toggle <- renderUI({
    res <- cv_res(); req(res)
    if (identical(res$kind, "form")) return(NULL)
    open <- isTRUE(cv_detail_open())
    # Named for what is BEHIND it, not for the mechanism. "Advanced" would make an
    # accountant feel it is not for her; "how it read this" is a question she may
    # genuinely want answered, and it is the honest description of the contents.
    div(style = "margin:16px 0 6px",
      actionLink("cv_more", style = "font-weight:700;font-size:14.5px",
        label = if (open) "Hide how it read this" else "Show me how it read this"),
      div(class = "muted", style = "font-size:13px;margin-top:2px",
          if (open) "The page, the checks, and the template it used."
          else "The page, the checks, and the template it used."))
  })

  # Empty state: shown before the first conversion. Tells a brand-new user what
  # this page is for and exactly what they'll get back, so the screen is never a
  # mystery or a wall of empty headers.
  output$cv_empty <- renderUI({
    div(style = "max-width:560px;color:#444;line-height:1.6",
      h4(style = "margin-top:4px", "Convert a bank statement"),
      p("Upload a statement on the left - a ", tags$b("PDF"), ", ", tags$b("CSV"),
        " or ", tags$b("Excel"), " file - and click ", tags$b("Convert"), "."),
      p(class = "muted", style = "margin-bottom:6px", "You'll get back, right here:"),
      tags$ul(style = "color:#444",
        tags$li(tags$b("Every transaction"), ", read verbatim - date, description, amount, balance."),
        tags$li(tags$b("Proof nothing's missing"), " - the balance reconciles, with a plain confidence level."),
        tags$li(tags$b("Your download"), " - Excel, CSV or JSON.")),
      p(class = "muted", "Your bank is detected automatically. A layout the tool hasn't seen points you to ",
        actionLink("cv_empty_to_tmpl", "Add a template"), " - a 2-minute, no-code setup."),
      # First visit, nothing to upload yet? One click shows the whole payoff on
      # a bundled specimen statement (public sample - not anyone's real data).
      if (file.exists(SAMPLE_STATEMENT))
        div(style = "margin-top:14px;padding:12px 14px;background:#f8faf9;border:1px dashed #bfe0c8;border-radius:10px",
          actionButton("cv_try_sample", "Try it on a sample statement", class = "btn-default"),
          div(class = "muted", style = "margin-top:6px",
              "No file needed - converts a bundled specimen so you can see a full result in seconds.")))
  })

  output$cv_status <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    # A successful transaction statement gets the plain hero headline (cv_headline
    # below); this card is kept for form results and for anything that did NOT
    # convert cleanly, so failures still explain themselves up top. It is the SAME
    # verdict card as the success headline -- one visual language for "how did it
    # go", rather than a second, hand-coloured one for bad news.
    if (isTRUE(res$status == "ok") && !identical(res$kind, "form")) return(NULL)
    st <- res$status %||% "failed"
    lvl <- switch(st, ok = "high", needs_review = , unsupported = "medium", "low")
    # A tie is not "no template yet" (STATUS_PLAIN_AMBIGUOUS), and it has no
    # confidence to quote -- nothing was converted to be confident about.
    ambig <- isTRUE(res$detect$ambiguous)
    headline <- if (ambig) STATUS_PLAIN_AMBIGUOUS else plain_status(st)
    # No confidence grade on a run that produced nothing: there is no work to be
    # confident about, and "confidence: low" beside "no template yet" reads as a
    # warning about the file rather than a plain statement of where we are.
    graded <- !ambig && st %in% c("ok", "needs_review")
    trust <- if (!is.null(res$trust) && graded) sprintf(" · confidence: %s", res$trust$level) else ""
    # Name the template ONLY when one was actually used to read the statement. On
    # an unsupported result the engine still carries a template id -- the CLOSEST
    # MISS, kept for the logs -- and printing it under "No template for this
    # statement yet" read as though that template had read the file. It had not.
    tid <- if (st %in% c("ok", "needs_review")) (res$template_id %||% NA_character_)[1]
           else NA_character_
    div(class = paste0("verdict verdict-", lvl),
      div(class = "verdict-ico", if (identical(lvl, "high")) "✓" else "!"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", paste0(headline, trust)),
        # Engine messages carry a leading machine code ("needs_review: ...") for the
        # logs; the card headline already says it in words, so drop the code here.
        lapply(sub("^(ok|needs_review|unsupported|failed):\\s*", "",
                   as.character(res$messages %||% character(0))),
               function(m) p(class = "verdict-body", m)),
        failed_checks_ui(res),
        if (!is.na(tid) && nzchar(tid))
          div(span(class = "chip", paste("Read as:", friendly_tpl(tid))))))
  })

  # failed_checks_ui(res) -- WHICH check failed, in the words the Checks table
  # uses, with the engine's own figures beside it.
  #
  # The engine knows exactly which of the ten checks failed and by how much; the
  # card only ever showed its message, whose reason fragment names the check by
  # its internal code ("1 KPI(s) failed: balance_reconciliation") and gives no
  # figures. So the one thing a reviewer needs on a needs-review result -- what to
  # go and look at -- was a click away at the bottom of the page under "Checks &
  # detail". Same source of truth as that table (res$kpis) and the same wording
  # (CHECK_PLAIN), so the two can never say different things. NULL when nothing
  # failed, so a clean result is unchanged.
  failed_checks_ui <- function(res) {
    k <- res$kpis
    if (is.null(k) || !all(c("name", "status") %in% names(k))) return(NULL)
    f <- k[k$status %in% "fail", , drop = FALSE]
    if (!nrow(f)) return(NULL)
    div(class = "verdict-body", style = "margin-top:2px",
      tags$b("What to check:"),
      tags$ul(style = "margin:4px 0 0 18px;padding:0",
        lapply(seq_len(nrow(f)), function(i) tags$li(
          tags$b(plain_check(f$name[i])),
          if (nzchar(f$detail[i] %||% "")) sprintf(" - %s", f$detail[i]) else NULL))))
  }

  # THE CHECKS THAT MATTER, ALWAYS ON SCREEN.
  #
  # Only FAILING checks were shown. That reads as "no news is good news", and for a
  # tool whose whole purpose is a defensible figure it is the wrong way round: the
  # reason to trust this output is that the opening balance plus every transaction
  # equals the closing balance the statement prints, and a passing proof said
  # nothing at all. It sat inside a panel nobody opens on a clean run.
  #
  # These four are the ones a forensic reviewer would ask about, in the order they
  # would ask. Everything else stays in the full checks table.
  #   - does it add up (the cardinal proof)
  #   - was every row read
  #   - does the running balance follow from row to row
  #   - could every date be read
  # A dash means the check could not run on this statement (no printed closing
  # balance, no running balance column) - which is a fact worth seeing, not a pass.
  .PROOF_CHECKS <- c("balance_reconciliation", "no_unparsed_rows",
                     "running_balance_continuity", "dates_readable")
  output$cv_proof <- renderUI({
    res <- cv_res(); req(res)
    k <- res$kpis
    if (is.null(k) || !all(c("name", "status") %in% names(k))) return(NULL)
    if (!nrow(k)) return(NULL)
    pick <- .PROOF_CHECKS[.PROOF_CHECKS %in% k$name]
    if (!length(pick)) return(NULL)
    chip <- function(nm) {
      st <- k$status[k$name == nm][1]
      m <- switch(st %||% "na",
        pass = list("\u2713", "#137333", "#e9f5ec"),
        fail = list("\u2717", "#b00020", "#fdecef"),
        list("\u2013", "#6b7780", "#f2f4f6"))     # na / anything else: could not run
      span(style = sprintf(paste0("display:inline-flex;align-items:center;gap:6px;",
                                  "margin:0 8px 6px 0;padding:5px 10px;border-radius:999px;",
                                  "background:%s;color:%s;font-size:13px;font-weight:600"),
                           m[[3]], m[[2]]),
           span(style = "font-size:14px", m[[1]]), plain_check(nm))
    }
    div(style = "margin:2px 0 12px", lapply(pick, chip))
  })

  # friendly_tpl -- turn a template id (e.g. "bnz_everyday_csv") into a name Beth
  # reads ("BNZ everyday statement"). Falls back to the id if we can't resolve it.
  friendly_tpl <- function(tid) {
    if (length(tid) != 1 || is.na(tid) || !nzchar(tid)) return(NA_character_)
    # Build the overview for JUST this template, not the whole set: friendly_tpl
    # runs on every successful convert and only needs this id's bank + type, and
    # the full-set build grows with every template the team adds. Same function,
    # one-element input -> identical row (missing id still falls back to `tid`).
    ov <- tryCatch(template_overview(all_templates()[tid]), error = function(e) NULL)
    if (is.null(ov) || !nrow(ov)) return(tid)
    r <- ov[ov$id == tid, , drop = FALSE]
    if (!nrow(r)) return(tid)
    lab <- trimws(paste(trimws(r$bank[1] %||% ""), trimws(r$type[1] %||% "")))
    if (nzchar(lab)) paste(lab, "statement") else tid
  }

  # cv_headline -- the EASY, plain-English verdict for a transaction result: did it
  # work, how many transactions, and can I trust it, said in words rather than KPI
  # codes. This is what a non-technical reviewer reads first; the KPI tables stay
  # available under "Checks & detail".
  # The medium line used to assert its own cause ("usually because this statement
  # has no running balance"). The engine KNOWS the cause -- and it is just as often
  # OCR, or a year it had to infer -- so the sentence sat directly under a chip
  # contradicting it. It now says which of the two it is, and trust_caveats()
  # below prints the engine's remaining reasons verbatim rather than guessing.
  plain_trust <- function(trust) {
    proven <- !isFALSE(trust$completeness_verified)
    switch(trust$level %||% "",
      high   = list(cls = "ok",   icon = "✓", line = "Every transaction adds up to the closing balance the statement prints. Nothing is missing."),
      medium = list(cls = "warn", icon = "✓", line = if (proven)
          "Read cleanly. One thing could not be proven — it is named just below."
        else
          "Read cleanly. This statement prints no closing balance, so check the number of rows against it."),
      low    = list(cls = "bad",  icon = "!",      line = "Check these against the statement before you use them."),
      list(cls = "warn", icon = "✓", line = "Read cleanly. Check the number of rows against the statement."))
  }
  # Row FLAGS worth a chip: things the engine recorded per row that a clean-looking
  # result would otherwise never mention on screen. Each caps the trust level in
  # R/reconcile.R, and each was reaching only the workbook -- most importantly the
  # inferred year, which makes dates that LOOK proven merely likely. Counted off
  # the flags column of the produced table, so the chip and the file agree.
  ROW_FLAG_CHIPS <- c(
    date_year_inferred = "%d row(s) took their YEAR from a number on the page, not a statement period - confirm it",
    date_unresolved    = "%d row(s) have no year at all (none was printed) - day and month only",
    ocr_low_conf       = "%d row(s) have a machine-read date/amount the scan was unsure of - check those cells")
  output$cv_headline <- renderUI({
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
    if (!isTRUE(res$status == "ok")) return(NULL)   # failures are shown by cv_status
    d <- cv_data(); n <- if (is.null(d)) NA_integer_ else nrow(d)   # reuse the shared read
    pt <- plain_trust(res$trust %||% list())
    lvl <- c(ok = "high", warn = "medium", bad = "low")[[pt$cls]]
    # Small honest-flags row: which template read it, and anything a reviewer
    # should know at a glance (OCR pages, honoured redactions, hand-added rows).
    # All of this already exists in the result - it was just buried in the tables.
    chip <- function(txt, warn = FALSE)
      span(class = if (warn) "chip chip-warn" else "chip", txt)
    chips <- list()
    tid <- (res$template_id %||% NA_character_)[1]
    if (!is.na(tid) && nzchar(tid)) chips <- c(chips, list(chip(paste("Read as:", friendly_tpl(tid)))))
    op <- suppressWarnings(as.integer(res$trust$ocr_pages %||% 0L))
    if (isTRUE(op > 0)) chips <- c(chips, list(chip(
      sprintf("%d page(s) machine-read (OCR) - double-check the numbers", op), warn = TRUE)))
    k <- res$kpis
    if (!is.null(k) && "name" %in% names(k)) {
      nred <- suppressWarnings(as.integer(k$actual[k$name == "redaction_summary"][1]))
      if (isTRUE(nred > 0)) chips <- c(chips, list(chip(
        sprintf("%d redacted row(s) honoured - hidden values stay hidden", nred))))
    }
    if (length(cv_forced())) chips <- c(chips, list(chip(
      sprintf("%d row(s) added by hand - flagged 'forced' in the output", length(cv_forced())),
      warn = TRUE)))
    fl <- if (!is.null(d) && "flags" %in% names(d)) as.character(d$flags) else character(0)
    for (f in names(ROW_FLAG_CHIPS)) {
      nf <- sum(grepl(f, fl, fixed = TRUE))
      if (nf > 0) chips <- c(chips, list(chip(sprintf(ROW_FLAG_CHIPS[[f]], nf), warn = TRUE)))
    }
    div(class = paste0("verdict verdict-", lvl),
      div(class = "verdict-ico", pt$icon),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", sprintf("Converted%s",
          if (!is.na(n)) sprintf(" — %d transaction%s read", n, if (n == 1) "" else "s") else "")),
        p(class = "verdict-body", pt$line),
        if (length(chips)) div(chips)))
  })

  # --- Analysis: the useful numbers + graphs pulled from the conversion -------
  # The displayed transactions come from the produced CSV; read them once here as
  # a data frame for the summary cards and the trend graph. No new dependency -
  # base graphics, the same ones the X-ray uses.
  cv_data <- reactive({
    res <- cv_res(); if (is.null(res) || is.null(res$outputs)) return(NULL)
    csv <- res$outputs[grepl("\\.csv$", res$outputs)]
    if (length(csv) != 1 || !file.exists(csv)) return(NULL)
    d <- tryCatch(utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d)) return(NULL)
    d$.date <- suppressWarnings(as.Date(d$date))
    d$.amt  <- suppressWarnings(as.numeric(d$amount))
    d$.bal  <- if ("balance" %in% names(d)) suppressWarnings(as.numeric(d$balance)) else NA_real_
    d
  })
  fmt_money <- function(x, cur = "") {
    if (length(x) != 1 || is.na(x)) return("-")
    sprintf("%s%s%s", if (x < 0) "-" else "", cur,
            formatC(abs(x), format = "f", digits = 2, big.mark = ","))
  }
  cur_symbol <- function(h) switch(h$currency %||% "", NZD = "$", AUD = "$", USD = "$",
                                   GBP = "£", EUR = "€", "")

  output$cv_summary <- renderUI({
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
    d <- cv_data(); h <- res$header %||% list(); cur <- cur_symbol(h)
    n   <- if (!is.null(d)) nrow(d) else (h$row_count %||% NA)
    amt <- if (!is.null(d)) d$.amt[!is.na(d$.amt)] else numeric(0)
    money_in <- sum(amt[amt > 0]); money_out <- sum(amt[amt < 0]); net <- sum(amt)
    drange <- if (!is.null(d) && any(!is.na(d$.date)))
        sprintf("%s to %s", format(min(d$.date, na.rm = TRUE), "%d %b %Y"),
                format(max(d$.date, na.rm = TRUE), "%d %b %Y"))
      else if (!is.na(h$period_start %||% NA_character_))
        sprintf("%s to %s", h$period_start, h$period_end %||% "?") else "-"
    card <- function(label, value, col = NULL)
      div(class = "stat",
          div(class = "stat-label", label),
          div(class = "stat-value", style = if (!is.null(col)) sprintf("color:%s", col) else NULL, value))
    has_close <- !is.na(suppressWarnings(as.numeric(h$closing_balance %||% NA)))
    tagList(
      div(class = "stat-grid",
        card("Transactions", if (is.na(n)) "-" else n),
        card("Money in",  fmt_money(money_in, cur),  "#137333"),
        card("Money out", fmt_money(money_out, cur), "#b00020"),
        card("Net",       fmt_money(net, cur), if (isTRUE(net < 0)) "#b00020" else "#137333"),
        if (has_close) card("Closing balance", fmt_money(as.numeric(h$closing_balance), cur))),
      p(class = "muted", style = "margin:0 0 4px", sprintf("Period: %s%s%s", drange,
        if (!is.na(h$account_number %||% NA_character_)) sprintf("  ·  Account: %s", h$account_number) else "",
        if (!is.na(h$bank %||% NA_character_)) sprintf("  ·  %s", h$bank) else "")))
  })

  # cv_split -- an auto-split bundle, statement by statement.
  #
  # When a template opts into auto-split, the engine parses and reconciles each
  # statement in the file SEPARATELY and keeps every one's period, balances, row
  # count and confidence in result$metadata$split$statements. The screen said only
  # "auto-split into 5 statements" and then showed one set of summary cards for the
  # whole bundle -- so the combined period, and a single confidence level that is
  # really the WEAKEST statement's, read as if they described one statement. This
  # is the same table the feed stamps onto each row, shown to the person reviewing.
  output$cv_split <- renderUI({
    res <- cv_res(); req(res)
    sts <- res$metadata$split$statements
    if (is.null(sts) || !length(sts)) return(NULL)
    cell <- function(v) { v <- as.character(v %||% NA)[1]; if (is.na(v) || !nzchar(v)) "-" else v }
    hdr <- c("Statement", "Pages", "Period", "Opening", "Closing", "Rows", "Confidence")
    tagList(
      h4(sprintf("The %d statements in this file", length(sts))),
      p(class = "muted", style = "margin:0 0 6px",
        paste("Each one was read and reconciled on its own. The cards above cover the whole file,",
              "and its confidence level is the weakest statement's; every output row carries a",
              "statement_index column saying which statement it came from.")),
      tags$table(class = "split-table",
        tags$thead(tags$tr(lapply(hdr, tags$th))),
        tags$tbody(lapply(sts, function(s) tags$tr(
          tags$td(cell(s$index)), tags$td(cell(s$pages)),
          tags$td(sprintf("%s to %s", cell(s$period_start), cell(s$period_end))),
          tags$td(cell(s$opening_balance)), tags$td(cell(s$closing_balance)),
          tags$td(cell(s$rows)), tags$td(cell(s$trust_level)))))))
  })

  output$cv_trend_note <- renderUI({
    req(cv_data())
    msg <- switch(input$an_view %||% "inout",
      inout   = "Green = money in, red = money out.",
      balance = "The running balance as it moves through the statement (only if the statement shows a balance column).",
      cumnet  = "The running total of every transaction added up over time - where the account net sits at each point.")
    p(class = "muted", style = "margin:6px 0 0", msg)
  })

  output$cv_trend <- renderPlot({
    d <- cv_data(); req(d); d <- d[!is.na(d$.date), , drop = FALSE]; req(nrow(d) > 0)
    view <- input$an_view %||% "inout"; grp <- input$an_group %||% "week"; unit <- input$an_unit %||% "amount"
    # On-brand, low-chrome base-R chart: no plot box, light gridlines behind,
    # human date labels and a $k money axis, so it reads as product, not raw plot.
    # Green = money in, red = out (their meaning everywhere); brand blue for the
    # neutral balance / cumulative lines, softly area-filled so they read as product.
    GREEN <- "#0b7a34"; RED <- "#b00020"; BLUE <- "#00205b"; BLUE_FILL <- "#00205b1f"
    INK <- "#1f2a33"; GRID <- "#eceef1"; AXIS <- "#6b7280"
    fmt_k <- function(v) ifelse(abs(v) >= 1000,
      paste0(formatC(v / 1000, format = "f", digits = 1), "k"),
      formatC(v, format = "f", digits = 0, big.mark = ","))
    money_lab <- function(at) if (unit == "count") formatC(at, format = "d", big.mark = ",") else paste0("$", fmt_k(at))
    op <- par(mar = c(4, 4.8, 0.6, 1), mgp = c(3, 0.5, 0), tcl = -0.2, family = "sans",
              col.axis = AXIS, col.lab = INK, cex.axis = 0.9, cex.lab = 1, xpd = FALSE)
    on.exit(par(op))
    xdate <- function(dates) axis.Date(1, x = dates, format = if (grp == "month") "%b %Y" else "%d %b",
                                       col = NA, col.ticks = NA, col.axis = AXIS)
    area_line <- function(x, y, col, fill) {
      polygon(c(x[1], x, x[length(x)]), c(min(y, 0), y, min(y, 0)), col = fill, border = NA)
      lines(x, y, col = col, lwd = 2.6)
      points(x, y, pch = 21, cex = 0.65, col = "#fff", bg = col, lwd = 1)
    }
    if (view == "balance") {
      b <- d[!is.na(d$.bal), , drop = FALSE]
      if (!nrow(b)) { plot.new(); text(0.5, 0.5, "This statement has no running balance column.", col = AXIS); return(invisible()) }
      b <- b[order(b$.date), , drop = FALSE]; aty <- pretty(range(b$.bal, na.rm = TRUE))
      plot(b$.date, b$.bal, type = "n", axes = FALSE, xlab = "", ylab = "Balance", ylim = range(aty))
      abline(h = aty, col = GRID)
      area_line(b$.date, b$.bal, BLUE, BLUE_FILL)
      axis(2, at = aty, labels = money_lab(aty), col = NA, col.ticks = NA, las = 1); xdate(b$.date)
    } else if (view == "cumnet") {
      dd <- d[order(d$.date), , drop = FALSE]; cn <- cumsum(ifelse(is.na(dd$.amt), 0, dd$.amt))
      aty <- pretty(range(c(0, cn)))
      plot(dd$.date, cn, type = "n", axes = FALSE, xlab = "", ylab = "Running total", ylim = range(aty))
      abline(h = aty, col = GRID); abline(h = 0, col = "#c3c9d2", lwd = 1.2)
      area_line(dd$.date, cn, BLUE, BLUE_FILL)
      axis(2, at = aty, labels = money_lab(aty), col = NA, col.ticks = NA, las = 1); xdate(dd$.date)
    } else {
      key <- switch(grp, day = d$.date, week = as.Date(cut(d$.date, "week")), month = as.Date(cut(d$.date, "month")))
      lv  <- sort(unique(key)); pf <- factor(as.character(key), levels = as.character(lv))
      val_in  <- ifelse(d$.amt > 0, if (unit == "count") 1 else d$.amt, 0)
      val_out <- ifelse(d$.amt < 0, if (unit == "count") 1 else -d$.amt, 0)
      ins  <- tapply(val_in,  pf, sum); ins[is.na(ins)]  <- 0
      outs <- tapply(val_out, pf, sum); outs[is.na(outs)] <- 0
      m <- rbind(as.numeric(ins), as.numeric(outs))
      # ~18% headroom above the tallest bar so the top value label and the legend
      # both clear the bars instead of colliding with them.
      aty <- pretty(c(0, max(m, 1, na.rm = TRUE) * 1.18))
      bp <- barplot(m, beside = TRUE, col = c(GREEN, RED), border = NA, axes = FALSE,
                    names.arg = rep("", ncol(m)), ylim = range(aty), space = c(0.1, 0.8),
                    ylab = if (unit == "count") "Transactions" else "Dollars")
      abline(h = aty, col = GRID)   # gridlines behind, then re-draw bars on top
      barplot(m, beside = TRUE, col = c(GREEN, RED), border = NA, axes = FALSE, add = TRUE,
              names.arg = rep("", ncol(m)), space = c(0.1, 0.8))
      axis(2, at = aty, labels = money_lab(aty), col = NA, col.ticks = NA, las = 1)
      # Value labels above each bar when the period count is small enough to stay legible.
      if (ncol(m) <= 8) {
        lab_v <- function(v) ifelse(v <= 0, "", if (unit == "count") formatC(v, format = "d") else paste0("$", fmt_k(v)))
        text(as.numeric(bp), as.numeric(m), labels = lab_v(as.numeric(m)),
             pos = 3, offset = 0.25, cex = 0.68, col = AXIS, xpd = NA)
      }
      lab <- switch(grp, month = format(lv, "%b %Y"), format(lv, "%d %b"))
      axis(1, at = colMeans(bp), labels = lab, col = NA, col.ticks = NA,
           las = if (length(lv) > 8) 2 else 1, cex.axis = if (length(lv) > 8) 0.75 else 0.9)
      legend("topleft", legend = c(if (unit == "count") "In" else "Money in", if (unit == "count") "Out" else "Money out"),
             fill = c(GREEN, RED), border = NA, bty = "n", cex = 0.9, horiz = TRUE)
    }
  })
  # Is this result a form (labelled values) rather than a transaction statement?
  output$cv_is_form <- reactive({ isTRUE((cv_res()$kind %||% "") == "form") })
  outputOptions(output, "cv_is_form", suspendWhenHidden = FALSE)
  output$cv_form <- renderUI({
    res <- cv_res(); req(res); req(identical(res$kind, "form"))
    tagList(
      # The card above already says how it went and which template read it, so this
      # says the one thing that card cannot: WHY there are no completeness checks on
      # a form, and therefore what the reviewer has to do instead.
      p(class = "muted", style = "margin:8px 0 12px; max-width:760px",
        "This is a form / labelled-value document, not a transaction statement: there is no transaction table and no running balance, so the completeness checks don't apply. Check each value below against the document. Your download is at the top of this page."),
      h4("Values found"), DTOutput("cv_fields"))
  })
  output$cv_fields <- renderDT({
    res <- cv_res(); req(res, !is.null(res$fields))
    d <- res$fields[, intersect(c("field", "label", "value", "matched", "required", "flagged"),
                                names(res$fields)), drop = FALSE]
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 30))
  })

  output$cv_kpis <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$kpis))
    k <- res$kpis
    # Show a plain-English check name + a plain result word, not snake_case codes.
    disp <- data.frame(
      Check  = plain_check(k$name),      # split-aware: "... (statement 2)"
      Result = plain_label(k$status, RESULT_PLAIN),
      Detail = if ("detail" %in% names(k)) k$detail else "",
      stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("Result", fontWeight = "bold",     # coloured off the same map
        color = styleEqual(unname(RESULT_PLAIN[c("pass", "fail")]), c("#137333", "#b00020")))
  })

  output$cv_diag <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$diagnostics))
    # Customer-facing: where / why / how-to-fix only. The fix-ownership triage
    # (template vs engine-gap vs escalate) is maintainer-only and lives on the
    # Admin tab, never here. Category codes render as plain words.
    d <- res$diagnostics[, intersect(c("where", "category", "severity", "detail", "how_to_fix"),
                                     names(res$diagnostics)), drop = FALSE]
    if ("category" %in% names(d)) d$category <- plain_label(d$category, DIAG_PLAIN)
    names(d) <- plain_label(names(d), c(where = "Where", category = "What",
                                        severity = "Severity", detail = "Detail",
                                        how_to_fix = "How to fix"))
    datatable(d, rownames = FALSE,
              options = list(dom = "t", pageLength = 20, scrollX = TRUE))
  })

  # "Checks & detail" is depth-as-an-option on a clean result -- and NOT optional
  # when the tool has just asked for a second pair of eyes. Anything other than a
  # clean pass (a non-ok status, or any check that FAILED) opens the section, so the
  # reasons sit in front of the reviewer instead of one collapsed click behind the
  # chart. A clean, fully-passing conversion still starts tidy.
  output$cv_detail <- renderUI({
    res <- cv_res(); req(res)
    k <- res$kpis
    any_failed <- !is.null(k) && "status" %in% names(k) && any(k$status %in% "fail")
    open_it <- !isTRUE((res$status %||% "") == "ok") || isTRUE(any_failed)
    tags$details(style = "margin-top:14px", open = if (open_it) NA else NULL,
      tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                   "Checks & detail (for review)"),
      div(style = "padding:8px 2px",
        h4("Checks"), DTOutput("cv_kpis"),
        h4("Diagnostics - where / why / how to fix"), DTOutput("cv_diag"),
        h4("Field coverage - what's present / empty / not on this statement"),
        uiOutput("cv_cov_summary"), DTOutput("cv_coverage")))
  })

  output$cv_cov_summary <- renderUI({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    p(class = "muted", coverage_summary(res$coverage))
  })
  output$cv_coverage <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    cov <- res$coverage[res$coverage$verdict != "unmapped" | res$coverage$field %in% c("balance","particulars","reference"), ]
    disp <- data.frame(
      field   = cov$field,
      verdict = plain_label(cov$verdict, COVERAGE_PLAIN),   # 'unmapped' -> 'not on this statement'
      populated = cov$populated, empty = cov$empty, note = cov$note,
      stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("verdict",
        backgroundColor = styleEqual(unname(COVERAGE_PLAIN),
                                     c("#e6f4ea","#fff8e6","#fde7e7","#f2f2f2")))
  })

  output$cv_txns <- renderDT({
    # Reuse the already-read output CSV (cv_data) rather than reading it from disk a
    # second time -- by the time this Preview tab renders, cv_headline / cv_summary /
    # cv_trend have all consumed cv_data(). Drop its three derived helper columns
    # (.date/.amt/.bal) so the display shape is exactly what read.csv gave before.
    d <- cv_data(); req(!is.null(d))
    df <- d[, setdiff(names(d), c(".date", ".amt", ".bal")), drop = FALSE]
    # The CSV is already the display shape (no verbatim *_raw; debit/credit when the
    # statement splits them). Trim columns this statement never fills so the table
    # shows only fields that were actually read.
    df <- df[, .cols_with_data(df), drop = FALSE]
    # Beth reads Date, Description, Amount, Balance first; the bank-technical
    # columns (in/out, particulars / code / reference / type / ...) follow. The
    # internal row id ("#") is dropped from the preview - the "Showing N" line
    # already counts, and the downloaded file keeps it.
    df <- df[, setdiff(names(df), "row_id"), drop = FALSE]
    lead <- intersect(c("date", "description", "amount", "debit", "credit", "balance",
                        "direction", "type", "reference", "particulars", "code", "other_party"),
                      names(df))
    df <- df[, c(lead, setdiff(names(df), lead)), drop = FALSE]
    datatable(df, rownames = FALSE, colnames = cv_friendly_cols(names(df)),
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # need_file(p) -- a download with nothing to give tells the user (a toast) and
  # aborts, instead of handing the browser an empty "NA" file. `character(0)` is
  # NOT NULL, so an unsupported/failed result (outputs = character(0)) must be
  # length-checked, not null-checked.
  need_file <- function(p) {
    if (length(p) != 1 || is.na(p) || !nzchar(p) || !file.exists(p)) {
      showNotification("Nothing to download - this produced no output (convert/run it first).",
                       type = "warning", duration = 6)
      req(FALSE)
    }
    p
  }
  # dl_buttons(outputs, ids) -- a Download button ONLY for formats actually produced
  # (e.g. no Excel on a host without openxlsx), so no button promises a missing file.
  # Excel is the primary (btn-primary) since it's what most reviewers want.
  dl_buttons <- function(outputs, ids) {
    labs <- c(xlsx = "⭳ Excel", csv = "⭳ CSV", json = "⭳ JSON")
    has <- function(ext) any(grepl(paste0("\\.", ext, "$"), outputs %||% character(0)))
    Filter(Negate(is.null), lapply(names(ids), function(ext)
      if (has(ext)) downloadButton(ids[[ext]], labs[[ext]],
        class = if (ext == "xlsx") "btn-primary" else NULL)))
  }
  output$cv_downloads <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    btns <- dl_buttons(res$outputs, c(xlsx = "dl_xlsx", csv = "dl_csv", json = "dl_json"))
    if (!length(btns)) return(NULL)
    # Prominent bar right under the verdict: the download is the point of the page,
    # so it's the most visible thing, not a quiet box tucked into the sidebar.
    div(class = "dl-hero", span(class = "dl-hero-label", "Download your converted data:"), btns)
  })

  # cv_feed -- did this conversion reach the org dashboards, and if not, why not?
  #
  # write_feed() already decides that, records it in the feed manifest and logs it;
  # this is the ONE place the person who ran the conversion is told. It matters
  # most on the quiet case: a perfectly good conversion read by a template built
  # here is withheld as "not proven", which is correct governance and was
  # completely invisible. Wording lives in FEED_PLAIN (ui_labels.R), keyed by the
  # engine's own gate reason, so the screen and the manifest cannot drift apart.
  output$cv_feed <- renderUI({
    res <- cv_res(); req(res)
    # Only where there were figures to publish. On an unsupported / failed result
    # the card above already says nothing was converted, and repeating it here
    # would push the "set this layout up" action down the page for no new fact.
    if (!isTRUE((res$status %||% "") %in% c("ok", "needs_review"))) return(NULL)
    fb <- plain_feed(cv_feed_gate())
    if (is.null(fb)) return(NULL)          # feed switched off, or a form result
    div(style = paste("display:flex;gap:8px;align-items:baseline;flex-wrap:wrap;",
                      "margin:0 0 12px;font-size:13px"),
      span(class = if (isTRUE(fb$ok)) "ok" else "bad", style = "font-weight:600", fb$line),
      span(class = "muted", fb$why))
  })
  # cv_rematch -- an obvious, always-there escape hatch for a WRONG match: a bank
  # that matched the wrong template (or the wrong bank) needs a one-click "no, set
  # up the right template for this" without hunting. Drafts a fresh template from
  # THIS file (never seeded from the wrong match).
  output$cv_rematch <- renderUI({
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
    st <- res$status %||% "failed"
    if (!(st %in% c("ok", "needs_review"))) return(NULL)   # unsupported/failed already prompt setup
    tid <- (res$template_id %||% NA_character_)[1]
    nice <- if (!is.na(tid) && nzchar(tid)) friendly_tpl(tid) else NA_character_
    if (identical(st, "ok")) {
      # Happy path: one quiet line. No "did I pick the right template?" doubt.
      div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:8px;margin:0 0 12px;color:var(--muted);font-size:13px",
        span(if (!is.na(nice)) sprintf("Read as %s.", nice) else "Read automatically."),
        actionLink("cv_rematch_go", "Wrong bank? Set up the right one"))
    } else {
      # needs_review: make the "fix the match" option clearly available.
      div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:8px;margin:0 0 12px;font-size:13px",
        span(if (!is.na(nice)) sprintf("Read as %s — worth checking it's the right match.", nice)
             else "Please check this is the right match."),
        actionButton("cv_rematch_go", "Set up the right template", class = "btn-warning btn-sm"))
    }
  })
  observeEvent(input$cv_rematch_go, {
    src <- cv_src(); req(src)
    # Fresh draft from the file itself, never seeded from the wrong match.
    open_guided(src$path, src$name, seed_tmpl = NULL, upload_id = cv_upload_id())
  })
  mk_dl <- function(ext) downloadHandler(
    filename = function() {
      p <- cv_res()$outputs[grepl(paste0("\\.", ext, "$"), cv_res()$outputs)]
      if (length(p)) basename(p[1]) else paste0("download.", ext)
    },
    content = function(file) {
      p <- cv_res()$outputs[grepl(paste0("\\.", ext, "$"), cv_res()$outputs)]
      file.copy(need_file(if (length(p)) p[1] else NA_character_), file, overwrite = TRUE)
    })
  output$dl_xlsx <- mk_dl("xlsx"); output$dl_csv <- mk_dl("csv"); output$dl_json <- mk_dl("json")

  # ---- Feedback (every conversion can be rated; one file per logs/feedback/) ----
  output$cv_feedback <- renderUI({
    res <- cv_res(); if (is.null(res) || is.null(res$run_id)) return(NULL)
    if (isTRUE(cv_fb_done()))
      return(div(style = "margin-top:16px", span(class = "ok",
        "Thanks - your feedback was recorded."), cv_fb_note()))
    div(style = "margin-top:16px;padding:12px;border:1px solid #ddd;border-radius:6px",
        h4("Was this conversion correct?"),
        # choiceNames/choiceValues (not named choices): a non-ASCII name in
        # c(name = value) becomes a SYMBOL at parse time, which on a C-locale
        # host mangles to '<U+2713>'. Lists of plain literals stay UTF-8.
        radioButtons("cv_fb_verdict", NULL, inline = TRUE,
          choiceNames = list("✓ Correct", "△ Minor issues", "✗ Wrong"),
          choiceValues = list("correct", "minor_issues", "wrong")),
        textAreaInput("cv_fb_comment", "Comment (optional - what was wrong?)",
                      width = "100%", rows = 2),
        actionButton("cv_fb_submit", "Submit feedback", class = "btn-primary"))
  })

  # Marking a conversion WRONG does more than log an opinion: submit_feedback()
  # withdraws that run's rows from the accepted feed (retract_feed, R/feed.R), so
  # figures a forensic accountant has called wrong stop reaching the dashboards.
  # The screen said only "Thanks - your feedback was recorded", which is the one
  # consequence she most needs confirmed. It now says what was withdrawn.
  cv_fb_note <- function() {
    rec <- cv_fb_rec(); if (is.null(rec)) return(NULL)
    n <- suppressWarnings(as.integer(rec$retracted_rows %||% NA))
    if (!identical(rec$verdict, "wrong")) return(NULL)
    div(class = "muted", style = "margin-top:4px",
      if (is.na(n))
        "Because you marked it wrong, the tool tried to withdraw its rows from the dashboards but could not reach the feed folder - tell whoever looks after the server."
      else if (n > 0)
        sprintf("Because you marked it wrong, %d row(s) have been withdrawn from the dashboards and moved to the held-back feed. Fixing the template and converting again puts the corrected figures back.", n)
      else
        "Nothing to withdraw from the dashboards - this conversion had not been published to them.")
  }
  observeEvent(input$cv_fb_submit, {
    res <- cv_res(); req(res, res$run_id)
    rec <- tryCatch(
      submit_feedback(run_id = res$run_id, verdict = input$cv_fb_verdict,
                      comment = input$cv_fb_comment, requested_by = who_now(),
                      template_id = res$template_id, logdir = LOGDIR,
                      # The app's own settings, not a second read of the file: the
                      # retraction must target the SAME feed folder write_feed used.
                      config = CONFIG),
      error = function(e) NULL)
    cv_fb_rec(rec)
    cv_fb_done(!is.null(rec))
    if (is.null(rec))
      showNotification("Could not save feedback.", type = "error")
  })

  # ---- Guided setup: teach the tool from a statement it couldn't read ----
  guided <- reactiveVal(NULL)   # list(path, name, tmpl)

  # "__report__" is the escape hatch: picking it means "none of these fit" and
  # reveals the "tell our team" box. guided_live treats it as no-override.
  REPORT_OPT <- stats::setNames("__report__", "None of these - tell our team")
  guided_date_choices <- function(extra = NULL) {
    base <- setNames(vapply(wd_date_table(), `[[`, "", "fmt"),
                     vapply(wd_date_table(), `[[`, "", "label"))
    # Always include the working template's OWN date format, even if it isn't one
    # of the standard options - so an exotic format set on the Advanced tab stays
    # selectable and is never silently reverted to a list value by guided_live().
    if (!is.null(extra) && nzchar(extra) && !(extra %in% base))
      base <- c(base, stats::setNames(extra, sprintf("%s  (from Advanced)", extra)))
    c(base, REPORT_OPT)
  }
  guided_sign_choices <- function()
    c(setNames(names(wd_amount_labels()), unname(wd_amount_labels())), REPORT_OPT)

  # The current date-format / amount-sign of a template, wherever the format
  # stores them (PDF keeps them under `table`, delimited at the top / in columns).
  # A template may declare SEVERAL candidate date formats (the engine accepts one
  # only if it reads every value). The Simple tab is a single dropdown, so it shows
  # the first candidate -- and apply_overrides below refuses to write that single
  # value back unless the user actually PICKED a different one, otherwise merely
  # opening the toolkit on a multi-format template and pressing Save would silently
  # narrow it back to one format and re-break the statement it was widened for.
  gv_datefmt_all <- function(tmpl) if (identical(tmpl$format, "pdf")) (tmpl$table$date_format %||% "%d/%m/%Y")
                                   else (tmpl$columns$date$format %||% "%d/%m/%Y")
  gv_datefmt <- function(tmpl) as.character(gv_datefmt_all(tmpl))[1]
  gv_sign    <- function(tmpl) if (identical(tmpl$format, "pdf")) (tmpl$table$amount_sign %||% "signed")
                               else (tmpl$amount_sign %||% "signed")

  # apply_overrides -- fold the Basic-tab choices onto the working template. Only
  # the common fields live here; everything else is edited as YAML on Advanced.
  # .datefmt_unchanged -- did the user leave the date dropdown on what the template
  # already declares? True when the template lists SEVERAL formats and the shown
  # (first) one came back unchanged: writing it would drop the other candidates.
  .datefmt_unchanged <- function(tmpl, datefmt) {
    cur <- as.character(gv_datefmt_all(tmpl))
    length(cur) > 1L && identical(datefmt, cur[1])
  }

  apply_overrides <- function(tmpl, bank, datefmt, sign, decimal = NULL,
                              unsigned_default = NULL, desc_col = NULL,
                              ref_col = NULL, bal_col = NULL,
                              id = NULL, type = NULL, currency = NULL,
                              date_col = NULL, amount_col = NULL,
                              keep_dateless = NULL,
                              type_debit_value = NULL, type_credit_value = NULL,
                              fingerprint_text = NULL) {
    if (!is.null(id) && nzchar(trimws(id)))
      tmpl$id <- gsub("[^A-Za-z0-9_]+", "_", trimws(id))   # the name it saves under
    if (!is.null(type) && nzchar(trimws(type))) tmpl$statement_type <- trimws(type)
    if (!is.null(currency) && nzchar(trimws(currency))) tmpl$currency <- trimws(currency)
    if (!is.null(bank) && nzchar(bank)) tmpl$bank <- bank
    if (identical(tmpl$format, "pdf")) {
      # The Simple-tab identifying phrases (one per line). PDF only -- the box is
      # not rendered for delimited/excel, and a Shiny input keeps its last value, so
      # applying it outside this branch would stamp the previous PDF's phrases onto
      # a CSV template. Blank means "leave whatever is there" (never silently wipe
      # a fingerprint); min_score follows the count so every phrase must be present.
      if (!is.null(fingerprint_text)) {
        ph <- fingerprint_phrases(fingerprint_text)
        cur <- as.character(unlist(tmpl$fingerprint$page_contains_all %||% list()))
        # ONLY when she actually changed them. A curated template opened for
        # refinement may deliberately carry min_score BELOW its phrase count
        # ("any 2 of these 4"); touching min_score on every keystroke would rewrite
        # that rule behind her back. An edit does set min_score to the phrase count,
        # which is what the box says out loud: all of them must appear.
        # A BLANK box means "leave whatever is there", as it always has: emptying
        # it used to wipe the list and then refuse the save with an accusation of
        # genericness about an empty box. Only a non-empty edit changes anything.
        if (length(ph) && !identical(ph, cur)) {
          # Assign the list outright; do NOT modifyList it. page_contains_all is
          # UNNAMED and modifyList merges only by name, so it silently kept the
          # drafter's phrases while min_score still dropped to the count of what she
          # typed -- the box appeared to work and instead LOOSENED the fingerprint
          # (3-of-3 became 1-of-3), which is exactly the "matched another bank's
          # statement" failure the gate in R/templates.R exists to stop. Updating
          # the list in place keeps any other fingerprint keys.
          fp <- tmpl$fingerprint %||% list()
          fp$page_contains_all <- as.list(ph)
          tmpl$fingerprint <- fp
          tmpl$min_score <- max(1L, length(ph))
        }
      }
      if (!is.null(datefmt) && nzchar(datefmt) && !.datefmt_unchanged(tmpl, datefmt))
        tmpl$table$date_format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$table$amount_sign <- sign
      # Shared-date (HSBC-style) opt-in: only stamp the key when ON, so normal
      # templates stay clean and unaffected.
      if (!is.null(keep_dateless))
        tmpl$table$keep_dateless_rows <- if (isTRUE(keep_dateless)) TRUE else NULL
    } else {
      if (!is.null(datefmt) && nzchar(datefmt) && !is.null(tmpl$columns$date) &&
          !.datefmt_unchanged(tmpl, datefmt))
        tmpl$columns$date$format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$amount_sign <- sign
      # Basic column-pickers (delimited): "" means "(none)" -> drop the mapping;
      # a name sets .source while preserving any other keys the field carries.
      set_src <- function(cols, field, val) {
        if (is.null(val)) return(cols)
        if (nzchar(val)) cols[[field]] <- modifyList(cols[[field]] %||% list(), list(source = val))
        else cols[[field]] <- NULL
        cols
      }
      tmpl$columns <- set_src(tmpl$columns, "description", desc_col)
      tmpl$columns <- set_src(tmpl$columns, "reference",   ref_col)
      tmpl$columns <- set_src(tmpl$columns, "balance",     bal_col)
      # Date / Amount pickers: set_src preserves the date's format key, and ""
      # (the "(pick a column)" placeholder) only ever drops an already-absent
      # mapping, so an auto-detected column is never silently unmapped.
      tmpl$columns <- set_src(tmpl$columns, "date",   date_col)
      tmpl$columns <- set_src(tmpl$columns, "amount", amount_col)
    }
    # decimal_mark / unsigned_default are top-level keys the engine reads.
    if (!is.null(decimal) && nzchar(decimal))
      tmpl$decimal_mark <- if (identical(decimal, "auto")) NULL else decimal
    if (!is.null(unsigned_default) && nzchar(unsigned_default) &&
        identical(sign, "unsigned"))
      tmpl$unsigned_default <- unsigned_default
    # type_dc tokens: which indicator value means a debit (required for the sign),
    # and optionally which means a credit (declaring it makes an unrecognised
    # indicator fail closed instead of defaulting to credit). Only stamped for the
    # type_dc style, so switching away clears them.
    if (identical(sign, "type_dc")) {
      if (!is.null(type_debit_value) && nzchar(trimws(type_debit_value)))
        tmpl$type_debit_value <- trimws(type_debit_value)
      tmpl$type_credit_value <- if (!is.null(type_credit_value) && nzchar(trimws(type_credit_value)))
        trimws(type_credit_value) else NULL
    } else {
      tmpl$type_debit_value <- NULL; tmpl$type_credit_value <- NULL
    }
    tmpl
  }

  # "Show the settings for this statement" -- the one control that separates the
  # accountant's four steps from everything the tool worked out for itself
  # (charter: the interface rule). Same pattern, and the same reasoning, as
  # cv_detail_open on the Convert result page.
  #
  # STICKY BY DESIGN. Once someone opens it, it stays open for every template they
  # set up for the rest of their session, so a maintainer refining templates all
  # afternoon opens it once. Closing it again is equally sticky.
  g_more_open <- reactiveVal(FALSE)
  observeEvent(input$g_more, g_more_open(!isTRUE(g_more_open())))
  output$g_more_open <- reactive({ isTRUE(g_more_open()) })
  outputOptions(output, "g_more_open", suspendWhenHidden = FALSE)

  output$g_more_toggle <- renderUI({
    open <- isTRUE(g_more_open())
    # Named for what is behind it, not for the mechanism. "Advanced" would tell an
    # accountant it is not for her; these really are the settings for this one
    # statement, and the honest thing to say is that they are already filled in.
    div(style = "margin:16px 0 6px",
      actionLink("g_more", style = "font-weight:700;font-size:14.5px",
        label = if (open) "Hide the settings for this statement"
                else "Show the settings for this statement"),
      div(class = "muted", style = "font-size:13px;margin-top:2px",
          "Identifying phrase, save name, number punctuation. Rarely needed."))
  })

  # Statement template toolkit. Your statement is ALWAYS on the left (the PDF page,
  # or sample rows for a CSV) so you can see what you're answering; the controls
  # are on the right (Simple for the common case, Advanced for the full YAML). A
  # live preview underneath shows exactly what will be pulled out.
  show_guided_modal <- function() {
    g <- guided(); req(g); tmpl <- g$tmpl
    is_pdf   <- identical(tmpl$format, "pdf")
    cur_fmt  <- gv_datefmt(tmpl); cur_sign <- gv_sign(tmpl)
    cur_dec  <- tmpl$decimal_mark %||% "auto"
    cur_ud   <- tmpl$unsigned_default %||% "debit"

    # LEFT: the statement itself, always visible. ONE gesture, asked once: draw a
    # box, say what it is, Assign. It used to be two gestures with two dropdowns
    # and four buttons -- a column band and a pinned header value -- which read as
    # two separate features when it is the same box either way. The dropdown now
    # carries the difference (see .meta_field), so the page has one thing to do.
    left_panel <- if (is_pdf) tagList(
      strong("Your statement"),
      p(class = "muted", HTML(
        "Drag a box across a column and say what it is. Only its <b>left-right</b> position matters - a column runs the full height of the page.")),
      fluidRow(
        column(3, numericInput("g_pdf_page",
          if (isTRUE(g$n_pages > 1L)) sprintf("Page (1 to %d)", g$n_pages) else "Page",
          1, min = 1, max = g$n_pages %||% NA, step = 1)),
        column(9, selectInput("g_pdf_field", "What did you draw a box around?",
                              list("(what is this?)" = "",
                                   "A column - read on every row" =
                                     c("date", "description", "amount", "balance", "particulars",
                                       "reference", "type", "debit", "credit", "other_party", "code"),
                                   "A one-off value - read from just that spot" =
                                     c("opening balance"          = "meta:opening_balance",
                                       "closing balance"          = "meta:closing_balance",
                                       "statement period - start" = "meta:period_start",
                                       "statement period - end"   = "meta:period_end",
                                       "account number"           = "meta:account_number",
                                       "account name"             = "meta:account_name")),
                              width = "100%"))),
      div(actionButton("g_pdf_assign", "Assign it", class = "btn-primary"),
          actionButton("g_pdf_remove", "Remove it")),
      # Behind the one disclosure: a column of your own naming, and the shared-date
      # opt-in. Neither belongs in setting up an ordinary statement. The toggle has
      # to be HERE as well as on the delimited panel -- these controls are on the
      # PDF side, and without it they could never be opened.
      uiOutput("g_more_toggle"),
      conditionalPanel("output.g_more_open == true",
        tags$hr(style = "margin:8px 0"),
        textInput("g_pdf_custom", "…or a column name of your own (used instead of the list above)",
                  "", width = "100%"),
        checkboxInput("g_keep_dateless",
          "Several rows share one date (e.g. HSBC) - keep the undated rows too (blank date, flagged)",
          value = isTRUE(tmpl$table$keep_dateless_rows))),
      plotOutput("g_pdf_plot", brush = brushOpts("g_pdf_brush"), height = "560px"))
    else tagList(
      strong("Your statement - sample rows"),
      p(class = "muted", "The first rows of your file, so you can see the columns while you set things up."),
      div(class = "mono", style = "max-height:560px;overflow:auto;border:1px solid #eee;padding:8px;font-size:12px",
          verbatimTextOutput("g_raw_sample")))

    # RIGHT: the controls.
    #
    # THE INTERFACE RULE (charter), applied to template setup.
    #
    # DEFAULT: the only things an accountant genuinely has to do -- point at the
    # columns (on the page for a PDF, in the pickers below for a CSV), say which
    # bank it is, check the preview underneath, Save. She confirms the tool's
    # reading by LOOKING AT HER OWN TRANSACTIONS, which she can do, rather than by
    # reading a date-format string or judging a recognition phrase, which she
    # cannot. Everything she is asked here, she can answer.
    #
    # BEHIND ONE CONTROL: everything the tool already worked out for itself from
    # her file, and only needs touching when the preview looks wrong -- the date
    # format, the amount style, the number punctuation, the phrase that
    # identifies the bank, the name it saves under, the "tell our team" hatch.
    # Nothing is deleted; the whole template as text is still on Advanced.
    #
    # The click is REMEMBERED for the session (g_more_open), exactly as on the
    # Convert result page: a maintainer opens it once and never sees it closed
    # again, and nobody is ever asked whether they are an advanced user.
    right_panel <- tabsetPanel(
      id = "g_tabs",
      tabPanel(
        "Simple", br(),
        textInput("g_bank", "Which bank is this statement from?", value = tmpl$bank, width = "100%"),
        if (!is.null(g$cols) && length(g$cols)) tagList(
          p(class = "muted", style = "margin:2px 0 8px",
            "Leave as detected unless the preview looks wrong."),
          fluidRow(
            column(4, selectInput("g_col_date", "Date (required)",
                                  choices = c("(pick a column)" = "", g$cols),
                                  selected = tmpl$columns$date$source %||% "")),
            column(4, selectInput("g_col_amt", "Amount",
                                  choices = c("(pick a column)" = "", g$cols),
                                  selected = tmpl$columns$amount$source %||% "")),
            column(4, selectInput("g_col_desc", "Description (required)",
                                  choices = g$cols,
                                  selected = tmpl$columns$description$source %||% g$cols[1])))),
        # THE DATE FORMAT AND THE AMOUNT STYLE STAY IN FRONT. They were briefly moved
        # behind the disclosure with everything else, on the theory that the drafter
        # fills them in. It does - and it is wrong often enough that these are the two
        # settings an analyst reports changing on almost every template she builds.
        # Frequency beats how technical a thing looks: hiding what is needed nearly
        # every time puts a click on the MOST common path, not the rarest. The rule is
        # "don't ask a question the tool can answer" - the tool cannot reliably answer
        # these two, so it asks, with its best guess already selected.
        tags$hr(style = "margin:14px 0 10px"),
        p(class = "muted", style = "margin:0 0 8px",
          "Detected from your statement - worth a check against the preview."),
        fluidRow(
          column(6, selectInput("g_date", "How are the dates written?",
                                choices = guided_date_choices(cur_fmt), selected = cur_fmt)),
          column(6, selectInput("g_sign", "How are amounts shown?",
                                choices = guided_sign_choices(), selected = cur_sign))),
        # These follow the amount style OUT: they appear only when that style is
        # chosen, and when they appear they are required. Left behind the disclosure,
        # picking "a D/C column" showed nowhere to say what D means.
        fluidRow(column(6, conditionalPanel(
              "input.g_sign == 'unsigned'",
              selectInput("g_unsigned_default", "A plain number (no + / − / CR) is a…",
                          choices = c("Charge - money out" = "debit",
                                      "Payment - money in" = "credit"),
                          selected = cur_ud)))),
        conditionalPanel(
            "input.g_sign == 'type_dc'",
            fluidRow(
              column(6, textInput("g_type_debit", "Which indicator value means money OUT (debit)?",
                                  value = tmpl$type_debit_value %||% "")),
              column(6, textInput("g_type_credit", "…and money IN (credit)?  (optional - leave blank to treat anything else as a credit)",
                                  value = tmpl$type_credit_value %||% "")))),
        uiOutput("g_more_toggle"),   # what is left is genuinely rare
        conditionalPanel("output.g_more_open == true",
          fluidRow(column(6, selectInput("g_decimal", "How are numbers punctuated? (usually leave on Auto)",
                                  choices = c("Auto-detect (NZ / AU / UK / US)" = "auto",
                                              "1,234.56 - dot is the decimal point" = "dot",
                                              "1.234,56 - comma is the decimal (European)" = "comma"),
                                  selected = cur_dec))),
          # THE PHRASE THAT RECOGNISES THIS BANK. For PDFs this is the one setting
          # that can refuse a save, and it used to be editable only in the raw YAML
          # box on Advanced -- a hard stop at the last step of the flow for the exact
          # person the toolkit exists for. So it stays here, in plain words, offering
          # phrases actually found on her statement; a refused save opens this panel
          # for her (see g_save) rather than naming a place she has to go and find.
          if (is_pdf) tagList(
            tags$hr(),
            strong("A distinctive phrase printed on this statement"),
            p(class = "muted", style = "margin:4px 0 6px",
              HTML(paste0("This is how the tool recognises this bank next time. Use something printed on the page that other banks do <b>not</b> print - the statement heading, or the bank's own name. ",
                          "Avoid single common words like &quot;Balance&quot; (they appear on every statement), and never a customer's name. One phrase per line; all of them must appear."))),
            if (length(g$fp_candidates)) fluidRow(
              column(9, selectInput("g_fp_pick", "Phrases found on your statement",
                                    choices = c("(choose one to add)" = "", g$fp_candidates),
                                    width = "100%")),
              column(3, br(), actionButton("g_fp_add", "Add it"))),
            textAreaInput("g_fp", NULL, width = "100%", rows = 3,
                          value = paste(unlist(tmpl$fingerprint$page_contains_all %||% list()),
                                        collapse = "\n")),
            uiOutput("g_fp_msg")),
          # The optional columns: real fields, but nothing is missing from the
          # result if they are left alone, so they wait here with the rest.
          if (!is.null(g$cols) && length(g$cols)) tagList(
            tags$hr(),
            fluidRow(
              column(6, selectInput("g_col_ref", "Reference (optional)",
                                    choices = c("(none)" = "", g$cols),
                                    selected = tmpl$columns$reference$source %||% "")),
              column(6, selectInput("g_col_bal", "Balance (optional)",
                                    choices = c("(none)" = "", g$cols),
                                    selected = tmpl$columns$balance$source %||% "")))),
          tags$hr(),
          fluidRow(
            column(6, textInput("g_id", "Saves under this name", value = tmpl$id %||% "")),
            column(6, textInput("g_currency", "Currency", value = tmpl$currency %||% "NZD"))),
          textInput("g_type", "Kind of statement", value = tmpl$statement_type %||% "everyday",
                    width = "100%"),
          tags$hr(),
          div(style = "padding:10px 12px;border:1px dashed #c98a00;background:#fffbe9;border-radius:8px",
            strong("None of these fit? Tell our team"),
            p(class = "muted", "Describe the FORMAT in plain words (no names / account numbers / statement details) and we'll build a template."),
            textAreaInput("g_req_detail", NULL, width = "100%", rows = 2,
              placeholder = "e.g. Dates look like 2 Dez (German). Amounts end in 'H' for Haben (credit)."),
            actionButton("g_req_send", "Send to our team", class = "btn-warning"),
            uiOutput("g_req_msg")))),
      tabPanel(
        "Advanced", br(),
        helpText(HTML("The <b>complete</b> template as text - edit anything (identifiers, column mapping, label synonyms, region bounds, row tolerance, metadata labels). Load your Simple choices in, edit, then Check &amp; apply.")),
        div(actionButton("g_adv_load", "Load current settings"),
            actionButton("g_adv_apply", "Check & apply", class = "btn-primary")),
        br(), uiOutput("g_adv_msg"),
        textAreaInput("g_yaml", NULL, value = template_yaml(tmpl), width = "100%", rows = 24)))

    showModal(modalDialog(
      title = "Statement template toolkit", size = "l", easyClose = FALSE,
      div(style = "padding:8px 12px;background:#eef4ff;border:1px solid #d6e2ff;border-radius:6px;margin-bottom:8px",
        HTML(sprintf("Setting up: <b>%s</b> &nbsp;·&nbsp; %s",
             htmltools::htmlEscape(g$name %||% "your file"),
             if (is_pdf) "PDF" else if (identical(tmpl$format, "excel")) "Excel" else "CSV / delimited"))),
      # An always-visible mini-guide: the 2-minute guide is a separate modal and
      # Shiny shows one modal at a time, so once the toolkit is open it can't be
      # reopened. This strip keeps the whole flow on screen the entire time.
      div(style = "padding:8px 12px;background:#f6faf7;border:1px solid #cfe6d8;border-radius:6px;margin-bottom:10px;font-size:13px",
        HTML(if (is_pdf)
          paste0("<b>How this works:</b> &nbsp;1&nbsp;Drag a box over a column and say what it is &nbsp;·&nbsp; ",
                 "2&nbsp;click <b>Assign it</b> &nbsp;·&nbsp; 3&nbsp;name the bank on the right &nbsp;·&nbsp; ",
                 "4&nbsp;check the <b>Preview</b> below &nbsp;·&nbsp; 5&nbsp;<b>Save</b>. ",
                 "Everything else is already filled in from your own file - you're just confirming it.")
        else
          paste0("<b>How this works:</b> &nbsp;1&nbsp;check each column picker matches your file &nbsp;·&nbsp; ",
                 "2&nbsp;name the bank &nbsp;·&nbsp; 3&nbsp;check the <b>Preview</b> below &nbsp;·&nbsp; ",
                 "4&nbsp;<b>Save</b>. ",
                 "Everything else is already filled in from your own file - you're just confirming it."))),
      fluidRow(column(6, left_panel), column(6, right_panel)),
      tags$hr(),
      h4("Preview - what will be pulled out of your statement"),
      uiOutput("g_status"),
      DTOutput("g_preview"),
      footer = tagList(modalButton("Cancel"),
        actionButton("g_save", "Save template", class = "btn-primary"))))
  }

  # open_guided -- the single entry into the setup modal, shared by every launch
  # point (Convert result, Admin pickup, Add-a-template). Drafts a template from
  # the file unless the caller already has one (e.g. the matched template).
  open_guided <- function(path, name, seed_tmpl = NULL, upload_id = NA_character_) {
    tmpl <- seed_tmpl
    if (is.null(tmpl)) {
      bankguess <- trimws(tools::toTitleCase(gsub("[^A-Za-z]+", " ", tools::file_path_sans_ext(name))))
      tmpl <- withProgress(message = "Opening the toolkit…", value = 0.4,
        tryCatch(draft_template(path, bank = if (nzchar(bankguess)) bankguess else "New bank"),
                 error = function(e) NULL))
    }
    if (is.null(tmpl)) {
      # Fail loud AND specific. An Excel draft comes back NULL only when no sheet
      # held a recognisable transaction table (a date column + a money column).
      if (tolower(tools::file_ext(name %||% "")) %in% c("xlsx", "xlsm", "xls")) {
        showNotification(paste("No transaction table found in this workbook - the toolkit needs a sheet",
                               "with a date column and an amount column. If the table is unusual,",
                               "save the sheet as CSV (File > Save As in Excel) and set that up instead."),
                         type = "warning", duration = 10)
      } else {
        showNotification(paste("Couldn't read this file automatically. If it's a scanned/image PDF give it a moment,",
                               "or try a text PDF / CSV export. If it isn't a transaction table, pick 'Something else' above."),
                         type = "error", duration = 10)
      }
      return(invisible(FALSE))
    }
    # Ids of the curated (tested) templates: saving a customised copy under one of
    # these would be shadowed (defaults win), so g_save gives it a distinct id.
    default_ids <- tryCatch(names(load_templates(TEMPLATES_DIR, strict = FALSE)),
                            error = function(e) character(0))
    # For delimited statements, offer the file's actual columns in the Basic
    # field-pickers (PDF columns are bands, edited visually / in Advanced).
    cols <- if (identical(tmpl$format, "delimited"))
      tryCatch(names(read_delimited(read_input(path), tmpl)$table), error = function(e) NULL)
    else if (identical(tmpl$format, "excel"))
      tryCatch(names(read_input(path)$table), error = function(e) NULL)
    else NULL
    # Candidate identifying phrases the drafter actually FOUND on this page, so the
    # Simple tab can offer real choices ("pick a phrase that's printed on your
    # statement") instead of sending a non-technical user to the raw YAML box.
    fp_cands <- if (identical(tmpl$format, "pdf"))
      safe(header_phrases(read_input(path), n = 8), character(0)) else character(0)
    # How many pages the sample has, counted once here: the toolkit's page box says
    # so out loud and can't be walked past the end of the document.
    npages <- if (identical(tmpl$format, "pdf"))
      safe({ i <- read_input(path); as.integer(length(i$pages %||% i$words %||% list())) },
           NA_integer_) else NA_integer_
    if (!isTRUE(npages >= 1L)) npages <- NA_integer_
    cv_upload_id(upload_id)
    guided(list(path = path, name = name, tmpl = tmpl, default_ids = default_ids,
                cols = cols, n_pages = npages,
                fp_candidates = unique(trimws(as.character(fp_cands %||% character(0))))))
    show_guided_modal()
    invisible(TRUE)
  }

  # Launch the same setup modal from the Add-a-template tab (not tied to a Convert
  # upload, so a successful Save just adds the template).
  observeEvent(input$ts_go, {
    if (is.null(input$ts_file)) {
      showNotification("Upload the document first (the file picker above), then open the toolkit.",
                       type = "warning", duration = 6)
      return()
    }
    sess <- tempfile("ts_")   # guaranteed-unique per session/process (no cross-user bleed)
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, input$ts_file$name)
    file.copy(input$ts_file$datapath, src, overwrite = TRUE)
    open_guided(src, input$ts_file$name)
  })

  output$cv_teach <- renderUI({
    res <- cv_res(); req(res)
    if (is.null(cv_src())) return(NULL)
    st <- res$status %||% "failed"
    if (identical(res$kind, "form")) {
      # A form result is set up in the PDF form builder, not the statement toolkit.
      return(div(style = "margin:12px 0;padding:10px 12px;border:1px solid #d9d9d9;background:#fafafa;border-radius:8px",
        span(class = "muted", "Want to change which values are pulled, or add more (including a value in a different place than its label)? "),
        actionLink("cv_goto_templates", "Open the PDF form builder →")))
    }
    if (identical(st, "unsupported")) {
      # "Unsupported" covers two opposite situations. If two or more templates fit
      # this statement EQUALLY well, the tool refuses to guess which -- but telling
      # the analyst "this layout is new, build a template" would be flatly wrong
      # advice: we already have templates that fit, and a third would only tie too.
      # So name them and let one click convert with either. Nothing is auto-chosen;
      # the analyst decides, which is the one thing a machine can't do here.
      tied <- as.character(res$detect$tied %||% character(0))
      if (isTRUE(res$detect$ambiguous) && length(tied) >= 2) {
        return(div(style = "margin:12px 0;padding:14px;border:1px solid var(--warn-line);background:var(--warn-bg);border-radius:8px",
          strong(sprintf("%d templates fit this statement equally well - which one is it?", length(tied))),
          p(class = "muted", style = "margin:6px 0 10px",
            "They match the same wording on the page, so the tool won't pick for you. Choose one and it converts straight away - nothing is saved or changed."),
          div(style = "display:flex;flex-wrap:wrap;gap:8px;align-items:flex-end",
            div(style = "flex:1 1 340px",
              selectInput("cv_tie_pick", NULL, choices = tied, width = "100%")),
            div(style = "margin-bottom:15px",
              actionButton("cv_tie_go", "Convert with this one", class = "btn-primary"))),
          div(class = "muted", style = "font-size:13px",
            "Sure it's neither? ",
            actionLink("cv_teach_go", "Set up a new template instead"),
            ".")))
      }
      # BEFORE offering to build a template: if templates built here are switched
      # OFF, the statement may already have one and we simply refused to look. The
      # old app said nothing at all, so the answer to "I built this template and it
      # doesn't work" was buried in a tick-box inside a collapsed "wrong bank?"
      # panel. One sentence, one button.
      # A new layout is a FORK, not a cliff. We WANT analysts setting up their own
      # templates, so the prominent GREEN action is "set it up yourself" (the tool
      # pre-fills what it can); handing it to the team is the small fallback.
      div(style = "margin:12px 0;padding:14px;border:1px solid #b7e1b0;background:#eef8ec;border-radius:8px",
        strong("This layout is new — set it up once and it converts every time."),
        p(class = "muted", style = "margin:6px 0 10px",
          "The tool pre-fills what it can detect from your statement — you confirm against a live preview and save. Takes a couple of minutes, no data background needed."),
        actionButton("cv_teach_go", "Set up a template for this statement", class = "btn-primary btn-lg"),
        div(style = "margin-top:8px",
          span(class = "muted", "Would rather not? "),
          actionLink("cv_unsup_raise", "Send it to the team instead")))
    } else {
      # Happy path stays quiet: the "Wrong bank?" line up top already offers a fix,
      # so we don't repeat a toolkit prompt here. needs_review/failed keep the offer.
      if (identical(st, "ok")) return(NULL)
      div(style = "margin:12px 0;padding:10px 12px;border:1px solid #d9d9d9;background:#fafafa;border-radius:8px",
        span(class = "muted", "Open this statement in setup to fix how it's read and save an improved template."), " ",
        actionButton("cv_teach_go", "Open the template toolkit", class = "btn-default"))
    }
  })
  # Tick the box AND re-run in one click. include_user = TRUE because the tick has
  # not reached the browser yet when this conversion starts.
  observeEvent(input$cv_goto_templates,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))
  observeEvent(input$cv_empty_to_tmpl,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))
  observeEvent(input$ab_go_convert,
    updateTabsetPanel(session, "main_tabs", selected = "Convert"))
  observeEvent(input$ab_go_template,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))

  observeEvent(input$cv_teach_go, {
    src <- cv_src(); req(src)
    res <- cv_res()
    seed <- NULL
    # If the conversion MATCHED a template (ok / needs_review), open that template
    # so the user refines the real one. An unsupported result also carries a
    # template id - the CLOSEST MISS, for the logs - and seeding from that would
    # open the wrong bank's settings and save a fingerprint that can never match
    # this file. Unsupported/failed always drafts fresh from the file itself.
    if ((res$status %||% "") %in% c("ok", "needs_review")) {
      tid <- (res$template_id %||% NA_character_)[1]
      if (!is.na(tid) && nzchar(tid)) {
        tset <- tryCatch(templates(), error = function(e) list())
        if (!is.null(tset[[tid]])) seed <- tset[[tid]]
      }
    }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
  })

  # Send an unsupported layout to the team (PII-safe: generic context only - a
  # file extension, the detected bank guess and the closest template - never file
  # contents or the file name).
  observeEvent(input$cv_unsup_raise, {
    res <- cv_res(); req(res)
    ctx <- list(
      file_ext = tolower(tools::file_ext(cv_src()$name %||% "")),
      format   = res$format %||% (res$header$format %||% "delimited"),
      bank     = res$header$bank %||% "",
      closest  = (res$template_id %||% "")[1])
    id <- tryCatch(record_template_request(
      "Unsupported statement layout - raised from Convert. Please set up a template.",
      ctx, requested_by = who_now(), dir = REQUESTS_DIR), error = function(e) NULL)
    if (is.null(id))
      showNotification("Couldn't send just now - please try again.", type = "error")
    else
      showNotification("Sent to the team. They'll set up a template for this layout - you'll be able to convert it once it's ready.",
                       type = "message", duration = 7)
  })

  # "Matched but maybe wrong": when a near-duplicate template nearly matched too,
  # show the candidates + margin and let the analyst re-open the toolkit with a
  # different one. Only surfaces on a genuine CLOSE CALL - a confident match stays
  # clutter-free (Beth is never asked "is this the right template?" without cause).
  output$cv_candidates <- renderUI({
    res <- cv_res(); req(res); req(!is.null(res$candidates))
    cand <- res$candidates
    if (is.null(nrow(cand)) || nrow(cand) < 2) return(NULL)
    thin <- isTRUE(res$detect$thin)
    if (!thin) return(NULL)   # only on a close call; the happy path shows nothing here
    top <- utils::head(cand, 4L)
    # The candidate frame includes the matched winner; the "nearest others" line
    # and the picker must both EXCLUDE it (else it reads "matched X. Nearest
    # others: X ...").
    others_df <- top[top$id != res$template_id, , drop = FALSE]
    others <- others_df$id
    style <- if (thin) "border:1px solid #f0c36d;background:#fff8e6"
             else "border:1px solid #e3e3e3;background:#fafafa"
    tagList(div(style = sprintf("margin:12px 0;padding:10px 12px;border-radius:8px;%s", style),
      strong(if (thin) "Close call - please confirm this is the right template"
             else "Template match"),
      p(class = "muted", if (nrow(others_df))
        sprintf("Matched %s. Nearest others: %s.", res$template_id,
                paste(sprintf("%s (score %s)", others_df$id, others_df$score), collapse = ", "))
        else sprintf("Matched %s.", res$template_id)),
      # "Try the other one" must BE one click, not a trip to the toolkit. Opening
      # the toolkit is a heavy action (a form full of settings, a template to name
      # and save) for what is really the question "did it pick the right variant?".
      # So converting with the other template is the primary button and the toolkit
      # is the secondary link, for when the answer is "neither of these is right".
      if (length(others)) tagList(
        selectInput("cv_cand_pick", "Wrong one? Try a different template:",
                    choices = others, width = "100%"),
        actionButton("cv_cand_convert", "Convert with this one instead", class = "btn-primary"),
        span(style = "margin-left:10px", class = "muted", "or "),
        actionLink("cv_cand_go", "open the toolkit with it"))))
  })
  observeEvent(input$cv_cand_go, {
    src <- cv_src(); req(src); tid <- input$cv_cand_pick; req(tid, nzchar(tid))
    tset <- tryCatch(templates(), error = function(e) list())
    seed <- tset[[tid]]
    if (is.null(seed)) { showNotification("That template isn't available.", type = "error"); return() }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
  })

  # convert_with_template(tid) -- re-run THIS statement against one exact template.
  # Nothing is saved and no template is edited: it is the same conversion the
  # analyst just ran, with the guess replaced by their choice. Reconciliation runs
  # in full, so a wrong choice still comes back as needs_review rather than being
  # trusted because a human picked it. Shared by the tie chooser and the close-call
  # panel so "try the other one" behaves identically wherever it is offered.
  convert_with_template <- function(tid) {
    src <- cv_src(); req(src, tid, nzchar(tid))
    uid <- cv_upload_id()          # same statement, same pickup - not a new upload
    run_conversion(src$path, src$name, record = FALSE, force_tpl = tid)
    cv_upload_id(uid)
    res <- cv_res()
    # The pickup record has to learn that this statement DID convert, or Admin
    # keeps asking someone to build a template for a file that already has one.
    if (!is.na(uid) && (res$status %||% "") %in% c("ok", "needs_review"))
      safe(set_upload_status(uid, res$status,
        run_id = res$run_id %||% NA_character_,
        template = res$template_id %||% NA_character_,
        trust = res$trust$level %||% NA_character_,
        detail = sprintf("converted with %s, chosen by hand from the matching templates", tid),
        dir = UPLOADS_DIR))
    showNotification(sprintf("Converted with %s.", tid), type = "message", duration = 5)
  }
  observeEvent(input$cv_tie_go, convert_with_template(input$cv_tie_pick))
  observeEvent(input$cv_cand_convert, convert_with_template(input$cv_cand_pick))

  # gl_build -- the guided template with the Simple-tab overrides applied.
  # meta_live = FALSE ISOLATES the three pure-metadata fields (template name / bank
  # / statement-type) so editing them doesn't invalidate the caller. The live
  # preview uses this: those three never appear in and never affect the parsed
  # transaction rows, yet each keystroke on them was forcing a full 3-page PDF
  # re-parse (~378 ms measured) -- a spinner-stall while typing a bank name.
  # `currency` STAYS live because it stamps the previewed currency column. Save
  # and the YAML editor use guided_live() (meta_live = TRUE), so every field is
  # still written exactly as typed.
  gl_build <- function(meta_live = TRUE) {
    g <- guided(); req(g)
    # "__report__" (the "none of these fit" option) is a no-override sentinel.
    no_sentinel <- function(v) if (identical(v, "__report__")) "" else v
    idv   <- if (meta_live) input$g_id   else isolate(input$g_id)
    bankv <- if (meta_live) input$g_bank else isolate(input$g_bank)
    typev <- if (meta_live) input$g_type else isolate(input$g_type)
    apply_overrides(g$tmpl, bankv, no_sentinel(input$g_date), no_sentinel(input$g_sign),
                    input$g_decimal, input$g_unsigned_default,
                    input$g_col_desc, input$g_col_ref, input$g_col_bal,
                    idv, typev, input$g_currency,
                    date_col = input$g_col_date, amount_col = input$g_col_amt,
                    keep_dateless = input$g_keep_dateless,
                    type_debit_value = input$g_type_debit,
                    type_credit_value = input$g_type_credit,
                    fingerprint_text = input$g_fp)
  }
  guided_live <- reactive(gl_build(meta_live = TRUE))

  # "Add it" -- append a phrase the drafter found on the page to the box, rather
  # than making the user retype it exactly (a fingerprint must match the page text
  # character for character, so retyping is where this goes wrong).
  observeEvent(input$g_fp_add, {
    pick <- trimws(input$g_fp_pick %||% "")
    if (!nzchar(pick)) {
      showNotification("Choose one of the phrases found on your statement first.", type = "warning"); return() }
    cur <- fingerprint_phrases(input$g_fp)
    if (pick %in% cur) {
      showNotification("That phrase is already in the box.", type = "message"); return() }
    updateTextAreaInput(session, "g_fp", value = paste(c(cur, pick), collapse = "\n"))
  })
  # Live verdict on the phrase(s), in the same place they are typed: the SAME rule
  # the save uses (validate_template), so "Save" can no longer be the first time a
  # too-generic fingerprint is mentioned.
  output$g_fp_msg <- renderUI({
    g <- guided(); req(g); req(identical(g$tmpl$format, "pdf"))
    # NA (not "no problems") when the check itself could not run -- an "all clear"
    # we did not actually earn is the one answer we must never give.
    probs <- safe(validate_template(guided_live()), NA_character_)
    if (length(probs) == 1L && is.na(probs))
      return(span(class = "muted", style = "font-size:12.5px",
                  "Couldn't check the phrase just now - Save will tell you for certain."))
    fpp <- probs[grepl("fingerprint", probs, fixed = TRUE)]
    if (!length(fpp))
      return(span(class = "ok", style = "font-size:12.5px",
                  "This phrase will do - it is specific enough to identify this layout."))
    span(class = "bad", style = "font-size:12.5px", paste(fpp, collapse = " "))
  })

  # Nudge the user to the "tell our team" box when they pick "none of these".
  observeEvent(list(input$g_date, input$g_sign), {
    if (identical(input$g_date, "__report__") || identical(input$g_sign, "__report__"))
      showNotification("None of the options fit? Use the 'Tell our team' box below to describe it.",
                       type = "message", duration = 6)
  }, ignoreInit = TRUE)

  # Raise a template request (PII-safe: free-text + generic context only).
  observeEvent(input$g_req_send, {
    g <- guided(); req(g)
    detail <- trimws(input$g_req_detail %||% "")
    if (!nzchar(detail)) {
      output$g_req_msg <- renderUI(span(class = "bad", "Please describe the format first.")); return() }
    ctx <- list(
      file_ext      = tolower(tools::file_ext(g$name %||% "")),
      format        = g$tmpl$format %||% "delimited",
      bank          = input$g_bank %||% (g$tmpl$bank %||% ""),
      date_choice   = input$g_date %||% "",
      amount_choice = input$g_sign %||% "")
    id <- tryCatch(record_template_request(detail, ctx, requested_by = who_now(), dir = REQUESTS_DIR),
                   error = function(e) NULL)
    if (is.null(id)) {
      output$g_req_msg <- renderUI(span(class = "bad", "Couldn't save - try again.")); return() }
    updateTextAreaInput(session, "g_req_detail", value = "")
    output$g_req_msg <- renderUI(span(class = "ok",
      "Thanks - raised for review. Our team will build a template for this format."))
  })

  # Advanced tab: pull the current Basic settings into the YAML editor on demand.
  observeEvent(input$g_adv_load, {
    req(guided())
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "muted", "Loaded current settings into the editor."))
  })

  # Advanced tab: validate the edited YAML and adopt it as the working template.
  # On success we re-seed the Basic controls so their live overrides match (never
  # clobbering an advanced-only change). Fail loud on bad YAML / invalid template.
  observeEvent(input$g_adv_apply, {
    g <- guided(); req(g)
    parsed <- tryCatch(yaml::yaml.load(input$g_yaml %||% ""), error = function(e) e)
    if (inherits(parsed, "error") || !is.list(parsed)) {
      output$g_adv_msg <- renderUI(span(class = "bad",
        paste("YAML error:", if (inherits(parsed, "error")) conditionMessage(parsed) else "not a template")))
      return()
    }
    probs <- tryCatch(validate_template(parsed), error = function(e) conditionMessage(e))
    if (length(probs)) {
      output$g_adv_msg <- renderUI(span(class = "bad",
        paste("Not a valid template:", paste(probs, collapse = "; "))))
      return()
    }
    g$tmpl <- parsed; guided(g)
    updateTextInput(session, "g_id", value = parsed$id %||% "")
    updateTextInput(session, "g_type", value = parsed$statement_type %||% "")
    updateTextInput(session, "g_currency", value = parsed$currency %||% "NZD")
    updateTextInput(session, "g_bank", value = parsed$bank %||% "")
    # Re-offer the date list WITH the applied format included, so an exotic
    # Advanced date_format is selectable and survives (not reverted by guided_live).
    updateSelectInput(session, "g_date", choices = guided_date_choices(gv_datefmt(parsed)),
                      selected = gv_datefmt(parsed))
    updateSelectInput(session, "g_sign", selected = gv_sign(parsed))
    updateSelectInput(session, "g_decimal", selected = parsed$decimal_mark %||% "auto")
    updateSelectInput(session, "g_unsigned_default", selected = parsed$unsigned_default %||% "debit")
    updateTextInput(session, "g_type_debit",  value = parsed$type_debit_value %||% "")
    updateTextInput(session, "g_type_credit", value = parsed$type_credit_value %||% "")
    updateSelectInput(session, "g_col_date", selected = parsed$columns$date$source %||% "")
    updateSelectInput(session, "g_col_amt",  selected = parsed$columns$amount$source %||% "")
    updateSelectInput(session, "g_col_desc", selected = parsed$columns$description$source %||% "")
    updateSelectInput(session, "g_col_ref",  selected = parsed$columns$reference$source %||% "")
    updateSelectInput(session, "g_col_bal",  selected = parsed$columns$balance$source %||% "")
    # Keep the Simple-tab phrase box in step with a fingerprint edited in the YAML,
    # or the next Simple-tab keystroke would silently put the old phrases back.
    if (identical(parsed$format, "pdf"))
      updateTextAreaInput(session, "g_fp",
        value = paste(unlist(parsed$fingerprint$page_contains_all %||% list()), collapse = "\n"))
    output$g_adv_msg <- renderUI(span(class = "ok", "Applied - preview updated below."))
  })

  # ---- Toolkit: visual PDF column editor --------------------------------------
  # Renders the chosen page and draws the working template's column bands on it;
  # a drawn box assigns/updates a column, keeping the YAML editor and preview in
  # sync so PDF setup is fully visual and in one place.
  # Sample rows of a delimited file, shown on the left of the toolkit so the user
  # can see the columns while answering bank / date / amount.
  output$g_raw_sample <- renderText({
    g <- guided(); req(g); req(!identical(g$tmpl$format, "pdf"))
    # Excel is binary - show the cleaned table (right sheet, preamble skipped),
    # not raw bytes. Delimited files show their first lines verbatim.
    if (identical(g$tmpl$format, "excel")) {
      t <- tryCatch(read_input(g$path)$table, error = function(e) NULL)
      if (is.null(t) || !nrow(t)) return("(couldn't read the workbook)")
      return(paste(utils::capture.output(print(utils::head(t, 25), row.names = FALSE)),
                   collapse = "\n"))
    }
    lines <- tryCatch(readLines(g$path, n = 40, warn = FALSE), error = function(e) character(0))
    if (!length(lines)) "(couldn't read the file)" else paste(lines, collapse = "\n")
  })

  g_pdf_render <- reactive({
    g <- guided(); req(g); req(identical(g$tmpl$format, "pdf"))
    # Only the file + page matter for the bitmap (bands are overlays drawn in the
    # plot), and the render is cached, so assigning a box no longer re-renders it.
    render_page_view(g$path, .clamp_page(input$g_pdf_page, g$n_pages %||% NA_integer_), 100)
  })
  output$g_pdf_plot <- renderPlot({
    r <- g_pdf_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    cols <- guided()$tmpl$table$columns %||% list()
    if (length(cols)) {
      pal <- grDevices::hcl(seq(0, 300, length.out = length(cols)), 70, 55)
      for (i in seq_along(cols)) {
        b <- cols[[i]]; if (is.null(b$x_min) || is.null(b$x_max)) next
        rect(b$x_min, 0, b$x_max, r$h, border = pal[i], lwd = 2)
        text(mean(c(b$x_min, b$x_max)), 16, names(cols)[i], col = pal[i], font = 2)
      }
    }
    # pinned header-value boxes (metadata_regions) for the CURRENT page, in orange
    mr <- guided()$tmpl$table$metadata_regions %||% list()
    pg <- r$pg   # the page actually drawn, so the overlay can never sit on another
    for (nm in names(mr)) { b <- mr[[nm]]
      if (is.null(b$x_min) || is.null(b$x_max)) next
      if (!identical(as.integer(b$page %||% 1), pg)) next
      y0 <- b$y_min %||% 0; y1 <- b$y_max %||% r$h
      rect(b$x_min, y0, b$x_max, y1, border = "#a15c00", lwd = 2)
      text(b$x_min, y0, nm, col = "#a15c00", font = 2, cex = 0.85, pos = 3, offset = 0.2)
    }
    # Live feedback: the box being drawn is shown as the FULL-HEIGHT column it will
    # become (a translucent band over the whole page height), so it is obvious the
    # box's top/bottom are ignored and only its left-right span defines the column.
    br <- input$g_pdf_brush
    if (!is.null(br) && is.finite(br$xmin) && is.finite(br$xmax)) {
      rect(br$xmin, 0, br$xmax, r$h, col = "#1a73e820", border = "#1a73e8", lty = 2, lwd = 2)
      text(mean(c(br$xmin, br$xmax)), r$h * 0.5, "this whole column",
           col = "#1a73e8", font = 2, cex = 0.95, srt = 90)
    }
  })
  .CANON_PDF_COLS <- c("date", "description", "amount", "balance", "debit", "credit",
                       "particulars", "code", "reference", "other_party", "type")
  # A custom name (typed) becomes an EXTRA column (output as x.<name>); a canonical
  # name is a normal table column. Which "slot" a field lives in for assign/remove.
  .pdf_field_ref <- function(f) if (f %in% .CANON_PDF_COLS) "columns" else "extras"
  .pdf_all_bands <- function(tbl) c(tbl$columns %||% list(), tbl$extras %||% list())
  .pdf_resize_region <- function(g) {
    xs <- unlist(lapply(.pdf_all_bands(g$tmpl$table), function(c) c(c$x_min, c$x_max)))
    reg <- g$tmpl$table$region %||% list()
    if (length(xs)) { reg$x_min <- min(xs) - 5; reg$x_max <- max(xs) + 5 }
    else { reg$x_min <- NULL; reg$x_max <- NULL }   # no bands left -> drop x-scope, keep y
    g$tmpl$table$region <- if (length(reg)) reg else NULL
    g
  }
  .pdf_chosen_field <- function() {
    # A hidden control must never override a visible one. The custom name lives
    # behind the disclosure; while that is closed the analyst cannot see it, so it
    # cannot be what she meant - the dropdown in front of her is.
    custom <- trimws(input$g_pdf_custom %||% "")
    if (isTRUE(g_more_open()) && nzchar(custom))
      gsub("[^A-Za-z0-9_]+", "_", custom) else input$g_pdf_field
  }
  # One dropdown answers both kinds of box, so the choice carries which kind it is:
  # "meta:" marks a one-off header value, anything else is a column. A typed custom
  # name can never collide -- .pdf_chosen_field strips the colon to an underscore.
  .meta_field <- function(f) if (grepl("^meta:", f %||% "")) sub("^meta:", "", f) else NA_character_

  observeEvent(input$g_pdf_assign, {
    g <- guided(); req(g); br <- input$g_pdf_brush
    if (is.null(br)) { showNotification("Draw a box on the page first.", type = "warning"); return() }
    f <- .pdf_chosen_field()
    if (!nzchar(trimws(f %||% ""))) {
      showNotification("First say what you boxed (date, description, amount, opening balance...), then Assign it.", type = "warning"); return() }
    # A one-off header value: a specific box (x AND y) around ONE value that isn't
    # on every row -- a balance, the statement period, an account detail -- read
    # straight from that spot when the automatic reader can't label it. It never
    # touches the transaction region, which is why it is not a column band.
    mf <- .meta_field(f)
    if (!is.na(mf)) {
      pg <- .clamp_page(input$g_pdf_page, g$n_pages %||% NA_integer_)
      g$tmpl$table$metadata_regions[[mf]] <- list(page = pg,
        x_min = round(br$xmin), x_max = round(br$xmax),
        y_min = round(br$ymin), y_max = round(br$ymax))
      guided(g)
      updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
      output$g_adv_msg <- renderUI(span(class = "ok",
        sprintf("Pinned '%s' to the box you drew on page %d.", mf, pg)))
      return()
    }
    slot <- .pdf_field_ref(f)
    g$tmpl$table[[slot]][[f]] <- list(x_min = round(br$xmin), x_max = round(br$xmax))
    # Mapping a money-in / money-out band means this is a separate debit/credit
    # statement: switch the amount style to match, so saving never demands a single
    # 'amount' column (the reported "amount is still required even when debit and
    # credit are present"). The dropdown is the source of truth guided_live() reads,
    # so update it too.
    switched <- FALSE
    if (f %in% c("debit", "credit") && !identical(g$tmpl$table$amount_sign, "debit_credit_cols")) {
      g$tmpl$table$amount_sign <- "debit_credit_cols"
      updateSelectInput(session, "g_sign", selected = "debit_credit_cols")
      switched <- TRUE
    }
    g <- .pdf_resize_region(g); guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "ok",
      sprintf("Set the '%s' column%s.%s Page and preview updated.", f,
              if (identical(slot, "extras")) " (custom / extra)" else "",
              if (switched) " Amount style set to separate money-in / money-out." else "")))
    if (isTRUE(switched))
      showNotification(paste("Amount style set to separate money-in / money-out, because you drew",
                             "both a money-out and a money-in column."),
                       type = "message", duration = 7)
  })
  # Delete a column band the auto-setup got wrong (a column that isn't on this
  # statement), or unpin a header value. Recomputes the table region from whatever
  # bands remain.
  observeEvent(input$g_pdf_remove, {
    g <- guided(); req(g); f <- .pdf_chosen_field()
    if (!nzchar(trimws(f %||% ""))) {
      showNotification("Pick what to remove first.", type = "warning"); return() }
    mf <- .meta_field(f)
    if (!is.na(mf)) {
      if (is.null(g$tmpl$table$metadata_regions[[mf]])) {
        showNotification(sprintf("There's no pinned box for '%s' to remove.", mf), type = "warning"); return() }
      g$tmpl$table$metadata_regions[[mf]] <- NULL
      if (!length(g$tmpl$table$metadata_regions)) g$tmpl$table$metadata_regions <- NULL
      guided(g)
      updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
      output$g_adv_msg <- renderUI(span(class = "ok", sprintf("Removed the pinned box for '%s'.", mf)))
      return()
    }
    slot <- if (!is.null(g$tmpl$table$columns[[f]])) "columns"
            else if (!is.null(g$tmpl$table$extras[[f]])) "extras" else NA
    if (is.na(slot)) {
      showNotification(sprintf("There's no '%s' column to remove.", f), type = "warning"); return() }
    g$tmpl$table[[slot]][[f]] <- NULL
    g <- .pdf_resize_region(g); guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "ok",
      sprintf("Removed the '%s' column. Page and preview updated.", f)))
  })

  # ONE parse per change, shared by the preview table + the status line (each used
  # to call draft_preview independently, doubling the parse on every box assignment).
  # The live preview parses only the FIRST FEW PDF pages -- enough to confirm the
  # columns read correctly -- so a big statement previews in a fraction of the time
  # (the full convert on Save still parses every page).
  g_preview_tx <- reactive({ g <- guided(); req(g)
    # Isolated build: editing template name / bank / statement-type won't re-parse
    # the statement (they don't affect the rows); currency and every column /
    # date / amount setting stay live, so the preview still updates on those.
    draft_preview(g$path, gl_build(meta_live = FALSE), preview_pages = 3L) })
  output$g_preview <- renderDT({
    tx <- g_preview_tx(); req(!is.null(tx))
    tx <- utils::head(tx, 12)
    # Show every field that was actually read -- including reference, and the
    # separate debit / credit columns when the statement splits them -- so the
    # user can confirm each mapped column, not just date/description/amount.
    show <- setdiff(.cols_with_data(tx), "row_id")
    lead <- intersect(c("date", "description", "amount", "debit", "credit", "direction",
                        "balance", "reference", "particulars", "code", "other_party", "type"), show)
    show <- c(lead, setdiff(show, lead))
    if (!length(show)) show <- names(tx)
    datatable(tx[, show, drop = FALSE], rownames = FALSE, colnames = cv_friendly_cols(show),
              options = list(dom = "t", pageLength = 12, scrollX = TRUE))
  })
  # The preview verdict. This is where a template gets abandoned: a grey line of
  # monospace saying "no rows detected" reads as a dead end, so it now says which
  # setting to reach for -- and, for a PDF, that the preview only reads the first
  # few pages, which is why a statement whose table starts later looks empty here.
  output$g_status <- renderUI({
    g <- guided(); req(g)
    tx <- g_preview_tx()
    n <- if (is.null(tx)) 0L else nrow(tx)
    is_pdf <- identical(g$tmpl$format, "pdf")
    if (n > 0L) return(div(class = "verdict verdict-high", style = "margin:2px 0 12px",
      div(class = "verdict-ico", "✓"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title",
            sprintf("%d transaction row%s read", n, if (n == 1L) "" else "s")),
        p(class = "verdict-body", style = "margin:0",
          "Check the dates, descriptions and amounts below against your statement. If they are right, click Save template."))))
    div(class = "verdict verdict-medium", style = "margin:2px 0 12px",
      div(class = "verdict-ico", "!"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", "No transaction rows read yet"),
        p(class = "verdict-body", style = "margin:0 0 4px", if (is_pdf)
            "Rows are found by their date, so start with the date column: does your box cover the dates on the page? If it does, open \"Show the settings for this statement\" and check the date format and the amount style."
          else "Start with the Date and Amount column pickers above - are they pointing at the right columns? If they are, open \"Show the settings for this statement\" and check the date format and the amount style."),
        if (is_pdf) p(class = "muted", style = "margin:0",
          "This preview reads only the first few pages. If the transactions start further into the document that is why nothing shows here - saving and converting reads every page.")))
  })
  observeEvent(input$g_save, {
    g <- guided(); req(g)
    tmpl <- guided_live()
    # If we opened a tested (default) template to refine it, saving under the same
    # id would be shadowed - curated defaults win on an id clash. Give the
    # customised copy a distinct id so the accountant's fix actually takes effect.
    # Record WHAT she was fixing, not just that the copy is different. Without this
    # her correction was outvoted by the very template she opened to correct: both
    # carry the same fingerprint, so they tie on every statement, and the tie-break
    # prefers the tested one. She then had no way to win except by making her
    # fingerprint MORE specific, which is not why she lost, and the tool told her
    # so. `refines` says "this exists to replace that one", and detection honours it.
    if (!is.null(g$default_ids) && (tmpl$id %||% "") %in% g$default_ids) {
      tmpl$refines <- tmpl$id
      tmpl$id <- paste0(tmpl$id, "_custom")
    }
    # Surface the ACTUAL reason a save fails (validation problems name the field,
    # never any statement content) instead of a dead-end generic toast.
    err <- tryCatch({ save_user_template(tmpl, USER_TEMPLATES_DIR); NULL },
                    error = function(e) conditionMessage(e))
    if (is.null(err)) {
      tpl_bump(isolate(tpl_bump()) + 1); removeModal()
      # mark this upload as taught, so it drops off the "needs pickup" list
      if (!is.na(cv_upload_id()))
        safe(set_upload_status(cv_upload_id(), "wizard_saved",
          template = tmpl$id %||% NA_character_, dir = UPLOADS_DIR))
      saved_id <- tmpl$id %||% NA_character_
      gp <- g$path; gn <- g$name %||% (if (!is.null(gp)) basename(gp) else NA_character_)
      if (!is.null(gp) && file.exists(gp) && !is.na(saved_id) && nzchar(saved_id)) {
        updateTabsetPanel(session, "main_tabs", selected = "Convert")
        # PROVE IT. The old flow re-converted with the new template FORCED by id,
        # which convert.R short-circuits to "template chosen by the user" -- so the one
        # moment the app could have shown that the new template auto-detects was the
        # one moment it skipped detection entirely. That is how templates with
        # fingerprints that could never match shipped under a green suite. Run a
        # REAL detection pass over the whole template set first, say in plain words
        # what it means, and only fall back to forcing when detection did NOT pick
        # this template (so she still sees her template's output, correctly labelled).
        recog <- safe({
          tset <- load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR)
          recognition_summary(detect_statement(read_input(gp), tset), saved_id)
        }, NULL)
        recog <- recog %||% recognition_summary(NULL, saved_id)
        run_conversion(gp, gn, record = FALSE,
                       force_tpl = if (isTRUE(recog$ok)) NULL else saved_id,
                       include_user = TRUE)
        showNotification(
          HTML(paste0("<b>Saved \"", htmltools::htmlEscape(saved_id), "\".</b><br>",
                      "<b>", htmltools::htmlEscape(recog$headline), "</b><br>",
                      htmltools::htmlEscape(recog$detail))),
          type = if (isTRUE(recog$ok)) "message" else "warning",
          duration = if (isTRUE(recog$ok)) 10 else NULL)
      } else {
        showNotification(sprintf("Saved as your template \"%s\". Click Convert again to run this statement with it.",
                                 saved_id %||% "template"),
                         type = "message", duration = 8)
      }
    } else {
      # A refused save is nearly always the identifying phrase, and its box now sits
      # behind the disclosure. OPEN it rather than naming a place she then has to go
      # and find -- the tool knows where the fix is, so it should not make her look.
      g_more_open(TRUE)
      showNotification(HTML(paste0("<b>Couldn't save.</b> ", htmltools::htmlEscape(err),
        "<br>The settings for this statement are now open on the right - fix it there, or use <b>Advanced</b> for the whole template as text.")),
        type = "error", duration = 12)
    }
  })

  # ---- Admin: insights from the logs -------------------------------
  adm_data <- reactiveVal(NULL)
  load_admin <- function() adm_data(list(
    runs = tryCatch(read_runs_all(LOGDIR), error = function(e) data.frame()),  # live + archived
    fb   = tryCatch(read_feedback(LOGDIR), error = function(e) data.frame())))
  # Admin dashboard data (run logs, feedback) is loaded ONLY for an authenticated
  # admin session, so a non-admin client can never pull it by marking a hidden
  # output visible -- every admin output does req(adm_data()), which stays NULL
  # (and therefore blank) without a load.
  observeEvent(input$adm_refresh, { req(admin_ok()); load_admin() })
  observe({ req(admin_ok()); if (is.null(adm_data())) load_admin() })

  output$adm_overview <- renderDT({
    d <- adm_data(); req(d)
    datatable(runs_overview(d$runs), rownames = FALSE, options = list(dom = "t"))
  })
  output$adm_status_plot <- renderPlot({
    d <- adm_data(); req(d); ov <- runs_overview(d$runs); if (!nrow(ov)) return(NULL)
    cols <- c(ok = "#137333", needs_review = "#e3b341", unsupported = "#b00020",
              failed = "#7d1a1a")[ov$status]
    cols[is.na(cols)] <- "#888"
    op <- par(mar = c(5, 4, 1, 1)); on.exit(par(op))
    barplot(setNames(ov$n, ov$status), col = cols, las = 2, ylab = "conversions")
  })
  output$adm_gaps <- renderDT({
    d <- adm_data(); req(d)
    g <- unsupported_clusters(d$runs)
    if (!nrow(g)) return(datatable(data.frame(message = "No unsupported statements logged yet."),
                                   rownames = FALSE, options = list(dom = "t")))
    datatable(g[, c("count", "layout", "closest_template", "why", "last_seen", "example_file")],
              rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
      formatStyle("count", fontWeight = "bold")
  })
  output$adm_usage <- renderDT({
    d <- adm_data(); req(d)
    u <- template_usage(d$runs, d$fb)
    if (!nrow(u)) return(datatable(data.frame(message = "No matched conversions yet."),
                                   rownames = FALSE, options = list(dom = "t")))
    datatable(u, rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
  output$adm_drift <- renderDT({
    d <- adm_data(); req(d)
    dr <- template_drift(d$runs)
    if (!nrow(dr)) return(datatable(data.frame(message = "No drift detected."),
                                    rownames = FALSE, options = list(dom = "t")))
    datatable(dr, rownames = FALSE, options = list(dom = "t")) |>
      formatStyle("drop", fontWeight = "bold", color = "#b00020")
  })
  observeEvent(input$adm_rollup, {
    req(admin_ok())
    r <- tryCatch(rollup_logs(LOGDIR, "runs", keep_days = LOG_KEEP_DAYS), error = function(e) NULL)
    r2 <- tryCatch(rollup_logs(LOGDIR, "feedback", keep_days = LOG_KEEP_DAYS), error = function(e) NULL)
    load_admin()
    output$adm_rollup_msg <- renderUI(span(class = "ok",
      sprintf("Archived %d old run file(s); %d kept. History is preserved in logs/archive/.",
              (r$archived %||% 0) + (r2$archived %||% 0), (r$kept %||% 0))))
  })
  observeEvent(input$adm_purge_uploads, {
    req(admin_ok())
    if (UPLOADS_KEEP_DAYS <= 0) {
      output$adm_purge_msg <- renderUI(span(class = "bad",
        "Nothing deleted: retention is set to keep saved statements indefinitely. Set retention.uploads_keep_days in config/config.yaml and restart."))
      return()
    }
    p <- tryCatch(purge_uploads(UPLOADS_DIR, keep_days = UPLOADS_KEEP_DAYS), error = function(e) NULL)
    if (is.null(p)) {
      output$adm_purge_msg <- renderUI(span(class = "bad",
        "Could not tidy up the saved statements - check folder permissions on uploads/."))
      return()
    }
    output$adm_purge_msg <- renderUI(span(class = "ok", sprintf(
      "Deleted %d saved statement file(s); %d still within the %d-day period. The record of each upload is kept - only the statement itself is gone.",
      p$purged, p$kept, as.integer(UPLOADS_KEEP_DAYS))))
  })
  output$adm_feedback <- renderDT({
    d <- adm_data(); req(d); fb <- d$fb
    if (is.null(fb) || !nrow(fb) || !("flagged" %in% names(fb)))
      return(datatable(data.frame(message = "No feedback yet."), rownames = FALSE, options = list(dom = "t")))
    fl <- fb[isTRUE(TRUE) & as.logical(fb$flagged) %in% TRUE, , drop = FALSE]
    if (!nrow(fl)) return(datatable(data.frame(message = "No flagged feedback."),
                                    rownames = FALSE, options = list(dom = "t")))
    datatable(fl[, intersect(c("ts", "verdict", "comment", "template_id", "run_id"), names(fl))],
              rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })
}

shinyApp(ui, server)
