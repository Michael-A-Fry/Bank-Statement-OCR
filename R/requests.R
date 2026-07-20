# requests.R -- the "none of these fits -- tell our team" escape hatch. When a
# statement's format matches no dropdown option, the accountant can describe it in
# plain words and raise it for review. The engine can't teach itself, but nothing
# is lost: a request is logged for a maintainer to turn into a template.
#
# PII-SAFE BY DESIGN: a request stores ONLY the free-text the user typed plus
# generic, non-identifying context (file extension, the bank label, and which
# date / amount options were on screen). It NEVER stores statement content, and
# the UI warns the user to describe the format, not paste statement details. The
# requests folder is git-ignored so nothing typed here is ever committed.

.requests_dir <- function(dir = NULL) dir %||% file.path(Sys.getenv("BSO_ROOT", "."), "requests")

# record_template_request(detail, context, requested_by, dir) -> request id.
# `detail` is the user's description; `context` is a small named list of generic,
# non-PII fields (file_ext, bank, date_format, amount_style, ...).
record_template_request <- function(detail, context = list(),
                                    requested_by = NULL, dir = NULL) {
  dir <- .requests_dir(dir); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  id <- paste0("req-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
               substr(sprintf("%09.0f", abs(sum(utf8ToInt(paste0(detail, ts))))), 1, 4))
  rec <- list(id = id, ts = ts, requested_by = requested_by %||% "unknown",
              status = "open", detail = as.character(detail %||% ""),
              context = context %||% list(),
              history = list(list(ts = ts, status = "open")))
  safe(writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE),
                  file.path(dir, paste0(id, ".json"))))
  id
}

# set_request_status(id, status, dir) -- triage a request (open/actioned/dismissed).
set_request_status <- function(id, status, dir = NULL) {
  dir <- .requests_dir(dir); f <- file.path(dir, paste0(id, ".json"))
  if (!file.exists(f)) return(invisible(FALSE))
  rec <- safe(jsonlite::fromJSON(f, simplifyVector = FALSE), NULL); if (is.null(rec)) return(invisible(FALSE))
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  rec$status <- status
  rec$history <- c(rec$history, list(list(ts = ts, status = status)))
  safe(writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE), f))
  invisible(TRUE)
}

# read_template_requests(dir) -> data.frame of raised requests, newest first, for
# the Admin review queue. The `context` map is flattened to a compact string.
read_template_requests <- function(dir = NULL) {
  dir <- .requests_dir(dir)
  recs <- Sys.glob(file.path(dir, "*.json"))
  empty <- data.frame(id = character(0), ts = character(0), requested_by = character(0),
                      status = character(0), detail = character(0), context = character(0),
                      stringsAsFactors = FALSE)
  if (!length(recs)) return(empty)
  rows <- lapply(recs, function(f) {
    r <- safe(jsonlite::fromJSON(f, simplifyVector = FALSE), NULL); if (is.null(r)) return(NULL)
    ctx <- r$context %||% list()
    ctx_str <- if (length(ctx))
      paste(vapply(names(ctx), function(k) sprintf("%s=%s", k, as.character(ctx[[k]] %||% "")),
                   character(1)), collapse = "; ") else ""
    data.frame(id = r$id %||% NA_character_, ts = r$ts %||% NA_character_,
      requested_by = as.character(r$requested_by %||% "unknown"),
      status = as.character(r$status %||% "open"),
      detail = as.character(r$detail %||% ""), context = ctx_str,
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(empty)
  out[order(out$ts, decreasing = TRUE), , drop = FALSE]
}
