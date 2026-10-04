# requests.R -- the format requests the team raised in 1.x, for triage on
# Admin -> Health. In 1.x, when a statement's format matched no template, the
# accountant could describe it in plain words and raise it for a maintainer to
# turn into a template. 2.0.0 retired templates and with them the screen that
# raised a request (its writer, record_template_request, had no caller left and
# was removed); the requests already on disk can still be read and marked
# actioned or dismissed here, so none is lost.
#
# PII-SAFE BY DESIGN: a request holds ONLY the free text the person typed plus
# generic, non-identifying context (file extension, the bank label, and which
# date / amount options were on screen). It never held statement content. The
# requests folder is local-only so nothing typed there is ever shared.

.requests_dir <- function(dir = NULL) dir %||% file.path(Sys.getenv("BSO_ROOT", "."), "requests")

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
