# inbox.R -- a read-only view of the folder-drop intake folders for Admin. It never moves files.

# data.frame(file, size_kb, modified), newest first.
.folder_files <- function(dir) {
  empty <- data.frame(file = character(0), size_kb = numeric(0),
                      modified = character(0), stringsAsFactors = FALSE)
  if (is.null(dir) || !dir.exists(dir)) return(empty)
  fs <- list.files(dir, full.names = TRUE)
  if (!length(fs)) return(empty)
  info <- file.info(fs)
  out <- data.frame(
    file     = basename(fs),
    size_kb  = round((info$size %||% 0) / 1024, 1),
    modified = format(info$mtime, "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE)
  out[order(info$mtime, decreasing = TRUE), , drop = FALSE]
}

# Returns list(folders, counts): inbox, processed, failed, stuck (could not be moved), outbox.
inbox_status <- function(root = ".") {
  folders <- c("inbox", "processed", "failed", "stuck", "outbox")
  out <- lapply(folders, function(d) .folder_files(file.path(root, d)))
  names(out) <- folders
  list(folders = out, counts = vapply(out, nrow, integer(1)))
}

# The path of a failed original, for re-audit or the wizard.
failed_file_path <- function(name, root = ".") {
  # `name` arrives from the browser: refuse anything but a plain filename, so it cannot walk out of
  # failed/ and read arbitrary files off the server.
  if (length(name) != 1L || is.na(name) || !nzchar(name) ||
      grepl("[/\\\\]", name) || name %in% c(".", ".."))
    return(NA_character_)
  p <- file.path(root, "failed", name)
  if (file.exists(p)) p else NA_character_
}
