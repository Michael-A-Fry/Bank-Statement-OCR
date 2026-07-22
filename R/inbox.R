# inbox.R -- a read-only view of the folder-drop intake folders for the Admin
# "Folder intake" panel. The folder-drop pipeline moves each dropped statement from
# inbox/ to processed/ (converted) or failed/ (couldn't), writing outputs to
# outbox/<name>/, and parks unmovable files in stuck/. This surfaces those
# folders in the app so the intake is visible and its failures are actionable --
# no more "nothing seems to be in processed/". Read-only: it never moves files.

# .folder_files(dir) -> data.frame(file, size_kb, modified), newest first.
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

# inbox_status(root) -> list(folders = named list<data.frame>, counts = named int).
# Folders: inbox (waiting), processed (done), failed (needs attention),
# stuck (couldn't be moved -- a permissions problem), outbox (output folders).
inbox_status <- function(root = ".") {
  folders <- c("inbox", "processed", "failed", "stuck", "outbox")
  out <- lapply(folders, function(d) .folder_files(file.path(root, d)))
  names(out) <- folders
  list(folders = out, counts = vapply(out, nrow, integer(1)))
}

# failed_file_path(name, root) -> path of a failed original, for re-audit/wizard.
failed_file_path <- function(name, root = ".") {
  p <- file.path(root, "failed", name)
  if (file.exists(p)) p else NA_character_
}
