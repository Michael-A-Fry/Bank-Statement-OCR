# fixes.R -- a person's word that the arithmetic could not back, held for an admin.
#
# Spec section 2: "an unprovable fix applies to that file only until an admin
# confirms it". When a person sets the columns' roles on Please check and the
# corrected reading still does not prove, or confirms a reading the arithmetic
# could not prove, that reading is used for THAT file only and nothing is learned.
# Its template is held here so an admin can make it one of the bank's layouts
# (layout_correct with no id, R/layouts.R) -- one person's word never teaches the
# tool on its own.
#
# Kept in <layouts>/.pending/<id>.yaml. The leading dot keeps the folder out of
# the layout listing (R/layouts.R lists bank folders by a slug pattern), so a held
# fix can never be read as a layout, nor change layouts_state_id(). A held fix
# carries no account number, name or figure: the template is roles, formats and
# heading words, made fit to keep by .layout_template.
#
# Edited column BOXES are not held: a layout deliberately does not remember
# positions (spec section 6), so boxes dragged for one file are that file's alone.
#
# Every function returns a result; none throws.

.FIX_KINDS <- c("roles", "confirm")

.fix_dir <- function(dir) file.path(dir, ".pending")

# fix_hold(template, bank, kind, by, dir) -> list(ok, id, why). The same template
# held twice for the same bank is one held fix (the id is a hash of both).
fix_hold <- function(template, bank, kind = "roles", by = NULL, dir = layouts_dir()) {
  tryCatch({
    b <- .layout_bank(bank)
    if (is.na(b$slug)) return(list(ok = FALSE, id = NA_character_, why = "No bank was given, so the fix was not kept."))
    if (!(kind %in% .FIX_KINDS)) return(list(ok = FALSE, id = NA_character_, why = "Not a kind of fix that is kept."))
    sig <- .layout_sig_norm(template$signature)
    if (!is.null(.layout_sig_problem(sig)))
      return(list(ok = FALSE, id = NA_character_, why = "The reading carries no usable layout signature, so it cannot be kept for an admin."))
    tpl <- .layout_template(template, "pending", b$bank, 1L)
    tpl$signature <- sig
    body <- yaml::as.yaml(tpl)
    id <- paste0(b$slug, "_", substr(.text_sha256(paste(b$slug, kind, body)), 1L, 12L))
    rec <- list(id = id, bank = b$bank, kind = kind, by = .layout_person(by), held = utc_stamp(),
                template = tpl)
    fd <- .fix_dir(dir)
    dir.create(fd, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(fd, paste0(id, ".yaml"))
    if (file.exists(path)) return(list(ok = TRUE, id = id, why = "This fix is already waiting for an admin."))
    ok <- save_yaml_safely(rec, path)
    if (!isTRUE(ok)) return(list(ok = FALSE, id = NA_character_, why = attr(ok, "reason") %||% "The fix could not be saved."))
    list(ok = TRUE, id = id, why = "Kept for an admin to confirm; until then it applies to this file only.")
  }, error = function(e) list(ok = FALSE, id = NA_character_, why = paste0("The fix could not be kept (", conditionMessage(e), ").")))
}

# fixes_pending(dir) -> data.frame(id, bank, kind, by, held), oldest first.
fixes_pending <- function(dir = layouts_dir()) {
  empty <- data.frame(id = character(0), bank = character(0), kind = character(0),
                      by = character(0), held = character(0), stringsAsFactors = FALSE)
  tryCatch({
    fs <- list.files(.fix_dir(dir), pattern = "^[a-z0-9_]+[.]yaml$", full.names = TRUE)
    if (!length(fs)) return(empty)
    rows <- lapply(fs, function(f) {
      r <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      if (!is.list(r) || is.null(r$id) || !identical(paste0(r$id, ".yaml"), basename(f))) return(NULL)
      data.frame(id = as.character(r$id), bank = as.character(r$bank %||% NA)[1],
                 kind = as.character(r$kind %||% NA)[1], by = as.character(r$by %||% NA)[1],
                 held = as.character(r$held %||% NA)[1], stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, c(list(empty), rows))
    out <- out[order(out$held, out$id, method = "radix"), , drop = FALSE]
    rownames(out) <- NULL
    out
  }, error = function(e) empty)
}

# fix_accept(id, dir, by) -> list(ok, ref, why): an admin makes the held fix a
# proven layout of its bank (origin corrected), and the held copy goes.
fix_accept <- function(id, dir = layouts_dir(), by = NULL) {
  tryCatch({
    path <- .fix_path(id, dir)
    if (is.null(path)) return(list(ok = FALSE, ref = NA_character_, why = "There is no such fix waiting."))
    r <- yaml::read_yaml(path)
    res <- layout_correct(NULL, r$template, dir = dir, by = by, bank = r$bank)
    if (isTRUE(res$ok)) safe(unlink(path))
    list(ok = isTRUE(res$ok), ref = res$ref, why = res$why)
  }, error = function(e) list(ok = FALSE, ref = NA_character_, why = paste0("The fix could not be confirmed (", conditionMessage(e), ").")))
}

# fix_discard(id, dir) -> list(ok, why): the admin turns the held fix down.
fix_discard <- function(id, dir = layouts_dir()) {
  path <- .fix_path(id, dir)
  if (is.null(path)) return(list(ok = FALSE, why = "There is no such fix waiting."))
  if (!isTRUE(safe(file.remove(path), FALSE))) return(list(ok = FALSE, why = "The fix could not be removed."))
  list(ok = TRUE, why = "removed")
}

.fix_path <- function(id, dir) {
  id <- as.character(id %||% "")[1]
  if (is.na(id) || !grepl("^[a-z0-9_]+$", id)) return(NULL)
  p <- file.path(.fix_dir(dir), paste0(id, ".yaml"))
  if (file.exists(p)) p else NULL
}
