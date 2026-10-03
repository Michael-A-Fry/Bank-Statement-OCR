# layouts.R -- the bank layouts the automatic reader has learned (spec section 6,
# Appendix A3).
#
# A layout is the tool's memory of one bank design. In memory it is a template
# list in today's schema (so the table reader, reconciliation and outputs work on
# it unchanged) plus a `layout` block:
#
#   layout:
#     id, bank, status (provisional | proven | retired), version, created,
#     proved_by (sha256 of each statement that proved it), origin (auto |
#     confirmed | corrected), signature (the shared layout signature), and
#     optionally name (an admin's rename) and confirmed_by / corrected_by.
#
# On disk: <dir>/<bank_slug>/<id>@v<version>.yaml. A file is NEVER edited: every
# change -- new evidence, a promotion, a confirm, a correction, a rename, a
# retirement -- writes the next version beside it, so an output stamped with
# layouts_state_id() can always be traced to exactly what was learned then, and an
# admin's undo is one more version rather than a lost one.
#
# Every public function returns a result that says what happened; none throws.

# Statements that must prove a new layout before it is trusted without a person.
LAYOUT_PROVEN_AFTER <- 3L

# layout_match() thresholds. The soft score is the mean of three parts, each 0..1:
#   date   1 when the date formats agree, 0 when they differ, 0.5 when unknown;
#   words  Jaccard overlap of the heading tokens; 0.5 when either side has none
#          (a scan whose headings OCR could not read is missing evidence, not
#          contrary evidence);
#   place  1 - mean |rel_x difference| / LAYOUT_X_SCALE, floored at 0; 0.5 when
#          either side has no positions.
# 0.70 needs two parts to agree strongly: a same-design statement scores about
# 0.9 (a description column's right edge moves with its longest line, so `place`
# is rarely a clean 1), while a different design that merely shares column roles
# and a date style -- other heading words, columns a few percent elsewhere --
# scores about 0.6 and stays a separate layout. Matching too loosely is the costly
# error: evidence from one design would promote another. Two products printed on
# one table (same roles, places and date style; only some heading words differ)
# score about 0.8 and are deliberately one layout: nothing a layout supplies to
# the reader differs between them.
LAYOUT_MATCH_MIN <- 0.70
LAYOUT_X_SCALE   <- 0.10

.LAYOUT_STATUSES <- c("provisional", "proven", "retired")
.LAYOUT_ORIGINS  <- c("auto", "confirmed", "corrected")
.LAYOUT_FILE_RE  <- "^([a-z0-9]+(_[a-z0-9]+)*)@v([1-9][0-9]*)[.]yaml$"
.LAYOUT_KINDS    <- c("pdf", "scan", "delimited", "excel")
.LAYOUT_MONEY    <- c("debit_credit_cols", "signed", "dr_cr_suffix", "unsigned", "type_dc")
.LAYOUT_SIG_FIELDS <- c("kind", "roles", "date_format", "money_style", "sign_markers", "balance_freq",
                        "newest_first", "heading_tokens", "producer", "rel_x", "extras")
.LAYOUT_BLOCK_FIELDS <- c("id", "bank", "status", "version", "created", "proved_by", "origin", "signature")

# layouts_dir(cfg) -- where the store lives (paths$layouts).
layouts_dir <- function(cfg = load_config()) {
  safe(cfg$paths$layouts, NULL) %||% file.path("templates", "layouts")
}

# ---- small helpers ----------------------------------------------------------------------

# .layout_slug(x) -- a bank name as a folder name: "ANZ" -> "anz",
# "The Co-operative Bank" -> "the_co_operative_bank". NA when nothing is left.
# Trimmed AFTER cutting to length, because "a_" would give ids ("a__1") that the
# file pattern never lists: the layout would be written and then never found. A
# name Windows keeps for a device ("con", "aux", "com1") cannot be a folder there,
# so it gets a suffix.
.layout_slug <- function(x) {
  s <- tolower(trimws(as.character(x %||% NA_character_)[1]))
  if (is.na(s)) return(NA_character_)
  s <- gsub("^_+|_+$", "", substr(gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", s)), 1L, 40L))
  if (!nzchar(s)) return(NA_character_)
  if (grepl("^(con|prn|aux|nul|com[0-9]|lpt[0-9])$", s)) s <- paste0(s, "_bank")
  s
}

# .layout_bank(bank) -- a bank given as a name/id or as bank_pick()'s result.
.layout_bank <- function(bank) {
  blocked <- FALSE
  if (is.list(bank)) {
    blocked <- isTRUE(bank$block_learning)
    bank <- bank$bank
  }
  b <- trimws(as.character(bank %||% NA_character_)[1])
  if (!is.na(b) && !nzchar(b)) b <- NA_character_
  list(bank = b, slug = .layout_slug(b), blocked = blocked)
}

# .layout_sort(x) -- sort that does not depend on the server's locale.
.layout_sort <- function(x) sort(x, method = "radix")

# .layout_sig_norm(sig) -- a signature in exactly the shared contract's types.
# YAML gives back list() for an empty vector and drops attributes, so whatever
# comes in -- from the reader or from a file -- leaves in one shape and two equal
# signatures compare equal.
.layout_sig_norm <- function(sig) {
  if (!is.list(sig)) return(NULL)
  chr <- function(x) { x <- as.character(unlist(x)); x[!is.na(x)] }
  one <- function(x, d) { x <- as.character(unlist(x)); if (length(x) && !is.na(x[1])) x[1] else d }
  rel <- suppressWarnings(as.numeric(unlist(sig$rel_x)))
  list(kind = one(sig$kind, ""), roles = chr(sig$roles), date_format = one(sig$date_format, ""),
       money_style = one(sig$money_style, ""),
       sign_markers = .layout_sort(unique(chr(sig$sign_markers))),
       balance_freq = one(sig$balance_freq, "none"),
       newest_first = isTRUE(as.logical(unlist(sig$newest_first))[1]),
       heading_tokens = utils::head(.layout_sort(unique(tolower(chr(sig$heading_tokens)))), 40L),
       producer = one(sig$producer, ""),
       rel_x = round(rel[!is.na(rel)], 2),
       extras = chr(sig$extras))
}

# .layout_sig_problem(sig) -- why a (normalised) signature cannot name a layout,
# or NULL when it can. A layout learned from a malformed signature would never
# match its own design again (a position list that does not cover the columns
# scores 0 on place), so every later statement of the design would start yet
# another layout: refused here instead.
.layout_sig_problem <- function(sig) {
  if (is.null(sig)) return("no layout signature")
  if (!(sig$kind %in% .LAYOUT_KINDS)) return("no known kind of file")
  if (!length(sig$roles) || any(!nzchar(sig$roles))) return("no column roles")
  if (!(sig$money_style %in% .LAYOUT_MONEY)) return("no known money style")
  if (length(sig$rel_x) != length(sig$roles) || any(sig$rel_x < 0 | sig$rel_x > 1.05))
    return("column positions that do not match its columns")
  NULL
}

# .layout_ordinal(id) -- the number in "anz_3", for ordering and display.
.layout_ordinal <- function(id) {
  n <- suppressWarnings(as.integer(sub("^.*_([0-9]+)$", "\\1", id)))
  ifelse(grepl("_[0-9]+$", id), n, NA_integer_)
}

# .layout_files(dir, slug) -- every version file in the store (or one bank's
# folder): data.frame(path, slug, id, version). Backups, temp files and anything
# not named <id>@v<n>.yaml are not layouts and are never listed; nor is a file
# whose id is not its own folder's (<slug>_<n>), since a copy of anz_1 dropped in
# another bank's folder would otherwise be read as one more version of anz_1.
.layout_files <- function(dir, slug = NULL) {
  empty <- data.frame(path = character(0), slug = character(0), id = character(0),
                      version = integer(0), stringsAsFactors = FALSE)
  if (is.null(dir) || !dir.exists(dir)) return(empty)
  slugs <- if (is.null(slug)) list.dirs(dir, full.names = FALSE, recursive = FALSE) else slug
  slugs <- slugs[!is.na(slugs) & grepl("^[a-z0-9_]+$", slugs)]
  rows <- lapply(.layout_sort(slugs), function(s) {
    f <- list.files(file.path(dir, s), pattern = "[.]yaml$")
    f <- f[grepl(.LAYOUT_FILE_RE, f) & grepl(paste0("^", s, "_[0-9]+@"), f)]
    if (!length(f)) return(NULL)
    data.frame(path = file.path(dir, s, f), slug = s, id = sub(.LAYOUT_FILE_RE, "\\1", f),
               version = as.integer(sub(.LAYOUT_FILE_RE, "\\3", f)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(empty), rows))
  if (!nrow(out)) return(empty)
  o <- order(out$slug, .layout_ordinal(out$id), out$id, out$version, method = "radix")
  out <- out[o, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# .layout_read(path, id, version) -- one version file, checked against its own
# name and for completeness. Returns the layout, or a string saying why it cannot
# be used. Files are only ever written whole (.layout_write), but one copied in
# by hand can be cut short, and a cut-short YAML usually still parses: so every
# field of the layout block and of its signature must be present, and the
# signature must be one a layout can be matched on.
.layout_read <- function(path, id, version) {
  ly <- tryCatch(suppressWarnings(yaml::read_yaml(path)), error = function(e) NULL)
  if (!is.list(ly) || !is.list(ly$layout)) return(sprintf("%s could not be read as a layout.", basename(path)))
  lb <- ly$layout
  if (!identical(as.character(lb$id %||% "")[1], id) ||
      !identical(suppressWarnings(as.integer(lb$version %||% NA)[1]), as.integer(version)))
    return(sprintf("%s does not hold the layout its name says (%s version %d).", basename(path), id, version))
  # a PDF layout's columns are under table:, a CSV or Excel one's at the top
  if (!all(.LAYOUT_BLOCK_FIELDS %in% names(lb)) || !(is.list(ly$table) || is.list(ly$columns)) ||
      !is.list(lb$signature) || !all(.LAYOUT_SIG_FIELDS %in% names(lb$signature)))
    return(sprintf("%s is incomplete (it may have been cut short).", basename(path)))
  if (!(as.character(lb$status %||% "")[1] %in% .LAYOUT_STATUSES) ||
      !(as.character(lb$origin %||% "")[1] %in% .LAYOUT_ORIGINS))
    return(sprintf("%s has no valid status or origin.", basename(path)))
  lb$status <- as.character(lb$status)[1]
  lb$origin <- as.character(lb$origin)[1]
  lb$version <- as.integer(version)
  pb <- unique(tolower(as.character(unlist(lb$proved_by))))
  lb$proved_by <- pb[!is.na(pb) & grepl("^[0-9a-f]{64}$", pb)]
  lb$signature <- .layout_sig_norm(lb$signature)
  why <- .layout_sig_problem(lb$signature)
  if (!is.null(why)) return(sprintf("%s has %s.", basename(path), why))
  ly$layout <- lb
  ly
}

# .layout_lock(slugdir) -- one writer per bank at a time. dir.create() is atomic
# on Windows and POSIX alike, so it is the lock; a lock older than two minutes was
# left by a process that died and is taken over. The holder leaves a token in it
# and releases only a lock that still holds its own token, so a slow holder whose
# lock was taken over cannot then free the new holder's. A batch learning many
# statements at once queues here for up to ten seconds (a learn holds the lock for
# a few milliseconds). Returns a release function, or NULL when the bank stayed busy.
.LAYOUT_LOCKS <- new.env(parent = emptyenv())
.layout_lock <- function(slugdir) {
  dir.create(slugdir, recursive = TRUE, showWarnings = FALSE)
  lk <- file.path(slugdir, ".lock")
  tok <- file.path(lk, "owner")
  # pid + clock + a counter, not sample(): a lock must not move the session's
  # random stream, which spot-check sampling and seeded tests depend on.
  .LAYOUT_LOCKS$n <- (.LAYOUT_LOCKS$n %||% 0L) + 1L
  me <- paste(Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS6"), .LAYOUT_LOCKS$n)
  for (i in seq_len(200L)) {
    if (isTRUE(suppressWarnings(dir.create(lk)))) {
      safe(writeLines(me, tok))
      return(function() {
        if (identical(safe(readLines(tok, warn = FALSE), character(0))[1], me)) unlink(lk, recursive = TRUE)
      })
    }
    age <- safe(as.numeric(difftime(Sys.time(), file.mtime(lk), units = "secs")), NA)
    if (!is.na(age) && age > 120) { unlink(lk, recursive = TRUE); next }
    Sys.sleep(0.05)
  }
  NULL
}

# .layout_write(ly, dir) -- write a NEW version file; never replaces one, even
# when two writers race for the same version (the lock makes that rare; a stale
# lock taken over by two processes at once is how it could still happen). The
# file is written whole to a temp name and then hard-linked into place: creating a
# link fails when the name exists, atomically, where a rename would silently
# replace the other writer's file. Where the disk cannot link (FAT, some shares)
# it falls back to a rename after checking the name is free.
.layout_write <- function(ly, dir) {
  lb <- ly$layout
  path <- file.path(dir, .layout_slug(lb$bank), sprintf("%s@v%d.yaml", lb$id, as.integer(lb$version)))
  taken <- structure(FALSE, reason = sprintf("%s version %d already exists, so nothing was changed.", lb$id, lb$version))
  if (file.exists(path)) return(taken)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".", Sys.getpid(), ".part")
  on.exit(safe(unlink(tmp)), add = TRUE)
  warned <- FALSE
  ok <- isTRUE(withCallingHandlers(
    tryCatch({ yaml::write_yaml(ly, tmp); TRUE }, error = function(e) FALSE),
    warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }))
  if (!ok || warned || !file.exists(tmp))
    return(structure(FALSE, reason = "could not write the file - check the folder permissions"))
  if (isTRUE(safe(suppressWarnings(file.link(tmp, path)), FALSE))) return(structure(TRUE, reason = "saved"))
  if (file.exists(path)) return(taken)
  if (!isTRUE(safe(file.rename(tmp, path), FALSE)))
    return(structure(FALSE, reason = "could not put the new version in place"))
  structure(TRUE, reason = "saved")
}

# .layout_block(...) -- the layout block in a fixed field order, so two writes of
# the same facts are the same file.
.layout_block <- function(id, bank, status, version, proved_by, origin, signature,
                          name = NULL, confirmed_by = NULL, corrected_by = NULL) {
  b <- list(id = id, bank = bank, status = status, version = as.integer(version),
            created = utc_stamp(), proved_by = as.list(unique(proved_by)), origin = origin,
            signature = .layout_sig_norm(signature), name = name,
            confirmed_by = confirmed_by, corrected_by = corrected_by)
  b[!vapply(b, is.null, logical(1))]
}

# .layout_template(tpl, id, bank, version) -- a reading's template made fit to
# keep: what belongs to one file (each page's own boxes, the per-file box frames,
# the outcome of that reading) is dropped, because absolute positions are
# deliberately not part of a layout's identity.
.layout_template <- function(tpl, id, bank, version) {
  tpl <- if (is.list(tpl)) tpl else list()
  tpl$layout <- NULL
  tpl$signature <- NULL
  tpl$boxes <- NULL
  if (is.list(tpl$table)) tpl$table$columns_by_page <- NULL
  if (is.list(tpl$auto)) tpl$auto$outcome <- NULL
  tpl$id <- id
  tpl$bank <- bank
  tpl$version <- as.integer(version)
  tpl
}

# .layout_person(by) -- who did it, as a short one-line name, or NULL.
.layout_person <- function(by) {
  s <- trimws(gsub("[[:cntrl:]]+", " ", as.character(by %||% NA_character_)[1]))
  if (is.na(s) || !nzchar(s)) NULL else substr(s, 1L, 80L)
}

# .layout_find(id, dir) -- the latest version of one layout, retired or not.
# Accepts "anz_3" or "anz_3@v2" (the version part is ignored: changes always
# build on the latest).
.layout_find <- function(id, dir) {
  id <- sub("@v?[0-9]+$", "", tolower(trimws(as.character(id %||% "")[1])))
  if (is.na(id) || !grepl("^[a-z0-9_]+$", id)) return(list(error = "No layout was named."))
  fs <- .layout_files(dir)
  fs <- fs[fs$id == id, , drop = FALSE]
  if (!nrow(fs)) return(list(error = sprintf("There is no layout %s.", id)))
  last <- fs[nrow(fs), ]
  ly <- .layout_read(last$path, last$id, last$version)
  if (is.character(ly)) return(list(error = ly))
  list(layout = ly, slug = last$slug)
}

# .layout_change(id, dir, change) -- the one path every admin change takes: under
# the bank's lock, read the latest version, let `change` return the next one (or a
# sentence saying why nothing changes), write it as version + 1.
.layout_change <- function(id, dir, change) {
  res <- function(ok, ref, why, changed = FALSE) list(ok = ok, changed = changed, ref = ref, why = why)
  tryCatch({
    f <- .layout_find(id, dir)
    if (!is.null(f$error)) return(res(FALSE, NA_character_, f$error))
    release <- .layout_lock(file.path(dir, f$slug))
    if (is.null(release)) return(res(FALSE, NA_character_, "The bank's layouts are being changed by someone else; try again."))
    on.exit(release(), add = TRUE)
    f <- .layout_find(id, dir)                # re-read under the lock
    if (!is.null(f$error)) return(res(FALSE, NA_character_, f$error))
    old <- f$layout
    nxt <- change(old)
    ref0 <- sprintf("%s@%d", old$layout$id, old$layout$version)
    if (is.character(nxt)) return(res(TRUE, ref0, nxt))
    if (!is.list(nxt)) return(res(FALSE, ref0, "The change could not be made."))
    v <- old$layout$version + 1L
    nxt$version <- v
    nxt$layout$version <- v
    ok <- .layout_write(nxt, dir)
    ref <- sprintf("%s@%d", nxt$layout$id, v)
    if (!isTRUE(ok)) return(res(FALSE, ref0, attr(ok, "reason") %||% "The layout could not be saved."))
    res(TRUE, ref, "saved", changed = TRUE)
  }, error = function(e) res(FALSE, NA_character_, paste0("The layout could not be changed (", conditionMessage(e), ").")))
}

# ---- reading the store ------------------------------------------------------------------

# layouts_load(dir, bank, include_retired) -> named list (by id) of the LATEST
# version of each layout, in a fixed order (bank, then layout number). A layout
# whose latest version is retired is left out unless asked for -- never replaced
# by an older version, which would quietly undo the retirement. A file that
# cannot be used is skipped and named in attr(, "problems").
layouts_load <- function(dir = layouts_dir(), bank = NULL, include_retired = FALSE) {
  tryCatch({
    slug <- if (is.null(bank)) NULL else .layout_bank(bank)$slug
    if (!is.null(bank) && is.na(slug)) return(structure(list(), problems = character(0)))
    fs <- .layout_files(dir, slug)
    out <- list(); problems <- character(0)
    if (nrow(fs)) {
      last <- fs[!duplicated(fs$id, fromLast = TRUE), , drop = FALSE]
      last <- last[order(last$slug, .layout_ordinal(last$id), last$id, method = "radix"), , drop = FALSE]
      for (i in seq_len(nrow(last))) {
        ly <- .layout_read(last$path[i], last$id[i], last$version[i])
        if (is.character(ly)) { problems <- c(problems, ly); next }
        if (!include_retired && identical(ly$layout$status, "retired")) next
        out[[last$id[i]]] <- ly
      }
    }
    structure(out, problems = problems)
  }, error = function(e) structure(list(), problems = paste0("The layouts could not be read (", conditionMessage(e), ").")))
}

# layouts_banks(dir) -> data.frame, one row per bank folder: bank (as shown),
# slug, layouts (in use), proven, provisional, retired, statements (distinct
# statements that proved its layouts in use).
layouts_banks <- function(dir = layouts_dir()) {
  empty <- data.frame(bank = character(0), slug = character(0), layouts = integer(0),
                      proven = integer(0), provisional = integer(0), retired = integer(0),
                      statements = integer(0), stringsAsFactors = FALSE)
  tryCatch({
    all <- layouts_load(dir, include_retired = TRUE)
    if (!length(all)) return(empty)
    slugs <- vapply(all, function(l) .layout_slug(l$layout$bank), "")
    rows <- lapply(unique(slugs), function(s) {
      ls <- all[slugs == s]
      st <- vapply(ls, function(l) l$layout$status, "")
      live <- ls[st != "retired"]
      data.frame(bank = .layout_bank_display(s, ls[[length(ls)]]$layout$bank), slug = s,
                 layouts = length(live), proven = sum(st == "proven"),
                 provisional = sum(st == "provisional"), retired = sum(st == "retired"),
                 statements = length(unique(unlist(lapply(live, function(l) l$layout$proved_by)))),
                 stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }, error = function(e) empty)
}

# .layout_bank_display(slug, fallback) -- the bank's name as people know it:
# from the bank list (R/bank_identity.R) when the folder is a known institution,
# else the name the layouts were filed under.
.layout_bank_display <- function(slug, fallback = slug) {
  ref <- if (exists(".bi_ref", mode = "function")) safe(.bi_ref(), NULL) else NULL
  d <- if (!is.null(ref) && !is.na(slug) && slug %in% names(ref$display)) ref$display[[slug]] else NULL
  as.character(d %||% fallback %||% slug)[1]
}

# layouts_state_id(dir) -> a short hash of every layout file's name and content:
# the learned-state version stamped on every output. Anything learned, confirmed,
# corrected, renamed or retired adds a file and so changes it; nothing else does
# (backups, temp files and the lock are not layout files). Lines are read as text
# so the same files give the same id whichever line endings they were written with.
# "empty" when nothing has been learned yet.
layouts_state_id <- function(dir = layouts_dir()) {
  tryCatch({
    fs <- .layout_files(dir)
    if (!nrow(fs)) return("empty")
    rel <- paste0(fs$slug, "/", basename(fs$path))
    o <- order(rel, method = "radix")
    hs <- vapply(o, function(i) {
      txt <- readLines(fs$path[i], warn = FALSE, encoding = "UTF-8")
      paste0(rel[i], " ", .text_sha256(paste(txt, collapse = "\n")))
    }, "")
    h <- .text_sha256(paste(hs, collapse = "\n"))
    if (is.na(h)) "unknown" else substr(h, 1L, 12L)
  }, error = function(e) "unknown")
}

# ---- matching ---------------------------------------------------------------------------

# layout_match(signature, layouts) -> the layout this signature belongs to, or
# NULL. Hard keys first -- kind family (a PDF and a scan of one design are one
# family), the column roles in order (extra text columns compared by place, not
# name: see .layout_role_key), the money style -- then the soft score
# described at LAYOUT_MATCH_MIN. Retired layouts never match. Ties go to the
# higher score, then a proven layout, then the one with more evidence, then the
# lower layout number, so the same store always gives the same answer.
# Returns list(ref = "<id>@<version>", id, version, score, date, words, place).
layout_match <- function(signature, layouts) {
  tryCatch({
    sig <- .layout_sig_norm(signature)
    if (is.null(sig) || !length(sig$roles) || !nzchar(sig$kind)) return(NULL)
    fam <- function(k) if (k %in% c("pdf", "scan")) "pdf" else k
    best <- NULL
    for (ly in layouts) {
      if (!is.list(ly)) next
      lb <- ly$layout %||% list()
      if (identical(lb$status, "retired")) next
      ls <- .layout_sig_norm(lb$signature %||% ly$signature)
      if (is.null(ls)) next
      if (!identical(fam(sig$kind), fam(ls$kind)) ||
          !identical(.layout_role_key(sig$roles, sig$money_style), .layout_role_key(ls$roles, ls$money_style)) ||
          !identical(sig$money_style, ls$money_style)) next
      d <- if (!nzchar(sig$date_format) || !nzchar(ls$date_format)) 0.5
           else as.numeric(identical(sig$date_format, ls$date_format))
      u <- union(sig$heading_tokens, ls$heading_tokens)
      w <- if (!length(sig$heading_tokens) || !length(ls$heading_tokens)) 0.5
           else length(intersect(sig$heading_tokens, ls$heading_tokens)) / length(u)
      p <- if (!length(sig$rel_x) || !length(ls$rel_x)) 0.5
           else if (length(sig$rel_x) == length(ls$rel_x))
             max(0, 1 - mean(abs(sig$rel_x - ls$rel_x)) / LAYOUT_X_SCALE) else 0
      score <- round((d + w + p) / 3, 3)
      if (score < LAYOUT_MATCH_MIN) next
      id <- as.character(lb$id %||% ly$id %||% "")[1]
      cand <- list(ref = sprintf("%s@%d", id, as.integer(lb$version %||% ly$version %||% 1L)), id = id,
                   version = as.integer(lb$version %||% ly$version %||% 1L), score = score,
                   date = d, words = round(w, 3), place = round(p, 3),
                   .proven = identical(lb$status, "proven"), .n = length(unlist(lb$proved_by)),
                   .ord = .layout_ordinal(id))
      if (is.null(best) || .layout_better(cand, best)) best <- cand
    }
    if (is.null(best)) return(NULL)
    best[c("ref", "id", "version", "score", "date", "words", "place")]
  }, error = function(e) NULL)
}

# .layout_role_key(roles, money_style) -- the roles as compared: a text column
# beyond the description is "text" whatever it was called. The contract names
# them text1, text2, ...; a reading that named one from its heading
# ("particulars", "type") or an OCR copy that did not must still meet the same
# layout. "type" is a role of its own only where it carries the sign (type_dc).
.LAYOUT_CORE_ROLES <- c("date", "date2", "description", "debit", "credit", "amount",
                        "balance", "other")
.layout_role_key <- function(roles, money_style = "") {
  core <- c(.LAYOUT_CORE_ROLES, if (identical(money_style, "type_dc")) "type")
  roles[!(roles %in% core)] <- "text"
  roles
}

.layout_better <- function(a, b) {
  if (a$score != b$score) return(a$score > b$score)
  if (a$.proven != b$.proven) return(a$.proven)
  if (a$.n != b$.n) return(a$.n > b$.n)
  oa <- if (is.na(a$.ord)) .Machine$integer.max else a$.ord
  ob <- if (is.na(b$.ord)) .Machine$integer.max else b$.ord
  if (oa != ob) return(oa < ob)
  if (a$id != b$id) return(.layout_sort(c(a$id, b$id))[1] == a$id)
  a$version > b$version
}

# ---- learning ---------------------------------------------------------------------------

# layout_learn(reading, bank, file_sha, dir) -> list(action, ref, id, version,
# status, why). The learning rules (spec section 6):
#   * only a PROVEN reading teaches anything -- a reading that needed a person,
#     or matched a layout without arithmetic, proves nothing about the layout;
#   * a bank pick that is blocked from learning (the statement names another
#     bank and nobody has confirmed which) teaches nothing;
#   * no matching layout -> a new provisional layout, version 1 ("created");
#   * a matching provisional layout gains this statement as evidence
#     ("evidence_added"), and becomes proven at LAYOUT_PROVEN_AFTER distinct
#     statements ("promoted"); heading words it had not seen are added, since the
#     arithmetic has just proved which columns they sit over;
#   * a matching proven layout needs nothing more, and the same statement read
#     twice counts once, towards one layout -- both "none", with the layout named;
#   * a signature that could never match its own design again (see
#     .layout_sig_problem) teaches nothing.
# `bank` is a bank name/id or bank_pick()'s result; `file_sha` is the statement's
# sha256.
layout_learn <- function(reading, bank, file_sha, dir = layouts_dir()) {
  out <- function(action, why, ly = NULL) {
    lb <- ly$layout
    list(action = action,
         ref = if (is.null(lb)) NA_character_ else sprintf("%s@%d", lb$id, as.integer(lb$version)),
         id = lb$id %||% NA_character_, version = if (is.null(lb)) NA_integer_ else as.integer(lb$version),
         status = lb$status %||% NA_character_, why = why)
  }
  tryCatch({
    b <- .layout_bank(bank)
    if (b$blocked) return(out("none", "The statement names a different bank from the one picked, so nothing is learned until a person confirms the bank."))
    if (is.na(b$slug)) return(out("none", "No bank was given, so nothing is learned."))
    oc <- as.character(reading$outcome %||% "none")[1]
    if (!identical(oc, "proven"))
      return(out("none", sprintf("Only a reading the arithmetic proved teaches a layout; this one is \"%s\".", oc)))
    # The reader never calls a reading proven with a failed check or a figure
    # filled from the balance; if one ever arrived so, it still teaches nothing.
    ck <- reading$checks
    if ((is.data.frame(ck) && any(ck$ok %in% FALSE)) || isTRUE(as.numeric(reading$proof$derived %||% 0)[1] > 0))
      return(out("none", "The reading has a failed check or a derived amount, so it teaches nothing."))
    sha <- tolower(trimws(as.character(file_sha %||% "")[1]))
    if (is.na(sha) || !grepl("^[0-9a-f]{64}$", sha))
      return(out("none", "The statement has no fingerprint (sha256), so it cannot be counted as evidence."))
    tpl <- reading$template
    sig <- .layout_sig_norm(tpl$signature %||% reading$signature)
    if (!is.list(tpl) || is.null(sig))
      return(out("none", "The reading carries no layout signature, so there is nothing to learn."))
    bad <- .layout_sig_problem(sig)
    if (!is.null(bad))
      return(out("none", sprintf("The reading's layout signature has %s, so nothing is learned from it.", bad)))

    release <- .layout_lock(file.path(dir, b$slug))
    if (is.null(release)) return(out("none", "The bank's layouts are being changed by someone else, so nothing was learned this time."))
    on.exit(release(), add = TRUE)

    lys <- layouts_load(dir, b$slug)
    # One statement is evidence for one design. Read again after the reader has
    # changed, it may no longer match the layout it proved -- it must not then
    # start (or prove) a second one as well.
    had <- Filter(function(l) sha %in% l$layout$proved_by, lys)
    m <- layout_match(sig, lys)
    if (length(had) && (is.null(m) || !(m$id %in% names(had)))) {
      h <- had[[1]]$layout
      return(out("none", sprintf("This statement already counts towards layout %s@%d.", h$id, h$version), had[[1]]))
    }
    if (is.null(m)) {
      fs <- .layout_files(dir, b$slug)
      n <- max(c(0L, .layout_ordinal(fs$id)), na.rm = TRUE) + 1L
      id <- sprintf("%s_%d", b$slug, n)
      ly <- .layout_template(tpl, id, b$bank, 1L)
      ly$layout <- .layout_block(id, b$bank, "provisional", 1L, sha, "auto", sig)
      ok <- .layout_write(ly, dir)
      if (!isTRUE(ok)) return(out("none", paste("A new layout could not be saved:", attr(ok, "reason") %||% "unknown reason.")))
      return(out("created", sprintf("A new layout %s was started from this statement; it is provisional until %d statements prove it or an admin confirms it.",
                                    id, LAYOUT_PROVEN_AFTER), ly))
    }
    old <- lys[[m$id]]
    lb <- old$layout
    if (identical(lb$status, "proven"))
      return(out("none", sprintf("The statement matches the proven layout %s; nothing new to learn.", m$ref), old))
    if (sha %in% lb$proved_by)
      return(out("none", sprintf("This statement already counts towards layout %s.", m$ref), old))
    pb <- c(lb$proved_by, sha)
    sig2 <- lb$signature
    sig2$heading_tokens <- c(sig2$heading_tokens, sig$heading_tokens)
    promote <- length(unique(pb)) >= LAYOUT_PROVEN_AFTER
    v <- lb$version + 1L
    ly <- old
    ly$version <- v
    ly$layout <- .layout_block(lb$id, lb$bank, if (promote) "proven" else lb$status, v, pb,
                               lb$origin %||% "auto", sig2, name = lb$name,
                               confirmed_by = lb$confirmed_by, corrected_by = lb$corrected_by)
    ok <- .layout_write(ly, dir)
    if (!isTRUE(ok)) return(out("none", paste("The evidence could not be saved:", attr(ok, "reason") %||% "unknown reason."), old))
    if (promote)
      return(out("promoted", sprintf("Layout %s is now proven: %d different statements have proved it.", lb$id, length(unique(pb))), ly))
    out("evidence_added", sprintf("This statement is evidence %d of %d for layout %s.", length(unique(pb)), LAYOUT_PROVEN_AFTER, lb$id), ly)
  }, error = function(e) out("none", paste0("Nothing was learned (", conditionMessage(e), ").")))
}

# ---- admin changes ----------------------------------------------------------------------
# Each returns list(ok, changed, ref, why); `ref` is the version now in force.

# layout_confirm(id, dir, by) -- an admin vouches for the layout: proven, origin
# confirmed. Confirming a retired layout brings it back.
layout_confirm <- function(id, dir = layouts_dir(), by = NULL) {
  .layout_change(id, dir, function(old) {
    lb <- old$layout
    if (identical(lb$status, "proven") && identical(lb$origin, "confirmed"))
      return(sprintf("Layout %s is already confirmed.", lb$id))
    old$layout <- .layout_block(lb$id, lb$bank, "proven", lb$version, lb$proved_by, "confirmed",
                                lb$signature, name = lb$name, confirmed_by = .layout_person(by),
                                corrected_by = lb$corrected_by)
    old
  })
}

# layout_correct(id, template, dir, by, bank) -- a person's corrected reading
# becomes the layout's next version: proven, origin corrected. The evidence it
# had is kept. With id = NULL a new layout is made for `bank` (an admin confirming
# a fix that the arithmetic could not prove: until then it applied to one file).
# The signature comes from the template (its own, or its layout block's), else the
# previous version's.
layout_correct <- function(id, template, dir = layouts_dir(), by = NULL, bank = NULL) {
  if (!is.list(template) || !(is.list(template$table) || is.list(template$columns)))
    return(list(ok = FALSE, changed = FALSE, ref = NA_character_, why = "The corrected reading has no columns to keep."))
  sig_new <- .layout_sig_norm(template$signature %||% template$layout$signature)
  if (is.null(id) || (length(id) == 1L && is.na(id))) {
    return(tryCatch({
      b <- .layout_bank(bank %||% template$bank)
      if (is.na(b$slug)) return(list(ok = FALSE, changed = FALSE, ref = NA_character_, why = "No bank was given for the new layout."))
      if (!is.null(.layout_sig_problem(sig_new)))
        return(list(ok = FALSE, changed = FALSE, ref = NA_character_, why = "The corrected reading carries no usable layout signature."))
      release <- .layout_lock(file.path(dir, b$slug))
      if (is.null(release)) return(list(ok = FALSE, changed = FALSE, ref = NA_character_, why = "The bank's layouts are being changed by someone else; try again."))
      on.exit(release(), add = TRUE)
      fs <- .layout_files(dir, b$slug)
      nid <- sprintf("%s_%d", b$slug, max(c(0L, .layout_ordinal(fs$id)), na.rm = TRUE) + 1L)
      ly <- .layout_template(template, nid, b$bank, 1L)
      ly$layout <- .layout_block(nid, b$bank, "proven", 1L, character(0), "corrected", sig_new,
                                 corrected_by = .layout_person(by))
      ok <- .layout_write(ly, dir)
      if (!isTRUE(ok)) return(list(ok = FALSE, changed = FALSE, ref = NA_character_, why = attr(ok, "reason") %||% "The layout could not be saved."))
      list(ok = TRUE, changed = TRUE, ref = sprintf("%s@1", nid), why = "saved")
    }, error = function(e) list(ok = FALSE, changed = FALSE, ref = NA_character_,
                                why = paste0("The layout could not be saved (", conditionMessage(e), ")."))))
  }
  .layout_change(id, dir, function(old) {
    lb <- old$layout
    ly <- .layout_template(template, lb$id, lb$bank, lb$version)
    ly$layout <- .layout_block(lb$id, lb$bank, "proven", lb$version, lb$proved_by, "corrected",
                               if (is.null(.layout_sig_problem(sig_new))) sig_new else lb$signature,
                               name = lb$name, confirmed_by = lb$confirmed_by,
                               corrected_by = .layout_person(by))
    ly
  })
}

# layout_retire(id, dir, by) -- the admin's "forget": a new version with status
# retired. Every earlier file stays, so outputs already issued can still be traced
# and the retirement itself can be undone by a confirm.
layout_retire <- function(id, dir = layouts_dir(), by = NULL) {
  .layout_change(id, dir, function(old) {
    lb <- old$layout
    if (identical(lb$status, "retired")) return(sprintf("Layout %s is already retired.", lb$id))
    old$layout <- .layout_block(lb$id, lb$bank, "retired", lb$version, lb$proved_by, lb$origin,
                                lb$signature, name = lb$name, confirmed_by = lb$confirmed_by,
                                corrected_by = lb$corrected_by)
    old
  })
}

# layout_rename(id, name, dir) -- the name people see in place of the column list.
layout_rename <- function(id, name, dir = layouts_dir()) {
  nm <- .layout_person(name)
  if (is.null(nm)) return(list(ok = FALSE, changed = FALSE, ref = NA_character_, why = "A layout needs a name with some words in it."))
  .layout_change(id, dir, function(old) {
    lb <- old$layout
    if (identical(lb$name, nm)) return(sprintf("Layout %s already has that name.", lb$id))
    old$layout <- .layout_block(lb$id, lb$bank, lb$status, lb$version, lb$proved_by, lb$origin,
                                lb$signature, name = nm, confirmed_by = lb$confirmed_by,
                                corrected_by = lb$corrected_by)
    old
  })
}

# ---- showing one ------------------------------------------------------------------------

.LAYOUT_ROLE_LABELS <- c(date = "Date", date2 = "Second date", description = "Description",
                         debit = "Money out", credit = "Money in", amount = "Amount",
                         balance = "Balance", type = "Type", other = "Other figure")

# layout_display_name(layout, bank_display) -> "ANZ layout 3: Date | Description |
# Money out | Money in | Balance", or "ANZ layout 3: Everyday account" once an
# admin has named it. Built from the column roles, never from heading text, which
# can carry an account number or a name printed over the table.
layout_display_name <- function(layout, bank_display = NULL) {
  tryCatch({
    lb <- layout$layout %||% list()
    id <- as.character(lb$id %||% layout$id %||% "layout")[1]
    bank <- bank_display %||% .layout_bank_display(.layout_slug(lb$bank %||% layout$bank), lb$bank %||% layout$bank %||% "")
    n <- .layout_ordinal(id)
    head <- trimws(paste(bank, "layout", if (is.na(n)) id else n))
    if (!is.null(lb$name) && nzchar(lb$name)) return(paste0(head, ": ", lb$name))
    roles <- .layout_sig_norm(lb$signature %||% layout$signature)$roles
    if (!length(roles)) return(head)
    lab <- ifelse(roles %in% names(.LAYOUT_ROLE_LABELS), .LAYOUT_ROLE_LABELS[roles],
           ifelse(grepl("^text[0-9]+$", roles), paste("Text", sub("^text", "", roles)),
                  paste0(toupper(substr(roles, 1, 1)), gsub("_", " ", substring(roles, 2)))))
    paste0(head, ": ", paste(lab, collapse = " | "))
  }, error = function(e) "layout")
}
