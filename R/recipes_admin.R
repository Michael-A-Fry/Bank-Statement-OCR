# recipes_admin.R -- the engine behind the Admin page's recipes (D15): what each
# recipe is doing, and every change an admin makes to one, in plain fields.
#
# THE RULES, the same as R/recipes.R's:
#   * A recipe file is NEVER edited or deleted. Every change -- on, off, an edited
#     answer, an undo, an accept, a merge -- is a NEW version file in the server's
#     own recipes folder (recipes_state_dir), one version higher than any there or
#     shipped, so the loader (recipes_load: highest version wins) uses it. A shipped
#     recipe changed here gets its override version in the server folder; an update
#     of the product never touches it.
#   * Off is a version whose status is retired; on again is a new version with the
#     status it had before. Retire = off and hidden from the list.
#   * A change is checked by .rc_validate before it is written, and refused with a
#     plain sentence when it would not be a recipe. Nobody sees YAML.
#   * The proof still gates every reading: nothing here makes a reading automatic.
#     recipe_test reads a statement with a recipe (saved or not) so the admin can
#     see whether it adds up before saving.
#   * No personal data: counts come from tracking (codes and counts only) and the
#     uploads' status records, never a statement's contents.

# .rca_dirs(dirs) -> list(shipped, server). `dirs` may be given (tests, the app);
# by default the install's recipes/ and the server's state folder.
.rca_dirs <- function(dirs = NULL) {
  if (!is.null(dirs$shipped) || !is.null(dirs$server)) return(list(shipped = dirs$shipped, server = dirs$server))
  root <- Sys.getenv("ENGINE_ROOT", "")
  cfg <- safe(load_config(), list())
  list(shipped = if (nzchar(root)) file.path(root, "recipes") else "recipes",
       server = safe(recipes_state_dir(safe(layouts_dir(cfg), NULL), cfg), NULL))
}

# .rca_versions(dirs) -> every valid version of every recipe, retired ones too:
# list(id, version, status, file, origin, y (the YAML as read), rc (validated)).
.rca_versions <- function(dirs) {
  out <- list()
  for (origin in c("shipped", "server")) {
    d <- as.character(dirs[[origin]] %||% NA_character_)[1]
    if (is.na(d) || !dir.exists(d)) next
    for (f in sort(list.files(d, "[.]ya?ml$", full.names = TRUE))) {
      y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL, warning = function(w) NULL)
      if (is.null(y)) next
      rc <- .rc_validate(y)
      if (!is.null(rc$error)) next
      rc$file <- f
      out[[length(out) + 1L]] <- list(id = rc$id, version = rc$version, status = rc$status, file = f,
                                      origin = origin, y = y, rc = rc)
    }
  }
  out
}

# .rca_of(vs, id) -> that recipe's versions, oldest first.
.rca_of <- function(vs, id) {
  v <- Filter(function(x) identical(x$id, id), vs)
  v[order(vapply(v, `[[`, 0L, "version"))]
}

.rca_ok <- function(why, ref = NA_character_, ...) c(list(ok = TRUE, ref = ref, why = why), list(...))
.rca_no <- function(why) list(ok = FALSE, ref = NA_character_, why = why)

# .rca_write(dirs, y) -- y as the next version of its recipe, in the server folder.
# Never replaces a file. Returns .rca_ok / .rca_no.
.rca_write <- function(dirs, y, why) {
  d <- dirs$server
  if (is.null(d) || is.na(d) || !nzchar(d)) return(.rca_no("No folder is set up for this server's recipes, so nothing was changed."))
  release <- .layout_lock(d)
  if (is.null(release)) return(.rca_no("Someone else is changing the recipes right now. Try again in a moment."))
  on.exit(release(), add = TRUE)
  top <- max(c(0L, vapply(.rca_of(.rca_versions(dirs), y$recipe), `[[`, 0L, "version")))
  y$version <- top + 1L
  rc <- .rc_validate(y)
  if (!is.null(rc$error)) return(.rca_no(paste("That change was not saved:", rc$error)))
  f <- file.path(d, sprintf("%s@v%d.yaml", y$recipe, y$version))
  if (file.exists(f)) return(.rca_no("A newer version was saved at the same moment; nothing was changed. Try again."))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(y, f)
  .rca_ok(why, rc$ref, file = f)
}

# .rca_top(dirs, id) -> list(top, live) or list(error): the newest version, and the
# newest one that is not retired (what "on again" goes back to).
.rca_top <- function(dirs, id) {
  vs <- .rca_of(.rca_versions(dirs), as.character(id %||% "")[1])
  if (!length(vs)) return(list(error = sprintf("There is no recipe called \"%s\".", as.character(id %||% "")[1])))
  live <- Filter(function(v) !identical(v$status, "retired"), vs)
  list(top = vs[[length(vs)]], live = if (length(live)) live[[length(live)]] else NULL, all = vs)
}

.rca_proofs <- function(dirs, id) {
  f <- file.path(dirs$server %||% "", ".evidence", paste0(id, ".yaml"))
  ev <- if (nzchar(dirs$server %||% "") && file.exists(f)) safe(yaml::read_yaml(f), list()) else list()
  list(proved_by = as.character(unlist(ev$proved_by)), groups = as.character(unlist(ev$groups)), salt = ev$salt)
}

# ---- the overview -----------------------------------------------------------------

# recipes_overview(dirs, tracking, days, hidden) -> one row per recipe: id, bank,
# title, status (draft / proven / retired), version, enabled, hidden (retired
# from the list), statements read and how many needed help in the last `days`
# days, proofs (a draft's checked statements), origin (shipped / server /
# shipped, changed here).
recipes_overview <- function(dirs = NULL, tracking = NULL, days = 30, hidden = TRUE) {
  dirs <- .rca_dirs(dirs)
  vs <- .rca_versions(dirs)
  ids <- sort(unique(vapply(vs, `[[`, "", "id")))
  recs <- .rca_tracked(tracking, days)
  rows <- lapply(ids, function(id) {
    v <- .rca_of(vs, id); top <- v[[length(v)]]
    mine <- Filter(function(r) identical(r$recipe_id, id), recs)
    o <- unique(vapply(v, `[[`, "", "origin"))
    data.frame(id = id, bank = .layout_bank_display(top$rc$bank), title = top$rc$title,
               status = top$status, version = top$version, enabled = !identical(top$status, "retired"),
               hidden = isTRUE(top$y$admin$hidden),
               read = length(mine),
               needed_help = sum(!vapply(mine, function(r) (r$outcome %||% "") %in% c("proven", "layout_match"), NA)),
               tried_not_proven = sum(vapply(mine, function(r) identical(r$recipe_used, "tried"), NA)),
               proofs = length(.rca_proofs(dirs, id)$proved_by),
               origin = if (identical(o, "shipped")) "shipped" else if (identical(o, "server")) "server" else "shipped, changed here",
               stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(id = character(0), bank = character(0), title = character(0), status = character(0),
               version = integer(0), enabled = logical(0), hidden = logical(0), read = integer(0),
               needed_help = integer(0), tried_not_proven = integer(0), proofs = integer(0),
               origin = character(0), stringsAsFactors = FALSE)
  if (!isTRUE(hidden)) out <- out[!out$hidden, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# .rca_tracked(path, days) -- the "convert" tracking records of the last `days`
# days that name a recipe.
.rca_tracked <- function(path, days = 30) {
  if (is.null(path)) path <- safe(tracking_dir(), NULL)
  if (is.null(path) || is.na(path)) return(list())
  since <- .track_since(Sys.Date() - as.integer(days))
  rd <- safe(.track_read(path, since), list(records = list()))
  Filter(function(r) identical(r$event, "convert") && !is.null(r$recipe_id), rd$records)
}

# ---- on / off, accept, retire, undo ------------------------------------------------

# recipe_set_enabled(id, on, dirs) -- Off: a new version, retired. On: a new
# version with the status the recipe had before it was switched off.
recipe_set_enabled <- function(id, on, dirs = NULL) {
  dirs <- .rca_dirs(dirs)
  t <- .rca_top(dirs, id); if (!is.null(t$error)) return(.rca_no(t$error))
  is_on <- !identical(t$top$status, "retired")
  if (isTRUE(on) == is_on) return(.rca_ok(sprintf("Recipe %s is already %s.", id, if (is_on) "on" else "off"), paste0(id, "@", t$top$version)))
  if (isTRUE(on)) {
    if (is.null(t$live)) return(.rca_no(sprintf("Recipe %s has never been on, so there is nothing to switch back on.", id)))
    y <- t$live$y; y$admin <- NULL
    return(.rca_write(dirs, y, sprintf("Recipe %s is on again: statements like it are tried with it.", id)))
  }
  y <- t$top$y; y$status <- "retired"; y$admin <- list(hidden = FALSE)
  .rca_write(dirs, y, sprintf("Recipe %s is off: statements like it get the questions instead.", id))
}

# recipe_retire(id, dirs) -- off, and hidden from the list (never deleted).
recipe_retire <- function(id, dirs = NULL) {
  dirs <- .rca_dirs(dirs)
  t <- .rca_top(dirs, id); if (!is.null(t$error)) return(.rca_no(t$error))
  y <- t$top$y; y$status <- "retired"; y$admin <- list(hidden = TRUE)
  .rca_write(dirs, y, sprintf("Recipe %s is retired: it is off and no longer listed. Its versions are kept.", id))
}

# recipe_accept(id, dirs) -- an admin accepts a draft: proven from now on.
recipe_accept <- function(id, dirs = NULL) {
  dirs <- .rca_dirs(dirs)
  t <- .rca_top(dirs, id); if (!is.null(t$error)) return(.rca_no(t$error))
  if (!identical(t$top$status, "draft"))
    return(.rca_no(sprintf("Recipe %s is not a draft waiting to be accepted (it is %s).", id, t$top$status)))
  y <- t$top$y; y$status <- "proven"; y$admin <- list(accepted = TRUE)
  .rca_write(dirs, y, sprintf("Recipe %s is accepted: statements like it are read on their own, and each reading must still add up.", id))
}

# recipe_undo(id, dirs) -- go back to the version before the newest: written as a
# new version holding the earlier one, so nothing is ever edited or lost.
recipe_undo <- function(id, dirs = NULL) {
  dirs <- .rca_dirs(dirs)
  t <- .rca_top(dirs, id); if (!is.null(t$error)) return(.rca_no(t$error))
  n <- length(t$all)
  if (n < 2L) return(.rca_no(sprintf("Recipe %s has no earlier version to go back to.", id)))
  prev <- t$all[[n - 1L]]
  .rca_write(dirs, prev$y, sprintf("Recipe %s is back to how it was in version %d.", id, prev$version))
}

# ---- changing a recipe in plain fields ---------------------------------------------

# The plain words a column's role may be given in.
.RCA_ROLES <- c("money out" = "debit", "withdrawals" = "debit", "debit" = "debit",
                "money in" = "credit", "deposits" = "credit", "credit" = "credit",
                "amount" = "amount", "balance" = "balance", "date" = "date",
                "description" = "description", "details" = "description",
                "particulars" = "particulars", "code" = "code", "reference" = "reference",
                "other party" = "other_party", "other_party" = "other_party", "type" = "type",
                "second date" = "date2", "date2" = "date2", "other" = "other", "ignore" = "other")
.RCA_CHANGES <- c("title", "header", "columns", "date_format", "money_style", "recognise_add",
                  "recognise_remove", "not_recognise_add", "not_recognise_remove",
                  "skip_add", "skip_remove", "ends_add", "ends_remove")

# .rca_apply(y, changes) -> the recipe's YAML with the changes made, or
# list(error). Changes are plain:
#   title         "ANZ everyday"
#   header        the heading words, left to right
#   columns       each column's role, left to right ("date", "description",
#                 "money out", "money in", "amount", "balance", "other", ...)
#   date_format   a format ("%d %b") or an example as printed ("03 Feb")
#   money_style   "money out and money in" (two columns) or "one amount"
#   recognise_add / recognise_remove          words that recognise the design
#   not_recognise_add / not_recognise_remove  words that mean it is NOT this design
#   skip_add / skip_remove                    lines left out of the table
#   ends_add / ends_remove                    lines that end the table on a page
.rca_apply <- function(y, changes) {
  no <- function(...) list(error = sprintf(...))
  ch <- changes %||% list()
  if (!is.list(ch) || (length(ch) && is.null(names(ch)))) return(no("The changes must be named, like title or columns."))
  bad <- setdiff(names(ch), .RCA_CHANGES)
  if (length(bad)) return(no("\"%s\" is not something a recipe can be changed by.", bad[1]))
  words <- function(x) { x <- trimws(as.character(unlist(x))); x[!is.na(x) & nzchar(x)] }
  edit <- function(cur, add, rm) {
    cur <- words(cur)
    cur <- cur[!(tolower(cur) %in% tolower(words(rm)))]
    a <- words(add); a <- a[!(tolower(a) %in% tolower(cur))]
    as.list(c(cur, a))
  }
  if (!is.null(ch$title)) y$title <- words(ch$title)[1] %||% y$title
  y$recognise <- y$recognise %||% list()
  if (!is.null(ch$recognise_add) || !is.null(ch$recognise_remove))
    y$recognise$all <- edit(y$recognise$all, ch$recognise_add, ch$recognise_remove)
  if (!is.null(ch$not_recognise_add) || !is.null(ch$not_recognise_remove))
    y$recognise$none <- edit(y$recognise$none, ch$not_recognise_add, ch$not_recognise_remove)
  if (!length(y$recognise$none)) y$recognise$none <- NULL
  tb <- y$table %||% list()
  if (!is.null(ch$skip_add) || !is.null(ch$skip_remove)) tb$skip <- edit(tb$skip, ch$skip_add, ch$skip_remove)
  if (!is.null(ch$ends_add) || !is.null(ch$ends_remove)) tb$ends_at <- edit(tb$ends_at, ch$ends_add, ch$ends_remove)
  if (!length(tb$skip)) tb$skip <- NULL
  if (!length(tb$ends_at)) tb$ends_at <- NULL
  if (!is.null(ch$header) || !is.null(ch$columns)) {
    old <- tb$columns %||% list()
    hd <- if (!is.null(ch$header)) words(ch$header) else words(tb$header)
    roles <- if (!is.null(ch$columns)) {
      r <- tolower(words(ch$columns))
      m <- unname(.RCA_ROLES[r])
      if (anyNA(m)) return(no("\"%s\" is not a column the reader knows (try date, description, money out, money in, amount, balance or other).", words(ch$columns)[is.na(m)][1]))
      n_other <- 0L
      for (j in seq_along(m)) if (m[j] == "other") { n_other <- n_other + 1L; m[j] <- sprintf("text%d", n_other) }
      m
    } else names(old)
    anchored <- length(old) && all(vapply(old, function(c) !is.null(c$under), NA))
    if (anchored || !is.null(ch$header)) {
      if (length(hd) != length(roles))
        return(no("There are %d heading words but %d columns: give one heading for each column.", length(hd), length(roles)))
      tb$header <- as.list(hd)
      tb$columns <- stats::setNames(lapply(hd, function(h) list(under = h)), roles)
      tb$ref_width <- NULL
    } else {
      if (length(old) != length(roles))
        return(no("This recipe has %d columns, but %d roles were given.", length(old), length(roles)))
      tb$columns <- stats::setNames(old, roles)
    }
  }
  y$table <- tb
  if (!is.null(ch$date_format)) {
    f <- words(ch$date_format)[1] %||% ""
    known <- vapply(.ar_date_formats(), `[[`, "", "fmt")
    if (!(f %in% known) && is.null(.rc_date_entry(f))) {
      hit <- strsplit(.ar_date_fmts(f, .ar_date_formats()), "|", fixed = TRUE)[[1]]
      if (!length(hit) || !nzchar(hit[1])) return(no("\"%s\" is not a date the reader knows how to read.", f))
      f <- hit[1]
    }
    y$dates <- list(format = f, year = if (grepl("%[Yy]", f)) "printed" else "period")
  }
  if (!is.null(ch$money_style)) {
    s <- tolower(words(ch$money_style)[1] %||% "")
    st <- if (s %in% c("debit_credit_cols", "money out and money in", "two columns", "money out and in")) "debit_credit_cols"
          else if (s %in% c("signed", "one amount", "one column", "amount")) "signed" else NA
    if (is.na(st)) return(no("\"%s\" is not a money style (say \"money out and money in\" or \"one amount\").", words(ch$money_style)[1]))
    y$money <- y$money %||% list(); y$money$style <- st
  }
  y
}

# recipe_update(id, changes, dirs, check) -- the changes as a new version. Refused
# with a plain sentence when the result is not a valid recipe, or when `check`
# (statements the recipe read before, as read_input() inputs) no longer all add up
# to the same figures read with the change.
recipe_update <- function(id, changes, dirs = NULL, check = list()) {
  dirs <- .rca_dirs(dirs)
  t <- .rca_top(dirs, id); if (!is.null(t$error)) return(.rca_no(t$error))
  base <- t$top
  y <- .rca_apply(base$y, changes)
  if (!is.null(y$error)) return(.rca_no(paste("That change was not saved:", y$error)))
  y$admin <- NULL
  if (identical(base$status, "retired")) y$admin <- base$y$admin
  rc <- .rc_validate(y)
  if (!is.null(rc$error)) return(.rca_no(paste("That change was not saved:", rc$error)))
  if (length(check)) {
    before <- base$rc
    if (identical(before$status, "retired") && !is.null(t$live)) before <- t$live$rc
    n_bad <- 0L; n_changed <- 0L
    for (inp in check) {
      a <- recipe_read(inp, before); b <- recipe_read(inp, rc)
      if (!identical(b$outcome, "proven")) n_bad <- n_bad + 1L
      else if (identical(a$outcome, "proven") && !identical(.figures(a), .figures(b))) n_changed <- n_changed + 1L
    }
    if (n_bad || n_changed)
      return(.rca_no(sprintf("That change was not saved: of %d statements this recipe read, %d would no longer add up and %d would change figures.",
                             length(check), n_bad, n_changed)))
  }
  .rca_write(dirs, y, sprintf("Recipe %s is changed; the earlier version is kept and can be brought back.", id))
}

# ---- testing before saving ---------------------------------------------------------

# recipe_test(recipe, input, dirs, changes) -> list(outcome ("proven" / "check"),
# rows, why, reading). `recipe` is an id (with `changes` applied, unsaved), a
# recipe as read from YAML, or a validated recipe. Nothing is written.
recipe_test <- function(recipe, input, dirs = NULL, changes = NULL) {
  ans <- function(outcome, why, rd = NULL)
    list(outcome = outcome, rows = if (is.data.frame(rd$transactions)) nrow(rd$transactions) else 0L, why = why, reading = rd)
  rc <- recipe
  if (is.character(recipe)) {
    t <- .rca_top(.rca_dirs(dirs), recipe); if (!is.null(t$error)) return(ans("check", t$error))
    rc <- (t$live %||% t$top)$y
  }
  if (is.list(rc) && is.null(rc$cols)) {
    if (!is.null(changes)) { rc <- .rca_apply(rc, changes); if (!is.null(rc$error)) return(ans("check", paste("That change cannot be tested:", rc$error))) }
    rc <- .rc_validate(rc)
    if (!is.null(rc$error)) return(ans("check", paste("That is not a recipe yet:", rc$error)))
  }
  rd <- recipe_read(input, rc)
  n <- if (is.data.frame(rd$transactions)) nrow(rd$transactions) else 0L
  if (identical(rd$outcome, "proven"))
    ans("proven", sprintf("It adds up: %d transaction%s, and the statement's own figures agree.", n, if (n == 1L) "" else "s"), rd)
  else ans("check", sprintf("It does not add up with this recipe: %s", rd$why %||% "no reason was given."), rd)
}

# ---- merging -----------------------------------------------------------------------

# .rca_table_key(rc) -- what must be the same for two recipes to be one design.
.rca_table_key <- function(rc)
  paste(c(rc$bank, tolower(rc$header), paste(rc$cols$field, tolower(rc$cols$under), rc$cols$x_min, rc$cols$x_max),
          rc$date_format, rc$year, rc$style, rc$dir), collapse = "|")

# recipe_merge(a, b, dirs, check) -- two recipes of one design become one: the one
# with more checked statements (then proven over draft, then the newer) is kept,
# with both recipes' recognise words where they do not contradict each other's
# must-not-appear words, and the other's checked statements; the other is switched
# off, pointing to it. Refused when the two read their tables differently, or when
# the kept recipe would no longer read every statement in `check`.
recipe_merge <- function(a, b, dirs = NULL, check = list()) {
  dirs <- .rca_dirs(dirs)
  if (identical(a, b)) return(.rca_no("A recipe cannot be merged with itself."))
  ta <- .rca_top(dirs, a); if (!is.null(ta$error)) return(.rca_no(ta$error))
  tb <- .rca_top(dirs, b); if (!is.null(tb$error)) return(.rca_no(tb$error))
  if (is.null(ta$live) || is.null(tb$live) || identical(ta$top$status, "retired") || identical(tb$top$status, "retired"))
    return(.rca_no("Only two recipes that are on can be merged."))
  if (!identical(.rca_table_key(ta$top$rc), .rca_table_key(tb$top$rc)))
    return(.rca_no(sprintf("Recipes %s and %s read their tables differently, so they are not one design and were not merged.", a, b)))
  pa <- .rca_proofs(dirs, a); pb <- .rca_proofs(dirs, b)
  score <- function(t, p) c(length(p$proved_by), identical(t$top$status, "proven"), t$top$version,
                            as.numeric(file.mtime(t$top$file)))
  sa <- score(ta, pa); sb <- score(tb, pb)
  keep_a <- TRUE
  for (j in seq_along(sa)) if (sa[j] != sb[j]) { keep_a <- sa[j] > sb[j]; break }
  K <- if (keep_a) ta else tb; O <- if (keep_a) tb else ta
  kid <- K$top$id; oid <- O$top$id
  y <- K$top$y
  none <- unique(c(K$top$rc$none, O$top$rc$none))
  clash <- function(w) any(vapply(none, function(n) .rc_has_phrase(.rc_flat(w), n) || .rc_has_phrase(.rc_flat(n), w), NA))
  add <- Filter(Negate(clash), O$top$rc$all)
  all <- unique(c(K$top$rc$all, add))
  all <- all[!duplicated(tolower(all))]
  y$recognise$all <- as.list(all)
  y$admin <- list(merged_from = oid)
  rc <- .rc_validate(y)
  if (!is.null(rc$error)) return(.rca_no(paste("The merge was not saved:", rc$error)))
  for (inp in check) {
    rg <- recipe_recognise(inp, list(rc), drafts = TRUE)
    if (is.null(rg$recipe) || !identical(recipe_read(inp, rc)$outcome, "proven"))
      return(.rca_no(sprintf("The merge was not saved: merged, recipe %s would no longer read every statement the two recipes read.", kid)))
  }
  w1 <- .rca_write(dirs, y, "")
  if (!isTRUE(w1$ok)) return(w1)
  yo <- O$top$y; yo$status <- "retired"; yo$admin <- list(hidden = FALSE, merged_into = kid)
  w2 <- .rca_write(dirs, yo, "")
  if (!isTRUE(w2$ok)) return(w2)
  # The other recipe's checked statements count for the one kept. (Accounts are
  # salted per recipe, so they cannot be pooled: only the statements are.)
  if (length(pb$proved_by) || length(pa$proved_by)) {
    from <- if (keep_a) pb else pa; into <- if (keep_a) pa else pb
    f <- file.path(dirs$server, ".evidence", paste0(kid, ".yaml"))
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(list(salt = into$salt %||% .layout_new_salt(kid, "merge"), groups = as.list(into$groups),
                          proved_by = as.list(unique(c(into$proved_by, from$proved_by)))), f)
  }
  .rca_ok(sprintf("Recipes %s and %s are one now: %s is kept with both recipes' words, and %s is off.", kid, oid, kid, oid),
          w1$ref, kept = kid, retired = oid)
}

# ---- a new recipe from a statement -------------------------------------------------

# recipe_from_statement(input, answers, dirs) -- a new DRAFT from one statement and
# the plain Please check answers: answers$bank (required) and answers$roles (the
# columns' roles, as Please check sends them; none = the automatic reading as it
# stands). Only a reading that adds up is saved, through recipe_learn.
recipe_from_statement <- function(input, answers = list(), dirs = NULL) {
  dirs <- .rca_dirs(dirs)
  bank <- answers$bank
  if (is.na(.layout_slug(bank %||% NA))) return(.rca_no("Say which bank the statement is from first."))
  if (is.null(dirs$server)) return(.rca_no("No folder is set up for this server's recipes, so nothing was saved."))
  rd <- auto_read(input, list(), bank, list(recipes = FALSE))
  if (length(answers$roles)) {
    o <- .override_roles(rd, answers$roles)
    if (!is.null(o$error)) return(.rca_no(o$error))
    rd <- auto_read(input, list(), bank, list(roles = o$roles))
  }
  if (!identical(rd$outcome, "proven"))
    return(.rca_no(sprintf("Read like that the statement does not add up, so no recipe was saved: %s", rd$why %||% "")))
  sha <- tolower(as.character(input$sha256 %||% NA_character_)[1])
  if (is.na(sha) || !grepl("^[0-9a-f]{64}$", sha)) {
    tf <- tempfile(); on.exit(unlink(tf), add = TRUE)
    writeLines(enc2utf8(as.character(.page_texts(input))), tf, useBytes = TRUE)
    sha <- file_sha256(tf)
  }
  r <- recipe_learn(rd, input, bank, sha, NULL, dirs$server)
  if (!identical(r$action, "created")) return(.rca_no(r$why))
  id <- sub("@v?[0-9]+$", "", r$ref)
  # A name the person gave ("Everyday account") is the draft's title: a new version.
  nm <- trimws(as.character(answers$title %||% "")[1])
  if (!is.na(nm) && nzchar(nm)) {
    u <- recipe_update(id, list(title = sprintf("%s %s", .layout_bank_display(.layout_slug(bank)), nm)), dirs)
    if (isTRUE(u$ok)) r$ref <- u$ref
  }
  .rca_ok(sprintf("Saved as draft recipe %s. Accept it to read statements like it on their own.", r$ref), r$ref, id = id)
}

# recipe_preview(input, bank, roles) -> list(outcome ("proven" / "check"), why,
# reading): the statement read by the automatic reader, with the person's column
# roles when given -- what "New recipe from a statement" shows before Save.
# Nothing is written.
recipe_preview <- function(input, bank = NULL, roles = NULL) {
  rd <- safe(auto_read(input, list(), bank, list(recipes = FALSE)), NULL)
  if (is.null(rd)) return(list(outcome = "check", why = "The statement could not be read.", reading = NULL))
  if (length(roles)) {
    o <- .override_roles(rd, roles)
    if (!is.null(o$error)) return(list(outcome = "check", why = o$error, reading = rd))
    rd <- safe(auto_read(input, list(), bank, list(roles = o$roles)), rd)
  }
  n <- if (is.data.frame(rd$transactions)) nrow(rd$transactions) else 0L
  if (identical(rd$outcome, "proven"))
    list(outcome = "proven", why = sprintf("It adds up: %d transaction%s, and the statement's own figures agree.", n, if (n == 1L) "" else "s"), reading = rd)
  else list(outcome = "check", why = sprintf("It does not add up yet: %s", rd$why %||% "no reason was given."), reading = rd)
}

# ---- what needs a look -------------------------------------------------------------

# needs_attention(dirs, tracking, uploads, days) -> list(counts, set_aside,
# drafts, failing, merges): statements set aside on Please check; drafts waiting
# to be accepted; recipes recognised recently whose reading did not add up (the
# bank may have changed the design); and pairs of recipes that look like one
# design (same bank, same table, sharing recognise words) -- each a merge offer.
needs_attention <- function(dirs = NULL, tracking = NULL, uploads = NULL, days = 30) {
  dirs <- .rca_dirs(dirs)
  up <- safe(read_uploads(uploads), NULL)
  aside <- if (is.data.frame(up) && nrow(up)) up[up$status %in% "set_aside" & !up$purged, c("id", "ts", "file_ext"), drop = FALSE]
           else data.frame(id = character(0), ts = character(0), file_ext = character(0), stringsAsFactors = FALSE)
  ov <- recipes_overview(dirs, tracking, days, hidden = FALSE)
  drafts <- ov[ov$enabled & ov$status == "draft", c("id", "bank", "title", "proofs"), drop = FALSE]
  failing <- ov[ov$enabled & ov$tried_not_proven > 0, c("id", "bank", "title", "tried_not_proven"), drop = FALSE]
  live <- Filter(function(t) !identical(t$status, "retired"),
                 lapply(ov$id[ov$enabled], function(id) .rca_top(dirs, id)$top))
  merges <- list()
  for (i in seq_along(live)) for (j in seq_along(live)) if (i < j) {
    a <- live[[i]]$rc; b <- live[[j]]$rc
    if (identical(.rca_table_key(a), .rca_table_key(b)) && any(tolower(a$all) %in% tolower(b$all)))
      merges[[length(merges) + 1L]] <- data.frame(a = a$id, b = b$id, bank = .layout_bank_display(a$bank), stringsAsFactors = FALSE)
  }
  merges <- if (length(merges)) do.call(rbind, merges) else data.frame(a = character(0), b = character(0), bank = character(0), stringsAsFactors = FALSE)
  for (x in c("aside", "drafts", "failing", "merges")) { v <- get(x); rownames(v) <- NULL; assign(x, v) }
  list(counts = c(set_aside = nrow(aside), drafts = nrow(drafts), failing = nrow(failing), merges = nrow(merges)),
       set_aside = aside, drafts = drafts, failing = failing, merges = merges)
}

# ---- the recipe card, in plain words -------------------------------------------------

# The plain word for each column role, as the card asks it (the keys .RCA_ROLES
# takes back).
.RCA_PLAIN <- c(date = "date", description = "description", debit = "money out", credit = "money in",
                amount = "amount", balance = "balance", date2 = "second date", particulars = "particulars",
                code = "code", reference = "reference", other_party = "other party", type = "type")
.rca_plain_role <- function(f) {
  f <- as.character(f)
  out <- unname(.RCA_PLAIN[f]); out[is.na(out)] <- "other"
  out
}

# recipe_card(id, dirs) -> what the card shows, in plain words, or list(error):
# id, bank, title, status, enabled, kind, columns (n, heading, role), date (an
# example as printed), money ("money out and money in" / "one amount"), recognise
# (the words), versions (version, status, what, when; newest first).
recipe_card <- function(id, dirs = NULL) {
  dirs <- .rca_dirs(dirs)
  t <- .rca_top(dirs, id); if (!is.null(t$error)) return(t)
  y <- t$top$y
  cols <- y$table$columns %||% list()
  hd <- vapply(cols, function(c) as.character(c$under %||% "")[1], "")
  ex <- function(fmt) {
    if (is.null(fmt) || is.na(fmt) || !nzchar(fmt)) return("")
    as.character(safe(format(as.Date("2026-02-03"), fmt), fmt))
  }
  what <- function(v) {
    a <- v$y$admin %||% list()
    if (!is.null(a$merged_into)) return(sprintf("merged into %s", a$merged_into))
    if (!is.null(a$merged_from)) return(sprintf("merged with %s", a$merged_from))
    if (identical(v$status, "retired")) return(if (isTRUE(a$hidden)) "retired" else "turned off")
    if (isTRUE(a$accepted)) return("accepted")
    if (v$version == 1L) return(if (identical(v$status, "draft")) "drafted from a statement" else "first version")
    "changed"
  }
  vs <- rev(t$all)
  list(id = t$top$id, bank = .layout_bank_display(y$bank), title = y$title %||% t$top$id,
       status = t$top$status, enabled = !identical(t$top$status, "retired"), kind = y$kind %||% "pdf",
       columns = data.frame(n = seq_along(cols), field = names(cols) %||% character(0), heading = unname(hd), role = .rca_plain_role(names(cols)),
                            stringsAsFactors = FALSE),
       date = ex(y$dates$format), date_format = as.character(y$dates$format %||% ""),
       money = if (identical(y$money$style, "signed")) "one amount" else "money out and money in",
       recognise = as.character(unlist(y$recognise$all)),
       proofs = length(.rca_proofs(dirs, t$top$id)$proved_by),
       versions = data.frame(version = vapply(vs, `[[`, 0L, "version"), status = vapply(vs, `[[`, "", "status"),
                             what = vapply(vs, what, ""),
                             when = vapply(vs, function(v) format(file.mtime(v$file), "%d %b %Y"), ""),
                             stringsAsFactors = FALSE))
}
