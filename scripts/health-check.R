#!/usr/bin/env Rscript
# health-check.R -- IS THIS SERVER FIT TO CONVERT, right now, in one command.
#
#   Rscript scripts/health-check.R          # from the app folder, or from anywhere
#
# Run it after every update, and any time somebody says "it has stopped working".
# It reads; it changes nothing. It exits 0 when everything passed and 1 when
# anything failed, so a scheduled task can watch it without anybody reading it.
#
# WHY IT EXISTS: nothing on the box could answer, in one place, the five questions
# an operator has after an update -- did the settings file parse, is Admin still
# shut behind the shipped placeholder password, how many templates loaded AND HOW
# MANY WERE REFUSED, can the app write to the folders it needs, and is the OCR
# software still installed. Each one had its own answer somewhere (a warning at
# startup, a banner inside Admin, an R warning nobody sees) and two of them could
# only be found by opening a browser on a server nobody logs into.
#
# The refusals are the reason this is not a nicety. All three template loaders
# already work out exactly why they skipped a template and hand the reasons back
# on attr(x, "load_errors") -- and NOTHING in the product read that attribute, so a
# template that stopped validating after an update simply vanished: gone from
# Admin, gone from detection, no message anywhere, and the statements it used to
# read quietly became "no template for this statement yet".
#
# KEEP IT SHORT. It is read under pressure, by somebody who has just been told the
# tool is down. One line per check, in one screen, no numbers nobody can act on.

.self_dir <- function() {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
}
root <- .self_dir(); setwd(root); Sys.setenv(ENGINE_ROOT = root)
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))
  suppressWarnings(try(source(f), silent = TRUE))
# THE FIRST THING AN UPDATE CAN BREAK is the engine itself, and this is the moment
# it is being asked about. Answer that in a sentence rather than with a stack of R
# text on the console of a server nobody logs into.
if (!exists("load_config", mode = "function")) {
  cat("\nFAIL  Engine     the program files in R\\ did not load, so this copy cannot",
      "convert anything - restore the previous version from the backup.\n\n")
  quit(status = 1L, save = "no")
}

# ---- the report --------------------------------------------------------------
# Two states, and only two. A third ("warning") is a state nobody knows what to do
# with, and this page exists to be acted on.
.results <- list()
say <- function(ok, area, detail) {
  .results[[length(.results) + 1L]] <<- list(ok = isTRUE(ok), area = area, detail = detail)
  cat(sprintf("%-5s %-10s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", area, detail))
}

ver <- tryCatch(trimws(readLines(file.path(root, "VERSION"), warn = FALSE)[1]),
                error = function(e) "?")
cat(sprintf("\nStatement Studio %s\n%s\n\n", ver, root))

# ---- 1. the settings file ----------------------------------------------------
cfg_path <- tryCatch(.config_path(), error = function(e) "config/config.yaml")
cfg <- tryCatch(load_config(refresh = TRUE), error = function(e) NULL)
cfg_err <- if (is.null(cfg)) {
  sprintf("%s could not be read at all, so every setting is a built-in default", cfg_path)
} else tryCatch(config_error(cfg), error = function(e) NULL)
if (is.null(cfg)) cfg <- tryCatch(.config_defaults(), error = function(e) list())
say(is.null(cfg_err), "Settings",
    if (is.null(cfg_err)) sprintf("%s was read and every setting in it was understood", cfg_path)
    else cfg_err)

# ---- 2. the admin password ---------------------------------------------------
shut <- isTRUE(tryCatch(admin_password_is_default(cfg), error = function(e) TRUE))
say(!shut, "Admin",
    if (shut) "the admin password is still the placeholder shipped in the example file, so Admin is closed to everybody"
    else "an admin password is set")

# ---- 3. the templates, loaded AND refused ------------------------------------
p <- cfg$paths %||% list()
# Each loader is asked for the reasons it skipped a template. The curated
# statement set is loaded strictly, so it STOPS rather than returning reasons --
# that message is a refusal too, and it is the one that takes the whole set down.
.load <- function(expr) {
  v <- tryCatch(expr, error = function(e) structure(list(), stopped = conditionMessage(e)))
  list(n = length(v),
       why = c(as.character(attr(v, "load_errors") %||% character(0)),
               as.character(attr(v, "stopped") %||% character(0))))
}
kinds <- list(
  "bank statement" = .load(load_template_set(p$templates %||% "templates/statements",
                                             p$user_templates %||% "templates/statements_user",
                                             include_hidden = TRUE)),
  "form"           = .load(load_fields_templates(p$fields %||% "templates/fields",
                                                 p$user_fields %||% "templates/fields_user",
                                                 include_hidden = TRUE)),
  "report"         = .load(load_document_templates(p$docs %||% "templates/documents",
                                                   p$user_docs %||% "templates/documents_user",
                                                   include_hidden = TRUE)))
counted <- paste(vapply(names(kinds), function(k) sprintf("%d %s", kinds[[k]]$n, k),
                        character(1)), collapse = ", ")
refused <- unlist(lapply(kinds, `[[`, "why"), use.names = FALSE)
say(!length(refused), "Templates", sprintf("%s; %s", counted,
    if (!length(refused)) "none were refused"
    else sprintf("%d refused and NOT in use - %s", length(refused),
                 paste(refused, collapse = "; "))))

# ---- 4. the folders it has to write to ---------------------------------------
# Tested by really writing, because "the folder is there" and "this account may
# write to it" are different questions on a Windows share, and only the second one
# decides whether a conversion can be recorded.
#
# A folder that is not there yet is not a fault: the app makes uploads\, logs\ and
# feed\ the first time it needs them. So the question asked of a missing folder is
# whether its PARENT can be written to, which is the same question one step up. It
# is never created here -- a check that changes the box it is checking is a check
# nobody can trust twice.
.writable <- function(d) {
  if (is.null(d) || !nzchar(d %||% "")) return(FALSE)
  while (!dir.exists(d) && nzchar(dirname(d)) && !identical(dirname(d), d)) d <- dirname(d)
  if (!dir.exists(d)) return(FALSE)
  f <- file.path(d, sprintf(".healthcheck.%s", Sys.getpid()))
  ok <- isTRUE(tryCatch({ writeLines("x", f); TRUE }, error = function(e) FALSE))
  unlink(f)
  ok && !file.exists(f)
}
folders <- c(p$logs %||% "logs", p$uploads %||% "uploads", p$requests %||% "requests",
             p$user_templates %||% "templates/statements_user",
             p$user_fields %||% "templates/fields_user",
             p$user_docs %||% "templates/documents_user",
             cfg$feed$feed_dir %||% "feed")
bad_folders <- folders[!vapply(folders, .writable, logical(1))]
say(!length(bad_folders), "Folders",
    if (!length(bad_folders)) sprintf("all %d can be written to", length(folders))
    else paste("cannot be written to:", paste(bad_folders, collapse = ", ")))

# ---- 5. reading scans --------------------------------------------------------
missing_ocr <- c("tesseract", "pdftoppm")[!nzchar(Sys.which(c("tesseract", "pdftoppm")))]
say(!length(missing_ocr), "Scans",
    if (!length(missing_ocr)) "the scan-reading software is installed"
    else sprintf("a scanned statement cannot be read on this server - %s %s not installed",
                 paste(missing_ocr, collapse = " and "),
                 if (length(missing_ocr) == 1L) "is" else "are"))

# ---- the verdict -------------------------------------------------------------
failed <- Filter(function(r) !r$ok, .results)
if (!length(failed)) {
  cat("\nPASS - this server is ready to convert.\n\n")
  quit(status = 0L, save = "no")
}
# Not repeated line by line: every failure is already printed above, in order, and
# a page that says the same thing twice is a page people stop reading.
cat(sprintf("\nFAIL - %d of %d checks did not pass (marked FAIL above).\n\n",
            length(failed), length(.results)))
quit(status = 1L, save = "no")
