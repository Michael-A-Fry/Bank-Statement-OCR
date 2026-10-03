#!/usr/bin/env Rscript
# audit-statement.R -- write a SAFE-TO-SHARE structural audit of a statement, so
# you (or a tool like Copilot) can describe layout/format issues WITHOUT sending
# any PII. Every value is masked to its shape only (letters -> x/X, digits -> 9);
# no merchant names, amounts, account numbers or dates appear.
#
#   Rscript scripts/audit-statement.R <statement.pdf> [out.md]
#
# Output defaults to <name>.audit.md next to where you run it. Read it, confirm it
# is safe, then share it. See docs/operational/when-something-goes-wrong.md.
#
# The statement is read the way a conversion on this install reads it: the bank
# worked out from the statement, then the automatic reader with the layouts this
# install has learned for that bank. Those are only READ -- nothing is learned,
# written or logged -- so it is safe to run on the live server.

.self_dir <- function() {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
}
root <- .self_dir(); Sys.setenv(ENGINE_ROOT = root)
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

args <- commandArgs(TRUE)
if (!length(args)) { cat("usage: Rscript scripts/audit-statement.R <statement.(pdf|csv|xlsx)> [out.md]\n"); quit(status = 1) }
path <- args[1]
if (!file.exists(path)) { cat("file not found:", path, "\n"); quit(status = 1) }
out <- if (length(args) >= 2) args[2] else paste0(tools::file_path_sans_ext(basename(path)), ".audit.md")

# The settings and the learned layouts are this install's, wherever this is run
# from: paths in config.yaml are relative to the app folder, not to the console.
cfg  <- load_config(if (nzchar(Sys.getenv("BSO_CONFIG"))) Sys.getenv("BSO_CONFIG")
                    else file.path(root, "config", "config.yaml"))
ldir <- layouts_dir(cfg)
if (!grepl("^([A-Za-z]:)?[/\\\\]", ldir)) ldir <- file.path(root, ldir)
writeLines(format_audit(statement_audit(path, layouts_dir = ldir)), out)
cat("Wrote safe audit ->", normalizePath(out), "\n")
cat("It contains NO PII (shapes only). Read it, then share it.\n")
