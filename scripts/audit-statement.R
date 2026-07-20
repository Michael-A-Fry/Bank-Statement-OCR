#!/usr/bin/env Rscript
# audit-statement.R -- write a SAFE-TO-SHARE structural audit of a statement, so
# you (or a tool like Copilot) can describe layout/format issues WITHOUT sending
# any PII. Every value is masked to its shape only (letters -> x/X, digits -> 9);
# no merchant names, amounts, account numbers or dates appear.
#
#   Rscript scripts/audit-statement.R <statement.pdf> [out.md]
#
# Output defaults to <name>.audit.md next to where you run it. Read it, confirm it
# is safe, then share it. See docs/COPILOT-STATEMENT-REVIEW.md for the Copilot flow.

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

tmpls <- load_template_set(file.path(root, "templates"), file.path(root, "templates_user"))
writeLines(format_audit(statement_audit(path, templates = tmpls)), out)
cat("Wrote safe audit ->", normalizePath(out), "\n")
cat("It contains NO PII (shapes only). Read it, then share it.\n")
