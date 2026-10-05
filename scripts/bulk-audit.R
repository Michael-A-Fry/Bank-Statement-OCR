#!/usr/bin/env Rscript
# bulk-audit.R -- point this at a FOLDER of statements (any bank, any variant,
# selectable OR scanned) and get ONE safe-to-share report: how many the automatic
# reader proves on its own, which it does not, the ones it cannot read clustered
# by layout biggest-gap-first, and which of its checks fail most. No PII ever
# leaves your machine.
#
#   Rscript scripts/bulk-audit.R <folder>            # -> bulk-audit.md
#   Rscript scripts/bulk-audit.R <folder> report.md
#
# It reads, and changes nothing: each file goes through the bank identification
# and the automatic reader with the layouts this install has learned for that
# bank, and nothing is converted, logged or learned -- a pile nobody chose to
# train on must not teach the tool. (It used to write a draft template per gap
# into audit-drafts/. Templates are gone since 2.0.0; a bank is trained on
# Admin -> Banks instead.)
#
# Large scanned batches take a while (each scanned page is OCR'd). Text PDFs / CSV
# / Excel are fast.

.self_dir <- function() {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
}
root <- .self_dir(); Sys.setenv(ENGINE_ROOT = root)
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

args <- commandArgs(TRUE)
if (!length(args)) { cat("usage: Rscript scripts/bulk-audit.R <folder> [report.md]\n"); quit(status = 1) }
folder <- args[1]
if (!dir.exists(folder)) { cat("not a folder:", folder, "\n"); quit(status = 1) }
out <- if (length(args) >= 2) args[2] else "bulk-audit.md"

paths <- list.files(folder, recursive = TRUE, full.names = TRUE,
                    pattern = "\\.(pdf|csv|tsv|tdv|txt|xlsx|xlsm|xls)$", ignore.case = TRUE)
if (!length(paths)) { cat("no statements found under", folder, "\n"); quit(status = 1) }
cat(sprintf("Auditing %d file(s) under %s ...\n", length(paths), folder))

# The settings and the learned layouts are this install's, wherever this is run
# from: paths in config.yaml are relative to the app folder, not to the console.
cfg  <- load_config(if (nzchar(Sys.getenv("BSO_CONFIG"))) Sys.getenv("BSO_CONFIG")
                    else file.path(root, "config", "config.yaml"))
ldir <- layouts_dir(cfg)
if (!grepl("^([A-Za-z]:)?[/\\\\]", ldir)) ldir <- file.path(root, ldir)
b <- batch_audit(paths, layouts_dir = ldir)
writeLines(format_batch_audit(b), out)

g <- b$feature_gaps
cat(sprintf("\nDone. %d statement(s): %s. %d not read, across %d distinct layout(s).\n",
    g$total, paste(sprintf("%s %s", names(g$by_outcome), unlist(g$by_outcome)), collapse = ", "),
    g$unread, g$distinct_gap_layouts))
cat("Safe report ->", normalizePath(out), "  (no PII - read it, then share it)\n")
