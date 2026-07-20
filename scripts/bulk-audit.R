#!/usr/bin/env Rscript
# bulk-audit.R -- point this at a FOLDER of statements (any bank, any variant,
# selectable OR scanned) and get ONE safe-to-share report: what parses, the
# unsupported layouts clustered biggest-gap-first, editable DRAFT templates for
# those gaps, and a feature-gap summary. No PII ever leaves your machine.
#
#   Rscript scripts/bulk-audit.R <folder>            # -> bulk-audit.md + audit-drafts/
#   Rscript scripts/bulk-audit.R <folder> report.md
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
                    pattern = "\\.(pdf|csv|tsv|tdv|txt|xlsx|xlsm)$", ignore.case = TRUE)
if (!length(paths)) { cat("no statements found under", folder, "\n"); quit(status = 1) }
cat(sprintf("Auditing %d file(s) under %s ...\n", length(paths), folder))

tmpls <- load_template_set(file.path(root, "templates"), file.path(root, "templates_user"))
b <- batch_audit(paths, templates = tmpls)
writeLines(format_batch_audit(b), out)

# write each recommended draft to audit-drafts/<id>.yaml so you can edit + Save
ddir <- "audit-drafts"; dir.create(ddir, showWarnings = FALSE)
for (r in b$recommendations) {
  id <- gsub("[^A-Za-z0-9_]+", "_", r$draft_id %||% paste0("draft_", substr(r$signature, 1, 6)))
  writeLines(r$draft_yaml, file.path(ddir, paste0(id, ".yaml")))
}
g <- b$feature_gaps
cat(sprintf("\nDone. %d parsed, %d unsupported across %d layouts. %d draft template(s) written to %s/.\n",
    g$total - g$unsupported, g$unsupported, g$distinct_gap_layouts, length(b$recommendations), ddir))
cat("Safe report ->", normalizePath(out), "  (no PII — read it, then share it)\n")
