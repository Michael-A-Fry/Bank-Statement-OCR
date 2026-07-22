#!/usr/bin/env Rscript
# convert_for_qlik.R -- the CLI Qlik's ODAG load script (EXECUTE) or the poller
# calls to convert ONE statement with PROVEN templates only. Writes the outputs
# (xlsx/csv/json) + a status.json into <outdir>, and prints the status.json path.
#
#   Rscript scripts/convert_for_qlik.R <statement-file> <outdir> [requested_by]
#
# Qlik then reads <outdir>/status.json:
#   * status ok/needs_review  -> LOAD <outdir>/<base>.csv
#   * needs_template == true  -> show the message + open shiny_url
#
# All settings (proven templates dir, shiny_url, uploads queue) come from
# config/config.yaml (see R/config.R). Self-locates so a scheduled task / Qlik
# EXECUTE can launch it from any working directory.

.script_dir <- function() {
  args <- commandArgs(FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
}
setwd(.script_dir())
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

a <- commandArgs(TRUE)
if (length(a) < 2) {
  cat("usage: Rscript scripts/convert_for_qlik.R <statement-file> <outdir> [requested_by]\n")
  quit(status = 2)
}
path <- a[1]; outdir <- a[2]; who <- if (length(a) >= 3) a[3] else "qlik"
if (!file.exists(path)) { cat(sprintf("no such file: %s\n", path)); quit(status = 2) }

st <- convert_for_qlik(path, outdir, config = load_config(), requested_by = who)
cat(sprintf("%s  status=%s  needs_template=%s  csv=%s\n",
            file.path(outdir, "status.json"), st$status, st$needs_template,
            st$csv %||% "NA"))
