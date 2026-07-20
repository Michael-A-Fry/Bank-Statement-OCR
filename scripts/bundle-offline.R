#!/usr/bin/env Rscript
# bundle-offline.R -- run ON THE INTERNET-CONNECTED LAPTOP to collect every R
# package this tool needs (plus ALL dependencies) into one self-contained folder
# you copy to the locked-down, offline Windows PC. See docs/OFFLINE-INSTALL.md.
#
#   Rscript scripts/bundle-offline.R           # -> ./bso-offline/  (Windows binaries)
#
# THE ONE RULE: run this under the SAME R x.y version the PC will run (e.g. both
# 4.6), because Windows package binaries are built per R minor version. Ideally
# run it on Windows R 4.6. It warns you if that doesn't look right.

out <- "bso-offline"
repo_path <- file.path(out, "repo")
pkgs <- c("shiny", "DT", "yaml", "jsonlite", "openxlsx", "readxl",
          "pdftools", "magick", "testthat")

cat(sprintf("This R:  %s\n", R.version.string))
cat(sprintf("Target:  Windows x86_64, same R x.y as above\n"))
rxy <- paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")
cat(sprintf("Bundling Windows binaries for R %s\n\n", rxy))
if (.Platform$OS.type != "windows")
  cat("!! You are NOT on Windows. This can still download Windows binaries, but",
      "running it on a Windows R", rxy, "is the safe path.\n\n")

dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
if (!requireNamespace("miniCRAN", quietly = TRUE)) {
  cat("Installing miniCRAN (needs internet -- this laptop has it)...\n")
  install.packages("miniCRAN", repos = "https://cloud.r-project.org")
}
type <- "win.binary"   # Windows target, regardless of this laptop's OS
all <- miniCRAN::pkgDep(pkgs, type = type, suggests = FALSE)
cat(sprintf("Resolved %d packages (with every dependency). Downloading...\n", length(all)))
miniCRAN::makeRepo(all, path = repo_path, type = type)

writeLines(pkgs, file.path(out, "packages.txt"))
sz <- tryCatch(sum(file.info(list.files(repo_path, recursive = TRUE,
        full.names = TRUE))$size, na.rm = TRUE) / 1e6, error = function(e) NA_real_)
if (is.na(sz)) sz <- 0
cat(sprintf("\nDone -> %s  (R %s, ~%.0f MB)\n", normalizePath(out), rxy, sz))
cat("Next: copy the whole 'bso-offline' folder to the PC, then on the PC run\n")
cat("      Rscript scripts/install-offline.R\n")
