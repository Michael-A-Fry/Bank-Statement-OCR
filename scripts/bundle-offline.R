#!/usr/bin/env Rscript
# bundle-offline.R -- RUN ON THE INTERNET-CONNECTED LAPTOP. One command collects
# EVERYTHING the offline Windows PC needs into a single ./bso-offline folder you
# then drag across:
#     bso-offline/
#       repo/          all R packages (+ every dependency) as Windows binaries
#       prereqs/       the R, Tesseract and Poppler installers (best effort)
#       install-on-pc.R  the script you run on the PC
#       packages.txt   the list, for reference
#
#   Rscript scripts/bundle-offline.R
#
# THE ONE RULE: run this under the SAME R x.y the PC will run (both 4.6). Windows
# package binaries are built per R minor version. See docs/OFFLINE-INSTALL.md.

.self_dir <- function() {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else getwd()
}

out       <- "bso-offline"
repo_path <- file.path(out, "repo")
prereq    <- file.path(out, "prereqs")
pkgs <- c("shiny", "DT", "yaml", "jsonlite", "openxlsx", "readxl",
          "pdftools", "magick", "testthat")
# Rserve: only for the OPTIONAL Qlik "Rserve / SSE" convert path. Bundled too so
# it's on hand if you use it (the recommended poller path needs none of this).
opt_pkgs <- "Rserve"
dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
dir.create(prereq,    recursive = TRUE, showWarnings = FALSE)

rxy   <- paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")
rfull <- paste(R.version$major, R.version$minor, sep = ".")
cat(sprintf("Laptop R: %s\n", R.version.string))
cat(sprintf("Bundling Windows binaries for R %s (the PC must run this same R x.y)\n\n", rxy))
options(timeout = 600)

## 1. R packages (+ all dependencies) -- the reliable core ------------------
if (!requireNamespace("miniCRAN", quietly = TRUE)) {
  cat("Installing miniCRAN (needs internet -- this laptop has it)...\n")
  install.packages("miniCRAN", repos = "https://cloud.r-project.org")
}
all <- miniCRAN::pkgDep(c(pkgs, opt_pkgs), type = "win.binary", suggests = FALSE)
cat(sprintf("Downloading %d R packages (with dependencies) into %s ...\n", length(all), repo_path))
miniCRAN::makeRepo(all, path = repo_path, type = "win.binary")
writeLines(pkgs, file.path(out, "packages.txt"))

## 2. System installers -- best effort; on any failure it logs the manual URL
grab <- function(url, dest, what) tryCatch({
  cat(sprintf("  %-10s downloading...\n", what))
  download.file(url, dest, mode = "wb", quiet = TRUE)
  cat(sprintf("  %-10s OK -> %s\n", what, basename(dest))); TRUE
}, error = function(e) {
  cat(sprintf("  %-10s skipped (%s)\n              download manually: %s\n",
              what, conditionMessage(e), url)); FALSE })

cat(sprintf("\nSystem installers into %s :\n", prereq))

# R installer for Windows, matching this laptop's exact version (try current
# release location, then the archived 'old' location).
r_dest <- file.path(prereq, sprintf("R-%s-win.exe", rfull))
if (!grab(sprintf("https://cran.r-project.org/bin/windows/base/R-%s-win.exe", rfull), r_dest, "R"))
  grab(sprintf("https://cran.r-project.org/bin/windows/base/old/%s/R-%s-win.exe", rfull, rfull), r_dest, "R (old)")

# Poppler for Windows -- newest release .zip (parse the GitHub API without needing
# jsonlite installed on the laptop yet).
tryCatch({
  txt <- paste(readLines("https://api.github.com/repos/oschwartz10612/poppler-windows/releases/latest",
                         warn = FALSE), collapse = "")
  z <- regmatches(txt, gregexpr("https://[^\"']*/Release-[^\"']*\\.zip", txt))[[1]]
  if (!length(z)) stop("no zip asset found")
  grab(z[1], file.path(prereq, "poppler-windows.zip"), "Poppler")
}, error = function(e)
  cat("  Poppler    skipped -- get it from github.com/oschwartz10612/poppler-windows/releases\n"))

# Tesseract for Windows -- newest UB Mannheim w64 installer (from its index page).
tryCatch({
  idx <- paste(readLines("https://digi.bib.uni-mannheim.de/tesseract/", warn = FALSE), collapse = "\n")
  m <- unique(regmatches(idx, gregexpr("tesseract-ocr-w64-setup-[0-9][^\"'> ]*\\.exe", idx))[[1]])
  if (!length(m)) stop("no installer on index")
  latest <- tail(sort(m), 1)
  grab(paste0("https://digi.bib.uni-mannheim.de/tesseract/", latest), file.path(prereq, latest), "Tesseract")
}, error = function(e)
  cat("  Tesseract  skipped -- get it from github.com/UB-Mannheim/tesseract/wiki\n"))

## 3. Make the folder self-contained: copy the PC-side script in -------------
pc <- file.path(.self_dir(), "install-offline.R")
if (file.exists(pc)) file.copy(pc, file.path(out, "install-on-pc.R"), overwrite = TRUE)

sz <- tryCatch(sum(file.info(list.files(out, recursive = TRUE, full.names = TRUE))$size,
                   na.rm = TRUE) / 1e6, error = function(e) NA_real_)
cat(sprintf("\nDONE -> %s  (R %s, ~%.0f MB total)\n", normalizePath(out), rxy,
            if (is.na(sz)) 0 else sz))
cat("Drag the whole 'bso-offline' folder to the PC, then on the PC run:\n")
cat("   Rscript install-on-pc.R\n")
