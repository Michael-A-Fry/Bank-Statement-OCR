#!/usr/bin/env Rscript
# bundle-offline.R -- RUN ON AN INTERNET-CONNECTED WINDOWS PC (usually via the
# double-click make-bundle.bat). One command assembles a SINGLE self-contained
# folder that is the whole product plus everything it needs to install itself on
# an air-gapped server:
#
#     StatementStudio-offline/
#       RUN-ME.bat          <- the only thing you run on the server (double-click)
#       app.R  R/  templates/  ...   the app itself
#       offline/
#         repo/             all R packages (+ every dependency) as Windows binaries
#         prereqs/          the R, Poppler and Tesseract installers
#         install-on-pc.R   package/OCR install (RUN-ME.bat calls this for you)
#         packages.txt      the list, for reference
#
#   Rscript scripts/bundle-offline.R
#
# Then: copy the whole 'StatementStudio-offline' folder to the server and
# double-click RUN-ME.bat. No internet is used on the server.
#
# NO VERSION MATCHING NEEDED: the bundle ships the R installer for THIS PC's R and,
# on the server, RUN-ME.bat installs and uses that exact R privately (inside the
# folder), so the packages here always match. Whatever R the server already has is
# ignored. So this just needs ANY recent R with internet. See docs/operational/first-time-setup.md.

.self_dir <- function() {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else getwd()
}
# The repo root is the parent of scripts/.
app_root <- normalizePath(file.path(.self_dir(), ".."), winslash = "/", mustWork = FALSE)

dist      <- "StatementStudio-offline"
bundle    <- file.path(dist, "offline")
repo_path <- file.path(bundle, "repo")
prereq    <- file.path(bundle, "prereqs")
pkgs <- c("shiny", "DT", "yaml", "jsonlite", "openxlsx", "readxl",
          "pdftools", "magick", "testthat")

unlink(dist, recursive = TRUE)
dir.create(repo_path, recursive = TRUE, showWarnings = FALSE)
dir.create(prereq,    recursive = TRUE, showWarnings = FALSE)

rxy   <- paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")
rfull <- paste(R.version$major, R.version$minor, sep = ".")
cat(sprintf("Building PC R: %s\n", R.version.string))
cat(sprintf("Assembling '%s' for R %s (the server must run this same R x.y)\n\n", dist, rxy))
options(timeout = 600)

## 1. Copy the app itself into the dist folder ------------------------------
# Everything the app needs at runtime, plus samples/docs/tests for the demo and
# smoke test. Runtime/PII dirs (logs, uploads, feed, out...) are deliberately NOT
# copied -- they are created on first run and must never travel.
# Deliberately no source-control or hidden dotfiles in the shipped folder: it is a
# plain product folder, nothing more.
app_items <- c("R", "templates", "templates_user", "templates_seed",
               "dictionaries", "fields_templates", "config", "scripts",
               "tests", "samples", "docs",
               "app.R", "ui_content.R", "run.R", "README.md", "RUN-ME.bat")
cat("Copying the app into the dist folder ...\n")
copied <- 0L; missing <- character(0)
for (it in app_items) {
  src <- file.path(app_root, it)
  if (!file.exists(src)) { missing <- c(missing, it); next }
  if (dir.exists(src)) {
    file.copy(src, dist, recursive = TRUE, copy.date = TRUE)
  } else {
    file.copy(src, file.path(dist, it), overwrite = TRUE, copy.date = TRUE)
  }
  copied <- copied + 1L
}
cat(sprintf("  copied %d items%s\n", copied,
            if (length(missing)) sprintf("; skipped (not present): %s",
                                         paste(missing, collapse = ", ")) else ""))
# RUN-ME.bat must be in the dist root -- if it wasn't in the repo, that's fatal.
if (!file.exists(file.path(dist, "RUN-ME.bat")))
  stop("RUN-ME.bat is missing from the repo root -- cannot build a runnable bundle.")

# Never ship a live config.yaml -- only config.example.yaml. This way copying a
# fresh bundle over an existing server install can't overwrite server settings;
# RUN-ME.bat seeds/restores config.yaml on the server.
unlink(file.path(dist, "config", "config.yaml"))

# Force every shipped .bat to CRLF so cmd.exe runs it reliably, regardless of how
# the folder was obtained (we ship no line-ending config to normalise them for us).
for (b in list.files(dist, pattern = "\\.bat$", recursive = TRUE, full.names = TRUE)) {
  ln <- readLines(b, warn = FALSE)
  con <- file(b, open = "wb"); writeLines(ln, con, sep = "\r\n"); close(con)
}

## 2. R packages (+ all dependencies) -- the reliable core ------------------
if (!requireNamespace("miniCRAN", quietly = TRUE)) {
  cat("\nInstalling miniCRAN (needs internet -- this PC has it)...\n")
  install.packages("miniCRAN", repos = "https://cloud.r-project.org")
}
all <- miniCRAN::pkgDep(pkgs, type = "win.binary", suggests = FALSE)
cat(sprintf("\nDownloading %d R packages (with dependencies) into %s ...\n", length(all), repo_path))
miniCRAN::makeRepo(all, path = repo_path, type = "win.binary")
writeLines(pkgs, file.path(bundle, "packages.txt"))

## 3. System installers -- best effort; on any failure it logs the manual URL
grab <- function(url, dest, what) tryCatch({
  cat(sprintf("  %-10s downloading...\n", what))
  download.file(url, dest, mode = "wb", quiet = TRUE)
  cat(sprintf("  %-10s OK -> %s\n", what, basename(dest))); TRUE
}, error = function(e) {
  cat(sprintf("  %-10s skipped (%s)\n              download manually: %s\n",
              what, conditionMessage(e), url)); FALSE })

cat(sprintf("\nSystem installers into %s :\n", prereq))

# R installer for Windows, matching this PC's exact version (try current release
# location, then the archived 'old' location).
r_dest <- file.path(prereq, sprintf("R-%s-win.exe", rfull))
if (!grab(sprintf("https://cran.r-project.org/bin/windows/base/R-%s-win.exe", rfull), r_dest, "R"))
  grab(sprintf("https://cran.r-project.org/bin/windows/base/old/%s/R-%s-win.exe", rfull, rfull), r_dest, "R (old)")

# Poppler for Windows -- newest release .zip (parse the releases API without needing
# jsonlite installed on this PC yet).
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

## 4. The PC-side install script (RUN-ME.bat calls this) --------------------
pc <- file.path(.self_dir(), "install-offline.R")
if (file.exists(pc)) file.copy(pc, file.path(bundle, "install-on-pc.R"), overwrite = TRUE)

sz <- tryCatch(sum(file.info(list.files(dist, recursive = TRUE, full.names = TRUE))$size,
                   na.rm = TRUE) / 1e6, error = function(e) NA_real_)
cat(sprintf("\nDONE -> %s  (R %s, ~%.0f MB total)\n", normalizePath(dist), rxy,
            if (is.na(sz)) 0 else sz))
cat("Next: copy the whole 'StatementStudio-offline' folder to the server and\n")
cat("      double-click RUN-ME.bat inside it. That's the entire server setup.\n")
