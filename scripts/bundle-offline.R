#!/usr/bin/env Rscript
# bundle-offline.R -- RUN ON AN INTERNET-CONNECTED WINDOWS PC (usually via the
# double-click make-bundle.bat). One command assembles a SINGLE self-contained
# folder that is the whole product plus everything it needs to install itself on
# an air-gapped server:
#
#     StatementStudio-offline/
#       RUN-ME.bat          <- the only thing you run on the server (double-click)
#       app.R  R/  templates/  ...   the app itself
#       config/config.example.yaml       seeds only if the server has no config yet
#       dictionaries/*.example.yaml      ditto -- the server's taught words are LIVE
#                                        state and are never overwritten by an update
#       offline/
#         repo/             all R packages (+ every dependency) as Windows binaries
#         prereqs/          the R, Poppler and Tesseract installers
#         install-on-pc.R   package/OCR install (RUN-ME.bat calls this for you)
#         packages.txt      the list, for reference
#         manifest.txt      what this bundle IS: app + R + package versions
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

# Paths that must NEVER travel inside a bundle. samples/_private_staging holds
# REAL bank statements -- its own README promises the folder "stays local ...
# never included in any package", and .gitignore refuses to commit it -- and
# tests/testthat/logs holds run logs that name them. Both sit INSIDE directories
# the bundle legitimately ships (samples/, tests/), and file.copy(recursive=TRUE)
# takes a directory whole, so nothing in app_items stops them being carried out of
# the building on the USB stick. Pruned, and then PROVEN pruned, in section 1.
never_ship      <- c("samples/_private_staging", "tests/testthat/logs")
never_ship_keep <- "samples/_private_staging/README.md"   # explains the empty folder

# The bundle IS the product. These are the parts without which it is not a
# runnable install at all, as opposed to one missing a capability.
# VERSION is on this list, not merely in app_items: engine_version() reads it and
# stamps the result into EVERY run record and every workbook. A bundle without it
# installs and converts perfectly while answering "which build produced this
# figure?" with "unknown" -- the one question investigating-a-wrong-conversion.md
# is built on, unanswerable ever afterwards for every run made before anyone notices.
essential <- c("R", "app.R", "ui_content.R", "ui_labels.R", "templates",
               "config", "scripts", "www", "RUN-ME.bat", "VERSION")

# .missing_from_repo(want, path) -- which of `want` has no .zip in the offline
# package repo. A named helper so the seam can be checked without the internet.
.missing_from_repo <- function(want, path)
  setdiff(want, sub("_.*$", "", basename(
    list.files(path, pattern = "\\.zip$", recursive = TRUE))))

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
#
# NOT in this list, deliberately: `dictionaries`. Those two files (labels.yaml,
# lexicon.yaml) are LIVE state on the server -- Admin writes them every time the
# team teaches the tool a new wording or recognition marker. Shipping them would
# mean a routine "replace the files in the destination" update silently reverts
# every word the team taught it, and statements that reconciled last week quietly
# stop reconciling. Same reason config.yaml is deleted from the dist below; the
# dist carries *.example.yaml instead and RUN-ME.bat seeds them only if absent.
# "www" carries app.css. Shiny serves that folder from disk, so leaving it out of
# this list does not fail the build or the tests -- it ships an install that runs
# perfectly and looks completely unstyled, which is the kind of break nobody finds
# until it is in front of someone. test-deployment-docs.R pins it.
app_items <- c("R", "templates", "templates_user", "templates_seed",
               "fields_templates", "doc_templates", "doc_templates_user",
               "config", "scripts", "www",
               "tests", "samples", "docs",
               "app.R", "ui_content.R", "ui_labels.R", "CHANGELOG.md", "run.R", "README.md", "RUN-ME.bat",
               "VERSION")
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

# Prune what must never travel, then PROVE the prune worked rather than trusting
# it. One real statement leaving on a USB stick is the failure this unit cannot
# take back, so anything still standing here stops the build.
for (np in never_ship) unlink(file.path(dist, np), recursive = TRUE)
keep_src <- file.path(app_root, never_ship_keep)
if (file.exists(keep_src)) {
  dir.create(dirname(file.path(dist, never_ship_keep)), recursive = TRUE, showWarnings = FALSE)
  file.copy(keep_src, file.path(dist, never_ship_keep), overwrite = TRUE)
}
left <- unlist(lapply(never_ship, function(np) {
  d <- file.path(dist, np)
  if (!dir.exists(d)) return(character(0))
  file.path(np, list.files(d, recursive = TRUE, all.files = TRUE))
}))
left <- setdiff(left, never_ship_keep)
if (length(left)) stop(sprintf(paste0(
  "private data would have shipped inside this bundle: %s\n",
  "  Those paths hold real statements and never leave this machine. Remove them\n",
  "  from '%s' and rebuild."), paste(left, collapse = ", "), dist))
cat(sprintf("  private staging excluded (%s)\n", paste(never_ship, collapse = ", ")))

# ...and every essential part actually ARRIVED. file.copy() reports failure by
# return value, which the loop above discards, so "copied N items" counts attempts,
# not successes: a full disk or a locked file otherwise yields a bundle with no app
# in it that says nothing until it is on the air-gapped box.
absent <- essential[!file.exists(file.path(dist, essential))]
if (length(absent)) stop(sprintf(paste0(
  "not a runnable bundle -- these never arrived in '%s': %s\n",
  "  Either they are missing from %s, or the copy failed (disk full, file open).\n",
  "  Fix that and rebuild."), dist, paste(absent, collapse = ", "), app_root))

# Never ship a live config.yaml -- only config.example.yaml. This way copying a
# fresh bundle over an existing server install can't overwrite server settings;
# RUN-ME.bat seeds/restores config.yaml on the server.
# Checked, not assumed: unlink() reports failure by return value. A config.yaml
# that survived (locked, read-only) carries the build PC's ADMIN PASSWORD into the
# bundle, and copying that bundle over an existing server install would replace
# the server's real settings with it -- including the feed folder.
unlink(file.path(dist, "config", "config.yaml"))
if (file.exists(file.path(dist, "config", "config.yaml")))
  stop("config/config.yaml could not be removed from the bundle. It holds the admin\n",
       "  password and must never ship. Close whatever has it open, then rebuild.")

# Dictionaries: ship EXAMPLES ONLY, for exactly the same reason as config.yaml.
# The repo's dictionaries/*.yaml are the shipped starting vocabularies; on the
# server the same filenames are Admin-edited live state. Copy each one to
# <name>.example.yaml so RUN-ME.bat can seed a first install, while an update
# leaves the server's taught words untouched.
dict_src <- file.path(app_root, "dictionaries")
dict_shipped <- 0L        # recorded in the manifest: no seeds is a real gap
if (dir.exists(dict_src)) {
  dir.create(file.path(dist, "dictionaries"), recursive = TRUE, showWarnings = FALSE)
  dfiles <- list.files(dict_src, pattern = "\\.ya?ml$", full.names = TRUE)
  dfiles <- dfiles[!grepl("\\.example\\.ya?ml$", dfiles)]   # don't double-suffix
  for (d in dfiles)
    file.copy(d, file.path(dist, "dictionaries",
                           sub("\\.(ya?ml)$", ".example.\\1", basename(d))),
              overwrite = TRUE, copy.date = TRUE)
  dict_shipped <- length(dfiles)
  cat(sprintf("  dictionaries shipped as %d example file(s) (server keeps its own)\n",
              dict_shipped))
}

# Force every shipped .bat to CRLF so cmd.exe runs it reliably, regardless of how
# the folder was obtained (we ship no line-ending config to normalise them for us).
for (b in list.files(dist, pattern = "\\.bat$", recursive = TRUE, full.names = TRUE)) {
  ln <- readLines(b, warn = FALSE)
  con <- file(b, open = "wb"); writeLines(ln, con, sep = "\r\n"); close(con)
}

## 2. R packages (+ all dependencies) -- the reliable core ------------------
# Set the CRAN mirror EXPLICITLY. Under a stock `Rscript` (no profile, which is
# exactly how make-bundle.bat calls this) getOption("repos") is the unresolved
# "@CRAN@" placeholder, and miniCRAN then either prompts for a mirror -- with
# nothing to answer it -- or resolves to nothing at all. Setting it here means the
# build behaves the same on every PC.
cran <- unname(getOption("repos")["CRAN"])
if (length(cran) != 1 || is.na(cran) || !nzchar(cran) || identical(cran, "@CRAN@"))
  cran <- "https://cloud.r-project.org"
options(repos = c(CRAN = cran))
cat(sprintf("CRAN mirror: %s\n", cran))
if (!requireNamespace("miniCRAN", quietly = TRUE)) {
  cat("\nInstalling miniCRAN (needs internet -- this PC has it)...\n")
  install.packages("miniCRAN", repos = cran)
}
all <- miniCRAN::pkgDep(pkgs, type = "win.binary", suggests = FALSE)
cat(sprintf("\nDownloading %d R packages (with dependencies) into %s ...\n", length(all), repo_path))
miniCRAN::makeRepo(all, path = repo_path, type = "win.binary")
writeLines(pkgs, file.path(bundle, "packages.txt"))

# makeRepo WARNS about a package it could not fetch and carries on, and a warning
# scrolls off the screen. The only proof the server can install offline is a .zip
# per package actually sitting in repo/. Without it the air-gapped box prints
# "SETUP INCOMPLETE", RUN-ME.bat refuses to start the app, and there is no CRAN in
# the room to fix it -- so it fails here, where there is.
no_zip <- .missing_from_repo(all, repo_path)
if (length(no_zip)) stop(sprintf(paste0(
  "%d of %d packages did not download, so the server cannot install offline: %s\n",
  "  Fix this PC's internet/proxy and run make-bundle.bat again."),
  length(no_zip), length(all), paste(sort(no_zip), collapse = ", ")))

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

# A bundle with no R installer is not a bundle: RUN-ME.bat cannot install the
# private R, so NOTHING works on the server. This used to print "skipped", carry on
# to "DONE" and exit 0 -- make-bundle.bat reported success and the failure surfaced
# days later, on the air-gapped box, as "Could not set up the private R". Fail here,
# where the internet is, and say exactly what to do.
if (!file.exists(r_dest)) stop(sprintf(paste0(
  "no R installer was downloaded, so this bundle cannot install R on the server.\n",
  "  Expected: %s\n",
  "  Fix: check this PC's internet/proxy and run make-bundle.bat again, or download\n",
  "       R-%s-win.exe from https://cran.r-project.org/bin/windows/base/ (or .../base/old/%s/)\n",
  "       into that folder by hand and re-run."), r_dest, rfull, rfull))

# Poppler for Windows -- newest release .zip (parse the releases API without needing
# jsonlite installed on this PC yet).
got_poppler <- tryCatch({
  txt <- paste(readLines("https://api.github.com/repos/oschwartz10612/poppler-windows/releases/latest",
                         warn = FALSE), collapse = "")
  z <- regmatches(txt, gregexpr("https://[^\"']*/Release-[^\"']*\\.zip", txt))[[1]]
  if (!length(z)) stop("no zip asset found")
  grab(z[1], file.path(prereq, "poppler-windows.zip"), "Poppler")
}, error = function(e) {
  cat("  Poppler    skipped -- get it from github.com/oschwartz10612/poppler-windows/releases\n")
  FALSE })

# Tesseract for Windows -- newest UB Mannheim w64 installer (from its index page).
got_tesseract <- tryCatch({
  idx <- paste(readLines("https://digi.bib.uni-mannheim.de/tesseract/", warn = FALSE), collapse = "\n")
  m <- unique(regmatches(idx, gregexpr("tesseract-ocr-w64-setup-[0-9][^\"'> ]*\\.exe", idx))[[1]])
  if (!length(m)) stop("no installer on index")
  latest <- tail(sort(m), 1)
  grab(paste0("https://digi.bib.uni-mannheim.de/tesseract/", latest), file.path(prereq, latest), "Tesseract")
}, error = function(e) {
  cat("  Tesseract  skipped -- get it from github.com/UB-Mannheim/tesseract/wiki\n")
  FALSE })

## 4. The PC-side install script (RUN-ME.bat calls this) --------------------
# THE THIRD PREREQUISITE, and the one that used to be silent. RUN-ME.bat's first
# run does `pushd offline` + `Rscript install-on-pc.R` unconditionally, so a bundle
# without this file installs nothing -- no packages, no OCR, no app -- and then
# blames "some R packages could not be installed", sending the operator off to
# rebuild for a reason that was never true. Poppler and Tesseract only cost you
# scanned PDFs, so they are recorded as MISSING and the build goes on; this one
# costs you the whole install, exactly like the R installer above, so it FAILS THE
# BUILD here, while the file that went missing is still within reach.
pc      <- file.path(.self_dir(), "install-offline.R")
pc_dest <- file.path(bundle, "install-on-pc.R")
if (!file.exists(pc)) stop(sprintf(paste0(
  "the bundle's own installer is missing, so nothing would install on the server.\n",
  "  Expected: %s\n",
  "  It ships as offline/install-on-pc.R and RUN-ME.bat runs it on the first\n",
  "  launch. If it was renamed or moved, put it back and rebuild."), pc))
if (!isTRUE(file.copy(pc, pc_dest, overwrite = TRUE)) || !file.exists(pc_dest))
  stop(sprintf("could not copy %s -> %s. Check free disk space, then rebuild.", pc, pc_dest))

## 5. Manifest -- WHAT is actually in this bundle ---------------------------
# An air-gapped box has no way to ask "which version am I running, and which
# package versions did it ship with?" -- and that is the first question anyone asks
# when a conversion behaves differently on the server than on the build PC. The
# manifest travels with the bundle and answers it without internet.
man <- c(
  "Statement Studio -- offline bundle manifest",
  sprintf("built_utc:   %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  sprintf("app_version: %s", tryCatch(
    paste(readLines(file.path(app_root, "VERSION"), warn = FALSE), collapse = " "),
    error = function(e) "(no VERSION file)")),
  sprintf("r_version:   %s   <- the server installs and runs THIS R", R.version.string),
  sprintf("built_on:    %s", Sys.info()[["sysname"]]),
  "",
  # EVERY prerequisite, present or absent -- not just the two that used to be
  # listed. updating.md tells the operator to "check its last lines for MISSING
  # before you carry it": the manifest is the only thing that travels with the
  # USB stick, so anything it does not mention cannot be checked at all, and a
  # prerequisite nobody knew was absent is discovered on the air-gapped box.
  "prerequisites -- what the server needs, and whether it is here:",
  sprintf("  r_installer:   %s", if (file.exists(r_dest)) basename(r_dest)
          else "MISSING (the server cannot install its private R -- nothing runs)"),
  sprintf("  installer:     %s", if (file.exists(pc_dest)) basename(pc_dest)
          else "MISSING (RUN-ME.bat has nothing to run -- nothing installs)"),
  sprintf("  packages:      %s", {
    gap <- .missing_from_repo(pkgs, repo_path)
    if (length(gap)) sprintf("MISSING %s (the app will not start)", paste(sort(gap), collapse = ", "))
    else sprintf("%d file(s) in offline/repo", length(list.files(repo_path, pattern = "\\.zip$", recursive = TRUE)))
  }),
  sprintf("  poppler:       %s", if (isTRUE(got_poppler)) "included" else "MISSING (scanned PDFs will not be readable)"),
  sprintf("  tesseract:     %s", if (isTRUE(got_tesseract)) "included" else "MISSING (scanned PDFs will not be readable)"),
  sprintf("  app_payload:   %s", if (length(missing)) sprintf("INCOMPLETE -- not in the repo: %s", paste(missing, collapse = ", "))
          else sprintf("complete (%d items)", copied)),
  sprintf("  dictionaries:  %s", if (dict_shipped > 0L)
          sprintf("%d starting vocabulary file(s)", dict_shipped)
          else "MISSING (a brand-new install starts with no taught wordings)"),
  sprintf("  private_data:  excluded (%s) -- real statements never travel", paste(never_ship, collapse = ", ")),
  "", "packages (name version) -- every file in offline/repo:")
zips <- list.files(repo_path, pattern = "\\.zip$", recursive = TRUE)
man <- c(man, if (length(zips)) sort(sub("^(.+)_([^_]+)\\.zip$", "\\1 \\2", basename(zips)))
              else "  (none -- the package download failed)")
writeLines(man, file.path(bundle, "manifest.txt"))

sz <- tryCatch(sum(file.info(list.files(dist, recursive = TRUE, full.names = TRUE))$size,
                   na.rm = TRUE) / 1e6, error = function(e) NA_real_)
cat(sprintf("\nDONE -> %s  (R %s, ~%.0f MB total)\n", normalizePath(dist), rxy,
            if (is.na(sz)) 0 else sz))
cat(sprintf("      manifest: %s\n", file.path(bundle, "manifest.txt")))

# A missing OCR tool is NOT a detail: the server will read text PDFs, CSV and Excel
# fine and then hit a scanned statement it cannot read at all. Say so here, loudly,
# while the internet is still available to fix it -- not days later on the server.
if (!isTRUE(got_poppler) || !isTRUE(got_tesseract)) {
  missing_ocr <- c(if (!isTRUE(got_poppler)) "Poppler", if (!isTRUE(got_tesseract)) "Tesseract")
  cat("\n***********************************************************************\n")
  cat(sprintf("*** WARNING -- this bundle has NO %s.\n", paste(missing_ocr, collapse = " and ")))
  cat("*** SCANNED PDFs WILL NOT BE READABLE ON THE SERVER. Text PDFs, CSV and\n")
  cat("*** Excel still work; a scanned page will be refused with a clear reason.\n")
  cat(sprintf("*** Fix: download the installer(s) into %s\n", prereq))
  cat("***      Poppler   github.com/oschwartz10612/poppler-windows/releases\n")
  cat("***      Tesseract github.com/UB-Mannheim/tesseract/wiki\n")
  cat("***      then re-run make-bundle.bat, or copy them in and rebuild.\n")
  cat("***********************************************************************\n")
}
cat("\nNext: copy the whole 'StatementStudio-offline' folder to the server and\n")
cat("      double-click RUN-ME.bat inside it. That's the entire server setup.\n")
