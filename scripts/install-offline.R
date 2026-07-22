#!/usr/bin/env Rscript
# install-offline.R -- RUN ON THE OFFLINE WINDOWS PC, from inside the bso-offline
# folder you dragged over (it also travels as 'install-on-pc.R'). No internet is
# used. It:
#   1) installs every R package from repo/            (safe, no admin)
#   2) unzips Poppler and adds it to your USER PATH    (for scanned-PDF OCR)
#   3) points you at the Tesseract installer to run once
#   4) runs the test suite if the app folder is alongside
#
#   Rscript install-on-pc.R
#
# (R itself must already be installed -- you're running Rscript. If you still need
# R on this machine, the matching installer is in prereqs/.)

.self_dir <- function() {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else getwd()
}
here <- .self_dir()

# Locate the bundle folders whether the script sits inside bso-offline/ or beside it.
find_dir <- function(name) {
  for (c in c(file.path(here, name), file.path(here, "bso-offline", name),
              file.path(getwd(), name), Sys.getenv(toupper(paste0("BSO_", name)), "")))
    if (nzchar(c) && dir.exists(c)) return(normalizePath(c, winslash = "/"))
  ""
}
repo   <- find_dir("repo")
prereq <- find_dir("prereqs")
pkgs <- c("shiny", "DT", "yaml", "jsonlite", "openxlsx", "readxl",
          "pdftools", "magick", "testthat")

## 1. R packages ------------------------------------------------------------
if (!nzchar(repo)) stop("Could not find the 'repo' folder. Run this from inside the 'bso-offline' folder.")
cat(sprintf("Installing R packages from %s\n(R %s.%s)\n\n", repo,
            R.version$major, sub("\\..*", "", R.version$minor)))
install.packages(pkgs, repos = paste0("file:///", repo), type = "win.binary", dependencies = TRUE)
ok   <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
miss <- setdiff(pkgs, ok)
cat(sprintf("\nR packages: %d/%d installed.\n", length(ok), length(pkgs)))
if (length(miss)) cat("  MISSING:", paste(miss, collapse = ", "),
  "\n  Likely an R-version mismatch -- rebuild the bundle on the laptop under R",
  sprintf("%s.%s.\n", R.version$major, sub("\\..*", "", R.version$minor)))

## 2. Poppler -> USER PATH (for scanned-PDF OCR) ----------------------------
if (nzchar(prereq) && length(Sys.glob(file.path(prereq, "poppler*.zip")))) {
  zip <- Sys.glob(file.path(prereq, "poppler*.zip"))[1]
  dest <- file.path(here, "poppler")
  tryCatch({
    unlink(dest, recursive = TRUE); dir.create(dest, showWarnings = FALSE)
    utils::unzip(zip, exdir = dest)
    bin <- dirname(list.files(dest, pattern = "^pdftoppm\\.exe$",
                              recursive = TRUE, full.names = TRUE))
    if (length(bin)) {
      bin <- normalizePath(bin[1])
      cur <- tryCatch({
        q <- system2("reg", c("query", "HKCU\\\\Environment", "/v", "Path"), stdout = TRUE, stderr = FALSE)
        sub(".*REG(_EXPAND)?_SZ\\s+", "", grep("Path", q, value = TRUE)[1])
      }, error = function(e) "")
      if (!grepl(bin, cur, fixed = TRUE)) {
        newp <- if (nzchar(cur)) paste0(cur, ";", bin) else bin
        system2("setx", c("PATH", shQuote(newp)), stdout = FALSE, stderr = FALSE)
        cat(sprintf("\nPoppler: added to your PATH -> %s\n  (open a NEW terminal for it to take effect)\n", bin))
      } else cat("\nPoppler: already on PATH.\n")
    } else cat("\nPoppler: unzipped to", dest, "-- add its bin\\ folder to PATH manually.\n")
  }, error = function(e) cat("\nPoppler: could not auto-configure --", conditionMessage(e),
                             "\n  Unzip", zip, "and add its bin\\ folder to PATH.\n"))
} else cat("\nPoppler: not in the bundle -- only needed for scanned-PDF OCR.\n")

## 3. Tesseract -> silent install + PATH (for scanned-PDF OCR) --------------
tess <- if (nzchar(prereq)) Sys.glob(file.path(prereq, "tesseract*setup*.exe")) else character(0)
on_path <- nzchar(Sys.which("tesseract"))
if (on_path) {
  cat("\nTesseract: already on PATH.\n")
} else if (length(tess)) {
  cat("\nTesseract: installing silently...\n")
  safe(system2(normalizePath(tess[1]), "/S", wait = TRUE))   # NSIS silent
  tdir <- file.path(Sys.getenv("ProgramFiles", "C:/Program Files"), "Tesseract-OCR")
  if (dir.exists(tdir)) {
    # Careful HKCU PATH append (same approach as Poppler above), not a raw setx of
    # the whole expanded PATH.
    cur <- tryCatch({
      q <- system2("reg", c("query", "HKCU\\\\Environment", "/v", "Path"), stdout = TRUE, stderr = FALSE)
      sub(".*REG(_EXPAND)?_SZ\\s+", "", grep("Path", q, value = TRUE)[1])
    }, error = function(e) "")
    if (!grepl(tdir, cur, fixed = TRUE)) {
      newp <- if (nzchar(cur)) paste0(cur, ";", tdir) else tdir
      system2("setx", c("PATH", shQuote(newp)), stdout = FALSE, stderr = FALSE)
      cat(sprintf("Tesseract: installed + added to PATH -> %s\n  (open a NEW terminal for it to take effect)\n", tdir))
    } else cat("Tesseract: installed; already on PATH.\n")
  } else cat("Tesseract: installer ran but", tdir, "not found -- add its bin folder to PATH manually.\n")
} else {
  cat("\nTesseract: not in the bundle -- only needed for scanned-PDF OCR.\n")
}

## 4. Next step --------------------------------------------------------------
cat("\nNext: from the app folder run  Rscript tests\\run_tests.R  (expect: failed: 0)\n")
if (!length(miss)) cat("All R packages present -- the tool will run.\n")
