#!/usr/bin/env Rscript
# install-offline.R -- run ON THE LOCKED-DOWN, OFFLINE WINDOWS PC. Installs every
# R package this tool needs from the 'bso-offline' folder you copied over from the
# laptop (see scripts/bundle-offline.R and docs/OFFLINE-INSTALL.md).
#
#   Rscript scripts/install-offline.R
#
# Run it from the folder that CONTAINS 'bso-offline', or point BSO_OFFLINE at the
# repo folder. No internet is used -- everything comes from the local files.

repo <- Sys.getenv("BSO_OFFLINE", "bso-offline/repo")
if (!dir.exists(repo)) stop(sprintf(
  "Local package repo not found at '%s'. Copy the 'bso-offline' folder here, or set BSO_OFFLINE.", repo))
repo <- normalizePath(repo, winslash = "/", mustWork = TRUE)
repo_url <- paste0("file:///", repo)

pkgs <- c("shiny", "DT", "yaml", "jsonlite", "openxlsx", "readxl",
          "pdftools", "magick", "testthat")

cat(sprintf("Installing from %s\n(R %s.%s)\n\n", repo_url,
            R.version$major, sub("\\..*", "", R.version$minor)))
install.packages(pkgs, repos = repo_url, type = "win.binary", dependencies = TRUE)

ok <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
miss <- setdiff(pkgs, ok)
cat(sprintf("\nInstalled %d/%d packages.\n", length(ok), length(pkgs)))
if (length(miss)) {
  cat("MISSING:", paste(miss, collapse = ", "), "\n")
  cat("Most likely the bundle was built under a DIFFERENT R x.y than this PC.",
      "Re-run scripts/bundle-offline.R on the laptop under R",
      sprintf("%s.%s.\n", R.version$major, sub("\\..*", "", R.version$minor)))
} else {
  cat("All present. Next: Rscript tests\\run_tests.R  (expect: failed: 0)\n")
  cat("For scanned-PDF OCR, also install Tesseract + Poppler (see docs/OFFLINE-INSTALL.md).\n")
}
