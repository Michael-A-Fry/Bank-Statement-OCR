#!/usr/bin/env Rscript
# install-offline.R -- the offline install step. On the server it travels inside
# the bundle as 'offline/install-on-pc.R' and RUN-ME.bat runs it for you on the
# first launch; you normally never call it by hand. No internet is used. It:
#   1) installs every R package from repo/            (safe, no admin)
#   2) unzips Poppler and adds it to your USER PATH    (for scanned-PDF OCR)
#   3) silent-installs Tesseract and adds it to PATH   (for scanned-PDF OCR)
#   4) prints the next step
#
#   Rscript install-on-pc.R      (run from inside the 'offline' folder)
#
# (R itself must already be installed -- you're running Rscript. If you still need
# R on this machine, the matching installer is in prereqs/ and RUN-ME.bat installs
# it silently before this script runs.)

.self_dir <- function() {
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) dirname(normalizePath(sub("^--file=", "", m[1]))) else getwd()
}
here <- .self_dir()

# Self-contained: this script sources nothing from the repo, so define the one
# helper it needs (run an expression, swallow any error) locally.
safe <- function(expr, default = NULL) tryCatch(expr, error = function(e) default)

# --- the USER PATH, read and written carefully ------------------------------
# Poppler and Tesseract are only usable if their folders are on PATH (R/ocr.R
# refuses a scan unless BOTH `tesseract` and `pdftoppm` resolve). Appending to the
# HKCU PATH is therefore part of setup -- but it rewrites the whole value, so
# getting the read wrong destroys the operator's environment silently.

# .reg(args) -- run reg.exe and report whether it actually succeeded. system2()
# returns character(0) with a status attribute on failure, so "no output" and
# "command failed" have to be told apart before anything is built from the result.
.reg <- function(args) {
  out <- tryCatch(suppressWarnings(system2("reg", args, stdout = TRUE, stderr = FALSE)),
                  error = function(e) structure(character(0), status = 1L))
  st <- attr(out, "status")
  list(ok = is.null(st) || identical(as.integer(st), 0L), out = as.character(out))
}

# .user_path() -- this user's own PATH exactly as stored (unexpanded, so a
# %USERPROFILE% in it survives), "" when they simply have none yet, and NA when it
# could not be read at all.
#
# The distinction is the whole point. The key was previously queried as
# "HKCU\\Environment" -- in R source that is a DOUBLED backslash, which is not the
# key name -- so the query always failed, the failed grep gave R's NA, and NA went
# on to be treated as a real value: nzchar(NA) is TRUE, so the new PATH became the
# literal string "NA;C:\...\poppler\bin" and everything the user had was gone.
# Then the Tesseract step repeated it and wiped the Poppler entry, so scanned PDFs
# stayed unreadable on a server that had shipped Poppler.
.user_path <- function() {
  if (!.reg(c("query", "HKCU\\Environment"))$ok) return(NA_character_)
  val <- .reg(c("query", "HKCU\\Environment", "/v", "Path"))
  if (!val$ok) return("")                      # readable key, no Path of their own
  ln <- grep("Path", grep("REG(_EXPAND)?_SZ", val$out, value = TRUE), value = TRUE)
  if (!length(ln)) return(NA_character_)
  sub(".*REG(_EXPAND)?_SZ\\s+", "", ln[1])
}

# .add_to_user_path(dir, what) -- append one folder, or say plainly why not.
# It REFUSES rather than guesses in two cases, because both alternatives are a
# machine broken in a way nothing announces:
#   - the current PATH could not be read (see above);
#   - the result would pass 1024 characters, where setx TRUNCATES silently.
.add_to_user_path <- function(dir, what) {
  cur <- .user_path()
  by_hand <- function(why) {
    cat(sprintf("%s: %s, so your PATH was NOT changed.\n", what, why))
    cat(sprintf("  Add this folder to your PATH by hand, then reopen the app: %s\n", dir))
    invisible(FALSE)
  }
  if (is.na(cur))              return(by_hand("your current PATH could not be read"))
  if (grepl(dir, cur, fixed = TRUE)) { cat(sprintf("%s: already on PATH.\n", what)); return(invisible(TRUE)) }
  newp <- if (nzchar(cur)) paste0(cur, ";", dir) else dir
  if (nchar(newp) > 1024)
    return(by_hand(sprintf("your PATH is %d characters and setx truncates at 1024", nchar(cur))))
  st <- safe(system2("setx", c("PATH", shQuote(newp)), stdout = FALSE, stderr = FALSE), default = 1L)
  if (!identical(as.integer(st), 0L)) return(by_hand("setx would not write it"))
  cat(sprintf("%s: added to your PATH -> %s\n  (open a NEW terminal for it to take effect)\n", what, dir))
  invisible(TRUE)
}

# Locate the bundle folders whether the script sits inside the offline/ folder or beside it.
find_dir <- function(name) {
  for (c in c(file.path(here, name), file.path(here, "offline", name),
              file.path(getwd(), name), Sys.getenv(toupper(paste0("BSO_", name)), "")))
    if (nzchar(c) && dir.exists(c)) return(normalizePath(c, winslash = "/"))
  ""
}
repo   <- find_dir("repo")
prereq <- find_dir("prereqs")
pkgs <- c("shiny", "DT", "yaml", "jsonlite", "openxlsx", "readxl",
          "pdftools", "magick", "testthat")

## 1. R packages ------------------------------------------------------------
if (!nzchar(repo)) stop("Could not find the 'repo' folder. Run this from inside the 'offline' folder.")
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
# FOUND BY WHAT IT CONTAINS, NOT BY WHAT IT IS CALLED. This used to glob
# `poppler*.zip`, which the builder satisfies because it renames its download --
# but the file a person downloads by hand from the project's releases page is
# called `Release-24.08.0-0.zip`, matches nothing, and the only sign was one line
# saying Poppler was "not in the bundle". The whole point of dropping it in by
# hand is that the automatic download failed, so that is exactly the moment the
# lookup has to be forgiving. Any zip here that carries pdftoppm.exe is Poppler,
# whatever it is named -- and an ALREADY-EXTRACTED copy is accepted too, because
# unzipping it first is the obvious thing to do and used to silently not work.
.poppler_bin <- function(prereq, here) {
  if (!nzchar(prereq)) return(NULL)
  # (a) already extracted, anywhere under prereqs/
  loose <- list.files(prereq, pattern = "^pdftoppm\\.exe$", recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  if (length(loose)) return(list(bin = dirname(loose[1]), from = "an extracted copy"))
  # (b) any zip whose listing contains pdftoppm.exe
  for (z in Sys.glob(file.path(prereq, "*.zip"))) {
    inside <- tryCatch(utils::unzip(z, list = TRUE)$Name, error = function(e) character(0))
    if (!any(grepl("(^|/)pdftoppm\\.exe$", inside, ignore.case = TRUE))) next
    dest <- file.path(here, "poppler")
    unlink(dest, recursive = TRUE); dir.create(dest, showWarnings = FALSE)
    utils::unzip(z, exdir = dest)
    bin <- dirname(list.files(dest, pattern = "^pdftoppm\\.exe$", recursive = TRUE,
                              full.names = TRUE, ignore.case = TRUE))
    if (length(bin)) return(list(bin = bin[1], from = basename(z)))
  }
  NULL
}
pop <- tryCatch(.poppler_bin(prereq, here), error = function(e) NULL)
if (!is.null(pop)) {
  cat("\nPoppler: found in", pop$from, "\n")
  .add_to_user_path(normalizePath(pop$bin), "Poppler")
} else {
  cat("\nPoppler: not found -- scanned PDFs will not be readable.\n")
  cat("  Looked in:", if (nzchar(prereq)) prereq else "(no prereqs folder)", "\n")
  cat("  Put the Poppler release ZIP there (any name), or an extracted copy -\n")
  cat("  anything containing pdftoppm.exe. Then run this again.\n")
  cat("  github.com/oschwartz10612/poppler-windows/releases\n")
}

## 3. Tesseract -> silent install + PATH (for scanned-PDF OCR) --------------
# Same forgiveness as Poppler above. The builder's download is
# `tesseract-ocr-w64-setup-<version>.exe`, but a hand-downloaded copy may be named
# anything, so any .exe here with "tesseract" in the name is taken as the
# installer -- narrow enough not to run something unrelated, wide enough to accept
# what a person actually ends up with.
tess <- if (nzchar(prereq))
          c(Sys.glob(file.path(prereq, "tesseract*setup*.exe")),
            Sys.glob(file.path(prereq, "*tesseract*.exe"))) else character(0)
tess <- unique(tess)
on_path <- nzchar(Sys.which("tesseract"))
if (on_path) {
  cat("\nTesseract: already on PATH.\n")
} else if (length(tess)) {
  cat("\nTesseract: installing silently...\n")
  safe(system2(normalizePath(tess[1]), "/S", wait = TRUE))   # NSIS silent
  tdir <- file.path(Sys.getenv("ProgramFiles", "C:/Program Files"), "Tesseract-OCR")
  if (dir.exists(tdir)) {
    # Same careful HKCU append as Poppler above -- and it re-reads the value setx
    # has just written, so adding Tesseract cannot drop the Poppler entry.
    .add_to_user_path(tdir, "Tesseract")
  } else cat("Tesseract: installer ran but", tdir, "not found -- add its bin folder to PATH manually.\n")
} else {
  cat("\nTesseract: not in the bundle -- only needed for scanned-PDF OCR.\n")
}

## 4. Next step --------------------------------------------------------------
# The exact command on the SERVER, where R is the app's own unregistered private
# copy: plain "Rscript" is either not on PATH or is some other R with none of these
# packages. See docs/operational/maintaining-the-engine.md.
cat("\nNext (from the app folder, one line at a time):\n")
cat("  set \"R_LIBS_USER=%CD%\\R-lib\"\n")
cat("  \"R-runtime\\bin\\x64\\Rscript.exe\" tests\\run_tests.R      (expect: failed: 0)\n")

## 5. Exit status -- RUN-ME.bat gates its .installed marker on this ----------
# A missing package is a FAILED setup, not a footnote: the app either will not
# start (shiny/yaml/jsonlite/openxlsx) or will silently lose a whole input path
# (pdftools/readxl/magick) or the ability to prove itself (testthat). Reporting
# success here would let RUN-ME.bat write its "never install again" marker over a
# broken install and freeze it in place.
if (length(miss)) {
  cat("\n[X] SETUP INCOMPLETE --", length(miss), "package(s) missing:",
      paste(miss, collapse = ", "), "\n")
  cat("    Nothing was marked as installed; running RUN-ME.bat again retries.\n")
  quit(status = 1)
}
cat("All R packages present -- the tool will run.\n")
quit(status = 0)
