#!/usr/bin/env Rscript
# qlik_poller.R -- the Qlik feed poller (interactive Mode B, async path).
#
# Watches the Qlik inbox (where Inphinity Forms writes uploads), converts each
# dropped statement with PROVEN templates only, and produces exactly what the Qlik
# app needs:
#   qlik/outbox/<key>/statement.csv   -- the ODAG app LOADs this (key = content hash)
#   qlik/outbox/<key>/status.json     -- status detail (incl. shiny_url on a miss)
#   qlik/index/<key>.csv              -- one status row; the selection app's file table
# then moves the original into qlik/processed/. No R needs to run inside Qlik.
#
#   Rscript scripts/qlik_poller.R          # process the inbox once, then exit
#   Rscript scripts/qlik_poller.R loop     # keep polling (default every 15s)
#
# Concurrency-safe: keyed by content hash, one folder per statement, no shared
# append (see docs/architecture/qlik-sense-integration.md section 16).

.script_dir <- function() {
  args <- commandArgs(FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) dirname(dirname(normalizePath(sub("^--file=", "", m[1])))) else getwd()
}
setwd(.script_dir())
for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

cfg <- load_config()
INBOX <- cfg$qlik$inbox; OUTBOX <- cfg$qlik$outbox
INDEX <- cfg$qlik$index; PROCESSED <- cfg$qlik$processed
for (d in c(INBOX, OUTBOX, INDEX, PROCESSED, cfg$paths$logs, cfg$paths$uploads))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# .stable(f) -- TRUE once a file's size has settled (not still being copied in).
.stable <- function(f) {
  s1 <- file.info(f)$size; Sys.sleep(1); s2 <- file.info(f)$size
  !is.na(s1) && !is.na(s2) && s1 == s2
}

.csv_cell <- function(x) {                 # minimal CSV quoting for the index row
  x <- as.character(x %||% ""); if (is.na(x)) x <- ""
  if (grepl('[",\n]', x)) paste0('"', gsub('"', '""', x), '"') else x
}

process_once <- function() {
  files <- list.files(INBOX, full.names = TRUE,
                      pattern = "\\.(csv|tsv|tdv|txt|xlsx|xlsm|pdf)$", ignore.case = TRUE)
  done <- 0L
  for (f in files) {
    if (!.stable(f)) { cat(sprintf("%s  still-copying  %s\n",
                                   format(Sys.time(), "%H:%M:%S"), basename(f))); next }
    key <- .qlik_key(f)
    st  <- convert_for_qlik(f, file.path(OUTBOX, key), config = cfg, requested_by = "qlik")
    # one status row -> the Qlik selection app's file-list table (wildcard-loaded)
    hdr <- "key,file,status,needs_template,row_count,converted_ts,csv,shiny_url"
    row <- paste(vapply(list(key, basename(f), st$status, tolower(as.character(st$needs_template)),
                             st$row_count, format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                             st$csv, st$shiny_url), .csv_cell, character(1)), collapse = ",")
    safe(writeLines(c(hdr, row), file.path(INDEX, paste0(key, ".csv"))))
    # move the original out of the inbox so it isn't reprocessed
    dest <- file.path(PROCESSED, basename(f))
    if (!suppressWarnings(file.rename(f, dest)))
      safe({ file.copy(f, dest, overwrite = TRUE); file.remove(f) })
    done <- done + 1L
    cat(sprintf("%s  %-13s  %s  ->  outbox/%s/\n", format(Sys.time(), "%H:%M:%S"),
                st$status, basename(f), key))
  }
  done
}

if (length(commandArgs(TRUE)) && identical(commandArgs(TRUE)[1], "loop")) {
  cat(sprintf("qlik_poller: polling %s every 15s (Ctrl-C to stop)\n", INBOX))
  repeat {
    tryCatch(process_once(),
             error = function(e) cat(sprintf("%s  !! poll error: %s\n",
               format(Sys.time(), "%H:%M:%S"), conditionMessage(e))))
    Sys.sleep(15)
  }
} else {
  n <- process_once(); cat(sprintf("done: %d file(s) processed\n", n))
}
