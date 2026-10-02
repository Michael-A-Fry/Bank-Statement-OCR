#!/usr/bin/env Rscript
# bench.R -- how long each stage of the engine takes on a BIG statement, and whether
# it is still right at that size.
#
#     python3 tools/synth/make_bench.py --out /tmp/bench
#     Rscript  tools/synth/bench.R /tmp/bench
#
# WHY PER STAGE AND NOT A TOTAL. A total says "slow" and nothing else. The first run
# of this harness is what found that the whole-document SVG sweep and the per-page
# raster scan dominate everything else put together on a 100-page file -- neither of
# which is visible in a single number, and neither of which is where anyone would
# have guessed.
#
# IT ALSO SCORES CORRECTNESS, because a fast wrong answer is not a result. Each
# benchmark file carries the same `.truth.json` a corpus case does.

suppressWarnings(suppressMessages({ library(yaml); library(jsonlite) }))

args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[1] else stop("usage: bench.R <bench-dir> [case-filter]")
filt <- if (length(args) > 1) args[2] else NULL

root <- Sys.getenv("ENGINE_ROOT", unset = normalizePath("."))
Sys.setenv(ENGINE_ROOT = root)
for (f in list.files(file.path(root, "R"), "[.]R$", full.names = TRUE)) source(f)
source(file.path(root, "tools", "synth", "truth.R"))

tset <- load_templates(file.path(root, "templates", "statements"))
TPL <- tset[["anz_everyday_pdf"]]

# .el(expr) -- elapsed seconds, and the value. Wall clock, not user time: the raster
# and SVG stages are EXTERNAL PROCESSES, so the user time of this R session does not
# see them at all and would report them as free.
.el <- function(expr) {
  t0 <- Sys.time()
  v <- force(expr)
  list(secs = as.numeric(difftime(Sys.time(), t0, units = "secs")), value = v)
}

.mb <- function() {
  g <- gc(FALSE)
  sum(g[, "used"] * c(8, 8)[seq_len(nrow(g))]) / 1024^2   # rough, and comparable
}


cases <- sort(sub("[.]truth[.]json$", "", basename(
  list.files(dir, "[.]truth[.]json$"))))
if (!is.null(filt)) cases <- cases[grepl(filt, cases, fixed = TRUE)]
if (!length(cases)) stop("no benchmark cases in ", dir)

hdr <- "%-13s %5s %6s %8s %8s %8s %8s %8s %8s %9s %7s %6s\n"
cat(sprintf(hdr, "case", "pages", "rows", "read", "detect", "parse", "recon",
            "diag", "write", "TOTAL s", "s/page", "FABR"))
cat(strrep("-", 118), "\n")

out <- list()
for (cs in cases) {
  pdf <- file.path(dir, paste0(cs, ".pdf"))
  if (!file.exists(pdf)) next
  want <- read_truth(file.path(dir, paste0(cs, ".truth.json")))$want
  clear_input_cache()
  m0 <- .mb()

  r_in  <- .el(read_input(pdf))
  input <- r_in$value
  np    <- length(input$words %||% list())

  r_det <- .el(detect_statement(input, tset))
  tpl   <- if (isTRUE(r_det$value$matched)) tset[[r_det$value$template_id]] else TPL

  r_par <- .el(parse_statement(input, tpl))
  parsed <- r_par$value
  tx <- parsed$transactions

  r_rec <- .el(reconcile(parsed, tpl))
  recon <- r_rec$value

  # the diagnostics stage as the engine really runs it, column_fit included: that is
  # the stage this harness was extended to watch
  r_dia <- .el(build_diagnostics("ok", parsed = parsed, recon = recon,
    metadata = c(.page_shape_note(input, tpl), .column_fit_note_meta(input, tpl))))

  od <- file.path(tempdir(), paste0("bench_", cs))
  dir.create(od, showWarnings = FALSE, recursive = TRUE)
  r_wri <- .el(write_outputs(parsed, recon, od, cs, c("xlsx", "csv")))

  peak <- .mb() - m0
  total <- r_in$secs + r_det$secs + r_par$secs + r_rec$secs + r_dia$secs + r_wri$secs

  # ---- still right? -------------------------------------------------------
  fab <- 0L; ref <- 0L
  n <- min(length(want), nrow(tx))
  for (i in seq_len(n)) {
    w <- want[[i]]
    g <- suppressWarnings(as.numeric(tx$amount[i]))
    if (!eq_money(g, w$amount)) {
      if (is.na(g)) ref <- ref + 1L else fab <- fab + 1L
    }
  }
  miss <- abs(length(want) - nrow(tx))

  cat(sprintf(hdr, cs, np, nrow(tx),
              sprintf("%.2f", r_in$secs), sprintf("%.2f", r_det$secs),
              sprintf("%.2f", r_par$secs), sprintf("%.2f", r_rec$secs),
              sprintf("%.2f", r_dia$secs), sprintf("%.2f", r_wri$secs),
              sprintf("%.1f", total),
              sprintf("%.3f", if (np) total / np else NA_real_),
              fab))
  out[[cs]] <- list(case = cs, pages = np, rows = nrow(tx),
                    read = r_in$secs, detect = r_det$secs, parse = r_par$secs,
                    recon = r_rec$secs, diag = r_dia$secs, write = r_wri$secs,
                    total = total, mb = peak, fabricated = fab, refused = ref,
                    missing = miss, trust = recon$trust$level %||% NA)
  unlink(od, recursive = TRUE)
  clear_input_cache()
}

cat(strrep("-", 118), "\n")
for (o in out)
  cat(sprintf("%-13s %3d pages  %5d rows  %6.1fs  (%.2f s/page)  ~%.0f MB  trust %s  fabricated %d  refused %d  missing %d\n",
              o$case, o$pages, o$rows, o$total, o$total / max(1, o$pages), o$mb,
              o$trust, o$fabricated, o$refused, o$missing))

bad <- Filter(function(o) o$fabricated > 0 || o$missing > 0, out)
if (length(bad)) {
  cat("\nFABRICATED OR MISSING AT SIZE -- a fast wrong answer is not a result\n")
  quit(status = 1)
}
cat("\n0 fabricated figures, 0 missing rows at every size.\n")
