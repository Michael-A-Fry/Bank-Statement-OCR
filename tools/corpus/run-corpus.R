#!/usr/bin/env Rscript
# run-corpus.R -- point the document engine at a folder of real PDFs and report
# what it does with each one.
#
# WHY THIS EXISTS. Every fixture in tests/testthat was written by somebody who
# already knew how the engine works, which is the one thing a fixture cannot fix
# about itself. A folder of documents nobody here wrote -- rotated pages, ruled
# and unruled tables, spanning headers, several tables to a page, forms, pages
# that are one big image -- is the only way to find out what the engine does when
# it meets a shape it was not designed against.
#
# WHAT IT IS NOT. It is not a test and it has no expected answers, because there
# are none: nobody has said what the right tables are on somebody else's PDF. It
# is a SURVEY. It reports, per document, whether the engine crashed, what it
# proposed, and whether what it proposed reads back out consistently. A crash and
# a disagreement are bugs; a document with no tables on it proposing none is the
# engine being right.
#
# The documents themselves are NOT in this repository. They are third-party files
# under their own licences, and this repository does not carry other people's
# PDFs. Point this at a folder you assembled yourself:
#
#   Rscript tools/corpus/run-corpus.R /path/to/pdfs [out.csv]
#
# tools/corpus/fetch-corpus.py collects a suitable folder from the test suites of
# the open-source table-extraction projects, if you have network access.

.this_dir <- function() {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
}
root <- normalizePath(file.path(.this_dir(), "..", ".."))
suppressWarnings(for (.loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"))
  if (nzchar(Sys.setlocale("LC_CTYPE", .loc))) break)
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

args <- commandArgs(TRUE)
dir <- if (length(args) >= 1L) args[1] else stop("give me a folder of PDFs")
out_csv <- if (length(args) >= 2L) args[2] else file.path(dir, "corpus-report.csv")
files <- sort(list.files(dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE))
if (!length(files)) stop("no PDFs in ", dir)

# .cap(expr) -- run it and come back with either the value or the error, never
# with the process ending. A survey that stops at the first bad document has
# surveyed one document.
.cap <- function(expr) tryCatch(list(ok = TRUE, v = force(expr)),
                                error = function(e) list(ok = FALSE, e = conditionMessage(e)))

rows <- vector("list", length(files))
for (k in seq_along(files)) {
  f <- files[k]
  nm <- basename(f)
  t0 <- Sys.time()
  r <- list(file = nm, pages = NA_integer_, words = NA_integer_, tables = 0L,
            table_rows = 0L, pairs = 0L, unclaimed = NA_integer_, thin = NA_integer_,
            by_heading = NA_integer_, agree = NA, secs = NA_real_,
            problem = "", note = "")

  inp <- .cap(read_input(f))
  if (!inp$ok) {
    r$problem <- paste("read_input:", inp$e)
    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE); next
  }
  i <- inp$v
  r$pages <- .doc_npages(i)
  r$words <- sum(vapply(i$words %||% list(), function(w) if (is.null(w)) 0L else nrow(w), integer(1)))
  if (isTRUE(r$words == 0L)) {
    r$note <- "no text layer (a scan); the engine needs OCR first"
    r$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE); next
  }

  pr <- .cap(propose_tables(i))
  if (!pr$ok) {
    r$problem <- paste("propose_tables:", pr$e)
    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE); next
  }
  props <- pr$v
  r$tables <- length(props)

  pp <- .cap(propose_pairs(i, page = 1L))
  pairs <- if (pp$ok) pp$v else list()
  if (!pp$ok) r$problem <- paste("propose_pairs:", pp$e)
  r$pairs <- length(pairs)

  if (!length(props) && !length(pairs)) {
    r$note <- "nothing proposed"
    r$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE); next
  }

  tm <- .cap(document_template_from_proposal(
    id = "corpus", bank = "Corpus", statement_type = "survey",
    phrases = "a phrase that will not be looked for here",
    tables = props, pairs = pairs, doc_pages = r$pages))
  if (!tm$ok) {
    r$problem <- paste("template:", tm$e)
    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE); next
  }
  tmpl <- tm$v
  tmpl$ref_width <- (i$page_width %||% .A4_W)[1]
  tmpl$ref_height <- (i$page_height %||% .A4_H)[1]

  ex <- .cap(extract_document(i, tmpl))
  if (!ex$ok) {
    r$problem <- paste("extract_document:", ex$e)
    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE); next
  }
  e <- ex$v
  s <- e$summary
  r$table_rows <- sum(as.integer(s$rows))
  r$unclaimed <- sum(as.integer(s$unclaimed_words))
  r$thin <- sum(as.integer(s$thin_columns))
  r$by_heading <- sum(s$found_by %in% c("its heading", "the rows it starts with"))
  # THE ONE THAT MATTERS. The proposer counted the rows one way and the reader
  # counted them another. If those two disagree, the template a person confirmed
  # on screen is not the template that produces their file.
  n_prop <- vapply(props, function(p) .doc_int(p$n_rows, NA_integer_), integer(1))
  n_read <- as.integer(s$rows)
  r$agree <- length(n_prop) == length(n_read) && all(n_prop == n_read, na.rm = TRUE)
  r$secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE)
  cat(sprintf("%3d/%d  %-58s %2d tables %5d rows %s\n", k, length(files),
              substr(nm, 1, 58), r$tables, r$table_rows,
              if (nzchar(r$problem)) "PROBLEM" else if (isFALSE(r$agree)) "DISAGREE" else ""))
}

d <- do.call(rbind, rows)
utils::write.csv(d, out_csv, row.names = FALSE, na = "")

cat("\n================ SURVEY ================\n")
cat(sprintf("documents          %d\n", nrow(d)))
cat(sprintf("crashed            %d\n", sum(nzchar(d$problem))))
cat(sprintf("no text layer      %d\n", sum(grepl("no text layer", d$note))))
cat(sprintf("nothing proposed   %d\n", sum(grepl("nothing proposed", d$note))))
cat(sprintf("tables proposed    %d across %d documents\n",
            sum(d$tables), sum(d$tables > 0, na.rm = TRUE)))
cat(sprintf("rows read          %d\n", sum(d$table_rows, na.rm = TRUE)))
cat(sprintf("values proposed    %d\n", sum(d$pairs, na.rm = TRUE)))
cat(sprintf("proposer and reader disagree on rows   %d\n", sum(isFALSE(d$agree) | !d$agree, na.rm = TRUE)))
cat(sprintf("documents with words no column claimed %d\n",
            sum(d$unclaimed > 0, na.rm = TRUE)))
cat(sprintf("slowest            %.1fs (%s)\n", max(d$secs, na.rm = TRUE),
            d$file[which.max(d$secs)]))
cat(sprintf("report             %s\n", out_csv))
if (sum(nzchar(d$problem))) {
  cat("\n---- crashes ----\n")
  bad <- d[nzchar(d$problem), c("file", "problem"), drop = FALSE]
  for (j in seq_len(nrow(bad))) cat(sprintf("  %-52s %s\n", bad$file[j], bad$problem[j]))
}
