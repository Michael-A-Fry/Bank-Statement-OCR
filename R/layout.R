# layout.R -- a stable, PII-light fingerprint of a statement's LAYOUT.
#
# Purpose: when many statements come through, the ones the engine can't yet parse
# should CLUSTER by format, so the admin reports can say "you've had 14 unsupported
# statements that all look the same -> build ONE template and unblock all 14".
#
# The signature is derived from STRUCTURAL text only (column headers / recurring
# labels), which is the same across different customers of the same bank and
# changes between banks -- so same layout => same signature. It is stored as a
# short hash (opaque) plus a short human hint of the top structural tokens; no
# amounts, dates, names or account numbers go into it.

# Kept as the lexicon's default `layout_stopwords` (R/lexicon.R). The signature
# itself no longer needs it: both branches below now key off the header keywords,
# so a word that is not layout vocabulary never reaches the hint to be stopped.
.LAYOUT_STOP <- c("the","and","for","you","your","from","with","this","that","are",
                  "was","been","will","have","has","not","all","any","per","was",
                  "our","their","them","these","those","which","into","only")

# Generic transaction-table column-header words. A statement's header line is the
# most reliable, customer-independent layout fingerprint (names/amounts/dates
# never match these), so the PDF signature keys off whichever line contains the
# most of them.
.HDR_KEYS <- c("date","balance","amount","withdrawal","withdrawals","deposit","deposits",
               "debit","credit","description","details","transaction","transactions",
               "particulars","reference","code","type","payment","payments","memo",
               "narrative","opening","closing","fee","fees","interest")

.str_hash <- function(s) {
  if (requireNamespace("openssl", quietly = TRUE)) return(paste0(openssl::sha256(charToRaw(s))))
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(s, algo = "sha256"))
  sprintf("%d-%d", sum(utf8ToInt(s)) %% .Machine$integer.max, nchar(s))  # last-resort
}

# layout_signature(input) -> list(signature, hint)
layout_signature <- function(input) {
  kind <- input$kind %||% "text"
  toks <- character(0)
  if (identical(kind, "excel")) {
    toks <- tolower(trimws(names(input$table %||% list())))
  } else if (identical(kind, "pdf")) {
    txt <- paste(input$pages %||% character(0), collapse = "\n")
    lines <- tolower(unlist(strsplit(txt, "\n", fixed = TRUE)))
    # the line matching the most header keywords is the table header -> its keys
    # are a stable, customer-independent signature.
    hdr_keys <- lex("header_keywords")
    key_hits <- lapply(lines, function(ln) {
      w <- unlist(regmatches(ln, gregexpr("[a-z]+", ln)))
      sort(unique(w[w %in% hdr_keys]))
    })
    best <- which.max(vapply(key_hits, length, integer(1)))
    if (length(best) && length(key_hits[[best]]) >= 2) {
      toks <- key_hits[[best]]
    } else {
      # THE FALLBACK IS STILL ONLY LAYOUT VOCABULARY.
      #
      # It used to be the twelve commonest four-letter-plus words on the page,
      # minus a stop list -- and on a document with no transaction header, which
      # is exactly the class of document that reaches this branch, the commonest
      # words on the page are the people named on it. Measured on one: the hint
      # came back "ambrose | whitcombe", and that string is written to
      # logs/runs/<run_id>.json AND to logs/metadata/<run_id>.json, both kept
      # after the uploaded file itself is purged, and the metadata module's own
      # header states that names are never stored.
      #
      # So the same vocabulary that keys the branch above keys this one: the
      # header keywords printed ANYWHERE on the page, commonest first. It costs
      # nothing that matters -- the hint exists to CLUSTER layouts, and a surname
      # clusters nothing; two documents of one family share their layout words,
      # not their customers. A page carrying none of them yields no hint at all,
      # which is the honest answer and is the one this function already gives for
      # a page it could read nothing off.
      words <- tolower(unlist(regmatches(txt, gregexpr("[A-Za-z]{3,}", txt))))
      words <- words[words %in% hdr_keys]
      if (length(words)) {
        tab <- sort(table(words), decreasing = TRUE)
        toks <- names(utils::head(tab, 12))
      }
    }
  } else {                                   # delimited: the header row
    lines <- input$lines %||% character(0)
    nz <- which(nzchar(trimws(lines)))
    hdr <- if (length(nz)) lines[nz[1]] else ""
    toks <- tolower(trimws(gsub('"', "", unlist(strsplit(hdr, "[,\t;|]")))))
  }
  toks <- sort(unique(toks[nzchar(toks)]))
  if (!length(toks)) return(list(signature = "empty", hint = ""))
  hint <- paste(utils::head(toks, 10), collapse = " | ")
  # The join separator is US-ASCII 0x01, written as an OCTAL ESCAPE rather than
  # as the raw control byte it used to be. Same byte, same signature -- but a
  # non-printable character sitting in a source file is invisible in every
  # editor and diff, and does not reliably survive the trip through an email
  # client or a zip on the way to an air-gapped box. Held to that by
  # test-labels.R, which now scans every R/*.R file for bytes outside
  # printable ASCII, literals and comments alike.
  list(signature = substr(.str_hash(paste(toks, collapse = "\001")), 1, 12),
       hint = hint)
}

# ---- a file nothing could be read from --------------------------------------------------
#
# N227. Admin -> Health groups the statements nothing could be read from by their
# layout signature, so one unreadable design is one row to look at. But an unread
# file has little or no signature: a CSV whose heading row was never found has
# none (its first line may be a preamble naming the holder, so it is never used),
# and a PDF with no heading words has "empty". Every such file landed in ONE
# "(unknown)" row, which says nothing about where to start.
#
# So an unread file gets a cheap STRUCTURAL fingerprint instead, made from no
# content at all: the kind of file, a page-count band, the software that wrote a
# PDF (letters only: no version number, nothing typed by a person), the shape of
# the columns the reader found (date / figure / text, left to right), how many
# fields a CSV row or a sheet has, and the heading words when there were any
# (layout_signature() above, structural words only). Two files of one design land
# together; a scanned letter and a 40-page spreadsheet export do not.

.FP_KIND_PLAIN <- c(pdf = "PDF", scan = "scanned PDF", delimited = "CSV", excel = "Excel")

# .pages_band(n) -- a page count as a band, so one design's short and long
# statements group together.
.pages_band <- function(n) {
  n <- suppressWarnings(as.integer(n))
  ifelse(is.na(n) | n < 1L, NA_character_,
         ifelse(n == 1L, "1 page", ifelse(n <= 3L, "2-3 pages", ifelse(n <= 10L, "4-10 pages", "over 10 pages"))))
}

# file_shape_label(kind, pages) -- "PDF, 4-10 pages": the shape the run log
# itself carries about a file (vectorised). The page band is said only for a PDF.
file_shape_label <- function(kind, pages) {
  kind <- as.character(kind)
  k <- ifelse(!is.na(kind) & kind %in% names(.FP_KIND_PLAIN), .FP_KIND_PLAIN[kind], "a file of unknown kind")
  pb <- ifelse(!is.na(kind) & kind %in% c("pdf", "scan"), .pages_band(pages), NA_character_)
  unname(ifelse(is.na(pb), k, paste0(k, ", ", pb)))
}

# .producer_words(p) -- the PDF's own "made by" field reduced to its first three
# words of letters ("Skia/PDF m141" -> "skia pdf").
.producer_words <- function(p) {
  p <- tolower(as.character(p %||% NA_character_)[1])
  if (is.na(p)) return(NA_character_)
  w <- regmatches(p, gregexpr("[a-z]+", p))[[1]]
  w <- w[nchar(w) >= 2L]
  if (length(w)) paste(utils::head(w, 3L), collapse = " ") else NA_character_
}

# .column_shape(columns) -- the columns the reader found on the first page that
# had any, left to right, as date / figure / text.
.column_shape <- function(columns) {
  for (cl in columns) {
    if (!is.data.frame(cl) || !nrow(cl) || !all(c("page", "kind", "x_min") %in% names(cl))) next
    p <- cl[cl$page == min(cl$page, na.rm = TRUE), , drop = FALSE]
    p <- p[order(p$x_min), , drop = FALSE]
    return(paste(ifelse(p$kind == "date", "date", ifelse(p$kind == "money", "figure", "text")), collapse = " "))
  }
  NA_character_
}

# .field_count(input) -- how many fields a CSV row (the commonest count, on the
# separator used most) or a sheet has. A count, never a value.
.field_count <- function(input) {
  if (identical(input$kind, "excel")) {
    t <- input$table
    if (!is.data.frame(t) || !ncol(t)) return(NA_integer_)
    used <- vapply(t, function(v) any(!is.na(v) & nzchar(trimws(as.character(v)))), NA)
    return(sum(used))
  }
  ln <- as.character(input$lines %||% character(0))
  ln <- ln[!is.na(ln) & nzchar(trimws(ln))]
  if (!length(ln)) return(NA_integer_)
  seps <- c(",", ";", "\t", "|")
  n <- vapply(seps, function(s) sum(lengths(regmatches(ln, gregexpr(s, ln, fixed = TRUE)))), 0)
  if (!any(n > 0)) return(1L)
  per <- lengths(regmatches(ln, gregexpr(seps[which.max(n)], ln, fixed = TRUE))) + 1L
  as.integer(names(sort(table(per), decreasing = TRUE))[1])
}

# unread_fingerprint(input, kind, pages, columns, base) -> list(signature, hint),
# the same shape as layout_signature(). `kind` is the kind of file as the run log
# records it (a scan is "scan"), `pages` its page count, `columns` the readings'
# found columns (data frames), `base` the file's layout_signature() result.
unread_fingerprint <- function(input, kind = NULL, pages = NA, columns = list(), base = NULL) {
  kind <- as.character(kind %||% input$kind %||% NA_character_)[1]
  heads <- as.character(base$signature %||% NA_character_)[1]
  if (!is.na(heads) && (!nzchar(heads) || heads == "empty")) heads <- NA_character_
  tab <- !is.na(kind) && kind %in% c("delimited", "excel")
  parts <- c(kind = kind,
             pages = if (!is.na(kind) && kind %in% c("pdf", "scan")) .pages_band(pages) else NA_character_,
             producer = if (tab) NA_character_ else .producer_words(input$meta$pdf_doc$producer),
             columns = .column_shape(columns),
             fields = if (tab) as.character(.field_count(input)) else NA_character_,
             headings = heads)
  said <- c(file_shape_label(kind, pages),
            if (!is.na(parts[["producer"]])) paste("made by", parts[["producer"]]),
            if (!is.na(parts[["fields"]])) paste(parts[["fields"]], if (identical(kind, "excel")) "columns" else "fields a row"),
            if (!is.na(parts[["columns"]])) paste("columns found:", parts[["columns"]]) else if (!tab) "no columns found",
            if (!is.na(heads) && nzchar(base$hint %||% "")) paste("headings:", base$hint))
  key <- paste(names(parts), ifelse(is.na(parts), "-", parts), sep = "=", collapse = "\001")
  list(signature = substr(.str_hash(key), 1, 12), hint = paste(said, collapse = ", "))
}
