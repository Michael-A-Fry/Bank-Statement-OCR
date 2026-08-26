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
