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
    key_hits <- lapply(lines, function(ln) {
      w <- unlist(regmatches(ln, gregexpr("[a-z]+", ln)))
      sort(unique(w[w %in% .HDR_KEYS]))
    })
    best <- which.max(vapply(key_hits, length, integer(1)))
    if (length(best) && length(key_hits[[best]]) >= 2) {
      toks <- key_hits[[best]]
    } else {
      # fallback: recurring structural words (multi-page headers repeat)
      words <- tolower(unlist(regmatches(txt, gregexpr("[A-Za-z]{4,}", txt))))
      words <- words[!words %in% .LAYOUT_STOP]
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
  list(signature = substr(.str_hash(paste(toks, collapse = "")), 1, 12),
       hint = hint)
}
