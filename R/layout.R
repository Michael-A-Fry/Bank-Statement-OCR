# layout.R -- a stable, PII-light fingerprint of a statement's LAYOUT, so statements the engine cannot
# parse CLUSTER by format: "14 unsupported statements all look the same - build ONE template".

# Kept as the lexicon's default `layout_stopwords`; both branches now key off the header keywords.
.LAYOUT_STOP <- c("the","and","for","you","your","from","with","this","that","are",
                  "was","been","will","have","has","not","all","any","per","was",
                  "our","their","them","these","those","which","into","only")

# Generic column-header words. A statement's header line is the most reliable customer-independent
# layout fingerprint, so the PDF signature keys off whichever line carries the most of them.
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
    # The line matching the most header keywords is the table header, so its keys are a stable signature.
    hdr_keys <- lex("header_keywords")
    key_hits <- lapply(lines, function(ln) {
      w <- unlist(regmatches(ln, gregexpr("[a-z]+", ln)))
      sort(unique(w[w %in% hdr_keys]))
    })
    best <- which.max(vapply(key_hits, length, integer(1)))
    if (length(best) && length(key_hits[[best]]) >= 2) {
      toks <- key_hits[[best]]
    } else {
      # THE FALLBACK IS STILL ONLY LAYOUT VOCABULARY. It used to be the twelve commonest long words on
      # the page, and on a document with no transaction header those are the people named on it: the
      # hint came back as two surnames, into logs kept after the upload is purged. No hint is honest.
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
  # The join separator is US-ASCII 0x01 as an OCTAL ESCAPE, not the raw control byte: a non-printable
  # character is invisible in every editor and diff and may not survive the trip to an air-gapped box.
  list(signature = substr(.str_hash(paste(toks, collapse = "\001")), 1, 12),
       hint = hint)
}
