# batch_audit.R -- review MANY statements at once (a whole folder of 250+ PDFs of
# every bank/variant) and produce a SINGLE, PII-safe picture: how many the reader
# proves on its own, which do not, the ones it cannot read CLUSTERED by layout,
# and which of its checks fail most -- what the reader would need to cover the
# rest. No PII: only shapes, counts, institution ids and layout hashes.

# .kind_of(input) -- a coarse, safe descriptor of the file kind.
.kind_of <- function(input) {
  if (identical(input$kind, "pdf"))
    return(if ((input$meta$ocr_pages %||% 0L) > 0L) "pdf-scanned" else "pdf-text")
  input$kind %||% "?"
}

# batch_audit(paths, layouts_dir) -> list(per_file, clusters, feature_gaps).
# Safe to share.
#
# A PICTURE, NOT A CONVERSION RUN -- which is why it calls the reader (bank_identify
# + auto_read) rather than convert_statement(). A conversion writes a workbook, a
# run-log record and a tracking line per file, and LEARNS; for a 250-file folder
# that is 250 spurious conversions on disk and layouts learned from a pile nobody
# chose to train on. The reader reaches the same verdict with no side effects.
# `layouts_dir`, when given, is read (never written), so the picture shows what
# the learned layouts already cover.
batch_audit <- function(paths, layouts_dir = NULL) {
  paths <- as.character(paths)
  rows <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    p <- paths[i]
    input <- safe(read_input(p), NULL)
    if (is.null(input)) {
      rows[[i]] <- data.frame(idx = i, file_type = tolower(tools::file_ext(p)), kind = "unreadable",
        pages = NA_integer_, bank = NA_character_, outcome = "unreadable", layout = NA_character_,
        checks_failed = NA_character_, n_rows = 0L, n_periods = NA, n_accounts = NA,
        amount_style = NA_character_, date_format = NA_character_,
        signature = NA_character_, layout_hint = "", stringsAsFactors = FALSE)
      next
    }
    meta <- safe(extract_metadata(input), list())
    lsig <- safe(layout_signature(input), list(signature = NA_character_, hint = ""))
    bank <- bank_pick(bank_identify(input), NULL)$bank
    layouts <- if (!is.null(layouts_dir) && !is.na(bank)) layouts_load(layouts_dir, bank) else list()
    rd <- auto_read(input, layouts, bank)
    sig <- rd$template$signature %||% list()
    ck <- rd$checks
    rows[[i]] <- data.frame(idx = i, file_type = tolower(tools::file_ext(p)),
      kind = .kind_of(input), pages = input$meta$page_count %||% NA_integer_,
      bank = bank, outcome = rd$outcome %||% "unread",
      layout = rd$matched_layout %||% NA_character_,
      checks_failed = if (is.data.frame(ck) && any(ck$ok %in% FALSE)) paste(ck$check[ck$ok %in% FALSE], collapse = ",") else NA_character_,
      n_rows = nrow(rd$transactions %||% data.frame()),
      n_periods = meta$n_periods %||% NA, n_accounts = meta$n_accounts %||% NA,
      amount_style = sig$money_style %||% NA_character_, date_format = sig$date_format %||% NA_character_,
      signature = lsig$signature %||% NA_character_,
      layout_hint = .layout_hint_safe(lsig$hint, input$kind), stringsAsFactors = FALSE)
  }
  per <- do.call(rbind, rows)

  # The files the reader could not read, clustered by layout: the biggest gap first.
  uns <- per[per$outcome %in% c("unread", "unreadable"), , drop = FALSE]
  clusters <- data.frame()
  sig_ok <- uns[!is.na(uns$signature), , drop = FALSE]
  if (nrow(sig_ok)) {
    tab <- sort(table(sig_ok$signature), decreasing = TRUE)
    clusters <- do.call(rbind, lapply(names(tab), function(s) {
      ex <- sig_ok[sig_ok$signature == s, , drop = FALSE][1, ]
      data.frame(signature = s, count = as.integer(tab[[s]]), example_idx = ex$idx,
                 kind = ex$kind, layout_hint = ex$layout_hint, stringsAsFactors = FALSE)
    }))
  }

  tallyNA <- function(x) { x <- x[!is.na(x) & nzchar(as.character(x))]; if (!length(x)) list() else as.list(sort(table(x), decreasing = TRUE)) }
  feature_gaps <- list(
    total = nrow(per),
    by_outcome = as.list(table(per$outcome)),
    by_kind = as.list(table(per$kind)),
    amount_styles = tallyNA(per$amount_style),
    date_formats = tallyNA(per$date_format),
    banks = tallyNA(per$bank),
    checks_failed = tallyNA(unlist(strsplit(per$checks_failed[!is.na(per$checks_failed)], ","))),
    scanned = sum(grepl("scanned", per$kind)),
    multi_account = sum(per$n_accounts > 1, na.rm = TRUE),
    multi_period = sum(per$n_periods > 1, na.rm = TRUE),
    unread = nrow(uns), distinct_gap_layouts = nrow(clusters))

  list(per_file = per, clusters = clusters, feature_gaps = feature_gaps)
}

# format_batch_audit(b) -> a safe-to-share markdown report of the whole batch.
format_batch_audit <- function(b) {
  L <- c(); add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  g <- b$feature_gaps
  tl <- function(x) paste(sprintf("%s(%s)", names(x), x), collapse = ", ")
  add("# Bulk statement audit (safe to share - no PII)\n")
  add(sprintf("**%d statements.** Reader outcome: %s.", g$total,
      paste(sprintf("%s=%s", names(g$by_outcome), g$by_outcome), collapse = ", ")))
  add(sprintf("Kinds: %s.", paste(sprintf("%s=%s", names(g$by_kind), g$by_kind), collapse = ", ")))
  add(sprintf("Scanned (OCR): %d &middot; multi-account: %d &middot; multi-period: %d",
      g$scanned, g$multi_account, g$multi_period))
  add("\n## What was read")
  add(sprintf("- amount styles seen: %s", tl(g$amount_styles)))
  add(sprintf("- date formats seen: %s", tl(g$date_formats)))
  add(sprintf("- banks identified: %s", tl(g$banks)))
  if (length(g$checks_failed)) add(sprintf("- checks that stopped a proof: %s", tl(g$checks_failed)))
  add(sprintf("\n## The gaps - %d not read across %d distinct layouts", g$unread, g$distinct_gap_layouts))
  if (nrow(b$clusters)) {
    add("```")
    add(paste(capture.output(print(b$clusters[, c("count", "kind", "layout_hint", "signature")], row.names = FALSE)), collapse = "\n"))
    add("```")
  }
  paste(unlist(L), collapse = "\n")
}
