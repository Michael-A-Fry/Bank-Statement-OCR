# schema.R -- canonical core schema, constructors, and coercion helpers.
# The core `transactions` table is STABLE and identical across every bank.

# Ordered core columns and their storage types (see build-contract section 2).
CORE_COLUMNS <- c(
  "row_id", "date", "date_raw", "description", "amount", "amount_raw",
  "direction", "balance", "balance_raw", "particulars", "code", "reference",
  "other_party", "type", "currency", "flags"
)

CORE_TYPES <- c(
  row_id = "integer", date = "character", date_raw = "character",
  description = "character", amount = "numeric", amount_raw = "character",
  direction = "character", balance = "numeric", balance_raw = "character",
  particulars = "character", code = "character", reference = "character",
  other_party = "character", type = "character", currency = "character",
  flags = "character"
)

# new_transactions() -- an empty core data.frame with the exact 16 columns/types.
new_transactions <- function() {
  data.frame(
    row_id      = integer(0),
    date        = character(0),
    date_raw    = character(0),
    description = character(0),
    amount      = numeric(0),
    amount_raw  = character(0),
    direction   = character(0),
    balance     = numeric(0),
    balance_raw = character(0),
    particulars = character(0),
    code        = character(0),
    reference   = character(0),
    other_party = character(0),
    type        = character(0),
    currency    = character(0),
    flags       = character(0),
    stringsAsFactors = FALSE
  )
}

# coerce_core(df) -- enforce column presence, order, and type on a core table.
coerce_core <- function(df) {
  if (is.null(df)) df <- new_transactions()
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n <- nrow(df)
  out <- vector("list", length(CORE_COLUMNS))
  names(out) <- CORE_COLUMNS
  for (col in CORE_COLUMNS) {
    v <- if (col %in% names(df)) df[[col]] else rep(NA, n)
    type <- CORE_TYPES[[col]]
    out[[col]] <- switch(type,
      integer   = as.integer(v),
      numeric   = as.numeric(v),
      character = as.character(v)
    )
  }
  res <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  # `flags` is contractually "" (never NA) when a row carries no flags.
  res$flags[is.na(res$flags)] <- ""
  res[, CORE_COLUMNS, drop = FALSE]
}

# new_result() -- the object convert_statement() always returns (section 6).
new_result <- function(status = "failed", template_id = NA_character_,
                       trust = list(level = "low", score = 0, reasons = character(0)),
                       kpis = NULL, header = list(), outputs = character(0),
                       messages = character(0)) {
  list(
    status      = status,
    template_id = template_id,
    trust       = trust,
    kpis        = kpis,
    header      = header,
    outputs     = outputs,
    messages    = messages
  )
}
