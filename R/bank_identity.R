# bank_identity.R -- which bank issued this statement (spec section 5, Appendix A2).
#
# bank_identify(input) reads the statement itself and says which institution
# issued it, how sure it is, and why. bank_pick(identified, chosen) turns that and
# the person's own pick into the bank to use, whether to ask, and whether learning
# must wait for a person to confirm.
#
# The order of trust, from the research note (bank_identity.md, section 5):
#   1. The HOLDER's own account number, looked up in the Payments NZ bank branch
#      register (bank + branch) and checked with IRD's check digits. The bank code
#      alone is only a family: 02 is BNZ and also the Co-operative Bank, Wise and
#      others; 03 is Westpac and the banks that clear through it.
#   2. The issuer's legal name, then its website, 0800 number and SWIFT code, then
#      masthead brand words.
# Everyone else's account numbers are everywhere on a statement (transfers, "Other
# Party" columns, "how to pay" boxes) and bank names are everywhere in the
# transactions (ANZ HOME LOAN, ASB Showgrounds). So a number counts only in the
# holder position (labelled, outside the transaction rows, never after To/From),
# and a bank name counts only outside the transaction table.
#
# PRIVACY. The account number is parsed, looked up and dropped inside this file.
# Nothing returned, logged or stored carries it, its branch or a hash of it (a
# branch has ~10^7 bodies, so a hash is reversible): only the two-digit bank code
# and the institution leave.
#
# Never throws: a failure is an "unknown" result that says so.

.BANK_ID_CACHE <- new.env(parent = emptyenv())

# Evidence strengths (research section 5.2). One number per kind, so the scale is
# readable in one place.
.BI_STRENGTH <- c(
  account = 100,                 # register hit + check digits valid
  account_unverified = 80,       # register hit; check digits not applicable / body masked
  account_citibank = 60,         # code 31: IRD's algorithm X passes every number
  account_checksum_failed = 50,  # register hit, but the check digits do not add up
  account_repaired = 50,         # bank code looked misread; the branch says which code
  account_code_masked = 50,      # bank code visible, branch masked or anonymised
  account_code = 40,             # bank code of a number whose branch is not in the register
  account_code_doubtful = 20,    # bank code of a number whose branch the register gives another code
  legal_name = 80, legal_name_global = 50,
  domain = 60, phone = 60, swift = 60,
  brand = 30)
.BI_ACCOUNT_KINDS <- c("account", "account_unverified", "account_checksum_failed",
                       "account_repaired", "account_code")
.BI_TEXT_KINDS <- c("legal_name", "domain", "phone", "swift", "brand")

# ---------------------------------------------------------------------------
# Reference data: dictionaries/nz_bank_branches.csv and dictionaries/nz_banks.yaml
# ---------------------------------------------------------------------------

.bi_data_dir <- function() {
  cands <- c(if (nzchar(Sys.getenv("ENGINE_ROOT")))
               file.path(Sys.getenv("ENGINE_ROOT"), "dictionaries"),
             "dictionaries")
  ok <- file.exists(file.path(cands, "nz_bank_branches.csv")) &
        file.exists(file.path(cands, "nz_banks.yaml"))
  if (any(ok)) cands[ok][1] else NA_character_
}

# .bi_clean(s) -- page text as plain ASCII, which every later step works on. Dashes
# become "-", bullets and black boxes become the mask "*" ("12-3456-*******-00"),
# accented letters lose their accents, anything else non-ASCII becomes a space.
# Done on iconv's "<xx>" spelling of each byte, because tolower(), chartr() and
# gsub() stop with "invalid multibyte string" on a bullet when R runs in a
# non-UTF-8 locale.
.BI_ASCII_MAP <- local({
  m <- list("-" = c("\u2010", "\u2011", "\u2012", "\u2013", "\u2014", "\u2212"),
            "*" = c("\u2022", "\u25cf", "\u2588", "\u25a0", "\u25aa", "\u00b7"),
            " " = "\u00a0", "a" = c("\u00e4", "\u0101"), "e" = c("\u00e9", "\u00e8", "\u0113"),
            "i" = "\u012b", "o" = c("\u00f6", "\u014d"), "u" = c("\u00fc", "\u016b"),
            "A" = "\u0100", "E" = "\u0112", "I" = "\u012a", "O" = "\u014c", "U" = "\u016a")
  to <- rep(names(m), lengths(m))
  from <- vapply(unlist(m), function(ch)
    paste0("<", as.character(charToRaw(enc2utf8(ch))), ">", collapse = ""), "")
  stats::setNames(to, from)
})
.bi_clean <- function(s) {
  s <- as.character(s)
  if (!length(s)) return(s)
  na <- is.na(s)
  s <- iconv(enc2utf8(s), "UTF-8", "ASCII", sub = "byte")
  s[is.na(s)] <- ""
  for (k in names(.BI_ASCII_MAP)) s <- gsub(k, .BI_ASCII_MAP[[k]], s, fixed = TRUE)
  s <- gsub("(<[0-9a-f]{2}>)+", " ", s)
  s[na] <- NA_character_
  s
}

# .bi_norm(s) -- the form names are compared in: lower case, punctuation to spaces,
# "Ltd" = "Limited", "Co-operative" = "Cooperative", so "ANZ Bank New Zealand Ltd."
# and "ANZ BANK NEW ZEALAND LIMITED" are the same name.
.bi_norm <- function(s) {
  s <- tolower(.bi_clean(s))
  s <- gsub("\\bco-?\\s?op", "coop", s, perl = TRUE)
  s <- gsub("[^a-z0-9&]+", " ", s)
  s <- gsub("\\bltd\\b", "limited", s, perl = TRUE)
  trimws(gsub(" +", " ", s))
}

.bi_digits <- function(s) gsub("[^0-9]", "", s)

# .bi_ref() -> the reference data, cached until either file changes; NULL when the
# files are missing or unreadable.
.bi_ref <- function() {
  dir <- .bi_data_dir()
  if (is.na(dir)) return(NULL)
  bf <- file.path(dir, "nz_bank_branches.csv"); yf <- file.path(dir, "nz_banks.yaml")
  key <- paste(bf, yf, file.mtime(bf), file.mtime(yf))
  if (identical(.BANK_ID_CACHE$key, key)) return(.BANK_ID_CACHE$ref)
  br <- tryCatch(utils::read.csv(bf, colClasses = "character", stringsAsFactors = FALSE),
                 error = function(e) NULL)
  yl <- tryCatch(yaml::read_yaml(yf), error = function(e) NULL)
  inst <- yl$institutions
  if (is.null(br) || !all(c("bank_code", "branch", "institution") %in% names(br)) ||
      !is.list(inst) || !length(inst)) return(NULL)
  ids <- names(inst)
  family <- vapply(ids, function(i) as.character(inst[[i]]$family %||% i), "")
  pseudo <- vapply(ids, function(i) isTRUE(inst[[i]]$pseudo), NA)
  display <- vapply(ids, function(i) as.character(inst[[i]]$display %||% i), "")
  br <- br[br$institution %in% ids, , drop = FALSE]
  # The bank code's family, from its register rows (02 -> bnz, 12 -> asb).
  code_family <- tapply(family[br$institution], br$bank_code,
                        function(v) names(sort(table(v), decreasing = TRUE))[1])
  pat <- list()
  add <- function(kind, id, needle, strength, extra = list())
    pat[[length(pat) + 1L]] <<- c(list(kind = kind, institution = id, needle = needle,
                                       strength = strength), extra)
  for (id in ids) {
    e <- inst[[id]]
    for (ln in e$legal_names %||% list()) {
      nm <- if (is.list(ln)) ln$name else ln
      if (!length(nm) || !nzchar(nm)) next
      add("legal_name", id, .bi_norm(nm),
          .BI_STRENGTH[[if (isTRUE(ln$global)) "legal_name_global" else "legal_name"]],
          list(not_after = .bi_norm(unlist(ln$not_after %||% character(0)))))
    }
    for (b in unlist(e$brand_words %||% character(0)))
      add("brand", id, .bi_norm(b), .BI_STRENGTH[["brand"]], list(not_after = character(0)))
  }
  contact <- function(field, f) {
    rows <- lapply(ids, function(id) {
      v <- unlist(inst[[id]][[field]] %||% character(0))
      if (!length(v)) return(NULL)
      data.frame(value = f(v), institution = id, stringsAsFactors = FALSE)
    })
    do.call(rbind, c(list(data.frame(value = character(0), institution = character(0))), rows))
  }
  ref <- list(
    branch_inst = stats::setNames(br$institution, paste0(br$bank_code, "-", br$branch)),
    branch_code = stats::setNames(br$bank_code, br$branch),
    code_family = code_family,
    family = family, pseudo = pseudo, display = display,
    patterns = pat,
    domains = contact("domains", tolower),
    phones = contact("phones", .bi_digits),
    swift = contact("swift", toupper))
  .BANK_ID_CACHE$key <- key
  .BANK_ID_CACHE$ref <- ref
  ref
}

# ---------------------------------------------------------------------------
# Account numbers: find, check digits, register
# ---------------------------------------------------------------------------

# IRD's allocated branch ranges (RWT/NRWT specification 2020, section 8). Used only
# to know whether a failed check means "invalid" (inside IRD's table) or "unknown"
# (register branches IRD never listed, and codes 05 and 88).
.BI_IRD_RANGES <- list(
  "01" = c(1, 999, 1100, 1199, 1800, 1899), "02" = c(1, 999, 1200, 1299),
  "03" = c(1, 999, 1300, 1399, 1500, 1599, 1700, 1799, 1900, 1999, 7350, 7399),
  "04" = c(2020, 2024), "06" = c(1, 999, 1400, 1499), "08" = c(6500, 6599),
  "09" = c(0, 0), "10" = c(5165, 5169), "11" = c(5000, 6499, 6600, 8999),
  "12" = c(3000, 3299, 3400, 3499, 3600, 3699), "13" = c(4900, 4999),
  "14" = c(4700, 4799), "15" = c(3900, 3999), "16" = c(4400, 4499),
  "17" = c(3300, 3399), "18" = c(3500, 3599), "19" = c(4600, 4649),
  "20" = c(4100, 4199), "21" = c(4800, 4899), "22" = c(4000, 4049),
  "23" = c(3700, 3799), "24" = c(4300, 4349), "25" = c(2500, 2599),
  "26" = c(2600, 2699), "27" = c(3800, 3849), "28" = c(2100, 2149),
  "29" = c(2150, 2299), "30" = c(2900, 2949), "31" = c(2800, 2849),
  "33" = c(6700, 6799), "35" = c(2400, 2499), "38" = c(9000, 9499))

.bi_in_ird <- function(code, branch) {
  r <- .BI_IRD_RANGES[[code]]
  if (is.null(r)) return(FALSE)
  b <- as.integer(branch)
  any(b >= r[c(TRUE, FALSE)] & b <= r[c(FALSE, TRUE)])
}

# nz_account_checksum(code, branch, base, suffix) -> TRUE (valid), FALSE (invalid)
# or NA (cannot say). IRD RWT/NRWT specification, section 8. The bank code carries
# weight 0 in every algorithm the banks use, so a pass says "a real NZ account
# number", never "this bank": the register says which bank. A failure outside
# IRD's table is NA, because IRD's table is older than the register.
nz_account_checksum <- function(code, branch, base, suffix) {
  if (!all(grepl("^[0-9]+$", c(code, branch, base, suffix)))) return(NA)
  base8 <- formatC(as.numeric(base), width = 8, flag = "0", format = "f", digits = 0)
  suf4 <- formatC(as.numeric(suffix), width = 4, flag = "0", format = "f", digits = 0)
  if (nchar(base8) != 8 || nchar(suf4) != 4) return(NA)
  if (as.numeric(base8) == 0) return(FALSE)
  W <- list(
    A = c(0,0, 6,3,7,9, 0,0,10,5,8,4,2,1, 0,0,0,0), B = c(0,0, 0,0,0,0, 0,0,10,5,8,4,2,1, 0,0,0,0),
    D = c(0,0, 0,0,0,0, 0,7,6,5,4,3,2,1, 0,0,0,0),  E = c(0,0, 0,0,0,0, 0,0,0,0,5,4,3,2, 0,0,0,1),
    F = c(0,0, 0,0,0,0, 0,1,7,3,1,7,3,1, 0,0,0,0),  G = c(0,0, 0,0,0,0, 0,1,3,7,1,3,7,1, 0,3,7,1))
  MOD <- c(A = 11, B = 11, D = 11, E = 11, F = 10, G = 10)
  fixed <- c("08" = "D", "09" = "E", "25" = "F", "33" = "F", "26" = "G", "28" = "G",
             "29" = "G", "31" = "X")
  alg <- if (code %in% names(fixed)) fixed[[code]] else if (as.numeric(base8) < 990000) "A" else "B"
  if (alg == "X") return(NA)   # code 31 "always verifies": no assurance at all
  d <- as.integer(strsplit(paste0(code, branch, base8, suf4), "")[[1]])
  p <- d * W[[alg]]
  # E and G add the digits of each product, then again (49 -> 13 -> 4). Not p %% 9:
  # that turns 18, 27, ... into 0 where the specification gives 9.
  if (alg %in% c("E", "G")) {
    ds <- function(x) (x %/% 10) + (x %% 10)
    p <- ds(ds(p))
  }
  if (sum(p) %% MOD[[alg]] == 0) TRUE else if (.bi_in_ird(code, branch)) FALSE else NA
}

# Digits OCR commonly swaps. Only a bank code differing from the register's by one
# of these is repaired.
.BI_OCR_PAIRS <- c("08", "06", "09", "17", "38", "56", "58", "68", "89")

.bi_ocr_pair <- function(a, b) {
  da <- strsplit(a, "")[[1]]; db <- strsplit(b, "")[[1]]
  diff <- which(da != db)
  if (length(diff) != 1) return(FALSE)
  paste(sort(c(da[diff], db[diff])), collapse = "") %in% .BI_OCR_PAIRS
}

.BI_SEP <- "[ -]{1,2}"
.BI_ACCT_RX <- paste0(
  "(?<![0-9A-Za-z])([0-9OoIlSB]{2})", .BI_SEP,
  "([0-9OoIlSB]{4}|[Xx*]{4}|[0-9]{2}[Xx*]{2})", .BI_SEP,
  "([0-9OoIlSBXx*]{7,8})", .BI_SEP,
  "([0-9OoIlSB]{2,4})(?![0-9A-Za-z])")
# OCR sometimes breaks the body in two ("15-9666-1484 100-022"). Only with dashes
# round the body, so a space-separated number is never split in the wrong place.
.BI_ACCT_SPLIT_RX <- paste0(
  "(?<![0-9A-Za-z])([0-9OoIlSB]{2})-([0-9OoIlSB]{4})-",
  "([0-9OoIlSB]{2,6} [0-9OoIlSB]{1,6})-([0-9OoIlSB]{2,4})(?![0-9A-Za-z])")

# .bi_find_accounts(s) -> data.frame(start, end, code, branch, base, suffix): every
# NZ-account-shaped run in one string. Letters OCR mistakes for digits are mapped
# back only inside such a run, and only a few of them.
.bi_find_accounts <- function(s) {
  none <- data.frame(start = integer(0), end = integer(0), code = character(0),
                     branch = character(0), base = character(0), suffix = character(0),
                     stringsAsFactors = FALSE)
  if (length(s) != 1 || is.na(s) || !nzchar(s)) return(none)
  s <- .bi_clean(s)
  m <- gregexpr(.BI_ACCT_RX, s, perl = TRUE)[[1]]
  if (m[1] < 0) {
    m <- gregexpr(.BI_ACCT_SPLIT_RX, s, perl = TRUE)[[1]]
    if (m[1] < 0) return(none)
  }
  cs <- attr(m, "capture.start"); cl <- attr(m, "capture.length")
  out <- lapply(seq_along(m), function(k) {
    part <- substring(s, cs[k, ], cs[k, ] + cl[k, ] - 1L)
    part[3] <- gsub(" ", "", part[3], fixed = TRUE)
    if (!nchar(part[3]) %in% 7:8) return(NULL)
    letters <- nchar(gsub("[^OoIlSB]", "", paste(part, collapse = "")))
    if (letters > 2) return(NULL)
    part <- chartr("OoIlSB", "001158", part)
    data.frame(start = as.integer(m[k]), end = as.integer(m[k] + attr(m, "match.length")[k] - 1L),
               code = part[1], branch = part[2], base = part[3], suffix = part[4],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(none), out))
  rownames(out) <- NULL
  out
}

# .bi_account_evidence(code, branch, base, suffix, ref) -> one evidence row (kind,
# institution, strength, code) or NULL. The number's digits stop here.
.bi_account_evidence <- function(code, branch, base, suffix, ref) {
  # Strength by the specific case; kind by what the reason will say.
  shown <- c(account_code_masked = "account_code", account_code_doubtful = "account_code",
             account_citibank = "account_unverified")
  row <- function(kind, inst, code)
    data.frame(kind = if (kind %in% names(shown)) shown[[kind]] else kind, institution = inst,
               strength = .BI_STRENGTH[[kind]], code = code, stringsAsFactors = FALSE)
  fam_of_code <- function(cd) if (cd %in% names(ref$code_family)) ref$code_family[[cd]] else NA_character_
  branch_masked <- !grepl("^[0-9]{4}$", branch)
  body_masked <- !grepl("^[0-9]+$", base)
  # Placeholder numbers from guides and test files (11-1111-1111111-00,
  # 02-1300-1234567-00) are nobody's account: not even their code says anything.
  # Judged by the body alone, since real branches include 1111 and 3456.
  b <- sub("^0+", "", base)
  if (!body_masked && (grepl("^([0-9])\\1*$", b) || grepl(b, "01234567890123456789", fixed = TRUE)))
    return(NULL)
  if (branch_masked) {
    f <- fam_of_code(code)
    return(if (is.na(f)) NULL else row("account_code_masked", f, code))
  }
  hit <- ref$branch_inst[paste0(code, "-", branch)]
  if (!is.na(hit)) {
    if (isTRUE(ref$pseudo[[hit]])) return(row("account_code_masked", ref$family[[hit]], code))
    if (code == "31") return(row("account_citibank", hit, code))
    if (body_masked) return(row("account_unverified", hit, code))
    ok <- nz_account_checksum(code, branch, base, suffix)
    kind <- if (isTRUE(ok)) "account" else if (is.na(ok)) "account_unverified" else "account_checksum_failed"
    return(row(kind, hit, code))
  }
  # Not in the register under this code. No branch number is shared by two codes
  # (checked in every register snapshot 2019-2026), so if the branch exists under
  # exactly one other code that is one OCR slip away, and the printed code could
  # never have this branch (outside IRD's ranges for it, so not merely a branch
  # newer than our register), that other code is the reading.
  other <- ref$branch_code[branch]
  if (!is.na(other) && .bi_ocr_pair(code, other) && !.bi_in_ird(code, branch) &&
      (body_masked || !isFALSE(nz_account_checksum(other, branch, base, suffix)))) {
    inst <- ref$branch_inst[[paste0(other, "-", branch)]]
    if (isTRUE(ref$pseudo[[inst]])) return(row("account_code", ref$family[[inst]], other))
    return(row("account_repaired", inst, other))
  }
  # A branch the register gives to another code, beyond repair: the code or the
  # branch is misread (38 read as 88), so the code is barely evidence.
  f <- fam_of_code(code)
  if (is.na(f)) NULL else row(if (is.na(other)) "account_code" else "account_code_doubtful", f, code)
}

# ---------------------------------------------------------------------------
# Where things are on the page: lines, cells, zones
# ---------------------------------------------------------------------------

.BI_MONTHS <- "(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)[a-z]*"
.BI_DATE_RX <- paste0(
  "\\b\\d{1,2}[/.-]\\d{1,2}[/.-]\\d{2,4}\\b|\\b\\d{4}[/.-]\\d{1,2}[/.-]\\d{1,2}\\b|",
  "\\b\\d{1,2}[ -]", .BI_MONTHS, "\\b|\\b", .BI_MONTHS, " \\d{1,2}\\b|\\b20\\d{2}[01]\\d[0-3]\\d\\b")
.BI_MONEY_RX <- "(?<![0-9.,])\\(?-?\\$?\\d[0-9,]*\\.\\d{2}(?![0-9%])"
.BI_FIGURE_RX <- "^\\(?[-+]?\\$?[0-9][0-9,]*(\\.[0-9]{2})?\\)?( ?(cr|dr|od))?-?$"
.BI_DATE_START_RX <- paste0("^(", .BI_DATE_RX, ")")
# A label is an account word in the last three words before the number, so an
# OCR'd "Account nurnber" and "Your account number is" still label it.
.BI_LABEL_RX <- "(^| )(account|acct|acc|a/c)[^ ]*( [^ ]{1,8}){0,2}[ :#.-]*$"
# A payment-instruction box ("How to pay", "Ways to pay your card") or a list of
# the holder's payees and standing payments: every account number in it belongs
# to whoever is paid.
.BI_PAYBOX_RX <- paste0(
  "\\b(how to pay|ways to pay|to pay (your|this|us|by|online|the)|pay (by|online|using|into|us)\\b|",
  "payment (options|instructions|methods|slip|advice)|remittance|make a payment|",
  "making (a )?payments?|paying (your|by|us)\\b|",
  "(automatic|scheduled|regular|future|recurring|bill) payments?\\b|direct (debits?|credits?)\\b|",
  "standing orders?\\b|payees\\b)")
# Words that make a labelled number someone else's: a payment's other party, or
# the account a loan or card is repaid from ("Direct debit account", "Linked
# account", "Nominated account"), which is usually at another bank.
.BI_CUE_RX <- paste0("\\b(to|from|tfr|trf|transfer|xfer|payee|payer|other|party|pay|paid|",
                     "payment|payments|ap|dd|dc|bp|ft|ref|reference|particulars|",
                     "originating|via|debit|debited|credited|direct|linked|link|nominated|",
                     "funding|repayment|repayments|repay|settlement|drawn|beneficiary)\\b")

# .bi_tail(s, n) -- the last n words of s, lower case.
.bi_tail <- function(s, n = 6L) {
  w <- strsplit(trimws(tolower(s)), "\\s+")[[1]]
  w <- w[nzchar(w)]
  paste(utils::tail(w, n), collapse = " ")
}

# .bi_lines_words(w) -> list of lines (top to bottom), each list(y, h, text,
# cells = data.frame(x0, x1, text)). Words share a line when their centres are
# within half a word height; a gap wider than a word height starts a new cell, so
# "Account number        01-..." is a label cell and a value cell.
.bi_lines_words <- function(w) {
  if (is.null(w) || !NROW(w)) return(list())
  txt <- trimws(.bi_clean(w$text))
  keep <- !is.na(txt) & nzchar(txt)
  w <- w[keep, , drop = FALSE]; txt <- txt[keep]
  if (!nrow(w)) return(list())
  x <- as.numeric(w$x); y <- as.numeric(w$y); wd <- as.numeric(w$width); h <- as.numeric(w$height)
  mh <- stats::median(h[is.finite(h) & h > 0]); if (!is.finite(mh)) mh <- 8
  h[!is.finite(h) | h <= 0] <- mh
  wd[!is.finite(wd) | wd < 0] <- 0
  yc <- y + h / 2
  o <- order(yc, x)
  id <- integer(length(o)); cur <- 0L; anchor <- -Inf
  for (i in o) {
    if (yc[i] - anchor > 0.5 * mh) { cur <- cur + 1L; anchor <- yc[i] }
    id[i] <- cur
  }
  lapply(seq_len(cur), function(k) {
    j <- which(id == k); j <- j[order(x[j])]
    gap <- c(Inf, x[j][-1] - (x[j] + wd[j])[-length(j)])
    cell <- cumsum(gap > max(stats::median(h[j]), 1))
    cells <- data.frame(
      x0 = tapply(x[j], cell, min), x1 = tapply(x[j] + wd[j], cell, max),
      text = tapply(txt[j], cell, paste, collapse = " "), stringsAsFactors = FALSE)
    list(y = min(y[j]), h = stats::median(h[j]), cells = cells,
         text = paste(cells$text, collapse = " "))
  })
}

# .bi_lines_text(s) -- the same shape from plain page text (no word boxes): one line
# per text line, cells split at runs of 3+ spaces, positions in characters.
.bi_lines_text <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(list())
  ln <- strsplit(.bi_clean(s), "\r?\n")[[1]]
  out <- list()
  for (k in seq_along(ln)) {
    if (!nzchar(trimws(ln[k]))) next
    m <- gregexpr("\\S+( {1,2}\\S+)*", ln[k])[[1]]
    tx <- regmatches(ln[k], list(m))[[1]]
    cells <- data.frame(x0 = as.numeric(m), x1 = as.numeric(m + attr(m, "match.length")),
                        text = tx, stringsAsFactors = FALSE)
    out[[length(out) + 1L]] <- list(y = k, h = 1, cells = cells,
                                    text = paste(tx, collapse = " "))
  }
  out
}

# .bi_zones(lines, page_h) -> character zone per line: "masthead", "header",
# "table", "footer" or "body". The table runs from the first transaction-shaped
# line (a date and an amount) that has another within four lines, to the last such
# line, plus wrapped lines directly under it. A page with no table has a header
# (top 30%), a footer (bottom 20%) and a body.
.bi_zones <- function(lines, page_h) {
  n <- length(lines)
  if (!n) return(character(0))
  y <- vapply(lines, function(l) l$y, 0)
  txt <- vapply(lines, function(l) l$text, "")
  if (!is.finite(page_h) || page_h <= 0) page_h <- max(y) + 1
  tx <- grepl(.BI_DATE_RX, tolower(txt), perl = TRUE) & grepl(.BI_MONEY_RX, txt, perl = TRUE)
  idx <- which(tx)
  near <- function(i, dir) any(idx != i & (idx - i) * dir > 0 & abs(idx - i) <= 4)
  st <- idx[vapply(idx, near, NA, dir = 1)][1]
  en <- utils::tail(idx[vapply(idx, near, NA, dir = -1)], 1)
  top <- y <= 0.3 * page_h
  if (is.na(st) || !length(en) || en < st) {
    z <- ifelse(top, "masthead", ifelse(y >= 0.8 * page_h, "footer", "body"))
    return(z)
  }
  pitch <- if (en > st) stats::median(diff(y[st:en])) else lines[[st]]$h * 1.2
  while (en < n && y[en + 1] - y[en] <= 1.6 * pitch) en <- en + 1L
  z <- rep("table", n)
  z[seq_len(st - 1L)] <- ifelse(top[seq_len(st - 1L)], "masthead", "header")
  if (en < n) z[(en + 1L):n] <- "footer"
  z
}

# ---------------------------------------------------------------------------
# Evidence collectors
# ---------------------------------------------------------------------------

.bi_ev <- function(kind = character(0), institution = character(0), strength = numeric(0),
                   zone = character(0), page = integer(0), code = character(0))
  data.frame(kind = kind, institution = institution, strength = strength, zone = zone,
             page = page, code = code, stringsAsFactors = FALSE)

# .bi_text_evidence(text, zone, page, ref) -- legal names, domains, phones, SWIFT
# codes and brand words in one line. Brand words count only in the masthead, the
# footer and an export's preamble. A name just after "not" ("Not issued by ...",
# "not guaranteed by ...") says nothing about the issuer.
.bi_text_evidence <- function(text, zone, page, ref) {
  ev <- .bi_ev()
  if (is.na(text) || !nzchar(text)) return(ev)
  hay <- paste0(" ", .bi_norm(text), " ")
  brand_ok <- zone %in% c("masthead", "footer", "preamble")
  # A logo OCR'd letter by letter ("B N Z") is read as the word it spells; only
  # for brand words, which are weak anyway.
  spaced <- paste0(" ", gsub("(?<=\\b[a-z]) (?=[a-z]\\b)", "", trimws(hay), perl = TRUE), " ")
  for (p in ref$patterns) {
    if (p$kind == "brand" && !brand_ok) next
    if (!nzchar(p$needle)) next
    pos <- gregexpr(paste0(" ", p$needle, " "), hay, fixed = TRUE)[[1]]
    if (pos[1] < 0 && p$kind == "brand" && zone == "masthead") {
      hay_b <- spaced
      pos <- gregexpr(paste0(" ", p$needle, " "), hay_b, fixed = TRUE)[[1]]
    } else hay_b <- hay
    if (pos[1] < 0) next
    for (s in pos) {
      before <- strsplit(trimws(substr(hay_b, 1, s)), " ")[[1]]
      if ("not" %in% utils::tail(before, 6)) next
      if (any(utils::tail(before, 1) %in% p$not_after)) next
      ev <- rbind(ev, .bi_ev(p$kind, p$institution, p$strength, zone, page, NA_character_))
      break
    }
  }
  low <- tolower(text)
  for (k in seq_len(nrow(ref$domains))) {
    d <- gsub(".", "\\.", ref$domains$value[k], fixed = TRUE)
    if (grepl(paste0("(^|[^a-z0-9.-])(www\\.)?", d, "($|[^a-z0-9-])"), low, perl = TRUE))
      ev <- rbind(ev, .bi_ev("domain", ref$domains$institution[k], .BI_STRENGTH[["domain"]],
                             zone, page, NA_character_))
  }
  ph <- regmatches(text, gregexpr("\\b0(800|508)([ -]?[0-9]){6,7}\\b", text, perl = TRUE))[[1]]
  for (d in unique(.bi_digits(ph))) {
    hit <- ref$phones$institution[ref$phones$value == d]
    for (h in unique(hit))
      ev <- rbind(ev, .bi_ev("phone", h, .BI_STRENGTH[["phone"]], zone, page, NA_character_))
  }
  for (k in seq_len(nrow(ref$swift))) {
    if (grepl(paste0("\\b", ref$swift$value[k], "(XXX)?\\b"), text, perl = TRUE))
      ev <- rbind(ev, .bi_ev("swift", ref$swift$institution[k], .BI_STRENGTH[["swift"]],
                             zone, page, NA_character_))
  }
  # A bank the list does not know ("Rimu Bank", "Bank of Melbourne", "Tui Credit
  # Union") at the top or foot of the page: the statement may be that bank's, so
  # nothing else on it can make the reading sure. Read field by field, so an
  # export's "Name,Bank,..." heading is not "Name Bank".
  if (brand_ok && !nrow(ev) &&
      any(vapply(strsplit(text, "[,;\t|]")[[1]], function(f) .bi_other_bank(.bi_norm(f)), NA)))
    ev <- .bi_ev("other_bank", NA_character_, 0, zone, page, NA_character_)
  # An Australian BSB, a UK sort code or "plc": the same names ("Westpac Banking
  # Corporation", "TSB Bank plc", "The Co-operative Bank p.l.c.") issue
  # statements outside New Zealand.
  if (grepl(.BI_FOREIGN_RX, low, perl = TRUE))
    ev <- rbind(ev, .bi_ev("foreign", NA_character_, 0, zone, page, NA_character_))
  ev
}

.BI_FOREIGN_RX <- paste0("\\bbsb\\b[ :.#no]*[0-9]{3}[ -]?[0-9]{3}\\b|\\bsort code\\b|",
                         "\\bp\\.?l\\.?c\\b|\\brouting (number|no)\\b")

# Words that, before "bank", make a phrase that names no bank ("your bank",
# "internet bank", "a registered bank", "the Reserve Bank").
.BI_NOT_A_BANK <- c(
  "the", "your", "our", "any", "another", "other", "this", "that", "each", "every", "all", "own",
  "its", "their", "and", "for", "from", "with", "not", "non", "same", "different", "internet",
  "online", "mobile", "phone", "telephone", "digital", "business", "personal", "private",
  "statement", "registered", "reserve", "central", "trading", "clearing", "issuing", "paying",
  "receiving", "sending", "overseas", "foreign", "local", "retail", "commercial", "investment",
  "merchant", "partner", "agency", "member", "savings", "zealand", "new", "food", "data",
  "blood", "piggy", "time", "energy", "xero", "myob")

# .bi_other_bank(hay) -- TRUE when normalised text names a bank by a "<Name>
# Bank" / "Bank of <Name>" / "<Name> Credit Union" / "<Name> Building Society"
# phrase. Called only for lines on which no known bank was found.
.bi_other_bank <- function(hay) {
  w <- strsplit(trimws(hay), " ")[[1]]
  w <- w[nzchar(w)]
  for (i in seq_along(w)) {
    nxt <- paste(w[i + 1:2], collapse = " ")
    lead <- identical(w[i + 1], "bank") || nxt %in% c("credit union", "building society")
    name <- if (lead) w[i] else if (w[i] == "bank" && identical(w[i + 1], "of")) w[i + 2] else NA
    if (is.na(name) || nchar(name) < 3 || grepl("[0-9]", name) || name %in% .BI_NOT_A_BANK) next
    if ("not" %in% utils::tail(w[seq_len(i - 1L)], 6)) next
    return(TRUE)
  }
  FALSE
}

# .bi_holder_numbers(text, zone, page, ref, need_label, prev = "") -- the account
# numbers in one cell that are the HOLDER's. need_label: the words just before the
# number (or the cell before it, `prev`) must name an account. Any payment word
# there (to, from, TFR, other party, pay ...) makes it someone else's number.
.bi_holder_numbers <- function(text, zone, page, ref, need_label = TRUE, prev = "") {
  ev <- .bi_ev()
  acc <- .bi_find_accounts(text)
  if (!nrow(acc)) acc <- .bi_asb_preamble(text)
  for (k in seq_len(nrow(acc))) {
    pre <- .bi_tail(substr(text, 1, acc$start[k] - 1L))
    if (!nzchar(pre)) pre <- .bi_tail(prev)
    else if (grepl("\\b(to|from|pay|payee|payer|other party)[ :]*$", .bi_tail(prev, 3L), perl = TRUE)) next
    if (grepl(.BI_CUE_RX, pre, perl = TRUE)) next
    if (need_label && !grepl(.BI_LABEL_RX, pre, perl = TRUE)) next
    r <- .bi_account_evidence(acc$code[k], acc$branch[k], acc$base[k], acc$suffix[k], ref)
    if (is.null(r)) next
    ev <- rbind(ev, .bi_ev(r$kind, r$institution, r$strength, zone, page, r$code))
  }
  ev
}

# ASB's CSV preamble prints the parts apart: "Bank 12; Branch 3456; Account 7890123-45-00".
.bi_asb_preamble <- function(text) {
  m <- regmatches(text, regexec(paste0(
    "(?i)bank\\W{0,3}([0-9]{2})\\W+branch\\W{0,3}([0-9]{4})\\W+account\\W{0,3}",
    "([0-9]{7,8})[ -]?([0-9]{2,4})"), text, perl = TRUE))[[1]]
  if (length(m) != 5) return(.bi_find_accounts(""))
  data.frame(start = 1L, end = nchar(text), code = m[2], branch = m[3], base = m[4],
             suffix = m[5], stringsAsFactors = FALSE)
}

# .bi_page_evidence(lines, page_h, page, ref) -- one page of a PDF or scan.
.bi_page_evidence <- function(lines, page_h, page, ref) {
  ev <- .bi_ev()
  if (!length(lines)) return(ev)
  zones <- .bi_zones(lines, page_h)
  txt <- vapply(lines, function(l) l$text, "")
  # Transaction-shaped lines, wherever the table finder put them: a payee's name
  # or number on one says nothing about the issuer.
  tx <- grepl(.BI_DATE_RX, tolower(txt), perl = TRUE) & grepl(.BI_MONEY_RX, txt, perl = TRUE)
  # Lines carrying a figure: a money amount, or a cell that is only a number
  # ("100", "1,250 CR"), as rows print amounts without cents.
  figure <- vapply(lines, function(l) grepl(.BI_MONEY_RX, l$text, perl = TRUE) ||
                     any(grepl(.BI_FIGURE_RX, tolower(l$cells$text), perl = TRUE)), NA)
  # A row the table finder missed (amounts without cents, an unusual date): it
  # starts with a date and carries a figure.
  dated <- vapply(lines, function(l) grepl(.BI_DATE_START_RX, tolower(l$cells$text[1]), perl = TRUE), NA)
  y <- vapply(lines, function(l) l$y, 0); lh <- vapply(lines, function(l) l$h, 0)
  # A description wrapped under its row ("ACCOUNT 12-3456-..." under "DIRECT
  # DEBIT"): a table line at row spacing below a row or another such line.
  tl <- which(zones == "table")
  pitch <- if (length(tl) > 1) stats::median(diff(y[tl])) else max(lh, 1) * 1.2
  wrapped <- logical(length(lines))
  for (i in tl[tl > 1])
    wrapped[i] <- !tx[i] && zones[i - 1L] == "table" && (tx[i - 1L] || wrapped[i - 1L]) &&
      y[i] - y[i - 1L] <= 1.6 * pitch
  # A payment-instruction box runs from its heading to the next blank stretch
  # (a gap of more than two lines) or the table, whichever comes first. A heading
  # carries no figure: "Direct debits  123.45" is a line of a summary.
  paybox <- logical(length(lines)); open <- FALSE
  for (i in seq_along(lines)) {
    if (open && (zones[i] == "table" || y[i] - y[i - 1L] > 3 * max(lh[i], 1))) open <- FALSE
    if (zones[i] != "table" && !figure[i] && grepl(.BI_PAYBOX_RX, tolower(txt[i]), perl = TRUE)) open <- TRUE
    paybox[i] <- open
  }
  for (i in seq_along(lines)) {
    z <- zones[i]; cells <- lines[[i]]$cells
    if (z != "table" && !tx[i]) ev <- rbind(ev, .bi_text_evidence(txt[i], z, page, ref))
    if (paybox[i] || (dated[i] && figure[i])) next
    # Inside the table a number counts only on a labelled line with no figure that
    # is not part of a row (an account's own heading in a combined statement).
    # Outside it a summary line can carry a date and a figure beside the holder's
    # labelled number.
    if (z == "table" && (figure[i] || wrapped[i])) next
    above <- if (i > 1) lines[[i - 1L]]$cells else NULL
    for (c in seq_len(nrow(cells))) {
      if (!nrow(.bi_find_accounts(cells$text[c])) &&
          !nrow(.bi_asb_preamble(cells$text[c]))) next
      prev <- if (c > 1) cells$text[c - 1L] else ""
      # A label printed above the number ("Account number" over its value).
      if (!nzchar(prev) && !is.null(above) && nrow(above)) {
        ov <- above$x0 < cells$x1[c] & above$x1 > cells$x0[c]
        if (any(ov)) prev <- above$text[which(ov)[1]]
      }
      ev <- rbind(ev, .bi_holder_numbers(cells$text[c], if (z == "table") "section" else
                                         sub("masthead", "header", z), page, ref,
                                         need_label = TRUE, prev = prev))
    }
  }
  ev
}

.bi_pdf_evidence <- function(input, ref) {
  words <- input$words %||% list()
  pages <- input$pages %||% character(0)
  np <- max(length(words), length(pages))
  ph <- as.numeric(input$page_height %||% numeric(0))
  ev <- .bi_ev()
  for (p in seq_len(np)) {
    w <- if (p <= length(words)) words[[p]] else NULL
    if (!is.null(w) && NROW(w)) {
      lines <- .bi_lines_words(w)
      h <- if (p <= length(ph)) ph[p] else NA_real_
    } else {
      lines <- .bi_lines_text(if (p <= length(pages)) pages[[p]] else NA_character_)
      h <- NA_real_
    }
    ev <- rbind(ev, .bi_page_evidence(lines, h, p, ref))
  }
  ev
}

# Column names that hold the HOLDER's account in an export ("Account number",
# "This Party Account"); never "Other Party Account".
.BI_ACCOUNT_COL_RX <- "^(this party )?(account|acct|a/c)( ?(number|no|num|#))?[ .:]*$"

.bi_luhn <- function(s) {
  d <- rev(as.integer(strsplit(s, "")[[1]]))
  k <- seq_along(d) %% 2 == 0
  d[k] <- d[k] * 2
  d[k] <- d[k] - 9 * (d[k] > 9)
  sum(d) %% 10 == 0
}

# .bi_digit_run(s, ref) -- an account printed as one run of digits, in dashed form,
# or "" when it cannot be read safely. A spreadsheet drops the leading zero of
# "0109020068389000", leaving 15 digits that also read as 10-9020-0683890-00, so
# a 15-digit run is used only when exactly one of its two readings is a register
# branch.
.bi_digit_run <- function(s, ref) {
  # A card number (scheme digit 3-6 and a valid Luhn check) is not an account,
  # though its digits can look like 38-... (NZ accounts carry no Luhn digit).
  if (grepl("^[3-6]", s) && .bi_luhn(s)) return("")
  dash <- function(d) paste(substr(d, 1, 2), substr(d, 3, 6), substr(d, 7, 13), substring(d, 14), sep = "-")
  if (nchar(s) == 16) return(dash(s))
  cand <- c(dash(s), dash(paste0("0", s)))
  hit <- vapply(cand, function(d) {
    p <- strsplit(d, "-")[[1]]
    paste0(p[1], "-", p[2]) %in% names(ref$branch_inst)
  }, NA)
  if (sum(hit) == 1) cand[hit] else ""
}

# .bi_column_evidence(values, ref) -- an export's account column. The holder's
# column repeats one number (or a few, for a multi-account export); a column with
# more than three different numbers is someone else's, whatever its heading says.
.bi_column_evidence <- function(values, ref) {
  if (is.numeric(values)) values <- format(values, scientific = FALSE, trim = TRUE)
  v <- unique(trimws(.bi_clean(values)))
  v <- v[!is.na(v) & nzchar(v)]
  v <- vapply(v, function(s) if (grepl("^[0-9]{15,16}$", s)) .bi_digit_run(s, ref) else s,
              "", USE.NAMES = FALSE)
  v <- v[vapply(v, function(s) nrow(.bi_find_accounts(s)) > 0, NA)]
  if (length(v) > 3) return(.bi_ev())
  do.call(rbind, c(list(.bi_ev()), lapply(v, .bi_holder_numbers, zone = "column", page = 1L,
                                          ref = ref, need_label = FALSE)))
}

.bi_preamble_evidence <- function(preamble, ref) {
  ev <- .bi_ev()
  for (ln in preamble) {
    if (is.na(ln) || !nzchar(trimws(ln))) next
    ev <- rbind(ev, .bi_text_evidence(ln, "preamble", 1L, ref),
                .bi_holder_numbers(ln, "preamble", 1L, ref, need_label = FALSE))
  }
  ev
}

# Quiet: a warning about a line would reach the caller's log, and the line can
# hold account numbers.
.bi_split_fields <- function(ln, delim) {
  f <- tryCatch(suppressWarnings(utils::read.table(
         text = ln, sep = delim, quote = "\"", header = FALSE, colClasses = "character",
         comment.char = "", fill = TRUE, strip.white = TRUE, na.strings = character(0))),
       error = function(e) NULL)
  if (is.null(f) || !nrow(f)) character(0) else as.character(unlist(f[1, ]))
}

.bi_delimited_evidence <- function(input, ref) {
  # A byte-order mark becomes a leading space, which the field splitter strips.
  lines <- .bi_clean(input$lines %||% character(0))
  if (!length(lines)) return(.bi_ev())
  first <- utils::head(lines, 40)
  cnt <- vapply(c(",", "\t", ";", "|"), function(d)
    sum(lengths(regmatches(first, gregexpr(d, first, fixed = TRUE)))), 0)
  delim <- names(cnt)[which.max(cnt)]
  hdr <- NA_integer_
  for (i in seq_along(first)) {
    f <- .bi_split_fields(first[i], delim)
    if (length(f) >= 3 && any(grepl("date|txn dt|processed", tolower(f)))) { hdr <- i; break }
  }
  if (is.na(hdr)) return(.bi_ev())
  ev <- .bi_preamble_evidence(lines[seq_len(hdr - 1L)], ref)
  hf <- tolower(trimws(.bi_split_fields(lines[hdr], delim)))
  col <- which(grepl(.BI_ACCOUNT_COL_RX, hf, perl = TRUE))
  if (length(col)) {
    body <- utils::head(lines[-seq_len(hdr)], 500)
    vals <- vapply(body, function(ln) {
      f <- .bi_split_fields(ln, delim)
      if (length(f) >= col[1]) f[col[1]] else NA_character_
    }, "")
    ev <- rbind(ev, .bi_column_evidence(vals, ref))
  }
  ev
}

.bi_excel_evidence <- function(input, ref) {
  tbl <- input$table
  pre <- .bi_clean(input$meta$preamble %||% character(0))
  # When read_excel_input found no transaction header it hands over the sheet as
  # is: the first row became the column names and the preamble sits in the first
  # rows. Read down to the first row naming a date (the real header), no further.
  if (!length(pre) && is.data.frame(tbl) && ncol(tbl)) {
    nm <- .bi_clean(names(tbl))
    pre <- paste(nm[!grepl("^\\.\\.\\.[0-9]+$|^col[0-9]+$", nm)], collapse = " ")
    hdr_found <- grepl("date", tolower(pre))
    for (r in seq_len(min(30L, nrow(tbl)))) {
      if (hdr_found) break
      cells <- trimws(.bi_clean(unlist(tbl[r, ], use.names = FALSE)))
      cells <- cells[!is.na(cells) & nzchar(cells)]
      if (any(grepl("date", tolower(cells)))) hdr_found <- TRUE else pre <- c(pre, paste(cells, collapse = " "))
    }
    if (!hdr_found) pre <- character(0)
  }
  ev <- .bi_preamble_evidence(pre, ref)
  if (is.data.frame(tbl) && ncol(tbl)) {
    col <- which(grepl(.BI_ACCOUNT_COL_RX, tolower(trimws(.bi_clean(names(tbl)))), perl = TRUE))
    if (length(col)) ev <- rbind(ev, .bi_column_evidence(tbl[[col[1]]], ref))
  }
  ev
}

# ---------------------------------------------------------------------------
# Deciding
# ---------------------------------------------------------------------------

# .bi_groups(ev) -> the strength each independent kind of evidence contributes:
# account (best account row), legal name, contact (website + phone + SWIFT, capped
# at 80 together), brand. Repeats of a kind add nothing.
.bi_groups <- function(ev) {
  mx <- function(k) { s <- ev$strength[ev$kind %in% k]; if (length(s)) max(s) else 0 }
  c(account = mx(.BI_ACCOUNT_KINDS), legal_name = mx("legal_name"),
    contact = min(80, mx("domain") + mx("phone") + mx("swift")), brand = mx("brand"))
}

# Research 5.2 bands. High: decisive, or strong plus another strong or medium
# signal. Medium: one strong signal, or two that agree and add up to a strong one
# (a website and the masthead; a masked number's bank code and the masthead).
# Low: weak signals only, such as a number whose branch is not in the register and
# a masthead. A contradiction caps the result at medium.
.bi_band <- function(g, contradicted) {
  v <- sort(g[g > 0], decreasing = TRUE)
  if (!length(v)) return("unknown")
  high <- v[1] >= 100 || (v[1] >= 80 && length(v) >= 2 && v[2] >= 60)
  medium <- v[1] >= 80 || (length(v) >= 2 && v[1] + v[2] >= 80)
  if (high && !contradicted) "high" else if (high || medium) "medium" else "low"
}

.bi_why_part <- function(kind, inst, ref, code, strength) {
  d <- ref$display[[inst]]
  switch(kind,
    account = sprintf("the account number is %s's in the bank branch register and its check digits are valid", d),
    account_unverified = sprintf("the account number's bank and branch are %s's in the bank branch register", d),
    account_checksum_failed = sprintf("the account number's bank and branch are %s's in the register, though its check digits do not add up", d),
    account_repaired = sprintf("the account number's branch is %s's (its bank code looks misread)", d),
    account_code = sprintf("the account number's bank code %s is %s's%s", code, d,
                           if (strength < .BI_STRENGTH[["account_code_masked"]])
                             " (its branch is not in the register under that code)" else ""),
    legal_name = sprintf("it names %s's legal entity", d),
    domain = sprintf("it shows %s's website", d),
    phone = sprintf("it shows %s's phone number", d),
    swift = sprintf("it shows %s's SWIFT code", d),
    brand = sprintf("its masthead says %s", d),
    "")
}

# .bi_decide(ev, ref) -> list(institution, bank_code, confidence, why, conflict).
# Families first (a bank and the banks that clear through it agree with each
# other), then the most specific institution inside the winning family.
.bi_decide <- function(ev, ref) {
  other <- any(ev$kind == "other_bank")
  foreign <- any(ev$kind == "foreign")
  ev <- ev[!is.na(ev$institution) & ev$strength > 0 & ev$institution %in% names(ref$family), , drop = FALSE]
  masked <- .BI_STRENGTH[["account_code_masked"]]
  # The holder account's bank code: from the strongest number of the bank chosen
  # (a combined statement can show an 01 and an 06 account, both ANZ's).
  code_of <- function(e) {
    e <- e[!is.na(e$code), , drop = FALSE]
    if (!nrow(e)) NA_character_ else e$code[order(-e$strength)][1]
  }
  out <- function(inst, conf, why, conflict = FALSE, code = NA_character_)
    list(institution = inst, confidence = conf, why = why, conflict = conflict, bank_code = code)
  if (!nrow(ev))
    return(out(NA_character_, "unknown", if (other)
      "The statement names a bank that is not in the bank list, and nothing on it identifies a bank that is." else
      "Nothing on the statement identifies the bank: no holder account number and no bank name outside the transactions."))
  if (foreign && !any(ev$kind %in% .BI_ACCOUNT_KINDS))
    return(out(NA_character_, "low",
               "The statement looks like one from outside New Zealand (it shows a BSB, a sort code or a UK company name) and shows no New Zealand account number, so its bank names may not mean the New Zealand banks."))
  all_codes <- unique(ev$code[!is.na(ev$code)])
  any_code <- if (length(all_codes) == 1) all_codes else NA_character_
  ev$family <- unname(ref$family[ev$institution])
  fams <- sort(unique(ev$family))
  fscore <- vapply(fams, function(f) sum(.bi_groups(ev[ev$family == f, ])), 0)
  fmax <- vapply(fams, function(f) max(ev$strength[ev$family == f]), 0)
  top <- fams[order(-fscore, fams)][1]
  rivals <- setdiff(fams, top)
  dname <- function(i) ref$display[[i]]
  if (length(rivals)) {
    r <- rivals[order(-fscore[rivals], rivals)][1]
    if (fscore[[r]] == fscore[[top]] || (fmax[[r]] >= 40 && fmax[[top]] >= 40))
      return(out(NA_character_, "low", sprintf(
        "The statement points to two different banks (%s and %s), so a person must decide.",
        dname(top), dname(r)), conflict = TRUE, code = any_code))
  }
  fe <- ev[ev$family == top, , drop = FALSE]
  fam_code_any <- code_of(fe)
  members <- names(ref$family)[ref$family == top & !ref$pseudo]
  text_score <- function(i) sum(.bi_groups(fe[fe$institution == i & fe$kind %in% .BI_TEXT_KINDS, ]))
  spec_acc <- unique(fe$institution[fe$kind %in% setdiff(.BI_ACCOUNT_KINDS, "account_code")])
  acc_members <- setdiff(spec_acc, top)
  others <- setdiff(members, top)
  ts <- vapply(others, text_score, 0)
  chosen <- NA_character_
  if (length(acc_members) > 1 || (length(acc_members) == 1 && top %in% spec_acc))
    return(out(NA_character_, "low", sprintf(
      "The statement shows account numbers of two banks in %s's family, so a person must decide.",
      dname(top)), conflict = TRUE, code = fam_code_any))
  # An agency bank named on a statement whose account sits in its clearing bank's
  # code: the agency bank issued it (SBS on 03, the Co-operative Bank on 02). When
  # the register puts the number in the clearing bank's own branch, only the agency
  # bank's legal name or contact details outweigh it, never a brand word.
  flip_at <- if (top %in% spec_acc) 60 else 30
  if (length(acc_members) == 1) {
    chosen <- acc_members
    rival_t <- ts[setdiff(names(ts), chosen)]
    if (any(rival_t >= 60))
      return(out(NA_character_, "low", sprintf(
        "The account number is %s's but the statement names %s, so a person must decide.",
        dname(chosen), dname(names(which.max(rival_t)))), conflict = TRUE, code = fam_code_any))
  } else if (length(ts) && max(ts) >= flip_at && max(ts) > text_score(top)) {
    best <- names(ts)[ts == max(ts)]
    if (length(best) > 1)
      return(out(NA_character_, "low", sprintf(
        "The statement names two banks in %s's family, so a person must decide.", dname(top)),
        conflict = TRUE, code = fam_code_any))
    chosen <- best
  } else if (top %in% fe$institution[fe$kind != "account_code"] || length(members) == 1) {
    chosen <- top
  } else if (all(fe$strength < masked)) {
    # A number whose branch is not in the register is a misreading or not a real
    # account (dummy and specimen numbers): its code alone names no bank.
    return(out(NA_character_, "low", sprintf(
      "The account number's bank code is %s's, but its branch is not in the bank branch register under that code, so it may be misread or not a real account number.",
      dname(top)), code = fam_code_any))
  } else {
    return(out(NA_character_, "low", sprintf(
      "Only the account number's bank code is readable, and %s shares it with other banks.",
      dname(top)), code = fam_code_any))
  }
  # What supports the chosen bank: its own evidence, the family's bank code, and
  # (for an agency bank) its clearing bank's account number, which proves the
  # family but not the member, so it counts no more than a bank code.
  own <- fe[fe$institution == chosen, , drop = FALSE]
  fam_code <- fe[fe$kind == "account_code" & fe$institution != chosen, , drop = FALSE]
  clear_acc <- fe[fe$institution != chosen & fe$kind %in% .BI_ACCOUNT_KINDS &
                    fe$kind != "account_code", , drop = FALSE]
  clear_acc$strength <- pmin(clear_acc$strength, .BI_STRENGTH[["account_code"]])
  sup <- rbind(own, fam_code, clear_acc)
  code <- code_of(sup)
  # A number that fails its check digits, or whose branch is not in the register,
  # is misread or not real (specimen numbers are): alone it is only a guess, and
  # names a bank only where its code belongs to one bank alone.
  weak <- sup$kind == "account_checksum_failed" | (sup$kind == "account_code" & sup$strength < masked)
  if (all(weak)) {
    doubtful <- any(sup$strength == .BI_STRENGTH[["account_code_doubtful"]])
    guess <- if (length(members) == 1 && !length(rivals) && !other && !foreign && !doubtful)
      chosen else NA_character_
    lead <- if (is.na(guess)) "" else sprintf("Possibly %s: ", dname(chosen))
    why <- if (any(sup$kind == "account_checksum_failed"))
      sprintf("the account number's bank and branch are %s's, but its check digits do not add up, so it may be misread or not a real account number.", dname(chosen))
    else sprintf("the account number's bank code is %s's, but its branch is not in the bank branch register under that code, so it may be misread or not a real account number.", dname(chosen))
    if (!nzchar(lead)) why <- paste0(toupper(substr(why, 1, 1)), substring(why, 2))
    return(out(guess, "low", paste0(lead, why), code = code))
  }
  contradicted <- length(rivals) > 0 || any(ts[setdiff(names(ts), chosen)] >= 30) || other || foreign
  conf <- .bi_band(.bi_groups(sup), contradicted)
  sup <- sup[order(-sup$strength), , drop = FALSE]
  sup <- sup[!duplicated(ifelse(sup$kind %in% .BI_ACCOUNT_KINDS, "account", sup$kind)), , drop = FALSE]
  parts <- vapply(seq_len(min(2, nrow(sup))), function(k)
    .bi_why_part(sup$kind[k], sup$institution[k], ref, sup$code[k], sup$strength[k]), "")
  why <- sprintf("%s: %s.", dname(chosen), paste(parts[nzchar(parts)], collapse = ", and "))
  if (contradicted) why <- sub("\\.$", if (foreign)
    "; it also shows signs of a statement from outside New Zealand." else if (other && !length(rivals))
    "; a bank that is not in the bank list is also named, outside the transactions." else
    "; another bank is also mentioned, outside the transactions.", why)
  out(chosen, conf, why, code = code)
}

# ---------------------------------------------------------------------------
# Public
# ---------------------------------------------------------------------------

.bi_unknown <- function(why) list(
  institution = NA_character_, bank_code = NA_character_, confidence = "unknown", why = why,
  evidence = data.frame(kind = character(0), institution = character(0),
                        strength = numeric(0), zone = character(0), stringsAsFactors = FALSE),
  display = NA_character_, needs_decision = FALSE,
  pages = data.frame(page = integer(0), institution = character(0), bank_code = character(0),
                     confidence = character(0), stringsAsFactors = FALSE),
  pages_agree = TRUE)

# bank_identify(input) -> list(institution, bank_code, confidence, why, evidence,
#   display, needs_decision, pages, pages_agree).
#   input        a read_input() object (pdf / scan / delimited / excel) or a file path.
#   institution  id from dictionaries/nz_banks.yaml ("anz", "coop", ...) or NA.
#   bank_code    the holder account's two-digit bank code, or NA. Never the number.
#   confidence   "high" | "medium" | "low" | "unknown".
#   evidence     data.frame(kind, institution, strength, zone). Kinds "other_bank"
#                (a bank the list does not know, named at the top or foot of a
#                page) and "foreign" (a BSB, sort code or "plc") carry institution
#                NA and strength 0: they only make a reading less sure.
#   pages        one row per PDF page with its own reading; pages_agree is FALSE
#                when two pages name different banks (a bundle), and then the
#                statement as a whole is left to a person.
#   Warnings are dropped, not passed on: one could quote a line of the statement.
bank_identify <- function(input) {
  tryCatch(suppressWarnings(.bank_identify(input)), error = function(e)
    .bi_unknown("The bank could not be worked out because the file could not be examined."))
}

.bank_identify <- function(input) {
  ref <- .bi_ref()
  if (is.null(ref))
    return(.bi_unknown("The bank list (dictionaries/nz_banks.yaml and nz_bank_branches.csv) could not be read."))
  if (is.character(input) && length(input) == 1) {
    input <- tryCatch(read_input(input), error = function(e) NULL)
    if (is.null(input)) return(.bi_unknown("The file could not be read."))
  }
  if (!is.list(input) || is.null(input$kind))
    return(.bi_unknown("Nothing was given to identify."))
  kind <- as.character(input$kind)[1]
  ev <- switch(kind,
               pdf = , scan = .bi_pdf_evidence(input, ref),
               delimited = .bi_delimited_evidence(input, ref),
               excel = .bi_excel_evidence(input, ref),
               .bi_ev())
  res <- .bi_decide(ev, ref)
  pages <- data.frame(page = integer(0), institution = character(0), bank_code = character(0),
                      confidence = character(0), stringsAsFactors = FALSE)
  agree <- TRUE
  if (kind %in% c("pdf", "scan") && nrow(ev)) {
    for (p in sort(unique(ev$page))) {
      d <- .bi_decide(ev[ev$page == p, , drop = FALSE], ref)
      pages <- rbind(pages, data.frame(page = p, institution = d$institution,
                                       bank_code = d$bank_code, confidence = d$confidence,
                                       stringsAsFactors = FALSE))
    }
    # Two pages that each name a bank with some confidence and name different
    # banks, or a page that names a bank and another that only hints at a bank
    # outside its family (a masthead, a guessed number): likely a bundle.
    named <- pages[!is.na(pages$institution), , drop = FALSE]
    sure <- named[named$confidence %in% c("high", "medium"), , drop = FALSE]
    hint <- named[named$confidence == "low", , drop = FALSE]
    hint <- hint[!ref$family[hint$institution] %in% ref$family[sure$institution], , drop = FALSE]
    clash <- rbind(sure[!duplicated(sure$institution), , drop = FALSE], hint)
    clash <- clash[!duplicated(clash$institution), , drop = FALSE]
    if (nrow(sure) && nrow(clash) > 1) {
      agree <- FALSE
      first <- clash[order(clash$page), , drop = FALSE][1:2, ]
      res <- list(institution = NA_character_, bank_code = NA_character_, confidence = "low",
                  conflict = TRUE, why = sprintf(
                    "The pages disagree about the bank (page %d says %s, page %d says %s), so this may be several statements from different banks.",
                    first$page[1], ref$display[[first$institution[1]]],
                    first$page[2], ref$display[[first$institution[2]]]))
    }
  }
  evid <- ev[ev$strength > 0 | ev$kind %in% c("other_bank", "foreign"), c("kind", "institution", "strength", "zone"), drop = FALSE]
  evid <- evid[order(-evid$strength, evid$kind, evid$institution, evid$zone), , drop = FALSE]
  evid <- evid[!duplicated(evid[, c("kind", "institution", "zone")]), , drop = FALSE]
  rownames(evid) <- NULL
  list(institution = res$institution, bank_code = res$bank_code,
       confidence = res$confidence, why = res$why, evidence = evid,
       display = if (is.na(res$institution)) NA_character_ else ref$display[[res$institution]],
       needs_decision = isTRUE(res$conflict), pages = pages, pages_agree = agree)
}

# bank_pick(identified, chosen, confirmed = FALSE) -> list(bank, ask,
#   block_learning, why). `identified` is bank_identify()'s result; `chosen` is
#   the person's pick (an id, a display name, or NULL/"" for none). `confirmed` is
#   TRUE once the person has seen the evidence and kept their pick.
#   A high-confidence identity that differs from the pick blocks learning until
#   confirmed; a medium one asks for a click-through; a low one only informs.
bank_pick <- function(identified, chosen = NULL, confirmed = FALSE) {
  tryCatch(.bank_pick(identified, chosen, isTRUE(confirmed)), error = function(e)
    list(bank = NA_character_, ask = TRUE, block_learning = TRUE,
         why = "The bank could not be settled; please pick it."))
}

.bank_pick <- function(identified, chosen, confirmed) {
  ref <- .bi_ref()
  id <- identified$institution %||% NA_character_
  if (length(id) != 1) id <- NA_character_
  conf <- identified$confidence %||% "unknown"
  disp <- function(i) if (!is.null(ref) && !is.na(i) && i %in% names(ref$display)) ref$display[[i]] else i
  pick <- if (is.null(chosen) || !length(chosen) || is.na(chosen[1])) "" else trimws(as.character(chosen[1]))
  # The pick as an id when it names a known bank (by id, display name or brand word).
  if (nzchar(pick) && !is.null(ref)) {
    np <- .bi_norm(pick)
    hit <- names(ref$display)[.bi_norm(names(ref$display)) == np | .bi_norm(ref$display) == np]
    if (!length(hit)) {
      br <- Filter(function(p) p$kind == "brand" && p$needle == np, ref$patterns)
      hit <- unique(vapply(br, function(p) p$institution, ""))
    }
    if (length(hit) == 1) pick <- hit
  }
  res <- function(bank, ask, block, why) list(bank = bank, ask = ask, block_learning = block, why = why)
  undecided <- isTRUE(identified$needs_decision) || isFALSE(identified$pages_agree)
  if (!nzchar(pick)) {
    if (!is.na(id) && conf %in% c("high", "medium"))
      return(res(id, FALSE, FALSE, sprintf("Pre-filled from the statement: %s", identified$why)))
    return(res(NA_character_, TRUE, TRUE, if (undecided) identified$why else
      "The statement does not say clearly which bank issued it; please pick the bank."))
  }
  if (undecided && !confirmed)
    return(res(pick, TRUE, TRUE, sprintf("%s Please confirm %s.", identified$why, disp(pick))))
  if (is.na(id) || identical(id, pick) || confirmed)
    return(res(pick, FALSE, FALSE, if (is.na(id) || !identical(id, pick))
      sprintf("Using %s, as picked.", disp(pick)) else
      sprintf("%s agrees with the statement.", disp(pick))))
  msg <- sprintf("You picked %s, but the statement looks like %s (%s confidence): %s",
                 disp(pick), disp(id), conf, identified$why)
  switch(conf,
    high = res(pick, TRUE, TRUE, paste(msg, "Nothing will be learned until you confirm.")),
    medium = res(pick, TRUE, FALSE, paste(msg, "Please confirm.")),
    res(pick, FALSE, FALSE, msg))
}
