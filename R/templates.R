# templates.R -- load + validate declarative per-bank YAML templates.

# Keys every template declares, regardless of format.
.TEMPLATE_COMMON <- c("id", "bank", "statement_type", "format", "version",
                      "min_score", "fingerprint", "currency")
.VALID_SIGN <- c("signed", "debit_credit_cols", "dr_cr_suffix", "type_dc", "unsigned")
# Header values a PDF template may pin with a drawn box (table.metadata_regions),
# for statements whose label wording the dictionary doesn't recognise.
.META_REGION_FIELDS <- c("opening_balance", "closing_balance", "period_start",
                         "period_end", "account_number", "account_name")

# Column names essentially EVERY statement export in the world carries. Used only
# by the delimited/excel half of the fingerprint gate below.
.FP_UNIVERSAL_COLUMNS <- c("date", "amount", "description", "balance", "details",
  "memo", "note", "notes", "type", "credit", "debit", "value", "total",
  "reference", "ref", "code", "id", "name", "number", "no", "account", "currency",
  "particulars", "payee", "comment", "comments", "category", "transaction",
  "deposit", "deposits", "withdrawal", "withdrawals")

# .fp_fingerprint_problems(ph, min_score, fmt) -- the load-time gate on a
# template's fingerprint. Two separate rules, both about the CARDINAL failure
# (a wrong figure that looks right):
#
# 1. DISTINCTIVENESS. A fingerprint made of words that sit on every statement
#    means the template matches statements it has never seen and parses them with
#    another bank's column bands. This actually happened: the ASB PDF template
#    scored its min_score of 2 on the bare words "Deposit" and "Balance", so a
#    FOREIGN bank's PDF was parsed as ASB (charges emitted as credits, status ok,
#    feed ACCEPTED) -- and, because it also scored 2 on real ANZ statements, it
#    became a permanent runner-up that pushed every genuine ANZ PDF into review.
#
#    The rule is stricter for PDF than for delimited/excel, deliberately:
#      * PDF phrases are matched as a substring ANYWHERE in the page text, so a
#        generic word is worth nothing as evidence -> min_score must NOT be
#        reachable by the generic phrases alone.
#      * delimited/excel phrases must equal a whole COLUMN NAME, so even a common
#        name is real evidence that the file has that exact column, and min_score
#        counts how many must line up at once -> we only require that not EVERY
#        phrase is generic. And "generic" there means BOTH short-and-single-word
#        AND one of the handful of universal column names, so a foreign-language
#        or unusual header ("Datum") still counts as distinctive. This is what
#        keeps the deliberately-generic excel_generic_xlsx (Date / Description /
#        Amount / Balance) valid -- "Description" is not a short word.
#
# 2. NO PII. A phrase that names a PERSON identifies one customer, not a layout,
#    and a template file is listed in Admin, shown in the YAML box and copied into
#    the offline bundle. The drafter already refuses to propose one; this closes
#    the hand-edited Advanced-YAML path too.
.fp_fingerprint_problems <- function(ph, min_score, fmt) {
  problems <- character(0)
  ph <- trimws(as.character(unlist(ph %||% character(0))))
  ph <- ph[!is.na(ph) & nzchar(ph)]

  pii <- if (length(ph)) .fp_has_pii(ph) else logical(0)
  if (any(pii)) problems <- c(problems, sprintf(paste0(
    "fingerprint phrase %s names a PERSON -- a fingerprint must identify the ",
    "LAYOUT, never a customer. Use a statement heading or a column label instead."),
    paste(sprintf("'%s'", ph[pii]), collapse = ", ")))

  ms <- suppressWarnings(as.numeric(min_score %||% length(ph))[1])
  if (is.na(ms)) ms <- length(ph)

  if (identical(fmt, "pdf")) {
    n_generic <- if (length(ph)) sum(!.fp_specific(ph)) else 0L
    if (!length(ph) || n_generic >= ms) problems <- c(problems, paste0(
      "fingerprint.page_contains_all is too generic for a pdf template: min_score ",
      "can be reached by generic phrases alone -- add a distinctive phrase (a ",
      "statement heading or a multi-word column label, not single words like ",
      "'Balance'), or raise min_score so a distinctive one is always required"))
  } else {
    key <- gsub("[^a-z0-9]", "", tolower(ph))
    generic <- if (length(ph)) !.fp_specific(ph) & (key %in% .FP_UNIVERSAL_COLUMNS) else logical(0)
    if (!length(ph) || all(generic)) problems <- c(problems, paste0(
      "fingerprint.header_contains_all is too generic: every column name in it ",
      "(", paste(ph, collapse = ", "), ") is one every statement carries, so this ",
      "template would match other banks' files -- include at least one column name ",
      "that is specific to this layout"))
  }
  problems
}

# resolve_date_format(values, formats) -> the ONE declared format that reads EVERY
# non-empty value, or NA_character_ when none of them does.
#
# WHY a template may declare a LIST of candidate formats: the same bank, the same
# export, the same header row -- and two different date styles across eras. ASB's
# FastNet CSV writes "2014/12/20" in one export and "13/10/2025" in another, so
# pinning one format made the other detect confidently and then return EVERY date
# NA (dates_readable = fail).
#
# WHY it is all-or-nothing: reading some rows under one format and the rest under
# another is precisely the silently-wrong outcome the charter forbids -- "13/10"
# and "10/13" are both readable, and a per-row mixture would swap day and month
# with nothing to show for it. So a candidate only wins if it reads the WHOLE
# column, the first such candidate (declaration order) wins so the result is
# deterministic, and "none of them fits" returns NA so the caller fails closed
# rather than guessing.
resolve_date_format <- function(values, formats) {
  fmts <- trimws(as.character(unlist(formats %||% character(0))))
  fmts <- fmts[!is.na(fmts) & nzchar(fmts)]
  if (!length(fmts)) return(NA_character_)
  if (length(fmts) == 1L) return(fmts[1])       # the ordinary case, behaviour unchanged
  v <- as.character(unlist(values))
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(fmts[1])               # nothing to judge on -> as declared
  for (f in fmts) if (all(!is.na(parse_date(v, f)$iso))) return(f)
  NA_character_
}

# resolve_delimiter(header_line, template) -> the ONE declared delimiter to read
# this file with. Like the date format above, a template MAY declare a LIST -- the
# same bank publishing the same export as CSV and as tab-delimited (ASB FastNet
# ships both: asb_transaction_export_01.csv and asb_transaction_export_02.tdv,
# byte-identical layout, different separator). Pinning one meant the other's header
# never split into columns at all, so it scored 0 and came back "unsupported".
#
# THE ALL-OR-NOTHING RULE, and why it is this one: a candidate wins only if the
# header row, split by it, contains EVERY column name the template's fingerprint
# names. Splitting a tab-delimited line on commas yields ONE field, so it can never
# satisfy that -- the wrong separator is rejected outright rather than "sort of"
# working. The test deliberately looks at the HEADER ONLY: making it depend on the
# data rows would let a single ragged row (which the reader is built to isolate and
# flag) reject the correct delimiter and pick a catastrophic one. When no candidate
# satisfies it, the FIRST declared delimiter is used -- identical to the old
# behaviour, so the file simply fails to score and stays honestly unsupported.
# A single declared delimiter short-circuits: every existing template is untouched.
resolve_delimiter <- function(header_line, template) {
  d <- as.character(unlist(template$delimiter %||% ","))
  d <- d[!is.na(d) & nzchar(d)]
  if (!length(d)) return(",")
  if (length(d) == 1L) return(d[1])            # the ordinary case, behaviour unchanged
  need <- trimws(as.character(unlist(template$fingerprint$header_contains_all %||% character(0))))
  need <- need[!is.na(need) & nzchar(need)]
  hl <- as.character(header_line)[1]
  if (!length(need) || is.na(hl) || !nzchar(hl)) return(d[1])
  for (delim in d) {
    fields <- trimws(as.character(.record_fields(hl, delim)))
    if (all(tolower(need) %in% tolower(fields))) return(delim)
  }
  d[1]
}

# .delimiter_problems(spec) -- validate a declared delimiter. A list is allowed
# (resolve_delimiter picks one), but every entry must be a real separator string:
# an empty entry would split every line into single characters.
.delimiter_problems <- function(spec) {
  if (is.null(spec)) return(character(0))
  d <- as.character(unlist(spec))
  bad <- is.na(d) | !nzchar(d)
  if (any(bad)) return("delimiter entries must each be a non-empty separator string")
  character(0)
}

# .date_format_problems(spec, where, allow_list) -- validate a declared date
# format. A LIST of candidates is allowed only where the reader resolves it
# (resolve_date_format); everywhere else a list must be REJECTED loudly, because
# base as.Date() recycles a format vector element-wise -- row 1 read as %Y/%m/%d,
# row 2 as %d/%m/%Y -- which is a whole column of plausible, wrong dates.
.date_format_problems <- function(spec, where, allow_list) {
  if (is.null(spec)) return(character(0))
  f <- as.character(unlist(spec))
  if (length(f) > 1L && !allow_list)
    return(sprintf("%s must be a single date format, not a list", where))
  bad <- is.na(f) | !nzchar(trimws(f)) | !grepl("%", f, fixed = TRUE)
  if (any(bad))
    return(sprintf("%s entry %s is not a date format (expected strptime codes like %%d/%%m/%%Y)",
                   where, paste(sprintf("'%s'", f[bad]), collapse = ", ")))
  character(0)
}

# .one_date_format(spec) -- a declared format collapsed to ONE display string, for
# the overview table and the duplicate-layout signature (both of which need a
# single scalar per template, not a vector).
.one_date_format <- function(spec) {
  f <- as.character(unlist(spec %||% character(0)))
  f <- f[!is.na(f) & nzchar(f)]
  if (!length(f)) NA_character_ else paste(f, collapse = " | ")
}

# validate_template(t) -> character vector of problems (length 0 if valid).
# Format-aware: delimited/excel carry columns+amount_sign at the top level and
# fingerprint on the header; pdf carries them under `table:` and fingerprints on
# page text.
validate_template <- function(t) {
  problems <- character(0)
  if (!is.list(t)) return("template is not a mapping")
  fmt <- if (is.null(t$format)) "delimited" else t$format

  for (k in .TEMPLATE_COMMON)
    if (is.null(t[[k]])) problems <- c(problems, sprintf("missing key '%s'", k))
  valid_fmt <- c("delimited", "excel", "pdf")
  if (!is.null(t$format) && !(fmt %in% valid_fmt))
    problems <- c(problems, sprintf("format '%s' is not one of %s", fmt,
                                    paste(valid_fmt, collapse = "/")))

  # decimal_mark is optional; when present it must name a known locale.
  dm <- t$decimal_mark %||% t$table$decimal_mark
  if (!is.null(dm) && !(dm %in% c("auto", "dot", "comma")))
    problems <- c(problems, sprintf("decimal_mark '%s' is not one of auto/dot/comma", dm))

  # effective_from / effective_to: a window that CLOSES BEFORE IT OPENS can never
  # contain a statement. Such a template loads, validates and detects like any
  # other, and the only sign of it is that every statement it should have caught
  # comes back "no template for this layout yet" -- a silent no-op, which is the
  # one thing this engine is not allowed to be. Unparseable bounds are left alone:
  # the soft window check in diagnose.R already skips those rather than guessing,
  # and a validator that rejected what it could not read would be guessing too.
  eff_from <- .tolerant_date(t$effective_from)
  eff_to   <- .tolerant_date(t$effective_to)
  if (!is.na(eff_from) && !is.na(eff_to) && eff_from > eff_to)
    problems <- c(problems, sprintf(
      paste("effective_from (%s) is later than effective_to (%s), so this template",
            "could never apply to any statement - swap the two dates, or clear the",
            "one you did not mean to set"),
      as.character(t$effective_from)[1], as.character(t$effective_to)[1]))

  if (identical(fmt, "pdf")) {
    if (!is.null(t$fingerprint) && is.null(t$fingerprint$page_contains_all))
      problems <- c(problems, "fingerprint.page_contains_all is required for pdf templates")
    # Distinctiveness + PII gate (see .fp_fingerprint_problems for the WHY).
    if (!is.null(t$fingerprint) && !is.null(t$fingerprint$page_contains_all))
      problems <- c(problems, .fp_fingerprint_problems(
        t$fingerprint$page_contains_all, t$min_score, "pdf"))
    # The pdf reader takes ONE date format; a list here would be recycled per row.
    problems <- c(problems, .date_format_problems(t$table$date_format,
                                                  "table.date_format", allow_list = FALSE))
    tab <- t$table
    if (is.null(tab)) {
      problems <- c(problems, "pdf templates require a 'table' block")
    } else {
      for (k in c("date", "description"))
        if (is.null(tab$columns[[k]]))
          problems <- c(problems, sprintf("table.columns.%s is required", k))
      # amount source depends on the sign style, exactly like the delimited path:
      # debit_credit_cols needs debit+credit bands; everything else needs amount.
      if (identical(tab$amount_sign, "debit_credit_cols")) {
        for (k in c("debit", "credit"))
          if (is.null(tab$columns[[k]]))
            problems <- c(problems, sprintf("table.amount_sign 'debit_credit_cols' requires table.columns.%s", k))
      } else if (is.null(tab$columns[["amount"]])) {
        problems <- c(problems, "table.columns.amount is required")
      }
      if (!is.null(tab$amount_sign) && !(tab$amount_sign %in% .VALID_SIGN))
        problems <- c(problems, sprintf("table.amount_sign '%s' is invalid", tab$amount_sign))
      # metadata_regions (optional): each entry pins a header value to a drawn box.
      # The field must be one we actually wire into the header, and the box needs an
      # x-band. Absent metadata_regions leaves validation exactly as it was.
      if (!is.null(tab$metadata_regions)) {
        mr <- tab$metadata_regions
        if (!is.list(mr)) problems <- c(problems, "table.metadata_regions must be a mapping")
        else for (field in names(mr)) {
          if (!(field %in% .META_REGION_FIELDS))
            problems <- c(problems, sprintf("table.metadata_regions.%s is not a header field (one of %s)",
                                            field, paste(.META_REGION_FIELDS, collapse = "/")))
          reg <- mr[[field]]
          if (!is.list(reg) || is.null(reg$x_min) || is.null(reg$x_max))
            problems <- c(problems, sprintf("table.metadata_regions.%s is malformed (needs x_min and x_max)", field))
        }
      }
    }
  } else {
    if (is.null(t$columns)) problems <- c(problems, "missing key 'columns'")
    if (is.null(t$amount_sign)) problems <- c(problems, "missing key 'amount_sign'")
    if (!is.null(t$fingerprint) && is.null(t$fingerprint$header_contains_all))
      problems <- c(problems, "fingerprint.header_contains_all is required")
    # Same distinctiveness + PII gate as the pdf branch above -- this branch used
    # to have NO content gate at all, so a [Date, Amount] template could be saved
    # and would then match any CSV in the building.
    if (!is.null(t$fingerprint) && !is.null(t$fingerprint$header_contains_all))
      problems <- c(problems, .fp_fingerprint_problems(
        t$fingerprint$header_contains_all, t$min_score, fmt))
    # columns.date.format MAY be a list of candidates (resolve_date_format picks
    # the one that reads the whole column, or none -- never a per-row mixture).
    problems <- c(problems, .date_format_problems(t$columns$date$format,
                                                  "columns.date.format", allow_list = TRUE))
    # delimiter MAY likewise be a list (resolve_delimiter picks the one under which
    # the header carries every fingerprinted column, or the first as before).
    problems <- c(problems, .delimiter_problems(t$delimiter))
    # date + description are always required; the money column depends on the
    # sign style, exactly like the pdf branch above. A debit_credit_cols template
    # (separate money-in / money-out columns) has NO single 'amount' column -- it
    # supplies debit + credit instead (checked below), so requiring 'amount' here
    # would reject the tool's own draft of a very common CSV shape.
    always <- if (identical(t$amount_sign, "debit_credit_cols")) c("date", "description")
              else c("date", "amount", "description")
    if (!is.null(t$columns)) for (k in always)
      if (is.null(t$columns[[k]])) problems <- c(problems, sprintf("columns.%s is required", k))
    if (!is.null(t$amount_sign) && !(t$amount_sign %in% .VALID_SIGN))
      problems <- c(problems, sprintf("amount_sign '%s' is not one of %s",
                                      t$amount_sign, paste(.VALID_SIGN, collapse = "/")))
    .has_col <- function(field) !is.null(t$columns) && !is.null(t$columns[[field]])
    if (identical(t$amount_sign, "debit_credit_cols")) for (k in c("debit", "credit"))
      if (!.has_col(k)) problems <- c(problems,
        sprintf("amount_sign 'debit_credit_cols' requires columns.%s", k))
    if (identical(t$amount_sign, "type_dc")) {
      if (!.has_col("type"))
        problems <- c(problems, "amount_sign 'type_dc' requires columns.type")
      # The debit token is mandatory: without it the reader falls back to a blind
      # "D", which silently flips the sign on any bank that writes it differently.
      if (is.null(t$type_debit_value) || !nzchar(trimws(as.character(t$type_debit_value))))
        problems <- c(problems, "amount_sign 'type_dc' requires type_debit_value (which indicator value means a debit)")
    }
  }

  # split: opt-in auto-split of a bundled upload (see split.R). Validate loudly so
  # a bad split config is rejected, never silently applied.
  if (!is.null(t$split) && !isFALSE(t$split)) {
    if (!identical(fmt, "pdf"))
      problems <- c(problems, "split is only supported for pdf templates")
    if (!isTRUE(t$split)) {
      if (!is.list(t$split)) problems <- c(problems, "split must be true or a mapping")
      else {
        on <- t$split$on
        if (!is.null(on) && !(tolower(as.character(on)[1]) %in% c("page1_marker", "opening_label")))
          problems <- c(problems, sprintf("split.on '%s' is not one of page1_marker/opening_label", on))
        ms <- suppressWarnings(as.integer(t$split$min_statements %||% 2L))
        if (is.na(ms) || ms < 2L)
          problems <- c(problems, "split.min_statements must be an integer >= 2")
      }
    }
  }

  # extras: {source} for delimited/excel, {x_min,x_max} bands for pdf.
  extras <- if (identical(fmt, "pdf")) t$table$extras else t$extras
  if (!is.null(extras)) {
    if (!is.list(extras)) problems <- c(problems, "extras must be a mapping")
    else for (field in names(extras)) {
      fs <- extras[[field]]
      ok <- if (identical(fmt, "pdf"))
              is.list(fs) && !is.null(fs$x_min) && !is.null(fs$x_max)
            else { src <- if (is.list(fs)) fs$source else fs
                   !is.null(src) && nzchar(as.character(src)[1]) }
      if (!isTRUE(ok)) problems <- c(problems, sprintf("extras.%s is malformed", field))
    }
  }
  problems
}

# load_templates(dir, origin, strict) -> named list<template> keyed by id.
# origin ("default" | "user") is stamped on each template so the app and reports
# can tell a curated team template from one an accountant created. strict=TRUE
# (curated set): an invalid template is a HARD error. strict=FALSE (user set):
# an invalid/duplicate template is skipped with a warning, so one bad user
# template can never break everyone's conversions.
load_templates <- function(dir, origin = "default", strict = TRUE) {
  if (!dir.exists(dir)) {
    if (strict) stop(sprintf("templates dir not found: %s", dir))
    return(list())
  }
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  templates <- list()
  errors <- character(0)
  for (f in files) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) e)
    if (inherits(t, "error")) {
      errors <- c(errors, sprintf("%s: YAML parse error: %s",
                                  basename(f), conditionMessage(t)))
      next
    }
    # AN UNFINISHED SEED IS NOT A TEMPLATE. templates_seed/ ships starting points
    # whose bands are placeholders marked "# TODO draw" - and YAML drops comments,
    # so once one is copied into templates_user/ nothing the loader can see says it
    # is unfinished. Six of the ten shipped seeds validate as-is, so a half-drawn
    # one would join detection with placeholder coordinates. It would fail loudly
    # (garbage rows break reconciliation; no rows reports "matched, read nothing"),
    # but the person would be debugging a template they had not finished drawing.
    # `draft: true` makes that state visible to the loader, which refuses it and
    # says what to do - the same shape as the `sample: true` exclusion below.
    if (isTRUE(t$draft)) {
      errors <- c(errors, sprintf("%s: this is an unfinished seed template - draw its bands in the toolkit, then delete the `draft: true` line",
                                  t$id %||% basename(f)))
      next
    }
    probs <- validate_template(t)
    if (length(probs)) {
      label <- if (!is.null(t$id)) t$id else basename(f)
      errors <- c(errors, sprintf("%s: %s", label, paste(probs, collapse = "; ")))
      next
    }
    if (!is.null(templates[[t$id]])) {
      errors <- c(errors, sprintf("%s: duplicate template id", t$id))
      next
    }
    t$origin <- origin
    templates[[t$id]] <- t
  }
  if (length(errors)) {
    if (strict) stop("Invalid template(s):\n", paste(errors, collapse = "\n"))
    warning("Skipped invalid user template(s):\n", paste(errors, collapse = "\n"))
  }
  # A skipped template DISAPPEARS -- it stops matching, stops showing in Admin, and
  # the only trace is an R warning nobody reads. Failing closed is right (a template
  # we cannot validate must not parse statements), but doing it SILENTLY is not: an
  # analyst whose saved template vanished after an update would have no way to find
  # out why. Carry the reasons back with the result so the app can say plainly
  # "N template(s) could not be loaded" and name each one.
  attr(templates, "load_errors") <- errors
  templates
}

# load_template_set(default_dir, user_dir, include_hidden) -> merged templates.
# Curated defaults load first and WIN on any id clash (a user template can never
# shadow a team-blessed one). User templates fill in the rest and are marked
# origin="user". A user template flagged `hidden: true` is EXCLUDED by default, so
# it stops taking part in detection/conversion without being deleted (a cluttered
# pile of near-duplicate drafts can be parked, not lost). include_hidden = TRUE
# returns them too, for the Admin management view that can un-hide them.
load_template_set <- function(default_dir = "templates", user_dir = "templates_user",
                              include_hidden = FALSE) {
  d <- load_templates(default_dir, origin = "default", strict = TRUE)
  # Carry the skip reasons across the merge. Subsetting a list (`u[...]`, the
  # `sample` filter below) DROPS its attributes, so without this the attribute
  # load_templates() attaches is silently thrown away here -- and a user template
  # dropped for, say, a generic fingerprint would vanish with no explanation
  # anywhere in the app, which is the exact defect attr(x, "load_errors") exists to
  # close. Both sets contribute (the curated set is strict, so in practice it
  # stops rather than returning errors, but carrying it costs nothing and is right
  # if that ever changes).
  errors <- as.character(attr(d, "load_errors") %||% character(0))
  if (!is.null(user_dir) && dir.exists(user_dir)) {
    u <- load_templates(user_dir, origin = "user", strict = FALSE)
    errors <- c(errors, as.character(attr(u, "load_errors") %||% character(0)))
    if (!include_hidden)
      u <- u[!vapply(u, function(t) isTRUE(t$hidden), logical(1))]
    for (id in names(u)) if (is.null(d[[id]])) d[[id]] <- u[[id]]
  }
  # Sample / tutorial templates (`sample: true`) are worked examples, not real
  # bank formats -- they must never take part in production detection, or their
  # generic wording could match (or tie with) a real statement and mis-parse it
  # with a demo's hardcoded bands. They stay loadable via load_templates() for the
  # wizard walkthrough and their own self-tests, just not in this detection set.
  out <- d[!vapply(d, function(t) isTRUE(t$sample), logical(1))]
  attr(out, "load_errors") <- errors   # re-attached AFTER the subset (which drops attrs)
  out
}

# template_overview(tset) -- a flat data.frame summarising every loaded template,
# for the Admin overview and the Convert "what's covered" panel. `origin` reads as
# "tested" (a shipped, golden-file-tested default) or "user" (built on this box).
template_overview <- function(tset) {
  cols <- c("id", "bank", "type", "format", "amount_sign", "date_format", "origin", "hidden", "version")
  if (!length(tset))
    return(setNames(data.frame(matrix(character(0), 0, length(cols))), cols))
  rows <- lapply(tset, function(t) {
    is_pdf <- identical(t$format %||% "delimited", "pdf")
    data.frame(
      id          = t$id %||% NA_character_,
      bank        = t$bank %||% NA_character_,
      type        = t$statement_type %||% NA_character_,
      format      = t$format %||% "delimited",
      amount_sign = (if (is_pdf) t$table$amount_sign else t$amount_sign) %||% "signed",
      # .one_date_format: a template may declare SEVERAL candidate date formats;
      # the overview is one row per template, so show them as one cell.
      date_format = .one_date_format(if (is_pdf) t$table$date_format else t$columns$date$format),
      origin      = if (identical(t$origin %||% "default", "user")) "user" else "tested",
      hidden      = if (isTRUE(t$hidden)) "hidden" else "",
      version     = as.character(t$version %||% NA),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out[order(out$bank, out$type, out$id), , drop = FALSE]
}

# template_display_name(t) -- the template said in words the person holding the
# statement can check ("ANZ everyday statement"). Anything customer-facing uses
# this; the id and the version stay for the logs. An id is a maintainer's handle
# and a score is an internal metric -- neither is something an accountant can act
# on, and the charter's interface rule forbids putting either in front of her.
# Falls back to the id when a template declares no bank or type.
template_display_name <- function(t) {
  if (is.null(t) || !is.list(t)) return(NA_character_)
  lab <- trimws(paste(trimws(as.character(t$bank %||% "")),
                      trimws(as.character(t$statement_type %||% ""))))
  if (nzchar(lab)) paste(lab, "statement") else as.character(t$id %||% NA_character_)
}

# template_yaml(t) -- the template rendered as YAML text for preview/edit, with the
# load-time `origin` marker stripped so it round-trips cleanly.
template_yaml <- function(t) { t$origin <- NULL; yaml::as.yaml(t) }

# .template_shape(t) -- a structural signature: format + amount_sign + date_format
# + the (sorted) column mapping. Two templates with the same shape read a
# statement identically, differing only in id / bank label / fingerprint -- i.e.
# they are the same layout drafted more than once.
.template_shape <- function(t) {
  is_pdf <- identical(t$format %||% "delimited", "pdf")
  cols   <- if (is_pdf) t$table$columns else t$columns
  sign   <- if (is_pdf) t$table$amount_sign else t$amount_sign
  # NA (no format declared) folds to "" so the signature is unchanged for the
  # templates that never declared one.
  dfmt   <- .one_date_format(if (is_pdf) t$table$date_format else t$columns$date$format)
  if (is.na(dfmt)) dfmt <- ""
  colsig <- if (is.null(cols)) "" else paste(sort(vapply(names(cols), function(k) {
    c <- cols[[k]]
    if (is_pdf) sprintf("%s:%s-%s", k, c$x_min %||% "", c$x_max %||% "")
    else sprintf("%s:%s", k, (if (is.list(c)) c$source else c) %||% "")
  }, character(1))), collapse = "|")
  paste(t$format %||% "delimited", sign %||% "", dfmt %||% "", colsig, sep = "~~")
}

# duplicate_template_groups(tset, user_only) -> list of id-vectors, one per group
# of templates that share a layout (see .template_shape). Only groups with >1
# member are returned -- these are the "heap of near-duplicates" to consolidate:
# keep one, hide or delete the rest. user_only ignores shipped templates (you
# can't delete those anyway). Deterministic; safe to show (ids + shapes only).
duplicate_template_groups <- function(tset, user_only = TRUE) {
  if (user_only) tset <- Filter(function(t) identical(t$origin %||% "default", "user"), tset)
  if (length(tset) < 2) return(list())
  sig <- vapply(tset, .template_shape, character(1))
  groups <- split(names(tset), sig)
  unname(groups[vapply(groups, length, integer(1)) > 1L])
}

# save_user_template(template, dir) -> path. Validates first (fail loud), writes
# <dir>/<id>.yaml. This is how the guided flow persists an accountant's template.
save_user_template <- function(template, dir = "templates_user") {
  template$origin <- NULL   # origin is assigned at load time, not stored
  probs <- validate_template(template)
  if (length(probs)) stop("template is not valid: ", paste(probs, collapse = "; "))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  base_slug <- gsub("[^A-Za-z0-9_]+", "_", template$id %||% "user_template")
  # Two DISTINCT ids can sanitise to the same file slug ("ANZ Go!" and "ANZ-Go"
  # both -> "ANZ_Go"). Only overwrite when the existing file holds the SAME
  # original id (a genuine edit); otherwise pick the next free slug so a different
  # template is never silently clobbered. (Templates are keyed by their id field
  # at load, so the distinct ids stay distinct -- only the filename is uniquified.)
  slug <- base_slug; k <- 2L
  repeat {
    path <- file.path(dir, paste0(slug, ".yaml"))
    if (!file.exists(path)) break
    existing_id <- tryCatch(yaml::read_yaml(path)$id, error = function(e) NULL)
    if (identical(existing_id, template$id)) break
    slug <- paste0(base_slug, "_", k); k <- k + 1L
  }
  path <- file.path(dir, paste0(slug, ".yaml"))
  yaml::write_yaml(template, path)
  invisible(path)
}

# user_template_ids(dir) -> ids of templates that live in the user dir (the only
# ones the app may delete / rename -- shipped "tested" templates are read-only).
user_template_ids <- function(dir = "templates_user") {
  if (!dir.exists(dir)) return(character(0))
  ids <- vapply(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE), function(f) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL); t$id %||% NA_character_
  }, character(1))
  unname(ids[!is.na(ids)])
}

# delete_user_template(id, dir) -> TRUE if a user template file was removed. Only
# ever touches the user dir; shipped templates cannot be deleted from the app.
delete_user_template <- function(id, dir = "templates_user") {
  if (!dir.exists(dir) || is.null(id) || !nzchar(id)) return(invisible(FALSE))
  safe_id <- gsub("[^A-Za-z0-9_]+", "_", id)
  hit <- FALSE
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (identical(t$id %||% "", id) || identical(tools::file_path_sans_ext(basename(f)), safe_id)) {
      if (file.remove(f)) hit <- TRUE
    }
  }
  invisible(hit)
}

# set_user_template_hidden(id, hidden, dir) -> the new hidden state. Writes (or
# clears) `hidden: true` in the user template's own file, keeping its filename.
# Hidden templates drop out of detection/conversion (load_template_set default)
# but stay on disk and in the Admin management view, so they can be un-hidden or
# merged later. Only user templates -- shipped ones are read-only.
set_user_template_hidden <- function(id, hidden = TRUE, dir = "templates_user") {
  if (!(id %in% user_template_ids(dir))) stop("only user-created templates can be hidden")
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (identical(t$id %||% "", id)) {
      t$origin <- NULL
      if (isTRUE(hidden)) t$hidden <- TRUE else t$hidden <- NULL
      yaml::write_yaml(t, f)
      return(invisible(isTRUE(hidden)))
    }
  }
  stop("template not found: ", id)
}

# There is no rename_user_template(). There was one, and nothing ever called it
# but its own test: no screen offers a rename, and a template id is a stable
# handle that the run log, the feed manifest and every audit record point back to
# -- so renaming one in place would leave those records naming a template that no
# longer exists. A user template is re-saved under a new id (save_user_template)
# and the old one deleted (delete_user_template) when that is genuinely wanted,
# which is two visible steps rather than one silent one.
