# words.R -- one question, one answer: "what does this wording on a statement mean?"
#
# The words the tool knows live in two files, for a reason nobody teaching it a
# word needs to know:
#   dictionaries/labels.yaml  -- what a FACT about the statement is called: the
#                                words printed in front of the opening balance,
#                                the closing balance, the statement period ...
#   dictionaries/lexicon.yaml -- what words INSIDE the transaction table mean: a
#                                DR / CR mark, a column heading, a total line.
# word_meanings() is the one list of things a wording can mean, in plain words,
# each with an example, and where it is kept. Admin -> Words and Please check both
# teach through teach_wording(), so a word taught on either screen lands in the
# same place and passes the same checks.
#
# ONLY WHAT THE TOOL READS, AND ONLY WHAT A PERSON TEACHES. A meaning is listed
# here only if the reader acts on it. The rarer lists (the overdrawn mark, the
# words of a heading row, the money and date patterns) stay in the whole-file
# editor, where the built-in values are shown beside them.
# labels.yaml used to carry total_credits, total_debits and account_name, and the
# Admin list offered them, but nothing has read them since templates were retired:
# a word taught to one changed nothing, and the screen said "Added".

# word_meanings() -> data.frame: id, plain (what the screen says), example (as a
# statement prints it), file ("labels" / "lexicon"), lists (where it is written,
# ";"-separated -- a money-in or money-out mark is written to both lists that
# read marks, so it works beside an amount and in a column of its own), check
# (the kind of thing it is, for the clash rules) and side ("out" / "in": a mark
# and a heading on the same side may share a word, "Debit" is both).
word_meanings <- function() {
  m <- function(id, plain, example, file, lists, check, side = NA_character_)
    data.frame(id = id, plain = plain, example = example, file = file,
               lists = lists, check = check, side = side, stringsAsFactors = FALSE)
  rbind(
    m("opening_balance", "Opening balance", "Balance brought forward", "labels",
      "opening_balance", "money"),
    m("closing_balance", "Closing balance", "Balance carried forward", "labels",
      "closing_balance", "money"),
    m("statement_period", "Statement period - the words in front of its two dates",
      "Period covered", "labels", "statement_period", "range"),
    m("statement_start", "The statement's first day", "Opening date", "labels",
      "statement_start", "date"),
    m("statement_end", "The statement's last day", "Closing date", "labels",
      "statement_end", "date"),
    m("statement_date", "The date the statement was issued", "Date of issue", "labels",
      "statement_date", "date"),
    m("money_out_mark", "A word marking a row as money out", "DR", "lexicon",
      "debit_markers;dr_cr_suffix_debit", "mark", "out"),
    m("money_in_mark", "A word marking a row as money in", "CR", "lexicon",
      "credit_markers;dr_cr_suffix_credit", "mark", "in"),
    m("money_out_heading", "The heading of a money-out column", "Withdrawals", "lexicon",
      "amount_style_debit_headers", "heading", "out"),
    m("money_in_heading", "The heading of a money-in column", "Deposits", "lexicon",
      "amount_style_credit_headers", "heading", "in"),
    m("not_a_transaction", "A line in the table that is not a transaction", "Page total",
      "lexicon", "summary_line_labels", "line"))
}

# word_meaning_choices(blank) -- the dropdown both screens show: plain words with
# an example, valued by id. With `blank`, it opens on "Pick what it means" and
# nothing chosen, so a word is never filed under whatever happened to be first.
word_meaning_choices <- function(blank = FALSE) {
  wm <- word_meanings()
  ch <- stats::setNames(wm$id, sprintf("%s (e.g. \"%s\")", wm$plain, wm$example))
  if (isTRUE(blank)) c(stats::setNames("", "Pick what it means"), ch) else ch
}

# .known_wordings(id, dict, lex_path) -- what the tool already reads as `id`.
.known_wordings <- function(id, dict, lex_path) {
  wm <- word_meanings(); row <- wm[wm$id == id, , drop = FALSE]
  if (!nrow(row)) return(character(0))
  lists <- strsplit(row$lists, ";", fixed = TRUE)[[1]]
  w <- if (identical(row$file, "labels")) {
    v <- unlist(lapply(lists, function(l) dict[[l]]$any_of %||% character(0)))
    if (identical(id, "statement_period")) c(v, .period_labels(NULL)) else v
  } else unlist(lapply(lists, function(l) safe(lex(l, lex_path), character(0))))
  unique(tolower(trimws(as.character(w))))
}

# wording_problem(id, wording, dict, lex_path) -> NULL when the wording can be
# taught, else one plain sentence saying why not. Three rules, each one sentence:
#  1. A wording that already means something else is refused: one wording, one
#     meaning, or the same printed figure is read as two things. (A mark and a
#     heading on the same side are one meaning said two ways: "Debit" is both.)
#  2. A label wording that is PART of another label of the same kind is refused
#     ("balance" for the opening balance would catch the "Closing balance" line
#     too), and so is one that HOLDS such a label: labels match anywhere in a line.
#  3. Brackets and the symbols * + ? | ^ $ are refused: some lists are read as
#     patterns, and one stray bracket would stop every statement being read.
wording_problem <- function(id, wording, dict = default_label_dict(), lex_path = .lexicon_path()) {
  wm <- word_meanings()
  w <- tolower(trimws(as.character(wording %||% "")[1]))
  if (is.na(w) || !nzchar(w)) return("type the wording first - as the statement prints it")
  row <- wm[wm$id == id, , drop = FALSE]
  if (!nrow(row)) return("pick what the wording means")
  if (grepl("[][(){}*+?|^$]", w))
    return("leave out brackets and the symbols * + ? | ^ $ - type just the words")
  if (identical(row$file, "labels") && nchar(w) < 3L)
    return("that is too short to be a label - it would be found inside too many other words")
  for (o in setdiff(wm$id, id)) {
    known <- .known_wordings(o, dict, lex_path)
    oplain <- tolower(wm$plain[wm$id == o])
    if (!is.na(row$side) && identical(wm$side[wm$id == o], row$side)) next
    if (w %in% known)
      return(sprintf("\"%s\" already means %s - one wording can only mean one thing", wording, oplain))
    same_kind <- identical(wm$check[wm$id == o], row$check) && row$check %in% c("money", "date", "range", "heading")
    if (!same_kind) next
    inside <- known[vapply(known, function(k) grepl(w, k, fixed = TRUE), logical(1))]
    if (length(inside))
      return(sprintf("\"%s\" is part of \"%s\", which means %s - lines with that wording would be read as both",
                     wording, inside[1], oplain))
    holds <- known[vapply(known, function(k) grepl(k, w, fixed = TRUE), logical(1))]
    if (length(holds))
      return(sprintf("\"%s\" holds \"%s\", which already means %s - lines with it would be read as both",
                     wording, holds[1], oplain))
  }
  NULL
}

# teach_wording(id, wording, dict_path, lex_path) -> TRUE / FALSE with attr
# "reason" (a sentence the screen shows as is) and attr "added". The one way a
# person teaches the tool a word, from any screen.
teach_wording <- function(id, wording, dict_path = .dictionary_path(), lex_path = .lexicon_path()) {
  # every reason leaves as a finished sentence, ready for the screen
  say <- function(x) { x <- paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
    if (grepl("[.]$", x)) x else paste0(x, ".") }
  fail <- function(why) structure(FALSE, reason = say(why), added = FALSE)
  wm <- word_meanings(); row <- wm[wm$id == id, , drop = FALSE]
  wording <- trimws(as.character(wording %||% "")[1])
  dict <- safe(load_label_dict(dict_path), list())
  if (nrow(row) && nzchar(wording) && tolower(wording) %in% .known_wordings(id, dict, lex_path))
    return(structure(TRUE, reason = say("it already knew that wording"), added = FALSE))
  why <- wording_problem(id, wording, dict, lex_path)
  if (!is.null(why)) return(fail(why))
  lists <- strsplit(row$lists, ";", fixed = TRUE)[[1]]
  if (identical(row$file, "labels")) {
    out <- dictionary_append(lists[1], tolower(wording), path = dict_path)
  } else {
    # Marks are matched in capitals and headings and lines in small letters, so
    # the word is kept in the case the list is read in.
    v <- if (identical(row$check, "mark")) toupper(wording) else tolower(wording)
    out <- lexicon_append(lists[1], v, lex_path)
    for (l in lists[-1]) if (isTRUE(out)) out <- lexicon_append(l, v, lex_path)
  }
  if (!isTRUE(out)) return(fail(attr(out, "reason") %||% "could not write the words file"))
  clear_lexicon_cache()
  structure(TRUE, reason = sprintf("From now on, \"%s\" is read as: %s.",
                                   wording, tolower(row$plain)), added = TRUE)
}

# statement_wordings(pages) -- the wordings a statement prints in front of a
# figure or a date, for Please check to offer: "Kickoff kitty" from a line
# "Kickoff kitty   $1,250.00". Read from the page text in this session only;
# nothing is kept. Lines that begin with a figure or a date are transactions and
# are left out, and so are wordings the tool already reads as something.
statement_wordings <- function(pages, dict = default_label_dict(), lex_path = .lexicon_path()) {
  lines <- trimws(unlist(strsplit(paste(pages %||% character(0), collapse = "\n"), "\n", fixed = TRUE)))
  if (!length(lines)) return(character(0))
  # One byte, one character: a dash or a currency sign the typesetter printed
  # becomes a space, so a match position is a character position as well.
  lines <- iconv(lines, "UTF-8", "ASCII", sub = " ")
  lines[is.na(lines)] <- ""
  money_rx <- safe(lex("money_regex", lex_path), .MONEY_RX)
  date_rx <- safe(lex("date_regex", lex_path), .DATE_RX)
  val <- regexpr(sprintf("(?:%s)|(?:%s)", money_rx, date_rx), lines, perl = TRUE, useBytes = TRUE)
  has <- val > 1L
  lab <- rep("", length(lines))
  lab[has] <- substr(lines[has], 1L, val[has] - 1L)
  # the words in front of the figure, back to the last wide gap, colon or bar
  lab <- vapply(strsplit(lab, "[[:blank:]]{2,}|:|[|]"), function(p) {
    p <- trimws(p[nzchar(trimws(p))]); if (length(p)) utils::tail(p, 1L) else "" }, "")
  lab <- trimws(sub("[[:blank:][:punct:]]+$", "", lab))
  # A bare joining word in front of a date ("from", "to", "as at") labels nothing.
  joins <- tolower(c(safe(lex("period_connectives", lex_path), character(0)), "from", "as at", "at", "on", "dated"))
  keep <- nzchar(lab) & nchar(lab) <= 60L & grepl("^[A-Za-z]", lab) & !grepl("^[0-9]", lines) &
    !(tolower(lab) %in% joins)
  lab <- unique(lab[keep])
  known <- unique(unlist(lapply(word_meanings()$id, .known_wordings, dict = dict, lex_path = lex_path)))
  lab <- lab[!(tolower(lab) %in% known)]
  # ... nor one the table reader already treats as a balance or a total line
  # ("Page total" is a pattern there, not a listed word).
  read <- vapply(lab, function(x) isTRUE(safe(.pdf_is_summary(x, NULL), FALSE)) ||
                   nzchar(safe(.ar_anchor_class(x), "")), logical(1))
  unname(lab[!read])
}
