# tables.R -- the REGION extractor: pull named tables and label/value pairs out of
# a document that is not a bank statement.
#
# WHY THIS EXISTS, AND WHY IT IS NOT THE STATEMENT PARSER.
# The statement parser (R/parse_pdf_table.R) reads ONE table, whose columns run
# the full height of every page, and it judges what it read against a running
# balance. Everything it does well depends on those three facts. The documents
# this file is for break all three: a forty-page report carrying thirty tables of
# different shapes, several to a page, some starting half-way down and ending
# half-way down the page after next, with no balance anywhere to check them
# against. Bolting that onto the statement parser would have put the untested,
# uncheckable case inside the code path that every real conversion runs through.
# So this is a separate engine with its own template mode ("document"), its own
# outputs, and NO route to the Qlik feed -- see write_feed(), which refuses this
# kind outright.
#
# WHAT A TABLE IS HERE.
#   * a START (page, y) and an END (page, y). Both are positions ON a page, not
#     whole pages, because a table can begin below a paragraph and finish above
#     the next heading.
#   * COLUMNS as x-bands that do NOT span the full height of the page -- they are
#     only meaningful between that start and that end.
#   * an ANCHOR: the wording of its header row, and the wording in its first
#     column. TEXT FIRST, POSITION SECOND -- the coordinates are the fallback,
#     never the primary, because the next copy of this document will have moved.
#
# WHAT IT REFUSES TO DO. It does not decide that a number is wrong, because it
# has nothing to check it against. What it does instead is MEASURE and REPORT:
# how it found each table (and how sure it is), what fraction of each column and
# each row it actually filled, and every word inside the table's boundary that no
# column claimed. A silently dropped column is the failure mode this design is
# built to make impossible to miss.
#
# Entry points:
#   doc_locate_table(input, tab, tmpl)      -> where this table is, and how we know
#   doc_table_rows(input, tab, tmpl)        -> its rows + the fill report
#   doc_pairs(input, tmpl)                  -> the label/value pairs
#   extract_document(input, tmpl)           -> all of it (see R/doc_extract.R)

# ---------------------------------------------------------------------------
# Geometry helpers
# ---------------------------------------------------------------------------

# .doc_num(v, dflt) -- one finite number out of whatever YAML handed us.
.doc_num <- function(v, dflt = NA_real_) {
  v <- suppressWarnings(as.numeric(v)[1])
  if (length(v) != 1L || is.na(v) || !is.finite(v)) dflt else v
}

# .doc_int(v, dflt) -- one finite integer, same contract.
.doc_int <- function(v, dflt = NA_integer_) {
  v <- suppressWarnings(as.integer(v)[1])
  if (length(v) != 1L || is.na(v)) dflt else v
}

# .doc_frame(tmpl) -- the point-space a document template's boxes were drawn in.
# Identical idea to pdf_band_frame(): the boxes are absolute coordinates, so a
# page of a different physical size has to be mapped into this frame before they
# mean anything. Defaults to A4, which is what every drawn box will be unless the
# template says otherwise.
.doc_frame <- function(tmpl) {
  # .doc_key, not $: `tmpl` is an optional argument that several callers pass
  # positionally, and propose_pairs(input, 1L) -- which reads as "page 1" and is
  # not -- put a bare integer here. `1L$ref_width` is an ERROR, so a caller's slip
  # became a crash in the middle of an extraction instead of a default frame.
  list(width  = .doc_num(.doc_key(tmpl, "ref_width"),  .A4_W),
       height = .doc_num(.doc_key(tmpl, "ref_height"), .A4_H))
}

# .doc_npages(input) -- how many pages this document has.
#
# read_pdf() puts the count at input$page_count; read_input(), which is what the
# app and the front door actually call, moves it to input$meta$page_count and
# leaves the top level empty. Reading only one of them means the count is right
# in the tests and NULL in production, which is the worst way round.
.doc_npages <- function(input) {
  n <- .doc_int(.doc_key(input, "page_count"))
  if (is.na(n)) n <- .doc_int(.doc_key(.doc_key(input, "meta"), "page_count"))
  if (is.na(n) || n < 1L) n <- length(.doc_key(input, "words") %||% list())
  as.integer(n)
}

# .doc_page_words(input, page, tmpl) -- one page's word boxes, mapped into the
# template's frame, with the centre coordinates every rule below works from.
# Returns NULL when the page has no readable words.
.doc_page_words <- function(input, page, tmpl = NULL) {
  wl <- input$words %||% list()
  if (is.na(page) || page < 1L || page > length(wl)) return(NULL)
  w <- wl[[page]]
  if (is.null(w) || !nrow(w)) return(NULL)
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  if (!is.null(tmpl)) {
    pw <- (input$page_width  %||% rep(NA_real_, length(wl)))[page]
    ph <- (input$page_height %||% rep(NA_real_, length(wl)))[page]
    fr <- .doc_frame(tmpl)
    # AN ORIENTATION IS NOT A SIZE DIFFERENCE.
    #
    # The frame exists so a template drawn on an A4 page still reads a Letter
    # one: the same page, slightly bigger, scaled to fit. A LANDSCAPE page in a
    # portrait frame is not that. Scaling it stretches every word sideways and
    # squashes it vertically, so every column band on that page claims the wrong
    # words -- and a report with a landscape appendix in it, or a portrait cover
    # on a landscape report, is an ordinary thing, not an exotic one. (Found on a
    # corpus of other people's PDFs: three of eighty-one mixed the two, and every
    # table on the odd page out read differently from the way it was proposed.)
    #
    # Such a page is read in its own space and left alone. It is the only honest
    # answer available: the boxes were drawn on a page shaped the other way, so
    # there is no scale that maps one onto the other.
    same_way <- !is.finite(pw) || !is.finite(ph) ||
      identical(pw >= ph, .doc_num(fr$width, 1) >= .doc_num(fr$height, 1))
    if (same_way) w <- .words_to_band_frame(w, fr, pw, ph)
  }
  w$text <- as.character(w$text)
  # A LONE VERTICAL BAR IS A BORDER, not a value. A condensed table prints one
  # between every pair of columns, and it arrives as an ordinary word box: it ends
  # up inside the cell beside it ("| 1,240.55" was measured), and it fills the
  # gutter that would otherwise separate the two columns. Dropping it here, at the
  # one point both the proposer and the reader come through, does both jobs at
  # once -- the columns separate on real whitespace and the cells hold only what
  # the document says. Only a bar ON ITS OWN: "A|B" is data.
  w <- w[!is.na(w$text) & nzchar(trimws(w$text)) &
           !grepl("^\\|+$", trimws(w$text)), , drop = FALSE]
  if (!nrow(w)) return(NULL)
  w$cx <- w$x + w$width / 2
  w$cy <- w$y + w$height / 2
  w$page <- as.integer(page)
  w
}

# .doc_key(x, k) -- x[[k]], and ONLY x[[k]].
#
# R's `$` partial-matches on lists. A pair spec carries `label_text`, so `sp$label`
# on a pair that has no label BOX silently returns the label's WORDING -- a
# character vector -- and the next line, `lb$page`, dies with "$ operator is
# invalid for atomic vectors". That is the whole extraction gone, on a template
# whose only sin was describing a value by its wording instead of by two boxes.
#
# It is not hypothetical and it is not confined to this pair: every one of these
# structures comes out of YAML, where a key can be added at any time, and the day
# somebody adds `page_hint` beside `page` the same trap opens under `$page`. So
# the fields of a spec are read by exact name, here, once.
.doc_key <- function(x, k) if (is.list(x) && k %in% names(x)) x[[k]] else NULL

# .doc_value_kind(v) -- money, date or text, decided by READING the value.
#
# THE MATCH HAS TO BE THE WHOLE VALUE. Both matchers find a pattern ANYWHERE in a
# string, which is right when pulling a figure off a line ("Closing balance
# $875.20") and wrong when deciding what a value IS. A reference number with a
# date-shaped run inside it gets typed as a date, and the extractor then returns
# the date-shaped fragment and drops the rest of the reference. A reference
# silently two characters short is exactly the kind of wrong answer that looks
# right.
.doc_value_kind <- function(v) {
  v <- trimws(as.character(v %||% ""))
  if (!nzchar(v)) return("text")
  m <- .value_from_line(v, "money")
  if (!is.na(m) && identical(trimws(m), v)) return("money")
  d <- .value_from_line(v, "date")
  if (!is.na(d) && identical(trimws(d), v)) return("date")
  "text"
}

# .doc_looks_amount(t) -- is this word a FIGURE rather than a word?
#
# Deliberately narrower than the money matcher, because it is used to throw
# things away. A thousands separator or exactly two decimal places is an amount
# in any locale that prints either; a bare run of digits is not, because "2012"
# is a perfectly good column heading on a position report and "0" is a perfectly
# good cell. Getting that wrong the other way renames a column after nothing.
.doc_looks_amount <- function(t) {
  t <- trimws(as.character(t %||% ""))
  if (!nzchar(t)) return(FALSE)
  grepl("^[-+(]?[0-9]{1,3}([,. ][0-9]{3})+([.,][0-9]+)?[)]?$", t) ||
    grepl("^[-+(]?[0-9]+[.,][0-9]{2}[)]?$", t) ||
    !is.na(.value_from_line(t, "money"))
}

# .doc_norm(s) -- the comparison form of a piece of wording: lowercase, letters
# and digits only. Every text anchor in this file is compared this way, so a
# heading that gained a colon, a stray space or a different case still matches.
.doc_norm <- function(s) gsub("[^a-z0-9]+", "", tolower(as.character(s %||% "")))

# ---------------------------------------------------------------------------
# Rows: grouping words into visual lines, with the line pitch LEARNED
# ---------------------------------------------------------------------------

# .doc_row_tol(ys, declared) -- how far apart two words' tops may be and still be
# the same row.
#
# WHY IT IS LEARNED AND NOT A CONSTANT. The statement parser's fixed 3pt tolerance
# is right for the one page-shape it reads. Across thirty tables in one report the
# line pitch varies from about 10pt (a dense schedule) to 24pt (a summary set in a
# larger face), and a tolerance that is too big MERGES rows -- two transactions
# reported as one, the exact silent corruption this engine exists to prevent --
# while one that is too small SPLITS a row whose cells sit on slightly different
# baselines. So: group once at the conservative statement tolerance, measure the
# gap between the row tops that produces, and take a fraction of that as the real
# tolerance. The measurement is per table, because that is the scope over which
# the pitch is actually constant.
.doc_pitch <- function(ys, q = 0.5) {
  ys <- sort(ys[is.finite(ys)])
  if (length(ys) < 3L) return(NA_real_)
  g <- .group_rows(ys, PARAM_PDF_ROW_TOL)
  tops <- as.numeric(tapply(ys, g, min))
  dif <- diff(sort(tops))
  dif <- dif[is.finite(dif) & dif > 0]
  if (!length(dif)) return(NA_real_)
  p <- as.numeric(stats::quantile(dif, probs = q, names = FALSE, type = 7))
  if (!is.finite(p) || p <= 0) NA_real_ else p
}

# .doc_is_wrap(d, prev_left, gap, ...) -- is this line the TAIL of the row above
# it rather than a row of its own?
#
# A description too long for its column wraps onto a second physical line, and
# that line has no date, no reference and no amount -- just more words, indented
# under the column it belongs to. Three facts identify it and all three are
# needed: it starts to the RIGHT of where the rows start, it sits within about one
# line of the row above, and it fills only one column. A genuinely sparse row (a
# sub-item, a continuation of a schedule with its own amount) fills two or more,
# so it is emitted as the row it is.
#
# Getting this wrong in the other direction is what a tighter row tolerance costs:
# once the wrap is correctly a separate LINE, a reader with no fold turns a
# four-row table into no table at all, because the one-cell line breaks the run.
#
# A WRAP IS NOT ALWAYS INDENTED, and requiring it to be was a real fault. The
# indent rule comes from a bank statement, where the description column is set in
# from the date and its second line sits under the words rather than under the
# date. A REPORT left-aligns the wrapped line under the first, at exactly the same
# x -- so `min(d$x) > prev_left + 4` was false and every wrapped line came back as
# its own row. Reported as "one row with data on 3 different lines coming back as
# 3 rows", on a two-column table.
#
# So indentation is one of TWO sufficient shapes, not a requirement:
#   indented past the row's left edge  -> a generous gap is still a wrap
#   at the SAME left edge              -> a wrap only under a tighter gap
#
# BOTH SHAPES CARRY THE SAME TWO STRUCTURAL GUARDS, and for a while only one did.
# The indented branch had NO guards at all, which is a bigger hole than it looks:
# a right-aligned figure printed alone on a line is "indented" BY CONSTRUCTION,
# so an unlabelled total under the last row of a schedule was folded into that
# row. Measured on a three-row schedule with a total of 6,000.00 under it: three
# rows came back, the last one holding "3,000.00 6,000.00" in its Value cell -
# and the parsed companion took the total, so the row's own figure was gone and
# the total had vanished as a row. Two wrong figures from one fold, both looking
# perfectly ordinary. That is the exact mirror of the fault above it.
#
# The guards, and each one is load bearing:
#
#  1. TIGHT GAP. No further apart than one line of type. A genuine sparse row (a
#     sub-heading, a total set apart) is printed with MORE air above it, not less.
#  2. THE TABLE HAS MORE THAN ONE COLUMN. A wrap is a cell overflowing its column,
#     which is not a thing that can happen when there is only one. Without this,
#     a one-column table of PROSE -- every line left-aligned at the same x, a
#     leading apart, filling the one column there is -- folds into a single row.
#     Measured: a 14-line block became 1.
#  3. THE ROW ABOVE FILLED MORE THAN ONE COLUMN. That is what makes this line the
#     overflow of a real row rather than the second line of a list.
#
# WHAT "ONE LINE" IS MEASURED AGAINST, and why it is not a page statistic.
#
# This used to be handed `pitch` -- .doc_pitch(w$y, q = 0.25), the lower quartile
# of the gaps between 3pt y-groups over EVERY word in the reading window. Two
# ordinary things put values into that sample that are not leadings at all:
#
#   * .doc_pitch groups at a fixed 3pt while .doc_lines groups the same words at
#     the ADAPTIVE .doc_row_tol, so any word sitting 4pt or more off its own
#     line's top -- a label in a larger face beside a smaller value, a
#     superscript, a bullet -- injects a phantom "gap" of 4-8pt;
#   * an open-ended window is the whole page, so a footer or a footnote block set
#     tighter than the table contributes ITS leading to the sample.
#
# Either one drags the quartile below the table's real leading, the ceiling falls
# under it, and EVERY wrap is rejected -- which is "one row with data on 3
# different lines coming back as 3 rows". The predicate was sound; the number
# handed to it was measured over the wrong population.
#
# So it is read off the two lines themselves: the type height of this line plus
# that of the physical line above it. In plain words the rule becomes "a
# one-column line sitting under the row with no room for a blank line between
# them", which is what a person reading the page sees, and it cannot be knocked
# over by a footnote below the table or by a heading set in a bigger face.
.doc_is_wrap <- function(d, prev_left, gap, n_filled = 1L,
                         n_cols = 2L, prev_filled = 2L, prev_height = NA_real_) {
  if (!is.finite(prev_left) || !is.finite(gap)) return(FALSE)
  if (n_filled > 1L || gap <= 0) return(FALSE)
  h  <- suppressWarnings(max(as.numeric(d$height %||% NA_real_), na.rm = TRUE))
  ph <- .doc_num(prev_height, NA_real_)
  # No type height to measure against (a caller that never carried one) -- fall
  # back to this line's own height twice over, and if there is not even that,
  # refuse rather than guess. A wrong fold merges two real rows into one.
  if (!is.finite(h)) h <- ph
  if (!is.finite(ph)) ph <- h
  if (!is.finite(h) || !is.finite(ph) || h <= 0) return(FALSE)
  span <- h + ph
  if (n_cols < 2L || prev_filled < 2L) return(FALSE)
  if (min(d$x) > prev_left + 4) return(gap <= span * 1.4)
  gap <= span * 1.15
}

# .doc_is_rule(d) -- is this line a printed RULE rather than a row of data?
#
# Plenty of reports draw their table borders with CHARACTERS rather than vector
# strokes: "+--------+--------+", a row of underscores under a total, a line of
# dots leading to a page number. Those arrive from pdf_data() as ordinary word
# boxes, and they do two kinds of damage: a rule spanning the full width merges
# every column band into one (measured: a bordered table proposed ZERO columns),
# and a rule between rows breaks the run of lines that identifies a table.
#
# A rule is a line whose text, with the spaces taken out, is made ONLY of the
# characters used to draw one, and is long enough not to be a stray dash in a
# cell. Anything with a letter or a digit in it is data.
.doc_is_rule <- function(d) {
  t <- gsub("[[:space:]]", "", paste(as.character(d$text), collapse = ""))
  nchar(t) >= 3L && !grepl("[^-=_+|~.]", t)
}

.doc_row_tol <- function(ys, declared = NULL) {
  d <- .doc_num(declared)
  if (!is.na(d) && d > 0) return(d)
  # THE LOWER QUARTILE, NOT THE MEDIAN, and this is the whole point of the
  # function. A table whose row heights alternate -- 10pt, 26pt, 10pt, 26pt, which
  # is what a schedule with a blank line between pairs looks like -- has a median
  # gap of 26. Six tenths of that is 15.6, which SWALLOWS every 10pt gap: measured
  # on the uneven-rows fixture, six rows came back as four, with "R2 R3" and
  # "20.00 30.00" in single cells. Two rows silently merged is the worst thing
  # this engine can do. The lower quartile tracks the TIGHTEST spacing the table
  # actually uses, so the tolerance is safe for every row rather than for the
  # average one, and on an evenly-set table the two are the same number.
  pitch <- .doc_pitch(ys, q = 0.25)
  if (is.na(pitch)) return(as.numeric(PARAM_PDF_ROW_TOL))
  # 0.6 of the pitch: comfortably more than the baseline jitter within one row,
  # comfortably less than the distance to the next one. Floored at the statement
  # tolerance so this can never be LESS forgiving than the proven parser.
  #
  # THE CAP IS 14, AND IT IS THE INTERESTING NUMBER. Baseline jitter inside one
  # printed row is a couple of points; 14 is already generous for that. What the
  # cap protects against is the other direction: a widely-leaded page (a title
  # block set at 40pt) hands back a pitch of 40, and 0.6 of that would swallow a
  # 22pt gap -- merging a label and the value printed under it into one line and
  # losing the pair entirely. A table whose rows genuinely stand further apart
  # than this, or one whose descriptions wrap, sets row_tol explicitly.
  max(as.numeric(PARAM_PDF_ROW_TOL), min(pitch * 0.6, 14))
}

# .doc_lines(w, tol) -- one page's words grouped into visual lines, in reading
# order. Returns a list of data.frames, each the words of one line ordered left to
# right, with attr "top" = the line's highest word top.
.doc_lines <- function(w, tol = NULL) {
  if (is.null(w) || !nrow(w)) return(list())
  w <- w[order(w$y, w$x), , drop = FALSE]
  tol <- .doc_row_tol(w$y, tol)
  g <- .group_rows(w$y, tol)
  lapply(split(seq_len(nrow(w)), g), function(ix) {
    d <- w[ix, , drop = FALSE]
    d <- d[order(d$x), , drop = FALSE]
    attr(d, "top") <- min(d$y)
    d
  })
}

# .doc_line_text(d) -- a line's words as one string, in reading order.
.doc_line_text <- function(d) paste(d$text, collapse = " ")

# .doc_join_wrapped(a, b) -- close a LINE BREAK between two fragments of the same
# cell.
#
# A space is right nearly always: a description wrapped onto a second line is two
# words, and "Payment to" + "Acme Ltd" is "Payment to Acme Ltd". It is wrong when
# the break falls inside ONE token, which is what a PDF does to an email address,
# a URL, a long reference or a hyphenated word:
#
#     xys@            ->  "xys@ gmail.com"   the reported fault
#     gmail.com           "xys@gmail.com"    what is printed
#
# So: no space when the last character of the first fragment is a joining
# character ATTACHED TO A WORD (`@ / \ _ = + -`), or when the second fragment
# OPENS with one (`@ . / \`) followed by a word character. Everything else keeps
# its space, which is why a trailing full stop is deliberately not on the list --
# "Acme Ltd." + "Payment received" is a sentence boundary far more often than it
# is a broken token, and joining those is the worse mistake.
.doc_join_wrapped <- function(a, b) {
  a <- as.character(a)[1]; b <- as.character(b)[1]
  if (is.na(a) || !nzchar(a)) return(b)
  if (is.na(b) || !nzchar(b)) return(a)
  if (grepl("[A-Za-z0-9][@/\\\\_=+-]$", a) || grepl("^[@./\\\\][A-Za-z0-9]", b))
    return(paste0(a, b))
  paste(a, b)
}

# .doc_join_lines(words) -- the text of a set of word boxes that may span several
# lines: words within a line joined by spaces, LINES joined by the rule above.
# Used wherever a box is read whole, so a value box drawn round a wrapped email
# reads the same as a wrapped cell does.
.doc_join_lines <- function(w, tol = NULL) {
  if (is.null(w) || !NROW(w)) return("")
  lns <- .doc_lines(w, tol)
  if (!length(lns)) return("")
  txt <- vapply(lns, function(d) paste(as.character(d$text), collapse = " "), character(1))
  Reduce(.doc_join_wrapped, txt)
}

# ---------------------------------------------------------------------------
# Anchoring: find the table by its WORDING, fall back to its POSITION
# ---------------------------------------------------------------------------

# .doc_header_hit(d, need) -> fraction of the header wordings present on line `d`.
# Substring on the normalised forms, so "Opening" matches a header cell printed
# "Opening balance", and a header split across two word boxes still matches
# because the whole LINE is what gets searched.
.doc_header_hit <- function(d, need) {
  need <- as.character(unlist(need %||% character(0)))
  need <- need[!is.na(need) & nzchar(trimws(need))]
  if (!length(need)) return(0)
  hay <- .doc_norm(.doc_line_text(d))
  if (!nzchar(hay)) return(0)
  mean(vapply(need, function(n) {
    k <- .doc_norm(n); nzchar(k) && grepl(k, hay, fixed = TRUE)
  }, logical(1)))
}

# .doc_find_header(lines, need, min_hit) -> list(index, top, hit) for the BEST
# line on the page, or NULL. The best is the highest-scoring line; on a tie the
# FIRST one wins, so a repeated header always resolves to the top of the table
# rather than to a total row that happens to echo the wording.
.doc_find_header <- function(lines, need, min_hit = 0.6) {
  if (!length(lines)) return(NULL)
  hits <- vapply(lines, .doc_header_hit, numeric(1), need = need)
  if (!length(hits) || !any(is.finite(hits))) return(NULL)
  best <- which.max(hits)
  if (!length(best) || hits[best] < min_hit) return(NULL)
  list(index = as.integer(best), top = attr(lines[[best]], "top"),
       hit = as.numeric(hits[best]))
}

# ---------------------------------------------------------------------------
# THE HEADING AS **THIS** DOCUMENT PRINTS IT.
#
# Reported: "sometimes there's headers on the next x pages that are being pulled
# as rows."
#
# On a continuation page exactly ONE thing used to close the top of the reading
# window: a >=0.6 wording match of the template's remembered anchor$header_text
# against a single printed line. There was no second answer. When that one test
# missed, the window fell back to band$y_min - which nothing in the product ever
# writes, so it is 0, the very top of the paper - and `skip` was hard-coded to 0
# for every page after the first. The window became "the whole page", and every
# line printed above the data, the reprinted heading included, was read as a row.
#
# The commonest way to miss it needs no bad luck at all: a table with no figures
# in it is marked has_header = FALSE by the proposer, which writes header_rows: 0
# and anchor.header_text: [] (R/tables_detect.R), so the wording test has nothing
# to match and can never fire.
#
# So stop asking the TEMPLATE what the heading says and ask the DOCUMENT. On the
# table's own first page the tool already knows which lines are the heading -
# they are the `skip` lines at the top of that window. Their text is the
# heading's signature for THIS copy. It is learned at read time, so it is right
# even when the template was drawn on a different copy, and it needs nothing on
# screen to answer.
#
# Two guards stop it ever deleting a real row:
#   * the signature is REFUSED if any learned heading line carries a figure - a
#     line with money in it is data, and a signature made of data would match
#     data;
#   * the match is WHOLE-LINE, not a fraction, so a data row cannot contain the
#     entire heading string by accident.

# .doc_header_printed(lines, y_from, n) -> the normalised text of the n heading
# lines at or below y_from, or character(0) when they cannot be trusted as one.
.doc_header_printed <- function(lines, y_from, n) {
  n <- .doc_int(n, 0L)
  if (!length(lines) || is.na(n) || n < 1L) return(character(0))
  tops <- vapply(lines, function(d) .doc_num(attr(d, "top"), NA_real_), numeric(1))
  ix <- which(is.finite(tops) & tops >= .doc_num(y_from, -Inf) - 1)
  if (!length(ix)) return(character(0))
  ix <- utils::head(ix[order(tops[ix])], n)
  for (i in ix) if (.doc_has_money(lines[[i]])) return(character(0))
  # unname: `which()` carries the names of whatever it indexed, and a signature is
  # a plain vector of strings -- a stray names attribute makes two identical
  # signatures compare unequal.
  sig <- unname(vapply(ix, function(i) .doc_norm(.doc_line_text(lines[[i]])),
                       character(1)))
  # A signature of two or three characters would match half the page.
  sig <- sig[nchar(sig) >= 6L]
  if (!length(sig)) character(0) else sig
}

# .doc_find_printed_header(lines, sig) -> list(index, top, n_lines) for the first
# run of lines whose whole normalised text matches the signature, or NULL. Only
# ever consulted after the wording test has already returned NULL.
.doc_find_printed_header <- function(lines, sig) {
  if (!length(lines) || !length(sig)) return(NULL)
  norm <- vapply(lines, function(d) .doc_norm(.doc_line_text(d)), character(1))
  for (i in seq_along(lines)) {
    if (!nzchar(norm[i])) next
    # How many of the signature's lines are reprinted here, in order. A page that
    # reprints only the first line of a two-line heading must skip ONE line, not
    # two, or a data row is lost.
    k <- 0L
    while (k < length(sig) && (i + k) <= length(lines) &&
           nzchar(norm[i + k]) &&
           (identical(norm[i + k], sig[k + 1L]) ||
            grepl(sig[k + 1L], norm[i + k], fixed = TRUE))) k <- k + 1L
    if (k >= 1L)
      return(list(index = as.integer(i), top = attr(lines[[i]], "top"),
                  n_lines = as.integer(k), hit = 1))
  }
  NULL
}

# ---------------------------------------------------------------------------
# WHERE THIS COPY PRINTS THE TABLE, SIDEWAYS.
#
# A table is found by its heading, which moves the reading WINDOW vertically to
# wherever that heading turned up. Nothing moved it HORIZONTALLY: the column
# bands stayed exactly where they were drawn on the one example document, in
# absolute page points. So a template could find a table perfectly and still read
# every value into the wrong column.
#
# Measured on a two-column table whose bands were drawn at 55-150 and 195-260,
# read on copies where the table sits further right:
#
#   shift  located by  confidence  what came out
#     0pt  header      1.00        ABC/100  DEF/250  GHI/375
#    40pt  header      1.00        ABC/100  DEF/250  GHI/375
#    60pt  header      1.00        ABC/NA   DEF/NA   GHI/NA
#   120pt  header      1.00        NA/ABC   NA/DEF   NA/GHI     <- Code in Units
#
# At 120pt the table is found with confidence 1.00 and Units reads as a perfectly
# full column of clean values, every one of them wrong. That is the cardinal
# failure this product exists to prevent, and it is exactly the case a template
# built from many examples meets every day.
#
# The correction is free at the moment of the match. The heading LINE is in hand,
# with its words and their x positions ON THIS DOCUMENT; the column names came
# off the heading words on the example. So: match each column's name to the cell
# of this heading that carries it, and the median difference between where those
# cells sit and where the bands sit is how far the table has moved.
#
# THREE REFUSALS, because a wrong shift is worse than none:
#   * fewer than two columns matched -- one agreement is a coincidence;
#   * the columns disagree by more than a few points -- then the table is not the
#     same shape here and moving the bands would make it worse, not better;
#   * EVERY heading cell already falls inside its own band -- the table has not
#     moved, so nothing is touched.
# Every one of those returns 0, which is today's behaviour exactly.
#
# That last refusal is what makes the measure trustworthy, and it is not
# obvious. A band is drawn WIDER than the heading word inside it, and not
# symmetrically, so the raw difference between a heading cell's centre and its
# band's centre is the sum of two things: how far the table moved (what we want)
# and how the band happened to be drawn around its heading on the example (which
# we do not). Measured on a table that had not moved at all, that constant was
# -15pt -- so a naive reading would shove every band 15pt left on a document that
# was already perfect. Asking "is anything actually outside its band?" first
# separates them: no, and there is nothing to correct; yes, and the raw median is
# a good enough estimate to bring every cell comfortably back inside.

# .doc_band_shift(d, cols) -> how far right (positive) this document prints the
# table, compared with where the template's bands sit. 0 when it cannot tell.
.doc_band_shift <- function(d, cols) {
  if (is.null(d) || !NROW(d) || !length(cols)) return(0)
  cells <- .doc_cells(d, .doc_gap_for(d))
  if (length(cells) < 2L) return(0)
  ctext <- vapply(cells, function(z) .doc_norm(paste(z$text, collapse = " ")),
                  character(1))
  ccx   <- vapply(cells, function(z)
    mean(c(min(z$x), max(z$x + .doc_num(z$width, 0)))), numeric(1))
  used <- rep(FALSE, length(cells))
  diffs <- numeric(0); outside <- logical(0)
  for (j in seq_along(cols)) {
    nm <- .doc_norm(as.character(cols[[j]]$name %||% ""))
    lo <- .doc_num(cols[[j]]$x_min, NA_real_); hi <- .doc_num(cols[[j]]$x_max, NA_real_)
    if (!nzchar(nm) || !is.finite(lo) || !is.finite(hi)) next
    # The cell that IS this column's heading. Exact first, then a containment
    # either way ("Unit price" printed as one cell, "Units" named "Units ").
    hit <- which(!used & nzchar(ctext) & ctext == nm)
    if (!length(hit))
      hit <- which(!used & nzchar(ctext) &
                     (vapply(ctext, function(t) grepl(t, nm, fixed = TRUE), logical(1)) |
                      vapply(ctext, function(t) grepl(nm, t, fixed = TRUE), logical(1))))
    if (length(hit) != 1L) next     # ambiguous is the same as unknown
    used[hit] <- TRUE
    diffs <- c(diffs, ccx[hit] - (lo + hi) / 2)
    outside <- c(outside, ccx[hit] < lo || ccx[hit] > hi)
  }
  if (length(diffs) < 2L) return(0)
  if (!any(outside)) return(0)      # it already fits -- leave it alone
  m <- stats::median(diffs)
  if (max(abs(diffs - m)) > 12) return(0)
  if (abs(m) < 2) return(0)
  as.numeric(m)
}

# ---------------------------------------------------------------------------
# A COLUMN THE DOCUMENT GAINED OR LOST, WHICH IS NOT A SHIFT.
#
# .doc_band_shift above moves every band by the SAME amount, which is what a
# table printed further across the page needs. A comparative table that gains a
# period column is a different animal entirely: it is right-anchored to the page,
# so a new Q5 pushes Q1..Q4 and Total one place LEFT and the bands stay where
# they are. Measured on a four-quarter table given a fifth:
#
#   Q1 held Q2's figure ... Total held Q5's, and every signal said perfect --
#   rows 2, unclaimed 0, low_fill FALSE on all six, found by its heading,
#   confidence 1.00, status ok.
#
# Nothing arithmetic can catch that on this route. But the evidence is sitting
# right there: the tool is holding the heading LINE of this copy, and
# doc_header_names already says which heading word falls inside which band. Read
# them and compare, name by name, with the names the template gave the columns.
# A band whose heading is not the heading it was drawn for is not over that
# column any more.
#
# IT IS REPORTED, NOT REPAIRED. A remapping guessed from a header row would be
# the tool inventing which quarter is which, and getting that wrong produces
# exactly the failure it was meant to prevent. So what comes back is the
# disagreement itself: which column, and what its band is actually sitting under.
#
# Only ever asked of a table found BY ITS HEADING - a table read from where it
# sat has no heading in hand to compare against, and would report every column as
# a disagreement on nothing.

# .doc_heading_disagreements(input, tab, tmpl, cols, loc) -> a two-column frame of
# (column, heading) for every band now sitting under a different heading.
.doc_heading_disagreements <- function(input, tab, tmpl, cols, loc) {
  none <- data.frame(column = character(0), heading = character(0),
                     stringsAsFactors = FALSE)
  if (!identical(loc$anchor, "header") || !length(loc$windows)) return(none)
  want <- .doc_column_names(tab)
  if (length(want) < 2L || length(cols) != length(want)) return(none)
  tab2 <- tab; tab2$columns <- cols
  edges <- doc_column_edges(tab2)
  if (is.null(edges) || length(edges) - 1L != length(want)) return(none)
  got <- tryCatch(doc_header_names(input, tab2, tmpl, edges, loc),
                  error = function(e) character(0))
  if (length(got) != length(want)) return(none)
  same <- function(a, b) {
    a <- .doc_norm(a); b <- .doc_norm(b)
    nzchar(a) && nzchar(b) && (grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE))
  }
  # A COLUMN MAY BE CALLED WHATEVER ITS AUTHOR CALLED IT. "the heading here is not
  # this column's name" is NOT evidence of a moved band - a person naming five
  # date columns M1..M5, which the shipped fixture does, is naming them, not
  # mis-drawing them, and reporting that as a fault on every ordinary read is how
  # a signal becomes noise nobody looks at.
  #
  # What IS evidence, and is not deniable, is a band sitting under a heading that
  # belongs to ANOTHER of this table's columns. That is the shifted-by-one shape
  # exactly, and nothing else produces it.
  bad <- vapply(seq_along(want), function(j) {
    if (!nzchar(trimws(got[j])) || same(want[j], got[j])) return(FALSE)
    any(vapply(want[-j], function(o) same(o, got[j]), logical(1)))
  }, logical(1))
  if (!any(bad)) return(none)
  data.frame(column = want[bad], heading = trimws(got[bad]),
             stringsAsFactors = FALSE)
}

# .doc_shift_cols(cols, dx) -> the same columns, moved.
.doc_shift_cols <- function(cols, dx) {
  dx <- .doc_num(dx, 0)
  if (!is.finite(dx) || dx == 0 || !length(cols)) return(cols)
  lapply(cols, function(cc) {
    if (is.finite(.doc_num(cc$x_min, NA_real_))) cc$x_min <- .doc_num(cc$x_min) + dx
    if (is.finite(.doc_num(cc$x_max, NA_real_))) cc$x_max <- .doc_num(cc$x_max) + dx
    cc
  })
}

# .doc_find_first_column(lines, need, col) -> the top of the first line whose
# FIRST-COLUMN band carries one of the remembered row labels, or NULL. This is
# the second anchor: a table whose header wording changed (or was never printed)
# is still identifiable by the rows it always starts with.
.doc_find_first_column <- function(lines, need, col) {
  need <- .doc_norm(as.character(unlist(need %||% character(0))))
  need <- need[nzchar(need)]
  if (!length(need) || is.null(col)) return(NULL)
  for (i in seq_along(lines)) {
    d <- lines[[i]]
    inband <- d$cx >= .doc_num(col$x_min, -Inf) & d$cx <= .doc_num(col$x_max, Inf)
    if (!any(inband)) next
    k <- .doc_norm(paste(d$text[inband], collapse = " "))
    if (nzchar(k) && any(vapply(need, function(n) grepl(n, k, fixed = TRUE), logical(1))))
      return(list(index = as.integer(i), top = attr(d, "top")))
  }
  NULL
}

# The four ways a table can be found, WORST last, each with the confidence the
# result is reported at. These are not probabilities; they are an ordering the
# screen turns into words ("found by its heading" / "read from where it was on the
# example"), and the whole point of keeping them distinct is that the last two
# deserve a look and the first does not.
.DOC_ANCHOR_RANK <- c(header = 1L, first_column = 2L,
                      position_same_page = 3L, position_other_page = 4L,
                      not_found = 5L)
.doc_anchor_confidence <- function(anchor, hit = NA_real_) {
  switch(anchor,
    header             = if (is.na(hit)) 0.9 else max(0.6, min(1, hit)),
    first_column       = 0.7,
    position_same_page = 0.4,
    position_other_page = 0.2,
    not_found          = 0,
    0.2)
}

# ---------------------------------------------------------------------------
# THE FIFTH OUTCOME: **THIS TABLE IS NOT HERE**.
#
# The ladder used to be header -> first_column -> POSITION, and the last rung
# always succeeded: p0 was clamped into range and a window opened regardless. So
# a table the document does not contain came back FULL -- of whatever ink sits at
# the coordinates it was drawn at. Measured on a template of thirty-five tables
# run against a document carrying one: thirty-four copies of that one table's row,
# each under a different table's name and columns, with `empty` counting zero and
# the workbook looking complete. That is the cardinal failure of this product --
# a wrong figure that looks right -- produced by the reader on its own.
#
# So there is now a way to say no. `anchor = "not_found"` carries confidence 0 and
# NO windows, which means doc_table_rows reads nothing, the table comes back with
# zero rows, and convert_tables already reports that as "those tables were not
# found on this document". Nothing new on any screen.
#
# It is returned in exactly two situations, and both are refusals of a GUESS:
#
#   * the table declares evidence -- a remembered heading wording, or the row
#     labels it starts with -- and NONE of it is on the page. The template said
#     what to look for and it is not there. (A table that declares neither has no
#     way to say "this is not it", so position remains all there is for it and
#     nothing about it changes.)
#   * the page it is declared to start on is past the end of this document. The
#     clamp used to put it on the LAST page and read whatever ink was there --
#     including for tables that ARE present elsewhere and would have matched by
#     heading, which is how a 12-page template read every one of its tables off
#     page 2 of a 2-page document.
.doc_no_table <- function(detail) {
  list(pages = integer(0), windows = list(), anchor = "not_found",
       confidence = .doc_anchor_confidence("not_found"),
       detail = as.character(detail)[1], extended_to = NA_integer_,
       header_tops = list(), header_sig = character(0), band_shift = 0)
}

# ---------------------------------------------------------------------------
# WHICH PAGE IS THIS TABLE ON -- asked of the whole document, not of one page.
#
# Reported: "A number of these reports will NOT have consistent page numbers...
# there may be x number of pages in between", and "it CANNOT assume that tables
# will be in the same place - what it's doing is saying hey I saw these here
# last time. NOT helpful when we're getting SIGNIFICANTLY differing reports."
#
# That was exactly it. The reader took `tab$start$page` and read THAT PAGE, so a
# report with two extra pages in the middle lost every table below the insert -
# measured on the fixture: insert two pages and all three tables come back
# "not found on this document".
#
# A TABLE'S IDENTITY IS ITS NAME PLUS ITS COLUMN NAMES. Both are already
# captured: the builder's first gesture is "drag a box round the table's TITLE"
# and its wording becomes tab$name, and "drag a box round the ROW OF COLUMN
# NAMES" fills anchor$header_text. The title was never used to find anything -
# it only labelled the sheet. Now the two together are what is searched for, on
# every page, and the declared page is a preference for breaking ties rather
# than a requirement.
#
# THE SCORE, out of 2: the fraction of the column names printed on one line
# (the same .doc_find_header the reader already trusts, so the bar is the same
# 0.6), plus 1 if the table's name is printed on that page. Either alone is
# enough to be looked at; both together is what a real match looks like.
.DOC_PAGE_MIN_SCORE <- 0.6

# .doc_page_has_title(lines, title) -- is the table's own name printed here.
# Whole words, normalised, so "Fees charged" finds "Fees charged (GST incl)".
# A one-word or very short name is refused: "Total" or "Notes" appears on half
# the pages of a report and would vote for all of them.
.doc_page_has_title <- function(lines, title) {
  key <- .doc_norm(title)
  if (nchar(key) < 8L) return(FALSE)
  any(vapply(lines, function(d) grepl(key, .doc_norm(.doc_line_text(d)), fixed = TRUE),
             logical(1)))
}

# .doc_page_score(input, tab, tmpl, p) -- how much this page looks like this
# table, out of 2.
.doc_page_score <- function(input, tab, tmpl, p, need, title) {
  lines <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
  if (!length(lines)) return(0)
  h <- if (length(need)) .doc_find_header(lines, need) else NULL
  (if (is.null(h)) 0 else as.numeric(h$hit)) +
    (if (.doc_page_has_title(lines, title)) 1 else 0)
}

# .doc_search_terms(tab) -- what this table can be recognised BY, or NULL.
#
# NOTHING TO SEARCH FOR IS NOT A SEARCH. A table with neither a usable name nor
# remembered column wording can only be found where it was drawn, and pretending
# otherwise would move it to whichever page scored the most noise.
.doc_search_terms <- function(tab) {
  need <- tab$anchor$header_text %||% .doc_column_names(tab)
  need <- as.character(unlist(need %||% character(0)))
  need <- need[!is.na(need) & nzchar(trimws(need))]
  title <- as.character(tab$name %||% "")[1]
  if (!length(need) && nchar(.doc_norm(title)) < 8L) return(NULL)
  list(need = need, title = title)
}

# .doc_page_scores(input, tab, tmpl) -> a score per page, 0 where nothing matched.
.doc_page_scores <- function(input, tab, tmpl = NULL) {
  npg <- .doc_npages(input)
  if (is.na(npg) || npg < 1L) return(numeric(0))
  s <- .doc_search_terms(tab); if (is.null(s)) return(numeric(0))
  vapply(seq_len(npg), function(p)
    .doc_page_score(input, tab, tmpl, p, s$need, s$title), numeric(1))
}

# .doc_find_page(input, tab, tmpl) -> the page this table is on, or NA to leave
# the declared page alone.
#
# THE DECLARED PAGE IS TRIED FIRST AND WINS WHENEVER THE TABLE IS ON IT. This is
# not an optimisation, it is the correctness rule: a report can print two
# DIFFERENT tables under the same column headings - the fixture does, one account
# per page - and searching the whole document for a table that is exactly where
# it was said to be would let the first of them answer for both. Look elsewhere
# only when the table is not where the template left it, which is also the only
# time anybody wanted a search. It keeps the ordinary document at one page
# scored instead of forty.
#
# Ties among the rest go to the page NEAREST the declared one; doc_locate_repeats()
# is what handles a table that is genuinely printed more than once.
.doc_find_page <- function(input, tab, tmpl = NULL) {
  s <- .doc_search_terms(tab); if (is.null(s)) return(NA_integer_)
  npg <- .doc_npages(input); if (is.na(npg) || npg < 1L) return(NA_integer_)
  p0 <- .doc_int(tab$start$page, 1L); if (is.na(p0) || p0 < 1L) p0 <- 1L
  if (p0 <= npg &&
      .doc_page_score(input, tab, tmpl, p0, s$need, s$title) >= .DOC_PAGE_MIN_SCORE)
    return(NA_integer_)
  sc <- vapply(seq_len(npg), function(p)
    .doc_page_score(input, tab, tmpl, p, s$need, s$title), numeric(1))
  if (!any(sc >= .DOC_PAGE_MIN_SCORE)) return(NA_integer_)
  best <- which(sc >= max(sc) - 1e-9)
  as.integer(best[which.min(abs(best - p0))])
}

# doc_locate_repeats(input, tab, tmpl) -> every page this table is printed on,
# best first, for a table set to be read wherever it appears.
#
# "It CAN appear more than once, but not always" - so this returns one page for
# the ordinary document and several for the one that repeats it, and the caller
# does not have to know which kind it is holding.
doc_locate_repeats <- function(input, tab, tmpl = NULL) {
  sc <- .doc_page_scores(input, tab, tmpl)
  if (!length(sc)) return(integer(0))
  hit <- which(sc >= .DOC_PAGE_MIN_SCORE)
  if (!length(hit)) return(integer(0))
  as.integer(hit[order(-sc[hit], hit)])
}

# doc_locate_table(input, tab, tmpl) -> list(
#   pages, windows, anchor, confidence, detail, extended_to, header_tops)
#
# `windows` is one entry per page the table covers: list(page, y_min, y_max),
# in the template's frame. Rows are only ever read inside these windows, which is
# what makes "several tables on one page" work at all.
doc_locate_table <- function(input, tab, tmpl = NULL) {
  npg <- .doc_npages(input)
  p0 <- .doc_int(tab$start$page, 1L); if (is.na(p0) || p0 < 1L) p0 <- 1L
  p1 <- .doc_int(tab$end$page, p0);   if (is.na(p1) || p1 < p0) p1 <- p0
  # A PAGE THIS DOCUMENT DOES NOT HAVE IS NOT THE LAST PAGE. See .doc_no_table:
  # clamping p0 into range read this table off whatever ink the last page happens
  # to carry. The END is still clamped -- a table declared to finish on page 7 of a
  # 5-page copy simply finishes at the bottom of page 5, which is a truncation the
  # follow rule and the row run already handle.
  # WHERE THIS COPY PRINTS IT, before anything is read off a page number. The
  # template's page is where the table WAS; .doc_find_page asks this document
  # where it IS, by the table's name and its column names. A table that spans
  # pages moves as a whole - the same number of pages, starting at the new one.
  # See .doc_find_page for what it refuses to answer.
  moved_from <- NA_integer_
  pf <- .doc_find_page(input, tab, tmpl)
  if (!is.na(pf) && pf != p0) {
    moved_from <- p0
    p1 <- min(p1 + (pf - p0), max(npg, 1L)); p0 <- pf
  }
  if (npg >= 1L && p0 > npg)
    return(.doc_no_table(sprintf(
      "this table is set to start on page %d and this document only has %d page%s",
      p0, npg, if (npg == 1L) "" else "s")))
  p0 <- min(p0, max(npg, 1L)); p1 <- min(p1, max(npg, 1L))

  band <- tab$band %||% list()
  band_top <- .doc_num(band$y_min, 0)
  # THE BOTTOM OF THE PAGE IS THE DOCUMENT'S, NOT A4's.
  #
  # band_bot decides two things that change the answer: whether a window is
  # OPEN-ENDED (and so has to be stopped by watching the rows) and whether a
  # table is a candidate to FOLLOW onto the next page. Falling back to A4 when no
  # frame is declared made both of those wrong for every page that is not A4: a
  # table ending 741pt down a 612pt-tall landscape page was read as ending
  # comfortably short of an 842pt bottom, so its window was treated as exact and
  # the row-run rule never ran. The document knows its own page size; ask it.
  page_h <- suppressWarnings(as.numeric(
    (input$page_height %||% NA_real_)[min(p0, length(input$page_height %||% 1))]))
  if (!is.finite(page_h) || page_h <= 0) page_h <- NA_real_
  band_bot <- .doc_num(band$y_max,
    .doc_num(.doc_key(tmpl, "ref_height"), page_h %||% .A4_H))
  if (is.na(band_bot)) band_bot <- if (is.na(page_h)) .A4_H else page_h
  y_start_declared <- .doc_num(tab$start$y, band_top)
  y_end_declared   <- .doc_num(tab$end$y, band_bot)

  need_hdr <- tab$anchor$header_text %||% .doc_column_names(tab)
  lines0 <- .doc_lines(.doc_page_words(input, p0, tmpl), tab$row_tol)

  n_header_rows <- max(0L, .doc_int(tab$header_rows, 1L))
  cols0 <- .doc_columns(tab)
  anchor <- NULL; hit <- NA_real_; y_start <- y_start_declared
  start_at_header <- FALSE
  h <- .doc_find_header(lines0, need_hdr)
  if (!is.null(h)) {
    anchor <- "header"; hit <- h$hit; y_start <- h$top; start_at_header <- TRUE
  } else {
    fc <- if (length(cols0))
            .doc_find_first_column(lines0, tab$anchor$first_column, cols0[[1]]) else NULL
    if (!is.null(fc)) {
      anchor <- "first_column"
      # The remembered rows are the table's BODY, so back up over the header rows
      # to where the table actually starts -- one line of pitch per header row.
      pitch <- .doc_pitch(if (length(lines0))
                            unlist(lapply(lines0, function(d) d$y)) else numeric(0))
      if (is.na(pitch)) pitch <- as.numeric(PARAM_PDF_ROW_TOL) * 4
      y_start <- fc$top - pitch * n_header_rows - 1
      start_at_header <- n_header_rows > 0L
    } else {
      # NOTHING THE TEMPLATE SAID TO LOOK FOR IS ON THE PAGE. That is the table
      # saying it is not here, and the answer is to say so rather than to read
      # the coordinates blind -- see .doc_no_table.
      sought <- c(as.character(unlist(.doc_key(.doc_key(tab, "anchor"), "header_text")
                                      %||% character(0))),
                  as.character(unlist(.doc_key(.doc_key(tab, "anchor"), "first_column")
                                      %||% character(0))))
      sought <- sought[!is.na(sought) & nzchar(trimws(sought))]
      if (length(sought))
        return(.doc_no_table("this table was not found on this document"))
      # THE DECLARED LENGTH IS WRITTEN AT THE TEMPLATE'S ROOT, and this graded
      # itself on `tab$doc_pages` -- a key nothing has ever written -- so the
      # comparison was always TRUE and the 0.2 tier could never be reached. A
      # per-table override still wins where one is set.
      declared_pages <- .doc_int(.doc_key(tab, "doc_pages"),
                                 .doc_int(.doc_key(tmpl, "doc_pages"), npg))
      anchor <- if (identical(npg, declared_pages))
                  "position_same_page" else "position_other_page"
      y_start <- y_start_declared
      # The declared start is the top of the table as it was DRAWN, which includes
      # its header -- so the same number of leading lines are headers here too.
      start_at_header <- n_header_rows > 0L
    }
  }

  # THE HEADING AS THIS DOCUMENT PRINTS IT, learned from the table's own first
  # page. The `skip` lines at the top of that window ARE the heading, so their
  # text is a signature that works even when the template's remembered wording
  # does not - which is the ordinary case for a table the proposer marked as
  # having no header (header_rows: 0, anchor.header_text: []). See
  # .doc_header_printed for the two guards that stop it eating a real row.
  hdr_sig <- if (start_at_header)
    .doc_header_printed(lines0, y_start, n_header_rows) else character(0)

  # ...and how far sideways this copy prints it. Only ever measured off a heading
  # the tool actually FOUND (anchor == "header"); a table read from the
  # coordinates it was drawn at has nothing to measure against and is left alone.
  band_shift <- if (identical(anchor, "header") && !is.null(h))
    .doc_band_shift(lines0[[h$index]], cols0) else 0

  # THE TABLE IS AS LONG AS IT IS HERE, not as long as it was on the example.
  #
  # "The MOST important thing is to be able to automatically detect the length of
  # each of the tables, and be able to consistently pull that content." A declared
  # end was treated as EXACT, so a schedule of twelve rows on the example and
  # three hundred here was cut off at twelve - and truncation is invisible,
  # because every row that did come out is right.
  #
  # doc_auto_end() has always known how to answer this: it opens the end, READS
  # the table, and reports the bottom of the last row the stop rule kept. It only
  # ever ran in the builder, so the length was measured once and then asserted
  # forever. Now it is asked again on every document.
  #
  # THREE REFUSALS, because the opposite mistake - swallowing whatever follows
  # the table on the page - is just as bad as truncating it:
  #   * it can only ever make the table LONGER. A shorter answer is ignored, so a
  #     document this rule reads badly can never come back with fewer rows than
  #     the exact window would have given.
  #   * it is only asked where the example's end is NOT already the paper's edge.
  #     Those tables are open-ended anyway and the follow rule owns them - which
  #     is also what stops this recursing, since doc_auto_end's own probe opens
  #     the end to the page height.
  #   * `auto_end: false` on the table turns it off for good, for a template whose
  #     end is deliberately mid-page.
  grew_to <- NULL
  if (!isFALSE(.doc_key(tab, "auto_end")) &&
      is.finite(y_end_declared) && y_end_declared < band_bot - 1) {
    probe <- tab
    probe$start$page <- p0; probe$end$page <- p1
    ae  <- tryCatch(doc_auto_end(input, probe, tmpl, follow = FALSE),
                    error = function(e) NULL)
    aep <- .doc_int(ae$page, NA_integer_); aey <- .doc_num(ae$y, NA_real_)
    if (is.finite(aep) && is.finite(aey) && aep >= p0 && aep <= max(npg, 1L) &&
        (aep > p1 || (aep == p1 && aey > y_end_declared + 1))) {
      grew_to <- list(page = aep, y = aey)
      p1 <- aep; y_end_declared <- aey
    }
  }

  # A found header on the start page tells us what the SAME header looks like on
  # a continuation page, which is the only reliable way to know a page is more of
  # this table rather than the start of the next one.
  windows <- list(); header_tops <- list()
  for (p in seq.int(p0, p1)) {
    y_min <- if (p == p0) y_start else band_top
    y_max <- if (p == p1) y_end_declared else band_bot
    # `skip` = how many leading lines of THIS window are the header. It is the
    # declared header_rows wherever the window opens at a header, and zero where
    # the window opens mid-table (a continuation page that does not repeat it).
    skip <- if (p == p0) (if (start_at_header) n_header_rows else 0L) else 0L
    if (p > p0) {
      lp <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
      hp <- .doc_find_header(lp, need_hdr)
      # ...and if the remembered WORDING did not find it, look for the heading
      # this document actually prints. Only ever reached when the wording test has
      # already returned NULL, so every template that works today is untouched.
      if (is.null(hp)) hp <- .doc_find_printed_header(lp, hdr_sig)
      if (!is.null(hp)) {
        y_min <- hp$top; header_tops[[as.character(p)]] <- hp$top
        # As many lines as ACTUALLY matched. A page that reprints only the first
        # line of a two-line heading must skip one, not two, or a data row is lost.
        skip <- .doc_int(hp$n_lines, n_header_rows)
      }
    } else if (!is.null(h)) header_tops[[as.character(p)]] <- h$top
    # OPEN-ENDED means "we do not actually know where this stops": the window runs
    # to the bottom of the page because the table carries on past it, not because
    # anyone said it ends there. Those are the windows doc_table_rows() has to
    # stop by watching the rows themselves (see the row-run rule); a window with a
    # real declared end is exact and is left alone.
    # OPEN-ENDED means "we do not actually know where this stops": the window runs
    # to the bottom of the page because the table carries on past it, not because
    # anyone said it ends there. Those are the windows doc_table_rows() has to
    # stop by watching the rows themselves (see the row-run rule); a window with a
    # real declared end is exact and is left alone.
    windows[[length(windows) + 1L]] <- list(page = as.integer(p),
                                            y_min = y_min, y_max = y_max,
                                            skip = as.integer(skip),
                                            open_end = (p != p1) || y_max >= band_bot - 1)
  }

  # FOLLOW: the same table, longer on this copy than on the example.
  #
  # A template drawn on a document whose schedule ran to page 5 will silently
  # TRUNCATE the same schedule when it runs to page 7 -- and truncation is
  # invisible: the rows that came out all look right. So a table whose declared
  # end is the BOTTOM of its last page keeps going while the following pages
  # repeat its header, and stops at the first page that does not. The extension is
  # recorded (`extended_to`) and reported, never silent, because the opposite
  # mistake -- swallowing the table that follows -- is just as bad.
  extended_to <- NA_integer_
  if (!isFALSE(tab$follow) && p1 < npg && y_end_declared >= band_bot - 1) {
    p <- p1 + 1L
    while (p <= npg) {
      lp <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
      hp <- .doc_find_header(lp, need_hdr, min_hit = 0.75)
      # A WHOLE-LINE signature match is stronger evidence than a 0.75 fraction of
      # remembered wordings, so it is allowed to extend the table too.
      if (is.null(hp)) hp <- .doc_find_printed_header(lp, hdr_sig)
      if (is.null(hp)) break
      windows[[length(windows) + 1L]] <- list(page = as.integer(p),
                                              y_min = hp$top, y_max = band_bot,
                                              skip = .doc_int(hp$n_lines, n_header_rows),
                                              open_end = TRUE)
      header_tops[[as.character(p)]] <- hp$top
      extended_to <- as.integer(p)
      p <- p + 1L
    }
  }

  # THE NEXT SECTION IS WHERE THIS ONE STOPS (R/doc_structure.R).
  #
  # Everything above decides how far this table RUNS. None of it can see the
  # thing that most often ends it: another section starting. A short section
  # followed to the bottom of its page swallows the next section's heading and
  # header as rows -- observed, three fabricated rows under a one-row table -- and
  # neither the declared end (measured on a copy with different row counts above
  # it) nor the row-run rule (a heading carries no money and fills few columns,
  # which is what a total row looks like too) reliably stops it.
  #
  # So the LAST word on extent is the document's own section index: this table
  # ends immediately before the nearest validated section start below it,
  # whichever section that is. No successor is named anywhere -- naming one would
  # be the same overfitting as a page number, and these reports reorder and omit
  # sections freely.
  #
  # It can only ever make a table SHORTER, and it only trims what a later section
  # would have claimed, so a template that reads correctly today cannot start
  # losing rows to it. Opt-in (schema_version 2), so version 1 templates are
  # untouched.
  capped <- NULL
  if (doc_sections_enabled(tmpl)) {
    ix <- doc_section_index_for(input, tmpl)
    if (is.data.frame(ix) && nrow(ix)) {
      # This table's OWN starts come out first, by name. Its heading sits at the
      # top of its own window, so leaving it in would cap the table at itself and
      # return nothing; and where the heading is reprinted on a continuation page
      # it is this table carrying on, not a new section.
      own <- .doc_norm(as.character(tab$name %||% "")[1])
      if (nzchar(own)) {
        tabs_all <- .doc_key(tmpl, "tables")
        mine <- vapply(ix$key, function(k) {
          tb <- .doc_key(tabs_all, k)
          identical(.doc_norm(as.character((tb %||% list())$name %||% k)[1]), own)
        }, logical(1), USE.NAMES = FALSE)
        ix <- ix[!mine, , drop = FALSE]
      }
      capped <- doc_section_cap(ix, p0, y_start)
    }
  }
  if (!is.null(capped)) {
    trimmed <- list()
    for (z in windows) {
      if (z$page > capped$page) next
      if (z$page == capped$page) {
        if (capped$y <= z$y_min) next          # the section starts above this window
        z$y_max <- min(.doc_num(z$y_max, capped$y), capped$y)
        z$open_end <- FALSE                    # it has a real end now
      }
      trimmed[[length(trimmed) + 1L]] <- z
    }
    # Never trim a table out of existence: an empty result would be indistinguishable
    # from "not on this document", which is a different and much worse answer.
    if (length(trimmed)) {
      windows <- trimmed
      if (!is.na(extended_to) && extended_to > capped$page) extended_to <- NA_integer_
    } else {
      capped <- NULL
    }
  }

  pages <- vapply(windows, function(z) z$page, integer(1))
  detail <- switch(anchor,
    header = sprintf("found by its heading on page %d", p0),
    first_column = sprintf("found by the rows it starts with on page %d", p0),
    position_same_page = sprintf("read from where it sat on the example (page %d)", p0),
    sprintf("read from where it sat on the example, and this document is a different length (page %d)", p0))
  # IT MOVED, AND THE REPORT SAYS SO. A table found two pages further on than the
  # template remembers is the ordinary case for these documents, not a fault -
  # but a reviewer checking a figure against the paper needs to be told which
  # page it actually came off, and a table that "moved" ten pages is worth a
  # second look at whether it is the same table at all.
  if (!is.na(moved_from))
    detail <- paste0(detail, sprintf(
      "; the template had it on page %d, and it is on page %d here",
      moved_from, p0))
  # IT IS LONGER HERE, AND THE REPORT SAYS SO. Reading past the example's end is
  # the point of the rule, and it is also the thing a reviewer would want to know
  # had happened before trusting a row count.
  if (!is.null(grew_to))
    detail <- paste0(detail, "; it runs further down the page here than on the example")
  if (!is.na(extended_to))
    detail <- paste0(detail, sprintf("; it carries on to page %d here", extended_to))
  # WHERE IT STOPPED, AND WHAT STOPPED IT. A table whose extent was decided by
  # another section beginning was not read to its declared end, and a reviewer
  # counting rows against the paper needs to know that is why.
  if (!is.null(capped)) {
    nt <- doc_section_note(doc_section_index_for(input, tmpl), capped)
    if (!is.null(nt)) detail <- paste0(detail, "; ", nt)
  }
  # ...and SAY that the heading was found again further on, because "those lines
  # are not rows" is a fact somebody checking the output needs. header_tops was
  # already collected and read by nothing.
  rep_pages <- setdiff(as.integer(names(header_tops)), p0)
  rep_pages <- sort(rep_pages[!is.na(rep_pages)])
  if (length(rep_pages))
    detail <- paste0(detail, sprintf(
      "; its heading is printed again on page%s %s and is not read as rows",
      if (length(rep_pages) == 1L) "" else "s",
      paste(rep_pages, collapse = ", ")))
  # A CORRECTION NOBODY CAN SEE IS HOW THE NEXT WRONG ANSWER GETS BUILT. If the
  # bands were moved, say by how much and on the strength of what, in the same
  # sentence that says how the table was found.
  if (is.finite(band_shift) && band_shift != 0)
    detail <- paste0(detail, sprintf(
      "; this copy prints it %.0fpt further %s than the example, and its columns were moved to match",
      abs(band_shift), if (band_shift > 0) "right" else "left"))
  list(pages = pages, windows = windows, anchor = anchor,
       confidence = .doc_anchor_confidence(anchor, hit),
       detail = detail, extended_to = extended_to, header_tops = header_tops,
       header_sig = hdr_sig, band_shift = as.numeric(band_shift),
       capped_at = capped)
}

# ---------------------------------------------------------------------------
# Columns
# ---------------------------------------------------------------------------

# .doc_columns(tab) -> the column specs as a plain list, whichever way YAML gave
# them (a sequence of mappings, or a mapping keyed by name).
.doc_columns <- function(tab) {
  cols <- tab$columns %||% list()
  if (!length(cols)) return(list())
  if (!is.null(names(cols)) && all(nzchar(names(cols))) && is.list(cols[[1]]) &&
      is.null(cols[[1]]$name)) {
    cols <- lapply(names(cols), function(n) { c(list(name = n), cols[[n]]) })
  }
  lapply(cols, function(c) if (is.list(c)) c else list(name = as.character(c)))
}

# .doc_column_names(tab) -- the column names as they will appear in the output,
# made safe and unique. `page` and `row` are the reader's own columns, and a
# duplicate name would make one column silently overwrite another when the rows
# are assembled -- so both are renamed rather than allowed to collide.
.DOC_RESERVED <- c("page", "row")
.doc_column_names <- function(tab) {
  cols <- .doc_columns(tab)
  if (!length(cols)) return(character(0))
  nm <- vapply(seq_along(cols), function(i) {
    n <- trimws(as.character(cols[[i]]$name %||% "")[1])
    if (is.na(n) || !nzchar(n)) sprintf("column_%d", i) else n
  }, character(1))
  nm[tolower(nm) %in% .DOC_RESERVED] <- paste0(nm[tolower(nm) %in% .DOC_RESERVED], "_")
  make.unique(nm, sep = "_")
}

# .doc_col_of(cx, cols) -> which column each x-centre falls in, NA for none.
# Bands are half-open on the right so two touching bands cannot both claim a word.
.doc_col_of <- function(cx, cols) {
  out <- rep(NA_integer_, length(cx))
  for (j in seq_along(cols)) {
    lo <- .doc_num(cols[[j]]$x_min, -Inf); hi <- .doc_num(cols[[j]]$x_max, Inf)
    hit <- is.na(out) & cx >= lo & cx < hi
    out[hit] <- j
  }
  # The last band takes its own right edge, so a word sitting exactly on the
  # outermost boundary is kept rather than reported as spilled.
  if (length(cols)) {
    j <- length(cols)
    hi <- .doc_num(cols[[j]]$x_max, Inf)
    out[is.na(out) & cx == hi] <- j
  }
  out
}

# ---------------------------------------------------------------------------
# Columns as EDGES -- what the builder actually manipulates
# ---------------------------------------------------------------------------
#
# A table's columns tile its width: they meet, they do not overlap, and there is
# no space between them. So the thing a person is really choosing is not N boxes
# but the N-1 DIVIDERS between them, plus the outer edge on each side.
#
# That reframing is worth more than it looks. Dragging seven boxes to define a
# seven-column table is seven drags, each of which can be a few points wrong in
# two directions -- and a gap loses a column of figures while an overlap deletes
# one (see .doc_table_problems). Clicking six dividers is six clicks, each of
# which cannot produce either fault, because the edges are shared by construction.
#
# doc_column_edges(tab)        columns   -> edges
# doc_columns_from_edges(...)  edges     -> columns
# doc_edge_click(edges, x)     one click -> the edges it should produce

# doc_column_edges(tab) -> the sorted edge positions of a table's columns:
# left edge, every divider, right edge. NULL when it has no columns yet.
doc_column_edges <- function(tab) {
  cols <- .doc_columns(tab)
  if (!length(cols)) return(NULL)
  lo <- vapply(cols, function(cc) .doc_num(cc$x_min, NA_real_), numeric(1))
  hi <- vapply(cols, function(cc) .doc_num(cc$x_max, NA_real_), numeric(1))
  if (any(is.na(lo)) || any(is.na(hi))) return(NULL)
  o <- order(lo)
  sort(unique(round(c(lo[o], hi[o[length(o)]]), 2)))
}

# doc_columns_from_edges(edges, old, names) -> the column list those edges make.
#
# EVERY COLUMN THAT DID NOT MOVE KEEPS WHAT IT WAS GIVEN. Rebuilding the whole
# list from scratch on every click would throw away the name and the kind a person
# had already set on the six columns they were happy with, which makes correcting
# the seventh feel like starting again. A column whose two edges are unchanged is
# the same column; anything else is new and takes its name from the header row.
doc_columns_from_edges <- function(edges, old = list(), names = character(0)) {
  edges <- sort(unique(round(as.numeric(edges), 2)))
  edges <- edges[is.finite(edges)]
  if (length(edges) < 2L) return(list())
  same <- function(a, b) is.finite(a) && is.finite(b) && abs(a - b) < 0.5
  lapply(seq_len(length(edges) - 1L), function(j) {
    lo <- edges[j]; hi <- edges[j + 1L]
    keep <- NULL
    for (cc in old)
      if (same(.doc_num(cc$x_min), lo) && same(.doc_num(cc$x_max), hi)) { keep <- cc; break }
    if (!is.null(keep)) { keep$x_min <- lo; keep$x_max <- hi; return(keep) }
    nm <- if (j <= length(names) && nzchar(trimws(names[j]))) trimws(names[j])
          else sprintf("column_%d", j)
    list(name = nm, x_min = lo, x_max = hi, type = "auto", may_be_blank = FALSE)
  })
}

# doc_edge_click(edges, x, tol) -> the edges after clicking the page at x.
#
# ONE GESTURE, FOUR MEANINGS, none of them ambiguous:
#   on a divider          remove it        (the two columns become one)
#   on an outer edge      move it there    (the table gets wider or narrower)
#   outside the table     move the nearer outer edge out to the click
#   anywhere else inside  add a divider    (one column becomes two)
#
# "On" means within `tol` points, which is what makes removing a divider possible
# with a mouse at all. Removing takes priority over adding, so a click that is
# nearly on a divider never quietly creates a three-point column beside it.
doc_edge_click <- function(edges, x, tol = 6) {
  x <- .doc_num(x)
  if (is.na(x)) return(edges)
  edges <- sort(unique(round(as.numeric(edges), 2)))
  edges <- edges[is.finite(edges)]
  if (length(edges) < 2L) return(edges)
  n <- length(edges)
  inner <- if (n > 2L) 2:(n - 1L) else integer(0)
  if (length(inner)) {
    d <- abs(edges[inner] - x)
    if (min(d) <= tol) return(edges[-inner[which.min(d)]])
  }
  if (abs(x - edges[1]) <= tol) { edges[1] <- x; return(sort(edges)) }
  if (abs(x - edges[n]) <= tol) { edges[n] <- x; return(sort(edges)) }
  if (x < edges[1]) { edges[1] <- x; return(edges) }
  if (x > edges[n]) { edges[n] <- x; return(edges) }
  sort(c(edges, x))
}

# ---------------------------------------------------------------------------
# COLUMNS MAY HAVE WHITESPACE BETWEEN THEM. THEY MAY NEVER OVERLAP.
#
# The two operations below work on the COLUMN LIST, not on the shared-edge view
# above, and that distinction is the whole design.
#
# The edge view exists because columns DERIVED from a header row should tile: a
# value that is a little wider on row 40 than it was in the heading still has to
# land somewhere, and a tiling set of columns is tolerant of that where a set of
# exact bands is not. doc_columns_from_box() therefore fills the gutters into the
# columns either side, deliberately.
#
# But a column somebody DRAWS is a different statement. "This band is a column"
# is exact, and forcing it to touch its neighbour means the neighbour has to
# move, or a column has to be invented in the gap. Both were tried and both are
# wrong: a report has real whitespace between its columns, and the tool inventing
# a column there -- or silently widening the one before it -- is the tool
# arguing with the person about what they can see.
#
# So: gaps are allowed and are exactly what was drawn. **Overlaps are still
# impossible**, because an overlap genuinely destroys data (R/doc_extract.R:
# whatever falls in the overlap is read into the first column and lost from the
# second). A move or an insert that would overlap is CLAMPED to the free space
# beside its neighbour rather than refused, so the gesture always does something,
# and it can never do the one thing that loses a figure.
#
# Nothing falls silently into a gap: doc_table_rows() counts every word inside
# the table that no column claimed, the reader reports it per table, and the
# screen says so. A gap is visible; a lost column is not.
# ---------------------------------------------------------------------------

# .doc_col_bands(cols) -> a 2-column matrix of (lo, hi), in column order, with
# unusable columns dropped. One place to get this right.
.doc_col_bands <- function(cols) {
  cols <- if (is.list(cols) && !is.null(cols$columns)) .doc_columns(cols) else cols
  if (!length(cols)) return(matrix(numeric(0), ncol = 2))
  lo <- vapply(cols, function(cc) .doc_num(cc$x_min, NA_real_), numeric(1))
  hi <- vapply(cols, function(cc) .doc_num(cc$x_max, NA_real_), numeric(1))
  ok <- is.finite(lo) & is.finite(hi) & hi > lo
  cbind(lo, hi)[ok, , drop = FALSE]
}

# .doc_clamp_band(bands, skip, lo, hi) -> (lo, hi) trimmed so it touches no other
# band. `skip` is the row index in `bands` that is allowed to be ignored (the
# column being moved); NA when inserting a new one. NULL when nothing is left.
.doc_clamp_band <- function(bands, skip, lo, hi) {
  if (NROW(bands)) {
    keep <- if (is.na(skip)) seq_len(nrow(bands)) else setdiff(seq_len(nrow(bands)), skip)
    for (k in keep) {
      a <- bands[k, 1]; b <- bands[k, 2]
      if (b <= lo || a >= hi) next                 # clear of it
      if (a <= lo && b >= hi) return(NULL)         # entirely inside a column
      if (a <= lo) lo <- b else hi <- a            # trim to its near side
    }
  }
  if (!is.finite(lo) || !is.finite(hi) || hi - lo < 1) return(NULL)
  c(lo, hi)
}

# doc_add_column(cols, x0, x1, names) -> the column list after "there is ANOTHER
# column, and it runs from x0 to x1". Returns `cols` unchanged when the drag
# cannot be a column at all; attr(x, "clamped") is TRUE when it had to be trimmed
# to keep clear of a neighbour, so the screen can say so.
#
# THIS IS NOT TWO CLICKS, AND IT IS NOT AN EDGE MOVE. It was both, in turn. Fed
# through doc_edge_click() twice, a drag beside the table moved the outer edge out
# to it -- twice -- and the answer was one column stretched over everything the
# drag crossed. Rewritten to keep every edge, the answer was the new column plus
# an invented one in the gap. The column list, and only the new band, is the
# answer: what was drawn, where it was drawn, and nothing else touched.
doc_add_column <- function(cols, x0, x1, names = character(0)) {
  # COLUMNS IN, COLUMNS OUT, ON EVERY PATH -- including the paths that change
  # nothing. Handing back whatever was passed in meant "nothing happened" and
  # "here are your columns" had different shapes, and the caller comparing
  # lengths to decide what to say would compare a table with a list.
  old <- if (is.list(cols) && !is.null(cols$columns)) .doc_columns(cols) else (cols %||% list())
  lo <- suppressWarnings(min(x0, x1)); hi <- suppressWarnings(max(x0, x1))
  if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi)) || hi - lo < 1) return(old)
  b <- .doc_clamp_band(.doc_col_bands(old), NA_integer_, lo, hi)
  if (is.null(b)) return(old)
  nm <- if (length(names) && nzchar(trimws(names[1]))) trimws(names[1]) else
          sprintf("column_%d", length(old) + 1L)
  new <- list(name = nm, x_min = round(b[1], 2), x_max = round(b[2], 2),
              type = "auto", may_be_blank = FALSE)
  out <- .doc_sort_columns(c(old, list(new)))
  attr(out, "clamped") <- !isTRUE(all.equal(c(lo, hi), b, tolerance = 1e-6))
  out
}

# doc_set_column_band(cols, j, x0, x1) -> the columns after column j is told to
# run from x0 to x1. Only column j moves. Clamped off its neighbours, never
# through them: absorbing a neighbour used to be the behaviour, on the reasoning
# that "Amount runs 380 to 452" is not also a request for a gap. It is now, and
# absorbing a column of figures because a drag was 3pt wide of the mark is the
# expensive half of that trade.
doc_set_column_band <- function(cols, j, x0, x1) {
  old <- if (is.list(cols) && !is.null(cols$columns)) .doc_columns(cols) else (cols %||% list())
  j <- .doc_int(j)
  if (is.na(j) || j < 1L || j > length(old)) return(old)
  lo <- suppressWarnings(min(x0, x1)); hi <- suppressWarnings(max(x0, x1))
  if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi)) || hi - lo < 1) return(old)
  b <- .doc_clamp_band(.doc_col_bands(old), j, lo, hi)
  if (is.null(b)) return(old)
  old[[j]]$x_min <- round(b[1], 2); old[[j]]$x_max <- round(b[2], 2)
  out <- .doc_sort_columns(old)
  attr(out, "clamped") <- !isTRUE(all.equal(c(lo, hi), b, tolerance = 1e-6))
  out
}

# .doc_sort_columns(cols) -- left to right, because everything downstream (the
# output's column order, the on-screen list, the numbering a person refers to)
# reads them in order.
.doc_sort_columns <- function(cols) {
  if (length(cols) < 2L) return(cols)
  lo <- vapply(cols, function(cc) .doc_num(cc$x_min, Inf), numeric(1))
  cols[order(lo)]
}

# doc_columns_touch(cols) -> TRUE when every column meets the next one, i.e. the
# columns tile with no whitespace. Used only to describe the table on screen.
doc_columns_touch <- function(cols) {
  b <- .doc_col_bands(cols)
  if (nrow(b) < 2L) return(TRUE)
  all(abs(b[-1, 1] - b[-nrow(b), 2]) < 0.5)
}

# doc_header_names(input, tab, tmpl) -> a name for each column, read from the
# table's own header row. Called after every edge change, so the names follow the
# columns instead of having to be typed: split a column and the two halves are
# named from the two header cells that were in it.
doc_header_names <- function(input, tab, tmpl = NULL, edges = NULL, loc = NULL) {
  edges <- edges %||% doc_column_edges(tab)
  if (is.null(edges) || length(edges) < 2L) return(character(0))
  n <- length(edges) - 1L
  out <- rep("", n)
  # `loc` is optional and is only ever a saving: a caller that has just located
  # this table hands its answer in rather than paying for the search twice.
  loc <- loc %||% tryCatch(doc_locate_table(input, tab, tmpl), error = function(e) NULL)
  if (is.null(loc) || !length(loc$windows)) return(out)
  win <- loc$windows[[1]]
  w <- .doc_page_words(input, win$page, tmpl)
  if (is.null(w)) return(out)
  w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
  lines <- .doc_lines(w, tab$row_tol)
  nh <- max(1L, .doc_int(win$skip, .doc_int(tab$header_rows, 1L)))
  if (!length(lines)) return(out)
  hdr <- lines[seq_len(min(nh, length(lines)))]
  d <- do.call(rbind, hdr)
  for (j in seq_len(n)) {
    sel <- d[d$cx >= edges[j] & d$cx < edges[j + 1L], , drop = FALSE]
    if (nrow(sel)) {
      sel <- sel[order(sel$y, sel$x), , drop = FALSE]
      # A COLUMN HEADING IS NOT AN AMOUNT. If the header row count is one line
      # too generous -- and on a heading printed over several baselines it can
      # be -- what comes back otherwise is a column called
      # "Revenue Medical & Public Health 47,824,589 2,241,609". A name like that
      # is not merely ugly: it becomes a worksheet column heading and a key in
      # the long output. Figures are dropped, and if that leaves nothing the
      # column keeps whatever name it already had.
      keep <- !vapply(as.character(sel$text), .doc_looks_amount, logical(1))
      if (any(keep)) sel <- sel[keep, , drop = FALSE]
      nm <- trimws(gsub("\\s+", " ", paste(sel$text, collapse = " ")))
      if (any(keep)) out[j] <- nm
    }
  }
  out
}

# doc_auto_end(input, tab, tmpl) -> list(page, y): where this table stops, worked
# out by READING it rather than by a second guess.
#
# Somebody who has just said where a table starts and what its columns are should
# not then have to hunt for where it ends -- that is the tool's job, and the tool
# already knows: it opens the table's end right up, reads it, and reports the
# bottom of the last row it kept. The stop rule that ends an open-ended window
# (see doc_table_rows) is what decides, so the end offered here is exactly the end
# the extraction would use. It is a PROPOSAL: it can be wrong, it is drawn on the
# page, and one click moves it.
doc_auto_end <- function(input, tab, tmpl = NULL, follow = TRUE) {
  probe <- tab
  # OPEN THE END OF THE START PAGE, AND FOLLOW -- do not simply point the end at
  # the last page of the document. Measured: pointing it at page 7 read pages 6
  # and 7 as well, because a middle window covers its whole page whether or not
  # the table is on it, so a four-row summary came back with 103 rows. Following
  # asks the right question one page at a time: does the NEXT page repeat this
  # table's header? The stop rule ends it within a page; the follow rule ends it
  # between pages.
  # FOLLOWING IS THE BUILDER'S QUESTION, NOT THE READER'S. In the builder nobody
  # has said where the table ends yet, so "does the next page carry on?" is part
  # of the answer. At READ time the table already has an end and the follow rule
  # in doc_locate_table owns the between-pages decision -- and that rule is gated
  # on the end reaching the paper's edge, which is exactly the guard that stops a
  # schedule swallowing the schedule after it. Forcing follow here removed that
  # guard: measured, every table with an identically-headed table on the next
  # page came back DOUBLE (AccountOne 6 rows -> 12, a fund pack 42 -> 168).
  probe$follow <- isTRUE(follow)
  probe$end <- list(page = .doc_int(tab$start$page, 1L),
                    y = .doc_frame(tmpl)$height)
  r <- tryCatch(doc_table_rows(input, probe, tmpl), error = function(e) NULL)
  if (is.null(r) || is.na(r$last_page) || is.na(r$last_y)) return(tab$end)
  list(page = as.integer(r$last_page), y = round(as.numeric(r$last_y) + 2, 1))
}

# ---------------------------------------------------------------------------
# A DIVISION ANY ONE LINE SHOWS IS A DIVISION.
#
# .doc_bands projects EVERY word in the sample onto the x axis and merges the
# intervals that touch. That is right for finding where the columns are and wrong
# for finding how many there are, because one long value reaching under its
# neighbour's heading joins the two intervals for good - and the bands TILE, so
# the merged band then claims the whole of both columns' territory and every
# figure in the second one is read into the first. Reported as "when I define
# columns, but one of the columns doesn't have a first row of data, it pulls all
# info in the last column into the second-to-last column".
#
# The evidence against the merge is already on the page: the header line, and
# every other line, breaks into CELLS at gutters wider than `gap`. If any single
# line puts two cells inside one band, that band is two columns, whatever the
# projection of everything else says. So the band is split between them.
#
# It can only ever ADD a division, never move or remove one, so a band nobody
# disagrees with comes back exactly as it was.
.doc_split_bands_by_cells <- function(bands, lines, gap) {
  if (length(bands) < 1L || !length(lines)) return(bands)
  spans <- lapply(lines, function(d) {
    if (is.null(d) || !NROW(d)) return(list())
    lapply(.doc_cells(d[order(d$x), , drop = FALSE], gap), function(z) {
      lo <- min(z$x); hi <- max(z$x + z$width)
      c(lo = lo, hi = hi, cx = (lo + hi) / 2)
    })
  })
  out <- list()
  for (b in bands) {
    lo <- .doc_num(b$x_min, NA_real_); hi <- .doc_num(b$x_max, NA_real_)
    if (!is.finite(lo) || !is.finite(hi)) { out[[length(out) + 1L]] <- b; next }
    # The line that shows the MOST divisions inside this band is the one that
    # knows most about it.
    best <- list()
    for (cs in spans) {
      inside <- Filter(function(z) z[["cx"]] >= lo && z[["cx"]] <= hi, cs)
      if (length(inside) > length(best)) best <- inside
    }
    if (length(best) < 2L) { out[[length(out) + 1L]] <- b; next }
    best <- best[order(vapply(best, function(z) z[["lo"]], numeric(1)))]
    cuts <- vapply(seq_len(length(best) - 1L), function(k)
      (best[[k]][["hi"]] + best[[k + 1L]][["lo"]]) / 2, numeric(1))
    e <- sort(unique(round(c(lo, cuts[cuts > lo & cuts < hi], hi), 1)))
    for (k in seq_len(length(e) - 1L))
      out[[length(out) + 1L]] <- list(x_min = e[k], x_max = e[k + 1L])
  }
  out
}

# doc_columns_from_box(input, box, tmpl) -> the columns a HEADER ROW implies.
#
# The header row is the one line on a table that says where its columns are, and
# a person can point at it in one gesture. Every cell of it becomes a column, and
# the boundary between two columns is the middle of the white space between two
# header cells -- which is where it belongs, and is what the eye would pick too.
doc_columns_from_box <- function(input, box, tmpl = NULL, y_to = NA_real_) {
  pg <- .doc_int(.doc_key(box, "page"), 1L)
  w <- .doc_page_words(input, pg, tmpl)
  if (is.null(w)) return(list())
  # A COLUMN WITH A BLANK HEADING HAS NO INK IN THE HEADER ROW, so a box drawn
  # round the headings cannot see it -- and it is usually the TOTAL column.
  #
  # Reported: "Column name, statement x-y, statement t-y, period x-z, (blank).
  # The blank is the total column, and has two blank rows then data for the total
  # of each period." Reproduced exactly: four bands, four columns read, every row
  # reporting itself complete, and the totals SILENTLY GONE - they sit to the
  # right of the box, so they are not even unclaimed.
  #
  # So when the caller says where the table ends, the bands are taken from the
  # whole table and the names from its heading line. The blank column then falls
  # out of the projection like any other (the totals lower down ARE ink) and
  # comes back named column_N, present and renameable, instead of absent.
  #
  # The x limits of the box are deliberately dropped in that case: the missing
  # column is BESIDE the box, which is the whole reason it was missed.
  yb <- .doc_num(y_to, NA_real_)
  wide <- is.finite(yb) && yb > .doc_num(box$y_max, -Inf)
  sel <- if (wide)
    w[w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= yb, , drop = FALSE]
  else
    w[w$cx >= .doc_num(box$x_min, -Inf) & w$cx <= .doc_num(box$x_max, Inf) &
      w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= .doc_num(box$y_max, Inf), ,
      drop = FALSE]
  if (!nrow(sel)) return(list())
  gap <- .doc_gap_for(sel)
  # ...and only TABLE-SHAPED lines vote on where the columns are. A footer, a
  # page number or a sentence under the table is one cell wide; letting it into
  # the projection would smear two real bands into one or invent a band of its
  # own. The heading line is kept whatever its shape, because it is the thing
  # being named from.
  if (wide) {
    keep <- unlist(lapply(.doc_lines(sel), function(d)
      if (length(.doc_cells(d[order(d$x), , drop = FALSE], gap)) >= 2L)
        as.numeric(d$cy) else numeric(0)))
    hdr_y <- .doc_num(box$y_max, -Inf)
    sel <- sel[sel$cy <= hdr_y | sel$cy %in% keep, , drop = FALSE]
    if (!nrow(sel)) return(list())
    gap <- .doc_gap_for(sel)
  }
  # THE NAMES COME FROM THE TOP LINE, the BANDS from everything in the box.
  # A box drawn round a header row is drawn by hand and will catch the first row
  # of data as often as not -- and it should, because the data is what the bands
  # have to fit. But a column called "Account Everyday 99-9999-9999999-99" is
  # nobody's column name, so the naming only ever reads the first line.
  lines <- .doc_lines(sel)
  top <- if (length(lines)) lines[[1]] else sel
  cells <- .doc_cells(top[order(top$x), , drop = FALSE], gap)
  if (!length(cells)) return(list())
  bands <- .doc_bands(sel, gap)
  if (!length(bands)) return(list())
  # ...and a band that any ONE line of the box breaks into two cells is two
  # columns, whatever the projection of all the ink together says. See
  # .doc_split_bands_by_cells: this is what stops a long value reaching under its
  # neighbour's heading from merging two columns into one for good.
  bands <- .doc_split_bands_by_cells(bands, lines, gap)
  edges <- c(vapply(bands, function(b) b$x_min, numeric(1)),
             bands[[length(bands)]]$x_max)
  nm <- make.unique(vapply(seq_along(bands), function(j) {
    hit <- Filter(function(cl) {
      cx <- mean(cl$x + cl$width / 2)
      cx >= bands[[j]]$x_min && cx <= bands[[j]]$x_max
    }, cells)
    if (!length(hit)) sprintf("column_%d", j)
    else trimws(gsub("^[|+:_-]+|[|+:_-]+$", "",
                     trimws(paste(unlist(lapply(hit, function(cl) cl$text)), collapse = " "))))
  }, character(1)), sep = "_")
  nm[!nzchar(nm)] <- sprintf("column_%d", which(!nzchar(nm)))
  doc_columns_from_edges(edges, names = nm)
}

# doc_fit_columns(input, tab, tmpl) -> columns derived from the words actually
# inside this table's current extent, by the same empty-gutter rule the proposer
# uses. This is the one-click answer to "I have set where it starts and stops --
# now work the columns out for me", and it is what makes correcting a table's
# boundary cheap: move the edge, press it again.
#
# IT MAY NEVER TAKE A COLUMN AWAY. A defined column is a fact about the template
# and this function is a convenience; a convenience that quietly deletes a column
# because the ink under it happens to be sparse this month is how a whole column
# of figures goes missing without a mark anywhere. So two rules, both of them
# refusals:
#
#   * a column already drawn on the table whose band holds NO ink in the table's
#     extent is KEPT exactly as drawn - an empty column is a column;
#   * the answer can never have fewer columns than the table already has. If the
#     ink says otherwise the ink is wrong about this, and what was drawn stands.
doc_fit_columns <- function(input, tab, tmpl = NULL) {
  old <- .doc_columns(tab)
  loc <- tryCatch(doc_locate_table(input, tab, tmpl), error = function(e) NULL)
  if (is.null(loc) || !length(loc$windows)) return(if (length(old)) old else list())
  parts <- list()
  for (win in loc$windows) {
    w <- .doc_page_words(input, win$page, tmpl)
    if (is.null(w)) next
    w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
    if (!nrow(w)) next
    # A printed rule spans the whole width and would merge every band into one.
    for (d in .doc_lines(w, tab$row_tol)) if (!.doc_is_rule(d)) parts[[length(parts) + 1L]] <- d
  }
  if (!length(parts)) return(if (length(old)) old else list())
  body <- do.call(rbind, parts)
  gap <- .doc_gap_for(body)
  bands <- .doc_bands(body, gap)
  if (length(bands) < 1L) return(if (length(old)) old else list())
  bands <- .doc_split_bands_by_cells(bands, parts, gap)
  edges <- c(vapply(bands, function(b) b$x_min, numeric(1)),
             bands[[length(bands)]]$x_max)
  # A DRAWN COLUMN WITH NOTHING IN IT KEEPS ITS TERRITORY. Its two edges are put
  # back into the set, which takes its space out of whichever neighbour's band
  # had swallowed it.
  for (cc in old) {
    lo <- .doc_num(cc$x_min, NA_real_); hi <- .doc_num(cc$x_max, NA_real_)
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) next
    if (any(body$cx >= lo & body$cx < hi)) next
    # Its two edges go back in, and any derived edge INSIDE it comes out: there
    # is no ink in there to argue for a division, and leaving one would hand the
    # column back in two halves under invented names.
    edges <- c(edges[edges <= lo | edges >= hi], lo, hi)
  }
  edges <- sort(unique(round(edges, 2)))
  cols <- doc_columns_from_edges(edges, old = old)
  if (length(cols) < length(old)) return(old)
  tab2 <- tab; tab2$columns <- cols
  nm <- doc_header_names(input, tab2, tmpl, edges, loc)
  out <- doc_columns_from_edges(edges, old = old, names = nm)
  if (length(out) < length(old)) old else out
}

# ---------------------------------------------------------------------------
# Cell values
# ---------------------------------------------------------------------------

.DOC_TYPES <- c("auto", "text", "money", "number", "date")

# ---------------------------------------------------------------------------
# THE COMPANION VALUE IS A VALUE, NOT A SUBSTRING.
#
# `.doc_cell_value` used to hand the cell to `.value_from_line`, which returns
# the matched SUBSTRING verbatim. Measured on the four shapes a report prints a
# negative in:
#
#   printed        __value was      what it means
#   (1,234.56)     "(1,234.56)"     minus 1,234.56 -- and nothing said so
#   987.65-        "987.65-"        minus 987.65
#   55.00 CR       "55.00 CR"       plus 55.00
#   12.34 DR       "12.34 DR"       minus 12.34
#
# NO SIGN WAS EVER RESOLVED, so a bracketed negative was indistinguishable from a
# positive to anything reading the column - a total in a spreadsheet, a sort, a
# chart. That is the cardinal failure with the document's own ink as the alibi.
#
# `parse_amount()` in R/normalise.R is the correct parser. It is proven, it
# carries the whole statement route, it resolves all four shapes above and the
# European decimal comma besides - and it was never called from here. It is now.
#
# The SHAPE test stays where it was: .value_from_line still decides whether the
# cell holds a figure at all, so a cell reading "Legal fees" is still NA rather
# than being coerced to a number by a parser that strips letters. Only what
# happens to a cell that IS a figure has changed.
#
# THE TEXT COLUMN IS UNTOUCHED. Every cell still carries exactly what the page
# printed, beside the parsed companion. Nothing is repaired in place.

# A WHOLE COLUMN AT A TIME. parse_amount and .date_strict are both vectorised,
# and calling either once per cell costs five times what calling it once per
# column does - measured at 0.5ms a cell on the money path, which on a
# forty-table pack is seconds of nothing. So the extraction (which is per cell,
# because the matchers are) and the PARSE (which is not) are separated, and the
# companion columns are computed once, after the rows are assembled.

# .doc_money_values(txt) -> the signed numbers a money column holds, NA per cell
# that holds no figure.
.doc_money_values <- function(txt) {
  m <- vapply(as.character(txt), function(t) {
    t <- trimws(as.character(t))
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    v <- .value_from_line(t, "money")
    if (is.na(v) || !nzchar(trimws(v))) NA_character_ else v
  }, character(1), USE.NAMES = FALSE)
  out <- rep(NA_real_, length(m))
  ok <- !is.na(m)
  if (any(ok)) out[ok] <- as.numeric(parse_amount(m[ok], "signed")$value)
  out
}

# .DOC_NUM_RX -- a plain number, INCLUDING the two ways a report writes a
# negative one without a minus in front of it. The old pattern started at
# [-+]?[0-9], so "(1,234)" matched as "1,234" and came back positive.
.DOC_NUM_RX <- "\\(?[-+]?[0-9][0-9,]*(?:\\.[0-9]+)?\\)?-?"

.doc_number_values <- function(txt) {
  m <- vapply(as.character(txt), function(t) {
    t <- trimws(as.character(t))
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    v <- regmatches(t, regexpr(.DOC_NUM_RX, t, perl = TRUE))
    if (!length(v) || !nzchar(v)) NA_character_ else v[1]
  }, character(1), USE.NAMES = FALSE)
  out <- rep(NA_real_, length(m))
  ok <- !is.na(m)
  if (any(ok)) out[ok] <- as.numeric(parse_amount(m[ok], "signed")$value)
  out
}

# ---------------------------------------------------------------------------
# A DATE THAT CANNOT START AT A YEAR IS NOT A DATE PARSER.
#
# The date branch ran .DATE_RX, which is "[0-9]{1,2}[/ .-]...". Measured:
# "2025-04-07" matched from the THIRD character and came back "25-04-07", which
# opens as 2007 in one reader and 1925 in another. "Apr 2025" came back NA with
# nothing said. And nothing was ever validated: the substring was handed on as
# though it were a date.
#
# The statement route has had the guards for a long time and they are one file
# away. `.date_strict` (R/normalise.R) reformats the parsed date back through the
# same format and requires it to match, and bounds the year - the pair that stops
# "13/08/2025" being read as 2020 under "%d/%m/%y". Both are used here now, and
# what comes out is ISO or nothing.
#
# WHICH FORMAT. A column may declare one (`format:` or `date_format:` on the
# column, exactly as a statement template declares its bank's), and a declared
# format is the only one tried - that is what "declared" means. With none
# declared the candidates below are tried in order, and they are DAY FIRST,
# because this tool reads New Zealand documents. A value that no candidate can
# parse trustworthily comes back NA and the cell's own text stands beside it
# unaltered, so nothing is hidden and nothing is invented.
.DOC_DATE_ISO_RX <- "[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}"
# Commonest first, because each candidate costs a parse and a round-trip. The
# order cannot change an ANSWER - a format that does not consume the whole string
# fails the round-trip and a two-digit year read under %Y fails the year bound -
# so it is only ever how quickly the right one is reached.
.DOC_DATE_FORMATS <- c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%d %b %Y",
                       "%d.%m.%Y", "%Y/%m/%d", "%Y.%m.%d", "%d %B %Y",
                       "%d/%m/%y", "%d-%m-%y", "%d.%m.%y", "%d %b %y")

# .doc_date_values(txt, fmt) -> ISO dates for a whole column, NA per cell that
# holds nothing a candidate format can parse trustworthily.
.doc_date_values <- function(txt, fmt = NULL) {
  raw <- vapply(as.character(txt), function(t) {
    t <- trimws(as.character(t))
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    iso <- regmatches(t, regexpr(.DOC_DATE_ISO_RX, t, perl = TRUE))
    v <- if (length(iso) && nzchar(iso)) iso[1] else .value_from_line(t, "date")
    if (is.na(v) || !nzchar(trimws(v))) NA_character_ else v
  }, character(1), USE.NAMES = FALSE)
  out <- rep(NA_character_, length(raw))
  ok <- which(!is.na(raw))
  if (!length(ok)) return(out)
  fmts <- as.character(fmt %||% character(0))
  fmts <- fmts[!is.na(fmts) & nzchar(trimws(fmts))]
  if (!length(fmts)) fmts <- .DOC_DATE_FORMATS
  s <- .normalise_date_str(raw[ok])
  todo <- seq_along(ok)
  for (f in fmts) {
    if (!length(todo)) break
    v <- suppressWarnings(.date_strict(s[todo], f))
    hit <- which(!is.na(v))
    if (length(hit)) {
      out[ok[todo[hit]]] <- as.character(v[hit])
      todo <- todo[-hit]
    }
  }
  out
}

# .doc_value_proto(type) -- the CLASS a declared column's companion holds. A
# money column's companion is a number; the empty-table prototype has to agree
# with the full one or the two cannot be rbound.
.doc_value_proto <- function(type)
  if (type %in% c("money", "number")) numeric(0) else character(0)

# .doc_cell_values(txt, type, fmt) -> the machine-readable form of a whole
# column, one value per cell, NA where the cell holds nothing of that kind.
# ONLY ever called for a column whose type the person DECLARED: an undeclared
# ("auto") column keeps exactly what the page says and nothing is inferred from
# it, because a guess that lands in a spreadsheet column headed "amount" is
# indistinguishable from a fact.
.doc_cell_values <- function(txt, type, fmt = NULL) {
  txt <- as.character(txt %||% character(0))
  num <- type %in% c("money", "number")
  if (!length(txt)) return(.doc_value_proto(type))
  v <- switch(type,
    money  = .doc_money_values(txt),
    number = .doc_number_values(txt),
    date   = .doc_date_values(txt, fmt),
    text   = trimws(txt),
    trimws(txt))         # any other declared type: the page's own words stand
  # switch() with no matching branch returns NULL, and a NULL assigned into the
  # rows frame would DELETE that column. Always the length and the class this
  # column's companion is declared to hold.
  if (is.null(v) || length(v) != length(txt))
    v <- rep(if (num) NA_real_ else NA_character_, length(txt))
  if (num) as.numeric(v) else {
    v <- as.character(v); v[!is.na(v) & !nzchar(v)] <- NA_character_; v
  }
}

# .doc_cell_value(txt, type, fmt) -- one cell, for a caller that has one.
.doc_cell_value <- function(txt, type, fmt = NULL)
  .doc_cell_values(as.character(txt %||% "")[1], type, fmt)[1]

# .doc_currency_mark(txt) -- the currency marker a figure was printed with, or "".
#
# `A$2,000.00` and `US$4,000.00` both parse to a number and both lose the thing
# that says WHICH money they are. The marker cannot go into the number, so it is
# recorded per column instead: a column that carries one marker says which, and a
# column that carries two is saying something a reviewer has to know before
# adding it up.
#
# AN ALTERNATION, MATCHED AS BYTES -- never a bracket expression. R/labels.R
# carries the full account of why: in a C locale a multibyte character inside
# `[...]` is read one byte at a time, and the leading byte of the value beside it
# is eaten. Every symbol below is its own branch for that reason.
.DOC_CCY_RX <- paste0("[A-Za-z]{0,3}[$]",
                      "|[A-Za-z]{0,3}\u00a3", "|[A-Za-z]{0,3}\u20ac",
                      "|[A-Za-z]{0,3}\u00a5",
                      "|(?<![A-Za-z])[A-Z]{3}(?![A-Za-z])")
.doc_currency_mark <- function(txt) {
  t <- trimws(as.character(txt %||% ""))
  if (!nzchar(t)) return("")
  m <- regmatches(t, regexpr(.DOC_CCY_RX, t, perl = TRUE, useBytes = TRUE))
  if (!length(m) || !nzchar(m)) "" else toupper(trimws(m[1]))
}

# ---------------------------------------------------------------------------
# The table reader
# ---------------------------------------------------------------------------

# doc_table_rows(input, tab, tmpl, loc) -> list(
#   rows      the table as a data.frame: page, row, one column per declared column
#             (plus <name>__value for any column with a declared type)
#   report    one row per column: fill rate, whether it was flagged, its type
#   spilled   every word inside the table's boundary that no column claimed
#   row_fill  the per-row fill rate, in row order
#   n_rows / n_header / anchor / confidence / detail
#
# THE CONTRACT. Every cell is emitted EXACTLY as the page prints it. Nothing is
# dropped for looking wrong, nothing is repaired, and no row is discarded for
# being sparse -- a total row with four empty cells is a real row of the document
# and this engine has no basis on which to disagree with it. What the reader does
# instead is measure: the fill rate per column and per row, and the words its
# columns did not claim. Those two numbers are the whole quality signal here.
doc_table_rows <- function(input, tab, tmpl = NULL, loc = NULL) {
  cols <- .doc_columns(tab)
  cnames <- .doc_column_names(tab)
  ctypes <- vapply(seq_along(cols), function(i) {
    t <- as.character(.doc_key(cols[[i]], "type") %||% "auto")[1]
    if (is.na(t) || !(t %in% .DOC_TYPES)) "auto" else t
  }, character(1))
  # A date column may say what shape its dates are printed in, exactly as a
  # statement template does. Read by exact name (see .doc_key): `$format` would
  # partial-match a `format_note` the day somebody adds one.
  cfmts <- vapply(seq_along(cols), function(i) {
    f <- as.character(.doc_key(cols[[i]], "format") %||%
                      .doc_key(cols[[i]], "date_format") %||% "")[1]
    if (is.na(f)) "" else trimws(f)
  }, character(1))
  loc <- loc %||% doc_locate_table(input, tab, tmpl)
  # ...MOVED TO WHERE THIS COPY PRINTS THEM. Measured off the heading the locator
  # found (.doc_band_shift); 0 whenever it could not tell, which is the behaviour
  # every template had before. The names and the types are unaffected - only where
  # each band sits on the paper.
  cols <- .doc_shift_cols(cols, loc$band_shift %||% 0)

  empty_rows <- function() {
    d <- data.frame(page = integer(0), row = integer(0), stringsAsFactors = FALSE)
    for (i in seq_along(cnames)) d[[cnames[i]]] <- character(0)
    for (i in seq_along(cnames)) if (!identical(ctypes[i], "auto"))
      d[[paste0(cnames[i], "__value")]] <- .doc_value_proto(ctypes[i])
    d
  }
  spilled <- data.frame(page = integer(0), y = numeric(0), x = numeric(0),
                        text = character(0), stringsAsFactors = FALSE)
  # Where an open-ended window decided the table had ended. Reported, because
  # "it stopped here" and "there was nothing more" are different facts.
  stops <- data.frame(page = integer(0), y = numeric(0), stringsAsFactors = FALSE)

  if (!length(cols)) {
    return(list(rows = empty_rows(),
                report = .doc_col_report(cnames, ctypes, integer(0), 0L, tab),
                spilled = spilled, stops = stops, row_fill = numeric(0), n_rows = 0L,
                last_page = NA_integer_, last_y = NA_real_,
                n_header = 0L, anchor = loc$anchor, confidence = loc$confidence,
                detail = "this table has no columns drawn on it yet"))
  }

  hdr_need <- tab$anchor$header_text %||% cnames
  n_header_rows <- max(0L, .doc_int(tab$header_rows, 1L))
  # PAGE FURNITURE, RECOGNISED ONCE FOR THE WHOLE DOCUMENT (R/doc_noise.R).
  # A disclaimer or a "Page 4 of 44" printed inside this table's window is not a
  # row of it, and no band or fill threshold can tell them apart -- on the page
  # they are both just text between the boundaries. Cached per document, so a
  # twenty-table report builds this once.
  noise <- doc_noise_mask_for(input, tmpl)
  noise_h <- .doc_frame(tmpl)$height
  n_noise <- 0L
  out <- list(); fills <- numeric(0); n_header <- 0L; rowno <- 0L
  # Where the last row this table actually kept came to an end. That is the
  # answer to "where does it stop?", and it is the reader's answer rather than a
  # second guess made somewhere else -- see doc_auto_end().
  last_pg <- NA_integer_; last_y <- NA_real_

  for (win in loc$windows) {
    w <- .doc_page_words(input, win$page, tmpl)
    if (is.null(w)) next
    # A word belongs to the window when its CENTRE is inside it, so a line sitting
    # exactly on the boundary lands on one side of it and not on both.
    w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
    if (!nrow(w)) next

    lines <- .doc_lines(w, tab$row_tol)
    skip <- max(0L, .doc_int(win$skip, 0L))
    # THE ROW RUN, for a window with no real end (see doc_locate_table).
    #
    # A table followed to the bottom of the page swallows whatever is printed
    # under it. Measured on the fixture: a schedule followed onto page 5 came back
    # with 93 rows instead of 83 -- the fee table and the contact table read as ten
    # more transactions, each with an amount in the wrong column and a date that
    # was not a date. Nothing about that looks wrong in a spreadsheet.
    #
    # So an open-ended window stops where the ROWS stop: a vertical gap much
    # bigger than the ones this table has been using AND a line that fills fewer
    # of its columns than its rows have been filling. Both halves are needed --
    # the gap alone would cut a table at the whitespace before its own total row,
    # which is a shape these documents genuinely use, and the column count alone
    # would cut it at any sparse row. Where it stops is recorded, never silent.
    open_end <- isTRUE(win$open_end)
    max_gap <- .doc_num(tab$max_gap, 2.2)
    kept_top <- numeric(0); kept_fill <- integer(0); kept_money <- logical(0)
    prev_left <- NA_real_; prev_i <- 0L
    # The previous PHYSICAL line, folded or kept. A wrap's leading is a distance
    # from the line above it, which is not the same thing as a distance from the
    # row above it once a cell wraps more than once.
    last_top <- NA_real_
    # ...and its TYPE HEIGHT, which is what "one line" is measured against. See
    # .doc_is_wrap: a page-wide pitch statistic is measured over the wrong
    # population and rejects every wrap on an ordinary table. Assigned at exactly
    # the same two places as last_top and nowhere else.
    last_h <- NA_real_
    # How many columns the last KEPT row filled. A wrap is the overflow of a row
    # that had something in more than one column; the second line of a list is not.
    prev_filled <- 0L
    for (li in seq_along(lines)) {
      d <- lines[[li]]
      # THE HEADER, TWO WAYS. Positionally: the leading lines of a window that
      # opens at the table's top (see doc_locate_table) -- this is what handles a
      # two-row header, which no wording rule can see. By wording: a header
      # repeated further down, on a continuation page whose top we did not find.
      # The wording rule refuses to fire on a line carrying money, so a totals row
      # that echoes the column names is kept as the row of data it is.
      # Three ways a line is a heading and not a row: it is one of the leading
      # `skip` lines of this window; it scores against the template's remembered
      # wording; or its whole text IS the heading this document prints (learned on
      # the table's own first page - see .doc_header_printed). The third is
      # belt-and-braces for a heading reprinted at a y the window did not open at.
      # All three sit inside the same no-money guard, which is what stops a totals
      # row that echoes the column names from being swallowed.
      #
      # AND THE LAST TWO ONLY APPLY AT THE TOP OF A WINDOW, which is where a
      # reprinted heading is. They used to apply ANYWHERE in the body, and they
      # DELETE the line they fire on - so a genuine row of a fee schedule reading
      # "Fee | basis under review" scored 0.6 on the wordings "Fee" and "Basis",
      # carried no money token (the column has no decimals in it), and was
      # deleted. Measured: three rows where the document prints four, fill_rate
      # 1.00, confidence 1.00, and nothing anywhere said a row had gone. A
      # reprinted heading is printed BEFORE the rows, never between them, so
      # asking only until the first row is kept costs nothing and closes it.
      at_window_top <- !length(kept_top)
      is_header <- li <= skip ||
        (at_window_top &&
         (.doc_header_hit(d, hdr_need) >= 0.6 ||
          (length(loc$header_sig) &&
             !is.null(.doc_find_printed_header(list(d), loc$header_sig)))) &&
         !any(vapply(d$text, function(t) !is.na(.value_from_line(t, "money")),
                     logical(1))))
      if (is_header) { n_header <- n_header + 1L; next }
      # A printed border is not a row. Skipped without disturbing anything: it
      # does not count as an unclaimed word, does not end an open window, and
      # does not enter the row-spacing the stop rule is measured against.
      if (.doc_is_rule(d)) next
      # ...and neither is the page's own furniture. Treated exactly as a printed
      # border is: not a row, not an unclaimed word, and not part of the row
      # spacing the stop rule measures -- because a footer sitting between two
      # rows must not look like the gap that ends the table. Counted, and said in
      # `detail` below: a line withheld without a word about it is the silent drop
      # this engine does not do.
      if (doc_is_noise(noise, .doc_line_text(d), attr(d, "top"), noise_h)) {
        n_noise <- n_noise + 1L; next
      }

      j <- .doc_col_of(d$cx, cols)
      cells <- rep(NA_character_, length(cols))
      for (k in which(!is.na(j))) {
        add <- trimws(d$text[k])
        cells[j[k]] <- if (is.na(cells[j[k]])) add else paste(cells[j[k]], add)
      }
      n_filled <- sum(!is.na(cells) & nzchar(cells))
      has_money <- any(vapply(d$text, function(t)
        !is.na(.value_from_line(t, "money")), logical(1)))

      # A WRAPPED CELL belongs to the row above it, not to a row of its own.
      # Folded before the stop rule looks at it, so a wrap never ends a table and
      # never counts as a row that filled one column.
      # MEASURED FROM THE LINE BEFORE IT, NOT FROM THE ROW. A cell wrapped over
      # THREE lines has its third line two leadings below the row's top, so a gap
      # measured from the row grows with every fold and the third line stops
      # looking like a wrap. `last_top` is the previous PHYSICAL line -- folded or
      # kept -- which is what the leading is actually a distance between.
      # A WRAP NEVER OVERWRITES A FIGURE. A description that runs on adds words to
      # a cell of words; a line that would put a second figure into a money,
      # number or date cell that ALREADY HOLDS ONE is not a continuation of that
      # row at all - it is the next thing the document printed, and it is emitted
      # as the row it is. Without this, an unlabelled total was appended to the
      # last row's amount and the parsed companion then took the TOTAL, so the
      # row lost its own figure and the total lost its row.
      clash <- prev_i > 0L && any(vapply(seq_along(cells), function(i) {
        if (is.na(cells[i]) || !nzchar(cells[i])) return(FALSE)
        if (!(ctypes[i] %in% c("money", "number", "date"))) return(FALSE)
        was <- out[[prev_i]][[cnames[i]]]
        !is.null(was) && !is.na(was) && nzchar(was)
      }, logical(1)))

      if (prev_i > 0L && n_filled >= 1L && !clash &&
          .doc_is_wrap(d, prev_left,
                       as.numeric(attr(d, "top")) -
                         (if (is.finite(last_top)) last_top else kept_top[length(kept_top)]),
                       n_filled, length(cols), prev_filled, last_h)) {
        for (i in seq_along(cells)) if (!is.na(cells[i]) && nzchar(cells[i])) {
          was <- out[[prev_i]][[cnames[i]]]
          out[[prev_i]][[cnames[i]]] <-
            if (is.na(was) || !nzchar(was)) cells[i] else .doc_join_wrapped(was, cells[i])
        }
        last_top <- as.numeric(attr(d, "top"))
        last_h <- suppressWarnings(max(as.numeric(d$height), na.rm = TRUE))
        next
      }

      # Does the table stop here? Decided BEFORE anything about this line is
      # recorded, so a line that ends the table contributes neither a row nor an
      # unclaimed word. Two kept rows are needed first, because the gap this table
      # uses has to be measured before an unusual one can be recognised.
      if (open_end && length(kept_top) >= 2L) {
        gaps <- diff(kept_top); gaps <- gaps[is.finite(gaps) & gaps > 0]
        pitch_k <- if (length(gaps)) stats::median(gaps) else NA_real_
        this_top <- as.numeric(attr(d, "top"))
        gap <- this_top - kept_top[length(kept_top)]
        typical <- stats::median(kept_fill)
        # WHAT MAKES A LINE AFTER THE GAP "NOT ONE OF THESE ROWS".
        #
        # Two signals, and it took a real document to find the second. Column
        # count alone let a heading through: "Fees charged", printed under a
        # schedule, lands in the first two of four narrow columns, so it filled
        # two where two was the floor -- and ten rows of a fee table and a
        # contact table were read as transactions with a date that was not a
        # date. (Measured: 93 rows where the schedule has 83.)
        #
        # The signal that separates a heading from a genuine sparse row is a
        # FIGURE. A total set apart by whitespace still carries its total; a
        # heading carries nothing. So: after a big gap, a line with no figure at
        # all ends a table whose rows normally have one. Tables with no figures
        # anywhere fall back to the column count, which is all there is.
        #
        # THE FLOOR CANNOT BE HIGHER THAN THE TABLE IS WIDE. A floor of two
        # filled columns is unreachable in a one-column table, so every line of
        # one looked wrong and the first paragraph gap ended it -- the proposer
        # counted twenty-one lines and the reader kept fifteen, which is the
        # template on screen not being the template that makes the file. (Found
        # on a corpus of other people's PDFs; a one-column table is a block of
        # prose or a list, and they are common.)
        rows_have_money <- length(kept_money) && mean(kept_money) >= 0.5
        floor_filled <- min(length(cols), max(2, ceiling(typical / 2)))
        looks_wrong <- n_filled < floor_filled ||
                       (rows_have_money && !has_money)
        if (is.finite(pitch_k) && pitch_k > 0 && is.finite(gap) &&
            gap > pitch_k * max_gap && looks_wrong) {
          stops <- rbind(stops, data.frame(page = as.integer(win$page),
                                           y = this_top,
                                           stringsAsFactors = FALSE))
          break
        }
      }

      # Words inside the table's boundary that NO column claimed. Recorded, never
      # silently dropped: a column band drawn a few points too narrow loses a
      # whole column of figures and every remaining row still looks perfect, so
      # this list is the only thing standing between that and a wrong answer.
      if (any(is.na(j))) {
        s <- d[is.na(j), , drop = FALSE]
        spilled <- rbind(spilled, data.frame(
          page = as.integer(s$page), y = as.numeric(s$y), x = as.numeric(s$x),
          text = as.character(s$text), stringsAsFactors = FALSE))
      }
      if (all(is.na(j)) || n_filled == 0L) next
      kept_top <- c(kept_top, as.numeric(attr(d, "top")))
      kept_fill <- c(kept_fill, n_filled)
      kept_money <- c(kept_money, has_money)
      rowno <- rowno + 1L
      rec <- list(page = as.integer(win$page), row = as.integer(rowno))
      for (i in seq_along(cnames)) rec[[cnames[i]]] <- cells[i]
      out[[length(out) + 1L]] <- rec
      prev_i <- length(out); prev_left <- min(d$x)
      last_top <- as.numeric(attr(d, "top")); prev_filled <- as.integer(n_filled)
      last_h <- suppressWarnings(max(as.numeric(d$height), na.rm = TRUE))
      last_pg <- as.integer(win$page); last_y <- max(d$y + d$height)
      fills <- c(fills, mean(!is.na(cells) & nzchar(cells)))
    }
  }

  # The FIRST header row is expected and is not evidence of anything; extra ones
  # are the continuations, which is worth saying out loud.
  n_header <- max(0L, n_header)
  # THE SINGLE BEST PIECE OF EVIDENCE THIS ROUTE CAN PRODUCE, and until now its
  # only reader anywhere was nrow(). On the measured wrong-column read it held
  # the four real closing balances - the figures the table lost, with the page
  # and the position they were printed at. So it comes back as a frame anybody
  # can write straight out: reading order, fixed column types, no stray row
  # names from the rbind that built it.
  if (nrow(spilled)) {
    spilled <- spilled[order(spilled$page, spilled$y, spilled$x), , drop = FALSE]
    rownames(spilled) <- NULL
  }
  rows <- if (!length(out)) empty_rows() else {
    df <- do.call(rbind, lapply(out, function(r)
      as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)))
    rownames(df) <- NULL
    # THE COMPANION COLUMNS, ONCE, AFTER THE ROWS ARE WHOLE. Parsing per cell as
    # the rows were built meant a wrapped cell had to be re-parsed the moment it
    # grew, and it cost five times what one call a column costs. Doing it here is
    # both cheaper and one fewer place that can disagree with itself.
    for (i in seq_along(cnames)) if (!identical(ctypes[i], "auto"))
      df[[paste0(cnames[i], "__value")]] <-
        .doc_cell_values(df[[cnames[i]]], ctypes[i], cfmts[i])
    df
  }
  filled <- if (!nrow(rows)) integer(0) else vapply(cnames, function(n) {
    v <- rows[[n]]; sum(!is.na(v) & nzchar(v))
  }, integer(1))
  # WHAT THE ENGINE ALREADY KNEW AND USED TO THROW AWAY. The __value companions
  # are computed for every declared column above; counting them costs nothing and
  # is the only thing that can tell a full column of the RIGHT words from a full
  # column of the wrong ones. See .doc_col_report.
  parsed <- if (!nrow(rows)) integer(0) else vapply(seq_along(cnames), function(i) {
    if (identical(ctypes[i], "auto")) return(NA_integer_)
    v <- rows[[paste0(cnames[i], "__value")]]
    if (is.null(v)) NA_integer_ else sum(!is.na(v) & nzchar(as.character(v)))
  }, integer(1))
  multi_token <- if (!nrow(rows)) integer(0) else vapply(seq_along(cnames), function(i) {
    if (identical(ctypes[i], "auto")) return(NA_integer_)
    v <- trimws(as.character(rows[[cnames[i]]]))
    sum(!is.na(v) & nzchar(v) & grepl("[[:space:]]", v))
  }, integer(1))
  # THE MARKER THE NUMBER CANNOT CARRY. A$2,000.00 and US$4,000.00 both become a
  # number, and the number has no room for which money it is. Recorded per column
  # off the cells that actually parsed, so a text cell in a money column cannot
  # invent one. See .doc_currency_mark.
  ccy <- rep("", length(cnames))
  if (nrow(rows)) for (i in seq_along(cnames)) {
    if (!identical(ctypes[i], "money")) next
    v <- rows[[paste0(cnames[i], "__value")]]
    txt <- as.character(rows[[cnames[i]]])
    seen <- unique(vapply(txt[!is.na(v)], .doc_currency_mark, character(1),
                          USE.NAMES = FALSE))
    seen <- sort(seen[nzchar(seen)])
    ccy[i] <- paste(seen, collapse = ", ")
  }
  # ...and whether each band is still over the heading it was drawn for. See
  # .doc_heading_disagreements: a column the document GAINED or LOST shifts every
  # figure one across with every other signal reading perfect.
  dis <- .doc_heading_disagreements(input, tab, tmpl, cols, loc)
  heading <- rep("", length(cnames))
  heading[match(dis$column, cnames)] <- dis$heading
  report <- .doc_col_report(cnames, ctypes, filled, nrow(rows), tab,
                            parsed, multi_token, ccy, heading)

  detail <- if (nrow(stops)) paste0(loc$detail, sprintf(
              "; it stops on page %d, where the rows stop fitting it",
              stops$page[nrow(stops)])) else loc$detail
  # NOTHING SILENT. A column told what it holds, holding words that are not it,
  # is not a thin column and does not read as one - so it says so itself, once,
  # in the line the screen already prints under the table's name.
  bad <- which(report$wrong_kind %in% TRUE)
  if (length(bad) == 1L)
    detail <- paste0(detail, sprintf(
      "; nothing in %s reads as %s, so check that column against the page",
      report$column[bad], .doc_kind_word(report$type[bad])))
  else if (length(bad) > 1L)
    detail <- paste0(detail, sprintf(
      "; nothing in %s reads as the kind of value it was set to hold, so check those columns against the page",
      .doc_and_list(report$column[bad])))
  # A BAND NO LONGER OVER THE COLUMN IT WAS DRAWN FOR. Said, never guessed at:
  # which column, and what this copy prints above it.
  # ONE FACT, THE FIRST ONE. A shifted table disagrees in every column at once,
  # and a sentence listing six of them is a sentence nobody finishes; the rest are
  # in the per-column report, which is where a list belongs.
  if (nrow(dis))
    detail <- paste0(detail, sprintf(
      "; on this copy the %s column sits under the heading %s, so this table has gained or lost a column",
      dis$column[1], dis$heading[1]))
  # HOW MANY LINES WERE TREATED AS A HEADING, which was counted and read by
  # nothing. One per page is what a table with a repeated heading spends; more
  # than that means a line was deleted for looking like a heading when it was a
  # row, and a deleted row is invisible in every other number here.
  header_budget <- max(n_header_rows, 1L) * max(length(loc$windows), 1L)
  if (n_header > header_budget)
    detail <- paste0(detail, sprintf(
      "; %d lines were skipped as its heading where %d were expected, so check nothing was dropped",
      n_header, header_budget))
  # ...and a money column carrying two different currencies is a column nobody
  # can add up, which is worth one sentence before anybody tries.
  mixed <- report$column[grepl(",", report$currency %||% "", fixed = TRUE)]
  if (length(mixed))
    detail <- paste0(detail, sprintf(
      "; %s carr%s figures in more than one currency",
      .doc_and_list(mixed), if (length(mixed) == 1L) "ies" else "y"))

  # WHAT WAS WITHHELD AS PAGE FURNITURE. The rows this table did NOT get are
  # invisible in every other number on this list -- fill rates and unclaimed words
  # are both computed over what survived -- so the count says itself.
  nz <- doc_noise_report(noise, n_noise)
  if (!is.null(nz)) detail <- paste0(detail, "; ", nz)

  list(rows = rows, report = report,
       spilled = spilled, stops = stops, row_fill = fills, n_rows = nrow(rows),
       n_header = n_header, anchor = loc$anchor, confidence = loc$confidence,
       last_page = last_pg, last_y = last_y,
       detail = detail, n_noise = n_noise,
       extended_to = loc$extended_to,
       pages = loc$pages, header_rows = n_header_rows)
}

# .doc_kind_word(type) -- a column's declared kind in the words a person uses for
# it. "money" is what the template calls it; "an amount" is what Beth calls it.
.doc_kind_word <- function(type) switch(as.character(type)[1],
  money = "an amount", number = "a number", date = "a date",
  "the kind of value it was set to hold")

# .doc_and_list(x) -- "Amount", "Amount and Balance", "Date, Amount and Balance".
# A message a person reads, not a vector printed at them.
.doc_and_list <- function(x) {
  x <- as.character(x[!is.na(x) & nzchar(x)])
  n <- length(x)
  if (n == 0L) return("")
  if (n == 1L) return(x)
  paste(paste(x[-n], collapse = ", "), "and", x[n])
}

# .doc_col_report(names, types, filled, n, tab, parsed, multi_token) -> one row per
# column: how full it came out, whether that is below this table's threshold, and
# -- for a column whose kind somebody DECLARED -- whether what landed in it is
# actually that kind of thing.
#
# A column can be legitimately empty -- the middle columns of a totals row, a
# "notes" column used twice in forty rows -- so the threshold FLAGS, it never
# drops, and a column marked `may_be_blank` is exempt from it entirely. The
# default 0.5 is a starting point to be tuned per table, which is why it lives on
# the table and not in params.R.
#
# WHY `parsed` IS THE MOST VALUABLE NUMBER IN THIS FRAME.
#
# fill_rate asks only whether a cell has ANY text in it. Measured: a column
# declared `money` whose band landed 60pt to the left held "Legal fees", "Court
# filing" and "Disbursements" -- and reported filled 3, fill_rate 1.00, low_fill
# FALSE, unclaimed 0, confidence 1.00, status ok. A perfectly full column of
# clean-looking values, every one of them the wrong column's words. And the
# engine ALREADY KNEW: it had computed `Amount__value` for each of those three
# rows and got NA every time, then thrown the answer away.
#
# So two numbers per declared column, both free:
#   parsed        how many cells produced a value of the declared kind
#   multi_token   how many hold more than one whitespace-separated token
# A money column at parse rate 0.00 is a BAND failure, not a thin column, and it
# is a different thing to go and fix. multi_token is the other half of the same
# picture: a column that swallowed its neighbour is full, parses, and holds two
# things in every cell. Both are NA for an `auto` column, which declared no kind
# and can therefore fail to be nothing.
#
# One guard, and between them they catch a band drawn over the wrong ink, a
# column shifted sideways, a column whose neighbour was swallowed (A3), band
# overflow, and OCR damage.
.doc_col_report <- function(cnames, ctypes, filled, n, tab,
                            parsed = NULL, multi_token = NULL, currency = NULL,
                            heading = NULL) {
  min_fill <- .doc_num(tab$min_fill, 0.5)
  cols <- .doc_columns(tab)
  blank_ok <- vapply(seq_along(cnames), function(i)
    isTRUE(cols[[i]]$may_be_blank), logical(1))
  if (!length(cnames))
    return(data.frame(column = character(0), type = character(0),
                      filled = integer(0), rows = integer(0), fill_rate = numeric(0),
                      parsed = integer(0), parse_rate = numeric(0),
                      multi_token = integer(0), currency = character(0),
                      heading = character(0),
                      may_be_blank = logical(0), low_fill = logical(0),
                      wrong_kind = logical(0), wrong_column = logical(0),
                      stringsAsFactors = FALSE))
  filled <- if (length(filled) == length(cnames)) as.integer(filled)
            else rep(0L, length(cnames))
  fit <- function(v) if (length(v) == length(cnames)) as.integer(v)
                     else rep(NA_integer_, length(cnames))
  parsed <- fit(parsed); multi_token <- fit(multi_token)
  currency <- if (length(currency) == length(cnames)) as.character(currency)
              else rep("", length(cnames))
  currency[is.na(currency)] <- ""
  heading <- if (length(heading) == length(cnames)) as.character(heading)
             else rep("", length(cnames))
  heading[is.na(heading)] <- ""
  typed <- !(ctypes %in% "auto")
  parsed[!typed] <- NA_integer_; multi_token[!typed] <- NA_integer_
  rate <- if (n > 0L) filled / n else rep(NA_real_, length(cnames))
  prate <- ifelse(is.na(parsed) | filled == 0L, NA_real_, parsed / pmax(filled, 1L))
  data.frame(column = cnames, type = ctypes, filled = filled,
             rows = rep(as.integer(n), length(cnames)),
             fill_rate = round(rate, 3),
             parsed = parsed, parse_rate = round(prate, 3),
             multi_token = multi_token, currency = currency,
             # WHAT THIS COPY PRINTS OVER THE BAND, and only when that is not the
             # heading the column was drawn for. Blank means "no disagreement",
             # which is the ordinary case and says nothing at all.
             heading = heading,
             may_be_blank = blank_ok,
             low_fill = !blank_ok & !is.na(rate) & rate < min_fill,
             # NOT ONE CELL of a column that was told what it holds actually holds
             # it. That is the band being wrong, and it is worth saying in its own
             # words rather than as a thin column, which it is not.
             wrong_kind = !is.na(prate) & prate == 0 & filled > 0L,
             wrong_column = nzchar(heading),
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Label / value pairs -- TWO drawn boxes, not one
# ---------------------------------------------------------------------------

# WHY TWO BOXES. The front matter of these documents prints a label and its value
# in every arrangement there is: side by side, one under the other, the value
# inside a ruled panel away to the right, and -- the awkward one -- the value
# FIRST with its label after it. A single box round the value cannot survive any
# of that moving, and a label-only matcher (the existing mode: fields) cannot
# express "the value is the thing 120pt to the right and 4pt down".
#
# So a pair is two boxes, and what is stored is the label's WORDING plus the
# OFFSET between the boxes. On the next document the label is found by its
# wording wherever it has moved to, and the same offset is applied from there.
# Direction never has to be named, because an offset already carries it.
# The declared boxes remain the fallback for when the wording is not found.

# .doc_box_words(w, box) -- the words whose centres lie in a box, reading order.
.doc_box_words <- function(w, box) {
  if (is.null(w) || !nrow(w)) return(w[0, , drop = FALSE])
  keep <- w$cx >= .doc_num(box$x_min, -Inf) & w$cx <= .doc_num(box$x_max, Inf) &
          w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= .doc_num(box$y_max, Inf)
  d <- w[keep, , drop = FALSE]
  if (!nrow(d)) return(d)
  d[order(round(d$y / 3), d$x), , drop = FALSE]
}

# .doc_find_phrase(lines, phrase) -> the bounding box of the words that make up
# `phrase` on this page, or NULL. Matched on the normalised line text, then
# narrowed to the words that actually contributed, so the box is the LABEL's box
# and not the whole line's.
.doc_find_phrase <- function(lines, phrase) {
  key <- .doc_norm(phrase)
  if (!nzchar(key)) return(NULL)
  for (d in lines) {
    toks <- .doc_norm(d$text)
    n <- length(toks)
    if (!n) next
    # Slide a window over the line's words and take the shortest run whose
    # concatenation contains the phrase. Words are compared normalised, so
    # punctuation between label and value ("Prepared for:") does not matter.
    for (i in seq_len(n)) {
      acc <- ""
      for (k in i:n) {
        acc <- paste0(acc, toks[k])
        if (grepl(key, acc, fixed = TRUE)) {
          sel <- d[i:k, , drop = FALSE]
          # Trim to the SMALLEST run of words that still carries the phrase. The
          # outer loop already stops at the earliest start and the shortest end,
          # but a phrase can begin part-way into the first word it swept up
          # ("for" found inside "Prepared for"), and the box returned here is the
          # anchor the value's offset is measured from -- a box one word too wide
          # moves the value box by exactly that word.
          while (nrow(sel) > 1L &&
                 grepl(key, paste(.doc_norm(sel$text[-1]), collapse = ""), fixed = TRUE))
            sel <- sel[-1, , drop = FALSE]
          return(list(x_min = min(sel$x), x_max = max(sel$x + sel$width),
                      y_min = min(sel$y), y_max = max(sel$y + sel$height),
                      page = sel$page[1]))
        }
        if (nchar(acc) > nchar(key) + 40L) break
      }
    }
  }
  NULL
}

# WHICH SIDE THE VALUE IS ON.
#
# An offset alone carries the direction, but only rigidly: "38pt right and 0
# down" misses the amount as soon as the label is a word longer or the figure is
# a digit wider, which on a re-print it usually is. What survives is the SIDE --
# the value is the next thing to the RIGHT of "Closing balance", or the thing
# UNDER "Prepared for" -- so that is what is stored, worked out from the two
# boxes the person drew, and on the next document the search runs in that
# direction from wherever the wording turned up.
#
# .doc_pair_rel(lb, vb) -> list(where, gap, cross, width, height)
#   where  right | left | below | above
#   gap    the clear distance between the two boxes along that direction
#   cross  how far the value is offset across it (kept for the report, not used
#          to search -- searching across the direction is what made it brittle)
.doc_pair_rel <- function(lb, vb) {
  n <- function(v, d) { x <- .doc_num(v, d); if (is.na(x)) d else x }
  lx0 <- n(lb$x_min, 0); lx1 <- n(lb$x_max, 0)
  ly0 <- n(lb$y_min, 0); ly1 <- n(lb$y_max, 0)
  vx0 <- n(vb$x_min, 0); vx1 <- n(vb$x_max, 0)
  vy0 <- n(vb$y_min, 0); vy1 <- n(vb$y_max, 0)
  # How far apart on each axis, counting an overlap as zero.
  dx_r <- vx0 - lx1; dx_l <- lx0 - vx1
  dy_b <- vy0 - ly1; dy_a <- ly0 - vy1
  # The axis they are genuinely separated on wins. When the boxes share a line
  # (they overlap vertically) that is side by side; when they share a column it
  # is one above the other. When both or neither, the bigger separation decides.
  same_line <- min(ly1, vy1) - max(ly0, vy0) > 0
  side <- if (dx_r >= 0) "right" else if (dx_l >= 0) "left" else NA_character_
  updown <- if (dy_b >= 0) "below" else if (dy_a >= 0) "above" else NA_character_
  where <- if (!is.na(side) && (same_line || is.na(updown))) side
           else if (!is.na(updown)) updown
           else if (!is.na(side)) side
           else if (abs(vx0 - lx0) >= abs(vy0 - ly0)) "right" else "below"
  gap <- switch(where, right = dx_r, left = dx_l, below = dy_b, above = dy_a, 0)
  cross <- switch(where,
    right = , left  = (vy0 + vy1) / 2 - (ly0 + ly1) / 2,
    below = , above = (vx0 + vx1) / 2 - (lx0 + lx1) / 2, 0)
  list(where = where, gap = round(max(gap, 0), 1), cross = round(cross, 1),
       width = round(max(vx1 - vx0, 0), 1), height = round(max(vy1 - vy0, 0), 1))
}

# .doc_pair_where_text(rel) -- the relation in the words a person would use.
.doc_pair_where_text <- function(rel) {
  w <- as.character(.doc_key(rel, "where") %||% "")[1]
  switch(w, right = "to the right of it", left = "to the left of it",
         below = "under it", above = "above it", "beside it")
}

# .doc_pair_window(hb, rel) -- where to look, given the label was found at `hb`.
# Generous ALONG the direction the value sits in and tight ACROSS it: that is
# what lets the figure be longer, or the label wider, without ever drifting onto
# the line above or the field next door.
.doc_pair_window <- function(hb, rel) {
  gap <- max(.doc_num(.doc_key(rel, "gap"), 0), 0, na.rm = TRUE)
  wdt <- max(.doc_num(.doc_key(rel, "width"), 0), 8, na.rm = TRUE)
  hgt <- max(.doc_num(.doc_key(rel, "height"), 0), 6, na.rm = TRUE)
  lh <- max(.doc_num(hb$y_max, 0) - .doc_num(hb$y_min, 0), hgt, 6)
  reach_x <- gap + wdt * 2 + 90
  reach_y <- gap + hgt * 2 + 26
  switch(as.character(.doc_key(rel, "where") %||% "right"),
    right = list(x_min = hb$x_max - 1, x_max = hb$x_max + reach_x,
                 y_min = hb$y_min - lh * 0.6, y_max = hb$y_max + lh * 0.6),
    left  = list(x_min = hb$x_min - reach_x, x_max = hb$x_min + 1,
                 y_min = hb$y_min - lh * 0.6, y_max = hb$y_max + lh * 0.6),
    below = list(x_min = hb$x_min - wdt * 0.6 - 24, x_max = hb$x_max + wdt + 70,
                 y_min = hb$y_max - 1, y_max = hb$y_max + reach_y),
    above = list(x_min = hb$x_min - wdt * 0.6 - 24, x_max = hb$x_max + wdt + 70,
                 y_min = hb$y_min - reach_y, y_max = hb$y_min + 1),
    NULL)
}

# .doc_pair_pick(w, win, where) -- the NEAREST run of words in the window: the
# nearest line to the label, and on it, the words up to the first real gap. A
# window wide enough to survive a longer figure is also wide enough to reach the
# next field along, and this is what stops there.
.doc_pair_pick <- function(w, win, where) {
  d <- .doc_box_words(w, win)
  if (is.null(d) || !nrow(d)) return(NULL)
  where <- as.character(where %||% "right")[1]
  d <- d[order(switch(where, right = d$x, left = -d$x, above = -d$y, d$y)), , drop = FALSE]
  seed <- d[1, , drop = FALSE]
  lh <- max(as.numeric(seed$height), 6)
  # THE WHOLE LINE THE SEED SITS ON, not only the part inside the window. The
  # window says where to LOOK; it must not also decide how long the answer may
  # be, or a value gets cut in half for the crime of being set two words longer
  # than the copy the template was drawn on.
  line <- w[abs(w$cy - as.numeric(seed$cy)) <= lh * 0.6, , drop = FALSE]
  # ...but never back across the label itself.
  line <- switch(where,
    right = line[line$x + line$width > .doc_num(win$x_min, -Inf), , drop = FALSE],
    left  = line[line$x < .doc_num(win$x_max, Inf), , drop = FALSE],
    line)
  if (!nrow(line)) line <- seed
  line <- line[order(line$x), , drop = FALSE]
  k <- which.min(abs(line$x - as.numeric(seed$x)))
  lim <- max(lh * 1.4, 8)
  lo <- k
  while (lo > 1L && line$x[lo] - (line$x[lo - 1L] + line$width[lo - 1L]) <= lim) lo <- lo - 1L
  hi <- k
  while (hi < nrow(line) &&
         line$x[hi + 1L] - (line$x[hi] + line$width[hi]) <= lim) hi <- hi + 1L
  d <- line[lo:hi, , drop = FALSE]
  box <- list(x_min = min(d$x), x_max = max(d$x + d$width),
              y_min = min(d$y), y_max = max(d$y + d$height))

  # A VALUE BROKEN ACROSS TWO LINES, INSIDE ONE WORD. The run above is one line
  # of words, which is right for a figure and wrong for the address a PDF wrapped
  # -- "beth.adams@" on one line and "example.co.nz" on the next is one value.
  #
  # Narrow on purpose: it reaches down ONE line, only when the run ends on a
  # character a word is broken after (`@ / \ _ = + -`), and only for words that
  # start underneath what has already been taken. A wrapped description ending in
  # an ordinary word is left alone, because there the space is correct and a
  # greedy reach would swallow the next field.
  last <- as.character(d$text[nrow(d)])
  if (isTRUE(grepl("[A-Za-z0-9][@/\\\\_=+-]$", last))) {
    nxt <- w[w$cy > box$y_max & w$cy <= box$y_max + lh * 1.8 &
             w$x + w$width > box$x_min - lh * 0.5 &
             w$x < box$x_max + lh * 2, , drop = FALSE]
    if (NROW(nxt)) {
      nxt <- nxt[order(nxt$x), , drop = FALSE]
      # only the unbroken run at the start of that line
      keep <- 1L
      while (keep < nrow(nxt) &&
             nxt$x[keep + 1L] - (nxt$x[keep] + nxt$width[keep]) <= lim) keep <- keep + 1L
      nxt <- nxt[seq_len(keep), , drop = FALSE]
      box$x_min <- min(box$x_min, min(nxt$x))
      box$x_max <- max(box$x_max, max(nxt$x + nxt$width))
      box$y_max <- max(box$y_max, max(nxt$y + nxt$height))
    }
  }
  box
}

# doc_pairs(input, tmpl) -> data.frame(pair, label, value, raw, page, found_by,
#                                      matched)
# `found_by` is "its wording" (the label was located and the value taken from the
# side it was on) or "its place on the page" (the wording was not found and the
# drawn box was read).
doc_pairs <- function(input, tmpl) {
  specs <- tmpl$pairs %||% list()
  cols <- c("pair", "label", "value", "raw", "page", "found_by", "matched")
  if (!length(specs)) {
    d <- data.frame(matrix(character(0), 0, length(cols)), stringsAsFactors = FALSE)
    names(d) <- cols; d$page <- integer(0); d$matched <- logical(0)
    return(d)
  }
  npg <- length(input$words %||% list())
  rows <- lapply(names(specs), function(nm) {
    sp <- specs[[nm]]
    lb <- .doc_key(sp, "label"); if (!is.list(lb)) lb <- list()
    vb <- .doc_key(sp, "value"); if (!is.list(vb)) vb <- list()
    vtype <- as.character(.doc_key(sp, "type") %||% "text")[1]
    label_text <- trimws(as.character(.doc_key(sp, "label_text") %||% "")[1])
    if (is.na(label_text)) label_text <- ""
    pg <- .doc_int(.doc_key(vb, "page") %||% .doc_key(lb, "page"), 1L)
    if (is.na(pg) || pg < 1L) pg <- 1L

    # NO BOX AT ALL, JUST WORDING. This is the "I already know what the fields
    # are called, let me type them" shortcut, and it is the oldest and most
    # portable way to read a form: the label matcher finds the wording anywhere on
    # any page and takes the value beside it, so nothing depends on where the
    # value sat when the template was made. Keeping it here is what lets ONE
    # builder cover a form and a report -- the two used to be separate screens
    # because the two engines were separate, which was never a reason a person
    # holding a document would recognise.
    if (!is.list(vb) || is.na(.doc_num(vb$x_min))) {
      m <- if (nzchar(label_text))
        tryCatch(match_label(list(any_of = list(label_text), value = vtype),
                             input$pages %||% character(0)), error = function(e) NULL)
        else NULL
      got <- if (!is.null(m) && isTRUE(m$matched)) trimws(m$value %||% "") else ""
      return(data.frame(pair = nm, label = if (nzchar(label_text)) label_text else nm,
                        value = got, raw = if (is.null(m)) "" else trimws(m$raw %||% ""),
                        page = NA_integer_, found_by = "its wording",
                        matched = nzchar(got), stringsAsFactors = FALSE))
    }

    # The side the value was on when the pair was drawn. Stored on the template
    # so it can be READ and CHANGED; worked out from the boxes when it is not.
    rel <- .doc_key(sp, "where")
    rel <- if (is.list(rel)) rel
           else if (is.character(rel) && nzchar(rel[1]))
             utils::modifyList(.doc_pair_rel(lb, vb), list(where = rel[1]))
           else .doc_pair_rel(lb, vb)

    box <- NULL; found_by <- "its place on the page"; use_page <- pg
    if (nzchar(label_text)) {
      # Search the declared page first, then every other page: the front matter of
      # a longer copy of the same document does move.
      order_pages <- unique(c(pg, seq_len(max(npg, 0L))))
      for (p in order_pages) {
        w <- .doc_page_words(input, p, tmpl)
        if (is.null(w)) next
        hitbox <- .doc_find_phrase(.doc_lines(w), label_text)
        if (is.null(hitbox)) next
        # LOOK ON THE SIDE IT WAS ON, not at a fixed distance. The distance is
        # only how far to reach; what is being asked is "the next thing to the
        # right of this wording", which is the thing that stays true when the
        # figure gets longer or the label gets wider.
        win <- .doc_pair_window(hitbox, rel)
        box <- if (is.null(win)) NULL else .doc_pair_pick(w, win, rel$where)
        if (is.null(box)) {
          # Nothing on that side here. Fall back to the offset between the two
          # drawn boxes, applied from where the label actually is.
          dx <- .doc_num(vb$x_min, 0) - .doc_num(lb$x_min, 0)
          dy <- .doc_num(vb$y_min, 0) - .doc_num(lb$y_min, 0)
          wdt <- .doc_num(vb$x_max, 0) - .doc_num(vb$x_min, 0)
          hgt <- .doc_num(vb$y_max, 0) - .doc_num(vb$y_min, 0)
          box <- list(x_min = hitbox$x_min + dx, x_max = hitbox$x_min + dx + wdt,
                      y_min = hitbox$y_min + dy, y_max = hitbox$y_min + dy + hgt)
        }
        found_by <- "its wording"; use_page <- p
        break
      }
    }
    if (is.null(box)) box <- list(x_min = .doc_num(vb$x_min, -Inf),
                                  x_max = .doc_num(vb$x_max, Inf),
                                  y_min = .doc_num(vb$y_min, -Inf),
                                  y_max = .doc_num(vb$y_max, Inf))
    w <- .doc_page_words(input, use_page, tmpl)
    # Read the box LINE BY LINE, not word by word: a value drawn round an email
    # that the PDF wrapped is one token broken in two, and a plain space between
    # every word turns xys@ / gmail.com into "xys@ gmail.com".
    raw <- if (is.null(w)) "" else .doc_join_lines(.doc_box_words(w, box))
    raw <- trimws(raw)
    val <- if (identical(vtype, "text") || !nzchar(raw)) raw
           else { v <- .value_from_line(raw, vtype); if (is.na(v)) "" else v }
    data.frame(pair = nm,
               label = if (nzchar(label_text)) label_text else nm,
               value = val, raw = raw, page = as.integer(use_page),
               found_by = found_by, matched = nzchar(val),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}
