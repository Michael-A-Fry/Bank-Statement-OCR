# test-survey-prompt.R -- guards docs/operational/survey-a-statement-with-ai.md.
#
# That page holds a prompt an analyst pastes into an AI assistant, WITH A REAL
# CLIENT STATEMENT ATTACHED, to get back a description of the layout. Two things
# about it can go wrong quietly:
#
#   1. The privacy instructions get edited away or watered down. Nothing would
#      fail; the next analyst would simply send client details to an assistant.
#   2. The engine grows a new amount style / date code / diagnostic name and the
#      prompt keeps asking for the old vocabulary. The surveys still come back,
#      still look right, and quietly can't describe the thing that was added.
#
# So: the privacy rules are asserted as present, and the prompt's vocabulary is
# asserted to MATCH THE ENGINE'S, read from the engine rather than restated here.

.survey_path <- function() file.path(engine_root(), "docs/operational/survey-a-statement-with-ai.md")
.survey_text <- function() paste(readLines(.survey_path(), warn = FALSE), collapse = "\n")

test_that("the survey page exists and is reachable from where an analyst gets stuck", {
  expect_true(file.exists(.survey_path()))
  # A page nobody can find is a page that does not exist. These are the three
  # places someone hits this problem: the index, the template wizard's docs, and
  # the "something looks wrong" page.
  for (rel in c("docs/operational/README.md",
                "docs/operational/adding-a-bank-template.md",
                "docs/operational/when-something-goes-wrong.md")) {
    txt <- paste(readLines(file.path(engine_root(), rel), warn = FALSE), collapse = "\n")
    expect_true(grepl("survey-a-statement-with-ai.md", txt, fixed = TRUE),
                info = paste("no link to the survey prompt in", rel))
  }
})

test_that("the prompt still tells the assistant to withhold client details", {
  txt <- .survey_text()
  # The rule itself.
  expect_true(grepl("RULE 1: NO PERSONAL INFORMATION", txt, fixed = TRUE))
  # Each category it must refuse. Losing any ONE of these lines is the whole risk.
  for (must in c("account numbers", "signature", "real money amounts",
                 "real transaction descriptions", "IRD"))
    expect_true(grepl(must, txt, fixed = TRUE), info = paste("privacy rule lost:", must))
  # The placeholders it is told to substitute, and the final self-check.
  for (must in c("[NAME]", "[ADDRESS]", "[DESCRIPTION]", "=== BEFORE YOU ANSWER ==="))
    expect_true(grepl(must, txt, fixed = TRUE), info = paste("lost:", must))
  # And the counterweight: bank wording is NOT personal information. Without it
  # an assistant redacts the column headings too and the survey is worthless.
  expect_true(grepl("THE BANK'S OWN WORDING IS NOT PERSONAL INFORMATION", txt, fixed = TRUE))
  # Fail closed, never guess -- the charter's rule, restated for the assistant.
  expect_true(grepl("RULE 3: NEVER GUESS", txt, fixed = TRUE))
})

test_that("the prompt asks for every amount style the engine can actually be told", {
  txt <- .survey_text()
  # .VALID_SIGN is the engine's list. Add a style there and this fails until the
  # prompt learns to ask about it -- which is the point: a survey that cannot
  # describe a style the engine supports sends us back to the statement.
  for (style in .VALID_SIGN)
    expect_true(grepl(style, txt, fixed = TRUE),
                info = paste("survey prompt never mentions amount style:", style))
})

test_that("the date codes the prompt teaches are ones the engine understands", {
  txt <- .survey_text()
  codes <- regmatches(txt, gregexpr("%[a-zA-Z]", txt))[[1]]
  codes <- sort(unique(codes))
  expect_gt(length(codes), 0)
  # Anything outside strptime's date fields would come back as a format string
  # the engine rejects at template-validation time.
  expect_true(all(codes %in% c("%d", "%m", "%b", "%B", "%Y", "%y")),
              info = paste("unknown date code in the prompt:",
                           paste(setdiff(codes, c("%d", "%m", "%b", "%B", "%Y", "%y")),
                                 collapse = ", ")))
  # And the ones it teaches must really parse.
  expect_false(is.na(as.Date("07/04/2025", format = "%d/%m/%Y")))
  expect_false(is.na(as.Date("07 Apr 2025", format = "%d %b %Y")))
})

test_that("every diagnostic name the page quotes is a real one", {
  txt <- .survey_text()
  # The page tells the analyst to paste the app's diagnostic wording alongside
  # the survey, and lists the names to look for. A name that no longer exists
  # sends them hunting for something the app will never print.
  quoted <- regmatches(txt, gregexpr("`[a-z_]+`", txt))[[1]]
  quoted <- gsub("`", "", quoted)
  quoted <- quoted[grepl("_", quoted)]                   # the diagnostic-shaped ones
  known <- c(names(.DIAG_FIX_OWNER), "amount_sign", "decimal_mark", "date_format",
             "user_templates_default")                   # config/template keys it also names
  expect_true(all(quoted %in% known),
              info = paste("unknown name quoted in the survey page:",
                           paste(setdiff(quoted, known), collapse = ", ")))
})

test_that("the machine-readable block parses and carries what a template needs", {
  lines <- readLines(.survey_path(), warn = FALSE)
  open <- grep("^```yaml$", lines)
  expect_length(open, 1L)
  close <- grep("^```$", lines); close <- close[close > open][1]
  expect_false(is.na(close))
  y <- yaml::yaml.load(paste(lines[(open + 1):(close - 1)], collapse = "\n"))
  expect_true(is.list(y$survey))
  # The fields that decide whether a template can be written at all. If the block
  # loses one, twenty-five surveys come back all missing the same thing.
  need <- c("bank", "file_type", "fingerprint_candidates", "columns_in_order",
            "date_format", "date_has_year", "amount_style", "amount_columns",
            "decimal_mark", "negative_style", "running_balance",
            "opening_balance_label", "closing_balance_label", "summary_labels",
            "multiple_statements", "top_risks")
  expect_true(all(need %in% names(y$survey)),
              info = paste("missing from the summary block:",
                           paste(setdiff(need, names(y$survey)), collapse = ", ")))
})
