# test-recipes-from-templates.R -- D14: the 13 old 1.x templates written as DRAFT
# recipes (R/recipes_admin.R recipe_from_template, tools/recipes/from_templates.R).
# A draft only fills Please check in: a statement it recognises still waits for a
# person once, and nothing is written over a draft already there. Every draft is
# written into a sandbox folder, never into the repo's templates/.

tpl_dir <- function() fixture("tests/testthat/fixtures/templates")

test_that("every 1.x template becomes a valid draft recipe of the same design", {
  out <- tempfile("d14_"); on.exit(unlink(out, recursive = TRUE))
  res <- recipes_from_templates(tpl_dir(), out)
  expect_identical(nrow(res), 13L)
  expect_true(all(res$written))
  rcs <- recipes_load(out)
  expect_identical(attr(rcs, "problems"), character(0))
  expect_length(rcs, 13L)
  expect_true(all(vapply(rcs, function(r) identical(r$status, "draft"), NA)))
  kinds <- table(vapply(rcs, `[[`, "", "kind"))
  expect_identical(as.integer(kinds[c("csv", "excel", "pdf")]), c(7L, 1L, 5L))
  # nothing already there is written over
  again <- recipes_from_templates(tpl_dir(), out)
  expect_false(any(again$written))
  expect_match(again$why[1], "already there")
  # no folder given: drafted, nothing written
  dry <- recipes_from_templates(tpl_dir())
  expect_false(any(dry$written))
})

test_that("a template's draft reads its design's statement, and the statement still waits for a person once", {
  out <- tempfile("d14_"); on.exit(unlink(out, recursive = TRUE))
  recipes_from_templates(tpl_dir(), out)
  rcs <- recipes_load(out)
  inp <- read_input(fixture("tests/testthat/fixtures/asb_everyday_pdf_sample.pdf"))
  r <- recipe_first(inp, opts = list(recipes = rcs))
  expect_identical(r$matched_recipe, "asb_everyday_pdf_from_1x@1")
  expect_true(isTRUE(r$draft))
  expect_identical(r$outcome, "proven")
  # a draft alone is never accepted: recognised only as a draft
  expect_null(recipe_recognise(inp, rcs)$recipe)
  # the type-signed card export keeps its D / C words
  cc <- Filter(function(x) identical(x$id, "anz_creditcard_csv_from_1x"), rcs)[[1]]
  expect_identical(cc$style, "type_words")
  expect_identical(c(cc$out_words, cc$in_words), c("D", "C"))
})

test_that("a template that cannot be a recipe says why, and nothing is half-written", {
  expect_match(recipe_from_template(list(bank = "ANZ", format = "word"))$error, "not a PDF, CSV or Excel")
  expect_match(recipe_from_template(list(format = "pdf"))$error, "names no bank")
  expect_match(recipe_from_template(list(bank = "ANZ", format = "pdf", table = list(date_format = "%d %b",
    columns = list(date = list(x_min = 1, x_max = 2)))))$error, "fingerprint")
})
