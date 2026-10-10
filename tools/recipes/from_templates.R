#!/usr/bin/env Rscript
# from_templates.R -- D14, a one-off: turn the old 1.x templates into DRAFT recipes.
#
#   Rscript tools/recipes/from_templates.R <out-folder> [templates-folder ...]
#
# <out-folder> is where drafts are kept: the server's recipes folder (config
# paths$recipes, or templates/recipes beside the layouts), or a scratch folder to
# look at first. The templates folders default to tests/testthat/fixtures/templates
# (the 13 shipped with 1.x); add a server's own 1.x templates folder to convert those
# too. A draft is never trusted alone: statements it recognises come back on Please
# check filled in from it, and it reads on its own only after 3 proofs from 2
# accounts. Nothing already in <out-folder> is overwritten. No statement is read.
.args <- commandArgs(TRUE)
if (!length(.args)) { cat("usage: Rscript tools/recipes/from_templates.R <out-folder> [templates-folder ...]\n"); quit(status = 2) }
.root <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "..", ".."))
suppressWarnings(suppressMessages(for (f in list.files(file.path(.root, "R"), "[.]R$", full.names = TRUE)) source(f)))
.dirs <- if (length(.args) > 1L) .args[-1] else file.path(.root, "tests", "testthat", "fixtures", "templates")
.res <- recipes_from_templates(.dirs, .args[1])
for (i in seq_len(nrow(.res))) cat(sprintf("%-28s -> %-30s %s\n", .res$template[i], .res$recipe[i] %||% "-", .res$why[i]))
cat(sprintf("%d of %d template(s) written as draft recipes in %s\n", sum(.res$written), nrow(.res), .args[1]))
