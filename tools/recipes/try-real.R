# tools/recipes/try-real.R -- the real-statement gate, run on the owner's own machine.
#
#   Rscript tools/recipes/try-real.R "C:\path\to\folder of statements"
#
# Reads every statement in the folder with the tool as it stands (recipes first,
# then the automatic reader) and prints ONE line per file, numbered, with NO file
# name, account, name or figure in it:
#
#   #  kind  pages  recipe matched          result        reason
#   1  pdf   2      anz_cashback_visa@1     automatic     The running balance checks...
#
# then a count per recipe. The printout is safe to paste to Claude. Nothing is
# written anywhere except a temporary folder that is deleted at the end.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || !dir.exists(args[1])) {
  cat("usage: Rscript tools/recipes/try-real.R <folder of statements>\n"); quit(status = 1)
}
here <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))),
                 error = function(e) ".")
root <- normalizePath(file.path(here, "..", ".."))
setwd(root)
suppressWarnings(suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)))

files <- list.files(args[1], pattern = "[.](pdf|csv|tsv|xlsx|xls)$", ignore.case = TRUE, full.names = TRUE)
if (!length(files)) { cat("No statements found in that folder.\n"); quit(status = 1) }
tmp <- tempfile("try_real_"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

one_line <- function(x, n = 90) { x <- gsub("[[:space:]]+", " ", as.character(x %||% "")[1]); if (nchar(x) > n) paste0(substr(x, 1, n - 3), "...") else x }
# The reason is the engine's own sentence; any run of 4+ digits is masked, so an
# account number or figure quoted in a reason cannot leave the machine.
mask <- function(x) gsub("[0-9][0-9,.]{3,}", "####", x)

rows <- list()
cat(sprintf("%-3s %-6s %-5s %-34s %-11s %s\n", "#", "kind", "pages", "recipe matched", "result", "reason"))
for (i in seq_along(files)) {
  t0 <- Sys.time()
  res <- tryCatch(convert_statement(files[i], outdir = file.path(tmp, i), logdir = file.path(tmp, "logs"), log = FALSE,
                                    layouts_dir = file.path(tmp, "lay"), tracking_dir = NA),
                  error = function(e) list(status = "failed", messages = conditionMessage(e)))
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  rd <- res$reading %||% list()
  rec <- unique(unlist(lapply(rd, function(r) r$matched_recipe %||% r$template$recipe$ref)))
  rec <- if (length(rec)) paste(rec, collapse = "+") else "-"
  st <- as.character(res$status %||% "failed")[1]
  result <- if (identical(st, "ok")) "automatic" else if (identical(st, "needs_review")) "needs you" else "couldn't read"
  why <- mask(one_line(res$reason %||% (if (length(rd)) rd[[1]]$why) %||% res$messages[1] %||% ""))
  kind <- tolower(tools::file_ext(files[i]))
  pages <- res$metadata$pages_actual %||% NA
  cat(sprintf("%-3d %-6s %-5s %-34s %-11s %s  (%ss)\n", i, kind, pages, one_line(rec, 34), result, why, secs))
  rows[[i]] <- data.frame(recipe = rec, result = result, stringsAsFactors = FALSE)
}
df <- do.call(rbind, rows)
cat("\nPer recipe:\n")
print(as.data.frame.matrix(table(df$recipe, factor(df$result, c("automatic", "needs you", "couldn't read")))))
cat(sprintf("\nTotal: %d files, %d automatic, %d need you, %d couldn't be read.\n", nrow(df),
            sum(df$result == "automatic"), sum(df$result == "needs you"), sum(df$result == "couldn't read")))
