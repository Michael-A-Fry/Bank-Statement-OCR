for (f in list.files("R", full.names = TRUE)) source(f)
mkinput <- function(dx = 0) {
  rows <- list(c("Code","Units"), c("ABC","100"), c("DEF","250"), c("GHI","375"))
  w <- do.call(rbind, lapply(seq_along(rows), function(i) data.frame(
    text = rows[[i]], x = c(60, 200) + dx, y = 100 + (i-1)*20,
    width = c(40, 40), height = 10, stringsAsFactors = FALSE)))
  list(kind="pdf", words=list(w), pages="x", page_width=595, page_height=842)
}
tab <- list(name="Holdings", start=list(page=1,y=95), end=list(page=1,y=200),
  header_rows=1L, anchor=list(header_text=list("Code","Units")),
  columns=list(list(name="Code",x_min=55,x_max=150), list(name="Units",x_min=195,x_max=260)))
tmpl <- list(ref_width=595, ref_height=842)
for (dx in c(0, 60, 120)) {
  inp <- mkinput(dx); loc <- doc_locate_table(inp, tab, tmpl)
  rr <- doc_table_rows(inp, tab, tmpl, loc)
  cat(sprintf("\n===== shifted %dpt =====\n", dx))
  cat("located by:", loc$anchor, " confidence:", loc$confidence, "\n")
  print(rr$rows)
  cat("-- the per-column report the UI reads --\n"); print(rr$report)
  cat("unclaimed words:", nrow(rr$spilled), "\n")
}
