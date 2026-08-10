# inspect_cvd.R -- dump the column names we need to wire up H4.
# Run:  Rscript inspect_cvd.R   (paste the whole output back)
suppressPackageStartupMessages(library(tidyverse))
path_cvd_load <- "/Users/priyanka/Downloads/DHS_India/cvd_load.RData"

e <- new.env(); load(path_cvd_load, envir = e)
cat("Objects in cvd_load.RData:\n"); print(ls(e))

for (nm in c("all_2015", "all_2019")) {
  if (!exists(nm, envir = e)) next
  df <- get(nm, envir = e); n <- names(df)
  cat("\n=====================", nm, "(", nrow(df), "rows,", ncol(df), "cols )",
      "=====================\n")
  show <- function(lab, pat) {
    hits <- grep(pat, n, ignore.case = TRUE, value = TRUE)
    cat("\n[", lab, "]\n"); if (length(hits)) print(hits) else cat("  (none)\n")
  }
  show("cluster/geo id", "clust|v001|hv001|dhsclust|latnum|longnum|dist|state|region")
  show("weight",         "wt|weight|v005|hv005|d005|mv005")
  show("blood pressure", "sb.*16|sb.*18|shb|systol|diastol|bp_|_bp|pressure")
  show("hypertension",   "hyper|htn|highbp|bp_high|bphigh")
  show("diabetes/sugar", "diab|sugar|glucose|sb.*70|sb.*71|s71[0-9]")
  show("age/sex",        "^v012$|^v013$|^mv012$|age|^v012|sex")
}
cat("\nDone.\n")
