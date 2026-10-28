# ==============================================================================
# 32_crosswalk_v1_v2_comparison.R  (standalone; fast)
#
# Compares the results of the run on the version-1 crosswalk (a frozen copy of
# its outputs) with the current run on the version-2 crosswalk, table by table,
# for every quantity the manuscript reports from district membership: the
# survey comparison and its levels, the benchmarks, the calibration and its
# cross-validation, the corrected surface, the variance decomposition, the NSSO
# check and the health models.
#
#   V1_DIR=<ACCESS_DIR>/outputs_crosswalk_v1 Rscript 32_crosswalk_v1_v2_comparison.R
#
# The same script compares a crosswalk sensitivity variant with the primary run:
#   OUTPUT_DIR=<ACCESS_DIR>/outputs_xw_exclude_uncertain V1_DIR=<ACCESS_DIR>/outputs \
#     CMP_TAG=primary_vs_exclude_uncertain Rscript 32_crosswalk_v1_v2_comparison.R
# ("v1" columns = the reference run in V1_DIR, "v2" = the run in OUTPUT_DIR.)
#
# Output: crosswalk_comparison_<CMP_TAG>.csv (long: table, row key, column,
# reference, current, difference) and crosswalk_comparison_<CMP_TAG>_summary.csv
# (largest absolute change per table). CMP_TAG defaults to v1_v2.
# ==============================================================================
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()
v1_dir  <- Sys.getenv("V1_DIR", file.path(dirname(dir_out), "outputs_crosswalk_v1"))
if (!dir.exists(v1_dir)) stop("frozen version-1 outputs not found at ", v1_dir, " (set V1_DIR)")

TABLES <- c("comparison_table.csv", "comparison_levels.csv", "si_benchmark_agreement.csv",
            "calibration_parameters.csv", "calibration_loso_summary.csv", "estimator_mix_levels.csv",
            "estimator_mix_revision_bands.csv", "correction_revision_by_state_2019.csv",
            "design_variance_decomposition.csv", "nsso78_comparison_table.csv",
            "health_effects_table.csv", "health_effects_table_all.csv", "health_effects_table_nonoverlap.csv",
            "health_small_cluster_inference.csv", "health_mi_comparison.csv",
            "si_adult_health_effects.csv", "stacking_state_validation.csv")

rows <- list(); summ <- list()
for (f in TABLES) {
  p1 <- file.path(v1_dir, f); p2 <- file.path(dir_out, f)
  if (!file.exists(p1) || !file.exists(p2)) { message("skip (absent in one run): ", f); next }
  a <- utils::read.csv(p1, stringsAsFactors = FALSE, check.names = FALSE)
  b <- utils::read.csv(p2, stringsAsFactors = FALSE, check.names = FALSE)
  num <- intersect(names(a)[vapply(a, is.numeric, TRUE)], names(b)[vapply(b, is.numeric, TRUE)])
  keyc <- setdiff(intersect(names(a), names(b)), num)
  mk <- function(d) {
    k <- if (length(keyc)) do.call(paste, c(d[keyc], sep = " | ")) else rep("", nrow(d))
    paste0(k, " #", stats::ave(seq_along(k), k, FUN = seq_along))   # disambiguate repeated keys
  }
  ka <- mk(a); kb <- mk(b); common <- intersect(ka, kb)
  if (!length(common) || !length(num)) next
  ia <- match(common, ka); ib <- match(common, kb)
  for (cn in num) {
    v1 <- a[[cn]][ia]; v2 <- b[[cn]][ib]
    rows[[length(rows) + 1]] <- data.frame(table = f, row = common, column = cn, v1 = v1, v2 = v2,
                                           difference = v2 - v1, stringsAsFactors = FALSE)
  }
  dd <- do.call(rbind, rows[vapply(rows, function(r) r$table[1] == f, TRUE)])
  j <- which.max(abs(dd$difference))
  summ[[length(summ) + 1]] <- data.frame(table = f, rows_compared = length(common),
    rows_only_v1 = length(setdiff(ka, kb)), rows_only_v2 = length(setdiff(kb, ka)),
    largest_abs_difference = if (length(j)) abs(dd$difference[j]) else NA,
    at = if (length(j)) paste(dd$row[j], dd$column[j], sep = " :: ") else NA, stringsAsFactors = FALSE)
}
out <- do.call(rbind, rows); sm <- do.call(rbind, summ)
tag <- Sys.getenv("CMP_TAG", "v1_v2")
utils::write.csv(out, file.path(dir_out, paste0("crosswalk_comparison_", tag, ".csv")), row.names = FALSE)
utils::write.csv(sm,  file.path(dir_out, paste0("crosswalk_comparison_", tag, "_summary.csv")), row.names = FALSE)
print(sm, row.names = FALSE)

# The headline numbers, side by side.
pick <- function(tab, rowpat, col) {
  z <- out[out$table == tab & grepl(rowpat, out$row) & out$column == col, ]
  if (nrow(z)) z[1, c("row", "v1", "v2", "difference")] else NULL
}
cat("\n== Headline quantities: version 1 -> version 2 ==\n")
for (q in list(c("comparison_table.csv", "^NFHS-5 \\(rural\\) vs IRES \\(rural\\)", "mean_diff"),
               c("comparison_table.csv", "^NFHS-5 \\(rural, design-wt\\)", "mean_diff"),
               c("comparison_table.csv", "^NFHS-4 \\(rural\\) vs ACCESS", "mean_diff"),
               c("calibration_parameters.csv", "^NFHS-5.*\\| slope", "estimate"),
               c("calibration_parameters.csv", "^NFHS-5.*\\| sd_residual \\#", "estimate"))) {
  r <- pick(q[1], q[2], q[3]); if (!is.null(r)) print(cbind(table = q[1], column = q[3], r), row.names = FALSE)
}
h <- out[out$table == "health_effects_table.csv" & grepl("infant", out$row, ignore.case = TRUE) &
           out$column %in% c("estimate", "conf.low", "conf.high", "p.value", "p_cr2"), ]
if (nrow(h)) { cat("\nInfant-mortality rows (health_effects_table.csv):\n"); print(utils::head(h, 30), row.names = FALSE) }
