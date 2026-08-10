# ==============================================================================
# run_all.R
# Master script: runs the full NFHS x ACCESS x IRES pipeline end-to-end.
#
# Usage (from this folder):
#   source("run_all.R")
# or from the command line:
#   Rscript run_all.R
#
# Before the first run:
#   1. Edit the paths at the top of 00_config.R to match your machine.
#   2. Resolve the three `<-- CHECK` items (see README.md):
#        - 02: ACCESS cylinder-refill column names
#        - 01: NFHS caste column names
#        - 03: IRES LPG code + rural/urban column
#
# Each step saves its outputs to dir_out (set in 00_config.R), so if a step
# fails you can fix it and resume from that step -- earlier steps don't need
# to be re-run. Set RERUN_ALL <- FALSE to skip steps whose outputs already
# exist.
# ==============================================================================

RERUN_ALL <- FALSE   # FALSE = skip steps whose main output file already exists

# Ensure we run from the scripts folder no matter how this file is invoked
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) setwd(dirname(sub("^--file=", "", fa[1])))
}

# ---- Check required packages up front ----------------------------------------
required <- c("tidyverse", "haven", "readstata13", "sf", "lme4", "survey",
              "srvyr", "WeMix", "broom", "broom.mixed", "brms", "nnet",
              "pROC", "cowplot", "naniar")
missing <- required[!vapply(required, requireNamespace, logical(1),
                            quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages -- install first:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))")
}

source("00_config.R")   # defines dir_out

steps <- list(
  list(script = "01_prep_nfhs.R",           out = "nfhs_districts.rds"),
  list(script = "02_prep_access.R",         out = "access_districts.rds"),
  list(script = "03_prep_ires.R",           out = "ires_districts.rds"),
  list(script = "04_compare.R",             out = "compare_pairs.rds"),
  list(script = "05_correction.R",          out = "corrected_nfhs_districts.rds"),
  list(script = "06_stacking_prediction.R", out = "district_exposure_proxy.rds"),
  list(script = "07_diagnostics_maps.R",    out = "diagnostics_summary.txt")
)

t0 <- Sys.time()
for (.step in steps) {
  out_path <- file.path(dir_out, .step$out)
  if (!RERUN_ALL && file.exists(out_path)) {
    message("== Skipping ", .step$script, " (", .step$out, " exists) ==")
    next
  }
  message("\n==== Running ", .step$script, " [", format(Sys.time(), "%H:%M:%S"),
          "] ====")
  step_t <- Sys.time()
  # fresh environment: scripts cannot clobber the runner's loop state
  tryCatch(
    source(.step$script, echo = FALSE, local = new.env()),
    error = function(e) {
      stop("\n*** ", .step$script, " FAILED: ", conditionMessage(e),
           "\nFix the issue and re-run run_all.R -- completed steps will be ",
           "skipped if you set RERUN_ALL <- FALSE.", call. = FALSE)
    }
  )
  message("==== ", .step$script, " done in ",
          round(difftime(Sys.time(), step_t, units = "mins"), 1), " min ====")
}

message("\nAll steps complete in ",
        round(difftime(Sys.time(), t0, units = "mins"), 1), " min.\n",
        "Key outputs in ", dir_out, ":\n",
        "  comparison_table.csv          (agreement metrics)\n",
        "  Scatter_45deg.jpeg, BlandAltman.jpeg\n",
        "  corrected_nfhs_districts.rds  (RC + Bayesian corrected estimates)\n",
        "  district_exposure_proxy.csv   (predicted stacking / HAP proxy)")
