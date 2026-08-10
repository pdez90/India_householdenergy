# ==============================================================================
# run_health.R   -- one-shot health-effects bundle (rural = MAIN, all = SI)
#
# Pipeline:
#   H3_env_covariates.R           -> ambient covariates (once; run only if missing)
#   H1_prep_mortality.R           -> district mortality, writes BOTH
#                                    health_district_wide.rds     (RURAL, main)
#                                    health_district_wide_all.rds (ALL, SI)
#   For POP in {rural, all}:
#     H2_health_models.R          -> child-mortality associations  (main / _all)
#     H4_si_adult_health.R        -> hypertension + diabetes (SI)   (main / _all)
#   H5_health_nuanced.R  (rural)  -> child mortality vs nuanced predicted exposures
#   H6_si_health_energy_scatter.R (rural) -> SI mortality-vs-energy scatter
#
# RURAL is the main analysis (matches the rural-only corrected exposure); ALL
# is an SI sensitivity (outputs suffixed "_all"). Rural outputs keep their
# original filenames so nothing downstream needs renaming.
#
# Usage (from the scripts folder):   Rscript run_health.R
#
# Prerequisites (from the main pipeline, files on disk):
#   corrected_nfhs_districts.rds                 <- 05_correction.R
#   district_exposure_proxy_consistent.rds (+draws) <- 06_stacking_prediction.R
#       (re-run 06 with DO_CONSISTENT_HEALTH = TRUE if these are missing)
#   cvd_load.RData (person-level BP/self-report) for H4
# ==============================================================================

# Run from this file's directory regardless of how it is invoked
args <- commandArgs(trailingOnly = FALSE)
fa <- grep("^--file=", args, value = TRUE)
if (length(fa)) setwd(dirname(sub("^--file=", "", fa[1])))

dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"

required <- c("tidyverse", "haven", "sf", "lme4", "lmtest", "sandwich", "broom", "nnet")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing))
  stop("Missing packages -- install first:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))")

if (!file.exists(file.path(dir_out, "corrected_nfhs_districts.rds")))
  message("NOTE: corrected_nfhs_districts.rds not found -- run 05_correction.R first.")
if (!file.exists(file.path(dir_out, "district_exposure_proxy_consistent.rds")))
  message("NOTE: district_exposure_proxy_consistent.rds not found -- re-run ",
          "06_stacking_prediction.R (DO_CONSISTENT_HEALTH = TRUE) first.")

## ---- step runner (fresh env; optional POP; clear failure message) ------------
run_step <- function(script, pop = NA_character_) {
  tag <- if (is.na(pop)) script else paste0(script, "  [POP = ", pop, "]")
  message("\n==== ", tag, "  [", format(Sys.time(), "%H:%M:%S"), "] ====")
  st <- Sys.time()
  e <- new.env()
  if (!is.na(pop)) assign("POP", pop, envir = e)
  tryCatch(source(script, echo = FALSE, local = e),
    error = function(err) stop("\n*** ", tag, " FAILED: ",
      conditionMessage(err), call. = FALSE))
  message("==== ", tag, " done in ",
          round(difftime(Sys.time(), st, units = "mins"), 1), " min ====")
}

t0 <- Sys.time()

## 1) ambient covariates -- population-independent; run only if missing (slow)
if (!file.exists(file.path(dir_out, "df_wide_health.rds")))
  run_step("H3_env_covariates.R") else
  message("== Skipping H3_env_covariates.R (df_wide_health.rds exists) ==")

## 2) mortality prep -- writes both rural (main) and all (SI) in one run
run_step("H1_prep_mortality.R")

## 3) outcome-population analyses: rural (main) then all (SI)
for (pop in c("rural", "all")) {
  run_step("H2_health_models.R",   pop)
  run_step("H4_si_adult_health.R", pop)
}

## 4) rural-tied analyses (nuanced proxy is rural; scatter is rural-vs-rural)
run_step("H5_health_nuanced.R", "rural")
run_step("H6_si_health_energy_scatter.R", "rural")

message("\nHealth analysis complete in ",
        round(difftime(Sys.time(), t0, units = "mins"), 1), " min.\n",
        "MAIN (rural) outputs keep original names; SI (all) outputs are suffixed _all:\n",
        "  health_district_wide.rds / _all.rds        (district mortality)\n",
        "  health_effects_table.csv / _all.csv        (mortality associations)\n",
        "  health_effects_coefplot.jpeg / _all.jpeg\n",
        "  health_nuanced_effects.csv                 (rural nuanced exposures)\n",
        "  si_adult_health_effects.csv / _all.csv     (hypertension + diabetes)\n",
        "  si_health_energy_corr.csv + maps/SI_health_energy_scatter.jpeg")
