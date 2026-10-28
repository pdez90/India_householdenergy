# ==============================================================================
# prepare_data.R -- build the app's data bundle (app/data/) from the published
# aggregate results. Run from the app/ folder after each pipeline export:
#
#   Rscript prepare_data.R
#
# Inputs (all aggregate; nothing household-level, no GPS, no cluster crosswalk):
#   ../results/*.csv                 exported by scripts/export_results_tables.R
#   ../data/district_lpg_estimates*  the public district file
#   district shapefile               polygons only, simplified to GeoJSON for maps
#                                    (env DISTRICT_SHP, default
#                                     $DHS_DIR/district_pc11_shrug/nfhs_data.shp,
#                                     built by scripts/build_district_boundaries.R)
#
# This script is NOT part of the deployed app (it needs sf and the shapefile).
# When writing the Connect Cloud manifest, list the app files explicitly so
# its packages are not recorded (see README).
# ==============================================================================
suppressMessages({ library(sf); library(readr); library(dplyr) })

res_dir  <- Sys.getenv("RESULTS_DIR", "../results")
data_dir <- Sys.getenv("PUBLIC_DATA_DIR", "../data")
out_dir  <- "data"
dir.create(out_dir, showWarnings = FALSE)

FILES <- c(
  # provenance
  "PROVENANCE.csv",
  # step 1: do the surveys agree?
  "comparison_table.csv", "benchmark_sidebyside_agreement.csv",
  "benchmark_district_wide.csv", "benchmark_reference_district.csv",
  "state_agreement_nfhs5_ires.csv", "season_sensitivity.csv",
  "ml_vs_designwt_agreement.csv", "glmer_cluster_marginalisation.csv",
  # step 2: correction
  "calibration_parameters.csv", "calibration_loso.csv", "calibration_loso_summary.csv",
  "calibration_support_summary.csv",
  # step 3: health
  "health_effects_table.csv", "health_small_cluster_inference.csv",
  "health_cluster_inference_calibration.csv", "mortality_birth_counts.csv",
  # step 4: where the corrected exposure's uncertainty comes from (descriptive).
  # The precision-frontier outputs (design_frontier, design_kappa_star,
  # design_floor_direct, ...) rested on the abandoned outcome-conditioned
  # imputation and are NOT part of the app; see BANNED below.
  "design_summary.csv", "design_variance_decomposition.csv",
  # under the hood: correction models and their sensitivity analyses
  "calibration_parameters_legA_wt.csv", "calibration_sampler.csv",
  "calibration_state_effect_check.csv", "me_se_diagnostics.csv", "me_shrinkage.csv",
  "estimator_mix_levels.csv", "estimator_mix_correlations.csv",
  "legA_symmetry_sensitivity.csv",
  "legA_symmetry_health_movement.csv", "legA_symmetry_health_movement_all.csv",
  "legA_symmetry_health_movement_nonoverlap.csv",
  "health_effects_table_all.csv", "health_effects_table_nonoverlap.csv",
  "health_small_cluster_inference_all.csv", "health_small_cluster_inference_nonoverlap.csv",
  "health_nuanced_effects.csv",
  "ppd_sensitivity.csv", "ppd_surface_comparison.csv", "ppd_summary.csv",
  # run diagnostics (results/diagnostics/), copied flat with a diag_ prefix
  "diagnostics/bayes_fit_diagnostics.csv", "diagnostics/model_fits_summary.csv",
  "diagnostics/pipeline_checks.csv", "diagnostics/transfer_predictive.csv",
  "diagnostics/transfer_coefficients.csv", "diagnostics/transfer_covariate_shift.csv",
  # fuel stacking (experimental page)
  "stacking_ldo_validation.csv", "stacking_state_validation.csv",
  "stacking_state_validation_summary.csv", "reference_stacking_district.csv",
  "district_exposure_proxy.csv",
  # SI S11 (scripts/28_stacking_place_and_season.R)
  "stacking_variance_partition.csv", "nfhs_main_fuel_seasonality.csv",
  "nfhs_main_fuel_seasonality_cells.csv")

# Outputs that must never reach the public app: the precision frontier and the
# outcome-conditioned imputation (failed convergence -- not for inference).
BANNED <- c("design_frontier.csv", "design_kappa_star.csv", "design_floor_direct.csv",
            "design_grid.csv", "design_direct.csv", "design_information_gain.csv",
            "design_mime_diagnostics.csv", "ppd_mime_diagnostics.csv",
            "mime_diagnostics.csv", "mime_validation.csv", "mime_validation_intervals.csv")
# Pattern as well as names, so a new outcome-conditioned or frontier output
# (e.g. mime2_stageA_*.csv) is caught without editing the list.
BANNED_RX <- "mime|frontier|kappa"
is_banned <- function(f) basename(f) %in% BANNED | grepl(BANNED_RX, basename(f), ignore.case = TRUE)
stopifnot(!any(is_banned(FILES)), !any(grepl("archive", FILES, ignore.case = TRUE)))
gone <- list.files(out_dir)[is_banned(list.files(out_dir))]
if (length(gone)) { file.remove(file.path(out_dir, gone)); message("  removed from the bundle: ", paste(gone, collapse = ", ")) }

# Validate the approved-output rules on the SOURCE files before anything is
# copied, so a rejected table never lands in the bundle even transiently.
mi_ok <- function(path) {
  h <- read_csv(path, show_col_types = FALSE)
  mi <- grepl("_MI$", h$term)
  !any(mi) || ("mi_method" %in% names(h) && all(h$mi_method[mi] %in% "exposure_only"))
}
for (f in c("health_effects_table.csv", "health_effects_table_all.csv", "health_effects_table_nonoverlap.csv")) {
  src <- file.path(res_dir, f)
  if (file.exists(src) && !mi_ok(src))
    stop(f, ": MI rows are not the approved exposure-only imputation. Re-run H2_health_models.R and re-export.")
}
if (file.exists(file.path(res_dir, "design_summary.csv"))) {
  ds <- read_csv(file.path(res_dir, "design_summary.csv"), show_col_types = FALSE)
  if (any(grepl("kappa|frontier|floor|direct|multiplier", ds$quantity)))
    stop("design_summary.csv carries precision-frontier quantities (a DESIGN_FRONTIER=1 run). Re-run 22_design_analysis.R and re-export.")
}

copied <- character()
for (f in FILES) {
  src <- file.path(res_dir, f)
  dst <- file.path(out_dir, sub("^diagnostics/", "diag_", f))
  if (file.exists(src)) { file.copy(src, dst, overwrite = TRUE); copied <- c(copied, basename(dst)) }
  else message("  (not in this export, skipped) ", f)
}
# Only the approved health outputs: every MI row must be the exposure-only
# imputation, and design_summary.csv must be the descriptive (non-frontier) one.
# ... and again on everything now in the bundle, including files a previous
# export left there that this export did not refresh.
for (f in intersect(list.files(out_dir), c("health_effects_table.csv", "health_effects_table_all.csv",
                              "health_effects_table_nonoverlap.csv"))) {
  h <- read_csv(file.path(out_dir, f), show_col_types = FALSE)
  mi <- grepl("_MI$", h$term)
  if (any(mi) && (!"mi_method" %in% names(h) || !all(h$mi_method[mi] %in% "exposure_only")))
    stop(f, ": MI rows are not the approved exposure-only imputation. Re-run H2_health_models.R and re-export.")
}
if ("design_summary.csv" %in% list.files(out_dir)) {
  ds <- read_csv(file.path(out_dir, "design_summary.csv"), show_col_types = FALSE)
  if (any(grepl("kappa|frontier|floor|direct|multiplier", ds$quantity)))
    stop("design_summary.csv carries precision-frontier quantities (a DESIGN_FRONTIER=1 run). Re-run 22_design_analysis.R and re-export.")
}
for (f in c("district_lpg_estimates.csv", "district_lpg_estimates_dictionary.csv")) {
  src <- file.path(data_dir, f)
  if (file.exists(src)) { file.copy(src, file.path(out_dir, f), overwrite = TRUE); copied <- c(copied, f) }
}

# District polygons -> simplified GeoJSON keyed on the 2011 district code.
shp <- Sys.getenv("DISTRICT_SHP",
  file.path(Sys.getenv("DHS_DIR", "~/Downloads/DHS_India"),
            "district_pc11_shrug/nfhs_data.shp"))   # SHRUG 2.2 2011 geometry (CC BY-NC-SA 4.0)
if (file.exists(path.expand(shp))) {
  g <- st_read(path.expand(shp), quiet = TRUE) %>%
    transmute(district = as.integer(dist_code), pc11_d_id = sprintf("%03d", as.integer(dist_code)),
              dist_name = as.character(dist_name), state_name = as.character(state_name)) %>%
    filter(!st_is_empty(.)) %>% st_make_valid()
  g <- st_simplify(g, dTolerance = 2500, preserveTopology = TRUE)   # metres (UTM 44N)
  g <- st_transform(g, 4326)
  g <- g %>% group_by(district, pc11_d_id, dist_name, state_name) %>% summarise(.groups = "drop")
  f <- file.path(out_dir, "districts.geojson")
  if (file.exists(f)) file.remove(f)
  st_write(g, f, quiet = TRUE, layer_options = "COORDINATE_PRECISION=4")
  message(sprintf("  districts.geojson: %d districts, %.1f MB", nrow(g), file.size(f)/1e6))
} else message("  district shapefile not found at ", shp, " -- maps will be unavailable")

# Provenance of the bundle itself
prov <- if (file.exists(file.path(out_dir, "PROVENANCE.csv")))
  read_csv(file.path(out_dir, "PROVENANCE.csv"), show_col_types = FALSE) else NULL
write_csv(tibble(file = copied), file.path(out_dir, "BUNDLE.csv"))
message(sprintf("App bundle: %d files from %s%s", length(copied), res_dir,
                if (!is.null(prov)) paste0(" (run ", prov$value[prov$quantity == "analysis_run_id"], ")") else ""))
