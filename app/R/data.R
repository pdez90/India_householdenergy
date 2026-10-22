# ==============================================================================
# data.R -- load the app bundle (app/data/, built by prepare_data.R) and derive
# every number the narrative quotes. Nothing here is hard-coded: when the bundle
# is rebuilt from a new pipeline run, the text and charts follow.
# ==============================================================================

DATA_DIR <- "data"

rd <- function(f) {
  p <- file.path(DATA_DIR, f)
  if (!file.exists(p)) return(NULL)
  readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
}

# ---- formatting helpers ------------------------------------------------------
pp   <- function(x, d = 1) ifelse(is.finite(x), formatC(100 * x, format = "f", digits = d), "NA")
num  <- function(x, d = 2) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "NA")
pfmt <- function(p) ifelse(!is.finite(p), "NA", ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3)))
signed <- function(x, d = 1) ifelse(is.finite(x), sprintf(paste0("%+.", d, "f"), x), "NA")
qv <- function(tab, key, col = "value") {
  if (is.null(tab)) return(NA_real_)
  v <- tab[[col]][tab$quantity == key]
  if (length(v)) suppressWarnings(as.numeric(v[1])) else NA_real_
}

# ---- load --------------------------------------------------------------------
load_bundle <- function() {
  B <- list()
  B$prov     <- rd("PROVENANCE.csv")
  B$run_id   <- if (!is.null(B$prov)) B$prov$value[B$prov$quantity == "analysis_run_id"] else "unknown"
  B$run_date <- if (!is.null(B$prov)) B$prov$value[B$prov$quantity == "generated"] else ""
  # When the export mixes runs (e.g. a targeted rerun of one script after the
  # main run), name them from the check log rather than showing a bare label.
  ck <- rd("diag_pipeline_checks.csv")
  B$run_main <- B$run_id; B$run_other <- NULL
  if (!is.null(ck) && "run_id" %in% names(ck) && length(unique(ck$run_id)) > 1) {
    tb <- sort(table(ck$run_id), decreasing = TRUE)
    B$run_main  <- names(tb)[1]
    B$run_other <- lapply(names(tb)[-1], function(r)
      list(id = r, scripts = paste(unique(ck$script[ck$run_id == r]), collapse = ", "), n = as.integer(tb[[r]])))
  }

  B$dist     <- rd("district_lpg_estimates.csv")
  B$dict     <- rd("district_lpg_estimates_dictionary.csv")
  B$geo      <- if (file.exists(file.path(DATA_DIR, "districts.geojson")))
                  jsonlite::read_json(file.path(DATA_DIR, "districts.geojson")) else NULL

  # Step 1
  B$cmp      <- rd("comparison_table.csv")
  B$side     <- rd("benchmark_sidebyside_agreement.csv")
  B$nfhs_b   <- rd("benchmark_district_wide.csv")
  B$ref_b    <- rd("benchmark_reference_district.csv")
  B$state_ag <- rd("state_agreement_nfhs5_ires.csv")
  B$season   <- rd("season_sensitivity.csv")
  B$ml_dw    <- rd("ml_vs_designwt_agreement.csv")
  B$marg     <- rd("glmer_cluster_marginalisation.csv")
  # Step 2
  B$calib    <- rd("calibration_parameters.csv")
  B$loso     <- rd("calibration_loso.csv")
  B$loso_s   <- rd("calibration_loso_summary.csv")
  B$support  <- rd("calibration_support_summary.csv")
  # Step 3
  B$health   <- rd("health_effects_table.csv")
  B$small    <- rd("health_small_cluster_inference.csv")
  B$simcal   <- rd("health_cluster_inference_calibration.csv")
  B$births   <- rd("mortality_birth_counts.csv")
  # Step 4 (descriptive variance decomposition only)
  B$dsum     <- rd("design_summary.csv")
  B$vdec     <- rd("design_variance_decomposition.csv")
  B$bundle   <- rd("BUNDLE.csv")

  # Under the hood
  B$calib_wt  <- rd("calibration_parameters_legA_wt.csv")
  B$sampler   <- rd("calibration_sampler.csv")
  B$rc_check  <- rd("calibration_state_effect_check.csv")
  B$me_se     <- rd("me_se_diagnostics.csv")
  B$shrink    <- rd("me_shrinkage.csv")
  B$mix_lev   <- rd("estimator_mix_levels.csv")
  B$mix_cor   <- rd("estimator_mix_correlations.csv")
  B$legA_s    <- rd("legA_symmetry_sensitivity.csv")
  B$nuanced   <- rd("health_nuanced_effects.csv")
  B$ppd       <- rd("ppd_sensitivity.csv")
  B$ppd_surf  <- rd("ppd_surface_comparison.csv")
  B$bayes_dx  <- rd("diag_bayes_fit_diagnostics.csv")
  B$fits_dx   <- rd("diag_model_fits_summary.csv")
  B$checks    <- rd("diag_pipeline_checks.csv")
  B$transfer  <- rd("diag_transfer_predictive.csv")
  B$transfer_coef  <- rd("diag_transfer_coefficients.csv")
  B$transfer_shift <- rd("diag_transfer_covariate_shift.csv")
  B$stack_ldo <- rd("stacking_ldo_validation.csv")
  B$stack_state     <- rd("stacking_state_validation.csv")
  B$stack_state_sum <- rd("stacking_state_validation_summary.csv")
  B$ref_stack <- rd("reference_stacking_district.csv")
  B$proxy     <- rd("district_exposure_proxy.csv")
  B$vpart     <- rd("stacking_variance_partition.csv")
  B$seas_nfhs <- rd("nfhs_main_fuel_seasonality.csv")

  # All health specifications, three analysis populations, one long table
  POPS <- c(rural = "Rural births (primary)", all = "All births", nonoverlap = "Rural, 36-month window (reduced cohort overlap)")
  sfx  <- c(rural = "", all = "_all", nonoverlap = "_nonoverlap")
  B$health_all <- do.call(rbind, lapply(names(POPS), function(k) {
    h <- rd(paste0("health_effects_table", sfx[[k]], ".csv"))
    if (is.null(h)) return(NULL)
    w <- rd(paste0("health_small_cluster_inference", sfx[[k]], ".csv"))
    h$p_wild <- NA_real_
    if (!is.null(w) && "p_wild" %in% names(w)) {
      key_h <- paste(h$outcome, h$term, h$weighting, h$adjusted %in% TRUE & h$support_stratum == "all")
      key_w <- paste(w$outcome, w$term, w$weighting, TRUE)
      h$p_wild <- w$p_wild[match(key_h, key_w)]
    }
    h$population <- POPS[[k]]
    mi <- grepl("_MI$", h$term)
    if (any(mi)) { appr <- if ("mi_method" %in% names(h)) h$mi_method %in% "exposure_only" else rep(FALSE, nrow(h)); h <- h[!mi | appr, ] }
    h[, setdiff(names(h), "mi_method"), drop = FALSE]
  }))
  B$legA_move <- do.call(rbind, lapply(names(POPS), function(k) {
    m <- rd(paste0("legA_symmetry_health_movement", sfx[[k]], ".csv"))
    if (is.null(m)) return(NULL); m$population <- POPS[[k]]; m
  }))
  B
}

# ---- derived quantities --------------------------------------------------------
derive <- function(B) {
  K <- list()
  s <- B$side
  g <- function(pair, var, col) {
    if (is.null(s)) return(NA_real_)
    v <- s[[col]][s$pair == pair & s$variable == var]
    if (length(v)) as.numeric(v[1]) else NA_real_
  }
  # Agreement on primary LPG, rural, design-weighted (like-for-like)
  K$r15   <- g("NFHS4_ACCESS", "lpg", "pearson_r");  K$d15 <- g("NFHS4_ACCESS", "lpg", "mean_diff_pp")
  K$r19   <- g("NFHS5_IRES",   "lpg", "pearson_r");  K$d19 <- g("NFHS5_IRES",   "lpg", "mean_diff_pp")
  K$n15   <- g("NFHS4_ACCESS", "lpg", "n_districts"); K$n19 <- g("NFHS5_IRES", "lpg", "n_districts")

  # Corrected surfaces: district averages (unweighted)
  d <- B$dist
  if (!is.null(d)) {
    K$raw15  <- mean(d$raw_wt_2015, na.rm = TRUE); K$raw19 <- mean(d$raw_wt_2019, na.rm = TRUE)
    K$cor15  <- mean(d$bayes_2015,  na.rm = TRUE); K$cor19 <- mean(d$bayes_2019,  na.rm = TRUE)
    K$n_dist <- sum(is.finite(d$bayes_2019))
    K$ext19  <- mean(d$extrapolated_2019 %in% TRUE & is.finite(d$bayes_2019)) /
                mean(is.finite(d$bayes_2019))
    K$ext15  <- mean(d$extrapolated_2015 %in% TRUE & is.finite(d$bayes_2015)) /
                mean(is.finite(d$bayes_2015))
    K$ciw19  <- median(d$bayes_hi_2019 - d$bayes_lo_2019, na.rm = TRUE)
  }
  # Headline averages and coverage come from the run's own summary files, so the
  # narrative cannot drift from the run if the district file is older.
  ml <- B$mix_lev
  if (!is.null(ml)) {
    g2 <- function(y, e) { v <- ml$mean_prev[ml$year == y & ml$estimator == e]; if (length(v)) as.numeric(v[1]) / 100 else NA_real_ }
    for (y in c(2015, 2019)) {
      r <- g2(y, "Raw design-weighted"); c2 <- g2(y, "Bayesian-corrected")
      if (is.finite(r)) K[[paste0("raw", y %% 100)]] <- r
      if (is.finite(c2)) K[[paste0("cor", y %% 100)]] <- c2
    }
  }
  cs <- B$support
  if (!is.null(cs) && all(c("n_districts", "n_state_support_2015", "n_state_support_2019") %in% names(cs))) {
    K$ext15 <- 1 - cs$n_state_support_2015[1] / cs$n_districts[1]
    K$ext19 <- 1 - cs$n_state_support_2019[1] / cs$n_districts[1]
  }
  if (!is.null(B$loso_s)) {
    K$loso_impr <- B$loso_s$n_improved[1]; K$loso_n <- B$loso_s$n_states_tested[1]
    K$loso_rmse <- B$loso_s$rmse_reduction_wtd_pc[1]
    K$loso_rmse_raw <- B$loso_s$rmse_raw_wtd[1]; K$loso_rmse_cor <- B$loso_s$rmse_corr_wtd[1]
  }

  # Health: adjusted, birth-weighted, all districts. Only approved rows: the
  # fixed surface and the exposure-only imputation (sensitivity). Any MI row
  # not marked exposure-only is dropped rather than shown.
  h <- B$health
  if (!is.null(h)) {
    mi <- grepl("_MI$", h$term)
    if (any(mi)) { appr <- if ("mi_method" %in% names(h)) h$mi_method %in% "exposure_only" else rep(FALSE, nrow(h)); h <- h[!mi | appr, ] }
    h <- h[h$adjusted %in% TRUE & h$weighting == "birth_hmean" & h$support_stratum == "all", ]
    K$h <- h
    row <- function(o, t) h[h$outcome == o & h$term == t, , drop = FALSE]
    K$inf_fix <- row("infant", "change_lpg_bayes"); K$inf_mi <- row("infant", "change_lpg_bayes_MI")
    K$n_health <- if (nrow(K$inf_fix)) K$inf_fix$n[1] else NA
    K$n_clus   <- if (nrow(K$inf_fix)) K$inf_fix$n_clusters[1] else NA
  }
  if (!is.null(B$simcal)) {
    conv <- B$simcal[grepl("^Conventional", B$simcal$rule), ]
    K$sim_conv <- if (nrow(conv)) conv$rejection_rate_at_nominal_05[1] else NA_real_
    cr2 <- B$simcal[grepl("^CR2", B$simcal$rule), ]
    K$sim_cr2  <- if (nrow(cr2)) cr2$rejection_rate_at_nominal_05[1] else NA_real_
    K$sim_reps <- if (nrow(cr2)) cr2$replications[1] else NA_real_
  }

  # Where the corrected exposure's uncertainty comes from (descriptive)
  K$decomp_ok   <- !identical(qv(B$dsum, "decomposition_valid"), 0)
  K$nfhs15      <- qv(B$dsum, "var_nfhs_side_share_2015")
  K$nfhs19      <- qv(B$dsum, "var_nfhs_side_share_2019")
  K
}

# District lookup with display labels
district_choices <- function(B) {
  d <- B$dist
  if (is.null(d)) return(character())
  d <- d[order(d$state_name, d$dist_name), ]
  stats::setNames(as.character(d$district),
                  paste0(trimws(gsub("\\s+", " ", d$dist_name)), ", ", d$state_name))
}

# Per-10-pp estimate: prefer the exported unrounded est_per10 over 10 x a
# rounded estimate.
per10 <- function(h) if ("est_per10" %in% names(h) && any(is.finite(h$est_per10))) h$est_per10 else 10 * h$estimate
