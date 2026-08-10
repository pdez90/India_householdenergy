# ==============================================================================
# H2_health_models.R   (STANDALONE -- does not source 00_config.R)
#
# Child-mortality associations using the CORRECTED clean-cooking exposure,
# in the district change-on-change design of India_lpg2.Rmd:
#
#   change(mortality, 2019-2015) ~ change(exposure) [+ covariate changes
#     + region FE], state-clustered SEs. MAIN model weights districts by the
#     HARMONIC MEAN of eligible births across the two rounds (w_births_hmean from
#     H1; #19) -- a births-based analytic weight, NOT a population denominator, so
#     it is labelled "birth_hmean", never "population-weighted". An UNWEIGHTED
#     model is reported alongside as a sensitivity.
#
# EVIDENCE HIERARCHY -- which row is the paper's answer.
# The exposure is measured four ways so you can watch the coefficient move as
# measurement error is removed. They are NOT co-equal, and the CSV now carries an
# explicit `evidence_rank` column so the ordering cannot be lost downstream:
#
#   rank 1  PRIMARY   Bayesian-corrected WITH correction uncertainty propagated
#                     (200-draw multiple imputation over the real posterior draws,
#                     pooled by Rubin's rules). This is the only specification
#                     whose interval accounts for the fact that the exposure is
#                     ESTIMATED rather than observed. It is the inferential result
#                     the manuscript reports and the one any claim rests on.
#   rank 2  SUPPORT   Bayesian-corrected point surface (correction applied, its
#                     uncertainty ignored). Intervals here are too narrow because
#                     they treat a corrected estimate as if it were measured.
#   rank 3  SUPPORT   Regression-calibrated.
#   rank 4  CONTEXT   Raw NFHS. Shown to display attenuation, not to be believed.
#
# Reporting rank 2 as the headline (as an earlier draft did) understates the
# uncertainty by exactly the quantity this paper exists to measure.
#
# Standalone inputs (files on disk only):
#   health_district_wide.rds        <- from H1_prep_mortality.R
#   corrected_nfhs_districts.rds     <- from 05_correction.R
# Optional:
#   df_wide_health.rds               <- from H3_env_covariates.R, for the
#                                       ambient covariates (PM2.5, mean
#                                       temperature, relative humidity, drought,
#                                       region). If absent, those are skipped and
#                                       only DHS-derived covariates are used.
#                                       H3 also writes weighted_temperature_change
#                                       (0.7*tmax + 0.3*tmin; formerly and wrongly
#                                       called a "heat index" -- it contains no
#                                       humidity term). It is collinear with
#                                       temp_change by construction and is NEVER
#                                       admitted to the adjustment set here.
#
# CALIBRATION-SUPPORT SENSITIVITY. 05_correction.R flags, per district, whether
# the correction was interpolation or extrapolation, in two distinct senses:
#   *_in_support   an ESTIMATED state effect was available (the district's state
#                  appears in the calibration sample), vs the pooled fallback;
#   *_cov_support  the district's RAW prevalence lies inside the range of raw
#                  prevalences the calibration model was actually fitted on.
# The PRIMARY (MI) estimate is re-fitted on three district strata -- all
# districts / estimated-state-effect only / full support -- and written with a
# `support_stratum` column, so a reader can see whether the association is being
# carried by districts whose exposure was extrapolated.
#
# Output: health_effects_table.csv, health_effects_coefplot.jpeg
# ==============================================================================

## ---- CONFIG ------------------------------------------------------------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"

# Population: "rural" (MAIN; matches the rural-only corrected exposure) or "all"
# (SI sensitivity). Set by run_health.R; defaults to rural for a standalone run.
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers
POP <- if (exists("POP")) POP else "rural"
# HEALTH_VARIANT="" = main (120-mo cohort); "_nonoverlap" = the SI short-window,
# non-overlapping-cohort sensitivity built by H1 (#16). Reads/writes the matching
# health_district_wide*<variant> and health_effects_table*<variant> files.
VARIANT <- Sys.getenv("HEALTH_VARIANT", "")
sfx <- paste0(if (POP == "all") "_all" else "", VARIANT)
message("H2 population: ", POP,
        if (nzchar(VARIANT)) paste0(" | variant: ", VARIANT) else "")

suppressPackageStartupMessages({
  library(tidyverse); library(lmtest); library(sandwich); library(broom)
})

path_health <- file.path(dir_out, paste0("health_district_wide", sfx, ".rds"))
path_corr   <- file.path(dir_out, "corrected_nfhs_districts.rds")
path_dfwide <- file.path(dir_out, "df_wide_health.rds")   # optional

for (p in c(path_health, path_corr))
  if (!file.exists(p)) stop("Required input missing: ", p,
                            "\nRun H1_prep_mortality.R and 05_correction.R first.")

health <- readRDS(path_health)
corr   <- readRDS(path_corr)

## ---- Join corrected exposures (keyed on dist_code, same as H1) ---------------
corr_j <- corr %>%
  transmute(district = as.character(as.numeric(district)),
            lpg15_raw = lpg_2015_rural, lpg19_raw = lpg_2019_rural,
            lpg15_rc  = .data[["lpg_2015_rc"]],  lpg19_rc = .data[["lpg_2019_rc"]],
            lpg15_b   = .data[["lpg_2015_bayes"]], lpg19_b = .data[["lpg_2019_bayes"]],
            lpg15_b_lo = .data[["lpg_2015_bayes_lo"]], lpg15_b_hi = .data[["lpg_2015_bayes_hi"]],
            lpg19_b_lo = .data[["lpg_2019_bayes_lo"]], lpg19_b_hi = .data[["lpg_2019_bayes_hi"]])

# Calibration-support flags (05_correction.R). Two different senses -- see the
# header. Absent if 05 has not been re-run since they were added, in which case
# the support-stratified sensitivity is skipped with a message rather than
# silently reporting the all-districts number three times.
supp_cols <- intersect(c("lpg_2015_in_support","lpg_2019_in_support",
                         "lpg_2015_cov_support","lpg_2019_cov_support",
                         "lpg_change_full_support"), names(corr))
DO_SUPPORT <- length(supp_cols) == 5L
if (length(supp_cols)) {
  corr_j <- left_join(
    corr_j,
    corr %>% transmute(district = as.character(as.numeric(district)),
                       across(all_of(supp_cols))),
    by = "district")
}
if (!DO_SUPPORT)
  message("Calibration-support flags not found in corrected_nfhs_districts.rds ",
          "(found: ", if (length(supp_cols)) paste(supp_cols, collapse = ", ") else "none",
          ") -- support-stratified sensitivity SKIPPED. Re-run 05_correction.R.")

# Instrument-consistent (IRES-rural calibration on BOTH rounds) exposure -- a
# sensitivity for the calibration-instrument-switch concern (option 3). Present
# only if 05 wrote the IRES-cal 2015 surface; otherwise skipped gracefully.
DO_IRESONLY <- "lpg_2015_irescal_bayes" %in% names(corr)
if (DO_IRESONLY) {
  ic <- corr %>% transmute(district = as.character(as.numeric(district)),
              lpg15_bic    = .data[["lpg_2015_irescal_bayes"]],
              lpg15_bic_lo = .data[["lpg_2015_irescal_bayes_lo"]],
              lpg15_bic_hi = .data[["lpg_2015_irescal_bayes_hi"]])
  corr_j <- left_join(corr_j, ic, by = "district")
}

# Leg-A SYMMETRY sensitivity exposure. The primary change_lpg_bayes is built from
# the ACCESS-calibrated 2015 surface, and that calibration used a binomial
# reference-side SE because the ACCESS multilevel reference has no design SE.
# 05_correction.R now also fits leg A with the ACCESS DESIGN-WEIGHTED estimate and
# its Taylor design SE -- exactly the leg-B treatment -- and writes the resulting
# 2015 surface. Recompute the corrected change from it, against the SAME 2019
# surface, so the movement isolates the leg-A reference construction.
DO_ACCESSWT <- "lpg_2015_accesswt_bayes" %in% names(corr)
if (DO_ACCESSWT) {
  aw <- corr %>% transmute(district = as.character(as.numeric(district)),
              lpg15_baw    = .data[["lpg_2015_accesswt_bayes"]],
              lpg15_baw_lo = .data[["lpg_2015_accesswt_bayes_lo"]],
              lpg15_baw_hi = .data[["lpg_2015_accesswt_bayes_hi"]])
  corr_j <- left_join(corr_j, aw, by = "district")
} else {
  message("Leg-A symmetry surface (lpg_2015_accesswt_bayes) not found in ",
          "corrected_nfhs_districts.rds -- sensitivity SKIPPED. Re-run 05_correction.R.")
}

df <- health %>%
  mutate(district = as.character(district)) %>%
  left_join(corr_j, by = "district") %>%
  mutate(
    change_lpg_raw   = 100 * (lpg19_raw - lpg15_raw),
    change_lpg_rc    = 100 * (lpg19_rc  - lpg15_rc),
    change_lpg_bayes = 100 * (lpg19_b   - lpg15_b),
    sd15 = (lpg15_b_hi - lpg15_b_lo) / 3.92,
    sd19 = (lpg19_b_hi - lpg19_b_lo) / 3.92
  )
if (DO_IRESONLY)
  df <- df %>% mutate(
    change_lpg_bayes_iresonly = 100 * (lpg19_b - lpg15_bic),  # IRES cal, both rounds
    sd15_ic = (lpg15_bic_hi - lpg15_bic_lo) / 3.92)
if (DO_ACCESSWT)
  df <- df %>% mutate(
    change_lpg_bayes_accesswt = 100 * (lpg19_b - lpg15_baw),  # design-weighted ACCESS ref
    sd15_aw = (lpg15_baw_hi - lpg15_baw_lo) / 3.92)
message(sprintf("Districts with mortality + corrected exposure: %d",
                sum(!is.na(df$change_lpg_bayes) & !is.na(df$change_neonatal_death))))

## ---- Optional: merge environmental + extra covariates from df_wide -----------
if (file.exists(path_dfwide)) {
  dfw <- readRDS(path_dfwide)
  id_col <- intersect(c("District.ID","dist_code","DHSREGCO","district"), names(dfw))[1]
  if (!is.na(id_col)) {
    dfw$district <- as.character(as.numeric(dfw[[id_col]]))
    # weighted_temperature_change is carried across for the collinearity
    # diagnostic printed below and for the SI sensitivity in H4; it is never
    # eligible for the adjustment set (see the stopifnot after `adj`).
    env_cols <- intersect(c("change_pm","temp_change","rh_change",
                            "weighted_temperature_change","droughtchange",
                            "evichange","region"), names(dfw))
    if (length(env_cols)) {
      df <- left_join(df, dfw %>% select(district, all_of(env_cols)) %>%
                        distinct(district, .keep_all = TRUE), by = "district")
      message("Merged from df_wide: ", paste(env_cols, collapse = ", "))
    }
  } else message("df_wide present but no recognizable district id; skipped.")
} else {
  message("df_wide_health.rds not found -- using DHS-derived covariates only ",
          "(no drought/EVI/heat/PM/region adjustment).")
}

## ---- District weight (#19): harmonic mean of eligible births across rounds ----
# This is an ANALYTIC weight based on eligible-birth counts (w_births_hmean from
# H1), NOT a population denominator, so results are labelled "birth_hmean" and
# never "population-weighted". If it is absent, the main model runs UNWEIGHTED and
# says so loudly (rather than silently). To weight by an actual child-population
# denominator you would have to merge one and rename accordingly.
wcol <- if ("w_births_hmean" %in% names(df)) "w_births_hmean" else NA
if (is.na(wcol))
  warning("w_births_hmean not found in health_district_wide -- H2 MAIN model runs ",
          "UNWEIGHTED. Re-run the updated H1_prep_mortality.R to add the ",
          "harmonic-mean birth weight (#19).")

# Coerce to a base data.frame so clustered-SE row recovery (rownames(model.frame))
# indexes the analysis frame positionally, robust to the upstream joins/filters.
df <- as.data.frame(df)

## ---- Adjustment set: whichever change_* covariates exist ---------------------
dhs_adj <- intersect(c("change_poor","change_mother_low_edu","change_electricity",
                       "change_muslim","change_improved_sanitation",
                       "change_improved_water"), names(df))
# NOTE: weighted_temperature_change (formerly mis-named "heat_index_change") is a
# weighted temperature composite, 0.7*tmax + 0.3*tmin with NO humidity term. It is
# a linear combination of the same two monthly fields as temp_change and is
# collinear with it by construction (r ~ 0.99), so only mean temperature change
# enters the adjustment set. The composite is deliberately excluded, and the
# stopifnot below makes that a hard error rather than a convention.
env_adj <- intersect(c("change_pm","temp_change","rh_change",
                       "droughtchange","evichange"),
                     names(df))
adj     <- c(dhs_adj, env_adj)
stopifnot(!("weighted_temperature_change" %in% adj),
          !("heat_index_change" %in% adj))

# The adjustment set is DOCUMENTED in Methods 2.4.5: six socioeconomic covariates
# and four ambient covariates. The intersect() calls above are permissive in both
# directions -- they silently DROP a documented covariate whose column failed to
# arrive from the upstream merge, and would silently ADD one (evichange) if
# H3_env_covariates.R ever supplied it. Either direction makes the Methods text
# false without any visible symptom, and the heat_index_change episode shows it
# can happen. Pin the realized set against the documented set here; the check is
# emitted with the rest at the end of the script.
ADJ_DOCUMENTED <- c("change_poor", "change_mother_low_edu", "change_electricity",
                    "change_muslim", "change_improved_sanitation",
                    "change_improved_water",          # six socioeconomic (Methods 2.4.5)
                    "change_pm", "temp_change", "rh_change", "droughtchange")
adj_missing <- setdiff(ADJ_DOCUMENTED, adj)
adj_extra   <- setdiff(adj, ADJ_DOCUMENTED)
if (length(adj_missing) || length(adj_extra))
  warning("adjustment set does not match Methods 2.4.5 -- missing: ",
          paste(c(adj_missing, "(none)")[seq_len(max(1L, length(adj_missing)))], collapse = ", "),
          " | unexpected: ",
          paste(c(adj_extra, "(none)")[seq_len(max(1L, length(adj_extra)))], collapse = ", "),
          call. = FALSE)
if (all(c("temp_change","weighted_temperature_change") %in% names(df)))
  message(sprintf(
    "  temp_change vs weighted_temperature_change: r = %.3f (collinear by construction; only the former is adjusted for)",
    suppressWarnings(cor(df$temp_change, df$weighted_temperature_change,
                         use = "complete.obs"))))
has_region <- "region" %in% names(df)
has_state  <- "state" %in% names(df)
message("Adjustment covariates: ", paste(adj, collapse = ", "),
        if (has_region) " + region FE" else "")

## ---- Model machinery ----------------------------------------------------------
# Reported mortality endpoints (see H1_prep_mortality.R): neonatal and infant.
OUTCOMES <- c(neonatal = "change_neonatal_death",
              infant   = "change_infant_death")

fit_one <- function(dat, outcome, exposure, adjusted = TRUE, wvec = NULL) {
  if (!outcome %in% names(dat)) return(NULL)
  dat <- as.data.frame(dat)   # base-frame semantics for row-name recovery below
  rhs <- c(exposure, if (adjusted) adj, if (has_region) "factor(region)")
  f   <- reformulate(rhs, response = outcome)
  # Weighted fits: drop rows with a missing/non-positive weight and reset row
  # names, so lm() accepts the weights and the clustered-SE row recovery below
  # stays positionally aligned with `dat`.
  if (!is.null(wvec)) {
    ok  <- is.finite(wvec) & wvec > 0
    dat <- dat[ok, , drop = FALSE]; rownames(dat) <- NULL; wvec <- wvec[ok]
  }
  m   <- lm(f, data = dat, weights = wvec)
  rows <- as.numeric(rownames(model.frame(m)))
  vc  <- if (has_state)
    sandwich::vcovCL(m, cluster = dat[["state"]][rows]) else sandwich::vcovHC(m, "HC1")
  ct  <- lmtest::coeftest(m, vcov. = vc)
  i   <- match(exposure, rownames(ct))
  tibble(term = exposure, estimate = ct[i,1], se = ct[i,2], p = ct[i,4],
         conf.low = ct[i,1] - 1.96*ct[i,2], conf.high = ct[i,1] + 1.96*ct[i,2],
         n = nobs(m))
}

## ---- Main comparison: raw / RC / Bayes, crude & adjusted ---------------------
exposures <- c("change_lpg_raw", "change_lpg_rc", "change_lpg_bayes")
if (DO_IRESONLY) exposures <- c(exposures, "change_lpg_bayes_iresonly")
if (DO_ACCESSWT) exposures <- c(exposures, "change_lpg_bayes_accesswt")
grid <- expand_grid(outcome = names(OUTCOMES),
                    exposure = exposures,
                    adjusted = c(FALSE, TRUE))
MAIN_TAG <- if (!is.na(wcol)) "birth_hmean" else "unweighted"
run_grid <- function(weighted) {
  wv  <- if (weighted && !is.na(wcol)) df[[wcol]] else NULL
  tag <- if (weighted) MAIN_TAG else "unweighted"
  pmap_dfr(grid, function(outcome, exposure, adjusted) {
    r <- fit_one(df, OUTCOMES[[outcome]], exposure, adjusted, wvec = wv)
    if (is.null(r)) return(NULL)
    mutate(r, outcome = outcome, adjusted = adjusted, weighting = tag, .before = 1)
  })
}
results      <- run_grid(TRUE)                          # MAIN (birth-hmean weighted)
results_unwt <- if (!is.na(wcol)) run_grid(FALSE) else NULL  # SENSITIVITY (unweighted)

## ---- Uncertainty propagation (multiple imputation) ---------------------------
# Preferred path: use the ACTUAL posterior draws of the corrected district LPG
# prevalence saved by 05_correction.R (correction_posterior_draws.rds). Each
# imputation takes one joint posterior draw for 2015 and 2019, so the imputed
# change reflects the true (asymmetric, district-structured, correlated) posterior
# rather than an independent Normal approximation from the 95% CrI endpoints.
# If that file is absent, fall back to the Normal-from-CrI approximation.
# set.seed only governs the Normal-from-CrI FALLBACK below; the real-draws path
# selects its imputations deterministically (see di_idx_all).
M <- 200; set.seed(2026)
path_draws <- file.path(dir_out, "correction_posterior_draws.rds")
use_real_draws <- file.exists(path_draws)
if (use_real_draws) {
  pd  <- readRDS(path_draws)
  Y15 <- pd$y2015; Y19 <- pd$y2019
  id15 <- as.character(as.numeric(pd$districts_2015))
  id19 <- as.character(as.numeric(pd$districts_2019))
  n_avail <- min(nrow(Y15), nrow(Y19))
  # IRES-calibrated 2015 draws for the instrument-consistent sensitivity MI.
  has_ic_draws <- DO_IRESONLY && "y2015_irescal" %in% names(pd)
  if (has_ic_draws) {
    Y15ic  <- pd$y2015_irescal
    id15ic <- as.character(as.numeric(pd$districts_2015_irescal))
  }
  # Design-weighted-ACCESS 2015 draws for the leg-A symmetry sensitivity MI.
  has_aw_draws <- DO_ACCESSWT && "y2015_accesswt" %in% names(pd)
  if (has_aw_draws) {
    Y15aw  <- pd$y2015_accesswt
    id15aw <- as.character(as.numeric(pd$districts_2015_accesswt))
  }
  message(sprintf("MI: using %d real posterior draws from %s", n_avail,
                  basename(path_draws)))

  # ---- WHICH draws -----------------------------------------------------------
  # Taking rows 1..M in file order (the previous behaviour) is a CONTIGUOUS BLOCK
  # from the start of chain 1 -- not a sample of the posterior. It excludes
  # between-chain variation entirely and its draws are autocorrelated, so the
  # between-imputation variance bvar in Rubin's rules comes out too small and the
  # pooled MI standard errors too narrow. Thin evenly across the whole draws
  # matrix instead: every chain is represented and successive imputations sit
  # ~n_avail/M draws apart, far beyond the chains' autocorrelation length. This is
  # deterministic (no RNG), so the MI results stay bit-reproducible.
  di_idx_all <- if (n_avail >= M) round(seq(1, n_avail, length.out = M)) else
                  ((seq_len(M) - 1L) %% n_avail) + 1L
  stopifnot(length(di_idx_all) == M, all(di_idx_all >= 1), all(di_idx_all <= n_avail))

  # Diagnostic that would have caught the old behaviour: a representative
  # selection must reproduce the SPREAD of the full posterior. Mean over districts
  # of the per-district posterior SD, selected draws vs all draws. Evenly thinned
  # -> ratio ~1; a single-chain head block -> visibly below 1.
  .sd_ratio <- function(Y)
    mean(apply(Y[di_idx_all, , drop = FALSE], 2, sd), na.rm = TRUE) /
    mean(apply(Y, 2, sd), na.rm = TRUE)
  mi_sd_ratio_15 <- .sd_ratio(Y15)
  mi_sd_ratio_19 <- .sd_ratio(Y19)
  message(sprintf(
    "MI: draw indices %d..%d, mean spacing %.1f, %d distinct of %d available",
    min(di_idx_all), max(di_idx_all),
    if (M > 1) mean(diff(sort(di_idx_all))) else NA_real_,
    length(unique(di_idx_all)), n_avail))
  message(sprintf(
    "MI: selected-draw SD / full-posterior SD = %.3f (2015), %.3f (2019)",
    mi_sd_ratio_15, mi_sd_ratio_19))
} else {
  has_ic_draws <- FALSE
  has_aw_draws <- FALSE
  di_idx_all   <- seq_len(M)
  mi_sd_ratio_15 <- mi_sd_ratio_19 <- NA_real_
  message("MI: correction_posterior_draws.rds not found -- falling back to the ",
          "Normal-from-CrI approximation.")
}

# Imputer for the PRIMARY (era-matched) corrected change.
impute_change <- function(m) {
  if (use_real_draws) {
    di_idx <- di_idx_all[m]
    v15 <- setNames(Y15[di_idx, ], id15); v19 <- setNames(Y19[di_idx, ], id19)
    df %>% mutate(change_lpg_mi = 100 * (unname(v19[district]) - unname(v15[district])))
  } else {
    df %>% mutate(
      l15 = pmin(pmax(rnorm(n(), lpg15_b, sd15), 0), 1),
      l19 = pmin(pmax(rnorm(n(), lpg19_b, sd19), 0), 1),
      change_lpg_mi = 100 * (l19 - l15))
  }
}

# Imputer for the instrument-consistent (IRES cal, both rounds) sensitivity.
impute_change_ic <- function(m) {
  if (has_ic_draws) {
    di_idx <- di_idx_all[m]
    v15 <- setNames(Y15ic[di_idx, ], id15ic); v19 <- setNames(Y19[di_idx, ], id19)
    df %>% mutate(change_lpg_mi = 100 * (unname(v19[district]) - unname(v15[district])))
  } else {
    df %>% mutate(
      l15 = pmin(pmax(rnorm(n(), lpg15_bic, sd15_ic), 0), 1),
      l19 = pmin(pmax(rnorm(n(), lpg19_b, sd19), 0), 1),
      change_lpg_mi = 100 * (l19 - l15))
  }
}

# Imputer for the leg-A symmetry (design-weighted ACCESS reference) sensitivity.
impute_change_aw <- function(m) {
  if (has_aw_draws) {
    di_idx <- di_idx_all[m]
    v15 <- setNames(Y15aw[di_idx, ], id15aw); v19 <- setNames(Y19[di_idx, ], id19)
    df %>% mutate(change_lpg_mi = 100 * (unname(v19[district]) - unname(v15[district])))
  } else {
    df %>% mutate(
      l15 = pmin(pmax(rnorm(n(), lpg15_baw, sd15_aw), 0), 1),
      l19 = pmin(pmax(rnorm(n(), lpg19_b, sd19), 0), 1),
      change_lpg_mi = 100 * (l19 - l15))
  }
}

# Rubin's-rules pooling of a MI imputer into one row per outcome.
# `keep` restricts the analysis to a subset of districts (used by the
# calibration-support sensitivity); it is a logical vector aligned to rows of df.
pool_mi <- function(imputer, term_name, keep = NULL, stratum = "all") {
  map_dfr(names(OUTCOMES), function(outc) {
    ocol <- OUTCOMES[[outc]]
    if (!ocol %in% names(df)) return(NULL)
    draws <- map_dfr(seq_len(M), function(m) {
      d_imp <- imputer(m)
      if (!is.null(keep)) {
        d_imp <- d_imp[which(keep), , drop = FALSE]
        rownames(d_imp) <- NULL
      }
      fit_one(d_imp, ocol, "change_lpg_mi", adjusted = TRUE,
              wvec = if (!is.na(wcol)) d_imp[[wcol]] else NULL)
    })
    if (nrow(draws) == 0) return(NULL)
    qbar <- mean(draws$estimate); ubar <- mean(draws$se^2); bvar <- var(draws$estimate)
    tvar <- ubar + (1 + 1/M) * bvar
    tibble(outcome = outc, adjusted = TRUE, weighting = MAIN_TAG, term = term_name,
           estimate = qbar, se = sqrt(tvar),
           conf.low = qbar - 1.96*sqrt(tvar), conf.high = qbar + 1.96*sqrt(tvar),
           p = 2*pnorm(-abs(qbar/sqrt(tvar))), n = max(draws$n),
           support_stratum = stratum)
  })
}
mi_results    <- pool_mi(impute_change,    "change_lpg_bayes_MI")
mi_results_ic <- if (DO_IRESONLY) pool_mi(impute_change_ic, "change_lpg_bayes_iresonly_MI") else NULL
mi_results_aw <- if (DO_ACCESSWT) pool_mi(impute_change_aw, "change_lpg_bayes_accesswt_MI") else NULL

## ---- Calibration-support sensitivity (review item 5) -------------------------
# Does the PRIMARY association survive when districts whose corrected exposure was
# an EXTRAPOLATION are dropped? Two strata beyond the full sample:
#   state_support  both rounds used an ESTIMATED state effect (the district's
#                  state is represented in the calibration survey), rather than
#                  the pooled fallback for unrepresented states;
#   full_support   the above AND both rounds' raw prevalence lies inside the
#                  range of raw prevalences the calibration model was fitted on
#                  (i.e. no covariate-space extrapolation either).
# Restricting support trades bias for precision: the strata are smaller, so wider
# intervals are EXPECTED and are not by themselves evidence against the result.
# What matters is whether the POINT estimate moves.
mi_support <- NULL
if (DO_SUPPORT) {
  keep_state <- coalesce(df$lpg_2015_in_support, FALSE) &
                coalesce(df$lpg_2019_in_support, FALSE)
  keep_full  <- coalesce(df$lpg_change_full_support, FALSE)
  strata <- list(state_support = keep_state, full_support = keep_full)
  cat("\n-- Calibration-support strata (districts in the H2 analysis frame) --\n")
  cat(sprintf("   all districts    : %d\n", nrow(df)))
  for (nm in names(strata))
    cat(sprintf("   %-17s: %d (%.1f%% of all)\n", nm, sum(strata[[nm]]),
                100 * mean(strata[[nm]])))
  mi_support <- map_dfr(names(strata), function(nm) {
    k <- strata[[nm]]
    # Refuse to fit a stratum too small to support the adjustment set; report the
    # skip rather than emitting an unstable estimate.
    if (sum(k) < (length(adj) + 10L)) {
      message(sprintf("  support stratum '%s' has only %d districts -- SKIPPED.",
                      nm, sum(k)))
      return(NULL)
    }
    pool_mi(impute_change, "change_lpg_bayes_MI", keep = k, stratum = nm)
  })
} else {
  message("Support-stratified MI sensitivity skipped (flags unavailable).")
}

# EVIDENCE HIERARCHY, made machine-readable (see the header). rank 1 is the
# paper's inferential result; rank 4 exists only to display attenuation. The
# `evidence_role` string travels with the rank so a downstream reader of the CSV
# alone cannot mistake a support row for the headline.
rank_of <- function(term) dplyr::case_when(
  grepl("_MI$", term)                  ~ 1L,   # uncertainty-propagated (PRIMARY)
  grepl("_bayes(_iresonly|_accesswt)?$", term) ~ 2L,  # corrected point surface
  term == "change_lpg_rc"              ~ 3L,   # regression-calibrated
  term == "change_lpg_raw"             ~ 4L,   # raw NFHS (context only)
  TRUE                                 ~ NA_integer_)
role_of <- function(rank) dplyr::case_when(
  rank == 1L ~ "PRIMARY (uncertainty-propagated)",
  rank == 2L ~ "SUPPORT (corrected point surface; interval too narrow)",
  rank == 3L ~ "SUPPORT (regression-calibrated)",
  rank == 4L ~ "CONTEXT (raw NFHS; attenuated, not to be believed)",
  TRUE       ~ NA_character_)

results_all <- bind_rows(results, results_unwt, mi_results, mi_results_ic,
                         mi_results_aw, mi_support)
# The non-MI grids carry no support_stratum; if every MI fit failed the column is
# absent entirely. Create it before filling so the pipeline degrades to a message
# rather than an error.
if (!"support_stratum" %in% names(results_all))
  results_all$support_stratum <- NA_character_
results_all <- results_all %>%
  mutate(support_stratum = tidyr::replace_na(support_stratum, "all"),
         est_per10 = estimate*10, lo_per10 = conf.low*10, hi_per10 = conf.high*10,
         across(where(is.numeric), ~ round(.x, 4))) %>%
  # Ranks are assigned AFTER the numeric rounding so `across(where(is.numeric))`
  # cannot coerce the integer rank to a rounded double.
  mutate(evidence_rank = rank_of(term),
         evidence_role = role_of(evidence_rank)) %>%
  # Sort so the PRIMARY rows are first in the file as well as in the printout.
  arrange(evidence_rank, outcome, support_stratum, weighting, desc(adjusted)) %>%
  relocate(evidence_rank, evidence_role, support_stratum, .after = term)
write_csv(results_all, file.path(dir_out, paste0("health_effects_table", sfx, ".csv")))

## ---- Leg-A symmetry sensitivity: how far does the headline move? -------------
# The question a reviewer asks is not "did the slope change" but "did the
# CONCLUSION change". Report, per outcome, the primary uncertainty-propagated
# estimate beside the sensitivity's, the absolute and relative movement, and
# whether the movement is small compared with the primary estimate's own SE --
# the only comparison that decides whether the asymmetry mattered.
legA_move <- NULL
if (DO_ACCESSWT) {
  .pull <- function(tm) results_all %>%
    filter(term == tm, weighting == MAIN_TAG, support_stratum == "all") %>%
    select(outcome, estimate, se, conf.low, conf.high, p)
  legA_move <- purrr::map_dfr(
    list(c("change_lpg_bayes_MI", "change_lpg_bayes_accesswt_MI", "MI (primary)"),
         c("change_lpg_bayes",    "change_lpg_bayes_accesswt",    "point surface")),
    function(z) {
      pr <- .pull(z[1]); se <- .pull(z[2])
      if (nrow(pr) == 0 || nrow(se) == 0) return(NULL)
      inner_join(pr, se, by = "outcome", suffix = c("_primary", "_sens")) %>%
        mutate(comparison = z[3],
               diff        = estimate_sens - estimate_primary,
               pct_change  = 100 * (estimate_sens - estimate_primary) /
                             ifelse(estimate_primary == 0, NA_real_,
                                    abs(estimate_primary)),
               diff_in_primary_se = diff / se_primary,
               sign_flip   = sign(estimate_sens) != sign(estimate_primary),
               sig_flip    = (p_primary < 0.05) != (p_sens < 0.05)) %>%
        relocate(comparison, .before = outcome)
    })
  write_csv(legA_move,
            file.path(dir_out, paste0("legA_symmetry_health_movement", sfx, ".csv")))
  cat("\n===== Leg-A symmetry sensitivity: design-weighted ACCESS reference =====\n")
  cat("    Primary calibrates NFHS-4 against the ACCESS MULTILEVEL estimate with a\n",
      "    binomial reference SE; the sensitivity uses the ACCESS DESIGN-WEIGHTED\n",
      "    estimate with its Taylor design SE, i.e. exactly the leg-B treatment.\n",
      "    2019 surface identical in both, so all movement is leg-A construction.\n", sep = "")
  print(as.data.frame(legA_move %>%
          transmute(comparison, outcome,
                    primary = round(estimate_primary, 4),
                    sensitivity = round(estimate_sens, 4),
                    diff = round(diff, 4),
                    pct = round(pct_change, 1),
                    `diff/SE` = round(diff_in_primary_se, 3),
                    sign_flip, sig_flip)), row.names = FALSE)
  chk("H2", "leg-A sensitivity does not flip the sign of any primary estimate",
      !any(legA_move$sign_flip, na.rm = TRUE),
      { u <- legA_move$outcome[which(legA_move$sign_flip)]
        if (length(u)) paste("SIGN FLIPPED:", paste(u, collapse = ", "))
        else "no sign flips" })
  chk_warn("H2", "leg-A sensitivity moves the MI estimate by < 0.5 of its own SE",
      { m <- legA_move %>% filter(comparison == "MI (primary)")
        nrow(m) > 0 && all(abs(m$diff_in_primary_se) < 0.5, na.rm = TRUE) },
      { m <- legA_move %>% filter(comparison == "MI (primary)")
        paste(sprintf("%s: %+.3f (%.2f SE)", m$outcome, m$diff,
                      m$diff_in_primary_se), collapse = "; ") })
  chk_warn("H2", "leg-A sensitivity does not change any 0.05 significance verdict",
      !any(legA_move$sig_flip, na.rm = TRUE),
      { u <- legA_move$outcome[which(legA_move$sig_flip)]
        if (length(u)) paste("VERDICT CHANGED:", paste(u, collapse = ", "))
        else "no verdict changes" })
  chk_file("H2", "leg-A symmetry movement table written",
           paste0("legA_symmetry_health_movement", sfx, ".csv"))
} else {
  message("Leg-A symmetry movement table skipped (sensitivity did not run).")
}

cat("\n===== Child-mortality associations (per 10-pp rise in LPG main-fuel) =====\n")
cat("    MAIN weighting = ", MAIN_TAG,
    "; an unweighted sensitivity is in the 'unweighted' rows of the CSV.\n", sep = "")
cat("    Rows are ordered by evidence_rank: rank 1 (MI, uncertainty propagated)\n",
    "    is the manuscript's inferential result. Ranks 2-4 show how the estimate\n",
    "    moves as measurement error is removed; their intervals do NOT account\n",
    "    for the exposure being estimated and must not be quoted as the headline.\n",
    sep = "")
cat("\n-- rank 1: PRIMARY (uncertainty-propagated, MI over posterior draws) --\n")
print(as.data.frame(results_all %>% filter(adjusted, evidence_rank == 1L) %>%
        select(outcome, term, support_stratum, weighting,
               est_per10, lo_per10, hi_per10, p, n)), digits = 3)
cat("\n-- ranks 2-4: supporting / context specifications --\n")
print(as.data.frame(results_all %>% filter(adjusted, evidence_rank > 1L) %>%
        select(outcome, term, evidence_rank, weighting,
               est_per10, lo_per10, hi_per10, p, n)), digits = 3)

## ---- Coefficient plot (MAIN weighting only) ----------------------------------
# Full-sample rows only: the support strata are a sensitivity, not extra
# exposures, and would otherwise plot as duplicate points on the same row.
plot_df <- results_all %>%
  filter(adjusted, weighting == MAIN_TAG, support_stratum == "all") %>%
  mutate(exposure = recode(term,
    change_lpg_raw="Raw NFHS", change_lpg_rc="Regression-calibrated",
    change_lpg_bayes="Bayesian-corrected",
    change_lpg_bayes_MI="Bayesian-corrected\n(+ uncertainty)",
    change_lpg_bayes_iresonly="Bayesian-corrected\n(IRES cal, both rounds)",
    change_lpg_bayes_iresonly_MI="Bayesian-corrected, IRES both\n(+ uncertainty)",
    change_lpg_bayes_accesswt="Bayesian-corrected\n(design-weighted ACCESS ref)",
    change_lpg_bayes_accesswt_MI="Bayesian-corrected, design-wtd ACCESS\n(+ uncertainty)"),
    exposure = factor(exposure, levels = c("Raw NFHS","Regression-calibrated",
      "Bayesian-corrected","Bayesian-corrected\n(+ uncertainty)",
      "Bayesian-corrected\n(IRES cal, both rounds)",
      "Bayesian-corrected, IRES both\n(+ uncertainty)",
      "Bayesian-corrected\n(design-weighted ACCESS ref)",
      "Bayesian-corrected, design-wtd ACCESS\n(+ uncertainty)")),
    outcome = factor(outcome, levels = c("neonatal","infant","child")))
p <- ggplot(plot_df, aes(est_per10, exposure)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_pointrange(aes(xmin = lo_per10, xmax = hi_per10)) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Change in mortality (per 100 births) per 10-pp rise in LPG main-fuel prevalence",
       y = NULL, title = NULL, subtitle = NULL)
ggsave(file.path(dir_out, paste0("health_effects_coefplot", sfx, ".jpeg")), p,
       width = 8, height = 8, dpi = 300)

## ---- CHECKS ------------------------------------------------------------------
chk_header(paste0("H2_health_models", if (nzchar(VARIANT)) VARIANT else ""))
chk("H2", "results table has all exposures + weighting column",
    chk_has_cols(results_all, c("term","weighting","est_per10")) &&
    all(c("change_lpg_raw","change_lpg_bayes") %in% results_all$term))
chk("H2", "instrument-consistent sensitivity ran (IRES cal both rounds)",
    DO_IRESONLY && "change_lpg_bayes_iresonly" %in% results_all$term,
    if (DO_IRESONLY) "present" else "SKIPPED (re-run 05 for lpg_2015_irescal)")
chk("H2", "leg-A symmetry sensitivity ran (design-weighted ACCESS reference)",
    DO_ACCESSWT && "change_lpg_bayes_accesswt" %in% results_all$term,
    if (DO_ACCESSWT) "present" else "SKIPPED (re-run 05 for lpg_2015_accesswt)")
chk("H2", "leg-A sensitivity used real posterior draws, not the CrI approximation",
    !DO_ACCESSWT || isTRUE(has_aw_draws),
    if (!DO_ACCESSWT) "n/a" else if (isTRUE(has_aw_draws)) "y2015_accesswt draws used"
      else "FELL BACK to Normal-from-CrI -- re-run 05")
chk("H2", "MI (uncertainty-propagated) rows present",
    any(grepl("_MI$", results_all$term)))
# The evidence hierarchy must not silently invert: the MI rows ARE the paper's
# primary result, and every row must carry a rank.
chk("H2", "evidence_rank present and MI rows are rank 1",
    chk_has_cols(results_all, c("evidence_rank","evidence_role","support_stratum")) &&
    all(results_all$evidence_rank[grepl("_MI$", results_all$term)] == 1L),
    paste0("ranks present: ",
           paste(sort(unique(results_all$evidence_rank)), collapse = ",")))
chk("H2", "no result row is left unranked",
    !any(is.na(results_all$evidence_rank)),
    { u <- unique(results_all$term[is.na(results_all$evidence_rank)])
      if (length(u)) paste("unranked terms:", paste(u, collapse = ", ")) else "all ranked" })
chk("H2", "raw NFHS is ranked last (context only)",
    all(results_all$evidence_rank[results_all$term == "change_lpg_raw"] == 4L))
chk("H2", "weighted temperature composite excluded from the adjustment set",
    !any(c("weighted_temperature_change","heat_index_change") %in% adj),
    paste("adj =", paste(adj, collapse = ", ")))
# The set itself, not just the two names known to have gone wrong before. Methods
# 2.4.5 names six socioeconomic and four ambient covariates; if the realized set
# is not exactly that set, the manuscript is describing a different model.
chk("H2", "adjustment set matches Methods 2.4.5 exactly",
    length(adj_missing) == 0 && length(adj_extra) == 0,
    sprintf("%d covariates | missing: %s | unexpected: %s", length(adj),
            if (length(adj_missing)) paste(adj_missing, collapse = ", ") else "none",
            if (length(adj_extra))   paste(adj_extra,   collapse = ", ") else "none"))
# Methods 2.4.5 states the MI uses the actual saved posterior draws, "not a normal
# approximation from the credible-interval endpoints". The fallback branch is
# silent by design, so assert the claim rather than trusting it.
chk("H2", "MI used the saved posterior draws (not the Normal fallback)",
    isTRUE(use_real_draws),
    if (isTRUE(use_real_draws)) "correction_posterior_draws.rds"
    else "FELL BACK to Normal-from-CrI -- Methods 2.4.5 claims the real draws")
# And that those draws are a representative slice of the posterior, not a
# contiguous head block from one chain (which biases Rubin's bvar downward).
# Structural test first -- the selection must REACH BOTH ENDS of the draws matrix,
# which a contiguous head block cannot do however long it is, and which is what
# actually guarantees every chain is represented. The SD ratio is the numeric
# corroboration: with chain means close together a head block can still look
# almost right on spread alone, so span is the decisive condition.
chk("H2", "MI draws span the full posterior (all chains represented)",
    isTRUE(exists("n_avail") && is.finite(n_avail) &&
           min(di_idx_all) <= 0.05 * n_avail &&
           max(di_idx_all) >= 0.95 * n_avail &&
           is.finite(mi_sd_ratio_15) && is.finite(mi_sd_ratio_19) &&
           abs(mi_sd_ratio_15 - 1) < 0.10 && abs(mi_sd_ratio_19 - 1) < 0.10),
    sprintf("draws %d..%d of %d (spacing %.1f); selected/full posterior SD %.3f (2015), %.3f (2019)",
            min(di_idx_all), max(di_idx_all),
            if (exists("n_avail")) n_avail else NA_integer_,
            if (length(di_idx_all) > 1) mean(diff(sort(di_idx_all))) else NA_real_,
            mi_sd_ratio_15, mi_sd_ratio_19))
chk_warn("H2", "calibration-support sensitivity ran",
    DO_SUPPORT && !is.null(mi_support) && nrow(mi_support) > 0,
    if (DO_SUPPORT) "state_support + full_support strata fitted"
    else "SKIPPED (re-run 05_correction.R for the support flags)")
# The substantive question the sensitivity exists to answer: does dropping
# extrapolated districts move the POINT estimate? Wider intervals are expected
# (smaller n) and are not a failure; a sign flip or a large shift is.
# The comparison is built HERE, before the check, so the check can report the
# numbers it actually saw. The previous version computed it inside the condition
# and passed a static sentence -- "no sign flip and <50% shift ..." -- as the
# detail, so a WARN printed a description of the criterion it had just failed
# and was indistinguishable from a PASS at a glance.
.sup_cmp <- if (!DO_SUPPORT || is.null(mi_support) || !nrow(mi_support)) NULL else {
  .a <- results_all %>% dplyr::filter(term == "change_lpg_bayes_MI",
                                      support_stratum == "all")
  .s <- results_all %>% dplyr::filter(term == "change_lpg_bayes_MI",
                                      support_stratum != "all")
  dplyr::inner_join(.a %>% dplyr::select(outcome, e_all = estimate),
                    .s %>% dplyr::select(outcome, support_stratum, e_s = estimate),
                    by = "outcome") %>%
    # Tolerance: 50% of the full-sample estimate, with an absolute floor of
    # 0.005 (= 0.05 deaths per 100 births per 10-pp) so a near-null full-sample
    # estimate does not make any stratum difference look like a failure.
    dplyr::mutate(tol = pmax(0.5 * abs(e_all), 0.005),
                  ok  = sign(e_all) == sign(e_s) & abs(e_s - e_all) <= tol,
                  pct = ifelse(e_all == 0, NA_real_,
                               100 * (e_s - e_all) / abs(e_all)))
}
chk_warn("H2", "primary estimate stable across calibration-support strata",
    is.null(.sup_cmp) || nrow(.sup_cmp) == 0 || all(.sup_cmp$ok),
    if (is.null(.sup_cmp))
      "support sensitivity not run -- nothing to compare"
    else if (!nrow(.sup_cmp))
      "no outcome appears in both the all-districts and stratified results"
    else
      paste(sprintf("%s / %s: all = %.4f, stratum = %.4f (%+.0f%%)%s",
                    .sup_cmp$outcome, .sup_cmp$support_stratum,
                    .sup_cmp$e_all, .sup_cmp$e_s, .sup_cmp$pct,
                    ifelse(.sup_cmp$ok, "", "  <-- EXCEEDS TOLERANCE")),
            collapse = " | "))
chk("H2", "main model weighted by eligible births (not 'population')",
    MAIN_TAG == "birth_hmean",
    paste0("MAIN weighting = ", MAIN_TAG))
chk_warn("H2", "unweighted sensitivity present",
    "unweighted" %in% results_all$weighting)
chk_warn("H2", "corrected >= raw effect (measurement-error direction)",
    { g <- results_all %>% dplyr::filter(adjusted, weighting == MAIN_TAG, outcome == "infant")
      rw <- g$estimate[g$term == "change_lpg_raw"]; bz <- g$estimate[g$term == "change_lpg_bayes"]
      length(rw) && length(bz) && bz <= rw },
    "infant: Bayes at least as protective as raw")
chk_file("H2", "effects table written",
    paste0("health_effects_table", sfx, ".csv"))

message("H2 done. See health_effects_table.csv and health_effects_coefplot.jpeg in ", dir_out)
message("  The manuscript's inferential result is the evidence_rank == 1 rows ",
        "(term ending _MI, support_stratum = 'all'): the corrected exposure with ",
        "correction uncertainty propagated. Ranks 2-4 are shown to display the ",
        "attenuation gradient and must not be quoted as the headline estimate.")
