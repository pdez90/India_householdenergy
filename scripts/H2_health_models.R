# ==============================================================================
# H2_health_models.R   (STANDALONE -- does not source 00_config.R)
#
# Child-mortality associations using the CORRECTED clean-cooking exposure,
# in the district change-on-change design of India_lpg2.Rmd:
#
#   change(mortality, 2019-2015) ~ change(exposure) [+ covariate changes
#     + region FE], state-clustered SEs. MAIN model weights districts by the
#     HARMONIC MEAN, across the two rounds, of the births ELIGIBLE for each
#     outcome (w_hmean_neonatal / w_hmean_infant from H1; #19) -- a births-based analytic weight, NOT a population denominator, so
#     it is labelled "birth_hmean", never "population-weighted". An UNWEIGHTED
#     model is reported alongside as a sensitivity.
#
# EVIDENCE HIERARCHY -- how each row may be read (restricted framing, Oct 2026).
# The exposure is measured several ways so you can watch the coefficient move as
# measurement error is removed. They are NOT co-equal, and the CSV carries an
# explicit `evidence_rank` column so the ordering cannot be lost downstream:
#
#   rank 1  CONDITIONAL / EXPLORATORY  Bayesian-corrected surface held fixed.
#                     The association the manuscript and app report, as a
#                     conditional, exploratory estimate: it is conditional on the
#                     fitted calibration and its interval EXCLUDES calibration
#                     uncertainty.
#   rank 2  SENSITIVITY  Exposure-only multiple imputation over the calibration
#                     posterior. Carries calibration uncertainty but does not
#                     condition on the outcome, so it is at risk of attenuation.
#                     The outcome-conditioned imputation that would address that
#                     failed its convergence checks and was abandoned
#                     (docs/mime_v2_spec.md, Section 9).
#   rank 3  SUPPORT   Regression-calibrated.
#   rank 4  CONTEXT   Raw NFHS. Shown to display attenuation by measurement error.
#
# No row is a validated estimate with calibration uncertainty propagated.
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
# The exposure-only MI (sensitivity) estimate is re-fitted on three district strata -- all
# districts / estimated-state-effect only / full support -- and written with a
# `support_stratum` column, so a reader can see whether the association is being
# carried by districts whose exposure was extrapolated.
#
# Output: health_effects_table.csv, health_effects_coefplot.jpeg
# ==============================================================================

## ---- CONFIG ------------------------------------------------------------------
if (!exists(".env_dir")) source("paths.R")   # path resolution; fails loudly if unconfigured
dir_out <- .dir_out()

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
# An ANALYTIC weight based on outcome-eligible birth counts, NOT a population
# denominator, so results are labelled "birth_hmean" and never
# "population-weighted". If the weights are absent the script stops (below).
# Outcome-specific analytic weight (H1): the harmonic mean, across the two rounds,
# of the births ELIGIBLE for that outcome (exposed for its full age interval), so
# each outcome is weighted by its own denominator; neonatal and infant
# eligibility differ. Required: a frame without these columns was written by an
# older H1, whose all-births weight included births not yet exposed for the
# full interval.
WCOLS <- c(change_neonatal_death = "w_hmean_neonatal",
           change_infant_death   = "w_hmean_infant")
if (!all(WCOLS %in% names(df)))
  stop("[H2] outcome-specific birth weights (", paste(WCOLS, collapse = ", "),
       ") not found in the health district frame; re-run H1_prep_mortality.R.", call. = FALSE)
wcol <- "w_hmean_<outcome>"   # weighted; the column is chosen per outcome by wv_for()
wv_for <- function(dat, ocol) {
  if (!ocol %in% names(WCOLS)) stop("[H2] no birth weight defined for outcome ", ocol, call. = FALSE)
  dat[[WCOLS[[ocol]]]]
}

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

# ---- Small-cluster inference: availability -----------------------------------
# clubSandwich supplies CR2 (Bell-McCaffrey bias-reduced clustered covariance)
# with Satterthwaite degrees of freedom. It is a REQUIRED dependency for the
# published inference: CR2/Satterthwaite is the primary rule for every
# state-clustered regression in the paper, and the manuscript builder reads the
# CR2 columns directly. Without the package the models still fit, but the run
# cannot reproduce the reported inference -- it produces the superseded
# conventional numbers only. run_everything.R therefore lists clubSandwich in
# its preflight and refuses to start without it; the guard here remains so that
# a single script sourced by hand fails with an explanation rather than with an
# unexplained column of NAs.
CR2_OK <- requireNamespace("clubSandwich", quietly = TRUE)
CR2_FAILURES <- 0L
if (!CR2_OK)
  message("[H2] clubSandwich is not installed. CR2/Satterthwaite is the PRIMARY ",
          "inference for this paper, so this run CANNOT reproduce the published ",
          "numbers. install.packages('clubSandwich'), or restore the recorded ",
          "environment with renv::restore().")
message("Adjustment covariates: ", paste(adj, collapse = ", "),
        if (has_region) " + region FE" else "")

## ---- Model machinery ----------------------------------------------------------
# Reported mortality endpoints (see H1_prep_mortality.R): neonatal and infant.
OUTCOMES <- c(neonatal = "change_neonatal_death",
              infant   = "change_infant_death")
# ---- ONE reference distribution for the interval and the p-value ------------
# These have to come from the same distribution, and for the coeftest arm they
# did not. lmtest::coeftest() on an lm object reports a t test on the residual
# degrees of freedom -- its columns are literally "t value" and "Pr(>|t|)" --
# while the interval here was built from a Normal quantile. At n = 607 with 589
# residual df the two quantiles are 1.959964 and 1.964000, a gap of 0.2%. That
# is invisible in a printed bound and decisive at the threshold: the
# all-household infant row has |t| = 1.963, which is p = 0.0496 under the Normal
# and p = 0.0501 under t(589). The reported p was the t one and the interval was
# the Normal one, so a row of the released table showed an interval excluding
# zero beside p = 0.0502.
#
# An earlier attempt at this replaced a hard-coded 1.96 with qnorm(0.975). That
# was a real fix for a real (3.6e-5) discrepancy, but it was not THIS one, and
# the check below is what caught the difference.
#
# Both quantities are now computed here from the residual df rather than read
# out of coeftest, so the reference distribution is visible at the call site
# instead of being a package default that a reader has to know.
#
# The multiple-imputation arm further down is a separate estimator and stays on
# the Normal for BOTH its interval and its p-value -- internally consistent, and
# in any case superseded by the CR2/Barnard-Rubin columns reported alongside it.
Z975 <- qnorm(0.975)   # MI arm only; the coeftest arm uses qt() on residual df

# Interval and p-value must agree about the null, in every row. This is the
# check that would have caught the 1.96 discrepancy above without anyone
# noticing it by hand.
.ci_p_consistent <- function(tab, lo, hi, p, label) {
  d <- tab[is.finite(tab[[lo]]) & is.finite(tab[[hi]]) & is.finite(tab[[p]]), , drop = FALSE]
  if (!nrow(d)) return(invisible(NULL))
  excl <- d[[lo]] * d[[hi]] > 0
  sig  <- d[[p]] < 0.05
  bad  <- which(excl != sig)
  chk("H2", sprintf("%s: every interval agrees with its own p-value about 0.05", label),
      length(bad) == 0,
      if (length(bad) == 0) sprintf("%d row(s) checked", nrow(d))
      else paste(sprintf("row %d: CI (%.6f, %.6f) vs p = %.4f", bad,
                         d[[lo]][bad], d[[hi]][bad], d[[p]][bad]), collapse = " | "))
  invisible(NULL)
}


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
  cl   <- if (has_state) dat[["state"]][rows] else NULL
  vc  <- if (has_state)
    sandwich::vcovCL(m, cluster = cl) else sandwich::vcovHC(m, "HC1")
  ct  <- lmtest::coeftest(m, vcov. = vc)
  i   <- match(exposure, rownames(ct))
  dfres <- df.residual(m)          # coeftest's own reference df for an lm
  tcrit <- qt(0.975, dfres)
  pval  <- 2 * pt(-abs(ct[i,1] / ct[i,2]), dfres)
  out <- tibble(term = exposure, estimate = ct[i,1], se = ct[i,2], p = pval,
                conf.low = ct[i,1] - tcrit*ct[i,2], conf.high = ct[i,1] + tcrit*ct[i,2],
                n = nobs(m), n_clusters = if (is.null(cl)) NA_integer_ else
                  dplyr::n_distinct(cl))
  # ---- Small-cluster inference (CR2 + Satterthwaite) --------------------------
  # The columns above are the conventional sandwich (vcovCL defaults: CR1, i.e.
  # CR0 with the G/(G-1) x (n-1)/(n-k) factor; the *_cr0 column names predate
  # this note) referred to a t distribution on the
  # RESIDUAL degrees of freedom -- coeftest's own reference distribution, fixed
  # a few lines up so that the interval and the p-value agree. That is the
  # conventional rule, and it is the rule the simulation below calibrates. With
  # ~30 state clusters it is the standard-but-optimistic choice: CR0 is
  # downward-biased in finite samples, and the residual df are not the degrees
  # of freedom the clustering costs -- t(589) gives 1.964 against the Normal's
  # 1.960, where the honest small-cluster critical value is above 2.1.
  # CR2 (Bell-McCaffrey) with Satterthwaite degrees of freedom is the recommended
  # correction at this cluster count, so it is computed alongside rather than
  # instead: every row carries both, the tables keep their existing inference,
  # and health_small_cluster_inference.csv reports the comparison. Nothing here
  # changes a point estimate.
  out$se_cr2 <- NA_real_; out$df_satt <- NA_real_
  out$p_cr2  <- NA_real_; out$ci_cr2_low <- NA_real_; out$ci_cr2_high <- NA_real_
  if (has_state && CR2_OK) {
    ok_cr2 <- try({
      ct2 <- clubSandwich::coef_test(m, vcov = "CR2", cluster = cl,
                                     test = "Satterthwaite", coefs = exposure)
      ci2 <- clubSandwich::conf_int(m, vcov = "CR2", cluster = cl,
                                    test = "Satterthwaite", coefs = exposure)
      out$se_cr2      <- ct2$SE[1]
      out$df_satt     <- ct2$df_Satt[1]
      out$p_cr2       <- ct2$p_Satt[1]
      out$ci_cr2_low  <- ci2$CI_L[1]
      out$ci_cr2_high <- ci2$CI_U[1]
    }, silent = TRUE)
    if (inherits(ok_cr2, "try-error"))
      CR2_FAILURES <<- CR2_FAILURES + 1L
  }
  out
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
  wv_on <- weighted && !is.na(wcol)
  tag <- if (weighted) MAIN_TAG else "unweighted"
  pmap_dfr(grid, function(outcome, exposure, adjusted) {
    r <- fit_one(df, OUTCOMES[[outcome]], exposure, adjusted,
                 wvec = if (wv_on) wv_for(df, OUTCOMES[[outcome]]) else NULL)
    if (is.null(r)) return(NULL)
    mutate(r, outcome = outcome, adjusted = adjusted, weighting = tag, .before = 1)
  })
}
results      <- run_grid(TRUE)                          # MAIN (birth-hmean weighted)
results_unwt <- if (!is.na(wcol)) run_grid(FALSE) else NULL  # SENSITIVITY (unweighted)

## ---- Wild cluster bootstrap --------------------------------------------------
# The second small-cluster check, and the one that does not rely on an asymptotic
# approximation at all. Cameron, Gelbach & Miller (2008) bootstrap-t: impose the
# null by refitting without the exposure, resample the restricted residuals with
# a Rademacher sign flipped once per STATE, refit, and compare the bootstrap t
# distribution with the observed t.
#
# Implemented directly in matrix form rather than through lm() + sandwich() per
# replication. X and (X'WX)^-1 do not change across replications -- only y does --
# so precomputing them turns ~10,000 model fits into ~10,000 matrix multiplies,
# seconds instead of many minutes. The cluster covariance below is the plain CR0
# sandwich with no small-sample factor: the factor is identical in the observed
# and bootstrap statistics and cancels in the comparison, so including it would
# change nothing except the runtime.
WCB_B <- as.integer(Sys.getenv("WCB_B", "9999"))

wild_cluster_p <- function(dat, outcome, exposure, adjusted = TRUE, wvec = NULL,
                           B = WCB_B, seed = 20260916) {
  if (!has_state || !outcome %in% names(dat)) return(NULL)
  dat <- as.data.frame(dat)
  rhs <- c(exposure, if (adjusted) adj, if (has_region) "factor(region)")
  f   <- reformulate(rhs, response = outcome)
  if (!is.null(wvec)) {
    ok  <- is.finite(wvec) & wvec > 0
    dat <- dat[ok, , drop = FALSE]; rownames(dat) <- NULL; wvec <- wvec[ok]
  }
  m    <- lm(f, data = dat, weights = wvec)
  mf   <- model.frame(m); rows <- as.numeric(rownames(mf))
  X    <- model.matrix(m); y <- as.numeric(model.response(mf))
  w    <- if (is.null(wvec)) rep(1, length(y)) else wvec[rows]
  cl   <- as.character(dat[["state"]][rows])
  j    <- match(exposure, colnames(X))
  if (is.na(j) || anyNA(X) || anyNA(y)) return(NULL)
  gs   <- unique(cl); G <- length(gs)
  idx  <- split(seq_along(cl), factor(cl, levels = gs))

  XtW    <- t(X * w)
  A      <- tryCatch(solve(XtW %*% X), error = function(e) NULL)
  if (is.null(A)) return(NULL)
  tstat <- function(yv) {
    b  <- A %*% (XtW %*% yv)
    u  <- as.numeric(yv - X %*% b)
    S  <- vapply(idx, function(g) XtW[, g, drop = FALSE] %*% u[g], numeric(ncol(X)))
    V  <- A %*% tcrossprod(S) %*% A
    as.numeric(b[j] / sqrt(V[j, j]))
  }
  t_obs <- tstat(y)

  # Restricted (null-imposed) fit: the same design without the exposure column.
  X0 <- X[, -j, drop = FALSE]
  A0 <- tryCatch(solve(t(X0 * w) %*% X0), error = function(e) NULL)
  if (is.null(A0)) return(NULL)
  b0    <- A0 %*% (t(X0 * w) %*% y)
  fit0  <- as.numeric(X0 %*% b0); res0 <- y - fit0

  set.seed(seed)
  flips <- matrix(sample(c(-1, 1), G * B, replace = TRUE), nrow = G)
  t_star <- vapply(seq_len(B), function(b_i) {
    v <- setNames(flips[, b_i], gs)
    tstat(fit0 + res0 * v[cl])
  }, numeric(1))
  t_star <- t_star[is.finite(t_star)]
  tibble(t_obs = t_obs, B = length(t_star), n_clusters = G,
         p_wild = (1 + sum(abs(t_star) >= abs(t_obs))) / (length(t_star) + 1))
}

## ---- Small-cluster inference comparison table --------------------------------
# Every fixed-surface row of the main grid, with its three inferences side by
# side: CR0 + t on residual df (the conventional rule, as reported in the
# superseded columns and as calibrated by the simulation below), CR2 + Satterthwaite, and the
# wild cluster bootstrap p-value. The point of the table is the comparison, so it
# is written even when a column is unavailable.
if (has_state) {
  wcb_grid <- expand_grid(outcome = names(OUTCOMES), exposure = exposures)
  t_wcb <- Sys.time()
  small_cl <- pmap_dfr(wcb_grid, function(outcome, exposure) {
    base <- results %>%
      filter(outcome == !!outcome, term == !!exposure, adjusted, weighting == MAIN_TAG)
    if (!nrow(base)) return(NULL)
    wb <- wild_cluster_p(df, OUTCOMES[[outcome]], exposure, adjusted = TRUE,
                         wvec = wv_for(df, OUTCOMES[[outcome]]))
    tibble(outcome = outcome, term = exposure, weighting = MAIN_TAG,
           estimate = base$estimate[1], n = base$n[1],
           n_clusters = base$n_clusters[1],
           se_cr0 = base$se[1],     p_cr0  = base$p[1],
           se_cr2 = base$se_cr2[1], p_cr2  = base$p_cr2[1],
           df_satt = base$df_satt[1],
           ci_cr2_low = base$ci_cr2_low[1], ci_cr2_high = base$ci_cr2_high[1],
           p_wild = if (is.null(wb)) NA_real_ else wb$p_wild[1],
           wild_B = if (is.null(wb)) NA_integer_ else wb$B[1])
  })
  if (nrow(small_cl)) {
    small_cl <- small_cl %>%
      mutate(across(c(estimate, se_cr0, se_cr2, ci_cr2_low, ci_cr2_high),
                    ~ .x * 10, .names = "{.col}_per10"))
    readr::write_csv(small_cl, file.path(
      dir_out, paste0("health_small_cluster_inference", sfx, ".csv")))
    message(sprintf(
      "\n== Small-cluster inference (%d state clusters, wild bootstrap B = %d, %.1f min) ==",
      max(small_cl$n_clusters, na.rm = TRUE), WCB_B,
      as.numeric(difftime(Sys.time(), t_wcb, units = "mins"))))
    print(as.data.frame(small_cl %>%
      transmute(outcome, term, est_per10 = round(estimate_per10, 4),
                p_cr0 = round(p_cr0, 4), p_cr2 = round(p_cr2, 4),
                p_wild = round(p_wild, 4), df_satt = round(df_satt, 1))),
      row.names = FALSE)
    # The paper's central contrast is that the fixed corrected surface reaches
    # p < 0.05 for infant mortality and the uncertainty-propagated version does
    # not. If CR2 or the bootstrap moves the fixed-surface row across 0.05, that
    # contrast is an artefact of the inference, not of the exposure, and the
    # manuscript has to say so. Flag it rather than leave it to be noticed.
    key <- small_cl %>% filter(outcome == "infant", term == "change_lpg_bayes")
    if (nrow(key)) {
      crossed <- c(if (isTRUE(key$p_cr0[1] < 0.05 & key$p_cr2[1]  >= 0.05)) "CR2",
                   if (isTRUE(key$p_cr0[1] < 0.05 & key$p_wild[1] >= 0.05)) "wild bootstrap")
      # The check passes when no rule moves the result ACROSS 0.05 -- which is
      # also the case when nothing was significant under any rule. The name says
      # so, so a PASS cannot be read as "significant".
      chk_warn("H2", sprintf("fixed-surface infant significance does not change across inference rules (%s under all)",
                             if (isTRUE(key$p_cr0[1] < 0.05)) "conventional p < 0.05" else "p >= 0.05 under the conventional rule"),
               length(crossed) == 0,
               sprintf("p: CR0 %.4f | CR2 %.4f | wild %.4f%s",
                       key$p_cr0[1], key$p_cr2[1], key$p_wild[1],
                       if (length(crossed)) paste0("  <-- CROSSES 0.05 under ",
                                                   paste(crossed, collapse = " and ")) else ""))
    }
  }

  ## ---- Calibrating the three inference rules against a known null ------------
  # The table above says the three rules disagree. It does not say which to
  # believe. This block answers that by simulation, on THIS design -- the real
  # X, the real weights, the real 32-state cluster structure -- with data
  # generated under a known null, and reports how often each rule rejects at a
  # nominal 5%.
  #
  # The DGP is deliberately NOT the bootstrap's own weight distribution. Drawing
  # the simulated errors with Rademacher state flips would test the wild cluster
  # bootstrap against data generated exactly the way it resamples, which flatters
  # it. Instead the errors are Gaussian with a state random effect,
  #   e_id = a_s + u_id,  a_s ~ N(0, tau^2),  u_id ~ N(0, sigma^2),
  # with tau^2 and sigma^2 recovered from the restricted (null-imposed) residuals
  # by a one-way decomposition on state. That reproduces the real intra-state
  # correlation while leaving the bootstrap no home advantage.
  #
  # Cost is the nested bootstrap: CLUSTER_SIM_R replications each running
  # CLUSTER_SIM_B bootstrap draws. Defaults are sized for a few minutes; set
  # CLUSTER_SIM=FALSE to skip, or raise CLUSTER_SIM_R for a tighter Monte Carlo
  # standard error (reported in the output, so the precision is never implicit).
  CLUSTER_SIM   <- toupper(Sys.getenv("CLUSTER_SIM", "TRUE")) == "TRUE"
  CLUSTER_SIM_R <- as.integer(Sys.getenv("CLUSTER_SIM_R", "1000"))
  CLUSTER_SIM_B <- as.integer(Sys.getenv("CLUSTER_SIM_B", "399"))
  # Only on the main pass: the calibration is a property of the design, and
  # rerunning it for each population/variant would trade minutes for nothing.
  if (CLUSTER_SIM && !nzchar(sfx) && CR2_OK) {
    t_sim <- Sys.time()
    sim_out <- try({
      exposure <- "change_lpg_bayes"
      outcome  <- OUTCOMES[["infant"]]
      dat <- as.data.frame(df)
      wv  <- wv_for(dat, outcome)
      rhs <- c(exposure, adj, if (has_region) "factor(region)")
      f   <- reformulate(rhs, response = outcome)
      if (!is.null(wv)) {
        ok  <- is.finite(wv) & wv > 0
        dat <- dat[ok, , drop = FALSE]; rownames(dat) <- NULL; wv <- wv[ok]
      }
      m0   <- lm(f, data = dat, weights = wv)
      mf   <- model.frame(m0); rows <- as.numeric(rownames(mf))
      X    <- model.matrix(m0); y <- as.numeric(model.response(mf))
      w    <- if (is.null(wv)) rep(1, length(y)) else wv[rows]
      cl   <- as.character(dat[["state"]][rows])
      j    <- match(exposure, colnames(X))
      gs   <- unique(cl); G <- length(gs)
      idx  <- split(seq_along(cl), factor(cl, levels = gs))
      XtW  <- t(X * w); A <- solve(XtW %*% X)
      # The conventional rule being calibrated must be the conventional rule
      # being REPORTED. fit_one() refers the CR0 statistic to t on the residual
      # df (coeftest's own reference distribution); this simulation previously
      # used pnorm(), i.e. a z rule, so it calibrated a slightly different
      # procedure from the one in the tables. At this n the two differ by ~0.2%
      # in the critical value -- which is exactly the margin that moved the
      # borderline infant row -- so the distinction is not academic here.
      dfres_sim <- df.residual(m0)

      # Conventional t on a supplied response, reusing the precomputed design.
      # fit_one() takes its conventional SE from sandwich::vcovCL(), whose
      # defaults for an lm (type "HC1", cadjust = TRUE) are the CR1 sandwich:
      # the plain CR0 meat times G/(G-1) x (n-1)/(n-k). Apply the same factor
      # here, so the rule whose rejection rate is simulated is exactly the one
      # the tables report.
      cr1_fac <- (G / (G - 1)) * ((length(y) - 1) / (length(y) - ncol(X)))
      t_cr0 <- function(yv) {
        b <- A %*% (XtW %*% yv)
        u <- as.numeric(yv - X %*% b)
        S <- vapply(idx, function(g) XtW[, g, drop = FALSE] %*% u[g], numeric(ncol(X)))
        V <- cr1_fac * (A %*% tcrossprod(S) %*% A)
        as.numeric(b[j] / sqrt(V[j, j]))
      }
      # The simulated statistic must reproduce the reported one on the observed data.
      t_obs_rep <- unname(coef(m0)[exposure] /
                          sqrt(sandwich::vcovCL(m0, cluster = cl)[exposure, exposure]))
      if (abs(t_cr0(y) - t_obs_rep) > 1e-6 * max(1, abs(t_obs_rep)))
        stop("null simulation: conventional t (", signif(t_cr0(y), 8), ") does not reproduce ",
             "the reported vcovCL t (", signif(t_obs_rep, 8), ")")
      # Null-imposed fit, and the variance components of its residuals.
      X0   <- X[, -j, drop = FALSE]
      A0   <- solve(t(X0 * w) %*% X0)
      fit0 <- as.numeric(X0 %*% (A0 %*% (t(X0 * w) %*% y)))
      r0   <- y - fit0
      gm   <- tapply(r0, cl, mean)
      tau2 <- max(0, var(as.numeric(gm)))
      sig2 <- max(1e-12, mean(tapply(r0, cl, function(v) if (length(v) > 1) var(v) else 0)))

      set.seed(20260916)
      rej <- matrix(FALSE, nrow = CLUSTER_SIM_R, ncol = 3,
                    dimnames = list(NULL, c("cr0", "cr2", "wild")))
      dsim <- dat[rows, , drop = FALSE]
      for (r_i in seq_len(CLUSTER_SIM_R)) {
        e <- rnorm(G, 0, sqrt(tau2))[match(cl, gs)] + rnorm(length(y), 0, sqrt(sig2))
        ysim <- fit0 + e                       # exposure coefficient is exactly 0
        # (1) conventional CR1 + t(residual df), the rule fit_one() reports
        tz <- t_cr0(ysim)
        rej[r_i, "cr0"] <- 2 * pt(-abs(tz), dfres_sim) < 0.05
        # (2) CR2 + Satterthwaite, through clubSandwich on a real refit
        # Response AND weights live in the data frame. clubSandwich re-extracts
        # both from the fitted object, and a `weights =` pointing at a variable
        # in the calling frame is the classic way that re-extraction fails.
        dsim$ysim_ <- ysim; dsim$wsim_ <- w
        msim <- lm(reformulate(rhs, response = "ysim_"), data = dsim,
                   weights = wsim_)
        ct2  <- try(clubSandwich::coef_test(msim, vcov = "CR2", cluster = cl,
                                            test = "Satterthwaite", coefs = exposure),
                    silent = TRUE)
        rej[r_i, "cr2"] <- !inherits(ct2, "try-error") && ct2$p_Satt[1] < 0.05
        # (3) wild cluster bootstrap, null imposed, on the simulated data
        b0s   <- A0 %*% (t(X0 * w) %*% ysim)
        f0s   <- as.numeric(X0 %*% b0s); e0s <- ysim - f0s
        flips <- matrix(sample(c(-1, 1), G * CLUSTER_SIM_B, replace = TRUE), nrow = G)
        tstar <- vapply(seq_len(CLUSTER_SIM_B), function(b_i) {
          v <- setNames(flips[, b_i], gs); t_cr0(f0s + e0s * v[cl])
        }, numeric(1))
        tstar <- tstar[is.finite(tstar)]
        pw <- (1 + sum(abs(tstar) >= abs(tz))) / (length(tstar) + 1)
        rej[r_i, "wild"] <- pw < 0.05
      }
      rate <- colMeans(rej)
      calib <- tibble(
        rule = c("Conventional CR1 + t(residual df)",
                 "CR2 / Satterthwaite",
                 "Wild cluster bootstrap (null imposed)"),
        rejection_rate_at_nominal_05 = as.numeric(rate),
        mcse = sqrt(as.numeric(rate) * (1 - as.numeric(rate)) / CLUSTER_SIM_R),
        replications = CLUSTER_SIM_R, bootstrap_B = c(NA_integer_, NA_integer_,
                                                      CLUSTER_SIM_B),
        n_clusters = G, n = length(y), nominal = 0.05,
        dgp = "state random effect + independent error, Gaussian; exposure coefficient exactly 0",
        tau2 = tau2, sigma2 = sig2,
        icc = tau2 / (tau2 + sig2),
        specification = paste0(outcome, " ~ ", exposure, " + covariates + region FE"))
      readr::write_csv(calib, file.path(dir_out,
                                        "health_cluster_inference_calibration.csv"))
      message(sprintf(
        "\n== Inference calibration under a known null (%d reps, %d clusters, %.1f min) ==",
        CLUSTER_SIM_R, G, as.numeric(difftime(Sys.time(), t_sim, units = "mins"))))
      print(as.data.frame(calib %>% transmute(
        rule, rejects_at_nominal_05 = sprintf("%.1f%% (+/- %.1f)",
                                              100 * rejection_rate_at_nominal_05,
                                              100 * mcse))), row.names = FALSE)
      calib
    }, silent = TRUE)
    if (inherits(sim_out, "try-error")) {
      chk_warn("H2", "inference-rule calibration simulation completed", FALSE,
               paste("simulation failed:", conditionMessage(attr(sim_out, "condition"))))
    } else {
      # The simulation exists to justify demoting the conventional clustered z.
      # If it did NOT find that rule anticonservative, the manuscript's reason
      # for reporting CR2 as primary is gone and has to be revisited.
      cr0r <- sim_out$rejection_rate_at_nominal_05[1]
      chk("H2", "conventional clustered (CR1, t on residual df) rule over-rejects at this cluster count",
          is.finite(cr0r) && cr0r > 0.05,
          sprintf("CR0 %.1f%%, CR2 %.1f%%, wild %.1f%% at nominal 5%% (%d reps, MCSE %.1f pp)",
                  100 * cr0r, 100 * sim_out$rejection_rate_at_nominal_05[2],
                  100 * sim_out$rejection_rate_at_nominal_05[3],
                  CLUSTER_SIM_R, 100 * sim_out$mcse[1]))
    }
  }
}

## ---- Uncertainty propagation (multiple imputation) ---------------------------
# The reported MI is EXPOSURE-ONLY: each imputation is one evenly thinned draw
# of the saved calibration posterior, which does not involve the outcome. It
# carries calibration uncertainty but is known to attenuate the pooled slope
# (29_mime_validation.R), so it is a labelled sensitivity analysis, not the
# primary estimate. The outcome-conditioned imputation (mime_draws in
# scripts/mime.R, which used the analysis model with the calibration posterior
# as the exposure prior) failed its convergence checks and was abandoned for
# this paper (docs/mime_v2_spec.md); its code is retained but not used for
# inference. Each completed data set is analysed exactly as the fixed surface
# (weighted lm, CR2 / Satterthwaite) and pooled by Rubin's rules with
# Barnard-Rubin df.
source("mime.R")
M <- 200
MIME_BURN <- 1000L; MIME_THIN <- 10L
path_comp <- file.path(dir_out, "correction_posterior_components.rds")
if (!file.exists(path_comp))
  stop("correction_posterior_components.rds not found -- re-run 05_correction.R ",
       "(it reuses the saved brms fits) before H2.")
comp  <- readRDS(path_comp)
PR15  <- mime_prior(comp$surfaces$lpg_2015, comp$rho)
PR19  <- mime_prior(comp$surfaces$lpg_2019, comp$rho)
PR15ic <- if (DO_IRESONLY && !is.null(comp$surfaces$lpg_2015_irescal))
            mime_prior(comp$surfaces$lpg_2015_irescal, comp$rho) else NULL
PR15aw <- if (DO_ACCESSWT && !is.null(comp$surfaces$lpg_2015_accesswt))
            mime_prior(comp$surfaces$lpg_2015_accesswt, comp$rho) else NULL
has_ic_draws <- !is.null(PR15ic); has_aw_draws <- !is.null(PR15aw)
use_real_draws <- TRUE
MI_RHS <- c(adj, if (has_region) "factor(region)")
# Seed per (exposure variant, outcome, stratum): fixed, so runs are bit-reproducible
# and 22 / 23 reproduce this script's imputations exactly.
mime_seed <- function(term, outc, stratum)
  20261021L + sum(utf8ToInt(paste(term, outc, stratum, sep = "|"))) %% 100000L
MIME_DIAG <- list()

# Rubin's-rules pooling of one exposure variant into one row per outcome.
# `keep` restricts the analysis to a subset of districts (calibration-support
# sensitivity); the imputation model is then fitted on the same subset.
# exposure_only = TRUE reproduces the earlier, outcome-independent imputation
# (each imputation one evenly thinned draw of the saved calibration posterior),
# kept as a labelled comparison: in simulation it attenuates the association
# (29_mime_validation.R), but reporting it shows how much the choice matters.
pool_mi <- function(pr15, term_name, keep = NULL, stratum = "all", shared_index = FALSE,
                    exposure_only = FALSE, pr19 = PR19) {
  map_dfr(names(OUTCOMES), function(outc) {
    ocol <- OUTCOMES[[outc]]
    if (!ocol %in% names(df)) return(NULL)
    d0 <- df
    if (!is.null(keep)) { d0 <- d0[which(keep), , drop = FALSE]; rownames(d0) <- NULL }
    w0 <- wv_for(d0, ocol)
    if (exposure_only) {
      im <- list(D = mime_exposure_only(d0, pr15, pr19, M = M, shared_index = shared_index,
                                        seed = mime_seed(term_name, outc, stratum)))
    } else {
    im <- mime_draws(d0, ocol, MI_RHS, w0, pr15, pr19, M = M, burn = MIME_BURN, thin = MIME_THIN,
                     shared_index = shared_index, seed = mime_seed(term_name, outc, stratum))
    }
    draws <- map_dfr(seq_len(M), function(m) {
      d_imp <- d0; d_imp$change_lpg_mi <- im$D[, m]
      fit_one(d_imp, ocol, "change_lpg_mi", adjusted = TRUE,
              wvec = wv_for(d_imp, ocol))
    })
    if (nrow(draws) == 0) return(NULL)
    if (!exposure_only) {
      # The completed-data health coefficient, compared across the chains too.
      ce <- mime_conv_est(draws$estimate, im$diag$chains)
      MIME_DIAG[[length(MIME_DIAG) + 1]] <<- data.frame(term = term_name, outcome = outc,
        support_stratum = stratum, population = POP, M = M, burn = MIME_BURN, thin = MIME_THIN,
        as.data.frame(im$diag), rhat_cr2_estimate = ce[["rhat"]], ess_cr2_estimate = ce[["ess"]])
    }
    pl <- mime_pool(draws$estimate, draws$se, draws$se_cr2, draws$df_satt)
    ok2 <- is.finite(pl[["se_cr2"]])
    tibble(outcome = outc, adjusted = TRUE, weighting = MAIN_TAG, term = term_name,
           estimate = pl[["estimate"]], se = pl[["se"]],
           conf.low = pl[["estimate"]] - Z975 * pl[["se"]], conf.high = pl[["estimate"]] + Z975 * pl[["se"]],
           p = pl[["p"]], n = max(draws$n), n_clusters = max(draws$n_clusters),
           se_cr2 = pl[["se_cr2"]], df_satt = pl[["df_br"]], p_cr2 = pl[["p_cr2"]],
           ci_cr2_low  = if (ok2) pl[["estimate"]] - pl[["crit_cr2"]] * pl[["se_cr2"]] else NA_real_,
           ci_cr2_high = if (ok2) pl[["estimate"]] + pl[["crit_cr2"]] * pl[["se_cr2"]] else NA_real_,
           support_stratum = stratum,
           mi_method = if (exposure_only) "exposure_only" else "outcome_conditioned")
  })
}
# The analysis frame (district aggregates only) and model terms, for the
# prespecified validation simulation (29_mime_validation.R).
saveRDS(list(df = df, rhs = MI_RHS, wcol = wcol, wcols = WCOLS, outcomes = OUTCOMES, pop = POP),
        file.path(dir_out, paste0("health_analysis_frame", sfx, ".rds")))
# ---- WHICH multiple imputation (Oct 2026) -----------------------------------
# The outcome-conditioned imputation (mime_draws) failed its convergence checks
# in two versions and was abandoned for this paper (docs/mime_v2_spec.md,
# Section 9); its outputs are archived as "failed convergence -- not for
# inference". The MI rows below are the EXPOSURE-ONLY imputation: each
# imputation is a draw of the calibration posterior (shared calibration draw
# plus the district's NFHS-side error, correlated across rounds), without
# conditioning on the outcome. It propagates calibration uncertainty but is
# known to be at risk of attenuation toward the null, so it is reported as a
# SENSITIVITY analysis, never as the primary or a validated estimate.
MI_EXPOSURE_ONLY <- TRUE
mi_results    <- pool_mi(PR15,   "change_lpg_bayes_MI", exposure_only = MI_EXPOSURE_ONLY)
mi_results_ic <- if (DO_IRESONLY && has_ic_draws) pool_mi(PR15ic, "change_lpg_bayes_iresonly_MI", shared_index = TRUE, exposure_only = MI_EXPOSURE_ONLY) else NULL
mi_results_aw <- if (DO_ACCESSWT && has_aw_draws) pool_mi(PR15aw, "change_lpg_bayes_accesswt_MI", exposure_only = MI_EXPOSURE_ONLY) else NULL
# ---- Four-way comparison: shrinkage (separate / joint rounds) x propagation
# (exposure-only / outcome-conditional), plus each shrinkage's point surface, so
# the effect of each change can be seen on its own. Written to
# health_mi_comparison.csv; the published table keeps joint-rounds rows (fixed surface and exposure-only MI).
HAVE_SEP <- !is.null(comp$surfaces$lpg_2015$mu_sep) && !is.null(comp$surfaces$lpg_2019$mu_sep)
chk_warn("H2", "separate-shrinkage moments available for the four-way comparison", HAVE_SEP,
         if (HAVE_SEP) "mu_sep / sd_sep present" else "components written by an older 05 -- separate-shrinkage rows skipped")
PR15s <- if (HAVE_SEP) mime_prior(comp$surfaces$lpg_2015, shrinkage = "separate") else NULL
PR19s <- if (HAVE_SEP) mime_prior(comp$surfaces$lpg_2019, shrinkage = "separate") else NULL
fixed_row <- function(pr15, pr19, label) map_dfr(names(OUTCOMES), function(outc) {
  d0 <- df
  d0$change_lpg_fx <- 100 * (plogis(pr19$Lbar[match(d0$district, pr19$ids)]) -
                             plogis(pr15$Lbar[match(d0$district, pr15$ids)]))
  r <- fit_one(d0, OUTCOMES[[outc]], "change_lpg_fx", adjusted = TRUE,
               wvec = wv_for(d0, OUTCOMES[[outc]]))
  tibble(outcome = outc, term = label, estimate = r$estimate, se = r$se, p = r$p,
         se_cr2 = r$se_cr2, df_satt = r$df_satt, p_cr2 = r$p_cr2,
         ci_cr2_low = r$ci_cr2_low, ci_cr2_high = r$ci_cr2_high, n = r$n, support_stratum = "all")
})
mi_cmp <- dplyr::bind_rows(
  if (HAVE_SEP) fixed_row(PR15s, PR19s, "fixed_separate") %>% dplyr::mutate(shrinkage = "separate rounds", propagation = "none (point surface)"),
  fixed_row(PR15,  PR19,  "fixed_joint")    %>% dplyr::mutate(shrinkage = "joint rounds",    propagation = "none (point surface)"),
  if (HAVE_SEP) pool_mi(PR15s, "eo_separate", exposure_only = TRUE, pr19 = PR19s) %>% dplyr::mutate(shrinkage = "separate rounds", propagation = "exposure-only MI"),
  pool_mi(PR15,  "eo_joint",    exposure_only = TRUE)                %>% dplyr::mutate(shrinkage = "joint rounds",    propagation = "exposure-only MI"),
  NULL) %>%
  dplyr::mutate(est_per10 = 10 * estimate, lo_cr2_per10 = 10 * ci_cr2_low, hi_cr2_per10 = 10 * ci_cr2_high) %>%
  dplyr::select(outcome, shrinkage, propagation, est_per10, lo_cr2_per10, hi_cr2_per10, p_cr2, df_satt,
                estimate, se, se_cr2, p, n)
readr::write_csv(mi_cmp, file.path(dir_out, paste0("health_mi_comparison", sfx, ".csv")))
cat("\n-- Shrinkage x propagation (per 10 pp; CR2) --\n")
print(as.data.frame(mi_cmp %>% dplyr::select(outcome, shrinkage, propagation, est_per10, lo_cr2_per10, hi_cr2_per10, p_cr2) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)

## ---- Calibration-support sensitivity (review item 5) -------------------------
# Does the exposure-only MI association survive when districts whose corrected exposure was
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
    pool_mi(PR15, "change_lpg_bayes_MI", keep = k, stratum = nm, exposure_only = MI_EXPOSURE_ONLY)
  })
} else {
  message("Support-stratified MI sensitivity skipped (flags unavailable).")
}

# EVIDENCE HIERARCHY, made machine-readable (see the header). rank 1 is the
# fixed corrected surface, reported as a conditional, exploratory association;
# rank 2 the exposure-only MI sensitivity; rank 4 exists only to display
# attenuation. The `evidence_role` string travels with the rank.
rank_of <- function(term) dplyr::case_when(
  grepl("_bayes(_iresonly|_accesswt)?$", term) ~ 1L,  # fixed corrected surface (conditional, exploratory)
  grepl("_MI$", term)                  ~ 2L,   # exposure-only MI (sensitivity)
  term == "change_lpg_rc"              ~ 3L,   # regression-calibrated
  term == "change_lpg_raw"             ~ 4L,   # raw NFHS (context only)
  TRUE                                 ~ NA_integer_)
role_of <- function(rank) dplyr::case_when(
  rank == 1L ~ "CONDITIONAL / EXPLORATORY (corrected surface held fixed; interval excludes calibration uncertainty)",
  rank == 2L ~ "SENSITIVITY (exposure-only MI: calibration uncertainty propagated without conditioning on the outcome; known attenuation concern)",
  rank == 3L ~ "SUPPORT (regression-calibrated)",
  rank == 4L ~ "CONTEXT (raw NFHS; attenuated by measurement error)",
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
         est_per10 = estimate*10, lo_per10 = conf.low*10, hi_per10 = conf.high*10)
# The CI/p consistency check runs on THIS object, before any rounding. The
# p-values are exported unrounded (see below), but the model quantities are
# rounded to 4 dp, and a check on rounded values (p = 0.04996 -> 0.0500) --
# which would manufacture a disagreement with an interval that legitimately
# excludes zero, in exactly the boundary region the check exists to police.
results_unrounded <- results_all
results_all <- results_all %>%
  # ROUND ONCE. The per-10 columns are what the manuscript builder prints, and
  # it rounds them again, to 3 dp. Rounding them here as well rounded twice: a
  # bound of 0.0734686 became 0.0735 in this file and then 0.074 on the page,
  # while SI Table S7 -- which prints the same bound from the full-precision
  # design_frontier.csv -- rounded it once and printed 0.073. One number, two
  # values, in the same manuscript. The model quantities stay at 4 dp because
  # nothing downstream re-rounds them; the display columns keep full precision
  # so that every printed value is rounded exactly once, from the same number.
  # conf.low/conf.high and the CR2 bounds join the display columns in being
  # left UNROUNDED. Rounding them to 4 dp on the raw coefficient scale turns a
  # borderline bound such as -1.9e-5 into exactly 0, so the released CSV shows
  # an interval that appears to touch the null beside a p-value of 0.0495.
  # p-values join them (Oct 2026): stored at 4 dp, a p of 0.13448 became
  # 0.1345 and then printed as 0.135 at 3 dp -- rounded twice, from different
  # numbers. Leaving p unrounded means it is rounded once, by the builder.
  mutate(across(!dplyr::any_of(c("est_per10", "lo_per10", "hi_per10",
                                 "conf.low", "conf.high",
                                 "ci_cr2_low", "ci_cr2_high",
                                 "p", "p_cr0", "p_cr2", "p_wild")) &
                where(is.numeric), ~ round(.x, 4))) %>%
  # Ranks are assigned AFTER the numeric rounding so `across(where(is.numeric))`
  # cannot coerce the integer rank to a rounded double.
  mutate(evidence_rank = rank_of(term),
         evidence_role = role_of(evidence_rank),
         mi_method = if ("mi_method" %in% names(.)) mi_method else NA_character_) %>%
  # Sort so the reported (fixed-surface) rows are first in the file and the printout.
  arrange(evidence_rank, outcome, support_stratum, weighting, desc(adjusted)) %>%
  relocate(evidence_rank, evidence_role, support_stratum, .after = term)
write_csv(results_all, file.path(dir_out, paste0("health_effects_table", sfx, ".csv")))

## ---- Leg-A symmetry sensitivity: how far do the estimates move? -------------
# The question a reviewer asks is not "did the slope change" but "did the
# CONCLUSION change". Report, per outcome, the fixed-surface and exposure-only MI
# estimate beside the sensitivity's, the absolute and relative movement, and
# whether the movement is small compared with the original estimate's own SE --
# the only comparison that decides whether the asymmetry mattered.
legA_move <- NULL
if (DO_ACCESSWT) {
  # Adjusted rows only, and from the UNROUNDED results: without the `adjusted`
  # filter each side returned both the adjusted and the crude row, and the join
  # on outcome paired crude with adjusted estimates.
  .pull <- function(tm) results_unrounded %>%
    filter(term == tm, weighting == MAIN_TAG, support_stratum == "all",
           as.character(adjusted) == "TRUE") %>%
    select(outcome, estimate, se, conf.low, conf.high, p, any_of("p_cr2")) %>%
    # The manuscript's inference is small-sample CR2 (Satterthwaite); judge
    # significance verdicts on that p-value, falling back to the model p only
    # where CR2 is unavailable.
    mutate(p_inf = if ("p_cr2" %in% names(.)) dplyr::coalesce(p_cr2, p) else p)
  legA_move <- purrr::map_dfr(
    list(c("change_lpg_bayes_MI", "change_lpg_bayes_accesswt_MI", "exposure-only MI (sensitivity)"),
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
               sig_flip    = (p_inf_primary < 0.05) != (p_inf_sens < 0.05)) %>%
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
  # A sign flip of an estimate within half an SE of zero is noise, not a
  # construction problem; only a flip that also moves the estimate by more than
  # 0.5 primary SE fails.
  chk("H2", "leg-A sensitivity does not flip the sign of any primary estimate by more than 0.5 SE",
      !any(legA_move$sign_flip & abs(legA_move$diff_in_primary_se) > 0.5, na.rm = TRUE),
      { u <- legA_move$outcome[which(legA_move$sign_flip)]
        if (length(u)) paste("SIGN FLIPPED:", paste(u, collapse = ", "))
        else "no sign flips" })
  chk_warn("H2", "leg-A sensitivity moves the exposure-only MI estimate by < 0.5 of its own SE",
      { m <- legA_move %>% filter(comparison == "exposure-only MI (sensitivity)")
        nrow(m) > 0 && all(abs(m$diff_in_primary_se) < 0.5, na.rm = TRUE) },
      { m <- legA_move %>% filter(comparison == "exposure-only MI (sensitivity)")
        paste(sprintf("%s: %+.3f (%.2f SE)", m$outcome, m$diff,
                      m$diff_in_primary_se), collapse = "; ") })
  chk_warn("H2", "leg-A sensitivity does not change any 0.05 significance verdict (CR2 p)",
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
cat("    Rows are ordered by evidence_rank. Rank 1 holds the corrected surface\n",
    "    fixed: the reported association, conditional on the calibration and\n",
    "    exploratory (its interval excludes calibration uncertainty). Rank 2 is the\n",
    "    exposure-only MI, a SENSITIVITY analysis with a known attenuation concern.\n",
    "    Neither is a validated estimate with calibration uncertainty propagated.\n",
    sep = "")
cat("\n-- rank 1: CONDITIONAL / EXPLORATORY (corrected surface held fixed; interval excludes calibration uncertainty) --\n")
print(as.data.frame(results_all %>% filter(adjusted, evidence_rank == 1L) %>%
        select(outcome, term, support_stratum, weighting,
               est_per10, lo_per10, hi_per10, p, n)), digits = 3)
cat("\n-- rank 2: SENSITIVITY (exposure-only MI; attenuation concern); ranks 3-4: support / context --\n")
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
    change_lpg_bayes="Bayesian-corrected\n(fixed surface)",
    change_lpg_bayes_MI="Bayesian-corrected\n(exposure-only MI, sensitivity)",
    change_lpg_bayes_iresonly="Bayesian-corrected\n(IRES cal, both rounds)",
    change_lpg_bayes_iresonly_MI="Bayesian-corrected, IRES both\n(exposure-only MI)",
    change_lpg_bayes_accesswt="Bayesian-corrected\n(design-weighted ACCESS ref)",
    change_lpg_bayes_accesswt_MI="Bayesian-corrected, design-wtd ACCESS\n(exposure-only MI)"),
    exposure = factor(exposure, levels = c("Raw NFHS","Regression-calibrated",
      "Bayesian-corrected\n(fixed surface)","Bayesian-corrected\n(exposure-only MI, sensitivity)",
      "Bayesian-corrected\n(IRES cal, both rounds)",
      "Bayesian-corrected, IRES both\n(exposure-only MI)",
      "Bayesian-corrected\n(design-weighted ACCESS ref)",
      "Bayesian-corrected, design-wtd ACCESS\n(exposure-only MI)")),
    outcome = factor(outcome, levels = c("neonatal","infant","child")))
p <- ggplot(plot_df, aes(est_per10, exposure)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  # CR2/Satterthwaite intervals, the rule the manuscript reports.
  geom_pointrange(aes(xmin = 10 * ci_cr2_low, xmax = 10 * ci_cr2_high)) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Change in mortality (per 100 births) per 10-pp rise in LPG main-fuel prevalence (95% CR2 interval)",
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
    if (!DO_ACCESSWT) "n/a" else if (isTRUE(has_aw_draws)) "y2015_accesswt components used"
      else "FELL BACK to Normal-from-CrI -- re-run 05")
chk("H2", "MI (uncertainty-propagated) rows present",
    any(grepl("_MI$", results_all$term)))
# The evidence hierarchy must not silently invert: the fixed corrected surface is
# the reported (conditional, exploratory) row, the exposure-only MI a sensitivity,
# and every row must carry a rank.
chk("H2", "evidence_rank present: fixed corrected surface rank 1, exposure-only MI rank 2",
    chk_has_cols(results_all, c("evidence_rank","evidence_role","support_stratum")) &&
    all(results_all$evidence_rank[grepl("_MI$", results_all$term)] == 2L) &&
    all(results_all$evidence_rank[grepl("_bayes(_iresonly|_accesswt)?$", results_all$term)] == 1L),
    paste0("ranks present: ",
           paste(sort(unique(results_all$evidence_rank)), collapse = ",")))
# Only the approved imputation may reach the results table (Oct 2026): every MI
# row must be the exposure-only imputation; an outcome-conditioned row here would
# mean the abandoned method had been switched back on.
chk("H2", "every MI row is the exposure-only imputation (outcome-conditioned MI abandoned)",
    all(results_all$mi_method[grepl("_MI$", results_all$term)] %in% "exposure_only"),
    paste(unique(stats::na.omit(results_all$mi_method)), collapse = ","))
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
# Every inferential claim in the manuscript uses CR2 (Satterthwaite) intervals.
# A failed clubSandwich call is caught and leaves p_cr2 NA, which would silently
# drop that row from the CR2 columns; make it visible.
chk("H2", "CR2 available and no clubSandwich failures across the health fits",
    CR2_OK && CR2_FAILURES == 0L,
    sprintf("clubSandwich available = %s; failures = %d", CR2_OK, CR2_FAILURES))
# Methods 2.4.5 states the MI uses the actual saved posterior draws, "not a normal
# approximation from the credible-interval endpoints". The fallback branch is
# silent by design, so assert the claim rather than trusting it.
# The outcome-conditional imputation (scripts/mime.R): write its diagnostics and
# check that the sampler moved. acc_idx is the acceptance rate of moves between
# saved calibration draws; distinct_idx the number of different calibration
# draws among the M imputations (too few = the shared calibration uncertainty is
# under-represented); and, across the independently initialised chains, rank-
# normalised split R-hat and bulk ESS for the imputation model's slope, the
# calibration slopes in use, the mean and SD of the imputed change, and the
# completed-data CR2 coefficient. A missing R-hat or ESS is a FAILED diagnostic
# (constant quantities are reported as R-hat 1, not NA).
.md <- dplyr::bind_rows(MIME_DIAG)
if (nrow(.md) == 0) {
  message("Outcome-conditioned imputation not run (MI_EXPOSURE_ONLY): no sampler diagnostics.")
} else {
readr::write_csv(.md, file.path(dir_out, paste0("mime_diagnostics", sfx, ".csv")))
print(as.data.frame(.md %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)
chk("H2", "outcome-conditional imputation ran for every MI row",
    nrow(.md) > 0 && all(is.finite(.md$beta_mean)),
    sprintf("%d imputation chains", nrow(.md)))
chk_warn("H2", "imputation chains agree (R-hat <= 1.05 and bulk ESS >= 100 on every monitored quantity; missing = fail)",
    nrow(.md) > 0 && all(mime_diag_ok(.md)),
    sprintf("%d chains per run; max R-hat %s; CR2-coefficient R-hat %s; min bulk ESS %s; CR2-coefficient ESS %s",
            max(.md$chains), chk_rng(.md$rhat_max), chk_rng(.md$rhat_cr2_estimate), chk_rng(.md$ess_min),
            chk_rng(.md$ess_cr2_estimate)))
chk_warn("H2", "imputation sampler moves (district acceptance > 0.2; >= 20 distinct calibration draws per round, none used by > 10% of imputations)",
    nrow(.md) > 0 && all(.md$acc_z > 0.2) &&
      all(.md$max_share_idx15 <= 0.10 | .md$K15 < 20) && all(.md$max_share_idx19 <= 0.10 | .md$K19 < 20) &&
      all(.md$distinct_idx15 >= 20 | .md$K15 < 20) && all(.md$distinct_idx19 >= 20 | .md$K19 < 20),
    sprintf("acc_z %s; acc_idx %s; distinct idx 2015 %s, 2019 %s; max share %s / %s",
            chk_rng(.md$acc_z), chk_rng(.md$acc_idx), chk_rng(.md$distinct_idx15),
            chk_rng(.md$distinct_idx19), chk_rng(.md$max_share_idx15), chk_rng(.md$max_share_idx19)))
}
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
chk_warn("H2", "exposure-only MI estimate stable across calibration-support strata",
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
    { g <- results_all %>% dplyr::filter(adjusted, weighting == MAIN_TAG, outcome == "infant")
      sprintf("infant, per 10 pp: raw %.4f, corrected (Bayes point) %.4f, corrected (MI) %.4f",
              10 * g$estimate[g$term == "change_lpg_raw"][1], 10 * g$estimate[g$term == "change_lpg_bayes"][1],
              10 * g$estimate[g$term == "change_lpg_bayes_MI" & g$support_stratum == "all"][1]) })
chk_file("H2", "effects table written",
    paste0("health_effects_table", sfx, ".csv"))
# On `results_unrounded`: the per-10 bounds AND the p-value as computed, before
# the 4-dp export rounding of the model quantities. Rounding either side of this comparison can invent
# or hide a disagreement at the 0.05 boundary, which is the only place the
# check has anything to say.
.ci_p_consistent(as.data.frame(results_unrounded), "lo_per10", "hi_per10", "p",
                 "conventional interval")

message("H2 done. See health_effects_table.csv and health_effects_coefplot.jpeg in ", dir_out)
message("  The manuscript reports the evidence_rank == 1 rows (change_lpg_bayes, corrected ",
        "surface held fixed) as a conditional, exploratory association whose interval excludes ",
        "calibration uncertainty; rank 2 (exposure-only MI) is a sensitivity analysis with an ",
        "attenuation concern. No row is a validated propagated estimate.")
