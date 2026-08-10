# ==============================================================================
# 23_ppd_sensitivity.R   (STANDALONE -- does not source 00_config.R)
#
# WHICH ESTIMAND IS THE CORRECTED EXPOSURE, AND DOES IT MATTER?
#
# 05_correction.R builds the corrected district surface from
#     pp    <- posterior_epred(fit, newdata = nd, ...)   # LOGIT scale
#     draws <- plogis(pp)                                # saved, used for MI
#     point <- plogis(colMeans(pp))                      # lpg_*_bayes
# posterior_epred() returns the posterior of the MEAN of the linear predictor.
# It therefore carries the uncertainty in the calibration parameters (a, b, u_s)
# but EXCLUDES the model's residual sigma. Two questions follow, and this script
# answers both without refitting anything.
#
# QUESTION 1 -- THE ESTIMAND (posterior_epred vs posterior_predict).
#   The paper's estimand is the CALIBRATED EXPECTATION of a district's LPG
#   prevalence, E[Y_d | x_d, calibration], not a draw of the district's true
#   prevalence. That is the right choice for a downstream linear regression, and
#   the reason is the structure of the two error components. Write the true
#   district value as
#       X_true = X_used + w,   w = (parameter error) + e,   e ~ N(0, sigma^2).
#   The parameter part is SYSTEMATIC -- the same estimated slope is applied to
#   every district, so an error in b rescales the health coefficient directly.
#   It must be propagated, and the multiple imputation over the calibration
#   posterior does exactly that; it is the dominant term and the reason the
#   primary estimate's interval covers zero.
#   The residual e, by contrast, is district-idiosyncratic and, conditional on
#   the data, independent of the regressor actually used. That is textbook
#   BERKSON error, which in a linear outcome model neither biases the slope nor
#   deflates a robust standard error: it passes into the composite outcome
#   residual (beta*e + epsilon), which remains uncorrelated with the regressor.
#   Switching to posterior_predict() would convert Berkson error into CLASSICAL
#   measurement error: it attenuates beta toward zero, and because it also
#   inflates Var(x) it SHARPENS rather than widens the standard error, so the
#   two shrink roughly together and the test statistic moves far less than
#   either. And because the imputation model does not condition on mortality it
#   is uncongenial in Meng's sense, so that attenuation is not guaranteed to be
#   conservative in general -- which is precisely why it is checked here rather
#   than asserted.
#   The sensitivity is nevertheless worth reporting, because a reader may hold
#   the other estimand. PART 2 below computes it.
#
# QUESTION 2 -- THE JENSEN GAP (the one real caveat).
#   The Berkson argument above is exact on the LOGIT scale, but the exposure
#   enters the health regression on the PROBABILITY scale, as 100 * delta p, and
#   plogis() is nonlinear. So p(E[logit]) != E[p(logit)], and with sigma large on
#   the 2019 leg the gap need not be negligible. PART 1 computes the corrected
#   surface BOTH ways --
#       p(E[logit]) = plogis(colMeans(L))      <- what the paper uses
#       E[p]        = colMeans(plogis(L))      <- the mean on the probability scale
#   -- reports the district-level gap in percentage points, and refits the
#   change-on-change model on each, so the claim "the coefficient is stable to
#   the scale on which the posterior is averaged" is checked rather than assumed.
#
# NOTHING IS REFITTED. Everything is read from the saved posterior draws and the
# saved brms fits.
#
# Standalone inputs (files on disk only, all from dir_out):
#   health_district_wide.rds            <- H1_prep_mortality.R
#   corrected_nfhs_districts.rds        <- 05_correction.R
#   correction_posterior_draws.rds      <- 05_correction.R  (REQUIRED)
#   df_wide_health.rds                  <- H3_env_covariates.R (optional)
#   brms_me_nfhs4_access.rds            <- 05_correction.R   (sigma draws)
#   brms_me_nfhs5_ires.rds              <- 05_correction.R   (sigma draws)
#   design_variance_decomposition.csv   <- 22_design_analysis.R (sigma fallback)
#   health_effects_table.csv            <- H2_health_models.R (validation only)
#
# Outputs (dir_out):
#   ppd_sensitivity.csv          beta/SE/z/p/FMI for every exposure variant
#   ppd_surface_comparison.csv   per-leg surface diagnostics (Jensen gap, sigma)
#   ppd_summary.csv              the scalars the manuscript quotes
# ==============================================================================

## ---- CONFIG ------------------------------------------------------------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers
SCRIPT <- "23_ppd_sensitivity.R"

# Fall back to no-op check helpers if checks.R is not on the path, so the script
# still runs standalone from another directory.
if (!exists("chk")) {
  chk      <- function(script, label, ok, detail = "") {
    cat(sprintf("  [%s] %-56s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label, detail)); invisible(ok) }
  chk_warn <- function(script, label, ok, detail = "") {
    cat(sprintf("  [%s] %-56s %s\n", if (isTRUE(ok)) "PASS" else "WARN", label, detail)); invisible(ok) }
  chk_header <- function(script) cat(sprintf("\n== CHECKS [%s] ==\n", script))
  chk_rng <- function(x) { x <- x[is.finite(x)]
    if (!length(x)) "no finite values" else sprintf("range [%.3g, %.3g], n=%d", min(x), max(x), length(x)) }
}

suppressPackageStartupMessages({
  library(tidyverse); library(lmtest); library(sandwich)
})

M       <- 200      # imputations, matching H2 and 22_design_analysis.R
SEED    <- 20260802 # residual draws for the posterior-predictive surface
# A coefficient shift of more than this fraction of the reference coefficient is
# reported as a WARN rather than a PASS. The Jensen gap is a scale artefact, not
# a finding; if it moved the coefficient by more than 5% it would need saying so
# in the text rather than being relegated to a sensitivity line.
JENSEN_TOL_REL <- 0.05

message("\n===== 23_ppd_sensitivity.R : estimand and probability-scale sensitivity =====")

## ==============================================================================
## PART 0 -- rebuild H2's analysis frame, EXACTLY as H2 (and 22) does
## ==============================================================================
path_health <- file.path(dir_out, "health_district_wide.rds")
path_corr   <- file.path(dir_out, "corrected_nfhs_districts.rds")
path_draws  <- file.path(dir_out, "correction_posterior_draws.rds")
path_dfwide <- file.path(dir_out, "df_wide_health.rds")
path_tab    <- file.path(dir_out, "health_effects_table.csv")
path_fitA   <- file.path(dir_out, "brms_me_nfhs4_access.rds")
path_fitI   <- file.path(dir_out, "brms_me_nfhs5_ires.rds")
path_vdec   <- file.path(dir_out, "design_variance_decomposition.csv")

for (p in c(path_health, path_corr, path_draws))
  if (!file.exists(p)) stop("Required input missing: ", p,
                            "\nRun H1_prep_mortality.R and 05_correction.R first.")

health <- readRDS(path_health)
corr   <- readRDS(path_corr)
pd     <- readRDS(path_draws)

corr_j <- corr %>%
  transmute(district   = as.character(as.numeric(district)),
            lpg15_raw  = lpg_2015_rural,  lpg19_raw = lpg_2019_rural,
            lpg15_rc   = .data[["lpg_2015_rc"]],    lpg19_rc = .data[["lpg_2019_rc"]],
            lpg15_b    = .data[["lpg_2015_bayes"]], lpg19_b  = .data[["lpg_2019_bayes"]])

df <- health %>%
  mutate(district = as.character(district)) %>%
  left_join(corr_j, by = "district") %>%
  mutate(change_lpg_raw   = 100 * (lpg19_raw - lpg15_raw),
         change_lpg_rc    = 100 * (lpg19_rc  - lpg15_rc),
         change_lpg_bayes = 100 * (lpg19_b   - lpg15_b))

if (file.exists(path_dfwide)) {
  dfw <- readRDS(path_dfwide)
  id_col <- intersect(c("District.ID","dist_code","DHSREGCO","district"), names(dfw))[1]
  if (!is.na(id_col)) {
    dfw$district <- as.character(as.numeric(dfw[[id_col]]))
    env_cols <- intersect(c("change_pm","temp_change","rh_change",
                            "droughtchange","evichange","region"), names(dfw))
    if (length(env_cols))
      df <- left_join(df, dfw %>% select(district, all_of(env_cols)) %>%
                        distinct(district, .keep_all = TRUE), by = "district")
  }
}
df <- as.data.frame(df)

dhs_adj <- intersect(c("change_poor","change_mother_low_edu","change_electricity",
                       "change_muslim","change_improved_sanitation",
                       "change_improved_water"), names(df))
env_adj <- intersect(c("change_pm","temp_change","rh_change",
                       "droughtchange","evichange"), names(df))
adj <- c(dhs_adj, env_adj)
stopifnot(!("weighted_temperature_change" %in% adj), !("heat_index_change" %in% adj))
has_region <- "region" %in% names(df)
has_state  <- "state"  %in% names(df)
wcol <- if ("w_births_hmean" %in% names(df)) "w_births_hmean" else NA
OUTCOMES <- c(neonatal = "change_neonatal_death", infant = "change_infant_death")

# H2's fit_one, verbatim in behaviour (lm + cluster-robust VCE + coeftest).
fit_one <- function(dat, outcome, exposure, wvec = NULL) {
  dat <- as.data.frame(dat)
  rhs <- c(exposure, adj, if (has_region) "factor(region)")
  f   <- reformulate(rhs, response = outcome)
  if (!is.null(wvec)) {
    ok  <- is.finite(wvec) & wvec > 0
    dat <- dat[ok, , drop = FALSE]; rownames(dat) <- NULL; wvec <- wvec[ok]
  }
  m    <- lm(f, data = dat, weights = wvec)
  rows <- as.numeric(rownames(model.frame(m)))
  vc   <- if (has_state) sandwich::vcovCL(m, cluster = dat[["state"]][rows])
          else sandwich::vcovHC(m, "HC1")
  ct   <- lmtest::coeftest(m, vcov. = vc)
  i    <- match(exposure, rownames(ct))
  c(estimate = ct[i,1], se = ct[i,2], n = nobs(m))
}

message(sprintf("Analysis frame: %d districts, %d adjustment covariates%s, weight = %s",
                nrow(df), length(adj), if (has_region) " + region FE" else "",
                if (is.na(wcol)) "NONE (unweighted)" else wcol))

## ---- the saved posterior, on the logit scale --------------------------------
L15  <- qlogis(pd$y2015);  L19 <- qlogis(pd$y2019)
id15 <- as.character(as.numeric(pd$districts_2015))
id19 <- as.character(as.numeric(pd$districts_2019))
m15  <- colMeans(L15);     m19 <- colMeans(L19)        # E[logit]
q15  <- colMeans(pd$y2015); q19 <- colMeans(pd$y2019)  # E[p]

n_avail <- min(nrow(L15), nrow(L19))
Meff    <- min(M, n_avail)
# ---- WHICH draws (must match H2_health_models.R exactly) --------------------
# Taking rows 1..M in file order is a CONTIGUOUS BLOCK from the head of chain 1,
# not a sample of the posterior: it excludes between-chain variation and its rows
# are autocorrelated, so the between-imputation variance in Rubin's rules comes
# out too small and the pooled MI standard errors too narrow. H2_health_models.R
# thins evenly across the whole draws matrix instead; this script MUST use the
# identical selection, or its MI row is computed from different imputations than
# the published one and the reproduction check below fails (it did). Even thinning
# is deterministic -- no RNG -- so the results stay bit-reproducible.
di_idx_all <- if (n_avail >= M) round(seq(1, n_avail, length.out = M)) else seq_len(Meff)
stopifnot(length(di_idx_all) == Meff, all(di_idx_all >= 1), all(di_idx_all <= n_avail))
L15s <- L15[di_idx_all, , drop = FALSE]
L19s <- L19[di_idx_all, , drop = FALSE]

chk_header(SCRIPT)
chk(SCRIPT, "posterior draws are finite on the logit scale",
    all(is.finite(L15)) && all(is.finite(L19)),
    sprintf("2015 %s | 2019 %s", chk_rng(as.numeric(L15)), chk_rng(as.numeric(L19))))
chk(SCRIPT, "draw matrices and district vectors are conformable",
    ncol(L15) == length(id15) && ncol(L19) == length(id19),
    sprintf("2015 %d cols / %d ids ; 2019 %d cols / %d ids",
            ncol(L15), length(id15), ncol(L19), length(id19)))
chk(SCRIPT, "M imputations available in the draw matrices", Meff == M,
    sprintf("M requested %d, available %d, using %d", M, n_avail, Meff))

# ---- and WHICH draws they are ----------------------------------------------
# Structural test FIRST: an evenly thinned selection reaches BOTH ENDS of the
# draws matrix, which a contiguous head block cannot do however long it is. The
# spread ratio alone is too blunt to catch that on its own -- a head block of
# 200 autocorrelated draws still scores about 0.97 -- so it is a second, weaker
# condition rather than the test.
.sd_ratio <- function(Y)
  mean(apply(Y[di_idx_all, , drop = FALSE], 2, sd), na.rm = TRUE) /
  mean(apply(Y, 2, sd), na.rm = TRUE)
mi_sd_ratio_15 <- .sd_ratio(pd$y2015)
mi_sd_ratio_19 <- .sd_ratio(pd$y2019)
message(sprintf(
  "MI: draw indices %d..%d of %d, mean spacing %.1f, %d distinct",
  min(di_idx_all), max(di_idx_all), n_avail,
  if (length(di_idx_all) > 1) mean(diff(sort(di_idx_all))) else NA_real_,
  length(unique(di_idx_all))))
message(sprintf(
  "MI: selected-draw SD / full-posterior SD = %.3f (2015), %.3f (2019)",
  mi_sd_ratio_15, mi_sd_ratio_19))
chk(SCRIPT, "MI draws span the full posterior (same selection as H2_health_models.R)",
    isTRUE(n_avail <= M ||
           (min(di_idx_all) <= 0.05 * n_avail && max(di_idx_all) >= 0.95 * n_avail &&
            is.finite(mi_sd_ratio_15) && is.finite(mi_sd_ratio_19) &&
            abs(mi_sd_ratio_15 - 1) < 0.10 && abs(mi_sd_ratio_19 - 1) < 0.10)),
    sprintf("draws %d..%d of %d (spacing %.1f); selected/full posterior SD %.3f (2015), %.3f (2019)",
            min(di_idx_all), max(di_idx_all), n_avail,
            if (length(di_idx_all) > 1) mean(diff(sort(di_idx_all))) else NA_real_,
            mi_sd_ratio_15, mi_sd_ratio_19))
chk(SCRIPT, "p(E[logit]) surface reproduces lpg_*_bayes exactly",
    {
      a <- plogis(m15); b <- corr[["lpg_2015_bayes"]][
             match(id15, as.character(as.numeric(corr$district)))]
      ok <- is.finite(a) & is.finite(b)
      max(abs(a[ok] - b[ok])) < 1e-8
    },
    "max |plogis(colMeans(logit)) - lpg_2015_bayes| < 1e-8")

## ==============================================================================
## PART 1 -- the Jensen gap: p(E[logit]) versus E[p]
## ==============================================================================
message("\n-- Part 1: probability-scale averaging (Jensen gap) --")

gap15 <- 100 * (q15 - plogis(m15))
gap19 <- 100 * (q19 - plogis(m19))

# The corrected CHANGE is the difference of the two legs, so what matters for
# the health model is not each leg's gap but the gap in the difference: a common
# shift cancels. Report both.
gapd  <- {
  common <- intersect(id15, id19)
  g19 <- setNames(gap19, id19)[common]
  g15 <- setNames(gap15, id15)[common]
  as.numeric(g19 - g15)
}

message(sprintf("  2015 leg: E[p] - p(E[logit])  mean %+.4f pp, median %+.4f pp, max |.| %.4f pp",
                mean(gap15), median(gap15), max(abs(gap15))))
message(sprintf("  2019 leg: E[p] - p(E[logit])  mean %+.4f pp, median %+.4f pp, max |.| %.4f pp",
                mean(gap19), median(gap19), max(abs(gap19))))
message(sprintf("  change   : gap in (2019 - 2015)  mean %+.4f pp, max |.| %.4f pp, SD %.4f pp",
                mean(gapd), max(abs(gapd)), sd(gapd)))

chk(SCRIPT, "Jensen gap is a second-order effect on the change",
    max(abs(gapd)) < 5,
    sprintf("max |gap in change| = %.3f pp (mean %+.3f)", max(abs(gapd)), mean(gapd)))

## ==============================================================================
## PART 2 -- sigma, and the posterior-predictive (sigma-inclusive) surface
## ==============================================================================
message("\n-- Part 2: recovering the residual SD (sigma) for each leg --")

# Preferred source: the saved brms fits themselves. Fall back to the sigma
# column 22_design_analysis.R wrote, so this script still runs if the (large)
# fit objects have been moved off disk. If neither is available the whole
# posterior-predictive sensitivity is skipped with a WARN rather than silently
# producing an epred-only table that would be mistaken for it.
sigma_draws <- function(path) {
  if (!file.exists(path)) return(NULL)
  fit <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  v <- tryCatch(as.numeric(as.matrix(fit, variable = "sigma")),
                error = function(e) NULL)
  if (is.null(v) || !length(v))
    v <- tryCatch(as.numeric(as.matrix(fit, pars = "^sigma$")),
                  error = function(e) NULL)
  if (is.null(v) || !length(v))
    v <- tryCatch({ d <- as.data.frame(fit); as.numeric(d[["sigma"]]) },
                  error = function(e) NULL)
  if (is.null(v) || !length(v) || !any(is.finite(v))) NULL else v[is.finite(v)]
}

sd15 <- sigma_draws(path_fitA)
sd19 <- sigma_draws(path_fitI)
SIGMA_SOURCE <- "brms posterior draws (root-mean-square summary)"

# --------------------------------------------------------------------------
# HOW SIGMA IS SUMMARISED, AND WHY IT MATTERS THAT 22 AND 23 AGREE.
# 22_design_analysis.R reports sigma as the posterior ROOT-MEAN-SQUARE,
# sqrt(mean(sigma_draws^2)), not the posterior median. That is not a stylistic
# choice. Sigma enters every design calculation through the observation
# variance om2 = sigma^2 + y_se^2 + b^2 * x_se^2, and 22 averages the posterior
# covariance over draws, so the scalar that reproduces those calculations is
# the posterior mean of sigma^2 -- i.e. the RMS on the SD scale.
# On the 2019 leg the two summaries agree to three decimals. On the 2015 leg
# they do not: the ACCESS sigma posterior is piled against zero with a long
# right tail, so RMS ~ 0.118 against a median ~ 0.091, a ~30% gap. Reporting
# the median here made SI Section S5 quote a different residual SD from SI
# Section S4 for the same fitted quantity. This script therefore uses 22's
# summary throughout for REPORTING and for the cross-check.
# The residual DRAWS generated below are unaffected: each imputation uses that
# draw's own sigma, so the sensitivity still carries sigma's full uncertainty
# rather than conditioning on any scalar summary.
# --------------------------------------------------------------------------
sig_summ <- function(v) sqrt(mean(v^2))

if (is.null(sd15) || is.null(sd19)) {
  if (file.exists(path_vdec)) {
    vd <- read_csv(path_vdec, show_col_types = FALSE)
    g  <- function(pat) {
      r <- vd[grepl(pat, as.character(vd$leg)), , drop = FALSE]
      if (nrow(r)) as.numeric(r$sigma[1]) else NA_real_
    }
    if (is.null(sd15)) sd15 <- g("^2015")
    if (is.null(sd19)) sd19 <- g("^2019")
    SIGMA_SOURCE <- "design_variance_decomposition.csv (point value, no sigma posterior)"
  }
}

HAVE_SIGMA <- !is.null(sd15) && !is.null(sd19) &&
              any(is.finite(sd15)) && any(is.finite(sd19))

chk_warn(SCRIPT, "residual SD (sigma) recovered for both calibration legs",
         HAVE_SIGMA,
         if (HAVE_SIGMA) sprintf("source: %s; sigma (posterior RMS) 2015 = %.4f, 2019 = %.4f",
                                 SIGMA_SOURCE, sig_summ(sd15), sig_summ(sd19))
         else "neither the brms fits nor design_variance_decomposition.csv available -- PPD sensitivity SKIPPED")

if (HAVE_SIGMA) {
  message(sprintf("  sigma source: %s", SIGMA_SOURCE))
  message(sprintf("  sigma (2015, NFHS-4 ~ ACCESS): rms %.4f (median %.4f)  [%s]",
                  sig_summ(sd15), median(sd15), chk_rng(sd15)))
  message(sprintf("  sigma (2019, NFHS-5 ~ IRES)  : rms %.4f (median %.4f)  [%s]",
                  sig_summ(sd19), median(sd19), chk_rng(sd19)))

  # Cross-check against 22's decomposition, if that file exists and the fits
  # were the source. The two must agree; if they do not, one of the objects on
  # disk is stale and the whole design story is being told from a different fit.
  if (file.exists(path_vdec) && grepl("^brms", SIGMA_SOURCE)) {
    vd <- read_csv(path_vdec, show_col_types = FALSE)
    s15v <- as.numeric(vd$sigma[grepl("^2015", vd$leg)][1])
    s19v <- as.numeric(vd$sigma[grepl("^2019", vd$leg)][1])
    chk(SCRIPT, "sigma agrees with 22_design_analysis.R decomposition",
        is.finite(s15v) && is.finite(s19v) &&
          abs(sig_summ(sd15) - s15v) < 0.005 && abs(sig_summ(sd19) - s19v) < 0.005,
        sprintf("rms of fits %.4f/%.4f vs CSV %.4f/%.4f (same summary, tol 0.005)",
                sig_summ(sd15), sig_summ(sd19), s15v, s19v))
  }

  # Draw the residuals. Each imputation m uses that draw's own sigma where a
  # sigma posterior is available, so the sensitivity carries the uncertainty in
  # sigma too rather than conditioning on a point value.
  set.seed(SEED)
  pick_sigma <- function(v, m) if (length(v) >= m) v[m] else
                               if (length(v) == 1) v else sample(v, 1)
  # The epred rows are THINNED across the posterior now (di_idx_all), so
  # imputation m carries posterior draw di_idx_all[m]. Its residual SD must be
  # that SAME draw's sigma, not the m-th one in the sigma vector -- otherwise the
  # predictive draws pair a district's draw with an unrelated draw's sigma and the
  # sensitivity stops being a posterior-predictive sample of anything.
  ALIGNED <- length(sd15) >= max(di_idx_all) && length(sd19) >= max(di_idx_all)
  chk_warn(SCRIPT, "sigma draw index aligns with the epred draw index", ALIGNED,
           sprintf("sigma draws available %d / %d vs largest epred draw index %d%s",
                   length(sd15), length(sd19), max(di_idx_all),
                   if (ALIGNED) "" else " -- sigma resampled, not index-matched"))

  E15 <- matrix(rnorm(Meff * ncol(L15s)), nrow = Meff) *
           vapply(seq_len(Meff), function(m) pick_sigma(sd15, di_idx_all[m]), numeric(1))
  E19 <- matrix(rnorm(Meff * ncol(L19s)), nrow = Meff) *
           vapply(seq_len(Meff), function(m) pick_sigma(sd19, di_idx_all[m]), numeric(1))
  P15 <- plogis(L15s + E15)      # posterior PREDICTIVE draws, probability scale
  P19 <- plogis(L19s + E19)

  chk(SCRIPT, "posterior-predictive draws widen the epred draws",
      mean(apply(P19, 2, sd)) > mean(apply(plogis(L19s), 2, sd)),
      sprintf("mean district SD (2019): epred %.4f -> PPD %.4f (x%.2f)",
              mean(apply(plogis(L19s), 2, sd)), mean(apply(P19, 2, sd)),
              mean(apply(P19, 2, sd)) / max(mean(apply(plogis(L19s), 2, sd)), 1e-12)))
}

## ==============================================================================
## PART 3 -- refit the change-on-change model on every exposure variant
## ==============================================================================
message("\n-- Part 3: change-on-change association under each exposure variant --")

# A single-surface (no imputation) fit.
fit_surface <- function(v15, v19, outc) {
  d <- df
  d$change_lpg_x <- 100 * (unname(setNames(v19, id19)[d$district]) -
                           unname(setNames(v15, id15)[d$district]))
  r <- fit_one(d, OUTCOMES[[outc]], "change_lpg_x",
               wvec = if (is.na(wcol)) NULL else d[[wcol]])
  c(estimate = unname(r[["estimate"]]), se = unname(r[["se"]]),
    ubar = unname(r[["se"]])^2, bvar = 0, fmi = 0,
    z = unname(r[["estimate"]]) / unname(r[["se"]]),
    p = 2 * pnorm(-abs(unname(r[["estimate"]]) / unname(r[["se"]]))),
    n = unname(r[["n"]]))
}

# A multiply-imputed fit over a pair of draw matrices, pooled by Rubin's rules
# exactly as H2 and 22 do.
fit_mi <- function(A15, A19, outc) {
  est <- numeric(Meff); sev <- numeric(Meff); nn <- numeric(Meff)
  for (mm in seq_len(Meff)) {
    v15 <- setNames(A15[mm, ], id15); v19 <- setNames(A19[mm, ], id19)
    d <- df
    d$change_lpg_mi <- 100 * (unname(v19[d$district]) - unname(v15[d$district]))
    r <- fit_one(d, OUTCOMES[[outc]], "change_lpg_mi",
                 wvec = if (is.na(wcol)) NULL else d[[wcol]])
    est[mm] <- r[["estimate"]]; sev[mm] <- r[["se"]]; nn[mm] <- r[["n"]]
  }
  qbar <- mean(est); ubar <- mean(sev^2); bvar <- var(est)
  tvar <- ubar + (1 + 1/Meff) * bvar
  c(estimate = qbar, se = sqrt(tvar), ubar = ubar, bvar = bvar,
    fmi = 1 - ubar/tvar, z = qbar/sqrt(tvar),
    p = 2*pnorm(-abs(qbar/sqrt(tvar))), n = max(nn))
}

VARIANTS <- list(
  list(key = "epred_point",
       label = "Calibrated expectation, point surface  p(E[logit])  [= change_lpg_bayes]",
       kind  = "point"),
  list(key = "emean_point",
       label = "Calibrated expectation, point surface  E[p]  (probability-scale mean)",
       kind  = "point"),
  list(key = "epred_MI",
       label = "Calibrated expectation, MI over calibration posterior  [= change_lpg_bayes_MI]",
       kind  = "mi"),
  list(key = "ppd_MI",
       label = "District true prevalence, MI over posterior PREDICTIVE (sigma-inclusive)",
       kind  = "mi"),
  list(key = "ppd_point",
       label = "District true prevalence, point surface  E[p] under the predictive",
       kind  = "point")
)

res <- list()
for (outc in names(OUTCOMES)) {
  for (V in VARIANTS) {
    r <- NULL
    if (V$key == "epred_point") r <- fit_surface(plogis(m15), plogis(m19), outc)
    if (V$key == "emean_point") r <- fit_surface(q15, q19, outc)
    if (V$key == "epred_MI")    r <- fit_mi(plogis(L15s), plogis(L19s), outc)
    if (V$key == "ppd_MI"    && HAVE_SIGMA) r <- fit_mi(P15, P19, outc)
    if (V$key == "ppd_point" && HAVE_SIGMA) r <- fit_surface(colMeans(P15), colMeans(P19), outc)
    if (is.null(r)) next
    res[[length(res)+1]] <- data.frame(outcome = outc, variant = V$key,
                                       label = V$label, t(r), row.names = NULL)
    cat(sprintf("  %-9s %-12s beta=%+.5f  se=%.5f  z=%+.3f  p=%.4f  FMI=%.3f  n=%d\n",
                outc, V$key, r[["estimate"]], r[["se"]], r[["z"]], r[["p"]],
                r[["fmi"]], as.integer(r[["n"]])))
  }
}
sens <- do.call(rbind, res)
write_csv(sens, file.path(dir_out, "ppd_sensitivity.csv"))
message(sprintf("  wrote ppd_sensitivity.csv (%d rows)", nrow(sens)))

getv <- function(outc, key, col) {
  r <- sens[sens$outcome == outc & sens$variant == key, , drop = FALSE]
  if (!nrow(r)) NA_real_ else as.numeric(r[[col]][1])
}

## ---- validate the two anchors against the published H2 rows -----------------
if (file.exists(path_tab)) {
  tab <- read_csv(path_tab, show_col_types = FALSE)
  pick <- function(o, tm) tryCatch({
    r <- tab
    if ("outcome"    %in% names(r)) r <- r[as.character(r$outcome) == OUTCOMES[[o]] |
                                           as.character(r$outcome) == o, , drop = FALSE]
    if ("term"       %in% names(r)) r <- r[as.character(r$term) == tm, , drop = FALSE]
    if ("adjusted"   %in% names(r)) r <- r[as.logical(r$adjusted) %in% TRUE, , drop = FALSE]
    if ("weighting"  %in% names(r)) r <- r[as.character(r$weighting) == "birth_hmean", , drop = FALSE]
    for (sc in c("support_stratum", "stratum", "support"))
      if (sc %in% names(r)) { r <- r[is.na(r[[sc]]) | as.character(r[[sc]]) == "all", , drop = FALSE]; break }
    if (nrow(r) == 0 || !all(c("estimate","se") %in% names(r))) NULL else as.list(r[1, ])
  }, error = function(e) NULL)
  for (o in names(OUTCOMES)) {
    for (pr in list(c("epred_point","change_lpg_bayes"), c("epred_MI","change_lpg_bayes_MI"))) {
      pp <- pick(o, pr[2])
      if (!is.null(pp))
        chk(SCRIPT, sprintf("%s reproduces published %s (%s)", pr[1], pr[2], o),
            abs(getv(o, pr[1], "estimate") - pp$estimate) < 5e-4 &&
              abs(getv(o, pr[1], "se") - pp$se) < 5e-4,
            sprintf("here %.5f/%.5f vs table %.4f/%.4f",
                    getv(o, pr[1], "estimate"), getv(o, pr[1], "se"), pp$estimate, pp$se))
      else
        chk_warn(SCRIPT, sprintf("%s cross-checked against H2 (%s)", pr[1], o),
                 FALSE, sprintf("%s row not found in health_effects_table.csv", pr[2]))
    }
  }
} else {
  chk_warn(SCRIPT, "health_effects_table.csv present for anchor validation",
           FALSE, "not found -- anchors NOT cross-checked; run H2 first")
}

## ---- the two substantive checks ---------------------------------------------
for (o in names(OUTCOMES)) {
  b_ep <- getv(o, "epred_point", "estimate"); b_em <- getv(o, "emean_point", "estimate")
  rel  <- if (is.finite(b_ep) && b_ep != 0) abs(b_em - b_ep) / abs(b_ep) else NA_real_
  chk(SCRIPT, sprintf("coefficient stable to probability- vs logit-scale averaging (%s)", o),
      is.finite(rel) && rel < JENSEN_TOL_REL,
      sprintf("p(E[logit]) %+.5f vs E[p] %+.5f -- relative shift %.2f%% (tol %.0f%%)",
              b_ep, b_em, 100*rel, 100*JENSEN_TOL_REL))
}

if (HAVE_SIGMA) {
  for (o in names(OUTCOMES)) {
    b_mi <- getv(o, "epred_MI", "estimate"); s_mi <- getv(o, "epred_MI", "se")
    b_pp <- getv(o, "ppd_MI",   "estimate"); s_pp <- getv(o, "ppd_MI",   "se")
    # WHY THIS IS NOT A TEST OF "THE POOLED SE SHOULD BE UNCHANGED".
    # An earlier version of this check warned unless the posterior-predictive
    # SE stayed within 20% of the epred SE, on the reasoning that a Berkson
    # error passes into the outcome residual rather than into the regressor's
    # information. That reasoning describes the PRIMARY estimand, where the
    # residual is never added to the regressor. It does not describe THIS
    # sensitivity, where the residual IS added: doing so inflates Var(x)
    # itself, so the coefficient attenuates roughly like Var(x)/Var(x*) while
    # its standard error falls roughly like 1/sd(x*). Both shrink, and the
    # quantity that should be approximately preserved is the ratio between
    # them -- the test statistic -- not the standard error. The old check
    # therefore fired [WARN] on a run that behaved exactly as classical
    # measurement error predicts. What is checked now is the classical-error
    # signature itself: both quantities shrink, and |z| survives.
    z_mi <- if (is.finite(b_mi) && is.finite(s_mi) && s_mi > 0) abs(b_mi/s_mi) else NA_real_
    z_pp <- if (is.finite(b_pp) && is.finite(s_pp) && s_pp > 0) abs(b_pp/s_pp) else NA_real_
    r_b  <- if (is.finite(b_mi) && b_mi != 0) b_pp/b_mi else NA_real_
    r_s  <- if (is.finite(s_mi) && s_mi != 0) s_pp/s_mi else NA_real_
    r_z  <- if (is.finite(z_mi) && z_mi > 0)  z_pp/z_mi else NA_real_
    chk_warn(SCRIPT,
             sprintf("coefficient and SE shrink together under the PPD estimand (%s)", o),
             is.finite(r_b) && is.finite(r_s) && is.finite(r_z) &&
               r_b < 1.05 && r_s < 1.05 && r_z > 0.5 && r_z < 2,
             sprintf("beta x%.3f, SE %.5f -> %.5f (x%.3f), |z| %.3f -> %.3f (x%.2f; tol 0.5-2.0)",
                     r_b, s_mi, s_pp, r_s, z_mi, z_pp, r_z))
    chk_warn(SCRIPT, sprintf("posterior-predictive coefficient attenuates, as predicted (%s)", o),
             is.finite(b_mi) && is.finite(b_pp) && abs(b_pp) <= abs(b_mi) * 1.05,
             sprintf("beta epred %+.5f -> PPD %+.5f (ratio %.3f; <1 = attenuation toward the null)",
                     b_mi, b_pp, b_pp / b_mi))
    chk_warn(SCRIPT, sprintf("neither estimand changes the qualitative conclusion (%s)", o),
             is.finite(b_mi) && is.finite(b_pp) &&
               sign(b_mi) == sign(b_pp) &&
               (getv(o,"epred_MI","p") < 0.05) == (getv(o,"ppd_MI","p") < 0.05),
             sprintf("sign %s/%s ; p %.4f vs %.4f",
                     ifelse(b_mi<0,"-","+"), ifelse(b_pp<0,"-","+"),
                     getv(o,"epred_MI","p"), getv(o,"ppd_MI","p")))
  }
}

## ==============================================================================
## PART 4 -- surface-level diagnostics and the summary scalars
## ==============================================================================
sd_ep19 <- mean(apply(plogis(L19s), 2, sd)); sd_ep15 <- mean(apply(plogis(L15s), 2, sd))
surf <- data.frame(
  leg              = c("2015 (NFHS-4 ~ ACCESS)", "2019 (NFHS-5 ~ IRES)"),
  n_districts      = c(length(id15), length(id19)),
  sigma            = c(if (HAVE_SIGMA) sig_summ(sd15) else NA_real_,
                       if (HAVE_SIGMA) sig_summ(sd19) else NA_real_),
  sigma_median     = c(if (HAVE_SIGMA) median(sd15) else NA_real_,
                       if (HAVE_SIGMA) median(sd19) else NA_real_),
  sigma_source     = c(if (HAVE_SIGMA) SIGMA_SOURCE else NA_character_,
                       if (HAVE_SIGMA) SIGMA_SOURCE else NA_character_),
  mean_p_Elogit    = c(100*mean(plogis(m15)), 100*mean(plogis(m19))),
  mean_Ep          = c(100*mean(q15),         100*mean(q19)),
  jensen_gap_mean  = c(mean(gap15), mean(gap19)),
  jensen_gap_maxabs= c(max(abs(gap15)), max(abs(gap19))),
  sd_epred_pp      = 100 * c(sd_ep15, sd_ep19),
  sd_ppd_pp        = 100 * c(if (HAVE_SIGMA) mean(apply(P15, 2, sd)) else NA_real_,
                             if (HAVE_SIGMA) mean(apply(P19, 2, sd)) else NA_real_),
  stringsAsFactors = FALSE
)
surf$sd_inflation <- surf$sd_ppd_pp / surf$sd_epred_pp
write_csv(surf, file.path(dir_out, "ppd_surface_comparison.csv"))
message("\n-- Part 4: surface diagnostics --")
print(surf %>% select(leg, sigma, mean_p_Elogit, mean_Ep, jensen_gap_mean,
                      sd_epred_pp, sd_ppd_pp, sd_inflation))

summ <- data.frame(
  quantity = c("jensen_gap_change_mean_pp", "jensen_gap_change_maxabs_pp",
               "jensen_rel_shift_infant_pct", "jensen_rel_shift_neonatal_pct",
               "sigma_2015", "sigma_2019", "sigma_source",
               "sd_inflation_2015", "sd_inflation_2019",
               "ppd_beta_infant", "ppd_se_infant", "ppd_p_infant", "ppd_fmi_infant",
               "ppd_beta_neonatal", "ppd_se_neonatal", "ppd_p_neonatal", "ppd_fmi_neonatal",
               "ppd_vs_epred_beta_ratio_infant", "ppd_vs_epred_se_ratio_infant",
               "have_sigma"),
  value = c(
    mean(gapd), max(abs(gapd)),
    100 * abs(getv("infant","emean_point","estimate") - getv("infant","epred_point","estimate")) /
      abs(getv("infant","epred_point","estimate")),
    100 * abs(getv("neonatal","emean_point","estimate") - getv("neonatal","epred_point","estimate")) /
      abs(getv("neonatal","epred_point","estimate")),
    if (HAVE_SIGMA) sig_summ(sd15) else NA, if (HAVE_SIGMA) sig_summ(sd19) else NA,
    if (HAVE_SIGMA) SIGMA_SOURCE else NA,
    surf$sd_inflation[1], surf$sd_inflation[2],
    getv("infant","ppd_MI","estimate"), getv("infant","ppd_MI","se"),
    getv("infant","ppd_MI","p"), getv("infant","ppd_MI","fmi"),
    getv("neonatal","ppd_MI","estimate"), getv("neonatal","ppd_MI","se"),
    getv("neonatal","ppd_MI","p"), getv("neonatal","ppd_MI","fmi"),
    getv("infant","ppd_MI","estimate") / getv("infant","epred_MI","estimate"),
    getv("infant","ppd_MI","se")       / getv("infant","epred_MI","se"),
    as.numeric(HAVE_SIGMA)),
  stringsAsFactors = FALSE
)
write_csv(summ, file.path(dir_out, "ppd_summary.csv"))
message("\n-- ppd_summary.csv --")
print(summ)

chk(SCRIPT, "all three output CSVs written",
    all(file.exists(file.path(dir_out, c("ppd_sensitivity.csv",
                                         "ppd_surface_comparison.csv",
                                         "ppd_summary.csv")))),
    "ppd_sensitivity.csv, ppd_surface_comparison.csv, ppd_summary.csv")
chk(SCRIPT, "sensitivity table covers both outcomes and all fitted variants",
    nrow(sens) == length(OUTCOMES) * (if (HAVE_SIGMA) length(VARIANTS) else 3),
    sprintf("%d rows (%d outcomes x %d variants)", nrow(sens), length(OUTCOMES),
            if (HAVE_SIGMA) length(VARIANTS) else 3))
chk(SCRIPT, "no non-finite estimates in the sensitivity table",
    all(is.finite(sens$estimate)) && all(is.finite(sens$se)),
    chk_rng(sens$estimate))

message("\n===== 23_ppd_sensitivity.R complete =====\n")
