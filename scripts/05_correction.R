# ==============================================================================
# 05_correction.R
# Calibrate NFHS to the higher-fidelity ENERGY-SURVEY reference (ACCESS W1 / IRES),
# treating the NFHS district estimate as error-prone. NOTE: the energy surveys are
# a better-instrumented REFERENCE, not verified ground truth -- they differ from
# NFHS in timing, frame, wording, respondent, and season, so the outputs are
# "reference-calibrated" estimates, not recovered true prevalence. See the
# Discussion limitations; the benchmark/season analyses characterize, but do not
# prove, that all NFHS-reference disagreement is NFHS measurement error.
# Two approaches, per plan:
#
#   (1) REGRESSION CALIBRATION (transparent, frequentist)
#       Logit-scale linear calibration of reference on NFHS, with state effects,
#       fit in the overlapping districts and applied to ALL NFHS districts.
#
#   (2) BAYESIAN HIERARCHICAL MEASUREMENT-ERROR MODEL (brms)
#       logit(p_ref_d) ~ me(logit_p_nfhs_d, se_d) + (1 | state)
#       - propagates sampling error in the NFHS district estimates
#       - partial pooling across states
#       - posterior predictive draws give corrected estimates + 95% CrIs
#         for every NFHS district nationally.
#
# Caveats encoded below:
#   - Pair A calibration is estimated on 6 poor northern states; applying it
#     nationally is an extrapolation. Pair B (IRES) spans 21 states and is the
#     better basis for a national correction; Pair A serves as replication.
#   - Part of NFHS-vs-reference disagreement is real temporal change (PMUY).
#
# Inputs : compare_pairs.rds, nfhs_districts.rds
# Outputs: corrected_nfhs_districts.rds, calibration_models.rds, figures
# ==============================================================================

source("00_config.R")
need_inputs(c("compare_pairs.rds"  = "04_compare.R",
              "nfhs_districts.rds" = "01_prep_nfhs.R"))
library(brms)

pairs <- readRDS(file.path(dir_out, "compare_pairs.rds"))
nfhs  <- readRDS(file.path(dir_out, "nfhs_districts.rds"))
pairA <- pairs$pairA   # NFHS-4 rural vs ACCESS W1
pairB <- pairs$pairB   # NFHS-5 rural vs IRES

clamp <- function(p, eps = 1e-3) pmin(pmax(p, eps), 1 - eps)

# ------------------------------------------------------------------------------
# (1) Regression calibration
# ------------------------------------------------------------------------------
# Model (fit in overlap districts):
#   logit(p_ref) = a + b * logit(p_nfhs) + state effect + error
# Weighted by reference-survey household counts.

calib_fit <- function(df, ref, nfhs_est, wt) {
  df <- df %>%
    mutate(y = qlogis(clamp(.data[[ref]])),
           x = qlogis(clamp(.data[[nfhs_est]])))
  lm(y ~ x + state_name, data = df, weights = df[[wt]])
}

calA <- calib_fit(pairA, "access_w1_mainlpg", "lpg_2015_rural", "n_access_w1")
calB <- calib_fit(pairB, "ires_mainlpg_rural", "lpg_2019_rural", "n_ires_rural")
summary(calA); summary(calB)

# Apply nationally. State effects only exist for calibration states, so for
# out-of-sample states we apply the average calibration (state effect = 0 on
# the mean-centered contrast). Simplest robust route: refit without state for
# the national application, keep the state version for within-sample checks.
calA_nat <- lm(qlogis(clamp(access_w1_mainlpg)) ~ qlogis(clamp(lpg_2015_rural)),
               data = pairA, weights = pairA$n_access_w1)
calB_nat <- lm(qlogis(clamp(ires_mainlpg_rural)) ~ qlogis(clamp(lpg_2019_rural)),
               data = pairB, weights = pairB$n_ires_rural)

nfhs_corrected <- nfhs %>%
  mutate(
    lpg_2015_rc = plogis(predict(calA_nat,
                    newdata = tibble(lpg_2015_rural = clamp(lpg_2015_rural)))),
    lpg_2019_rc = plogis(predict(calB_nat,
                    newdata = tibble(lpg_2019_rural = clamp(lpg_2019_rural))))
  )

# ------------------------------------------------------------------------------
# (2) Bayesian hierarchical measurement-error model (brms)
# ------------------------------------------------------------------------------
# SEs for the NFHS logit-scale estimates: use the design-weighted SEs from 01
# (delta method to logit scale: se_logit = se_p / (p * (1 - p))).

prep_me <- function(df, ref, nfhs_est, nfhs_se, ref_n, ref_se = NULL) {
  keep <- complete.cases(df[[ref]], df[[nfhs_est]], df[[nfhs_se]])
  df   <- df[keep, , drop = FALSE]
  p_ref <- clamp(df[[ref]])
  # Reference-side logit SE. PREFERRED: the reference survey's DESIGN-BASED
  # (Taylor-linearized) SE, delta-transformed to the logit scale -- a proper
  # design SE (survey weights + village clustering), used whenever a ref_se column
  # is supplied. NFHS-5/IRES now passes the rural design-weighted IRES SE here, so
  # the reference-side measurement-error variance is design-valid, not a binomial
  # guess. FALLBACK (ref_se absent or non-finite for a district): the simple-
  # binomial logit SE from the raw reference count, se(logit p) ~ 1/sqrt(n*p*(1-p)),
  # which ignores weights/clustering and understates uncertainty -- a rough,
  # sensitivity-style value used only where no design SE is available (e.g. the
  # ACCESS/NFHS-4 replication, which still uses the multilevel reference).
  y_se_binom <- 1 / sqrt(pmax(df[[ref_n]], 1) * p_ref * (1 - p_ref))
  if (!is.null(ref_se) && ref_se %in% names(df)) {
    y_se_design <- df[[ref_se]] / (p_ref * (1 - p_ref))          # delta method to logit
    y_se <- ifelse(is.finite(y_se_design) & y_se_design > 0, y_se_design, y_se_binom)
  } else {
    y_se <- y_se_binom
  }
  df$p_ref <- p_ref
  df$y     <- qlogis(p_ref)
  df$y_se  <- pmin(pmax(y_se, 0.02), 2)
  df$x_obs <- qlogis(clamp(df[[nfhs_est]]))
  df$x_se  <- pmin(pmax(df[[nfhs_se]] /
                (clamp(df[[nfhs_est]]) * (1 - clamp(df[[nfhs_est]]))), 0.01), 2)
  df
}

# NFHS-4 replication: ACCESS multilevel reference (binomial y_se fallback).
meA <- prep_me(pairA, "access_w1_mainlpg", "lpg_2015_rural_wt",
               "lpg_2015_rural_wt_se", "n_access_w1")
# LEG-A SYMMETRY SENSITIVITY. The primary NFHS-4 leg above uses the ACCESS
# multilevel (glmer) reference, for which no Taylor-linearized design SE exists,
# so prep_me() falls back to the simple-binomial logit SE. That fallback ignores
# ACCESS's village clustering and its village-level weights, understating
# reference-side uncertainty and therefore letting the NFHS-4 calibration weight
# ACCESS observations more heavily than a design-valid SE would. Leg B has no
# such asymmetry: it passes the IRES rural design SE through ref_se. This
# sensitivity refits leg A with EXACTLY the leg-B treatment -- the ACCESS
# design-weighted direct estimate as the reference, and its own Taylor design SE
# as the reference-side measurement error -- so the two legs are constructed
# identically. It is a sensitivity, not the primary: the design-weighted ACCESS
# estimate is noisier district by district than the multilevel one, which is why
# the multilevel reference was preferred in the first place. What matters is how
# far the downstream corrected change moves, which H2 reports.
meA_wt <- prep_me(pairA, "access_w1_mainlpg_wt", "lpg_2015_rural_wt",
                  "lpg_2015_rural_wt_se", "n_access_w1",
                  ref_se = "access_w1_mainlpg_wt_se")
# NFHS-5 national product: RURAL DESIGN-WEIGHTED IRES reference, with its
# Taylor-linearized design SE as the reference-side measurement error (like
# against like -- both sides are the rural design-weighted direct estimate).
meB <- prep_me(pairB, "ires_mainlpg_rural_wt", "lpg_2019_rural_wt",
               "lpg_2019_rural_wt_se", "n_ires_rural",
               ref_se = "ires_mainlpg_rural_wt_se")

# y | se(y_se, sigma=TRUE): the reference logit is a noisy observation of the
# latent district value with known SE y_se, plus a residual (sigma) for
# questionnaire/design non-comparability. Both surveys' uncertainty now enter.
bform <- bf(y | se(y_se, sigma = TRUE) ~ me(x_obs, x_se) + (1 | state_name))

fit_me <- function(dat, file_tag) {
  brm(bform, data = dat, family = gaussian(),
      prior = c(prior(normal(0, 2), class = "b"),
                prior(normal(0, 2), class = "Intercept"),
                prior(exponential(1), class = "sd"),
                prior(exponential(1), class = "sigma")),
      # iter 12000 (was 6000). The 2026-08-01 run left the NFHS-5 ~ IRES fit
      # under-mixed: max Rhat 1.020 against the 1.01 threshold below, and
      # minimum bulk ESS 304 against 400. Neither is near the conventional
      # danger line (Rhat > 1.05, ESS < 100), but low tail ESS degrades exactly
      # the quantity the paper reports -- the 95% CrI endpoints of the
      # calibration slope -- so the honest fix is more draws rather than a
      # relaxed threshold. ESS scales roughly linearly in draws, so doubling
      # should clear 400 with margin.
      chains = 4, cores = 4, iter = 12000, seed = 1234,
      # adapt_delta 0.999 / max_treedepth 15 (was 0.995 / 15, and 0.99 / 12
      # before that). The NFHS-4 ~ ACCESS fit still produced ONE divergent
      # transition after warmup at 0.995. A single divergence in 12,000 draws
      # is unlikely to move the posterior materially, but it signals the
      # sampler grazed a region it could not resolve, and doubling the draws
      # would otherwise be expected to double the divergence count. Smaller
      # steps cost only run time. If divergences persist at 0.999 the problem
      # is the geometry of the me() funnel, not the tuning, and the check below
      # will say so rather than letting it pass silently as it did before.
      control = list(adapt_delta = 0.999, max_treedepth = 15),
      file = file.path(dir_out, paste0("brms_me_", file_tag)),
      # CAUTION: file_refit = "on_change" compares the Stan code, the Stan data
      # and the algorithm. It does NOT look at iter, chains, or control, so
      # changing any sampler argument above will silently reload the OLD cached
      # fit and reproduce the OLD diagnostics. Delete or move aside
      # <dir_out>/brms_me_nfhs4_access.rds and brms_me_nfhs5_ires.rds whenever
      # the sampler settings change. rerun_mcmc_only.sh does this for you.
      file_refit = "on_change")   # refit automatically if formula/data change
}

bA <- fit_me(meA, "nfhs4_access")
bB <- fit_me(meB, "nfhs5_ires")
# Fresh file_tag on purpose: file_refit = "on_change" would otherwise be free to
# reload brms_me_nfhs4_access.rds, since it compares Stan code/data/algorithm and
# this fit shares the formula. A distinct filename makes collision impossible.
bA_wt <- fit_me(meA_wt, "nfhs4_access_wt")
print(summary(bA)); print(summary(bB)); print(summary(bA_wt))

# ---- SE-construction diagnostics -> CSV -------------------------------------
# Section 2.4.2, Section 2.4.3 and SI Method S1 describe HOW the measurement-
# error standard errors were built: which districts got the reference survey's
# Taylor-linearized design SE and which fell back to the binomial form, where
# the pre-fit bounds actually bind, and how small the IRES rural subsample gets.
# Those are properties of prep_me()'s data preparation, invisible in the brms
# summary, and until now they lived only in this script's source. Write them
# out so the manuscript sources them instead of restating them by hand.
se_diagnostics <- function(df, ref, nfhs_est, nfhs_se, ref_n, ref_se = NULL) {
  keep  <- complete.cases(df[[ref]], df[[nfhs_est]], df[[nfhs_se]])
  d     <- df[keep, , drop = FALSE]
  p_ref <- clamp(d[[ref]])
  binom <- 1 / sqrt(pmax(d[[ref_n]], 1) * p_ref * (1 - p_ref))
  if (!is.null(ref_se) && ref_se %in% names(d)) {
    dsn <- d[[ref_se]] / (p_ref * (1 - p_ref))
    ok  <- is.finite(dsn) & dsn > 0
    y   <- ifelse(ok, dsn, binom)
  } else {
    ok <- rep(FALSE, nrow(d)); y <- binom
  }
  xs <- d[[nfhs_se]] / (clamp(d[[nfhs_est]]) * (1 - clamp(d[[nfhs_est]])))
  list(n = nrow(d), n_design = sum(ok), n_fallback = sum(!ok),
       y = y, x = xs, n_ref = d[[ref_n]],
       # the district(s) whose raw reference SE exceeds the upper bound
       clamped_hi = y[y > 2], n_ref_at_bound = d[[ref_n]][d[[ref]] > 1 - 1e-3])
}
dgA <- se_diagnostics(pairA, "access_w1_mainlpg", "lpg_2015_rural_wt",
                      "lpg_2015_rural_wt_se", "n_access_w1")
dgB <- se_diagnostics(pairB, "ires_mainlpg_rural_wt", "lpg_2019_rural_wt",
                      "lpg_2019_rural_wt_se", "n_ires_rural",
                      ref_se = "ires_mainlpg_rural_wt_se")
dgA_wt <- se_diagnostics(pairA, "access_w1_mainlpg_wt", "lpg_2015_rural_wt",
                         "lpg_2015_rural_wt_se", "n_access_w1",
                         ref_se = "access_w1_mainlpg_wt_se")
# How much reference-side uncertainty the primary leg-A fallback was throwing
# away: the ratio of the design logit SE to the binomial logit SE it replaces.
.legA_binom <- { p <- clamp(pairA$access_w1_mainlpg_wt)
                 1 / sqrt(pmax(pairA$n_access_w1, 1) * p * (1 - p)) }
.legA_ok    <- is.finite(dgA_wt$y) & is.finite(.legA_binom) & .legA_binom > 0
y_unclamped <- c(dgA$y[dgA$y <= 2], dgB$y[dgB$y <= 2])
x_all       <- c(dgA$x, dgB$x)
me_se_diag <- tibble::tibble(
  quantity = c("a_n", "b_n", "b_design", "b_fallback", "b_allyes_n",
               "clamp_raw", "y_lo", "y_hi", "x_lo", "x_hi",
               "ires_rural_med", "ires_rural_min", "ires_rural_max",
               "n_clamped_hi"),
  value = c(dgA$n, dgB$n, dgB$n_design, dgB$n_fallback,
            if (length(dgB$n_ref_at_bound)) min(dgB$n_ref_at_bound) else NA_real_,
            round(if (length(dgB$clamped_hi)) max(dgB$clamped_hi) else NA_real_, 2),
            round(min(y_unclamped), 2), round(max(y_unclamped), 2),
            round(min(x_all), 2), round(max(x_all), 2),
            stats::median(dgB$n_ref), min(dgB$n_ref), max(dgB$n_ref),
            length(dgA$clamped_hi) + length(dgB$clamped_hi))
)
readr::write_csv(me_se_diag, file.path(dir_out, "me_se_diagnostics.csv"))
message("me_se_diagnostics.csv written:")
print(as.data.frame(me_se_diag))
chk("05", "reference-side design SE used for the large majority of NFHS-5/IRES districts",
    dgB$n_design >= 0.9 * dgB$n,
    sprintf("design SE %d of %d districts (binomial fallback %d)",
            dgB$n_design, dgB$n, dgB$n_fallback))
chk("05", "pre-fit SE bounds bind for at most one district across both panels",
    (length(dgA$clamped_hi) + length(dgB$clamped_hi)) <= 1,
    sprintf("districts hitting the upper SE bound: %d",
            length(dgA$clamped_hi) + length(dgB$clamped_hi)))

# ---- Calibration parameters -> CSV -----------------------------------------
# Table S3 and the Section 3.2 prose quote the slope, intercept, between-state
# SD and residual SD of these two fits. Until now those numbers were read off
# the printed summary and re-typed into the manuscript by hand, which is exactly
# the failure mode the rest of the pipeline avoids: a re-run that moved the
# posterior would leave a stale slope alive in the paper. Write them out so the
# document builder can source them. Columns are brms' own summary quantities
# (posterior mean and 95% credible interval), not re-derived from the draws.
# Post-warmup draws of a named stan parameter without requiring rstan on the
# search path: brms keeps the stanfit in $fit and its raw sample lists in the
# "sim" attribute, with warmup2 giving the warmup iterations retained. Identical
# accessor to 22_design_analysis.R, so the two scripts summarise the same draws.
sig_rms <- function(fit, tag) {
  v <- tryCatch({
    sm <- attr(fit$fit, "sim"); w <- sm$warmup2[1]
    unlist(lapply(sm$samples,
                  function(ch) ch[["sigma"]][(w + 1):length(ch[["sigma"]])]))
  }, error = function(e) NULL)
  tibble::tibble(calibration = tag, parameter = "sd_residual_rms",
                 estimate = if (length(v)) sqrt(mean(v^2)) else NA_real_,
                 lo95 = NA_real_, hi95 = NA_real_)
}

calib_params <- function(fit, tag) {
  sm <- summary(fit)
  grab <- function(tab, rn, param) {
    if (is.null(tab) || !rn %in% rownames(tab)) {
      return(tibble::tibble(calibration = tag, parameter = param,
                            estimate = NA_real_, lo95 = NA_real_, hi95 = NA_real_))
    }
    r <- tab[rn, ]
    tibble::tibble(calibration = tag, parameter = param,
                   estimate = as.numeric(r[["Estimate"]]),
                   lo95     = as.numeric(r[["l-95% CI"]]),
                   hi95     = as.numeric(r[["u-95% CI"]]))
  }
  # The measurement-error slope is named for the variable pair brms built it
  # from, so match by prefix rather than hard-coding "mex_obsx_se".
  slope_rn <- grep("^bsp_|^mex_obs", rownames(sm$fixed), value = TRUE)
  slope_rn <- if (length(slope_rn)) slope_rn[1] else "mex_obsx_se"
  dplyr::bind_rows(
    grab(sm$fixed, slope_rn, "slope"),
    grab(sm$fixed, "Intercept", "intercept"),
    grab(sm$random$state_name, "sd(Intercept)", "sd_state"),
    grab(sm$spec_pars, "sigma", "sd_residual"),
    # sigma has TWO summaries in this manuscript and they are DIFFERENT NUMBERS:
    #   sd_residual      posterior MEAN of sigma (brms' own summary). Table S3.
    #   sd_residual_rms  posterior ROOT-MEAN-SQUARE, sqrt(mean(sigma^2)).
    # The RMS is what the design analysis needs, because sigma enters the variance
    # budget in 22_design_analysis.R through sigma^2 (om2 = sigma^2 + y_se^2 +
    # b^2 x_se^2), where the second moment -- not the mean -- is the right
    # summary; SI Section S4 quotes it. Exporting both under distinct names means
    # neither can be quoted as the other, and 22_design_analysis.R cross-checks
    # the value it computes itself against the mean written here.
    sig_rms(fit, tag),
    tibble::tibble(calibration = tag, parameter = "n_districts",
                   estimate = as.numeric(if (!is.null(sm$nobs)) sm$nobs
                                         else nrow(fit$data)),
                   lo95 = NA_real_, hi95 = NA_real_))
}
calib_param_tbl <- dplyr::bind_rows(
  calib_params(bA, "NFHS-4 ~ ACCESS W1 (rural)"),
  calib_params(bB, "NFHS-5 ~ IRES (rural)"))
readr::write_csv(calib_param_tbl, file.path(dir_out, "calibration_parameters.csv"))
cat("\n== calibration parameters (written to calibration_parameters.csv) ==\n")
print(as.data.frame(calib_param_tbl), row.names = FALSE, digits = 4)
chk("05", "calibration parameter table written",
    file.exists(file.path(dir_out, "calibration_parameters.csv")) &&
      all(is.finite(calib_param_tbl$estimate)),
    sprintf("%d rows, %d fits", nrow(calib_param_tbl),
            dplyr::n_distinct(calib_param_tbl$calibration)))

# Sensitivity-fit calibration parameters go to their OWN file. Appending them to
# calibration_parameters.csv would change the row set that 22_design_analysis.R
# and the document builder read, and the sensitivity must not be able to leak
# into a primary number by accident.
calib_param_wt <- calib_params(bA_wt,
                               "NFHS-4 ~ ACCESS W1 design-weighted (rural, SENSITIVITY)")
readr::write_csv(calib_param_wt,
                 file.path(dir_out, "calibration_parameters_legA_wt.csv"))
cat("\n== leg-A symmetry sensitivity: calibration parameters ==\n")
print(as.data.frame(calib_param_wt), row.names = FALSE, digits = 4)
.gp <- function(tb, par) tb$estimate[tb$parameter == par][1]
cat(sprintf("   slope: primary %.3f -> sensitivity %.3f | sigma: %.3f -> %.3f\n",
            .gp(calib_param_tbl[calib_param_tbl$calibration ==
                                  "NFHS-4 ~ ACCESS W1 (rural)", ], "slope"),
            .gp(calib_param_wt, "slope"),
            .gp(calib_param_tbl[calib_param_tbl$calibration ==
                                  "NFHS-4 ~ ACCESS W1 (rural)", ], "sd_residual"),
            .gp(calib_param_wt, "sd_residual")))
chk("05", "leg-A symmetry sensitivity fit converged to finite parameters",
    all(is.finite(calib_param_wt$estimate)),
    sprintf("%d parameters written to calibration_parameters_legA_wt.csv",
            nrow(calib_param_wt)))
chk("05", "leg-A sensitivity uses the ACCESS design SE, not the binomial fallback",
    dgA_wt$n_design >= 0.9 * dgA_wt$n && dgA$n_design == 0,
    sprintf("sensitivity: design SE %d of %d districts (fallback %d); primary: design SE %d",
            dgA_wt$n_design, dgA_wt$n, dgA_wt$n_fallback, dgA$n_design))
chk_warn("05", "design SE exceeds the binomial SE it replaces (understatement confirmed)",
    sum(.legA_ok) > 0 &&
      median(dgA_wt$y[.legA_ok] / .legA_binom[.legA_ok], na.rm = TRUE) > 1,
    sprintf("median design/binomial logit-SE ratio %.2f over %d districts",
            median(dgA_wt$y[.legA_ok] / .legA_binom[.legA_ok], na.rm = TRUE),
            sum(.legA_ok)))

# The two sigma summaries side by side, so the difference is visible in the run
# log rather than surfacing as two numbers for one parameter in the manuscript.
.sig_get <- function(par) {
  x <- calib_param_tbl$estimate[calib_param_tbl$parameter == par]
  names(x) <- calib_param_tbl$calibration[calib_param_tbl$parameter == par]
  x
}
.sig_mean <- .sig_get("sd_residual"); .sig_rmsv <- .sig_get("sd_residual_rms")
.sig_pair <- data.frame(calibration = names(.sig_mean),
                        sd_residual = as.numeric(.sig_mean),
                        sd_residual_rms = as.numeric(.sig_rmsv[names(.sig_mean)]),
                        stringsAsFactors = FALSE)
cat("\n== residual SD (sigma): the two summaries this manuscript reports ==\n")
cat("   sd_residual     = posterior MEAN  -> Table S3\n")
cat("   sd_residual_rms = posterior RMS    -> SI Section S4 (enters as sigma^2)\n")
print(as.data.frame(.sig_pair), row.names = FALSE, digits = 4)
# RMS >= mean always (Jensen); a violation means the draws accessor grabbed the
# wrong parameter, and a wild ratio means the sigma posterior is far from tight.
chk("05", "sigma RMS recovered and >= posterior mean (Jensen)",
    all(is.finite(.sig_pair$sd_residual_rms)) &&
      all(.sig_pair$sd_residual_rms >= .sig_pair$sd_residual - 1e-8) &&
      all(.sig_pair$sd_residual_rms <= 2 * .sig_pair$sd_residual),
    paste(sprintf("%s: mean %.4f, rms %.4f", .sig_pair$calibration,
                  .sig_pair$sd_residual, .sig_pair$sd_residual_rms),
          collapse = "; "))

# ---- Posterior-corrected national estimates ---------------------------------
# For each NFHS district: draw corrected logit from the fitted calibration line
# (new states -> re_formula = NA uses population-level effects only;
#  allow_new_levels = TRUE would instead draw a random state effect).
correct_bayes <- function(fit, nfhs_df, est_col, se_col, out_prefix) {
  if (!"state_name" %in% names(nfhs_df)) {
    if ("state" %in% names(nfhs_df)) nfhs_df$state_name <- nfhs_df$state
    else stop("nfhs_df needs a state_name/state column to retain state effects.")
  }
  nd <- nfhs_df %>%
    filter(!is.na(.data[[est_col]])) %>%
    transmute(district, state_name = as.character(state_name),
              x_obs = qlogis(clamp(.data[[est_col]])),
              x_se  = pmin(pmax(.data[[se_col]] /
                       (clamp(.data[[est_col]]) * (1 - clamp(.data[[est_col]]))),
                       0.01), 2),
              # Placeholder for the response-side SE term in the model formula
              # (y | se(y_se, ...)): brms requires the variable to be present in
              # newdata, but posterior_epred returns the expected value of the
              # linear predictor, which does NOT depend on y_se, so any positive
              # constant gives the identical corrected estimate.
              y_se = 0.1)
  fitted_states <- unique(as.character(fit$data$state_name))
  in_support <- nd$state_name %in% fitted_states
  message(sprintf("  correct_bayes[%s]: %d of %d districts in calibration states ",
                  out_prefix, sum(in_support), nrow(nd)),
          "(estimated state effect applied); the rest drawn from the state-effect distribution.")
  # Retain the estimated state effect for calibration states; for unseen states
  # draw a Gaussian state effect (allow_new_levels + sample_new_levels), instead
  # of discarding all state structure with re_formula = NA.
  # Seed the RNG so the Gaussian draws for out-of-sample states are reproducible:
  # without this the corrected posterior (and every downstream Bayesian/MI health
  # estimate) jitters by ~0.001-0.005 run-to-run. Fixed seed => bit-reproducible.
  set.seed(20240714L)
  pp <- posterior_epred(fit, newdata = nd, re_formula = NULL,
                        allow_new_levels = TRUE, sample_new_levels = "gaussian")
  list(
    summary = nd %>%
      mutate(!!paste0(out_prefix, "_bayes")        := plogis(colMeans(pp)),
             !!paste0(out_prefix, "_bayes_lo")     := plogis(apply(pp, 2, quantile, .025)),
             !!paste0(out_prefix, "_bayes_hi")     := plogis(apply(pp, 2, quantile, .975)),
             !!paste0(out_prefix, "_in_support")   := in_support) %>%
      select(-x_obs, -x_se, -state_name),
    draws = plogis(pp),                 # (n_draws x n_districts), probability scale
    districts = nd$district
  )
}

corrA <- correct_bayes(bA, nfhs_corrected, "lpg_2015_rural_wt",
                       "lpg_2015_rural_wt_se", "lpg_2015")
corrB <- correct_bayes(bB, nfhs_corrected, "lpg_2019_rural_wt",
                       "lpg_2019_rural_wt_se", "lpg_2019")
# ---- Instrument-consistent sensitivity surface (health option 3) -------------
# The PRIMARY corrected change is era-matched (ACCESS calibrates 2015, IRES
# calibrates 2019), which is right for the LEVELS but means the corrected 2015->
# 2019 CHANGE switches calibration instrument across rounds. For a sensitivity
# that removes that switch, apply the SAME IRES-RURAL calibration (bB) to the
# NFHS-4 rural estimate as well, so both endpoints share one instrument. The 2019
# IRES-cal surface is identical to lpg_2019_bayes; only the 2015 endpoint differs.
# Rural throughout by design (ACCESS is rural; the main analysis is rural).
corrB15 <- correct_bayes(bB, nfhs_corrected, "lpg_2015_rural_wt",
                         "lpg_2015_rural_wt_se", "lpg_2015_irescal")

# ---- Leg-A symmetry sensitivity surface -------------------------------------
# Same NFHS-4 rural input, same estimand, but calibrated by bA_wt (design-weighted
# ACCESS reference + its Taylor design SE) instead of bA. Paired with the SAME
# 2019 surface downstream, so any movement in the corrected change is attributable
# to the leg-A reference construction alone.
corrA_wt <- correct_bayes(bA_wt, nfhs_corrected, "lpg_2015_rural_wt",
                          "lpg_2015_rural_wt_se", "lpg_2015_accesswt")

nfhs_corrected <- nfhs_corrected %>%
  left_join(corrA$summary,    by = "district") %>%
  left_join(corrB$summary,    by = "district") %>%
  left_join(corrB15$summary,  by = "district") %>%
  left_join(corrA_wt$summary, by = "district")

# District-level movement of the 2015 surface, written out so the manuscript
# sources it rather than restating it. The health-model movement (the number the
# reader actually cares about) is computed in H2_health_models.R.
legA_sens <- { d <- nfhs_corrected$lpg_2015_accesswt_bayes -
                    nfhs_corrected$lpg_2015_bayes
               ok <- is.finite(d)
               tibble::tibble(
                 quantity = c("n_districts", "mean_abs_diff_pp", "median_diff_pp",
                              "max_abs_diff_pp", "corr_primary_sens",
                              "mean_primary_pc", "mean_sens_pc"),
                 value = c(sum(ok),
                           round(100 * mean(abs(d[ok])), 3),
                           round(100 * median(d[ok]), 3),
                           round(100 * max(abs(d[ok])), 3),
                           round(cor(nfhs_corrected$lpg_2015_bayes[ok],
                                     nfhs_corrected$lpg_2015_accesswt_bayes[ok]), 4),
                           round(100 * mean(nfhs_corrected$lpg_2015_bayes[ok]), 3),
                           round(100 * mean(nfhs_corrected$lpg_2015_accesswt_bayes[ok]), 3))) }
readr::write_csv(legA_sens, file.path(dir_out, "legA_symmetry_sensitivity.csv"))
cat("\n== leg-A symmetry sensitivity: 2015 corrected surface ==\n")
print(as.data.frame(legA_sens), row.names = FALSE)
chk("05", "leg-A sensitivity 2015 surface present and in [0,1]",
    chk_has_cols(nfhs_corrected, c("lpg_2015_accesswt_bayes",
      "lpg_2015_accesswt_bayes_lo","lpg_2015_accesswt_bayes_hi")) &&
      chk_in_range(nfhs_corrected$lpg_2015_accesswt_bayes, 0, 1))
chk("05", "leg-A sensitivity tracks the primary 2015 surface (r > 0.9)",
    { v <- legA_sens$value[legA_sens$quantity == "corr_primary_sens"]
      is.finite(v) && v > 0.9 },
    sprintf("r = %.4f; mean |difference| = %.3f pp",
            legA_sens$value[legA_sens$quantity == "corr_primary_sens"],
            legA_sens$value[legA_sens$quantity == "mean_abs_diff_pp"]))
chk_file("05", "leg-A symmetry sensitivity summary written",
         "legA_symmetry_sensitivity.csv")

# ---- CALIBRATION SUPPORT FLAGS ----------------------------------------------
# The correction is estimated on a minority of districts (ACCESS: 6 rural
# northern states; IRES: 21 states) and then applied nationwide. Two distinct
# senses of "supported" matter, and they are NOT the same thing:
#
#   *_in_support        (set inside correct_bayes) -- the district's STATE was in
#                       the calibration sample, so an ESTIMATED state effect was
#                       applied. Elsewhere the state effect is a draw from the
#                       fitted between-state distribution: correct on average,
#                       but carrying no district- or state-specific information.
#
#   *_cov_support (here) -- the district's RAW NFHS prevalence lies inside the
#                       range of raw prevalences the calibration model actually
#                       saw. The calibration is a line fitted on the logit scale;
#                       applying it to a district far outside that range is
#                       EXTRAPOLATION, regardless of which state the district is
#                       in. With a single predictor, common support is simply the
#                       observed [min, max] of that predictor in the fit data.
#
# H2 stratifies on both (see the calibration-support sensitivity there), so the
# reader can see how much of the headline association comes from districts the
# correction was never estimated for.
cov_support <- function(x, calib_x) {
  lo <- min(calib_x, na.rm = TRUE); hi <- max(calib_x, na.rm = TRUE)
  is.finite(x) & x >= lo & x <= hi
}
supp_2015_rng <- range(pairA$lpg_2015_rural, na.rm = TRUE)
supp_2019_rng <- range(pairB$lpg_2019_rural, na.rm = TRUE)
nfhs_corrected <- nfhs_corrected %>%
  mutate(
    lpg_2015_cov_support = cov_support(lpg_2015_rural, pairA$lpg_2015_rural),
    lpg_2019_cov_support = cov_support(lpg_2019_rural, pairB$lpg_2019_rural),
    # A district is fully supported only if BOTH endpoints of the change are
    # inside the calibration range AND both rounds used an estimated state effect.
    lpg_change_full_support =
      lpg_2015_cov_support & lpg_2019_cov_support &
      coalesce(lpg_2015_in_support, FALSE) & coalesce(lpg_2019_in_support, FALSE))
message(sprintf(
  "\nCalibration support:\n  2015 calibration raw-prevalence range [%.3f, %.3f]; %d of %d districts inside (%.1f%%)",
  supp_2015_rng[1], supp_2015_rng[2], sum(nfhs_corrected$lpg_2015_cov_support, na.rm = TRUE),
  nrow(nfhs_corrected), 100 * mean(nfhs_corrected$lpg_2015_cov_support, na.rm = TRUE)))
message(sprintf(
  "  2019 calibration raw-prevalence range [%.3f, %.3f]; %d of %d districts inside (%.1f%%)",
  supp_2019_rng[1], supp_2019_rng[2], sum(nfhs_corrected$lpg_2019_cov_support, na.rm = TRUE),
  nrow(nfhs_corrected), 100 * mean(nfhs_corrected$lpg_2019_cov_support, na.rm = TRUE)))
message(sprintf(
  "  districts in a calibration STATE: 2015 %d (%.1f%%) | 2019 %d (%.1f%%)",
  sum(nfhs_corrected$lpg_2015_in_support, na.rm = TRUE),
  100 * mean(nfhs_corrected$lpg_2015_in_support, na.rm = TRUE),
  sum(nfhs_corrected$lpg_2019_in_support, na.rm = TRUE),
  100 * mean(nfhs_corrected$lpg_2019_in_support, na.rm = TRUE)))
message(sprintf(
  "  FULLY supported for the 2015->2019 CHANGE (both ranges + both states): %d (%.1f%%)",
  sum(nfhs_corrected$lpg_change_full_support, na.rm = TRUE),
  100 * mean(nfhs_corrected$lpg_change_full_support, na.rm = TRUE)))

support_tbl <- nfhs_corrected %>%
  summarise(n_districts = n(),
            n_cov_support_2015 = sum(lpg_2015_cov_support, na.rm = TRUE),
            n_cov_support_2019 = sum(lpg_2019_cov_support, na.rm = TRUE),
            n_state_support_2015 = sum(lpg_2015_in_support, na.rm = TRUE),
            n_state_support_2019 = sum(lpg_2019_in_support, na.rm = TRUE),
            n_full_support_change = sum(lpg_change_full_support, na.rm = TRUE),
            calib_range_2015_lo = supp_2015_rng[1], calib_range_2015_hi = supp_2015_rng[2],
            calib_range_2019_lo = supp_2019_rng[1], calib_range_2019_hi = supp_2019_rng[2])
write_csv(support_tbl, file.path(dir_out, "calibration_support_summary.csv"))

saveRDS(nfhs_corrected, file.path(dir_out, "corrected_nfhs_districts.rds"))
# Actual posterior draws of the corrected prevalence, per district and year, so
# downstream health models propagate the true posterior (asymmetric on the
# probability scale, district-structured) rather than a normal-from-CrI approx.
# y2015_irescal: the 2015 surface under the IRES-rural calibration, for the
# instrument-consistent MI sensitivity in H2 (paired with y2019).
# y2015_accesswt: the 2015 surface under the leg-A symmetry sensitivity
# (design-weighted ACCESS reference + its design SE), for the matching MI
# sensitivity in H2 (also paired with y2019).
saveRDS(list(y2015 = corrA$draws, districts_2015 = corrA$districts,
             y2019 = corrB$draws, districts_2019 = corrB$districts,
             y2015_irescal = corrB15$draws,
             districts_2015_irescal = corrB15$districts,
             y2015_accesswt = corrA_wt$draws,
             districts_2015_accesswt = corrA_wt$districts),
        file.path(dir_out, "correction_posterior_draws.rds"))
saveRDS(list(calA = calA, calB = calB, calA_nat = calA_nat, calB_nat = calB_nat),
        file.path(dir_out, "calibration_models.rds"))

# ---- Diagnostics: corrected vs raw, and in-sample recovery -------------------
p1 <- ggplot(nfhs_corrected, aes(lpg_2019_rural, lpg_2019_rc)) +
  geom_abline(linetype = 2, color = "grey40") +
  geom_point(alpha = .5) +
  geom_point(aes(y = lpg_2019_bayes), color = "steelblue", alpha = .5) +
  labs(x = "NFHS-5 rural (raw)", y = "Corrected (black = RC, blue = Bayes)") +
  theme_bw()
ggsave(file.path(dir_out, "corrected_vs_raw_2019.jpeg"), p1,
       width = 7, height = 6, dpi = 300)

# ---- Leave-one-state-out transportability -----------------------------------
# Does the calibration transport to a state it never saw? Hold out each state in
# turn, refit on the rest, and compare the held-out RMSE against the uncorrected
# NFHS RMSE. This is the single most important external-validity diagnostic in
# the paper: the correction is only useful in a non-calibration state if it beats
# doing nothing THERE, not merely in-sample.
loso <- map_dfr(unique(pairB$state_name), function(s) {
  tr <- pairB %>% filter(state_name != s)
  te <- pairB %>% filter(state_name == s)
  m  <- lm(qlogis(clamp(ires_mainlpg_rural)) ~ qlogis(clamp(lpg_2019_rural)),
           data = tr, weights = tr$n_ires_rural)
  te %>% mutate(pred = plogis(predict(m, newdata = te)),
                state_left_out = s) %>%
    summarise(state_left_out = s,
              n_districts = sum(is.finite(lpg_2019_rural) & is.finite(ires_mainlpg_rural)),
              n_ires = sum(n_ires_rural, na.rm = TRUE),
              rmse_raw  = sqrt(mean((lpg_2019_rural - ires_mainlpg_rural)^2, na.rm = TRUE)),
              rmse_corr = sqrt(mean((pred - ires_mainlpg_rural)^2, na.rm = TRUE)))
}) %>%
  mutate(improvement    = rmse_raw - rmse_corr,
         improvement_pc = 100 * (rmse_raw - rmse_corr) / rmse_raw,
         improved       = is.finite(improvement) & improvement > 0)
write_csv(loso, file.path(dir_out, "calibration_loso.csv"))
print(as.data.frame(loso), row.names = FALSE, digits = 3)

# --- LOSO summary, including an explicit account of any non-finite rows -------
# A state contributes NaN when it has too few overlapping districts for an RMSE
# to be defined (a single district gives a degenerate 1-observation RMSE; zero
# overlapping districts gives NaN outright). Delhi is the recurring case: it is
# almost entirely urban, so the RURAL overlap is one district or none. Rather
# than dropping such rows silently, they are named and excluded from the summary.
loso_bad <- loso %>% filter(!is.finite(rmse_raw) | !is.finite(rmse_corr))
loso_ok  <- loso %>% filter(is.finite(rmse_raw),  is.finite(rmse_corr))
if (nrow(loso_bad)) {
  message("\nLOSO states with an undefined RMSE (excluded from the summary):")
  message(paste(sprintf("  %s: %d overlapping rural districts, n_IRES = %.0f",
                        loso_bad$state_left_out, loso_bad$n_districts, loso_bad$n_ires),
                collapse = "\n"))
  message("  (a state needs at least one district with BOTH a rural NFHS and a rural IRES ",
          "estimate; predominantly urban states such as Delhi may have none.)")
}
loso_summary <- tibble(
  n_states_tested       = nrow(loso_ok),
  n_states_undefined    = nrow(loso_bad),
  n_improved            = sum(loso_ok$improved),
  pct_improved          = 100 * mean(loso_ok$improved),
  # District-count-weighted RMSE: the unweighted mean over states lets a
  # 2-district state count as much as a 40-district one.
  rmse_raw_wtd          = sqrt(weighted.mean(loso_ok$rmse_raw^2,  loso_ok$n_districts)),
  rmse_corr_wtd         = sqrt(weighted.mean(loso_ok$rmse_corr^2, loso_ok$n_districts)),
  rmse_raw_unwtd        = mean(loso_ok$rmse_raw),
  rmse_corr_unwtd       = mean(loso_ok$rmse_corr),
  worst_deterioration   = min(loso_ok$improvement),
  worst_state           = loso_ok$state_left_out[which.min(loso_ok$improvement)],
  best_improvement      = max(loso_ok$improvement),
  best_state            = loso_ok$state_left_out[which.max(loso_ok$improvement)]) %>%
  mutate(rmse_reduction_wtd_pc = 100 * (rmse_raw_wtd - rmse_corr_wtd) / rmse_raw_wtd)
write_csv(loso_summary, file.path(dir_out, "calibration_loso_summary.csv"))
cat("\n== Leave-one-state-out transportability summary ==\n")
message(sprintf("  states tested                 : %d (%d undefined, excluded)",
                loso_summary$n_states_tested, loso_summary$n_states_undefined))
message(sprintf("  states where correction HELPED: %d of %d (%.0f%%)",
                loso_summary$n_improved, loso_summary$n_states_tested,
                loso_summary$pct_improved))
message(sprintf("  district-weighted RMSE        : raw %.4f -> corrected %.4f (%.1f%% reduction)",
                loso_summary$rmse_raw_wtd, loso_summary$rmse_corr_wtd,
                loso_summary$rmse_reduction_wtd_pc))
message(sprintf("  unweighted mean RMSE          : raw %.4f -> corrected %.4f",
                loso_summary$rmse_raw_unwtd, loso_summary$rmse_corr_unwtd))
message(sprintf("  best  : %s (RMSE falls %.4f)", loso_summary$best_state,
                loso_summary$best_improvement))
message(sprintf("  worst : %s (RMSE changes %+.4f)", loso_summary$worst_state,
                loso_summary$worst_deterioration))

## ---- CHECKS ------------------------------------------------------------------
chk_header("05_correction")
# ---- Sampler diagnostics -----------------------------------------------------
# Until now nothing in the check trail looked at the Stan fit itself: the mixed
# -model registry covers lme4 fits only, so divergences, Rhat and ESS were
# visible in the console and nowhere else. The primary estimate of this paper is
# a posterior summary from these two fits, so their convergence is a first-order
# fact about the result and belongs in the same ledger as everything else.
bayes_diag <- function(fit, tag) {
  ndiv <- tryCatch({
    np <- brms::nuts_params(fit)
    sum(np$Value[np$Parameter == "divergent__"], na.rm = TRUE)
  }, error = function(e) NA_real_)
  # posterior:: is a hard dependency of brms, so this needs no extra install.
  # summarise_draws() is used rather than brms::rhat()/neff_ratio(), which are
  # deprecated in current brms and would emit their own warnings.
  sm <- tryCatch(
    posterior::summarise_draws(posterior::as_draws_df(fit),
                               "rhat", "ess_bulk", "ess_tail"),
    error = function(e) NULL)
  gv <- function(col, f) {
    if (is.null(sm) || !col %in% names(sm)) return(NA_real_)
    v <- sm[[col]][is.finite(sm[[col]])]
    if (!length(v)) NA_real_ else as.numeric(f(v))
  }
  tibble::tibble(
    fit = tag,
    divergences  = as.numeric(ndiv),
    max_rhat     = gv("rhat", max),
    min_ess_bulk = gv("ess_bulk", min),
    min_ess_tail = gv("ess_tail", min))
}
bayes_tbl <- dplyr::bind_rows(bayes_diag(bA, "nfhs4_access"),
                              bayes_diag(bB, "nfhs5_ires"),
                              bayes_diag(bA_wt, "nfhs4_access_wt"))
dir.create(file.path(dir_out, "diagnostics"), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(bayes_tbl, file.path(dir_out, "diagnostics", "bayes_fit_diagnostics.csv"))
cat("\n== brms sampler diagnostics ==\n")
print(as.data.frame(bayes_tbl), row.names = FALSE, digits = 4)

.bd <- function(col) paste(sprintf("%s = %s", bayes_tbl$fit,
                                   format(bayes_tbl[[col]], digits = 4)),
                           collapse = " | ")
chk("05", "no divergent transitions in any calibration fit",
    all(is.finite(bayes_tbl$divergences)) && all(bayes_tbl$divergences == 0),
    .bd("divergences"))
chk("05", "max Rhat < 1.01 in all calibration fits",
    all(is.finite(bayes_tbl$max_rhat)) && all(bayes_tbl$max_rhat < 1.01),
    .bd("max_rhat"))
chk("05", "min bulk ESS > 400 in all calibration fits",
    all(is.finite(bayes_tbl$min_ess_bulk)) && all(bayes_tbl$min_ess_bulk > 400),
    .bd("min_ess_bulk"))
chk("05", "min tail ESS > 400 in all calibration fits",
    all(is.finite(bayes_tbl$min_ess_tail)) && all(bayes_tbl$min_ess_tail > 400),
    .bd("min_ess_tail"))

chk("05", "corrected surfaces present (raw/RC/Bayes, both rounds)",
    chk_has_cols(nfhs_corrected, c("lpg_2015_rural","lpg_2019_rural",
      "lpg_2015_rc","lpg_2019_rc","lpg_2015_bayes","lpg_2019_bayes",
      "lpg_2015_bayes_lo","lpg_2019_bayes_hi")))
chk("05", "instrument-consistent 2015 surface present (IRES cal, option 3)",
    chk_has_cols(nfhs_corrected, c("lpg_2015_irescal_bayes",
      "lpg_2015_irescal_bayes_lo","lpg_2015_irescal_bayes_hi")))
chk("05", "all corrected prevalences in [0,1]",
    chk_in_range(nfhs_corrected$lpg_2019_bayes, 0, 1) &&
    chk_in_range(nfhs_corrected$lpg_2015_bayes, 0, 1) &&
    chk_in_range(nfhs_corrected$lpg_2015_irescal_bayes, 0, 1))
chk("05", "posterior draws file written with IRES-cal and leg-A-sensitivity 2015 draws",
    { p <- file.path(dir_out, "correction_posterior_draws.rds")
      file.exists(p) && all(c("y2015","y2019","y2015_irescal","y2015_accesswt") %in%
                            names(readRDS(p))) })
chk("05", "correction raises NFHS-5 LPG (IRES > NFHS direction)",
    mean(nfhs_corrected$lpg_2019_bayes, na.rm = TRUE) >
      mean(nfhs_corrected$lpg_2019_rural, na.rm = TRUE),
    sprintf("mean raw %.3f -> Bayes %.3f",
            mean(nfhs_corrected$lpg_2019_rural, na.rm = TRUE),
            mean(nfhs_corrected$lpg_2019_bayes, na.rm = TRUE)))
chk("05", "calibration-support flags present (state support + covariate overlap)",
    chk_has_cols(nfhs_corrected, c("lpg_2015_in_support","lpg_2019_in_support",
      "lpg_2015_cov_support","lpg_2019_cov_support","lpg_change_full_support")))
chk_file("05", "calibration support summary written", "calibration_support_summary.csv")
chk_file("05", "LOSO transportability summary written", "calibration_loso_summary.csv")
chk("05", "every calibration district is inside its own covariate support",
    { a <- cov_support(pairA$lpg_2015_rural, pairA$lpg_2015_rural)
      b <- cov_support(pairB$lpg_2019_rural, pairB$lpg_2019_rural)
      all(a[is.finite(pairA$lpg_2015_rural)]) && all(b[is.finite(pairB$lpg_2019_rural)]) })
# TRANSPORTABILITY, reported as a WARN and not suppressed. If the correction does
# not beat the raw NFHS in held-out states, it cannot be claimed to generalize
# beyond the calibration states, and the manuscript must say so plainly.
chk_warn("05", "calibration transports out of sample (weighted RMSE falls in LOSO)",
    is.finite(loso_summary$rmse_reduction_wtd_pc) &&
      loso_summary$rmse_reduction_wtd_pc > 0,
    sprintf("district-weighted RMSE raw %.4f -> corrected %.4f (%+.1f%%); helped in %d of %d states",
            loso_summary$rmse_raw_wtd, loso_summary$rmse_corr_wtd,
            loso_summary$rmse_reduction_wtd_pc, loso_summary$n_improved,
            loso_summary$n_states_tested))
chk_warn("05", "correction helps in a majority of held-out states",
    loso_summary$pct_improved > 50,
    sprintf("%.0f%% of held-out states improved", loso_summary$pct_improved))
chk_warn("05", "most districts lie inside the calibration covariate range",
    mean(nfhs_corrected$lpg_2019_cov_support, na.rm = TRUE) > 0.9,
    sprintf("%.1f%% of districts inside the 2019 calibration range [%.3f, %.3f]",
            100 * mean(nfhs_corrected$lpg_2019_cov_support, na.rm = TRUE),
            supp_2019_rng[1], supp_2019_rng[2]))
chk_warn("05", "ACCESS-cal vs IRES-cal 2015 differ (instrument switch is real)",
    { d <- abs(nfhs_corrected$lpg_2015_bayes - nfhs_corrected$lpg_2015_irescal_bayes)
      mean(d, na.rm = TRUE) > 1e-4 },
    sprintf("mean |2015 ACCESS-cal - IRES-cal| = %.3f",
            mean(abs(nfhs_corrected$lpg_2015_bayes -
                     nfhs_corrected$lpg_2015_irescal_bayes), na.rm = TRUE)))

message("05_correction.R done.")
