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

# Logit-scale standard-error bounds (see prep_me() for the rationale).
# SE_FLOOR only guarantees positivity for brms' se() term and is deliberately
# far below anything observed; SE_CAP bounds the one saturated district whose
# reference prevalence is clamped at 1 - eps and whose binomial logit SE is
# therefore enormous. Both are reported in me_se_diagnostics.csv.
SE_FLOOR <- 1e-4
SE_CAP   <- 2

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

# WHY THESE ARE FITTED BUT NOT APPLIED. calA/calB include state fixed effects;
# the national correction below (calA_nat/calB_nat) deliberately does not.
# A state fixed effect is estimable only for states the reference survey
# sampled, so a with-state calibration cannot be evaluated in the states that
# most need correcting -- it would produce no prediction outside the 6 (ACCESS)
# and 19-21 (IRES) calibration states. The with-state fits are therefore a
# WITHIN-SAMPLE ROBUSTNESS CHECK on the slope: if adding state effects moved
# the slope materially, the pooled national slope would be confounded by state
# composition. They were previously fitted, printed, and then left unused,
# which reads as dead code; their slopes are now written out so the check is
# part of the record. (The Bayesian model handles the same issue properly, with
# state as a random effect that shrinks to the national mean out of sample.)
calib_state_check <- tibble::tibble(
  leg          = c("2015 (NFHS-4 ~ ACCESS)", "2019 (NFHS-5 ~ IRES)"),
  slope_state  = c(unname(coef(calA)["x"]), unname(coef(calB)["x"])),
  se_state     = c(sqrt(diag(vcov(calA)))["x"], sqrt(diag(vcov(calB)))["x"]),
  n_states     = c(dplyr::n_distinct(pairA$state_name),
                   dplyr::n_distinct(pairB$state_name)),
  adj_r2_state = c(summary(calA)$adj.r.squared, summary(calB)$adj.r.squared))
readr::write_csv(calib_state_check,
                 file.path(dir_out, "calibration_state_effect_check.csv"))

# Apply nationally. State effects only exist for calibration states, so for
# out-of-sample states we apply the average calibration (state effect = 0 on
# the mean-centered contrast). Simplest robust route: refit without state for
# the national application, keep the state version for within-sample checks
# (calib_state_check above; the slopes are compared in the CHECKS block).
# This no-state model IS the regression calibration reported in the paper.
calA_nat <- lm(qlogis(clamp(access_w1_mainlpg)) ~ qlogis(clamp(lpg_2015_rural)),
               data = pairA, weights = pairA$n_access_w1)
calB_nat <- lm(qlogis(clamp(ires_mainlpg_rural)) ~ qlogis(clamp(lpg_2019_rural)),
               data = pairB, weights = pairB$n_ires_rural)
# Record the applied (no-state) slope beside the with-state slope, so the
# within-sample check reported in SI Methods S1 reads both from one file.
calib_state_check$slope_applied <- c(unname(coef(calA_nat)[2]), unname(coef(calB_nat)[2]))
readr::write_csv(calib_state_check,
                 file.path(dir_out, "calibration_state_effect_check.csv"))

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
    # A SATURATED district (every sampled household reports LPG, or none does)
    # has a design SE that is zero in exact arithmetic but can surface as
    # floating-point dust (e.g. 1.6e-16 for Rangareddy, IRES rural n = 17,
    # p = 1). `> 0` accepted that dust, and the district then entered the
    # measurement-error model at the clamped prevalence with an SE at the
    # floor -- an essentially exact observation at logit(0.999) = 6.9, a
    # leverage point that moved the 2019 calibration slope. Such a district
    # carries no usable design SE: route it to the binomial fallback (which the
    # SE_CAP then bounds), as the manuscript's SE-bounds paragraph describes.
    saturated <- df[[ref]] <= 0 | df[[ref]] >= 1
    y_se <- ifelse(is.finite(y_se_design) & y_se_design > 1e-6 & !saturated,
                   y_se_design, y_se_binom)
  } else {
    y_se <- y_se_binom
  }
  df$p_ref <- p_ref
  df$y     <- qlogis(p_ref)
  # BOUNDS. brms' se() term requires a strictly positive standard error, so a
  # floor is needed only to keep a degenerate district (zero estimated design
  # SE) from producing se = 0. Earlier versions used 0.02 / 0.01, which a
  # reviewer rightly flagged as large enough to be doing real work. They were
  # not -- the smallest observed values are ~0.16 (reference) and ~0.09 (NFHS),
  # eight to nine times those floors -- but a bound that is meant to be inert
  # should be visibly inert, so the floors are now set to SE_FLOOR = 1e-4 and
  # the number of districts touching either bound is written to
  # me_se_diagnostics.csv on every run. The upper bound of 2 logits is retained
  # and does bind, for exactly one district (see SE_CAP note below).
  df$y_se  <- pmin(pmax(y_se, SE_FLOOR), SE_CAP)
  df$x_obs <- qlogis(clamp(df[[nfhs_est]]))
  df$x_se  <- pmin(pmax(df[[nfhs_se]] /
                (clamp(df[[nfhs_est]]) * (1 - clamp(df[[nfhs_est]]))),
                SE_FLOOR), SE_CAP)
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

# ---- sampler settings, in one place, so they can be VERIFIED after loading ---
# file_refit = "on_change" compares the Stan code, the Stan data and the
# algorithm. It does NOT compare iter, chains or control, so raising iter or
# tightening adapt_delta silently reloads the old cached fit and reproduces the
# old diagnostics -- a stale result that no downstream check could detect,
# because every number in it is internally consistent. Naming the settings once
# here lets assert_sampler() below compare them against what the loaded fit
# actually used, turning that silent staleness into a hard stop.
## iter 24000 (was 12000), October 2026. The household-level re-run of
## 2026-10-19 left the NFHS-5 ~ IRES fit at max Rhat 1.011 against the 1.01
## threshold (bulk ESS 575, tail 1178, no divergences) -- marginal, and the same
## fit that needed doubling in August. The rule this script already follows is
## more draws, not a relaxed threshold, so the warmup and the post-warmup draws
## both double (48,000 post-warmup draws over 4 chains).
SAMPLER <- list(chains = 4L, iter = 24000L,
                control = list(adapt_delta = 0.999, max_treedepth = 15L))

assert_sampler <- function(m, file_tag) {
  got <- tryCatch({
    sf  <- m$fit
    ctl <- sf@stan_args[[1]]$control
    list(chains        = length(sf@stan_args),
         iter          = sf@stan_args[[1]]$iter,
         adapt_delta   = ctl$adapt_delta,
         max_treedepth = ctl$max_treedepth)
  }, error = function(e) NULL)

  # Only rstan-backed fits expose stan_args this way. If the backend changes,
  # say so rather than silently asserting nothing.
  if (is.null(got) || is.null(got$iter)) {
    chk_warn("05", sprintf("cached fit's sampler settings verifiable (%s)", file_tag),
             FALSE,
             paste0("could not read stan_args from the fitted object, so a stale ",
                    "cached fit cannot be ruled out here; delete ",
                    "brms_me_", file_tag, ".rds if the sampler settings changed"))
    return(invisible(NULL))
  }

  num_eq <- function(a, b) is.numeric(a) && is.numeric(b) && isTRUE(all.equal(a, b))
  mism <- c(
    if (!identical(as.integer(got$chains), as.integer(SAMPLER$chains)))
      sprintf("chains %s (cached) vs %s (requested)", got$chains, SAMPLER$chains),
    if (!identical(as.integer(got$iter), as.integer(SAMPLER$iter)))
      sprintf("iter %s vs %s", got$iter, SAMPLER$iter),
    if (!num_eq(got$adapt_delta, SAMPLER$control$adapt_delta))
      sprintf("adapt_delta %s vs %s", format(got$adapt_delta),
              format(SAMPLER$control$adapt_delta)),
    if (!identical(as.integer(got$max_treedepth),
                   as.integer(SAMPLER$control$max_treedepth)))
      sprintf("max_treedepth %s vs %s", got$max_treedepth,
              SAMPLER$control$max_treedepth))

  if (length(mism))
    stop("\n[05] The cached brms fit '", file_tag, "' was drawn with DIFFERENT ",
         "sampler settings than this script now requests:\n",
         paste0("    ", mism, collapse = "\n"), "\n",
         "  file_refit = \"on_change\" does not compare sampler arguments, so it ",
         "reloaded the old fit.\n",
         "  Every diagnostic and credible interval below would describe that old ",
         "run, not this one.\n",
         "  Delete the cache and re-fit:\n",
         "      rm ", file.path(dir_out, paste0("brms_me_", file_tag, ".rds")), "\n",
         "  or clear all three at once:\n",
         "      rm ", file.path(dir_out, "brms_me_*.rds"), "\n",
         "  (an ad-hoc rerun_mcmc_only.sh used to be quoted here; it carried a\n",
         "   hard-coded path to one machine and now lives in scripts/_to_delete/.)",
         call. = FALSE)

  chk("05", sprintf("cached fit matches the requested sampler settings (%s)", file_tag),
      TRUE, sprintf("chains %d, iter %d, adapt_delta %s, max_treedepth %d",
                    as.integer(got$chains), as.integer(got$iter),
                    format(got$adapt_delta), as.integer(got$max_treedepth)))
  invisible(NULL)
}

fit_me <- function(dat, file_tag) {
  m <- brm(bform, data = dat, family = gaussian(),
      prior = c(prior(normal(0, 2), class = "b"),
                prior(normal(0, 2), class = "Intercept"),
                prior(exponential(1), class = "sd"),
                prior(exponential(1), class = "sigma")),
      # iter = SAMPLER$iter (24000; see the SAMPLER note above). Raised from 6000
      # to 12000 after the 2026-08-01 run left the NFHS-5 ~ IRES fit
      # under-mixed: max Rhat 1.020 against the 1.01 threshold below, and
      # minimum bulk ESS 304 against 400. Neither is near the conventional
      # danger line (Rhat > 1.05, ESS < 100), but low tail ESS degrades exactly
      # the quantity the paper reports -- the 95% CrI endpoints of the
      # calibration slope -- so the honest fix is more draws rather than a
      # relaxed threshold. ESS scales roughly linearly in draws, so doubling
      # should clear 400 with margin.
      chains = SAMPLER$chains, cores = 4, iter = SAMPLER$iter, seed = 1234,
      # adapt_delta 0.999 / max_treedepth 15 (was 0.995 / 15, and 0.99 / 12
      # before that). The NFHS-4 ~ ACCESS fit still produced ONE divergent
      # transition after warmup at 0.995. A single divergence in 24,000 draws
      # (the post-warmup total at iter 12000)
      # is unlikely to move the posterior materially, but it signals the
      # sampler grazed a region it could not resolve, and doubling the draws
      # would otherwise be expected to double the divergence count. Smaller
      # steps cost only run time. If divergences persist at 0.999 the problem
      # is the geometry of the me() funnel, not the tuning, and the check below
      # will say so rather than letting it pass silently as it did before.
      control = SAMPLER$control,
      file = file.path(dir_out, paste0("brms_me_", file_tag)),
      # file_refit = "on_change" compares the Stan code, the Stan data and the
      # algorithm, but NOT iter, chains or control. assert_sampler() below
      # closes that gap: it reads the settings back off the loaded fit and
      # stops the run if they disagree with SAMPLER above, so a stale cache is
      # a hard failure with the rm command in the message rather than a silent
      # wrong answer. `rm <dir_out>/brms_me_*.rds` clears all three.
      file_refit = "on_change")   # refit automatically if formula/data change
  assert_sampler(m, file_tag)
  m
}

# ---- sampler settings to disk, so the manuscript stops PINNING them ----------
# Section 2.4.3 states the chain count and the post-warmup draw total. Those
# were typed into build_manuscript.js and announced on every build as a pin,
# because they are arguments here rather than values in any output file. That is
# one assertion the number audit cannot make. Writing them out turns the last
# avoidable pin into a derived value: brms defaults warmup to iter/2, so the
# post-warmup total is chains * iter/2, computed here rather than by a reader.
readr::write_csv(
  tibble::tibble(
    quantity = c("chains", "iter_total_per_chain", "warmup_per_chain",
                 "post_warmup_draws_total", "adapt_delta", "max_treedepth", "seed"),
    value = c(SAMPLER$chains, SAMPLER$iter, SAMPLER$iter / 2,
              SAMPLER$chains * SAMPLER$iter / 2,
              SAMPLER$control$adapt_delta, SAMPLER$control$max_treedepth, 1234)),
  file.path(dir_out, "calibration_sampler.csv"))
chk("05", "sampler settings exported for the manuscript",
    TRUE, sprintf("%d chains x %d iter -> %d post-warmup draws",
                  SAMPLER$chains, SAMPLER$iter,
                  SAMPLER$chains * SAMPLER$iter / 2))

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
    ok  <- is.finite(dsn) & dsn > 1e-6 & !(d[[ref]] <= 0 | d[[ref]] >= 1)   # as in prep_me()
    y   <- ifelse(ok, dsn, binom)
  } else {
    ok <- rep(FALSE, nrow(d)); y <- binom
  }
  xs <- d[[nfhs_se]] / (clamp(d[[nfhs_est]]) * (1 - clamp(d[[nfhs_est]])))
  list(n = nrow(d), n_design = sum(ok), n_fallback = sum(!ok),
       y = y, x = xs, n_ref = d[[ref_n]],
       # the district(s) whose raw reference SE exceeds the upper bound
       clamped_hi = y[y > SE_CAP], n_ref_at_bound = d[[ref_n]][d[[ref]] > 1 - 1e-3],
       # how often each bound actually binds, so "the bounds are inert" is a
       # measured statement rather than an assurance
       n_y_at_floor = sum(y  <= SE_FLOOR), n_x_at_floor = sum(xs <= SE_FLOOR),
       n_x_at_cap   = sum(xs >= SE_CAP),
       y_min = min(y), x_min = min(xs),
       # districts whose prevalence itself is pinned by the eps clamp
       n_p_clamped = sum(d[[ref]] <= 1e-3 | d[[ref]] >= 1 - 1e-3))
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
               "n_clamped_hi",
               "se_floor", "se_cap",
               "n_y_at_floor", "n_x_at_floor", "n_x_at_cap",
               "y_se_min", "x_se_min", "n_prevalence_clamped"),
  value = c(dgA$n, dgB$n, dgB$n_design, dgB$n_fallback,
            if (length(dgB$n_ref_at_bound)) min(dgB$n_ref_at_bound) else NA_real_,
            round(if (length(dgB$clamped_hi)) max(dgB$clamped_hi) else NA_real_, 2),
            round(min(y_unclamped), 2), round(max(y_unclamped), 2),
            round(min(x_all), 2), round(max(x_all), 2),
            stats::median(dgB$n_ref), min(dgB$n_ref), max(dgB$n_ref),
            length(dgA$clamped_hi) + length(dgB$clamped_hi),
            SE_FLOOR, SE_CAP,
            dgA$n_y_at_floor + dgB$n_y_at_floor,
            dgA$n_x_at_floor + dgB$n_x_at_floor,
            dgA$n_x_at_cap   + dgB$n_x_at_cap,
            round(min(dgA$y_min, dgB$y_min), 3),
            round(min(dgA$x_min, dgB$x_min), 3),
            dgA$n_p_clamped + dgB$n_p_clamped)
)
readr::write_csv(me_se_diag, file.path(dir_out, "me_se_diagnostics.csv"))
message("me_se_diagnostics.csv written:")
print(as.data.frame(me_se_diag))
chk("05", "SE floors are inert (no district sits on them)",
    (dgA$n_y_at_floor + dgB$n_y_at_floor + dgA$n_x_at_floor + dgB$n_x_at_floor) == 0,
    sprintf("min reference logit SE %.3f, min NFHS logit SE %.3f, floor %.0e",
            min(dgA$y_min, dgB$y_min), min(dgA$x_min, dgB$x_min), SE_FLOOR))
chk_warn("05", "state-effect calibration slope agrees with the applied no-state slope",
    { d <- abs(calib_state_check$slope_state -
               c(unname(coef(calA_nat)[2]), unname(coef(calB_nat)[2])))
      all(is.finite(d)) && max(d) < 0.25 },
    { d <- calib_state_check$slope_state -
           c(unname(coef(calA_nat)[2]), unname(coef(calB_nat)[2]))
      sprintf("slope difference (with-state minus applied): %s",
              paste(sprintf("%+.3f", d), collapse = ", ")) })
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
# SHRINKAGE OF THE NFHS INPUT. For newdata, brms draws the noise-free covariate
# of an me() term as rnorm(x_obs, x_se): it plugs in the district's raw NFHS
# estimate and adds noise of the size of its sampling SE, i.e. it does NOT use
# the posterior of the district's true value given its noisy estimate. The
# calibration relation, however, is a relation in the TRUE NFHS value, so the
# corrected prevalence must be predicted from E[X | x_obs] with its posterior
# SD. We therefore pass brms the posterior moments of X instead of (x_obs, x_se):
# an empirical-Bayes normal-normal model fitted to ALL NFHS districts of the
# round (not only the calibration districts, whose x distribution need not
# represent the nation), with district logits centred on their state mean,
#   x_d ~ N(X_d, se_d^2),  X_d ~ N(mu_state, tau^2),
#   lambda_d = tau^2 / (tau^2 + se_d^2),
#   X_d | x_d ~ N(mu_state + lambda_d (x_d - mu_state), lambda_d * se_d^2).
# X is then drawn from exactly that posterior (see predict_surfaces). tau^2 is the DerSimonian-Laird
# moment estimate pooled within states; mu_state is the precision-weighted state
# mean (states with fewer than 3 districts pooled into one group). The
# NFHS-side variance component isolated by 22_design_analysis.R is then
# b^2 * lambda_d * se_d^2. The shrinkage moments are written to
# me_shrinkage.csv.
eb_shrink <- function(x, se, state) {
  w  <- 1 / se^2
  st <- as.character(state); st[is.na(st)] <- ".none"
  n_st <- table(st)
  # States with fewer than 3 districts are pooled into one group with a common mean.
  g  <- ifelse(n_st[st] >= 3, st, ".small")
  mu_g <- tapply(w * x, g, sum) / tapply(w, g, sum)
  mu <- as.numeric(mu_g[g])
  # DerSimonian-Laird between-district variance around the group means
  Q   <- sum(w * (x - mu)^2)
  dfq <- length(x) - length(unique(g))
  C   <- sum(w) - sum(tapply(w^2, g, sum) / tapply(w, g, sum))
  tau2 <- max(0, (Q - dfq) / C)
  lam <- tau2 / (tau2 + se^2)
  list(mean = as.numeric(mu + lam * (x - mu)), sd = as.numeric(sqrt(lam) * se),
       lambda = as.numeric(lam), mu = mu, tau2 = tau2)
}
# JOINT SHRINKAGE ACROSS ROUNDS. The health analysis uses the CHANGE between
# rounds, and a district's true NFHS values in 2015 and 2019 are strongly
# correlated (a district that cooked with LPG in 2015 still did in 2019), while
# the two rounds' sampling errors are independent (different samples). Shrinking
# each round separately ignores that correlation and gives the wrong posterior
# for the change: the change is much less reliable than either level, so it
# should be shrunk much more. The two rounds are therefore shrunk jointly, with
# a bivariate normal prior around the state means,
#   (X15, X19) ~ N((mu15, mu19), Sigma),  Sigma = [tau15^2, c; c, tau19^2],
#   x_t ~ N(X_t, se_t^2) independently,
# so (X15, X19) | (x15, x19) ~ N(m, V) with V = (Sigma^-1 + D^-1)^-1 and
# m = mu + V D^-1 (x - mu), D = diag(se15^2, se19^2). tau15^2, tau19^2 and the
# state means are each round's own DerSimonian-Laird estimates (as before); the
# covariance c is the moment estimate from the within-state residuals of the
# districts observed in both rounds (sampling errors are independent across
# rounds, so the raw cross-product has no noise term). Districts observed in only
# one round keep the univariate posterior. The posterior correlation of
# (X15, X19) is carried into the draws (see make_z) and into the imputation in
# H2 / 22 / 23, so that the NFHS-side error of the CHANGE is right.
# This is an empirical-Bayes approximation: the between-round correlation is
# ESTIMATED once and then treated as known. Its state-bootstrap interval is
# reported (me_shrinkage_joint.csv) but its uncertainty is not propagated into
# the posterior draws or the imputations.
# Uncertainty in the between-round correlation: resample whole states (500
# cluster-bootstrap replicates) and re-estimate tau^2 and the covariance.
eb_joint_boot <- function(d15, d19, B = 500L) {
  set.seed(20261022L)
  sts <- unique(as.character(d15$state_name))
  r <- vapply(seq_len(B), function(k) {
    s <- sample(sts, length(sts), replace = TRUE)
    pick <- function(d) dplyr::bind_rows(lapply(seq_along(s), function(i) {
      x <- d[as.character(d$state_name) == s[i], , drop = FALSE]
      x$state_name <- paste0(s[i], "#", i); x$district <- paste0(x$district, "#", i); x }))
    tryCatch({ z <- eb_joint(pick(d15), pick(d19), boot = FALSE, quiet = TRUE); c(z$rho_prior, z$rho_raw) },
             error = function(e) c(NA_real_, NA_real_))
  }, numeric(2))
  raw <- r[2, ]; r <- r[1, ]
  # Failed replicates are counted, not silently dropped from the interval.
  # The interval is reported from the UNCAPPED replicate estimates, with the
  # share of replicates at the +/-0.95 cap alongside (31_postrun_intervals.R
  # writes every replicate).
  structure(stats::quantile(raw, c(.025, .975), na.rm = TRUE), n_failed = sum(!is.finite(r)), n_boot = B,
            share_at_cap = mean(abs(r[is.finite(r)]) >= 0.95 - 1e-9))
}
eb_joint <- function(d15, d19, boot = TRUE, quiet = FALSE) {
  e15 <- eb_shrink(d15$x_obs, d15$x_se, d15$state_name)
  e19 <- eb_shrink(d19$x_obs, d19$x_se, d19$state_name)
  a <- tibble(district = d15$district, x15 = d15$x_obs, s15 = d15$x_se, mu15 = e15$mu,
              m15 = e15$mean, sd15 = e15$sd)
  b <- tibble(district = d19$district, x19 = d19$x_obs, s19 = d19$x_se, mu19 = e19$mu,
              m19 = e19$mean, sd19 = e19$sd)
  j  <- dplyr::inner_join(a, b, by = "district")
  r15 <- j$x15 - j$mu15; r19 <- j$x19 - j$mu19
  t15 <- e15$tau2; t19 <- e19$tau2
  # Covariance of the true values: a precision-weighted mean of the residual
  # cross-products (sampling errors are independent across rounds, so
  # E[r15 r19] = cov), over districts with usable standard errors in both
  # rounds. Districts whose estimate sat at 0 or 1 (SE set to SE_CAP) carry no
  # information and would otherwise dominate with huge same-signed residuals.
  use <- j$s15 < SE_CAP & j$s19 < SE_CAP
  wc  <- 1 / sqrt((t15 + j$s15^2) * (t19 + j$s19^2))
  n_grp <- length(unique(d15$state_name[match(j$district[use], d15$district)]))
  # The CORRELATION is estimated from three moments computed the same way --
  # same districts, same weights, same degrees-of-freedom correction -- so that
  # numerator and denominator are on a common footing:
  #   cov  = E_w[r15 r19]                 (independent sampling errors drop out)
  #   var_t = E_w[r_t^2 - se_t^2]          (sampling variance removed)
  # Dividing the covariance by the DerSimonian-Laird tau^2 instead (an earlier
  # version) mixed two estimators and overshot 1 on these data (1.06, capped).
  # The covariance used in Sigma is then rho * tau15 * tau19, so the marginal
  # variances stay the DL values each round's own shrinkage uses.
  mw  <- function(v) sum(wc[use] * v[use]) / sum(wc[use]) * sum(use) / max(sum(use) - n_grp, 1)
  c_m <- mw(r15 * r19); v15m <- mw(r15^2 - j$s15^2); v19m <- mw(r19^2 - j$s19^2)
  rho_raw <- if (v15m > 0 && v19m > 0) c_m / sqrt(v15m * v19m) else 0
  rho_prior <- if (t15 > 0 && t19 > 0) max(min(rho_raw, 0.95), -0.95) else 0
  cv <- rho_prior * sqrt(t15 * t19)
  rho_ci <- if (isTRUE(boot)) eb_joint_boot(d15, d19) else c(NA_real_, NA_real_)
  Sg  <- matrix(c(t15, cv, cv, t19), 2)
  # Posterior in the form that needs only Sigma + D to be invertible, so it is
  # valid when Sigma is singular (a between-district variance of exactly zero,
  # which eb_shrink allows, or |rho| at the bound): with G = Sigma (Sigma + D)^-1,
  #   m = mu + G (x - mu),   V = Sigma - G Sigma.
  # D (the sampling variances) is positive definite, so Sigma + D always is.
  post <- t(vapply(seq_len(nrow(j)), function(i) {
    Di <- diag(c(j$s15[i]^2, j$s19[i]^2))
    G  <- Sg %*% solve(Sg + Di)
    m  <- c(j$mu15[i], j$mu19[i]) + G %*% c(r15[i], r19[i])
    V  <- Sg - G %*% Sg; V <- (V + t(V)) / 2
    sdv <- sqrt(pmax(diag(V), 0))
    c(m[1], m[2], sdv[1], sdv[2], if (all(sdv > 0)) V[1, 2] / (sdv[1] * sdv[2]) else 0)
  }, numeric(5)))
  j$m15 <- post[, 1]; j$m19 <- post[, 2]; j$sd15 <- post[, 3]; j$sd19 <- post[, 4]
  j$rho_post <- post[, 5]
  if (!quiet) message(sprintf("  joint shrinkage: %d districts in both rounds; between-district correlation of true values %.3f (unbounded estimate %.3f; state-bootstrap 95%% CI %.3f to %.3f); median posterior correlation %.3f",
                  nrow(j), rho_prior, rho_raw, rho_ci[1], rho_ci[2], median(j$rho_post)))
  list(p15 = dplyr::bind_rows(j %>% transmute(district, mean = m15, sd = sd15),
                              a %>% filter(!district %in% j$district) %>% transmute(district, mean = m15, sd = sd15)),
       p19 = dplyr::bind_rows(j %>% transmute(district, mean = m19, sd = sd19),
                              b %>% filter(!district %in% j$district) %>% transmute(district, mean = m19, sd = sd19)),
       rho = j %>% transmute(district, rho_post), rho_prior = rho_prior, rho_raw = rho_raw, rho_ci = rho_ci, cov = cv)
}
SHRINK_LOG <- list()
# Step 1 (per surface): the NFHS input of one round, shrunk to the posterior of
# its true value.
prep_input <- function(nfhs_df, est_col, se_col) {
  if (!"state_name" %in% names(nfhs_df)) nfhs_df$state_name <- nfhs_df$state
  nd <- nfhs_df %>%
    filter(!is.na(.data[[est_col]])) %>%
    transmute(district, state_name = as.character(state_name),
              x_obs = qlogis(clamp(.data[[est_col]])),
              x_se  = pmin(pmax(.data[[se_col]] /
                       (clamp(.data[[est_col]]) * (1 - clamp(.data[[est_col]]))),
                       SE_FLOOR), SE_CAP))
  nd <- nd %>% filter(is.finite(x_obs), is.finite(x_se))
  raw_p  <- nfhs_df[[est_col]][match(nd$district, nfhs_df$district)]
  raw_se <- nfhs_df[[se_col]][match(nd$district, nfhs_df$district)]
  degenerate <- !is.finite(raw_se) | raw_se <= 1e-6 | nd$x_se <= 1e-3 | raw_p <= 0 | raw_p >= 1
  nd$x_se[degenerate] <- SE_CAP
  nd
}
prep_surface <- function(fit, nfhs_df, est_col, se_col, out_prefix, round = NA_integer_, post = NULL) {
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
                       SE_FLOOR), SE_CAP),
              # Placeholder for the response-side SE term in the model formula
              # (y | se(y_se, ...)): brms requires the variable to be present in
              # newdata, but posterior_epred returns the expected value of the
              # linear predictor, which does NOT depend on y_se, so any positive
              # constant gives the identical corrected estimate.
              y_se = 0.1)
  nd <- nd %>% filter(is.finite(x_obs), is.finite(x_se))
  # A district whose design SE is (numerically) zero -- e.g. a single-PSU rural
  # domain, or an estimate of exactly 0 or 1 -- carries no usable precision
  # information; floored at SE_FLOOR it would get weight 1e8 and dominate its
  # state mean and tau. Treat it as uninformative (SE at the cap), the rule
  # prep_me() applies on the reference side.
  raw_p  <- nfhs_df[[est_col]][match(nd$district, nfhs_df$district)]
  raw_se <- nfhs_df[[se_col]][match(nd$district, nfhs_df$district)]
  degenerate <- !is.finite(raw_se) | raw_se <= 1e-6 | nd$x_se <= 1e-3 | raw_p <= 0 | raw_p >= 1
  nd$x_se[degenerate] <- SE_CAP
  eb <- eb_shrink(nd$x_obs, nd$x_se, nd$state_name)
  nd$x_mean_sep <- eb$mean; nd$x_sd_sep <- pmax(eb$sd, SE_FLOOR)   # each round on its own (comparison)
  if (!is.null(post)) {           # joint (two-round) posterior replaces the univariate one
    k <- match(nd$district, post$district)
    if (anyNA(k)) stop("joint posterior missing districts for ", out_prefix)
    eb$mean <- post$mean[k]; eb$sd <- post$sd[k]
    eb$lambda <- pmin(1, (eb$sd / nd$x_se)^2)
  }
  SHRINK_LOG[[out_prefix]] <<- tibble(surface = out_prefix, district = nd$district,
    degenerate_se = degenerate,
    state_name = nd$state_name, x_obs = nd$x_obs, x_se = nd$x_se,
    state_mean = eb$mu, lambda = eb$lambda, x_post_mean = eb$mean,
    x_post_sd = eb$sd, tau2 = eb$tau2)
  message(sprintf("  correct_bayes[%s]: empirical-Bayes tau = %.3f (logit); median shrinkage weight lambda = %.2f",
                  out_prefix, sqrt(eb$tau2), median(eb$lambda)))
  nd$x_obs <- eb$mean                       # posterior mean of the true value
  nd$x_sd  <- pmax(eb$sd, SE_FLOOR)         # its posterior SD
  fitted_states <- unique(as.character(fit$data$state_name))
  nd$in_support <- nd$state_name %in% fitted_states
  message(sprintf("  correct_bayes[%s]: %d of %d districts in calibration states ",
                  out_prefix, sum(nd$in_support), nrow(nd)),
          "(estimated state effect applied); the rest drawn from the state-effect distribution.")
  list(nd = nd, prefix = out_prefix, round = round)
}

# The NFHS-side noise z of every surface is generated per district from a seed
# fixed by the district and the round, so all surfaces built on the same NFHS
# round share the same z (it is the same sampling error), and the 2019 z is
# correlated with the 2015 z by the district's posterior correlation (JOINT).
Z_SEED <- c("2015" = 20240715L, "2019" = 20240716L)
make_z <- function(round, ids, ndraw) {
  rho <- JOINT$rho$rho_post[match(as.character(ids), as.character(JOINT$rho$district))]
  rho[is.na(rho)] <- 0
  z <- matrix(0, ndraw, length(ids))
  for (k in seq_along(ids)) {
    # One seed per district; both rounds' noise come from that one stream
    # (2015 first, then the independent part of 2019), so no two districts or
    # rounds can share a stream.
    set.seed(Z_SEED[["2015"]] + 7919L * as.integer(as.numeric(ids[k])))
    z15 <- rnorm(ndraw)
    if (round == 2015) { z[, k] <- z15; next }
    z[, k] <- rho[k] * z15 + sqrt(1 - rho[k]^2) * rnorm(ndraw)
  }
  z
}

# Step 2 (per FIT): one posterior_epred call for every surface predicted from
# that fit, then the NFHS-side uncertainty added surface by surface.
#
# Two sources of randomness, handled differently on purpose:
#  * The state effect of a state outside the calibration sample is drawn from the
#    fitted between-state distribution (allow_new_levels + sample_new_levels =
#    "gaussian"). It is a property of the FIT and the STATE, not of the survey
#    round, so two surfaces predicted from the same fit (the 2019 surface and the
#    IRES-calibrated 2015 surface) must use the SAME draw for the same state;
#    otherwise the instrument-consistent change would carry state-effect noise
#    that has nothing to do with the change. brms draws one effect per new level
#    per posterior draw within a call, so stacking the surfaces into ONE call
#    gives exactly that. The seed is fixed per fit (bit-reproducible runs).
#  * The NFHS-side error (the true district value given its noisy estimate) is
#    specific to each round's survey sample, so it is independent across
#    surfaces. brms would add it as X ~ N(x_obs, x_se) inside posterior_epred;
#    instead X is held at its posterior mean there (x_se = SE_FLOOR) and the
#    term b * x_sd * z, z ~ N(0,1), is added afterwards with a fixed seed per
#    surface. On the logit (linear-predictor) scale this is the same quantity:
#    b * X = b * mean + b * sd * z.
predict_surfaces <- function(fit, fit_tag, preps) {
  nd_all <- dplyr::bind_rows(lapply(preps, function(p)
    p$nd %>% mutate(.surface = p$prefix, x_se = SE_FLOOR)))
  set.seed(20240714L + sum(utf8ToInt(fit_tag)))
  pp_all <- posterior_epred(fit, newdata = nd_all %>% select(district, state_name, x_obs, x_se, y_se),
                            re_formula = NULL,
                            allow_new_levels = TRUE, sample_new_levels = "gaussian")
  bvar <- grep("^bsp_me", brms::variables(fit), value = TRUE)
  if (length(bvar) != 1) stop("measurement-error slope not found uniquely in ", fit_tag,
                              ": ", paste(bvar, collapse = ", "))
  b <- as.numeric(as.matrix(fit, variable = bvar))
  sig <- as.numeric(as.matrix(fit, variable = "sigma"))
  comp_idx <- unique(round(seq(1, nrow(pp_all), length.out = min(K_COMP, nrow(pp_all)))))
  if (length(b) != nrow(pp_all))
    stop(sprintf("slope draws (%d) and posterior_epred draws (%d) differ for %s",
                 length(b), nrow(pp_all), fit_tag))
  out <- list()
  for (p in preps) {
    j  <- which(nd_all$.surface == p$prefix)
    nd <- p$nd
    z  <- make_z(p$round, nd$district, nrow(pp_all))
    # b (length = draws) recycles down the rows: element [i, k] gets b[i] * sd[k] * z[i, k]
    pp <- pp_all[, j, drop = FALSE] + b * sweep(z, 2, nd$x_sd, `*`)
    rm(z)
    out[[p$prefix]] <- list(
      summary = nd %>%
        transmute(district,
               !!paste0(p$prefix, "_bayes")        := plogis(colMeans(pp)),
               !!paste0(p$prefix, "_bayes_lo")     := plogis(apply(pp, 2, quantile, .025)),
               !!paste0(p$prefix, "_bayes_hi")     := plogis(apply(pp, 2, quantile, .975)),
               !!paste0(p$prefix, "_in_support")   := in_support),
      draws = plogis(pp),                 # (n_draws x n_districts), probability scale
      districts = nd$district,
      # Components for the outcome-conditional imputation (scripts/mime.R):
      # the shared part S = a + b * E[X] + u_s on a thinned set of draws, the
      # slope draws, the posterior SD of the true NFHS value, and the posterior
      # mean of the corrected logit (the point surface, kappa = 0).
      comp = list(S = pp_all[comp_idx, j, drop = FALSE], b = b[comp_idx], x_sd = nd$x_sd,
                  mu_joint = nd$x_obs, mu_sep = nd$x_mean_sep, sd_sep = nd$x_sd_sep,
                  Lbar = colMeans(pp), districts = nd$district, round = p$round,
                  sigma = sig[comp_idx], draw_index = comp_idx))
    rm(pp); gc(verbose = FALSE)
  }
  rm(pp_all); gc(verbose = FALSE)
  out
}

K_COMP <- 4000L   # thinned calibration draws saved for the outcome-conditional imputation
JOINT <- eb_joint(prep_input(nfhs_corrected, "lpg_2015_rural_wt", "lpg_2015_rural_wt_se"),
                  prep_input(nfhs_corrected, "lpg_2019_rural_wt", "lpg_2019_rural_wt_se"))
pA   <- prep_surface(bA,    nfhs_corrected, "lpg_2015_rural_wt", "lpg_2015_rural_wt_se", "lpg_2015", 2015, JOINT$p15)
pB   <- prep_surface(bB,    nfhs_corrected, "lpg_2019_rural_wt", "lpg_2019_rural_wt_se", "lpg_2019", 2019, JOINT$p19)
# ---- Instrument-consistent sensitivity surface (health option 3) -------------
# The PRIMARY corrected change is era-matched (ACCESS calibrates 2015, IRES
# calibrates 2019), which is right for the LEVELS but means the corrected 2015->
# 2019 CHANGE switches calibration instrument across rounds. For a sensitivity
# that removes that switch, apply the SAME IRES-RURAL calibration (bB) to the
# NFHS-4 rural estimate as well, so both endpoints share one instrument. The 2019
# IRES-cal surface is identical to lpg_2019_bayes; only the 2015 endpoint differs.
# Rural throughout by design (ACCESS is rural; the main analysis is rural).
pB15 <- prep_surface(bB,    nfhs_corrected, "lpg_2015_rural_wt", "lpg_2015_rural_wt_se", "lpg_2015_irescal", 2015, JOINT$p15)
# ---- Leg-A symmetry sensitivity surface -------------------------------------
# Same NFHS-4 rural input, same estimand, but calibrated by bA_wt (design-weighted
# ACCESS reference + its Taylor design SE) instead of bA. Paired with the SAME
# 2019 surface downstream, so any movement in the corrected change is attributable
# to the leg-A reference construction alone.
pAwt <- prep_surface(bA_wt, nfhs_corrected, "lpg_2015_rural_wt", "lpg_2015_rural_wt_se", "lpg_2015_accesswt", 2015, JOINT$p15)

# corrB and corrB15 come from one call, so they share the new-state draws.
.rB  <- predict_surfaces(bB, "bB", list(pB, pB15))
corrB <- .rB[["lpg_2019"]]; corrB15 <- .rB[["lpg_2015_irescal"]]; rm(.rB)
corrA    <- predict_surfaces(bA,    "bA",    list(pA))[["lpg_2015"]]
corrA_wt <- predict_surfaces(bA_wt, "bA_wt", list(pAwt))[["lpg_2015_accesswt"]]
readr::write_csv(dplyr::bind_rows(SHRINK_LOG), file.path(dir_out, "me_shrinkage.csv"))
chk("05", "empirical-Bayes shrinkage of the NFHS input is well defined (0 < tau < 1.5 logit, no SE at the floor)",
    all(vapply(SHRINK_LOG, function(t) all(is.finite(t$x_post_mean)) &&
                 all(t$lambda > 0 & t$lambda <= 1) && t$tau2[1] > 0 &&
                 sqrt(t$tau2[1]) < 1.5 && all(t$x_se > SE_FLOOR), logical(1))),
    paste(vapply(names(SHRINK_LOG), function(k) sprintf("%s: tau %.3f, median lambda %.2f",
          k, sqrt(SHRINK_LOG[[k]]$tau2[1]), median(SHRINK_LOG[[k]]$lambda)), character(1)),
          collapse = "; "))
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
# Support is judged on the predictor the PRIMARY (Bayesian) model was fitted
# on -- the design-weighted rural NFHS estimate -- not on the multilevel
# estimate that regression calibration uses.
supp_2015_rng <- range(pairA$lpg_2015_rural_wt, na.rm = TRUE)
supp_2019_rng <- range(pairB$lpg_2019_rural_wt, na.rm = TRUE)
nfhs_corrected <- nfhs_corrected %>%
  mutate(
    lpg_2015_cov_support = cov_support(lpg_2015_rural_wt, pairA$lpg_2015_rural_wt),
    lpg_2019_cov_support = cov_support(lpg_2019_rural_wt, pairB$lpg_2019_rural_wt),
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
saveRDS(list(surfaces = list(lpg_2015 = corrA$comp, lpg_2019 = corrB$comp,
                             lpg_2015_irescal = corrB15$comp, lpg_2015_accesswt = corrA_wt$comp),
             rho = JOINT$rho, rho_prior = JOINT$rho_prior),
        file.path(dir_out, "correction_posterior_components.rds"))
readr::write_csv(tibble(quantity = c("between_round_correlation_true_values", "between_round_correlation_ci_lo",
                                     "between_round_correlation_ci_hi", "between_round_covariance",
                                     "median_posterior_correlation", "n_districts_both_rounds",
                                     "bootstrap_replicates", "bootstrap_failed", "bootstrap_share_at_cap"),
                        value = c(JOINT$rho_prior, JOINT$rho_ci[1], JOINT$rho_ci[2], JOINT$cov,
                                  median(JOINT$rho$rho_post), nrow(JOINT$rho),
                                  if (is.null(attr(JOINT$rho_ci, "n_boot"))) NA_real_ else attr(JOINT$rho_ci, "n_boot"),
                                  if (is.null(attr(JOINT$rho_ci, "n_failed"))) NA_real_ else attr(JOINT$rho_ci, "n_failed"),
                                  if (is.null(attr(JOINT$rho_ci, "share_at_cap"))) NA_real_ else attr(JOINT$rho_ci, "share_at_cap"))),
                 file.path(dir_out, "me_shrinkage_joint.csv"))
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
#
# SCOPE (stated because it is easy to over-read). This cross-validation refits
# the REGRESSION CALIBRATION (lm) in each of the ~20 held-out folds, not the
# Bayesian measurement-error model: an honest Bayesian LOSO would require ~20
# additional brms fits of 4 chains x 24,000 iterations, which is prohibitive
# for a diagnostic whose purpose is to test whether the CALIBRATION RELATION
# transports across states. The two estimators share that relation (their
# fitted slopes agree closely; Table S9.3), so the regression-calibration LOSO
# is informative about both, but it is not a validation of the Bayesian
# posterior and the manuscript says so explicitly.
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
chk("05", "imputation components written (shared part, slope, NFHS-side SD, point surface; joint shrinkage)",
    { p <- file.path(dir_out, "correction_posterior_components.rds")
      file.exists(p) && { cc <- readRDS(p)
        all(c("lpg_2015", "lpg_2019", "lpg_2015_irescal", "lpg_2015_accesswt") %in% names(cc$surfaces)) &&
        all(vapply(cc$surfaces, function(s) nrow(s$S) == length(s$b) && ncol(s$S) == length(s$x_sd) &&
                     all(is.finite(s$S)) && all(abs(plogis(s$Lbar) - plogis(colMeans(s$S))) < 0.05), logical(1))) &&
        is.finite(cc$rho_prior) && abs(cc$rho_prior) < 0.95 - 1e-9 } },
    sprintf("between-round correlation of true district values %.3f; median posterior correlation %.3f",
            JOINT$rho_prior, median(JOINT$rho$rho_post)))
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
# Checked on the flags actually written (nfhs_corrected), not by re-deriving the
# range from itself (which could never fail).
chk_warn("05", "every calibration district is flagged inside its own covariate support",
    { ia <- as.character(nfhs_corrected$district) %in%
              as.character(pairA$district[is.finite(pairA$lpg_2015_rural_wt)])
      ib <- as.character(nfhs_corrected$district) %in%
              as.character(pairB$district[is.finite(pairB$lpg_2019_rural_wt)])
      any(ia) && any(ib) &&
        all(nfhs_corrected$lpg_2015_cov_support[ia] %in% TRUE) &&
        all(nfhs_corrected$lpg_2019_cov_support[ib] %in% TRUE) },
    "calibration districts with covariate-support flag FALSE would mean the flag and the fit data disagree")
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
