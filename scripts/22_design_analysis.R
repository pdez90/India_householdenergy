# ==============================================================================
# 22_design_analysis.R   (STANDALONE -- does not source 00_config.R)
#
# HOW PRECISE WOULD A VALIDATION SURVEY HAVE TO BE?
#
# The paper's corrected-exposure estimates (H2: the fixed Bayesian surface,
# change_lpg_bayes, and the exposure-only MI sensitivity, change_lpg_bayes_MI)
# are not statistically significant under CR2, and part of the reason is
# explicit in the pipeline: the corrected district exposure is ESTIMATED, and
# propagating that estimation uncertainty widens the interval further.
# The natural next question -- and the one this script answers -- is not
# "is the association real?" but:
#
#     What would a validation survey have to look like for this design to
#     return a conventionally significant answer at all?
#
# That is a DESIGN question, and it is answered in three stages.
#
# STAGE 1 -- THE PRECISION FRONTIER (kappa).
#   05_correction.R computes the corrected surface as
#       pp    <- posterior_epred(fit, newdata = nd, ...)   # LOGIT scale
#       draws <- plogis(pp)                                # saved
#       point <- plogis(colMeans(pp))                      # lpg_*_bayes
#   so shrinking the SAVED draws toward their across-draw mean ON THE LOGIT
#   SCALE by a factor kappa in [0, 1],
#       L(kappa) = Lbar + kappa * (L - Lbar),   y(kappa) = plogis(L(kappa)),
#   has MATHEMATICALLY EXACT endpoints:
#       kappa = 1  reproduces the real posterior  -> change_lpg_bayes_MI
#       kappa = 0  collapses to plogis(colMeans)  -> change_lpg_bayes
#   No clamping is required; qlogis/plogis are exact inverses over the observed
#   range. kappa is therefore a clean, interpretable index of calibration
#   precision: "the posterior SD is kappa times what we actually achieved".
#   Re-running H2's MI machinery across kappa traces beta, SE, z and the
#   fraction of missing information, and locating the z = 1.96 crossing gives
#   kappa*, the precision the correction would have to reach.
#
#   IMPORTANT INTERPRETATION LIMIT. This is a PRECISION calculation conditional
#   on the current point surface. It answers "how much sharper would the
#   correction have to be", NOT "what would a better survey find" -- a real new
#   survey would move the point estimates too. It is a power/design calculation,
#   and must be reported as one.
#
# STAGE 2 -- WHERE THE POSTERIOR VARIANCE COMES FROM.
#   The corrected district value is a + b*x + u_s evaluated at the district's
#   NFHS predictor x. Its posterior variance has two parts:
#     (i)  CALIBRATION-SIDE: uncertainty in (a, b, u_s). This is what a bigger
#          or better validation survey buys down. It is reproduced here in
#          closed form (a Bayesian LMM with known variance components, averaged
#          over the posterior draws of sigma and psi rather than conditioning
#          on their medians; design_vc_plugin_penalty.csv reports what that
#          choice is actually worth, and it is leg-specific), validated
#          against the two real
#          brms fits.
#     (ii) NFHS-SIDE: the target district's OWN sampling error in x, which the
#          me() term propagates. This is a property of NFHS's district sample
#          sizes and is INVARIANT to the validation survey's design.
#   Part (ii) sets a floor, kappa_floor, that no validation survey can go below.
#
# STAGE 3 -- DESIGN MAPPING.
#   kappa is mapped back to a survey design (D districts, S states, m households
#   per district relative to the existing surveys, instrument residual sigma),
#   anchored so that the two ACTUAL designs -- ACCESS 51 districts / 6 states and
#   IRES 144 districts / 19 states -- reproduce kappa = 1 by construction. Two
#   routes are costed:
#     TRANSFER route   fit a calibration line on D districts and predict the
#                      rest. Bounded below by kappa_floor.
#     DIRECT route     field the good instrument in EVERY study district, in
#                      both rounds, so no transfer function is needed at all.
#                      Bounded only by the instrument's own sampling error.
#
# Standalone inputs (files on disk only, all from dir_out):
#   health_district_wide.rds          <- H1_prep_mortality.R
#   corrected_nfhs_districts.rds      <- 05_correction.R
#   correction_posterior_draws.rds    <- 05_correction.R  (REQUIRED)
#   brms_me_nfhs4_access.rds          <- 05_correction.R  (stages 2-3)
#   brms_me_nfhs5_ires.rds            <- 05_correction.R  (stages 2-3)
#   district_exposure_proxy.csv       <- 06_stacking_prediction.R (stages 2-3)
#   health_effects_table.csv          <- H2_health_models.R (validation only)
#   df_wide_health.rds                <- H3_env_covariates.R (optional)
#
# Outputs (dir_out):
#   design_frontier.csv               beta/SE/z/p/FMI at each kappa
#   design_kappa_star.csv             kappa*, reachability, per outcome
#   design_variance_decomposition.csv calibration-side vs NFHS-side variance
#   design_grid.csv                   transfer-route designs
#   design_direct.csv                 direct-measurement route
#   design_summary.csv                the scalars the manuscript quotes
#   design_frontier.jpeg              the design-frontier figure
# ==============================================================================

## ---- CONFIG ------------------------------------------------------------------
if (!exists(".env_dir")) source("paths.R")   # path resolution; fails loudly if unconfigured
dir_out <- .dir_out()
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers
SCRIPT <- "22_design_analysis.R"

# Fall back to no-op check helpers if checks.R is not on the path, so the script
# still runs standalone from another directory.
if (!exists("chk")) {
  chk      <- function(script, label, ok, detail = "") {
    cat(sprintf("  [%s] %-52s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label, detail)); invisible(ok) }
  chk_warn <- function(script, label, ok, detail = "") {
    cat(sprintf("  [%s] %-52s %s\n", if (isTRUE(ok)) "PASS" else "WARN", label, detail)); invisible(ok) }
  chk_header <- function(script) cat(sprintf("\n== CHECKS [%s] ==\n", script))
  chk_rng <- function(x) { x <- x[is.finite(x)]
    if (!length(x)) "no finite values" else sprintf("range [%.3g, %.3g], n=%d", min(x), max(x), length(x)) }
}

suppressPackageStartupMessages({
  library(tidyverse); library(lmtest); library(sandwich)
})

M          <- 200            # imputations, matching H2
# The precision frontier, kappa*, the fixed-calibration scenario and the design
# grid used the outcome-conditioned imputation, which failed its convergence
# checks and was abandoned for this paper (docs/mime_v2_spec.md, Section 9).
# They are OFF by default; only the variance decomposition (Part 2) runs.
# Their last outputs are archived as "failed convergence -- not for inference".
DESIGN_FRONTIER <- identical(Sys.getenv("DESIGN_FRONTIER", "0"), "1")
Z_CRIT     <- 1.96
KAPPA_GRID <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50,
                0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.90, 1.00)
N_STATES_INDIA <- 36         # states + UTs, for the coverage schedule
NFHS5_HOUSEHOLDS <- 636699   # NFHS-5 fielded sample, for the cost comparison

message("\n===== 22_design_analysis.R : validation-survey design analysis =====")

## ==============================================================================
## PART 0 -- rebuild H2's analysis frame, EXACTLY as H2 does
## ==============================================================================
path_health <- file.path(dir_out, "health_district_wide.rds")
path_corr   <- file.path(dir_out, "corrected_nfhs_districts.rds")
path_draws  <- file.path(dir_out, "correction_posterior_draws.rds")
path_dfwide <- file.path(dir_out, "df_wide_health.rds")
path_tab    <- file.path(dir_out, "health_effects_table.csv")
path_proxy  <- file.path(dir_out, "district_exposure_proxy.csv")
path_fitA   <- file.path(dir_out, "brms_me_nfhs4_access.rds")
path_fitI   <- file.path(dir_out, "brms_me_nfhs5_ires.rds")

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
OUTCOMES <- c(neonatal = "change_neonatal_death", infant = "change_infant_death")
# Outcome-specific analytic weight (H1): the harmonic mean, across the two rounds,
# of the births ELIGIBLE for that outcome (exposed for its full age interval), so
# each outcome is weighted by its own denominator; neonatal and infant
# eligibility differ. Required: a frame without these columns was written by an
# older H1, whose all-births weight included births not yet exposed for the
# full interval.
WCOLS <- c(change_neonatal_death = "w_hmean_neonatal",
           change_infant_death   = "w_hmean_infant")
if (!all(WCOLS %in% names(df)))
  stop("[22] outcome-specific birth weights (", paste(WCOLS, collapse = ", "),
       ") not found in the health district frame; re-run H1_prep_mortality.R.", call. = FALSE)
wcol <- "w_hmean_<outcome>"   # weighted; the column is chosen per outcome by wv_for()
wv_for <- function(dat, ocol) {
  if (!ocol %in% names(WCOLS)) stop("[22] no birth weight defined for outcome ", ocol, call. = FALSE)
  dat[[WCOLS[[ocol]]]]
}

CR2_OK       <- requireNamespace("clubSandwich", quietly = TRUE)
CR2_FAILURES <- 0L
# H2's fit_one, verbatim in behaviour (lm + cluster-robust VCE + coeftest),
# extended with the CR2/Satterthwaite pair H2 now reports as primary.
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
  cl   <- if (has_state) dat[["state"]][rows] else NULL
  vc   <- if (has_state) sandwich::vcovCL(m, cluster = cl)
          else sandwich::vcovHC(m, "HC1")
  ct   <- lmtest::coeftest(m, vcov. = vc)
  i    <- match(exposure, rownames(ct))
  # CR2 (Bell-McCaffrey) with Satterthwaite df, alongside the conventional
  # sandwich (vcovCL defaults, i.e. CR1; called CR0 in the variable names).
  # Both are carried so that kappa* can be located under EITHER inference rule
  # and the two can be reported side by side: with ~32 state clusters the
  # conventional z is anticonservative (see the null simulation in
  # H2_health_models.R), so the conventional crossing is an OPTIMISTIC
  # bound on the precision a study would actually need.
  se2 <- NA_real_; dfs <- NA_real_
  if (has_state && CR2_OK) {
    ok <- try({
      ct2 <- clubSandwich::coef_test(m, vcov = "CR2", cluster = cl,
                                     test = "Satterthwaite", coefs = exposure)
      se2 <- ct2$SE[1]; dfs <- ct2$df_Satt[1]
    }, silent = TRUE)
    if (inherits(ok, "try-error")) CR2_FAILURES <<- CR2_FAILURES + 1L
  }
  c(estimate = ct[i,1], se = ct[i,2], se_cr2 = se2, df_satt = dfs, n = nobs(m))
}

message(sprintf("Analysis frame: %d districts, %d adjustment covariates%s, weight = %s",
                nrow(df), length(adj), if (has_region) " + region FE" else "",
                if (is.na(wcol)) "NONE (unweighted)" else wcol))

## ==============================================================================
## PART 1 -- the precision frontier
## ==============================================================================
if (DESIGN_FRONTIER) {   # ---- Part 1 (frontier): disabled by default
# Logit-scale posterior. colMeans on the LOGIT scale is what 05_correction.R
# passes through plogis() to make lpg_*_bayes, so kappa = 0 lands on it exactly.
L15  <- qlogis(pd$y2015);  L19 <- qlogis(pd$y2019)
id15 <- as.character(as.numeric(pd$districts_2015))
id19 <- as.character(as.numeric(pd$districts_2019))
m15  <- colMeans(L15);     m19 <- colMeans(L19)
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
chk(SCRIPT, "kappa=0 surface reproduces lpg_*_bayes exactly",
    {
      a <- plogis(m15); b <- corr[[ "lpg_2015_bayes" ]][
             match(id15, as.character(as.numeric(corr$district)))]
      ok <- is.finite(a) & is.finite(b)
      max(abs(a[ok] - b[ok])) < 1e-8
    },
    "max |plogis(colMeans(logit)) - lpg_2015_bayes| < 1e-8")
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

# The frontier is traced with the SAME outcome-conditional imputation as H2's
# primary MI row (scripts/mime.R): at each kappa the calibration posterior is
# rescaled around the point surface, L(kappa) = Lbar + kappa (L - Lbar), and the
# exposure is imputed from that rescaled prior and the outcome model. The seed,
# M, burn-in and thinning equal H2's, so kappa = 1 reproduces H2's published MI
# row and kappa = 0 (nothing to impute) its fixed-surface row. Priors can be
# supplied directly (pr15, pr19) to pool any other imputation scenario, e.g.
# the fixed-calibration route below.
source("mime.R")
path_comp <- file.path(dir_out, "correction_posterior_components.rds")
if (!file.exists(path_comp))
  stop("correction_posterior_components.rds not found -- re-run 05_correction.R first.")
COMP <- readRDS(path_comp)
PR15 <- mime_prior(COMP$surfaces$lpg_2015, COMP$rho)
PR19 <- mime_prior(COMP$surfaces$lpg_2019, COMP$rho)
MIME_BURN <- 1000L; MIME_THIN <- 10L
mime_seed <- function(term, outc, stratum)
  20261021L + sum(utf8ToInt(paste(term, outc, stratum, sep = "|"))) %% 100000L
MI_RHS <- c(adj, if (has_region) "factor(region)")
DESIGN_DIAG <- list()
pool_at <- function(kap, outc, pr15 = NULL, pr19 = NULL, tag = sprintf("kappa = %.4f", kap)) {
  if (is.null(pr15)) pr15 <- mime_kappa(PR15, kap)
  if (is.null(pr19)) pr19 <- mime_kappa(PR19, kap)
  im <- mime_draws(df, OUTCOMES[[outc]], MI_RHS, wv_for(df, OUTCOMES[[outc]]),
                   pr15, pr19, M = Meff, burn = MIME_BURN, thin = MIME_THIN,
                   seed = mime_seed("change_lpg_bayes_MI", outc, "all"))
  est <- numeric(Meff); sev <- numeric(Meff); nn <- numeric(Meff)
  se2 <- numeric(Meff); dfs <- numeric(Meff)
  for (mm in seq_len(Meff)) {
    d <- df
    d$change_lpg_mi <- im$D[, mm]
    r <- fit_one(d, OUTCOMES[[outc]], "change_lpg_mi",
                 wvec = wv_for(d, OUTCOMES[[outc]]))
    est[mm] <- r[["estimate"]]; sev[mm] <- r[["se"]]; nn[mm] <- r[["n"]]
    se2[mm] <- r[["se_cr2"]];   dfs[mm] <- r[["df_satt"]]
  }
  # Sampler diagnostics for THIS point (convergence at kappa = 1 in H2 says
  # nothing about other kappas or scenarios), with the completed-data coefficient.
  ce <- mime_conv_est(est, im$diag$chains)
  DESIGN_DIAG[[length(DESIGN_DIAG) + 1]] <<- data.frame(scenario = tag, kappa = kap, outcome = outc,
    as.data.frame(im$diag), rhat_cr2_estimate = ce[["rhat"]], ess_cr2_estimate = ce[["ess"]])
  qbar <- mean(est); ubar <- mean(sev^2); bvar <- var(est)
  tvar <- ubar + (1 + 1/Meff) * bvar

  # ---- the same pooling on the CR2 leg, with Barnard-Rubin df ----------------
  # Two df corrections compose here and neither can be dropped. Satterthwaite
  # handles the small number of state clusters WITHIN an imputation; Barnard-
  # Rubin handles the finite number of imputations BETWEEN them, using the
  # within-imputation (complete-data) df as its nu_com. At kappa = 0 every draw
  # is identical, so bvar = 0, lambda = 0, and df_BR collapses to nu_com --
  # i.e. to the plain Satterthwaite df of the fixed-surface fit, which is the
  # right limit and is asserted below against H2's published row.
  ok2   <- all(is.finite(se2)) && all(is.finite(dfs))
  tvar2 <- if (ok2) mean(se2^2) + (1 + 1/Meff) * bvar else NA_real_
  df_br <- NA_real_
  if (ok2) {
    nu_com <- mean(dfs)
    lam    <- (1 + 1/Meff) * bvar / tvar2
    if (!is.finite(lam) || lam <= 1e-12) {
      # lambda = 0 means there is NO between-imputation variance -- at kappa = 0
      # every draw is the posterior mean, so the 200 "imputations" are 200
      # copies of one dataset and nothing is imputed at all. The Barnard-Rubin
      # formula must not be applied here. Its nu_obs term carries the factor
      # (nu_com + 1)/(nu_com + 3), which is 0.852 at nu_com = 10.5, so the
      # formula returns 0.852 * nu_com rather than nu_com in the limit -- the
      # adjustment is derived for lambda > 0 and is discontinuous there. Taking
      # it literally made the kappa = 0 endpoint of this frontier disagree with
      # H2's published fixed-surface row (df 8.97 against 10.53) while the
      # standard error agreed to five decimals, which is what the endpoint
      # check below caught. With no missing information the degrees of freedom
      # ARE the complete-data degrees of freedom.
      df_br <- nu_com
    } else {
      nu_old <- (Meff - 1) / lam^2
      nu_obs <- (nu_com + 1) / (nu_com + 3) * nu_com * (1 - lam)
      df_br  <- 1 / (1/nu_old + 1/nu_obs)
    }
  }
  t2 <- if (ok2) qbar/sqrt(tvar2) else NA_real_

  c(kappa = kap, estimate = qbar, se = sqrt(tvar), ubar = ubar, bvar = bvar,
    fmi = 1 - ubar/tvar, z = qbar/sqrt(tvar),
    p = 2*pnorm(-abs(qbar/sqrt(tvar))), n = max(nn),
    se_cr2 = if (ok2) sqrt(tvar2) else NA_real_, df_br = df_br, t_cr2 = t2,
    p_cr2 = if (ok2) 2*pt(-abs(t2), df_br) else NA_real_,
    crit_cr2 = if (ok2) qt(0.975, df_br) else NA_real_)
}

message("\n-- Stage 1: tracing the precision frontier --")
front <- list()
for (outc in names(OUTCOMES)) {
  for (kap in KAPPA_GRID) {
    r <- pool_at(kap, outc)
    front[[length(front)+1]] <- data.frame(outcome = outc, t(r))
    cat(sprintf("  %-9s kappa=%.2f  beta=%+.5f  se=%.5f  z=%+.3f  p=%.4f  FMI=%.3f\n",
                outc, kap, r[["estimate"]], r[["se"]], r[["z"]], r[["p"]], r[["fmi"]]))
  }
}
frontier <- do.call(rbind, front)
write_csv(frontier, file.path(dir_out, "design_frontier.csv"))

## ---- validate the two endpoints against the published H2 rows ---------------
if (file.exists(path_tab)) {
  tab <- read_csv(path_tab, show_col_types = FALSE)
  # Defensive row selection: H2's table has gained columns over time, so filter
  # only on the columns that are actually present rather than assuming a schema.
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
    f1 <- frontier %>% filter(outcome == o, kappa == 1)
    f0 <- frontier %>% filter(outcome == o, kappa == 0)
    p1 <- pick(o, "change_lpg_bayes_MI"); p0 <- pick(o, "change_lpg_bayes")
    if (!is.null(p1))
      chk(SCRIPT, sprintf("kappa=1 reproduces published MI row (%s)", o),
          abs(f1$estimate - p1$estimate) < 5e-4 && abs(f1$se - p1$se) < 5e-4,
          sprintf("frontier %.5f/%.5f vs table %.4f/%.4f",
                  f1$estimate, f1$se, p1$estimate, p1$se))
    else
      chk_warn(SCRIPT, sprintf("kappa=1 endpoint cross-checked against H2 (%s)", o),
               FALSE, "change_lpg_bayes_MI row not found in health_effects_table.csv")
    if (!is.null(p0))
      chk(SCRIPT, sprintf("kappa=0 reproduces published point-surface row (%s)", o),
          abs(f0$estimate - p0$estimate) < 5e-4 && abs(f0$se - p0$se) < 5e-4,
          sprintf("frontier %.5f/%.5f vs table %.4f/%.4f",
                  f0$estimate, f0$se, p0$estimate, p0$se))
    # The CR2 leg has to land on H2's published CR2 row as well, or the
    # frontier and the results table would be quoting two different inference
    # procedures under the same name. kappa=0 is the sharpest test: there the
    # 200 draws are identical, so df_BR must collapse exactly to H2's
    # Satterthwaite df and se_cr2 to H2's se_cr2.
    if (!is.null(p0) && "se_cr2" %in% names(p0) && is.finite(f0$se_cr2))
      chk(SCRIPT, sprintf("kappa=0 reproduces published CR2 row (%s)", o),
          abs(f0$se_cr2 - as.numeric(p0$se_cr2)) < 5e-4 &&
          (!("df_satt" %in% names(p0)) ||
             abs(f0$df_br - as.numeric(p0$df_satt)) < 1e-2),
          sprintf("frontier se_cr2 %.5f df %.3f vs table %.4f / %.4f",
                  f0$se_cr2, f0$df_br, as.numeric(p0$se_cr2), as.numeric(p0$df_satt)))
    if (!is.null(p1) && "se_cr2" %in% names(p1) && is.finite(f1$se_cr2))
      chk(SCRIPT, sprintf("kappa=1 reproduces published CR2 MI row (%s)", o),
          abs(f1$se_cr2 - as.numeric(p1$se_cr2)) < 5e-4,
          sprintf("frontier se_cr2 %.5f df_BR %.3f vs table %.4f / %.4f",
                  f1$se_cr2, f1$df_br, as.numeric(p1$se_cr2), as.numeric(p1$df_satt)))
    else
      chk_warn(SCRIPT, sprintf("kappa=1 CR2 endpoint cross-checked against H2 (%s)", o),
               FALSE, "change_lpg_bayes_MI row (with CR2 columns) not found in health_effects_table.csv")
    if (is.null(p0))
      chk_warn(SCRIPT, sprintf("kappa=0 endpoint cross-checked against H2 (%s)", o),
               FALSE, "change_lpg_bayes row not found in health_effects_table.csv")
  }
} else {
  chk_warn(SCRIPT, "health_effects_table.csv present for endpoint validation",
           FALSE, "not found -- endpoints NOT cross-checked; run H2 first")
}
for (o in names(OUTCOMES)) {
  fo <- frontier %>% filter(outcome == o) %>% arrange(kappa)
  # Descriptive: shrinking the draws toward their mean need not make |z| fall
  # monotonically (the exposure enters through plogis and every kappa is a
  # fresh set of regressions). kappa* below is located without assuming it.
  chk_warn(SCRIPT, sprintf("|z| is monotone decreasing in kappa (%s)", o),
      all(diff(abs(fo$z)) <= 1e-6),
      sprintf("|z| from %.3f at kappa=0 to %.3f at kappa=1",
              abs(fo$z[1]), abs(fo$z[nrow(fo)])))
  chk(SCRIPT, sprintf("FMI at kappa=1 is in (0,1) (%s)", o),
      { v <- fo$fmi[fo$kappa == 1]; is.finite(v) && v > 0 && v < 1 },
      sprintf("FMI = %.3f (Rubin: 1 - ubar/tvar)", fo$fmi[fo$kappa == 1]))
}

## ---- kappa*: the significance crossing, without assuming monotonicity -------
# kappa* is defined as the LARGEST kappa in [0, 1] at which the association is
# significant -- the least improvement in calibration precision that suffices.
# The frontier grid is scanned for the largest grid kappa that is significant;
# kappa* is then refined by bisection between it and the next grid point (where
# significance is lost). If the grid shows more than one sign change, the
# frontier is not monotone; the number of crossings is recorded and warned on,
# and the definition above still picks the right crossing.
locate_kstar <- function(outc, gfun, gcol_fun) {
  fo <- frontier[frontier$outcome == outc, , drop = FALSE]
  fo <- fo[order(fo$kappa), , drop = FALSE]
  gv <- gcol_fun(fo)
  if (!all(is.finite(gv))) return(list(ok = FALSE))
  sig <- gv > 0
  ncross <- sum(diff(sig) != 0)
  g0 <- gv[fo$kappa == 0][1]
  if (!any(sig)) return(list(ok = TRUE, reachable = FALSE, ks = NA_real_, g0 = g0, ncross = ncross))
  j <- max(which(sig))
  if (j == nrow(fo)) return(list(ok = TRUE, reachable = TRUE, ks = fo$kappa[j], g0 = g0, ncross = ncross))
  lo <- fo$kappa[j]; hi <- fo$kappa[j + 1]
  for (it in 1:10) { mid <- (lo + hi)/2; if (gfun(mid) > 0) lo <- mid else hi <- mid }
  list(ok = TRUE, reachable = TRUE, ks = (lo + hi)/2, g0 = g0, ncross = ncross)
}

message("\n-- Stage 1b: locating kappa* (the z = 1.96 crossing) --")
kstar_rows <- list()
for (outc in names(OUTCOMES)) {
  L1 <- locate_kstar(outc, function(k) abs(pool_at(k, outc)[["z"]]) - Z_CRIT,
                     function(fo) abs(fo$z) - Z_CRIT)
  chk_warn(SCRIPT, sprintf("conventional frontier crosses significance at most once (%s)", outc),
           L1$ncross <= 1, sprintf("%d sign change(s) on the kappa grid", L1$ncross))
  if (!L1$reachable) {
    cat(sprintf("  %-9s UNREACHABLE: no kappa in [0,1] gives |z| >= %.2f (|z| = %.3f at kappa = 0)\n",
                outc, Z_CRIT, L1$g0 + Z_CRIT))
    kstar_rows[[length(kstar_rows)+1]] <- data.frame(
      outcome = outc, reachable = FALSE, kappa_star = NA_real_,
      z_at_zero = L1$g0 + Z_CRIT, estimate = NA_real_, se = NA_real_, fmi = NA_real_,
      n_crossings = L1$ncross)
  } else {
    ks <- L1$ks
    r  <- pool_at(ks, outc)
    cat(sprintf("  %-9s kappa* = %.4f  (beta=%+.5f se=%.5f z=%+.4f FMI=%.3f)\n",
                outc, ks, r[["estimate"]], r[["se"]], r[["z"]], r[["fmi"]]))
    kstar_rows[[length(kstar_rows)+1]] <- data.frame(
      outcome = outc, reachable = TRUE, kappa_star = ks, z_at_zero = L1$g0 + Z_CRIT,
      estimate = r[["estimate"]], se = r[["se"]], fmi = r[["fmi"]],
      n_crossings = L1$ncross)
  }
}
kstar <- do.call(rbind, kstar_rows)

## ---- kappa* under the PRIMARY inference rule (CR2 / Satterthwaite) ----------
# The criterion is |t| > qt(0.975, df_BR) rather than |z| > 1.96. CR2 standard
# errors are usually, but not necessarily, larger than the conventional ones,
# so kappa*(CR2) is usually the more demanding target; the ordering is checked
# (as a warning), not assumed.
message("\n-- Stage 1c: locating kappa* under CR2/Satterthwaite (primary inference) --")
kstar$kappa_star_cr2 <- NA_real_
kstar$reachable_cr2  <- NA
kstar$t_at_zero_cr2  <- NA_real_
kstar$crit_at_zero_cr2 <- NA_real_
kstar$n_crossings_cr2 <- NA_integer_
for (outc in names(OUTCOMES)) {
  i  <- which(kstar$outcome == outc)
  L2 <- locate_kstar(outc,
                     function(k) { r <- pool_at(k, outc); abs(r[["t_cr2"]]) - r[["crit_cr2"]] },
                     function(fo) abs(fo$t_cr2) - fo$crit_cr2)
  if (!isTRUE(L2$ok)) {
    cat(sprintf("  %-9s CR2 unavailable (clubSandwich missing or failed)\n", outc))
    next
  }
  f0 <- frontier[frontier$outcome == outc & frontier$kappa == 0, , drop = FALSE][1, ]
  kstar$t_at_zero_cr2[i]    <- abs(f0$t_cr2)
  kstar$crit_at_zero_cr2[i] <- f0$crit_cr2
  kstar$n_crossings_cr2[i]  <- L2$ncross
  chk_warn(SCRIPT, sprintf("CR2 frontier crosses significance at most once (%s)", outc),
           L2$ncross <= 1, sprintf("%d sign change(s) on the kappa grid", L2$ncross))
  if (!L2$reachable) {
    cat(sprintf("  %-9s UNREACHABLE under CR2: no kappa in [0,1] reaches it (|t| = %.3f, crit %.3f at kappa = 0)\n",
                outc, abs(f0$t_cr2), f0$crit_cr2))
    kstar$reachable_cr2[i] <- FALSE
  } else {
    ks2 <- L2$ks
    r2  <- pool_at(ks2, outc)
    cat(sprintf("  %-9s kappa*(CR2) = %.4f  (beta=%+.5f se=%.5f t=%+.4f df=%.1f)\n",
                outc, ks2, r2[["estimate"]], r2[["se_cr2"]], r2[["t_cr2"]], r2[["df_br"]]))
    kstar$reachable_cr2[i]  <- TRUE
    kstar$kappa_star_cr2[i] <- ks2
  }
}
write_csv(kstar, file.path(dir_out, "design_kappa_star.csv"))

KS <- kstar$kappa_star[kstar$outcome == "infant"]
# kappa* is NA when the endpoint is already significant at kappa = 1 or cannot
# reach significance even at kappa = 0. That is a finding, not a pipeline error:
# warn, keep KS as a scalar NA, and let every use below treat NA as "not reached".
KS <- if (length(KS) == 1 && is.finite(KS)) KS else NA_real_
chk_warn(SCRIPT, "kappa* located for the infant endpoint",
    length(KS) == 1 && is.finite(KS) && KS > 0 && KS < 1,
    sprintf("kappa* = %.4f (posterior SD must fall to %.0f%% of current, a %.1fx precision gain)",
            KS, 100*KS, 1/KS))
chk_warn(SCRIPT, "neonatal endpoint reachable at some calibration precision",
         isTRUE(kstar$reachable[kstar$outcome == "neonatal"]),
         sprintf("best possible |z| = %.3f at kappa = 0",
                 kstar$z_at_zero[kstar$outcome == "neonatal"]))

KS2 <- kstar$kappa_star_cr2[kstar$outcome == "infant"]
# Descriptive, not guaranteed: CR2 is usually but not always larger than the
# CR1 (conventional) cluster-robust SE, so the ordering warns rather than fails.
chk_warn(SCRIPT, "CR2 standard errors are never smaller than the conventional (CR1) SE on the traced frontier",
    all(is.na(frontier$se_cr2) | frontier$se_cr2 >= frontier$se - 1e-10),
    sprintf("%d of %d frontier points have se_cr2 >= conventional se",
            sum(is.na(frontier$se_cr2) | frontier$se_cr2 >= frontier$se - 1e-10),
            nrow(frontier)))
chk_warn(SCRIPT, "kappa*(CR2) is no larger than kappa* under the conventional SE",
    !(length(KS2) == 1 && is.finite(KS2) && length(KS) == 1 && is.finite(KS)) ||
      KS2 <= KS + 1e-6,
    if (length(KS2) == 1 && is.finite(KS2))
      sprintf("kappa*(CR2) = %.4f vs kappa*(conventional) = %.4f", KS2, KS)
    else "kappa*(CR2) not located (infant endpoint unreachable under CR2)")
chk(SCRIPT, "no CR2 failures while tracing the frontier",
    CR2_FAILURES == 0L,
    sprintf("clubSandwich failed on %d fit(s); CR2 available = %s",
            CR2_FAILURES, CR2_OK))

}  # end DESIGN_FRONTIER (Part 1)
## ==============================================================================
## PART 2 -- where the posterior variance comes from
## ==============================================================================
HAVE_FITS <- file.exists(path_fitA) && file.exists(path_fitI) && file.exists(path_proxy)
# Never let a previous run's decomposition stand in for this run's.
if (!DESIGN_FRONTIER)
  unlink(file.path(dir_out, c("design_summary.csv", "design_variance_decomposition.csv",
                              "design_vc_plugin_penalty.csv")))
chk_warn(SCRIPT, "calibration fits + district proxy available for design mapping",
         HAVE_FITS,
         if (HAVE_FITS) "brms_me_*.rds and district_exposure_proxy.csv found"
         else "missing -- the variance decomposition is SKIPPED")

if (HAVE_FITS) {

proxy <- read_csv(path_proxy, show_col_types = FALSE)

# Post-warmup draws for a named stan parameter, without requiring rstan to be
# attached: brms stores the stanfit in $fit and the raw sample lists in its
# "sim" attribute, with warmup2 giving the number of warmup iterations kept.
post_of <- function(fit) {
  s <- attr(fit$fit, "sim"); w <- s$warmup2[1]
  function(nm) unlist(lapply(s$samples, function(ch) ch[[nm]][(w+1):length(ch[[nm]])]))
}

# Posterior covariance of (a, b, u_1..u_S) for a Bayesian LMM with KNOWN
# variance components: y_d = a + b x_d + u_s(d) + e_d, with
#   Var(e_d) = sigma^2 + y_se_d^2 + b^2 x_se_d^2   (equation error + both
#                                                   sampling errors)
#   u_s ~ N(0, psi^2)  (a proper prior, so it enters the precision as I/psi^2)
post_cov <- function(x, om2, state, psi) {
  st <- sort(unique(state)); S <- length(st)
  Z  <- cbind(1, x, model.matrix(~ 0 + factor(state, levels = st)))
  P  <- crossprod(Z, diag(1/om2) %*% Z)
  P[3:(S+2), 3:(S+2)] <- P[3:(S+2), 3:(S+2)] + diag(S)/psi^2
  list(Sig = solve(P), st = st, S = S)
}
# Conditioning on the posterior MEDIAN of (sigma, psi) discards the variance
# components' own uncertainty; averaging Sigma over posterior draws of
# (sigma, psi) keeps it. How much that is worth is NOT a fixed number. It is
# near zero at the calibration districts themselves, and matters mainly for
# TARGET districts in states the calibration survey never visited, where a
# fresh state effect enters as psi^2 and the right skew of psi's posterior
# makes median(psi)^2 much smaller than mean(psi^2). It is therefore quantified
# per leg in design_vc_plugin_penalty.csv rather than asserted here.
# nd = ALL draws. An earlier version averaged over 200 evenly spaced draws,
# which is not enough: on the 2015 leg, 396 of 627 target districts lie in
# states ACCESS did not visit, so their calibration-side variance is dominated
# by psi^2, and with only six calibration states psi is weakly identified and
# right-skewed (posterior ~0.01-0.57). The Monte-Carlo SE of mean(psi^2) over
# 200 draws (~0.005) moved the NFHS-side share of the 2015 posterior variance
# by +/- 3-4 percentage points between otherwise identical runs on different
# machines (40% vs 48%). Each post_cov() call inverts an (S+2) x (S+2) matrix,
# so averaging over all posterior draws (48,000 at 4 chains x 24000 iter, 12000
# warmup) costs seconds and removes the noise.
avg_cov <- function(x, yse, xse, state, sig_d, psi_d, b, nd = length(sig_d)) {
  ii <- round(seq(1, length(sig_d), length.out = nd))
  acc <- NULL; st <- NULL; S <- NULL
  for (j in ii) {
    om2 <- sig_d[j]^2 + yse^2 + (b^2)*xse^2
    r <- post_cov(x, om2, state, psi_d[j])
    acc <- if (is.null(acc)) r$Sig else acc + r$Sig; st <- r$st; S <- r$S
  }
  list(Sig = acc/length(ii), st = st, S = S, psi2 = mean(psi_d[ii]^2))
}
# Predictive variance of (a + b x0 + u_s) for a target district. A district in a
# state the calibration survey covered gets the ESTIMATED state effect; a
# district in an uncovered state gets the population-level line plus a fresh
# state effect (brms sample_new_levels = "gaussian"), hence the + psi^2.
pv <- function(Sig, S, psi2, x0, sidx) {
  vapply(seq_along(x0), function(i) {
    j <- sidx[i]
    if (is.na(j)) { cc <- c(1, x0[i], rep(0, S)); as.numeric(t(cc) %*% Sig %*% cc) + psi2 }
    else { cc <- c(1, x0[i], as.numeric(seq_len(S) == j)); as.numeric(t(cc) %*% Sig %*% cc) }
  }, numeric(1))
}

# The brms fits carry each survey's own state spellings ("Chattisgarh",
# "Uttarakhaand"), while district_exposure_proxy.csv carries the canonical names
# 06_stacking_prediction.R assigns. Match on a canonical form; keep the raw fit
# names for the r_state_name[...] posterior lookup.
canon_st <- function(x) {
  x <- stringr::str_squish(tools::toTitleCase(tolower(as.character(x))))
  dplyr::recode(x, "Chattisgarh" = "Chhattisgarh", "Uttarakhaand" = "Uttarakhand",
                "Uttaranchal" = "Uttarakhand", "Nct of Delhi" = "Delhi",
                "Nct Of Delhi" = "Delhi", "Orissa" = "Odisha",
                "Pondicherry" = "Puducherry", "Jammu and Kashmir" = "Jammu & Kashmir",
                .default = x)
}
SE_CAP_05 <- local({
  f <- file.path(dir_out, "me_se_diagnostics.csv")
  v <- if (file.exists(f)) { m <- read.csv(f); m$value[m$quantity == "se_cap"] } else numeric(0)
  if (length(v) == 1 && is.finite(v)) v else 2
})
legsetup <- function(fitfile, draws, distid, survey, se_col) {
  fit <- readRDS(fitfile); po <- post_of(fit)
  ad <- po("b_Intercept"); bd <- po("bsp_mex_obsx_se")
  psid <- po("sd_state_name__Intercept"); sgd <- po("sigma")
  a <- mean(ad); b <- mean(bd)
  d <- fit$data; st <- sort(unique(as.character(d$state_name)))
  U <- vapply(st, function(z) mean(po(paste0("r_state_name[", gsub(" ", ".", z),
                                             ",Intercept]"))), numeric(1))
  L <- qlogis(draws); mL <- colMeans(L); vObs <- apply(L, 2, var)
  ids <- as.character(as.numeric(distid))
  ps  <- proxy %>% filter(survey == !!survey) %>%
           mutate(did = as.character(as.numeric(district)))
  k <- match(ids, ps$did)
  tstate <- ps$state[k]; nhh <- ps$n_hh[k]
  sidx <- match(canon_st(tstate), canon_st(st))
  ub <- ifelse(is.na(sidx), 0, U[sidx]); ub[is.na(ub)] <- 0
  # Recover the target district's NFHS predictor by inverting the calibration.
  # Exact to O(1e-3): the across-draw mean of a fresh gaussian state effect over
  # ~48,000 draws has SE ~ psi/sqrt(48000).
  x0   <- (mL - a - ub)/b
  r <- avg_cov(d$x_obs, d$y_se, d$x_se, as.character(d$state_name), sgd, psid, b)
  Vcal <- pv(r$Sig, r$S, r$psi2, x0, sidx)     # calibration-side variance
  # The same predictive variance, but with the variance components FROZEN at
  # their posterior medians instead of averaged over draws. Carried so the cost
  # of the plug-in shortcut is measured (below) rather than asserted.
  sig_med  <- median(sgd); psi_med <- median(psid)
  om2_med  <- sig_med^2 + d$y_se^2 + (b^2)*d$x_se^2
  r_med    <- post_cov(d$x_obs, om2_med, as.character(d$state_name), psi_med)
  Vcal_med <- pv(r_med$Sig, r_med$S, psi_med^2, x0, sidx)
  # sigma is summarised TWO ways in this manuscript and the numbers differ:
  #   sigma       = sqrt(mean(sgd^2)), the posterior ROOT-MEAN-SQUARE. This is the
  #                 summary the design algebra needs, because sigma enters only as
  #                 sigma^2 in om2 below; it is what SI Section S4 quotes.
  #   sigma_mean  = mean(sgd), the posterior MEAN. This is brms' own summary,
  #                 which 05_correction.R writes to calibration_parameters.csv as
  #                 sd_residual and Table S3 prints.
  # Both are carried and both are exported, so neither can be quoted as the other.
  list(a = a, b = b, b2 = mean(bd^2), psi = sqrt(mean(psid^2)), sigma = sqrt(mean(sgd^2)),
       sigma_mean = mean(sgd),
       d = d, st = st, x0 = x0, tstate = tstate, sidx = sidx, nhh = nhh,
       Sig = r$Sig, psi2 = r$psi2, Vcal = Vcal, vObs = vObs,
       Vcal_med = Vcal_med, sig_med = sig_med, psi_med = psi_med,
       psi_rms = sqrt(mean(psid^2)),
       F0 = mean(vObs) - mean(Vcal),           # NFHS-side, design-invariant
       # Typical reference-side SE, excluding a district whose SE was set to the
       # pre-fit cap (05_correction.R): that value is a bound, not a measurement,
       # and one such district would otherwise carry ~11% of the mean y_se^2.
       yse_ref = sqrt(mean(d$y_se[d$y_se < SE_CAP_05 - 1e-9]^2)), xse_ref = sqrt(mean(d$x_se^2)),
       neff = median(1/(d$y_se^2 * plogis(d$y)*(1-plogis(d$y)))),
       D = nrow(d), S = length(st), se_col = se_col)
}

message("\n-- Stage 2: decomposing the corrected-exposure posterior variance --")
A <- legsetup(path_fitA, pd$y2015, pd$districts_2015, "NFHS4", "lpg_2015_rural_wt_se")
I <- legsetup(path_fitI, pd$y2019, pd$districts_2019, "NFHS5", "lpg_2019_rural_wt_se")
# The number of target districts given an estimated state effect must equal the
# number 05_correction.R placed in calibration states; a mismatch means state
# names failed to match and those districts were treated as uncovered.
.sup <- tryCatch(readr::read_csv(file.path(dir_out, "calibration_support_summary.csv"),
                                 show_col_types = FALSE), error = function(e) NULL)
if (!is.null(.sup))
  chk_warn(SCRIPT, "districts with an estimated state effect match 05's calibration-state counts",
    sum(!is.na(A$sidx)) == .sup$n_state_support_2015 && sum(!is.na(I$sidx)) == .sup$n_state_support_2019,
    sprintf("22: %d / %d; 05: %d / %d", sum(!is.na(A$sidx)), sum(!is.na(I$sidx)),
            .sup$n_state_support_2015, .sup$n_state_support_2019))
BASE <- mean(A$vObs) + mean(I$vObs)

vdec <- data.frame(
  leg          = c("2015 (NFHS-4 ~ ACCESS)", "2019 (NFHS-5 ~ IRES)"),
  calib_districts = c(A$D, I$D), calib_states = c(A$S, I$S),
  sigma        = c(A$sigma, I$sigma),          # posterior RMS (back-compatible name)
  sigma_rms      = c(A$sigma, I$sigma),        # ... the same value, explicitly named
  sigma_postmean = c(A$sigma_mean, I$sigma_mean),  # brms' summary; Table S3's number
  psi = c(A$psi, I$psi),
  slope_b      = c(A$b, I$b),
  yse_ref      = c(A$yse_ref, I$yse_ref),
  neff_hh_per_district = c(A$neff, I$neff),
  targets      = c(length(A$x0), length(I$x0)),
  targets_covered_state = c(sum(!is.na(A$sidx)), sum(!is.na(I$sidx))),
  var_observed = c(mean(A$vObs), mean(I$vObs)),
  var_calibration_side = c(mean(A$Vcal), mean(I$Vcal)),
  var_nfhs_side = c(A$F0, I$F0),
  pct_nfhs_side = 100*c(A$F0/mean(A$vObs), I$F0/mean(I$vObs)))
print(vdec, digits = 3)
write_csv(vdec, file.path(dir_out, "design_variance_decomposition.csv"))

# ---- what does plugging in at the posterior median actually cost? -----------
# SI Section S4 says the closed form averages over posterior draws of the
# variance components "rather than plugged in at their medians". That sentence
# is generated from THIS file, so the number it quotes has to be computed, not
# remembered. Reported on the SD scale (the scale a design is quoted in) and on
# the variance scale (the scale the algebra propagates), and split by whether
# the target district sits in a state the calibration survey covered: uncovered
# states draw a FRESH state effect, so psi enters as psi^2, and that is where
# the whole discrepancy lives.
pen_leg <- function(L, lab) {
  sd_avg <- mean(sqrt(L$Vcal)); sd_med <- mean(sqrt(L$Vcal_med))
  cv  <- !is.na(L$sidx)
  split_pct <- function(sel) if (!any(sel)) NA_real_ else
    100 * (mean(sqrt(L$Vcal[sel])) - mean(sqrt(L$Vcal_med[sel]))) / mean(sqrt(L$Vcal[sel]))
  data.frame(
    leg = lab,
    targets = length(L$x0),
    targets_covered_state   = sum(cv),
    targets_uncovered_state = sum(!cv),
    sigma_median = L$sig_med, sigma_rms = L$sigma,
    psi_median   = L$psi_med, psi_rms   = L$psi_rms,
    sd_posterior_averaged = sd_avg,
    sd_plugin_median      = sd_med,
    pct_under_sd  = 100 * (sd_avg - sd_med) / sd_avg,
    pct_under_var = 100 * (mean(L$Vcal) - mean(L$Vcal_med)) / mean(L$Vcal),
    pct_under_sd_covered_states   = split_pct(cv),
    pct_under_sd_uncovered_states = split_pct(!cv))
}
vcpen <- rbind(pen_leg(A, "2015 (NFHS-4 ~ ACCESS)"), pen_leg(I, "2019 (NFHS-5 ~ IRES)"))
cat("\n  cost of freezing (sigma, psi) at their posterior medians:\n")
print(vcpen[, c("leg", "targets_uncovered_state", "psi_median", "psi_rms",
                "sd_posterior_averaged", "sd_plugin_median",
                "pct_under_sd", "pct_under_var",
                "pct_under_sd_covered_states", "pct_under_sd_uncovered_states")],
      digits = 3, row.names = FALSE)
write_csv(vcpen, file.path(dir_out, "design_vc_plugin_penalty.csv"))

chk(SCRIPT, "plug-in penalty computed on both legs",
    nrow(vcpen) == 2 && all(is.finite(vcpen$pct_under_sd)) &&
      all(is.finite(vcpen$pct_under_var)),
    sprintf("SD scale: %.1f%% (2015), %.1f%% (2019)",
            vcpen$pct_under_sd[1], vcpen$pct_under_sd[2]))
# A NEGATIVE penalty is not an error -- the plug-in shortcut is not guaranteed
# to err downward -- but it would invert the framing of the S4 sentence, so it
# is surfaced rather than failed.
chk_warn(SCRIPT, "plug-in does not beat the averaged form by more than 5% of the SD",
    all(vcpen$pct_under_sd > -5),
    sprintf("most negative %.1f%%", min(vcpen$pct_under_sd)))
# RMS >= mean is guaranteed (Jensen); RMS >= median is not, so this only warns.
chk_warn(SCRIPT, "posterior RMS of psi >= its median on both legs",
    all(vcpen$psi_rms >= vcpen$psi_median - 1e-12),
    sprintf("rms/median = %.2f, %.2f",
            vcpen$psi_rms[1]/vcpen$psi_median[1],
            vcpen$psi_rms[2]/vcpen$psi_median[2]))
chk_warn(SCRIPT,
    "plug-in penalty is concentrated in uncovered-state target districts",
    all(is.na(vcpen$pct_under_sd_uncovered_states) |
        is.na(vcpen$pct_under_sd_covered_states) |
        vcpen$pct_under_sd_uncovered_states >= vcpen$pct_under_sd_covered_states - 1e-9),
    sprintf("covered %.1f%%/%.1f%% vs uncovered %.1f%%/%.1f%%",
            vcpen$pct_under_sd_covered_states[1], vcpen$pct_under_sd_covered_states[2],
            vcpen$pct_under_sd_uncovered_states[1], vcpen$pct_under_sd_uncovered_states[2]))

# ---- bind the two sigma numbers the manuscript prints -----------------------
# Table S3 prints sd_residual from calibration_parameters.csv (posterior MEAN);
# SI Section S4 quotes sigma from this file (posterior RMS). They are different
# summaries of one parameter, so the danger is not that they differ -- it is that
# one gets quoted as the other, or that the two files drift onto different fits.
# Recomputing the MEAN here from the same draws and matching it against 05's
# exported value makes that drift a visible check rather than a silent error.
.path_calpar <- file.path(dir_out, "calibration_parameters.csv")
if (file.exists(.path_calpar)) {
  cp <- read_csv(.path_calpar, show_col_types = FALSE)
  cp_mean <- function(pat) {
    r <- cp[grepl(pat, cp$calibration) & cp$parameter == "sd_residual", ]
    if (nrow(r)) as.numeric(r$estimate[1]) else NA_real_
  }
  m05 <- c(cp_mean("NFHS-4"), cp_mean("NFHS-5"))
  m22 <- c(A$sigma_mean, I$sigma_mean)
  cat("\n  residual SD (sigma), the two summaries this manuscript reports:\n")
  for (i in 1:2)
    cat(sprintf("    %-22s posterior mean %.4f (05 wrote %.4f)   posterior RMS %.4f\n",
                vdec$leg[i], m22[i], m05[i], vdec$sigma_rms[i]))
  chk(SCRIPT, "sigma posterior mean agrees with 05_correction.R's export",
      all(is.finite(m05)) && all(abs(m22 - m05) < 5e-3),
      sprintf("22 recomputes %.4f / %.4f; calibration_parameters.csv has %.4f / %.4f",
              m22[1], m22[2], m05[1], m05[2]))
  chk(SCRIPT, "sigma RMS >= posterior mean on both legs (Jensen)",
      all(vdec$sigma_rms >= vdec$sigma_postmean - 1e-8),
      sprintf("rms %.4f/%.4f vs mean %.4f/%.4f", vdec$sigma_rms[1], vdec$sigma_rms[2],
              vdec$sigma_postmean[1], vdec$sigma_postmean[2]))
} else {
  chk_warn(SCRIPT, "sigma posterior mean cross-checked against 05's export",
           FALSE, "calibration_parameters.csv not found -- cross-check SKIPPED")
}

KAPPA_FLOOR <- sqrt((A$F0 + I$F0)/BASE)
cat(sprintf("\n  kappa_floor (perfect validation survey, NFHS sample sizes as they are) = %.4f\n",
            KAPPA_FLOOR))
if (DESIGN_FRONTIER) cat(sprintf("  kappa* (infant)                                                        = %.4f\n", KS))

## ---- checks on the decomposition --------------------------------------------
# The closed-form calibration variance is an approximation. If it exceeds the
# observed posterior variance in more than 10% of districts on either leg, the
# decomposition (and kappa_floor built on it) is flagged invalid for this run:
# it is written as decomposition_valid = 0 and the manuscript then reports only
# the direct simulation below, which does not use the decomposition.
DECOMP_OK <- TRUE
for (z in list(list(A, "2015"), list(I, "2019"))) {
  L <- z[[1]]; rs <- L$vObs - L$Vcal
  if (!(mean(rs > 0) > 0.9)) DECOMP_OK <- FALSE
  chk_warn(SCRIPT, sprintf("closed-form calibration variance <= observed, %s leg", z[[2]]),
      mean(rs > 0) > 0.9,
      sprintf("%.0f%% of target districts have a positive NFHS-side residual", 100*mean(rs > 0)))
  ok <- is.finite(rs) & is.finite(L$nhh) & L$nhh > 0
  cr <- if (sum(ok) > 10) cor(rs[ok], 1/L$nhh[ok]) else NA_real_
  # Diagnostic correlations: with the empirical-Bayes input and SE-capped
  # (degenerate) districts these are informative but not structural, so they
  # warn rather than stop a publication run.
  chk_warn(SCRIPT, sprintf("NFHS-side residual rises as district n falls, %s leg", z[[2]]),
      is.finite(cr) && cr > 0,
      sprintf("cor(residual, 1/n_hh) = %+.3f", cr))
  chk_warn(SCRIPT, sprintf("closed-form tracks the real posterior, %s leg", z[[2]]),
      isTRUE(cor(L$Vcal, L$vObs) > 0.6),
      sprintf("cor(model variance, observed posterior variance) = %+.3f",
              cor(L$Vcal, L$vObs)))
}

# INDEPENDENT check: the NFHS-side residual should equal b^2 * x_se^2 computed
# from NFHS's OWN district standard errors -- the quantity 05_correction.R feeds
# to me(x_obs, x_se). This is the decomposition's strongest validation, because
# nothing in the residual calculation used those SEs.
clampf <- function(p) pmin(pmax(p, 1e-4), 1 - 1e-4)
for (z in list(list(A, "2015", "lpg_2015_rural_wt", "lpg_2015_rural_wt_se"),
               list(I, "2019", "lpg_2019_rural_wt", "lpg_2019_rural_wt_se"))) {
  L <- z[[1]]
  if (all(c(z[[3]], z[[4]]) %in% names(corr))) {
    cd  <- as.character(as.numeric(corr$district))
    ids <- as.character(as.numeric(if (z[[2]] == "2015") pd$districts_2015 else pd$districts_2019))
    k   <- match(ids, cd)
    est <- clampf(corr[[z[[3]]]][k]); se <- corr[[z[[4]]]][k]
    # 05 now uses the same floor (SE_FLOOR = 1e-4) and cap (2) in the fit and
    # in the application path.
    xse <- pmin(pmax(se/(est*(1-est)), 1e-4), 2)          # 05's SE formula and bounds
    # 05 predicts from the empirical-Bayes posterior of the true NFHS value, so
    # the NFHS-side term is b^2 times its POSTERIOR variance (lambda * se^2).
    shf <- file.path(dir_out, "me_shrinkage.csv")
    if (file.exists(shf)) {
      sh <- read.csv(shf)
      sh <- sh[sh$surface == paste0("lpg_", z[[2]]), ]
      psd <- sh$x_post_sd[match(ids, as.character(as.numeric(sh$district)))]
      xse <- ifelse(is.finite(psd), psd, xse)
    }
    # E[b^2], not (E b)^2: 05 adds b_draw * x_post_sd * z draw by draw.
    pred <- mean(L$b2*xse^2, na.rm = TRUE)
    # WARN, not FAIL. On the 2019 leg these two routes to the NFHS-side variance
    # disagree by a factor of about 1.5 in the October 2026 run, and that disagreement is a REPORTED
    # RESULT: the main text says the 2019 share "should be read as approximate"
    # and gives the factor. A publication run stops on FAIL (STRICT_CHECKS), and
    # a known, disclosed limitation must not be the thing that stops it -- nor
    # should the tolerance be widened until it silently passes, which would hide
    # the very quantity the manuscript discusses. So it warns, loudly, forever.
    chk_warn(SCRIPT, sprintf("NFHS-side floor matches b^2 * posterior var of the NFHS input, %s", z[[2]]),
        is.finite(pred) && abs(pred - L$F0)/max(L$F0, 1e-9) < 0.35,
        sprintf("empirical residual %.4f vs b^2*mean(posterior var) %.4f (ratio %.2f)",
                L$F0, pred, pred/L$F0))
  } else {
    chk_warn(SCRIPT, sprintf("NFHS district SEs available for the floor check, %s", z[[2]]),
             FALSE, sprintf("columns %s / %s not in corrected_nfhs_districts.rds", z[[3]], z[[4]]))
  }
}

if (!DESIGN_FRONTIER) {
  # Decomposition only: the shares of each corrected value's posterior variance
  # that come from the NFHS side and the calibration side (descriptive; no
  # health model, no imputation).
  summ <- data.frame(
    quantity = c("var_nfhs_side_share_2015", "var_nfhs_side_share_2019",
                 "n_study_districts", "decomposition_valid"),
    value = c(100*A$F0/mean(A$vObs), 100*I$F0/mean(I$vObs), length(A$x0), as.numeric(DECOMP_OK)))
  write_csv(summ, file.path(dir_out, "design_summary.csv"))
  print(summ, digits = 4)
} else {   # ---- frontier-dependent design results: disabled by default
## ---- validity of the scalar design device ---------------------------------
# Everything built on kappa_floor and the closed-form decomposition (the scalar
# comparison at kappa_floor, the transfer-design grid, the NFHS-multiplier sweep)
# is skipped when the decomposition failed its check or kappa_floor is not a
# finite positive number. The fixed-calibration scenario and the direct route do
# not use the decomposition and always run.
DESIGN_OK <- isTRUE(DECOMP_OK) && length(KAPPA_FLOOR) == 1 &&
  is.finite(KAPPA_FLOOR) && KAPPA_FLOOR > 0
chk_warn(SCRIPT, "scalar design device usable (decomposition valid, kappa_floor finite)",
         DESIGN_OK, sprintf("decomposition valid = %s; kappa_floor = %s",
                            DECOMP_OK, format(KAPPA_FLOOR)))
# A rejected floor is kept only as a diagnostic (kappa_floor_raw); everything
# downstream -- the figure, design_summary.csv's kappa_floor -- sees NA.
KAPPA_FLOOR_RAW <- KAPPA_FLOOR
if (!DESIGN_OK) KAPPA_FLOOR <- NA_real_

# Is the (conventional) association significant at a given kappa? The shortcut
# "significant iff kappa <= kappa*" is used only when the traced grid CONFIRMS
# that pattern -- every grid kappa at or below kappa* significant and every grid
# kappa above it not. Otherwise (several crossings, or significance that appears
# as kappa grows) each proposed kappa in [0, 1] is evaluated directly on the
# frontier (pool_at), cached by value. A kappa above 1 lies outside the traced
# frontier and is returned as NA (not evaluated), never assumed.
.fo_inf <- frontier[frontier$outcome == "infant", , drop = FALSE]
.sig_grid <- abs(.fo_inf$z) - Z_CRIT > 0
SHORTCUT_OK <- is.finite(KS) && all(is.finite(.sig_grid)) &&
  all(.sig_grid == (.fo_inf$kappa <= KS + 1e-12))
chk_warn(SCRIPT, "significance region is kappa <= kappa* on the traced grid (infant)",
         SHORTCUT_OK, if (SHORTCUT_OK) "confirmed on every grid point" else
           "not confirmed: proposed designs are evaluated directly on the frontier")
.sig_cache <- new.env()
sig_kappa <- function(k) {
  vapply(k, function(kk) {
    if (!is.finite(kk)) return(NA)
    if (kk > 1 + 1e-12) return(NA)
    if (SHORTCUT_OK) return(kk <= KS)
    key <- sprintf("%.6f", kk)
    if (is.null(.sig_cache[[key]]))
      .sig_cache[[key]] <- abs(pool_at(kk, "infant")[["z"]]) >= Z_CRIT
    .sig_cache[[key]]
  }, logical(1))
}

## ---- the floor tested directly, not through the scalar kappa ---------------
# kappa scales EVERY posterior deviation by one factor, and kappa_floor is set
# from AVERAGE district variances. A perfect validation survey does something
# structurally different: it removes the calibration-side error, which is
# shared across districts (one slope, one state effect per state), and leaves
# the NFHS-side error, which is independent district by district. The two can
# affect the health coefficient differently even at equal average variance. So
# the perfect-validation case is also simulated directly: the calibration is
# fixed at its posterior mean (a, b, state effects known), and each imputation
# carries only the NFHS-side error, sqrt(E[b^2]) * x_post_sd * z, independent
# across districts and rounds -- exactly the term 05_correction.R adds. The
# health model is refitted and pooled with the same CR2 / Barnard-Rubin
# machinery as the frontier. The kappa_floor row of the frontier is reported
# beside it, so the scalar approximation is checked rather than assumed.
floor_direct <- NULL
.shf <- file.path(dir_out, "me_shrinkage.csv")
if (file.exists(.shf)) {
  .sh <- read.csv(.shf)
  .psd <- function(surf, ids) {
    z <- .sh[.sh$surface == surf, ]
    z$x_post_sd[match(ids, as.character(as.numeric(z$district)))]
  }
  sd15 <- .psd("lpg_2015", id15); sd19 <- .psd("lpg_2019", id19)
  if (all(is.finite(sd15)) && all(is.finite(sd19))) {
    # Calibration fixed at its posterior mean: the shared part held at the point
    # surface, only the NFHS-side error b * x_post_sd * z left (correlated across
    # rounds as in 05), slope = E[b]; a second version uses sqrt(E[b^2]) (the RMS
    # slope) as a labelled sensitivity. The exposure is imputed conditional on
    # the outcome, as on the frontier.
    Pf15  <- mime_fixed(PR15, abs(A$b));   Pf19  <- mime_fixed(PR19, abs(I$b))
    Pfr15 <- mime_fixed(PR15, sqrt(A$b2)); Pfr19 <- mime_fixed(PR19, sqrt(I$b2))
    floor_direct <- do.call(rbind, lapply(names(OUTCOMES), function(outc) {
      rd <- pool_at(NA_real_, outc, pr15 = Pf15,  pr19 = Pf19,  tag = "fixed calibration (slope E[b])")
      rr <- pool_at(NA_real_, outc, pr15 = Pfr15, pr19 = Pfr19, tag = "fixed calibration (RMS slope)")
      rk <- if (DESIGN_OK) pool_at(KAPPA_FLOOR, outc) else
        c(estimate = NA_real_, se_cr2 = NA_real_, df_br = NA_real_, t_cr2 = NA_real_,
          p_cr2 = NA_real_, z = NA_real_, p = NA_real_, fmi = NA_real_)
      L3 <- list(rd, rr, rk)
      g  <- function(col) vapply(L3, function(x) unname(x[[col]]), numeric(1))
      data.frame(outcome = outc,
                 route = c("fixed: calibration fixed at posterior mean (slope E[b]), NFHS-side error only",
                           "fixed_rms: as above, slope scaled to sqrt(E[b^2]) (sensitivity)",
                           "scalar: frontier at kappa_floor"),
                 kappa = c(NA_real_, NA_real_, if (DESIGN_OK) KAPPA_FLOOR else NA_real_),
                 estimate = g("estimate"), se_cr2 = g("se_cr2"), df_br = g("df_br"),
                 t_cr2 = g("t_cr2"), p_cr2 = g("p_cr2"), z = g("z"), p = g("p"),
                 fmi = g("fmi"))
    }))
    write_csv(floor_direct, file.path(dir_out, "design_floor_direct.csv"))
    cat("\n  perfect validation survey, simulated directly vs the scalar kappa_floor:\n")
    print(as.data.frame(floor_direct), digits = 4, row.names = FALSE)
    fi_d <- floor_direct[floor_direct$outcome == "infant", ]
    chk_warn(SCRIPT, "fixed-calibration simulation agrees with the scalar kappa_floor on significance (infant, CR2)",
             nrow(fi_d) == 3 && all(is.finite(fi_d$p_cr2)) &&
               ((fi_d$p_cr2[1] < 0.05) == (fi_d$p_cr2[3] < 0.05)),
             sprintf("fixed p = %.3f (|t| %.2f); RMS-slope p = %.3f; scalar p = %.3f (|t| %.2f)",
                     fi_d$p_cr2[1], abs(fi_d$t_cr2[1]), fi_d$p_cr2[2],
                     fi_d$p_cr2[3], abs(fi_d$t_cr2[3])))
  }
}
if (is.null(floor_direct))
  chk_warn(SCRIPT, "direct perfect-validation simulation run", FALSE,
           "me_shrinkage.csv missing or incomplete -- floor not simulated directly")

## ==============================================================================
## PART 3 -- design mapping
## ==============================================================================
message("\n-- Stage 3: mapping kappa back to a survey design --")
grid <- NULL; nmult_tab <- NULL; info_gain <- NULL
if (DESIGN_OK) {

# State coverage schedule: a survey fielding in S states is assumed to take the
# S states holding the most study districts, which is what both real surveys did.
covfun <- function(sv) {
  tb <- sort(table(proxy$state[proxy$survey == sv]), decreasing = TRUE)
  cs <- cumsum(tb)/sum(tb)
  function(S) if (S >= length(tb)) 1 else as.numeric(cs[S])
}
covA <- covfun("NFHS4"); covI <- covfun("NFHS5")
chk(SCRIPT, "coverage schedule reproduces the real surveys' district coverage",
    abs(covA(A$S) - mean(!is.na(A$sidx))) < 0.12 &&
    abs(covI(I$S) - mean(!is.na(I$sidx))) < 0.12,
    sprintf("S=%d -> %.3f (actual %.3f); S=%d -> %.3f (actual %.3f)",
            A$S, covA(A$S), mean(!is.na(A$sidx)),
            I$S, covI(I$S), mean(!is.na(I$sidx))))

# Calibration-side variance under a hypothetical design. D districts drawn from
# the calibration survey's own x distribution, spread over S states; the
# reference-side sampling SE scales as 1/sqrt(m) in households per district;
# sfac rescales the instrument residual sigma.
sim_cal <- function(L, D, S, mratio, sfac, covf, nrep = 6) {
  yse <- L$yse_ref/sqrt(mratio); sg <- L$sigma*sfac
  om2 <- rep(sg^2 + yse^2 + (L$b^2)*(L$xse_ref^2), D)
  n <- length(L$x0); ncov <- round(covf(S)*n)
  out <- numeric(nrep)
  for (r in seq_len(nrep)) {
    set.seed(20260802L + r)
    xs <- sample(L$d$x_obs, D, replace = TRUE)
    ss <- paste0("S", rep(seq_len(S), length.out = D))
    rr <- post_cov(xs, om2, ss, L$psi)
    sidx <- rep(NA_integer_, n)
    if (ncov > 0) sidx[sample(n, ncov)] <- rep(seq_len(rr$S), length.out = ncov)[seq_len(ncov)]
    out[r] <- mean(pv(rr$Sig, rr$S, L$psi^2, L$x0, sidx))
  }
  mean(out)
}
# Anchor the simulator so the ACTUAL designs return their observed variance.
A$anch <- mean(A$Vcal)/sim_cal(A, A$D, A$S, 1, 1, covA)
I$anch <- mean(I$Vcal)/sim_cal(I, I$D, I$S, 1, 1, covI)
chk_warn(SCRIPT, "design simulator reproduces the actual designs within 10%",
    abs(A$anch - 1) < 0.10 && abs(I$anch - 1) < 0.10,
    sprintf("anchor factors: 2015 %.3f, 2019 %.3f (1.000 = simulator exact)",
            A$anch, I$anch))

kappa_of <- function(D, S, mratio, sfac = 1, nmult = 1)
  sqrt((A$anch*sim_cal(A, D, S, mratio, sfac, covA) + A$F0/nmult +
        I$anch*sim_cal(I, D, S, mratio, sfac, covI) + I$F0/nmult) / BASE)

chk(SCRIPT, "kappa = 1 at the two actual designs, by construction",
    abs(sqrt((mean(A$Vcal)+A$F0 + mean(I$Vcal)+I$F0)/BASE) - 1) < 1e-8,
    "definitional anchor: kappa(actual) = 1.000")

grid <- expand.grid(D = c(150, 300, 600, 1200), S = c(12, 24, 36),
                    m = c(1, 4, 16), sigma_factor = c(1, 0.5, 0.25),
                    KEEP.OUT.ATTRS = FALSE)
grid$kappa <- vapply(seq_len(nrow(grid)), function(i)
  kappa_of(grid$D[i], grid$S[i], grid$m[i], grid$sigma_factor[i]), numeric(1))
grid$reaches_p05 <- sig_kappa(grid$kappa)
grid$route <- "transfer"
grid <- grid[order(grid$kappa), ]
write_csv(grid, file.path(dir_out, "design_grid.csv"))
cat(sprintf("\n  best TRANSFER design in the grid (D=%d, S=%d, m=%dx, sigma x%.2f): kappa = %.3f\n",
            grid$D[1], grid$S[1], grid$m[1], grid$sigma_factor[1], grid$kappa[1]))
cat(sprintf("  transfer designs reaching p<0.05: %d of %d\n",
            sum(grid$reaches_p05, na.rm = TRUE), sum(!is.na(grid$reaches_p05))))
chk(SCRIPT, "no transfer-route design in the grid beats kappa_floor",
    all(grid$kappa >= KAPPA_FLOOR - 1e-6),
    sprintf("grid minimum %.4f vs floor %.4f", min(grid$kappa), KAPPA_FLOOR))

# ---- Where the grid's best design gets its information, leg by leg ----------
# SI Methods S4.5 walks the best grid cell through the variance algebra by hand
# and quotes, per leg, how much more information one district carries and how
# much the larger district count adds. Those factors were arithmetic in prose
# with nothing to check them against, and the two legs differ by more than they
# look: the reference-sample and instrument levers act identically on both, but
# the ROW multiple does not, because the realized designs have 51 and 144
# districts against the same 1200. Export the pieces so the text is checkable.
#
# om2 is the per-district error variance the simulator uses, sigma^2 + yse^2/m +
# b^2 xse^2. Only the first two terms respond to the design; the third is the
# NFHS-side term no validation survey can touch, which is why the leg with the
# larger invariant term gains less per district.
om2_of <- function(L, mratio, sfac)
  (L$sigma * sfac)^2 + (L$yse_ref^2)/mratio + (L$b^2) * (L$xse_ref^2)
info_gain <- do.call(rbind, lapply(
  list(list("2015 (NFHS-4 ~ ACCESS)", A), list("2019 (NFHS-5 ~ IRES)", I)),
  function(z) {
    L <- z[[2]]
    o_now  <- om2_of(L, 1, 1)
    o_best <- om2_of(L, grid$m[1], grid$sigma_factor[1])
    data.frame(leg = z[[1]],
               sigma_rms = L$sigma, yse_ref = L$yse_ref, xse_ref = L$xse_ref,
               slope_b = L$b, nfhs_side_term = (L$b^2) * (L$xse_ref^2),
               om2_realized = o_now, om2_best_design = o_best,
               sd_per_district_realized = sqrt(o_now),
               info_per_district = o_now / o_best,
               districts_realized = L$D, districts_best_design = grid$D[1],
               row_multiple = grid$D[1] / L$D,
               info_total = (o_now / o_best) * (grid$D[1] / L$D),
               stringsAsFactors = FALSE)
  }))
write_csv(info_gain, file.path(dir_out, "design_information_gain.csv"))
cat("\n  information gain at the grid's best transfer design, by leg:\n")
for (i in seq_len(nrow(info_gain)))
  cat(sprintf("    %-24s per-district %5.1fx  x  rows %5.1fx  =  %5.0fx total\n",
              info_gain$leg[i], info_gain$info_per_district[i],
              info_gain$row_multiple[i], info_gain$info_total[i]))
chk(SCRIPT, "per-leg information gain is finite and above one",
    all(is.finite(info_gain$info_total)) && all(info_gain$info_total > 1),
    paste(sprintf("%s: %.0fx", info_gain$leg, info_gain$info_total), collapse = " | "))

# The NFHS-side lever: what if NFHS districts were also sampled more deeply?
nmult_tab <- data.frame(nmult = c(1, 2, 4, 8, 16))
nmult_tab$kappa <- vapply(nmult_tab$nmult, function(nm)
  kappa_of(max(grid$D), max(grid$S), max(grid$m), min(grid$sigma_factor), nm), numeric(1))
nmult_tab$reaches_p05 <- sig_kappa(nmult_tab$kappa)
# This sweep is evaluated on the grid's EXTREME cell (most districts, most states,
# deepest reference sample, best instrument), not on the realized validation
# design -- so any statement built from it is conditional on that best-case
# validation survey. `grid` is sorted by kappa, so grid[1, ] should BE that cell;
# assert it, because the manuscript sentence names the design from gridBest.
chk(SCRIPT, "NFHS-multiplier sweep sits on the grid's best transfer design",
    grid$D[1] == max(grid$D) && grid$S[1] == max(grid$S) &&
      grid$m[1] == max(grid$m) && grid$sigma_factor[1] == min(grid$sigma_factor),
    sprintf("sweep base D=%d S=%d m=%dx sigma x%.2f; grid best D=%d S=%d m=%dx sigma x%.2f",
            max(grid$D), max(grid$S), max(grid$m), min(grid$sigma_factor),
            grid$D[1], grid$S[1], grid$m[1], grid$sigma_factor[1]))
cat("\n  best transfer design PLUS a deeper NFHS district sample:\n")
for (i in seq_len(nrow(nmult_tab)))
  cat(sprintf("    NFHS district sample x%-3d  kappa = %.3f%s\n",
              nmult_tab$nmult[i], nmult_tab$kappa[i],
              if (isTRUE(nmult_tab$reaches_p05[i])) "   REACHES p<0.05" else ""))

} else {
  message("  scalar design device not usable in this run: transfer grid and NFHS-multiplier sweep skipped")
  grid <- data.frame(D = numeric(), S = numeric(), m = numeric(), sigma_factor = numeric(),
                     kappa = numeric(), reaches_p05 = logical(), route = character())
  write_csv(grid, file.path(dir_out, "design_grid.csv"))
}

## ---- the DIRECT route -------------------------------------------------------
# Field the good instrument in every study district in both rounds. No transfer
# function, so no calibration-parameter uncertainty, no state effect and no
# NFHS-side error: the corrected value's variance is just the good instrument's
# own district sampling variance, y_se_ref^2 / m.
mm <- 1:80
direct <- data.frame(
  m_ratio = mm,
  hh_per_district_2015 = round(mm*A$neff),
  hh_per_district_2019 = round(mm*I$neff),
  kappa = sqrt((A$yse_ref^2/mm + I$yse_ref^2/mm)/BASE))
direct$reaches_p05 <- sig_kappa(direct$kappa)
direct$route <- "direct"
write_csv(direct, file.path(dir_out, "design_direct.csv"))

hit <- which(direct$reaches_p05 %in% TRUE)
M_STAR <- if (length(hit)) direct$m_ratio[hit[1]] else NA_integer_
N_DISTRICTS <- length(A$x0)
HH_TOTAL <- if (is.na(M_STAR)) NA_real_ else M_STAR*(A$neff + I$neff)*N_DISTRICTS
cat(sprintf("\n  effective households per calibration district: ACCESS %.0f, IRES %.0f\n",
            A$neff, I$neff))
cat(sprintf("  DIRECT route reaches p<0.05 at m* = %sx the existing per-district sample\n",
            ifelse(is.na(M_STAR), "never (within 80x)", M_STAR)))
if (!is.na(M_STAR))
  cat(sprintf("  -> %.0f households (%.2f million) across %d districts and both rounds; %.1fx an NFHS-5 round\n",
              HH_TOTAL, HH_TOTAL/1e6, N_DISTRICTS, HH_TOTAL/NFHS5_HOUSEHOLDS))
chk_warn(SCRIPT, "direct route at m=1x is no better than the fitted correction",
    direct$kappa[1] > 1,
    sprintf("kappa = %.3f at m=1x -- the transfer model borrows strength across districts",
            direct$kappa[1]))
chk_warn(SCRIPT, "direct route reaches significance at a finite sample multiplier",
    !is.na(M_STAR), sprintf("m* = %s", ifelse(is.na(M_STAR), "not within 80x", M_STAR)))

## ---- the scalars the manuscript quotes --------------------------------------
fi <- frontier %>% filter(outcome == "infant")
.fd <- function(route, col) {
  if (is.null(floor_direct)) return(NA_real_)
  r <- floor_direct[floor_direct$outcome == "infant" & startsWith(floor_direct$route, route), ]
  if (nrow(r)) as.numeric(r[[col]][1]) else NA_real_
}
# One value per quantity, always (an empty object must not shift the columns).
summ <- data.frame(
  quantity = c("kappa_star_infant", "precision_gain_infant",
               "z_at_kappa0_neonatal", "kappa_floor",
               "fmi_infant_kappa1", "fmi_neonatal_kappa1",
               "var_nfhs_side_share_2015", "var_nfhs_side_share_2019",
               "best_transfer_kappa", "nfhs_multiplier_needed",
               "direct_m_star", "direct_hh_total", "direct_hh_vs_nfhs5_rounds",
               "n_study_districts",
               "floor_direct_p_cr2_infant", "floor_direct_t_cr2_infant",
               "floor_direct_rms_p_cr2_infant",
               "floor_scalar_p_cr2_infant", "floor_scalar_t_cr2_infant",
               "decomposition_valid",
               "kappa_star_cr2_infant", "reachable_cr2_infant",
               "kappa_floor_raw"),
  value = c(KS, 1/KS,
            kstar$z_at_zero[kstar$outcome == "neonatal"], KAPPA_FLOOR,
            fi$fmi[fi$kappa == 1],
            frontier$fmi[frontier$outcome == "neonatal" & frontier$kappa == 1],
            100*A$F0/mean(A$vObs), 100*I$F0/mean(I$vObs),
            if (!is.null(grid) && nrow(grid)) grid$kappa[1] else NA_real_,
            if (!is.null(nmult_tab) && any(nmult_tab$reaches_p05 %in% TRUE)) nmult_tab$nmult[which(nmult_tab$reaches_p05 %in% TRUE)[1]] else NA_real_,
            M_STAR, HH_TOTAL, HH_TOTAL/NFHS5_HOUSEHOLDS, N_DISTRICTS,
            .fd("fixed:", "p_cr2"), .fd("fixed:", "t_cr2"),
            .fd("fixed_rms:", "p_cr2"),
            .fd("scalar:", "p_cr2"), .fd("scalar:", "t_cr2"),
            as.numeric(DESIGN_OK),
            kstar$kappa_star_cr2[kstar$outcome == "infant"][1],
            as.numeric(kstar$reachable_cr2[kstar$outcome == "infant"][1]),   # NA if CR2 unavailable
            KAPPA_FLOOR_RAW))
stopifnot(nrow(summ) == 23)
write_csv(summ, file.path(dir_out, "design_summary.csv"))
print(summ, digits = 4)

## ---- figure ------------------------------------------------------------------
# Two inference rules on one axis. |z| against 1.96 and |t| against a
# df-dependent qt() cannot share a y-axis honestly, because the CR2 critical
# value MOVES with kappa (df_BR changes as the between-imputation variance
# does). Plotting each statistic DIVIDED BY ITS OWN 5% critical value puts both
# on a common, dimensionless scale where the threshold is the single line
# y = 1, and each curve's crossing is that rule's kappa*. The published CR0
# curve is unchanged in shape -- it is the old |z| curve rescaled by 1/1.96.
# The figure itself lives in fig_precision_frontier.R, the one definition that
# make_revision_figures.R also uses to redraw it from design_frontier.csv
# without re-running this script. The co-author review (Oct 2026) added an
# estimate + CR2 95% CI panel above this one and a labelled kappa_floor.
source("fig_precision_frontier.R")
fig <- plot_precision_frontier(frontier, direct, KS, KS2, KAPPA_FLOOR, Z_CRIT)
ggsave(file.path(dir_out, "design_frontier.jpeg"), fig,
       width = 7.2, height = 7.4, dpi = 300)
chk(SCRIPT, "design-frontier figure written",
    file.exists(file.path(dir_out, "design_frontier.jpeg")),
    file.path(dir_out, "design_frontier.jpeg"))

}  # end DESIGN_FRONTIER (design results)
}  # end HAVE_FITS

if (DESIGN_FRONTIER) {
for (f in c("design_frontier.csv", "design_kappa_star.csv"))
  chk(SCRIPT, sprintf("output written: %s", f), file.exists(file.path(dir_out, f)), "")
if (HAVE_FITS)
  for (f in c("design_variance_decomposition.csv", "design_vc_plugin_penalty.csv",
              "design_grid.csv",
              "design_direct.csv", "design_summary.csv"))
    chk(SCRIPT, sprintf("output written: %s", f), file.exists(file.path(dir_out, f)), "")

# ---- imputation diagnostics for every frontier point and scenario ----------
.dd <- dplyr::bind_rows(DESIGN_DIAG) %>% dplyr::distinct(scenario, outcome, .keep_all = TRUE)
write_csv(.dd, file.path(dir_out, "design_mime_diagnostics.csv"))
.bad <- .dd[!mime_diag_ok(.dd), , drop = FALSE]
chk_warn(SCRIPT, "imputation chains agree at every frontier point and scenario (R-hat <= 1.05, ESS >= 100 incl. the completed-data coefficient; missing = fail)",
         nrow(.dd) > 0 && nrow(.bad) == 0,
         sprintf("%d points; %d fail%s; max R-hat %s; min ESS %s", nrow(.dd), nrow(.bad),
                 if (nrow(.bad)) paste0(" (", paste(head(paste(.bad$outcome, .bad$scenario), 6), collapse = "; "), ")") else "",
                 chk_rng(.dd$rhat_max), chk_rng(pmin(.dd$ess_min, .dd$ess_cr2_estimate))))

} else {
  # The manuscript requires both files; their absence is a failure, not a skip.
  for (f in c("design_variance_decomposition.csv", "design_summary.csv"))
    chk(SCRIPT, sprintf("output written: %s", f), file.exists(file.path(dir_out, f)),
        if (HAVE_FITS) "" else "calibration fits or district proxy missing")
}

message("\n===== 22_design_analysis.R complete =====")
