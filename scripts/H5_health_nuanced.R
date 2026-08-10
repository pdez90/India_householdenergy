# ==============================================================================
# H5_health_nuanced.R   (STANDALONE -- does not source 00_config.R)
#
# *** EXPLORATORY / HYPOTHESIS-GENERATING. NOT A CONFIRMATORY ANALYSIS. ***
#
# Child-mortality change-on-change using the MODEL-CONSISTENT nuanced exposure
# metrics from 06 (same IRES models applied to BOTH NFHS rounds, so the district
# change is not contaminated by switching models across eras), with the
# Bayesian correction's uncertainty propagated by multiple imputation over the
# posterior draws of the corrected district-LPG context. Compared side by side
# with the corrected primary-LPG prevalence.
#
# WHY THIS IS EXPLORATORY -- three reasons, all of which must be stated wherever
# these numbers appear:
#
#  (1) COMPOSITIONAL CONSTRAINT. The fuel-use categories are shares of one
#      household distribution and therefore sum to 1 by construction. A district
#      cannot raise its "LPG, no solid fuel reported" share without lowering some
#      other share. A univariable coefficient on any single share is therefore a
#      SUBSTITUTION effect -- "what happens when this share rises and the others
#      fall in whatever proportion they happen to fall in this sample" -- not the
#      standalone effect of that fuel category. The reference category is implicit
#      and data-driven, so the coefficients are NOT independent of one another and
#      should not be read as separate causal quantities. A proper treatment would
#      model log-ratios of the shares against an explicit reference; that is left
#      for future work and is not what this script does.
#
#  (2) THE PREDICTED METRICS ARE MODEL OUTPUT, NOT MEASUREMENTS. Every metric
#      here except the corrected primary-LPG prevalence is a PREDICTION from the
#      06 stacking models, which transfer poorly across surveys (the cross-survey
#      transferability of the stacking model is essentially zero -- see
#      06_stacking_prediction.R and the SI). Prediction error that is correlated
#      with district characteristics will propagate into these coefficients in
#      ways the MI intervals do NOT capture, because MI here propagates only the
#      correction posterior, not stacking-model misspecification.
#
#  (3) THE SIGNS ARE INTERNALLY CONTRADICTORY. In the observed results the
#      cleaner-cooking metrics do not all point the same way once oriented, which
#      is exactly what (1) and (2) predict. This script now DETECTS that
#      condition and emits a WARN rather than asserting the signs agree.
#
# The confirmatory result of the paper is H2 (corrected primary-LPG prevalence,
# MI-propagated). H5 exists to show what the richer fuel-composition metrics look
# like and to be honest about why they cannot be interpreted the same way.
#
# Exposures (district change, NFHS-4 -> NFHS-5):
#   * Corrected primary-LPG prevalence (Bayesian)      [comparison; measured]
#   * LPG, no solid fuel reported (share)   (change_excl_lpg)   predicted
#   * Any solid fuel reported (share)       (change_any_solid)  predicted
#   * LPG and solid fuel reported (share)   (change_stacking)   predicted
#   * Predicted LPG consumption             (change_kg, per 10 kg/yr)
#
# Standalone inputs (files on disk only):
#   district_exposure_proxy_consistent.rds       <- 06 (point, IRES-both-rounds)
#   district_proxy_consistent_draws.rds          <- 06 (posterior draws)
#   health_district_wide.rds                     <- H1 (mortality + SES change)
#   corrected_nfhs_districts.rds                  <- 05 (primary-LPG comparison)
# Optional:
#   df_wide_health.rds                           <- H3 (PM2.5/temp/RH/drought/region)
#
# Output: health_nuanced_effects.csv, health_nuanced_coefplot.jpeg,
#         health_nuanced_exposure_correlations.csv,
#         health_nuanced_exposure_vs_rawlpg.jpeg
# ==============================================================================

## ---- CONFIG ------------------------------------------------------------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers

suppressPackageStartupMessages({
  library(tidyverse); library(lmtest); library(sandwich)
})

path_prox   <- file.path(dir_out, "district_exposure_proxy_consistent.rds")
path_draws  <- file.path(dir_out, "district_proxy_consistent_draws.rds")
path_health <- file.path(dir_out, "health_district_wide.rds")
path_corr   <- file.path(dir_out, "corrected_nfhs_districts.rds")
path_dfwide <- file.path(dir_out, "df_wide_health.rds")   # optional

for (p in c(path_prox, path_health, path_corr))
  if (!file.exists(p)) stop("Required input missing: ", p,
    "\nRun 06_stacking_prediction.R (with DO_CONSISTENT_HEALTH=TRUE), ",
    "H1_prep_mortality.R, and 05_correction.R first.")

## ---- Assemble district frame -------------------------------------------------
prox <- readRDS(path_prox) %>% mutate(district = as.character(district)) %>%
  select(district, change_any_solid, change_excl_lpg, change_stacking, change_kg)

health <- readRDS(path_health) %>% mutate(district = as.character(district))
corr <- readRDS(path_corr) %>%
  transmute(district = as.character(as.numeric(district)),
            change_lpg_bayes = 100 * (lpg_2019_bayes - lpg_2015_bayes),
            # raw (uncorrected) NFHS change, kept for the diagnostic scatter below
            change_lpg_raw   = 100 * (lpg_2019_rural - lpg_2015_rural))

df <- health %>% left_join(prox, by = "district") %>% left_join(corr, by = "district")

if (file.exists(path_dfwide)) {
  dfw <- readRDS(path_dfwide) %>% mutate(district = as.character(district))
  envc <- intersect(c("change_pm","temp_change","rh_change",
                      "droughtchange","region"), names(dfw))
  df <- left_join(df, dfw %>% select(district, all_of(envc)) %>%
                    distinct(district, .keep_all = TRUE), by = "district")
}

## ---- Adjustment set (same as H2) ---------------------------------------------
dhs_adj <- intersect(c("change_poor","change_mother_low_edu","change_electricity",
                       "change_muslim","change_improved_sanitation",
                       "change_improved_water"), names(df))
# The 0.7*tmax + 0.3*tmin composite (weighted_temperature_change, formerly and
# wrongly called heat_index_change) is deliberately EXCLUDED: it is a near-perfect
# linear function of the same monthly fields as temp_change, and adjusting for
# both leaves neither interpretable. This set is now identical to H2's and H4's.
env_adj <- intersect(c("change_pm","temp_change","rh_change",
                       "droughtchange"), names(df))
adj <- c(dhs_adj, env_adj)
stopifnot(!("weighted_temperature_change" %in% adj))
has_region <- "region" %in% names(df); has_state <- "state" %in% names(df)
message("Adjustment covariates: ", paste(adj, collapse = ", "),
        if (has_region) " + region FE" else "")

fit_one <- function(dat, outcome, exposure, per = 10) {
  if (!outcome %in% names(dat) || !exposure %in% names(dat)) return(NULL)
  if (all(is.na(dat[[exposure]]))) return(NULL)
  rhs <- c(exposure, adj, if (has_region) "factor(region)")
  m <- lm(reformulate(rhs, response = outcome), data = dat)
  rows <- as.numeric(rownames(model.frame(m)))
  vc <- if (has_state) sandwich::vcovCL(m, cluster = dat[["state"]][rows]) else
    sandwich::vcovHC(m, "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc); i <- match(exposure, rownames(ct))
  tibble(est = ct[i,1]*per, se = ct[i,2]*per,
         lo = (ct[i,1]-1.96*ct[i,2])*per, hi = (ct[i,1]+1.96*ct[i,2])*per,
         p = ct[i,4], n = nobs(m))
}

## ---- Exposures ---------------------------------------------------------------
# `clean_sign` is the direction in which a rise in the metric is HYPOTHESISED to
# move toward cleaner cooking. It is used ONLY to orient the figure; it is not
# evidence about the sign the data actually produce, and the sign-consistency
# check below reports the observed signs regardless of this column.
EXP <- tribble(
  ~label,                                  ~col,               ~unit,       ~per, ~clean_sign, ~fig, ~mi,
  "Primary LPG (Bayes-corrected)",         "change_lpg_bayes", "10 pp",     10,    1,          TRUE, FALSE,
  "LPG, no solid fuel reported (share)",   "change_excl_lpg",  "10 pp",     10,    1,          TRUE, TRUE,
  "Any solid fuel reported (share)",       "change_any_solid", "10 pp",     10,   -1,          TRUE, TRUE,
  "LPG and solid fuel reported (share)",   "change_stacking",  "10 pp",     10,   -1,          TRUE, TRUE,
  "Predicted LPG consumption",             "change_kg",        "10 kg/yr",  10,    1,          FALSE, TRUE
)
OUTCOMES <- c(neonatal = "change_neonatal_death",
              infant   = "change_infant_death")

## ---- Compositional diagnostics (run BEFORE any model is interpreted) ---------
# The whole point of this block is to make the compositional constraint visible.
# If the predicted shares are strongly correlated with one another -- which they
# must be, since they are shares of one distribution -- then the individual
# coefficients below are substitution contrasts, not separable effects.
exp_cols <- intersect(c("change_lpg_bayes", "change_lpg_raw", "change_excl_lpg",
                        "change_any_solid", "change_stacking", "change_kg"), names(df))
cor_mat <- suppressWarnings(cor(df[, exp_cols], use = "pairwise.complete.obs"))
cor_out <- as.data.frame(round(cor_mat, 3)) %>% rownames_to_column("metric")
write_csv(cor_out, file.path(dir_out, "health_nuanced_exposure_correlations.csv"))
cat("\n== Pairwise correlations among the district exposure-change metrics ==\n")
cat("   (shares sum to 1 by construction -> these are NOT independent exposures;\n",
    "    a coefficient on any one of them is a substitution contrast)\n\n", sep = "")
print(cor_out, row.names = FALSE)

# Do the predicted composition changes agree, even directionally, with the
# measured NFHS primary-LPG change? If not, the composition metrics are tracking
# stacking-model structure rather than the observed clean-cooking transition.
if (all(c("change_lpg_raw", "change_excl_lpg") %in% names(df))) {
  scat <- df %>%
    select(district, change_lpg_raw, any_of(c("change_excl_lpg", "change_any_solid",
                                              "change_stacking", "change_kg"))) %>%
    pivot_longer(-c(district, change_lpg_raw), names_to = "metric", values_to = "value") %>%
    filter(is.finite(change_lpg_raw), is.finite(value)) %>%
    mutate(metric = recode(metric,
      change_excl_lpg  = "LPG, no solid fuel reported (pp)",
      change_any_solid = "Any solid fuel reported (pp)",
      change_stacking  = "LPG and solid fuel reported (pp)",
      change_kg        = "Predicted LPG consumption (kg/yr)"))
  cor_lab <- scat %>% group_by(metric) %>%
    summarise(r = cor(change_lpg_raw, value), n = n(), .groups = "drop") %>%
    mutate(lab = sprintf("r = %.3f (n = %d)", r, n))
  cat("\n== Predicted composition change vs RAW NFHS primary-LPG change ==\n")
  print(as.data.frame(cor_lab %>% select(metric, r, n)), row.names = FALSE, digits = 3)
  p_scat <- ggplot(scat, aes(change_lpg_raw, value)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_point(alpha = 0.35, size = 0.8) +
    geom_smooth(method = "lm", se = FALSE, colour = "firebrick", linewidth = 0.6,
                formula = y ~ x) +
    geom_text(data = cor_lab, aes(x = -Inf, y = Inf, label = lab),
              hjust = -0.08, vjust = 1.6, size = 3, inherit.aes = FALSE) +
    facet_wrap(~ metric, scales = "free_y") +
    theme_bw(base_size = 10) +
    labs(x = "Raw NFHS change in primary-LPG prevalence, 2015 to 2019 (pp)",
         y = "Predicted change in the fuel-composition metric",
         title = NULL, subtitle = NULL)
  ggsave(file.path(dir_out, "health_nuanced_exposure_vs_rawlpg.jpeg"), p_scat,
         width = 8, height = 6, dpi = 300)
}

## ---- Point estimates ---------------------------------------------------------
grid <- expand_grid(outcome = names(OUTCOMES), e = seq_len(nrow(EXP)))
point <- pmap_dfr(grid, function(outcome, e) {
  r <- fit_one(df, OUTCOMES[[outcome]], EXP$col[e], per = EXP$per[e])
  if (is.null(r)) return(NULL)
  mutate(r, outcome = outcome, exposure = EXP$label[e], unit = EXP$unit[e],
         clean_sign = EXP$clean_sign[e], fig = EXP$fig[e], .before = 1)
})

## ---- Multiple imputation over correction-uncertainty draws -------------------
mi_results <- NULL
if (file.exists(path_draws)) {
  draws <- readRDS(path_draws) %>% mutate(district = as.character(district))
  M <- max(draws$draw)
  mi_cols <- EXP$col[EXP$mi]
  mi_one <- function(outcome, expo, per) {
    ocol <- OUTCOMES[[outcome]]
    base <- df %>% select(-any_of(expo))
    qs <- numeric(0); us <- numeric(0)
    for (m in seq_len(M)) {
      dm <- base %>% left_join(
        draws %>% filter(draw == m) %>% select(district, all_of(expo)), by = "district")
      r <- fit_one(dm, ocol, expo, per = per)
      if (!is.null(r)) { qs <- c(qs, r$est); us <- c(us, r$se^2) }
    }
    if (!length(qs)) return(NULL)
    qbar <- mean(qs); ubar <- mean(us); b <- var(qs)
    Tv <- ubar + (1 + 1/length(qs)) * b; se <- sqrt(Tv)
    tibble(outcome = outcome, exposure = NA, unit = NA, col = expo,
           est = qbar, se = se, lo = qbar-1.96*se, hi = qbar+1.96*se,
           p = 2*pnorm(-abs(qbar/se)))
  }
  mi_results <- expand_grid(outcome = names(OUTCOMES), expo = mi_cols) %>%
    pmap_dfr(function(outcome, expo)
      mi_one(outcome, expo, per = EXP$per[match(expo, EXP$col)])) %>%
    left_join(EXP %>% select(col, exposure_lab = label, unit_lab = unit),
              by = c("col")) %>%
    mutate(exposure = exposure_lab, unit = unit_lab) %>%
    select(outcome, exposure, unit, est, se, lo, hi, p)
}

## ---- Write table -------------------------------------------------------------
tab_point <- point %>%
  transmute(analysis = "point", outcome, exposure, `per(+)` = unit,
            est_per10 = round(est,4), lo = round(lo,4), hi = round(hi,4),
            p = round(p,4), n)
tab_mi <- if (!is.null(mi_results)) mi_results %>%
  transmute(analysis = "MI (correction uncertainty)", outcome, exposure,
            `per(+)` = unit, est_per10 = round(est,4), lo = round(lo,4),
            hi = round(hi,4), p = round(p,4), n = NA_integer_) else NULL
tab <- bind_rows(tab_point, tab_mi)
write_csv(tab, file.path(dir_out, "health_nuanced_effects.csv"))
cat("\n== EXPLORATORY: child mortality vs. model-consistent fuel-composition metrics ==\n")
cat("   change in deaths per 100 births per +10 units of the named metric\n")
cat("   HYPOTHESISED directions: rise in any-solid / LPG-and-solid = HARMFUL;\n")
cat("   rise in LPG-no-solid / consumption = PROTECTIVE.\n")
cat("   These are SUBSTITUTION contrasts on shares that sum to 1, estimated on\n")
cat("   PREDICTED (not measured) composition. Do not read them as separable\n")
cat("   causal effects, and do not present them alongside H2 as equivalent\n")
cat("   evidence. See the header of this script for the full caveat.\n\n")
print(as.data.frame(tab), digits = 3)

## ---- Oriented comparison figure (point estimates) ----------------------------
lab_or <- c(
  "Primary LPG (Bayes-corrected)"       = "Primary LPG (+10 pp)",
  "LPG, no solid fuel reported (share)" = "LPG, no solid fuel reported (+10 pp)",
  "Any solid fuel reported (share)"     = "Any solid fuel reported (-10 pp)",
  "LPG and solid fuel reported (share)" = "LPG and solid fuel reported (-10 pp)")
plot_df <- point %>% filter(fig) %>%
  mutate(oe = est*clean_sign,
         olo = pmin(lo*clean_sign, hi*clean_sign),
         ohi = pmax(lo*clean_sign, hi*clean_sign),
         exposure = recode(exposure, !!!lab_or),
         exposure = factor(exposure, levels = rev(unname(lab_or))),
         outcome = factor(outcome, levels = c("neonatal","infant"),
                          labels = c("Neonatal","Infant")))
p <- ggplot(plot_df, aes(oe, exposure)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_pointrange(aes(xmin = olo, xmax = ohi)) +
  facet_wrap(~ outcome, ncol = 1) +
  theme_bw(base_size = 11) +
  labs(x = "Change in mortality (deaths per 100 births) per 10-pp move toward cleaner cooking",
       y = NULL,
       caption = paste("Exploratory. Shares sum to one, so each estimate is a",
                       "substitution contrast on predicted, not measured,\nfuel",
                       "composition; the estimates are not independent of one another."),
       title = NULL, subtitle = NULL)
ggsave(file.path(dir_out, "health_nuanced_coefplot.jpeg"), p,
       width = 8, height = 8, dpi = 400)

message("\nH5 done -> health_nuanced_effects.csv (point + MI rows), ",
        "health_nuanced_coefplot.jpeg in ", dir_out)
## ---- CHECKS ------------------------------------------------------------------
chk_header("H5_health_nuanced")
chk("H5", "nuanced effects table produced", exists("tab") && nrow(tab) > 0)
chk_file("H5", "nuanced effects table written", "health_nuanced_effects.csv")
chk_warn("H5", "nuanced coefplot written",
    file.exists(file.path(dir_out, "health_nuanced_coefplot.jpeg")))
chk_file("H5", "exposure correlation matrix written",
    "health_nuanced_exposure_correlations.csv")

# --- SIGN CONSISTENCY: report what the data did, do not assert what it should do -
# Orient every estimate so that POSITIVE = "moving toward cleaner cooking", then
# ask whether the oriented signs agree. Under the compositional constraint they
# need not, and when they do not that is a substantive finding about the metrics,
# not a bug -- so this is a WARN, and the observed signs are printed either way.
sign_tbl <- point %>%
  mutate(oriented = est * clean_sign,
         direction = ifelse(oriented < 0, "protective", "harmful")) %>%
  select(outcome, exposure, est, clean_sign, oriented, direction, p)
cat("\n== Oriented signs (negative oriented estimate = protective) ==\n")
print(as.data.frame(sign_tbl), row.names = FALSE, digits = 3)

signs_agree <- sign_tbl %>% group_by(outcome) %>%
  summarise(agree = length(unique(sign(oriented))) <= 1, .groups = "drop")
# The check NAME states the property being tested; the DETAIL must state the
# VERDICT. Before this change the WARN line read
#     [WARN] oriented composition effects point the same way within each outcome
# followed by a list showing that they emphatically do not -- a line that
# contradicts itself and reads like a passing check to anyone scanning the log.
# The detail now opens with AGREE or DISAGREE, so the verdict is legible without
# parsing the list, and the disagreeing outcomes are named.
.disagree <- signs_agree$outcome[!signs_agree$agree]
chk_warn("H5", "oriented composition effects agree in sign within each outcome",
    all(signs_agree$agree),
    paste0(if (length(.disagree))
             paste0("DISAGREE in ", paste(.disagree, collapse = ", "),
                    " -- oriented signs are NOT consistent; under the ",
                    "compositional constraint this is a substantive finding ",
                    "about the metrics, not a coding error. ")
           else "AGREE in every outcome. ",
           paste(sprintf("%s: %s", sign_tbl$outcome,
                         paste0(sign_tbl$exposure, "=", sign_tbl$direction)),
                 collapse = " | ")))

# The composition metrics should at least track the measured NFHS transition. If
# the predicted "LPG, no solid fuel" change is uncorrelated with the raw NFHS LPG
# change, these metrics are reflecting stacking-model structure, not the data.
if (all(c("change_lpg_raw","change_excl_lpg") %in% names(df))) {
  r_excl <- suppressWarnings(cor(df$change_lpg_raw, df$change_excl_lpg,
                                 use = "complete.obs"))
  chk_warn("H5", "predicted LPG-no-solid change tracks the raw NFHS LPG change (|r| > 0.3)",
      is.finite(r_excl) && abs(r_excl) > 0.3, sprintf("r = %.3f", r_excl))
}

message("\nH5 is EXPLORATORY. The fuel-composition shares sum to one, so each ",
        "coefficient is a substitution contrast estimated on PREDICTED composition, ",
        "not a separable effect of that fuel category. Where the oriented signs ",
        "disagree, read that as evidence about the limits of the predicted metrics ",
        "-- not as a competing estimate of the effect in H2.")
