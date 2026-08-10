# ==============================================================================
# 18_correction_comparison.R  (standalone; fast, fully deterministic)
#
# SI robustness: compare the district primary-LPG surfaces produced by every
# estimator / correction, and in particular show that REGRESSION CALIBRATION is
# insensitive to whether it is fed the multilevel or the design-weighted raw
# NFHS estimate. Five district surfaces per round:
#
#   raw_ml : raw multilevel (primary specification)         lpg_YYYY_rural
#   raw_wt : raw design-weighted direct                     lpg_YYYY_rural_wt
#   rc_ml  : regression-calibrated, fit on the multilevel   lpg_YYYY_rc     (from 05)
#   rc_wt  : regression-calibrated, fit on the design-wt     (re-fit here)
#   bayes  : Bayesian measurement-error corrected           lpg_YYYY_bayes  (from 05)
#
# The calibration is the SAME logit-scale, reference-weighted line as 05; only
# the raw NFHS predictor changes between rc_ml and rc_wt. lm() is deterministic,
# so this script is bit-reproducible.
#
# Inputs : corrected_nfhs_districts.rds (05), compare_pairs.rds (04)
# Outputs: estimator_mix_levels.csv, estimator_mix_correlations.csv,
#          maps/SI_estimator_mix.jpeg
# ==============================================================================

source("00_config.R")
need_inputs(c("corrected_nfhs_districts.rds" = "05_correction.R",
              "compare_pairs.rds"            = "04_compare.R"))

corr  <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))
pairs <- readRDS(file.path(dir_out, "compare_pairs.rds"))
pairA <- pairs$pairA   # NFHS-4 vs ACCESS W1
pairB <- pairs$pairB   # NFHS-5 vs IRES

clamp <- function(p, eps = 1e-3) pmin(pmax(p, eps), 1 - eps)

# ---- regression calibration: fit on overlap, apply nationally ----------------
# Formula uses bare column names (as in 05_correction.R) so predict() on new data
# supplying that column works exactly. Weighted by reference-survey hh count.
fit_rc <- function(pair, ref, nfhs_in, wtcol) {
  f <- stats::as.formula(sprintf("qlogis(clamp(%s)) ~ qlogis(clamp(%s))", ref, nfhs_in))
  d <- pair[stats::complete.cases(pair[[ref]], pair[[nfhs_in]], pair[[wtcol]]), , drop = FALSE]
  stats::lm(f, data = d, weights = d[[wtcol]])
}
rc_apply <- function(pair, ref, nfhs_in, wtcol, newx) {
  fit <- fit_rc(pair, ref, nfhs_in, wtcol)
  nd  <- stats::setNames(data.frame(newx), nfhs_in)
  as.numeric(plogis(predict(fit, newdata = nd)))
}

# ---- assemble the five surfaces per round ------------------------------------
mix <- corr |>
  dplyr::transmute(
    district,
    raw_ml_2015 = lpg_2015_rural, raw_wt_2015 = lpg_2015_rural_wt,
    rc_ml_2015  = lpg_2015_rc,    bayes_2015  = lpg_2015_bayes,
    raw_ml_2019 = lpg_2019_rural, raw_wt_2019 = lpg_2019_rural_wt,
    rc_ml_2019  = lpg_2019_rc,    bayes_2019  = lpg_2019_bayes)

# regression calibration re-fit on the DESIGN-WEIGHTED raw estimate, applied to
# the national design-weighted surface
mix$rc_wt_2015 <- rc_apply(pairA, "access_w1_mainlpg",  "lpg_2015_rural_wt",
                           "n_access_w1",  mix$raw_wt_2015)
mix$rc_wt_2019 <- rc_apply(pairB, "ires_mainlpg_rural", "lpg_2019_rural_wt",
                           "n_ires_rural", mix$raw_wt_2019)

ESTS <- c(raw_ml = "Raw multilevel", raw_wt = "Raw design-weighted",
          rc_ml  = "Reg.-calibrated (multilevel input)",
          rc_wt  = "Reg.-calibrated (design-wt input)",
          bayes  = "Bayesian-corrected")

# ---- mean national prevalence per estimator ----------------------------------
lvl <- purrr::map_dfr(c("2015", "2019"), function(yr) {
  dplyr::tibble(
    year = yr, key = names(ESTS), estimator = unname(ESTS),
    mean_prev = vapply(names(ESTS), function(k)
      round(100 * mean(mix[[paste0(k, "_", yr)]], na.rm = TRUE), 1), numeric(1))) |>
    dplyr::select(year, estimator, mean_prev)
})
readr::write_csv(lvl, file.path(dir_out, "estimator_mix_levels.csv"))
cat("\n== Mean national district prevalence (%) by estimator ==\n")
print(as.data.frame(tidyr::pivot_wider(lvl, names_from = year, values_from = mean_prev)),
      row.names = FALSE)

# ---- pairwise correlations among estimators, per round -----------------------
cor_tab <- purrr::map_dfr(c("2015", "2019"), function(yr) {
  M <- as.data.frame(lapply(names(ESTS), function(k) mix[[paste0(k, "_", yr)]]))
  names(M) <- names(ESTS)
  cm <- round(cor(M, use = "pairwise.complete.obs"), 3)
  tibble::rownames_to_column(as.data.frame(cm), "estimator") |>
    dplyr::mutate(year = yr, .before = 1)
})
readr::write_csv(cor_tab, file.path(dir_out, "estimator_mix_correlations.csv"))
cat("\n== Pairwise Pearson r among estimators (per round) ==\n")
print(as.data.frame(cor_tab), row.names = FALSE)

# headline robustness number: RC on multilevel vs RC on design-weighted
rc_agree <- vapply(c("2015", "2019"), function(yr)
  round(cor(mix[[paste0("rc_ml_", yr)]], mix[[paste0("rc_wt_", yr)]],
            use = "complete.obs"), 4), numeric(1))
message("Regression calibration -- multilevel-input vs design-wt-input district ",
        "correlation: 2015 r=", rc_agree["2015"], " | 2019 r=", rc_agree["2019"])

# ---- figure: each alternative estimator vs the raw multilevel ----------------
long <- purrr::map_dfr(c("2015", "2019"), function(yr)
  purrr::map_dfr(c("raw_wt", "rc_ml", "rc_wt", "bayes"), function(k)
    dplyr::tibble(
      round = if (yr == "2015") "NFHS-4 (2015)" else "NFHS-5 (2019)",
      estimator = unname(ESTS[k]),
      raw_ml = mix[[paste0("raw_ml_", yr)]],
      value  = mix[[paste0(k, "_", yr)]]))) |>
  dplyr::filter(is.finite(raw_ml), is.finite(value))
long$estimator <- factor(long$estimator,
                         levels = unname(ESTS[c("raw_wt", "rc_ml", "rc_wt", "bayes")]))

p <- ggplot(long, aes(raw_ml, value)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey45") +
  geom_point(alpha = 0.4, size = 0.8, color = "steelblue") +
  facet_grid(round ~ estimator) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Raw multilevel district prevalence (primary LPG)",
       y = "Alternative estimator / correction") +
  theme_bw() + theme(strip.background = element_rect(fill = "grey92"))
dir.create(file.path(dir_out, "maps"), showWarnings = FALSE)
ggsave(file.path(dir_out, "maps", "SI_estimator_mix.jpeg"), p,
       width = 10, height = 6, dpi = 300)

## ---- CHECKS ------------------------------------------------------------------
chk_header("18_correction_comparison")
chk("18", "all five estimator surfaces produced",
    chk_has_cols(mix, c("raw_ml_2015","raw_wt_2015","rc_ml_2015","rc_wt_2015","bayes_2015")))
chk_warn("18", "RC insensitive to multilevel vs design-wt input (2019 r)",
    is.finite(rc_agree["2019"]) && rc_agree["2019"] > 0.9,
    sprintf("2015 r=%.3f | 2019 r=%.3f", rc_agree["2015"], rc_agree["2019"]))
chk_file("18", "estimator mix levels written", "estimator_mix_levels.csv")

message("18_correction_comparison.R done -> estimator_mix_levels.csv, ",
        "estimator_mix_correlations.csv, maps/SI_estimator_mix.jpeg")
