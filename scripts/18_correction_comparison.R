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
#          estimator_mix_revision_bands.csv, comparison_levels.csv,
#          correction_revision_by_state_2019.csv,
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

# ---- where the correction is largest, in percentage points -------------------
# The calibration slope is a logit-scale quantity; a slope below 1 does not by
# itself say where the percentage-point revision is largest. Tabulate the
# district revision (corrected minus raw multilevel, in points) by band of the
# raw estimate so the manuscript can describe the pattern from the outputs.
BANDS <- c(0, 10, 20, 40, 60, 80, 100)
band_tab <- purrr::map_dfr(c("2015", "2019"), function(yr)
  purrr::map_dfr(c("rc_ml", "bayes"), function(k) {
    raw <- 100 * mix[[paste0("raw_ml_", yr)]]
    rev <- 100 * mix[[paste0(k, "_", yr)]] - raw
    ok  <- is.finite(raw) & is.finite(rev)
    b   <- cut(raw[ok], BANDS, include.lowest = TRUE, right = FALSE)
    dplyr::tibble(band = b, rev = rev[ok]) |>
      dplyr::group_by(band, .drop = FALSE) |>
      dplyr::summarise(n = dplyr::n(),
                       mean_rev_pp = round(mean(rev), 1),
                       median_rev_pp = round(stats::median(rev), 1),
                       .groups = "drop") |>
      dplyr::mutate(year = yr, estimator = unname(ESTS[k]),
                    band_lo = BANDS[as.integer(band)],
                    band_hi = BANDS[as.integer(band) + 1], .before = 1) |>
      dplyr::select(year, estimator, band_lo, band_hi, n, mean_rev_pp, median_rev_pp)
  }))
band_tab$mean_rev_pp[band_tab$n == 0] <- NA
band_tab$median_rev_pp[band_tab$n == 0] <- NA
readr::write_csv(band_tab, file.path(dir_out, "estimator_mix_revision_bands.csv"))
cat("\n== District revision (corrected - raw, points) by raw-prevalence band ==\n")
print(as.data.frame(band_tab), row.names = FALSE)

# ---- levels behind the head-to-head gaps (Section 3.1) -----------------------
# comparison_table.csv (04) reports the mean district DIFFERENCE; the manuscript
# also states the two levels it is a difference of, so a reader sees "44% vs
# 62%" and not only "18 points". Same district sets as comparison_table.csv
# (complete cases on both sides), so the levels difference to its mean_diff.
lvl_pair <- function(df, x, y, label) {
  ok <- is.finite(df[[x]]) & is.finite(df[[y]])
  dplyr::tibble(comparison = label, n_districts = sum(ok),
                mean_nfhs_pct = round(100 * mean(df[[x]][ok]), 1),
                mean_ref_pct  = round(100 * mean(df[[y]][ok]), 1))
}
cmp_lvl <- dplyr::bind_rows(
  lvl_pair(pairA, "lpg_2015", "access_w1_mainlpg", "NFHS-4 (all) vs ACCESS W1"),
  lvl_pair(pairA, "lpg_2015_rural", "access_w1_mainlpg", "NFHS-4 (rural) vs ACCESS W1"),
  lvl_pair(pairA, "lpg_2015_rural_wt", "access_w1_mainlpg_wt",
           "NFHS-4 (rural, design-wt) vs ACCESS W1 (design-wt)"),
  lvl_pair(pairB, "lpg_2019", "ires_mainlpg", "NFHS-5 (all) vs IRES (all)"),
  lvl_pair(pairB, "lpg_2019_rural", "ires_mainlpg_rural", "NFHS-5 (rural) vs IRES (rural)"),
  lvl_pair(pairB, "lpg_2019_rural_wt", "ires_mainlpg_rural_wt",
           "NFHS-5 (rural, design-wt) vs IRES (rural, design-wt)"))
readr::write_csv(cmp_lvl, file.path(dir_out, "comparison_levels.csv"))
cat("\n== District-mean primary-LPG levels behind each comparison (%) ==\n")
print(as.data.frame(cmp_lvl), row.names = FALSE)

# ---- where the 2019 correction is largest, by state --------------------------
# District revision = Bayesian-corrected minus raw multilevel rural estimate, in
# points, summarised by state so Section 3.2 can describe the geography of the
# correction from the outputs. The model carries state effects, so these state
# means partly reflect fitted state-level departures from the common line.
st_rev <- corr |>
  dplyr::transmute(state, rev = 100 * (lpg_2019_bayes - lpg_2019_rural)) |>
  dplyr::filter(is.finite(rev)) |>
  dplyr::group_by(state) |>
  dplyr::summarise(n = dplyr::n(), mean_rev_pp = round(mean(rev), 1),
                   n_up = sum(rev > 0), n_down = sum(rev < 0), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(mean_rev_pp))
readr::write_csv(st_rev, file.path(dir_out, "correction_revision_by_state_2019.csv"))
cat("\n== 2019 district revision (corrected - raw, points) by state ==\n")
print(as.data.frame(st_rev), row.names = FALSE)

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
chk_file("18", "revision-by-band table written", "estimator_mix_revision_bands.csv")
chk_file("18", "comparison levels table written", "comparison_levels.csv")
chk_file("18", "state revision table written", "correction_revision_by_state_2019.csv")

message("18_correction_comparison.R done -> estimator_mix_levels.csv, estimator_mix_revision_bands.csv, ",
        "estimator_mix_correlations.csv, maps/SI_estimator_mix.jpeg")
