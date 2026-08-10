# ==============================================================================
# 04_compare.R
# Head-to-head district comparisons, answering four questions:
#   1. correlation (and why it might be low)     -> multiple agreement metrics
#   2. scatterplots + Bland-Altman               -> divergence at high/low levels
#   3. urban/rural                               -> rural-only NFHS estimates
#   4. population weighting                      -> household-count-weighted corr
#
# Pairs:
#   A. NFHS-4 (rural) vs ACCESS Wave 1   [2015-16 vs 2014-15; 6 states]
#   B. NFHS-5 (rural) vs IRES            [2019-21 vs 2019-20; national]
#   (ACCESS Wave 2 predates NFHS-5 - reported only as descriptive context.)
#
# Inputs : nfhs_districts.rds, access_districts.rds, ires_districts.rds
# Outputs: comparison_table.csv, scatter/BA figures (jpeg), compare_pairs.rds
# ==============================================================================

source("00_config.R")
need_inputs(c("nfhs_districts.rds"   = "01_prep_nfhs.R",
              "access_districts.rds" = "02_prep_access.R",
              "ires_districts.rds"   = "03_prep_ires.R"))

nfhs   <- readRDS(file.path(dir_out, "nfhs_districts.rds"))
access <- readRDS(file.path(dir_out, "access_districts.rds"))
ires   <- readRDS(file.path(dir_out, "ires_districts.rds"))

districts_shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.character(as.numeric(dist_code)))

# ---- Pair A: NFHS-4 rural vs ACCESS Wave 1 -------------------------------------
pairA <- access %>%
  select(district, state_access = state, access_w1_mainlpg,
         access_w1_mainlpg_wt, access_w1_mainlpg_wt_se, n_access_w1) %>%
  inner_join(nfhs %>% select(district, state, lpg_2015, lpg_2015_rural,
                             lpg_2015_rural_wt, lpg_2015_rural_wt_se,
                             n_2015_rural),
             by = "district") %>%
  left_join(districts_shp %>% st_drop_geometry() %>%
              select(dist_code, state_name, dist_name),
            by = c("district" = "dist_code"))

# ---- Pair B: NFHS-5 rural vs IRES ----------------------------------------------
pairB <- ires %>%
  select(district, ires_mainlpg, ires_mainlpg_rural, ires_mainlpg_wt,
         ires_mainlpg_wt_se, ires_mainlpg_rural_wt, ires_mainlpg_rural_wt_se,
         n_ires, n_ires_rural) %>%
  inner_join(nfhs %>% select(district, lpg_2019, lpg_2019_rural,
                             lpg_2019_rural_wt, lpg_2019_rural_wt_se,
                             n_2019_rural),
             by = "district") %>%
  left_join(districts_shp %>% st_drop_geometry() %>%
              select(dist_code, state_name, dist_name),
            by = c("district" = "dist_code"))

# If IRES rural-only estimates were unavailable (rural flag not set in 03),
# fall back to all-household IRES estimates so the comparison still runs.
if (all(is.na(pairB$ires_mainlpg_rural))) {
  message("IRES rural estimates unavailable -- substituting all-household ",
          "IRES estimates in the rural comparison (interpret accordingly).")
  pairB$ires_mainlpg_rural         <- pairB$ires_mainlpg
  pairB$n_ires_rural               <- pairB$n_ires
  pairB$ires_mainlpg_rural_wt      <- pairB$ires_mainlpg_wt
  pairB$ires_mainlpg_rural_wt_se   <- pairB$ires_mainlpg_wt_se
}

saveRDS(list(pairA = pairA, pairB = pairB), file.path(dir_out, "compare_pairs.rds"))

# ---- Agreement metrics -----------------------------------------------------------
agreement <- function(df, x, y, w = NULL, label = "") {
  xv <- df[[x]]; yv <- df[[y]]
  wv <- if (!is.null(w)) df[[w]] else rep(1, nrow(df))
  tibble(
    comparison   = label,
    n_districts  = sum(complete.cases(xv, yv)),
    pearson      = cor(xv, yv, use = "pairwise.complete.obs"),
    spearman     = cor(xv, yv, use = "pairwise.complete.obs", method = "spearman"),
    # Weighted by REFERENCE-SURVEY HOUSEHOLD COUNT (n_access_w1 / n_ires_*), i.e.
    # a sample-size-weighted correlation that downweights sparsely sampled
    # districts -- NOT a population-weighted correlation (that would need a
    # district population denominator). Named accordingly.
    pearson_ref_samplewt = weighted_cor(xv, yv, wv),
    ccc          = ccc(xv, yv),                   # agreement, not just association
    mean_diff    = mean(xv - yv, na.rm = TRUE),
    # correlation on the logit scale often behaves better for proportions:
    pearson_logit= cor(qlogis(pmin(pmax(xv, .001), .999)),
                       qlogis(pmin(pmax(yv, .001), .999)),
                       use = "pairwise.complete.obs")
  )
}

comparison_table <- bind_rows(
  agreement(pairA, "lpg_2015",          "access_w1_mainlpg", "n_access_w1",
            "NFHS-4 (all) vs ACCESS W1"),
  agreement(pairA, "lpg_2015_rural",    "access_w1_mainlpg", "n_access_w1",
            "NFHS-4 (rural) vs ACCESS W1"),
  agreement(pairA, "lpg_2015_rural_wt", "access_w1_mainlpg_wt", "n_access_w1",
            "NFHS-4 (rural, design-wt) vs ACCESS W1 (design-wt)"),
  agreement(pairB, "lpg_2019",          "ires_mainlpg", "n_ires",
            "NFHS-5 (all) vs IRES (all)"),
  agreement(pairB, "lpg_2019_rural",    "ires_mainlpg_rural", "n_ires_rural",
            "NFHS-5 (rural) vs IRES (rural)"),
  agreement(pairB, "lpg_2019_rural_wt", "ires_mainlpg_rural_wt", "n_ires_rural",
            "NFHS-5 (rural, design-wt) vs IRES (rural, design-wt)")
)
write_csv(comparison_table, file.path(dir_out, "comparison_table.csv"))
print(comparison_table)

# ---- Scatterplots (45-degree line, sized by households) --------------------------
scatter_pair <- function(df, x, y, xlab, ylab, size_var) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]], color = state_name)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey40") +
    geom_point(aes(size = .data[[size_var]]), alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, inherit.aes = FALSE,
                aes(x = .data[[x]], y = .data[[y]])) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = xlab, y = ylab, color = "State", size = "Households") +
    theme_bw()
}

pA_scatter <- scatter_pair(pairA, "lpg_2015_rural", "access_w1_mainlpg",
                           "NFHS-4 (rural), P(main fuel = LPG)",
                           "ACCESS Wave 1, P(main fuel = LPG)", "n_access_w1")
pB_scatter <- scatter_pair(pairB, "lpg_2019_rural", "ires_mainlpg_rural",
                           "NFHS-5 (rural), P(main fuel = LPG)",
                           "IRES (rural), P(main fuel = LPG)", "n_ires_rural")

jpeg(file.path(dir_out, "Scatter_45deg.jpeg"), res = 300, width = 3000, height = 4500)
print(cowplot::plot_grid(pA_scatter, pB_scatter, labels = c("A)", "B)"), ncol = 1))
dev.off()

# ---- Supplementary: ALL-household vs ALL-household (like against like) -----------
# The primary comparison and the correction are rural-vs-rural. To show the
# NFHS-IRES gap is not an artifact of the rural restriction, compare ALL-household
# NFHS-5 against ALL-household IRES district prevalence (both same estimator).
# Agreement statistics are the "NFHS-5 (all) vs IRES (all)" row of comparison_table.
pB_all_scatter <- scatter_pair(pairB, "lpg_2019", "ires_mainlpg",
                               "NFHS-5 (all households), P(main fuel = LPG)",
                               "IRES (all households), P(main fuel = LPG)", "n_ires")
ggsave(file.path(dir_out, "SI_all_vs_all_nfhs5_ires.jpeg"), pB_all_scatter,
       width = 7, height = 7, dpi = 300)

# ---- Bland-Altman (does agreement diverge at higher/lower levels?) ---------------
pA_ba <- bland_altman_plot(pairA, "lpg_2015_rural", "access_w1_mainlpg",
                           "NFHS-4 rural", "ACCESS W1",
                           color_by = "state_name", weight = "n_access_w1")
pB_ba <- bland_altman_plot(pairB, "lpg_2019_rural", "ires_mainlpg_rural",
                           "NFHS-5 rural", "IRES rural",
                           color_by = "state_name", weight = "n_ires_rural")

jpeg(file.path(dir_out, "BlandAltman.jpeg"), res = 300, width = 3000, height = 4500)
print(cowplot::plot_grid(pA_ba, pB_ba, labels = c("A)", "B)"), ncol = 1))
dev.off()

# ---- Timing note: interview-date overlap -----------------------------------------
# NFHS-4 fieldwork: Jan 2015 - Nov 2016 | ACCESS W1: Nov 2014 - May 2015
# NFHS-5 fieldwork: Jun 2019 - Apr 2021 | IRES: Mar - Jun 2019 (+2019-20 module)
# PMUY (2016+) and COVID-era disruptions mean part of any disagreement is real
# temporal change, not error. Consider a sensitivity restricting NFHS-5 to
# households interviewed in 2019 (pre-COVID) via month/yr_interview.

## ---- CHECKS ------------------------------------------------------------------
chk_header("04_compare")
chk("04", "pairB carries rural design-weighted IRES (from 03 -> feeds 05)",
    chk_has_cols(pairB, c("ires_mainlpg_rural_wt","ires_mainlpg_rural_wt_se",
                          "lpg_2019_rural_wt","lpg_2019_rural_wt_se")))
chk("04", "rural design-weighted IRES not all-NA in overlap",
    any(is.finite(pairB$ires_mainlpg_rural_wt)),
    paste0(sum(is.finite(pairB$ires_mainlpg_rural_wt)), " overlap districts"))
chk("04", "pairA carries design-weighted ACCESS + its SE (feeds 05 leg-A sensitivity)",
    chk_has_cols(pairA, c("access_w1_mainlpg_wt","access_w1_mainlpg_wt_se",
                          "lpg_2015_rural_wt","lpg_2015_rural_wt_se")))
chk("04", "ACCESS design SE finite and strictly positive where the estimate exists",
    { ok <- is.finite(pairA$access_w1_mainlpg_wt)
      all(is.finite(pairA$access_w1_mainlpg_wt_se[ok])) &&
      all(pairA$access_w1_mainlpg_wt_se[ok] > 0) },
    paste0(sum(is.finite(pairA$access_w1_mainlpg_wt_se)), "/", nrow(pairA),
           " finite; median ",
           round(median(pairA$access_w1_mainlpg_wt_se, na.rm = TRUE), 4)))
chk_warn("04", "ACCESS design SE is larger than the binomial approximation it replaces",
    { p <- pairA$access_w1_mainlpg_wt; n <- pairA$n_access_w1
      bin <- sqrt(p * (1 - p) / n)
      ok <- is.finite(bin) & is.finite(pairA$access_w1_mainlpg_wt_se) & bin > 0
      sum(ok) > 0 &&
        median(pairA$access_w1_mainlpg_wt_se[ok] / bin[ok], na.rm = TRUE) > 1 },
    { p <- pairA$access_w1_mainlpg_wt; n <- pairA$n_access_w1
      bin <- sqrt(p * (1 - p) / n)
      ok <- is.finite(bin) & is.finite(pairA$access_w1_mainlpg_wt_se) & bin > 0
      paste0("median design/binomial SE ratio ",
             round(median(pairA$access_w1_mainlpg_wt_se[ok] / bin[ok],
                          na.rm = TRUE), 2)) })
chk("04", "correlation renamed to sample-size-weighted (not 'popwt')",
    "pearson_ref_samplewt" %in% names(comparison_table) &&
    !"pearson_popwt" %in% names(comparison_table))
chk("04", "compare_pairs.rds written (input to 05)",
    file.exists(file.path(dir_out, "compare_pairs.rds")))
chk_file("04", "all-vs-all (2019) SI figure written", "SI_all_vs_all_nfhs5_ires.jpeg")
chk_warn("04", "ACCESS overlap >=45, IRES overlap >=130 districts",
    nrow(pairA) >= 45 && nrow(pairB) >= 130,
    paste0("pairA ", nrow(pairA), ", pairB ", nrow(pairB)))

message("04_compare.R done. See comparison_table.csv and figures in ", dir_out)
