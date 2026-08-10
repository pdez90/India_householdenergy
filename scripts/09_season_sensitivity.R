# ==============================================================================
# 09_season_sensitivity.R
# Interview-season sensitivity for the NFHS-5 vs IRES comparison.
#
# Motivation (Gould et al. 2022, World Development): 75% of fuel-stacking
# households switch their PRIMARY fuel across seasons -- solid fuel dominates
# in winter, gas in the rainy season. IRES was fielded almost entirely in
# winter (Nov 2019 - Mar 2020), i.e., the season when stacking households are
# MOST likely to report a solid fuel as primary. NFHS-5 interviews span all
# seasons. This script re-estimates NFHS-5 rural district LPG prevalence using
# ONLY winter interviews (Dec-Feb) -- the season-matched comparison to IRES --
# and using only 2019 interviews (pre-COVID), and compares each variant's
# agreement with IRES against the all-interview baseline.
#
# If the NFHS-IRES gap were driven by seasonal reporting, the winter-restricted
# NFHS estimates should CLOSE the gap. If the gap persists (or widens), the
# seasonal explanation is ruled out and the measurement interpretation
# strengthens -- note that IRES's winter fielding biases its own reports
# TOWARD solid fuel, making the observed LPG gap conservative.
#
# Inputs : nfhs_hh_covariates.rds (01; must contain month_interview),
#          ires_districts.rds (03)
# Output : season_sensitivity.csv
# Runtime: two national glmer fits, ~3-8 min.
# ==============================================================================

source("00_config.R")
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "09_season_sensitivity"

need_inputs(c("nfhs_hh_covariates.rds" = "01_prep_nfhs.R",
              "ires_districts.rds"     = "03_prep_ires.R"))

nfhs_hh <- readRDS(file.path(dir_out, "nfhs_hh_covariates.rds"))
ires    <- readRDS(file.path(dir_out, "ires_districts.rds"))
if (!"month_interview" %in% names(nfhs_hh) ||
    all(is.na(nfhs_hh$month_interview)))
  stop("nfhs_hh_covariates.rds lacks month_interview -- re-run the updated ",
       "01_prep_nfhs.R first.")

n5r <- nfhs_hh %>%
  filter(survey == "NFHS5", rural == 1) %>%
  mutate(state = factor(state), district = factor(district),
         clust = factor(clust))

est_variant <- function(dat, label) {
  message("Estimating NFHS-5 rural district LPG: ", label,
          " (n = ", nrow(dat), ") ...")
  district_estimates_glmer(dat, "lpg") %>%
    transmute(district = as.character(district),
              !!paste0("lpg_", label) := p_hat)
}

e_winter <- est_variant(n5r %>% filter(month_interview %in% c(12, 1, 2)),
                        "winter")

# Pre-COVID variant: interviews through February 2020 (2019 plus Jan-Feb 2020),
# i.e. EXCLUDING March 2020 onward, before India's late-March 2020 lockdown
# disrupted fieldwork. Needs yr_interview from the updated 01; skipped if absent.
has_yr <- "yr_interview" %in% names(n5r) && !all(is.na(n5r$yr_interview))
if (has_yr) {
  e_2019 <- est_variant(
    n5r %>% filter(yr_interview <= 2019 |
                     (yr_interview == 2020 & month_interview < 3)),
    "y2019")
} else {
  message("yr_interview not in household frame -- pre-COVID variant skipped ",
          "(re-run updated 01 to enable).")
  e_2019 <- tibble(district = character(), lpg_y2019 = numeric())
}

comp <- ires %>%
  transmute(district = as.character(district), ires_mainlpg_rural,
            n_ires_rural) %>%
  inner_join(readRDS(file.path(dir_out, "nfhs_districts.rds")) %>%
               transmute(district = as.character(district),
                         lpg_all = lpg_2019_rural),
             by = "district") %>%
  left_join(e_winter, by = "district") %>%
  left_join(e_2019,   by = "district")

row_stats <- function(x, y, w, label) {
  ok <- complete.cases(x, y)
  tibble(variant = label, n_districts = sum(ok),
         pearson = cor(x, y, use = "pairwise.complete.obs"),
         # reference-sample-size-weighted (w = n_ires_rural), not population-weighted
         pearson_ref_samplewt = weighted_cor(x, y, w),
         mean_diff = mean(x - y, na.rm = TRUE))
}
out <- bind_rows(
  row_stats(comp$lpg_all,    comp$ires_mainlpg_rural, comp$n_ires_rural,
            "NFHS-5 all interviews"),
  row_stats(comp$lpg_winter, comp$ires_mainlpg_rural, comp$n_ires_rural,
            "NFHS-5 winter interviews only (Dec-Feb, season-matched to IRES)"),
  row_stats(comp$lpg_y2019,  comp$ires_mainlpg_rural, comp$n_ires_rural,
            "NFHS-5 interviews through Feb 2020 (pre-COVID; excludes Mar 2020 onward)")
) %>% mutate(across(where(is.numeric), ~round(.x, 3)))

write_csv(out, file.path(dir_out, "season_sensitivity.csv"))
print(as.data.frame(out))
## ---- CHECKS ------------------------------------------------------------------
chk_header("09_season_sensitivity")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("09", "09_season_sensitivity")

chk("09", "correlation column renamed to sample-size-weighted",
    "pearson_ref_samplewt" %in% names(out) && !"pearson_popwt" %in% names(out))
chk("09", "all three interview-timing variants present",
    nrow(out) >= 3, paste0(nrow(out), " variants"))
chk_file("09", "season sensitivity table written", "season_sensitivity.csv")

message("09_season_sensitivity.R done -> season_sensitivity.csv")
