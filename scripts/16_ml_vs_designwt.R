# ==============================================================================
# 16_ml_vs_designwt.R  (standalone; fast)
#
# SI robustness check: within EACH survey, does the district primary-LPG
# prevalence from the unweighted multilevel small-area model (the primary
# specification) agree with the design-weighted direct estimate (weighted
# proportion with Taylor-linearized SE)? Close agreement means the weighting
# choice is not what drives the NFHS-vs-reference comparison.
#
#   NFHS-4 rural : lpg_2015_rural        vs lpg_2015_rural_wt
#   NFHS-5 rural : lpg_2019_rural        vs lpg_2019_rural_wt
#   ACCESS W1    : access_w1_mainlpg     vs access_w1_mainlpg_wt
#   IRES         : ires_mainlpg          vs ires_mainlpg_wt
#
# Inputs : nfhs_districts.rds, access_districts.rds, ires_districts.rds
# Outputs: ml_vs_designwt_agreement.csv, maps/SI_ml_vs_designwt.jpeg
# ==============================================================================

source("00_config.R")
need_inputs(c("nfhs_districts.rds"   = "01_prep_nfhs.R",
              "access_districts.rds" = "02_prep_access.R",
              "ires_districts.rds"   = "03_prep_ires.R"))

nfhs   <- readRDS(file.path(dir_out, "nfhs_districts.rds"))
access <- readRDS(file.path(dir_out, "access_districts.rds"))
ires   <- readRDS(file.path(dir_out, "ires_districts.rds"))

# Lin's concordance correlation coefficient
ccc <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NA_real_)
  2 * stats::cov(x, y) / (stats::var(x) + stats::var(y) + (mean(x) - mean(y))^2)
}

grab <- function(df, ml, wt, label) {
  if (!all(c(ml, wt) %in% names(df))) {
    message("  [skip] ", label, ": missing ", paste(setdiff(c(ml, wt), names(df)), collapse=", "))
    return(NULL)
  }
  tibble(pair = label, ml = as.numeric(df[[ml]]), wt = as.numeric(df[[wt]])) %>%
    filter(is.finite(ml), is.finite(wt))
}

d <- dplyr::bind_rows(
  grab(nfhs,   "lpg_2015_rural",    "lpg_2015_rural_wt",    "NFHS-4 rural"),
  grab(nfhs,   "lpg_2019_rural",    "lpg_2019_rural_wt",    "NFHS-5 rural"),
  grab(access, "access_w1_mainlpg", "access_w1_mainlpg_wt", "ACCESS W1 (rural)"),
  grab(ires,   "ires_mainlpg",      "ires_mainlpg_wt",      "IRES (all)")
)
d$pair <- factor(d$pair, levels = c("NFHS-4 rural", "NFHS-5 rural",
                                    "ACCESS W1 (rural)", "IRES (all)"))

# ---- agreement statistics -----------------------------------------------------
agr <- d %>% group_by(pair) %>%
  summarise(n_districts = dplyr::n(),
            pearson_r   = cor(ml, wt, use = "complete.obs"),
            ccc         = ccc(ml, wt),
            mean_diff   = mean(ml - wt),                 # multilevel - design-weighted
            mean_abs_diff = mean(abs(ml - wt)),
            .groups = "drop") %>%
  mutate(across(c(pearson_r, ccc, mean_diff, mean_abs_diff), ~ round(.x, 3)))
write_csv(agr, file.path(dir_out, "ml_vs_designwt_agreement.csv"))
cat("\n== Multilevel vs design-weighted district prevalence: agreement ==\n")
print(as.data.frame(agr), row.names = FALSE)

# ---- figure -------------------------------------------------------------------
lab <- agr %>% mutate(txt = sprintf("r = %.2f\nCCC = %.2f\nmean diff = %+.1f pp",
                                    pearson_r, ccc, 100 * mean_diff))
p <- ggplot(d, aes(wt, ml)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey45") +
  geom_point(alpha = 0.5, size = 1.1) +
  geom_text(data = lab, aes(x = 0.02, y = 0.98, label = txt),
            hjust = 0, vjust = 1, size = 3, inherit.aes = FALSE) +
  facet_wrap(~ pair, ncol = 2) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Design-weighted direct district prevalence (primary LPG)",
       y = "Unweighted multilevel district prevalence (primary LPG)") +
  theme_bw() + theme(strip.background = element_rect(fill = "grey92"))
dir.create(file.path(dir_out, "maps"), showWarnings = FALSE)
ggsave(file.path(dir_out, "maps", "SI_ml_vs_designwt.jpeg"), p,
       width = 8, height = 8, dpi = 300)

## ---- CHECKS ------------------------------------------------------------------
chk_header("16_ml_vs_designwt")
chk("16", "agreement computed for all survey pairs", nrow(agr) >= 3,
    paste0(nrow(agr), " pairs"))
chk_warn("16", "multilevel vs design-weighted agreement high (min r)",
    min(agr$pearson_r, na.rm = TRUE) > 0.9,
    sprintf("min r = %.3f", min(agr$pearson_r, na.rm = TRUE)))
chk_file("16", "agreement table written", "ml_vs_designwt_agreement.csv")

message("16_ml_vs_designwt.R done -> ml_vs_designwt_agreement.csv, maps/SI_ml_vs_designwt.jpeg")
