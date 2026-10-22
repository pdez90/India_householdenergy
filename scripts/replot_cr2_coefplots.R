# ==============================================================================
# replot_cr2_coefplots.R  (standalone; reads saved CSVs only, refits nothing)
#
# Redraws the two SI coefficient plots with the PRIMARY (CR2, Satterthwaite)
# intervals that Tables S5 and S8 report:
#   si_adult_health_coefplot[_all].jpeg  <- si_adult_health_effects[_all].csv (H4)
#   health_nuanced_coefplot.jpeg         <- health_nuanced_effects.csv        (H5)
# H4 and H5 draw the same figures (CR2 intervals) on a full run; this script
# exists so the figures can be regenerated from saved results without
# re-fitting. Layout matches H4/H5.
# ==============================================================================

source("00_config.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(ggplot2) })

## ---- SI Figure S9: adult cardiometabolic outcomes (H4) ----------------------
for (sfx in c("", "_all")) {
  f <- file.path(dir_out, paste0("si_adult_health_effects", sfx, ".csv"))
  if (!file.exists(f)) { message("[replot] skip (not found): ", basename(f)); next }
  plot_df <- read_csv(f, show_col_types = FALSE) %>%
    filter(adjusted %in% c(TRUE, "TRUE")) %>%
    mutate(exposure = recode(term, change_lpg_raw = "Raw NFHS",
                             change_lpg_rc = "Regression-calibrated",
                             change_lpg_bayes = "Bayesian-corrected"),
           exposure = factor(exposure, levels = c("Raw NFHS", "Regression-calibrated",
                                                  "Bayesian-corrected")))
  if (!all(c("lo_cr2_per10", "hi_cr2_per10") %in% names(plot_df)) ||
      any(!is.finite(plot_df$lo_cr2_per10) | !is.finite(plot_df$hi_cr2_per10)))
    stop("[replot] ", basename(f), " lacks CR2 intervals for some adjusted rows; ",
         "re-run H4_si_adult_health.R with clubSandwich installed.", call. = FALSE)
  p <- ggplot(plot_df, aes(est_per10, exposure)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_pointrange(aes(xmin = lo_cr2_per10, xmax = hi_cr2_per10)) +
    facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
    theme_bw() +
    labs(x = "Change in adult prevalence (pp) per 10-pp rise in LPG main-fuel prevalence\n(point estimate and 95% CR2 interval)",
         y = NULL, title = NULL, subtitle = NULL)
  out <- file.path(dir_out, paste0("si_adult_health_coefplot", sfx, ".jpeg"))
  ggsave(out, p, width = 8, height = 8.5, dpi = 300)
  message("[replot] wrote ", basename(out), " (CR2 intervals, ", nrow(plot_df), " rows)")
}

## ---- SI Figure S10: predicted composition metrics (H5) ----------------------
f <- file.path(dir_out, "health_nuanced_effects.csv")
if (file.exists(f)) {
  # clean_sign and the figure rows, exactly as EXP in H5_health_nuanced.R
  ORIENT <- c("Primary LPG (Bayes-corrected)"       =  1,
              "LPG, no solid fuel reported (share)" =  1,
              "Any solid fuel reported (share)"     = -1,
              "LPG and solid fuel reported (share)" = -1)
  lab_or <- c(
    "Primary LPG (Bayes-corrected)"       = "Primary LPG (+10 pp)",
    "LPG, no solid fuel reported (share)" = "LPG, no solid fuel reported (+10 pp)",
    "Any solid fuel reported (share)"     = "Any solid fuel reported (-10 pp)",
    "LPG and solid fuel reported (share)" = "LPG and solid fuel reported (-10 pp)")
  plot_df <- read_csv(f, show_col_types = FALSE) %>%
    filter(analysis == "point", exposure %in% names(ORIENT))
  if (nrow(plot_df) != 2 * length(ORIENT))
    stop("[replot] expected ", 2 * length(ORIENT), " point rows in ", basename(f),
         ", found ", nrow(plot_df), call. = FALSE)
  if (any(!is.finite(plot_df$lo_cr2) | !is.finite(plot_df$hi_cr2)))
    stop("[replot] ", basename(f), " lacks CR2 intervals for some plotted rows; ",
         "re-run H5_health_nuanced.R with clubSandwich installed.", call. = FALSE)
  plot_df <- plot_df %>%
    mutate(clean_sign = unname(ORIENT[exposure]),
           oe  = est_per10 * clean_sign,
           olo = pmin(lo_cr2 * clean_sign, hi_cr2 * clean_sign),
           ohi = pmax(lo_cr2 * clean_sign, hi_cr2 * clean_sign),
           exposure = recode(exposure, !!!lab_or),
           exposure = factor(exposure, levels = rev(unname(lab_or))),
           outcome = factor(outcome, levels = c("neonatal", "infant"),
                            labels = c("Neonatal", "Infant")))
  p <- ggplot(plot_df, aes(oe, exposure)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_pointrange(aes(xmin = olo, xmax = ohi)) +
    facet_wrap(~ outcome, ncol = 1) +
    theme_bw(base_size = 11) +
    labs(x = "Change in mortality (deaths per 100 births)\nper 10-pp move toward cleaner cooking (point estimate and 95% CR2 interval)",
         y = NULL,
         caption = paste("Exploratory. Shares sum to one, so each estimate is a",
                         "substitution contrast on predicted, not measured,\nfuel",
                         "composition; the estimates are not independent of one another."),
         title = NULL, subtitle = NULL)
  out <- file.path(dir_out, "health_nuanced_coefplot.jpeg")
  ggsave(out, p, width = 8, height = 8, dpi = 400)
  message("[replot] wrote ", basename(out), " (CR2 intervals)")
} else message("[replot] skip (not found): health_nuanced_effects.csv")

## ---- SI Figure S21: child mortality by exposure measurement (H2) ------------
# Same rows and labels as H2_health_models.R's coefficient plot (adjusted,
# birth-weighted, all districts), redrawn from the saved table so the x-axis
# title is wrapped rather than clipped.
f <- file.path(dir_out, "health_effects_table.csv")
if (file.exists(f)) {
  lab_h2 <- c(change_lpg_raw = "Raw NFHS", change_lpg_rc = "Regression-calibrated",
    change_lpg_bayes = "Bayesian-corrected\n(fixed surface)",
    change_lpg_bayes_MI = "Bayesian-corrected\n(exposure-only MI, sensitivity)",
    change_lpg_bayes_iresonly = "Bayesian-corrected\n(IRES cal, both rounds)",
    change_lpg_bayes_iresonly_MI = "Bayesian-corrected, IRES both\n(exposure-only MI)",
    change_lpg_bayes_accesswt = "Bayesian-corrected\n(design-weighted ACCESS ref)",
    change_lpg_bayes_accesswt_MI = "Bayesian-corrected, design-wtd ACCESS\n(exposure-only MI)")
  plot_df <- read_csv(f, show_col_types = FALSE) %>%
    filter(adjusted %in% c(TRUE, "TRUE"), weighting == "birth_hmean",
           support_stratum == "all", term %in% names(lab_h2)) %>%
    mutate(exposure = factor(unname(lab_h2[term]), levels = unname(lab_h2)),
           outcome = factor(outcome, levels = c("neonatal", "infant")))
  if (any(!is.finite(plot_df$ci_cr2_low) | !is.finite(plot_df$ci_cr2_high)))
    stop("[replot] health_effects_table.csv lacks CR2 intervals for some plotted rows", call. = FALSE)
  p <- ggplot(plot_df, aes(est_per10, exposure)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_pointrange(aes(xmin = 10 * ci_cr2_low, xmax = 10 * ci_cr2_high)) +
    facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
    theme_bw() +
    labs(x = "Change in mortality (deaths per 100 births) per 10-pp rise in LPG main-fuel prevalence\n(point estimate and 95% CR2 interval)",
         y = NULL, title = NULL, subtitle = NULL)
  out <- file.path(dir_out, "health_effects_coefplot.jpeg")
  ggsave(out, p, width = 8, height = 8, dpi = 300)
  message("[replot] wrote ", basename(out), " (CR2 intervals, ", nrow(plot_df), " rows)")
} else message("[replot] skip (not found): health_effects_table.csv")
