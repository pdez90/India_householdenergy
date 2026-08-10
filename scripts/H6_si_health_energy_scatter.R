# ==============================================================================
# H6_si_health_energy_scatter.R   (STANDALONE -- does not source 00_config.R)
#
# SI descriptive figure: simple (unadjusted) cross-sectional district-level
# associations between NFHS child-mortality prevalence and the REFERENCE
# survey's observed energy-metric prevalence, within the overlapping districts,
# for each temporally matched pair:
#     NFHS-4 mortality  vs  ACCESS Wave 1 energy metrics   (rural; ACCESS states)
#     NFHS-5 mortality  vs  IRES energy metrics            (rural)
#
# Energy metrics (observed in the reference survey, not predicted): primary-LPG
# share, exclusive-LPG share, any-solid-fuel-burning share and stacking share.
#
# ACCESS is a PANEL: wave 0 = 2014-15, wave 1 = 2018. Only wave 0 is temporally
# matched to NFHS-4, so only wave 0 is used here. Pooling both waves -- which an
# earlier version of this script did, because ref_energy() never filtered on
# wave -- mixes 2018 households into a 2015 comparison.
# Outcome: infant mortality (plotted); neonatal also in the CSV.
#
# This is purely descriptive -- unadjusted district scatter with Pearson r -- and
# complements the change-on-change analyses (H2/H5), which are the inferential
# results.
#
# Inputs (files on disk only):
#   access_hh.rds, ires_hh.rds        <- 02/03 prep (household energy data)
#   health_district_wide.rds           <- H1 (district mortality, *_2015/_2019)
# Output: maps/SI_health_energy_scatter.jpeg, si_health_energy_corr.csv
# ==============================================================================

dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers
suppressPackageStartupMessages({ library(tidyverse) })

path_access <- file.path(dir_out, "access_hh.rds")
path_ires   <- file.path(dir_out, "ires_hh.rds")
path_health <- file.path(dir_out, "health_district_wide.rds")
for (p in c(path_access, path_ires, path_health))
  if (!file.exists(p)) stop("Required input missing: ", p)

## ---- District energy metrics observed in a reference survey ------------------
## The four use3cat levels are created in 02_prep_access.R (lines ~129-136) and
## 03_prep_ires.R (lines ~104-111) and must be referenced by their exact
## canonical spelling. They are written out ONCE here so a mismatch is a single
## edit rather than four scattered string literals.
##
## Why the constant exists at all: an earlier version of this function matched
## "Solid only" and "LPG + solid (stacking)" -- labels that appear in NEITHER
## prep script. Every household therefore scored 0 on .anysolid and .stack, the
## district shares were constant, and cor() returned NA for two of the three
## plotted metrics. Nothing stopped: the figure rendered with "r = NA" over a
## vertical line of points at x = 0, si_health_energy_corr.csv wrote NA in eight
## of its twelve rows, and all three H6 checks reported PASS because none of
## them looked at the values. Exact matching is only safer than substring
## matching if the strings are right, so assert_levels() now verifies that they
## are, and the checks at the foot of this script test variance and finiteness
## rather than mere availability.
USE3 <- c(solid_only = "Solid fuel reported, no LPG",
          stacking   = "LPG and solid fuel reported",
          excl_lpg   = "LPG, no solid fuel reported",
          neither    = "Neither LPG nor solid fuel reported")

assert_levels <- function(u, what) {
  present <- unique(stats::na.omit(as.character(u)))
  gone    <- setdiff(unname(USE3), present)
  if (length(gone))
    stop("use3cat labels not found in ", what, ": ",
         paste(gone, collapse = " | "),
         "\n  levels actually observed: ", paste(present, collapse = " | "),
         "\n  Update USE3 in H6_si_health_energy_scatter.R to match ",
         "02_prep_access.R / 03_prep_ires.R.", call. = FALSE)
  invisible(TRUE)
}

ref_energy <- function(hh, what, rural_only = TRUE, wave_keep = NULL) {
  d <- hh
  if (!is.null(wave_keep)) {
    if (!"wave" %in% names(d))
      stop("wave_keep given but ", what, " has no 'wave' column.", call. = FALSE)
    d <- dplyr::filter(d, wave %in% wave_keep)
    message("[H6] ", what, ": kept wave ", paste(wave_keep, collapse = ","),
            " -> ", nrow(d), " households")
  }
  if (rural_only && "rural" %in% names(d)) d <- dplyr::filter(d, rural == 1)
  lpgcol <- intersect(c("main_fuel_lpg","primary_lpg","lpg"), names(d))[1]
  if (is.na(lpgcol)) stop("No LPG column found in ", what, call. = FALSE)
  if (!"use3cat" %in% names(d))
    stop("use3cat missing from ", what, " -- re-run 02/03 prep.", call. = FALSE)
  u <- as.character(d$use3cat)
  assert_levels(u, what)
  d %>%
    mutate(.lpg      = .data[[lpgcol]],
           .excl     = as.integer(u == USE3[["excl_lpg"]]),
           .anysolid = as.integer(u %in% USE3[c("solid_only", "stacking")]),
           .stack    = as.integer(u == USE3[["stacking"]]),
           district  = as.character(as.numeric(district))) %>%
    filter(!is.na(district)) %>%
    group_by(district) %>%
    summarise(primary_lpg = mean(.lpg, na.rm = TRUE),
              excl_lpg    = mean(.excl, na.rm = TRUE),
              any_solid   = mean(.anysolid, na.rm = TRUE),
              stacking    = mean(.stack, na.rm = TRUE),
              n_ref = n(), .groups = "drop")
}

# wave 0 only: NFHS-4 fieldwork is 2015-16 and ACCESS wave 0 is 2014-15. Wave 1
# (2018) belongs to no pair in this figure.
acc <- ref_energy(readRDS(path_access), "ACCESS wave 0 (2014-15, rural)",
                  wave_keep = 0)
ire <- ref_energy(readRDS(path_ires),   "IRES (rural)")

## ---- NFHS district mortality (as prevalence per 100 births) ------------------
health <- readRDS(path_health) %>% mutate(district = as.character(district))
mort <- function(yr) {
  cols <- paste0(c("neonatal_death","infant_death"), "_", yr)
  cols <- cols[cols %in% names(health)]
  health %>% select(district, all_of(cols)) %>%
    rename_with(~sub(paste0("_", yr), "", .x), all_of(cols)) %>%
    mutate(across(-district, ~ .x * 100))   # deaths per 100 births
}
h15 <- mort(2015); h19 <- mort(2019)

## ---- Build long plotting frame -----------------------------------------------
# NOTE: with FOUR categories, exclusive-LPG is NOT the exact mirror of any-solid
# -- "Other non-solid (non-LPG)" households are in neither, so the two shares do
# not sum to 1 and their mortality correlations are not exact sign reversals. We
# show three genuinely distinct metrics; exclusive-LPG is available in the CSV.
ENERGY <- c(primary_lpg = "Primary LPG", any_solid = "Any solid-fuel burning",
            stacking = "Fuel stacking")
OUT    <- c(neonatal_death = "Neonatal", infant_death = "Infant")

make_pair <- function(energy, health, pairlab) {
  d <- inner_join(energy, health, by = "district")
  map_dfr(names(ENERGY), function(em) {
    if (!em %in% names(d)) return(NULL)
    map_dfr(names(OUT), function(om) {
      if (!om %in% names(d)) return(NULL)
      tibble(pair = pairlab, energy = ENERGY[[em]], outcome = OUT[[om]],
             x = d[[em]], y = d[[om]], n = d$n_ref)
    })
  })
}
long <- bind_rows(
  make_pair(acc, h15, "NFHS-4 vs ACCESS (rural)"),
  make_pair(ire, h19, "NFHS-5 vs IRES (rural)")
) %>% filter(is.finite(x), is.finite(y))

## ---- Correlation table (all outcomes x metrics x pairs) ----------------------
corr_tab <- long %>% group_by(pair, energy, outcome) %>%
  summarise(n_districts = sum(is.finite(x) & is.finite(y)),
            pearson = suppressWarnings(cor(x, y, use = "pairwise.complete.obs")),
            .groups = "drop") %>%
  mutate(pearson = round(pearson, 3))
write_csv(corr_tab, file.path(dir_out, "si_health_energy_corr.csv"))
cat("\n== District cross-sectional correlations: NFHS mortality vs reference energy ==\n")
print(as.data.frame(corr_tab))

## ---- Figure: infant mortality vs energy metrics (both pairs) -----------------
dir.create(file.path(dir_out, "maps"), showWarnings = FALSE, recursive = TRUE)
pf <- long %>% filter(outcome == "Infant") %>%
  mutate(energy = factor(energy, levels = unname(ENERGY)))
rlab <- pf %>% group_by(pair, energy) %>%
  summarise(r = cor(x, y, use = "pairwise.complete.obs"),
            xr = max(x, na.rm = TRUE), yr = max(y, na.rm = TRUE), .groups = "drop")
p <- ggplot(pf, aes(x, y)) +
  geom_point(aes(size = n), alpha = 0.5, colour = "#2166AC", stroke = 0) +
  geom_smooth(method = "lm", se = TRUE, colour = "#B2182B", linewidth = 0.6) +
  geom_text(data = rlab, aes(x = xr, y = yr, label = sprintf("r = %.2f", r)),
            hjust = 1, vjust = 1, size = 3.2, inherit.aes = FALSE) +
  facet_grid(pair ~ energy, scales = "free_x") +
  scale_size_area(max_size = 3.2, guide = "none") +
  theme_bw(base_size = 10) +
  labs(x = "Reference-survey district energy-metric prevalence",
       y = "NFHS district infant mortality (deaths per 100 births)",
       title = NULL, subtitle = NULL)
ggsave(file.path(dir_out, "maps", "SI_health_energy_scatter.jpeg"), p,
       width = 10, height = 6.5, dpi = 300)

## ---- CHECKS ------------------------------------------------------------------
chk_header("H6_si_health_energy_scatter")
chk("H6", "energy-mortality correlation table produced",
    exists("corr_tab") && nrow(corr_tab) > 0,
    paste0(if (exists("corr_tab")) nrow(corr_tab) else 0, " rows"))
chk_file("H6", "correlation table written", "si_health_energy_corr.csv")
chk("H6", "both survey pairs present",
    exists("long") && dplyr::n_distinct(long$pair) == 2,
    paste(sort(unique(long$pair)), collapse = " | "))

# The three checks above all passed throughout the period when two of the three
# energy metrics were identically zero, because none of them looked at a value.
# These two do. A share that never varies cannot correlate with anything, so
# zero variance is the failure to catch -- and it is caught here, at the point
# where it is still a one-line label fix, rather than in the manuscript.
.var0 <- long %>% dplyr::group_by(pair, energy) %>%
  dplyr::summarise(sd = stats::sd(x, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(!is.finite(sd) | sd <= 0)
chk("H6", "every energy metric varies across districts",
    nrow(.var0) == 0,
    if (nrow(.var0))
      paste0("ZERO VARIANCE (check the use3cat labels in USE3): ",
             paste(.var0$pair, .var0$energy, sep = " / ", collapse = " | "))
    else paste0("all ", dplyr::n_distinct(long$pair) * dplyr::n_distinct(long$energy),
                " pair x metric combinations vary"))

.bad_r <- corr_tab %>% dplyr::filter(!is.finite(pearson))
chk("H6", "no correlation is NA",
    nrow(.bad_r) == 0,
    if (nrow(.bad_r))
      paste0("NA r in ", nrow(.bad_r), " of ", nrow(corr_tab), " rows: ",
             paste(.bad_r$pair, .bad_r$energy, .bad_r$outcome,
                   sep = " / ", collapse = " | "))
    else paste0("all ", nrow(corr_tab), " correlations finite; r range [",
                paste(sprintf("%.3f", range(corr_tab$pearson)), collapse = ", "), "]"))

message("\nH6 done -> maps/SI_health_energy_scatter.jpeg, si_health_energy_corr.csv in ", dir_out)
