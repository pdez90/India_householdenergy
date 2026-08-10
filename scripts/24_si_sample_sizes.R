# ==============================================================================
# 24_si_sample_sizes.R  (standalone; fast, no model fitting)
#
# Reviewer request: report the achieved sample size behind every district-level
# estimate, for every survey used in the calibration, RURAL-ONLY and ALL
# households, with state-level information.
#
# Surveys covered
#   NFHS-4 (2015-16), NFHS-5 (2019-21)   rural and all households
#   ACCESS Wave 1 (2014-15), Wave 2 (2018)   RURAL ONLY BY DESIGN -- ACCESS was
#       fielded in rural areas of six states, so there is no urban stratum and
#       no "all households" domain to report. Its cells are left empty in the
#       "all" columns rather than filled with the rural number, which would
#       falsely imply national/urban coverage.
#   IRES (2019-20)                        rural and all households
#
# "Sample size" is reported two ways, because both matter for a district-level
# analysis:
#   n_hh     households contributing a non-missing primary-cooking-fuel response
#            (the analytic denominator behind each district estimate)
#   n_clust  distinct primary sampling units behind those households
#            (NFHS: DHS cluster `clust`; ACCESS/IRES: village/ward code).
#            Households inside a cluster are not independent, so n_clust is the
#            number that governs how much information a district really carries.
#
# The counts are recomputed here from the household frames written by 01/02/03,
# and then checked cell-by-cell against the n columns already stored in
# nfhs_districts.rds / access_districts.rds / ires_districts.rds. They must agree
# exactly: the same filter (non-missing outcome, state, district, cluster) is
# applied here as in district_estimates_glmer() in 00_config.R.
#
# Inputs : nfhs_hh_covariates.rds (01), access_hh.rds (02), ires_hh.rds (03)
#          nfhs_districts.rds, access_districts.rds, ires_districts.rds [check]
#          ires_linkage_diagnostics.csv (03)                           [check]
#          comparison_table.csv (04)                                   [check]
#          district shapefile (path_districts_shp, 00_config.R)  [names/state]
#
# Outputs: si_sample_size_district.csv   one row per NFHS-4 district code
#          si_sample_size_state.csv      one row per state (+ _long version)
#          si_sample_size_summary.csv    one row per survey x domain (the
#                                        compact table for the response letter)
#          maps/SI_sample_size_summary.jpeg      SUMMARY FIGURE
#          maps/SI_sample_size_by_district.jpeg  detailed four-panel figure
#
# Run:     cd <scripts dir> && Rscript 24_si_sample_sizes.R
# ==============================================================================

source("00_config.R")
if (file.exists("checks.R")) source("checks.R")
need_inputs(c("nfhs_hh_covariates.rds" = "01_prep_nfhs.R",
              "nfhs_districts.rds"     = "01_prep_nfhs.R",
              "access_hh.rds"          = "02_prep_access.R",
              "access_districts.rds"   = "02_prep_access.R",
              "ires_hh.rds"            = "03_prep_ires.R",
              "ires_districts.rds"     = "03_prep_ires.R"))

library(ggplot2)
suppressPackageStartupMessages(library(cowplot))

dir_maps <- file.path(dir_out, "maps")
dir.create(dir_maps, showWarnings = FALSE, recursive = TRUE)

SURVEYS <- c("NFHS-4", "NFHS-5", "ACCESS W1", "ACCESS W2", "IRES")

message("\n================ 24_si_sample_sizes.R ================")

# ==============================================================================
# 1. Household frames
# ==============================================================================
nfhs_hh   <- readRDS(file.path(dir_out, "nfhs_hh_covariates.rds"))
access_hh <- readRDS(file.path(dir_out, "access_hh.rds"))
ires_hh   <- readRDS(file.path(dir_out, "ires_hh.rds"))

message("Loaded nfhs_hh_covariates.rds: ", nrow(nfhs_hh), " rows, ",
        ncol(nfhs_hh), " cols  (surveys: ",
        paste(sort(unique(nfhs_hh$survey)), collapse = ", "), ")")
message("Loaded access_hh.rds         : ", nrow(access_hh), " rows, ",
        ncol(access_hh), " cols  (waves: ",
        paste(sort(unique(access_hh$survey)), collapse = ", "), ")")
message("Loaded ires_hh.rds           : ", nrow(ires_hh), " rows, ",
        ncol(ires_hh), " cols")

# Fail early and loudly if a column moved, rather than silently counting NAs.
need_cols <- function(df, cols, what) {
  if (!all(cols %in% names(df)))
    stop(what, " is missing: ", paste(setdiff(cols, names(df)), collapse = ", "))
}
need_cols(nfhs_hh,   c("survey", "state", "district", "clust", "rural", "lpg"),
          "nfhs_hh_covariates.rds")
need_cols(access_hh, c("survey", "state", "district", "village", "main_fuel_lpg"),
          "access_hh.rds")
need_cols(ires_hh,   c("state", "district", "village", "rural", "main_fuel_lpg"),
          "ires_hh.rds")

# ==============================================================================
# 2. Counting rule -- identical to district_estimates_glmer() in 00_config.R
# ==============================================================================
# district_estimates_glmer() drops rows with a missing outcome, state, district
# or cluster before fitting, then counts rows per (state, district). Reproduce
# exactly that, so the counts here are the denominators of the published
# district estimates and not some looser row count.
count_district <- function(df, outcome, state, district, cluster, rural_only) {
  d <- df
  if (rural_only) {
    if (!"rural" %in% names(d))
      stop("count_district: no `rural` column to restrict on")
    d <- d[!is.na(d$rural) & d$rural == 1, , drop = FALSE]
  }
  d <- d[!is.na(d[[outcome]]) & !is.na(d[[state]]) &
         !is.na(d[[district]]) & !is.na(d[[cluster]]), , drop = FALSE]
  tibble::tibble(state    = as.character(d[[state]]),
                 district = as.character(d[[district]]),
                 clust    = as.character(d[[cluster]])) %>%
    dplyr::group_by(state, district) %>%
    dplyr::summarise(n_hh    = dplyr::n(),
                     n_clust = dplyr::n_distinct(clust),
                     .groups = "drop")
}

# One tidy long frame: survey x domain x district.
# `domains` lets ACCESS report only the rural domain it actually sampled.
grab <- function(df, survey, outcome, cluster, state = "state",
                 district = "district", domains = c("all", "rural")) {
  out <- list()
  if ("all" %in% domains)
    out$all <- count_district(df, outcome, state, district, cluster,
                              rural_only = FALSE) %>%
      dplyr::mutate(domain = "all")
  if ("rural" %in% domains) {
    # ACCESS carries no rural flag because it is rural by design: every sampled
    # household is rural, so the unfiltered count IS the rural count.
    ro <- "rural" %in% names(df)
    out$rural <- count_district(df, outcome, state, district, cluster,
                                rural_only = ro) %>%
      dplyr::mutate(domain = "rural")
  }
  dplyr::bind_rows(out) %>% dplyr::mutate(survey = survey, .before = 1)
}

nfhs4 <- grab(dplyr::filter(nfhs_hh, survey == "NFHS4"), "NFHS-4", "lpg", "clust")
nfhs5 <- grab(dplyr::filter(nfhs_hh, survey == "NFHS5"), "NFHS-5", "lpg", "clust")
acc1  <- grab(dplyr::filter(access_hh, survey == "ACCESS_W1"), "ACCESS W1",
              "main_fuel_lpg", "village", domains = "rural")
acc2  <- grab(dplyr::filter(access_hh, survey == "ACCESS_W2"), "ACCESS W2",
              "main_fuel_lpg", "village", domains = "rural")
ires  <- grab(ires_hh, "IRES", "main_fuel_lpg", "village")

long <- dplyr::bind_rows(nfhs4, nfhs5, acc1, acc2, ires) %>%
  dplyr::mutate(survey = factor(survey, levels = SURVEYS),
                domain = factor(domain, levels = c("rural", "all")))

message("\nDistrict-level counts computed:")
print(as.data.frame(
  long %>% dplyr::group_by(survey, domain) %>%
    dplyr::summarise(districts = dplyr::n_distinct(district),
                     households = sum(n_hh), clusters = sum(n_clust),
                     .groups = "drop")), row.names = FALSE)
message("(ACCESS is rural by design -- it has no 'all households' domain.)")

# ==============================================================================
# 3. District names and a single consistent state label
# ==============================================================================
# Every survey here is keyed to the NFHS-4 (2011 census) district code: NFHS-5
# clusters were assigned to that geography in 01, ACCESS districts in 02 and
# IRES districts in 03. So the shapefile is the one authoritative source of
# district and state names, and using it everywhere keeps the state rollup from
# splitting on spelling differences between surveys.
xwalk <- NULL
if (file.exists(path_districts_shp)) {
  xwalk <- sf::st_read(path_districts_shp, quiet = TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(district   = as.character(as.numeric(dist_code)),
                     dist_name  = as.character(dist_name),
                     state_name = as.character(state_name)) %>%
    dplyr::distinct(district, .keep_all = TRUE)
  message("\nDistrict shapefile crosswalk: ", nrow(xwalk), " districts, ",
          dplyr::n_distinct(xwalk$state_name), " states.")
} else {
  message("\n[note] Shapefile not found at ", path_districts_shp,
          " -- falling back to each survey's own state label and to the IRES ",
          "district_name where available.")
}

# IRES ships its own district name; keep it as a fallback and as a spot-check.
ires_names <- if ("district_name" %in% names(ires_hh)) {
  ires_hh %>%
    dplyr::filter(!is.na(district)) %>%
    dplyr::transmute(district = as.character(district),
                     ires_name = as.character(district_name)) %>%
    dplyr::distinct(district, .keep_all = TRUE)
} else {
  tibble::tibble(district = character(), ires_name = character())
}

# Survey-reported state label, used only where the crosswalk has no entry.
# ACCESS's numeric state codes are the least informative, so they sort last.
fallback_state <- long %>%
  dplyr::arrange(survey) %>%
  dplyr::distinct(district, .keep_all = TRUE) %>%
  dplyr::select(district, state_fallback = state)

label <- tibble::tibble(district = sort(unique(long$district))) %>%
  { if (is.null(xwalk)) dplyr::mutate(., dist_name = NA_character_,
                                      state_name = NA_character_)
    else dplyr::left_join(., xwalk, by = "district") } %>%
  dplyr::left_join(ires_names,     by = "district") %>%
  dplyr::left_join(fallback_state, by = "district") %>%
  dplyr::mutate(
    dist_name  = dplyr::coalesce(dist_name, ires_name),
    state_name = dplyr::coalesce(state_name, state_fallback)) %>%
  dplyr::select(district, dist_name, state_name)

# ==============================================================================
# 4. Output 1 -- wide district table
# ==============================================================================
wide <- long %>%
  tidyr::pivot_wider(
    id_cols = district,
    names_from = c(survey, domain),
    values_from = c(n_hh, n_clust),
    names_glue = "{survey}_{domain}_{.value}") %>%
  dplyr::rename_with(~ gsub("[- ]", "", .x)) %>%
  dplyr::rename_with(~ gsub("n_hh", "hh", .x)) %>%
  dplyr::rename_with(~ gsub("n_clust", "clust", .x))

ord <- c("NFHS4_rural_hh",    "NFHS4_rural_clust",
         "NFHS4_all_hh",      "NFHS4_all_clust",
         "NFHS5_rural_hh",    "NFHS5_rural_clust",
         "NFHS5_all_hh",      "NFHS5_all_clust",
         "ACCESSW1_rural_hh", "ACCESSW1_rural_clust",
         "ACCESSW2_rural_hh", "ACCESSW2_rural_clust",
         "IRES_rural_hh",     "IRES_rural_clust",
         "IRES_all_hh",       "IRES_all_clust")
missing_cols <- setdiff(ord, names(wide))
if (length(missing_cols))
  message("[note] absent count columns (survey x domain not sampled): ",
          paste(missing_cols, collapse = ", "))
ord <- ord[ord %in% names(wide)]

district_tab <- label %>%
  dplyr::left_join(wide, by = "district") %>%
  dplyr::select(district, dist_name, state_name, dplyr::all_of(ord))

# Districts carrying BOTH sides of a calibration pair are the ones that pair is
# actually estimated on; flag them so a reviewer can see the overlap without
# recomputing it. Guarded: a flag is NA if either column is absent.
flag_overlap <- function(tab, a, b) {
  if (all(c(a, b) %in% names(tab)))
    as.integer(!is.na(tab[[a]]) & !is.na(tab[[b]]))
  else { message("[note] cannot flag ", a, " x ", b, " -- column absent")
         NA_integer_ }
}
district_tab$in_nfhs4_access_overlap <-
  flag_overlap(district_tab, "NFHS4_rural_hh", "ACCESSW1_rural_hh")
district_tab$in_nfhs5_ires_overlap <-
  flag_overlap(district_tab, "NFHS5_rural_hh", "IRES_rural_hh")

district_tab <- district_tab %>%
  dplyr::arrange(state_name, dist_name, district)

readr::write_csv(district_tab, file.path(dir_out, "si_sample_size_district.csv"))
message("\nWrote si_sample_size_district.csv: ", nrow(district_tab),
        " districts x ", ncol(district_tab), " columns.")

# ==============================================================================
# 5. Output 2 -- state-level table
# ==============================================================================
state_long <- long %>%
  dplyr::left_join(label, by = "district") %>%
  dplyr::group_by(state_name, survey, domain) %>%
  dplyr::summarise(districts  = dplyr::n_distinct(district),
                   households = sum(n_hh),
                   clusters   = sum(n_clust),
                   hh_median  = stats::median(n_hh),
                   hh_q25     = stats::quantile(n_hh, 0.25, names = FALSE),
                   hh_q75     = stats::quantile(n_hh, 0.75, names = FALSE),
                   hh_min     = min(n_hh),
                   hh_max     = max(n_hh),
                   clust_median = stats::median(n_clust),
                   .groups = "drop")

state_tab <- state_long %>%
  tidyr::pivot_wider(
    id_cols = state_name,
    names_from = c(survey, domain),
    values_from = c(districts, households, clusters, hh_median),
    names_glue = "{survey}_{domain}_{.value}") %>%
  dplyr::rename_with(~ gsub("[- ]", "", .x)) %>%
  dplyr::arrange(state_name)

readr::write_csv(state_tab,  file.path(dir_out, "si_sample_size_state.csv"))
readr::write_csv(state_long, file.path(dir_out, "si_sample_size_state_long.csv"))
message("Wrote si_sample_size_state.csv: ", nrow(state_tab), " states.")

# ==============================================================================
# 6. Output 3 -- compact national summary (the table for the response letter)
# ==============================================================================
summary_tab <- long %>%
  dplyr::group_by(survey, domain) %>%
  dplyr::summarise(
    districts        = dplyr::n_distinct(district),
    states           = dplyr::n_distinct(state),
    households_total = sum(n_hh),
    clusters_total   = sum(n_clust),
    hh_per_district_median = stats::median(n_hh),
    hh_per_district_q25    = stats::quantile(n_hh, 0.25, names = FALSE),
    hh_per_district_q75    = stats::quantile(n_hh, 0.75, names = FALSE),
    hh_per_district_min    = min(n_hh),
    hh_per_district_max    = max(n_hh),
    districts_lt_30_hh     = sum(n_hh < 30),
    districts_lt_50_hh     = sum(n_hh < 50),
    clust_per_district_median = stats::median(n_clust),
    clust_per_district_min    = min(n_clust),
    clust_per_district_max    = max(n_clust),
    districts_1_cluster       = sum(n_clust == 1),
    .groups = "drop") %>%
  dplyr::mutate(
    hh_per_district_iqr = paste0(hh_per_district_median, " [",
                                 hh_per_district_q25, ", ",
                                 hh_per_district_q75, "]"),
    .after = districts)

readr::write_csv(summary_tab, file.path(dir_out, "si_sample_size_summary.csv"))
message("Wrote si_sample_size_summary.csv.\n")
print(as.data.frame(summary_tab %>%
  dplyr::select(survey, domain, districts, states, households_total,
                clusters_total, hh_per_district_iqr, hh_per_district_min,
                hh_per_district_max, districts_lt_30_hh, districts_1_cluster)),
  row.names = FALSE)

# ==============================================================================
# 6b. Two things a reviewer will ask about next -- print them explicitly
# ==============================================================================
# (i) Thinnest districts. A district estimate resting on a handful of households
#     or a single cluster is exactly what the calibration's equation error has
#     to absorb, so name them rather than leaving them inside a median.
thin <- long %>%
  dplyr::filter(domain == "rural", n_hh < 50) %>%
  dplyr::left_join(label, by = "district") %>%
  dplyr::arrange(survey, n_hh) %>%
  dplyr::select(survey, district, dist_name, state_name, n_hh, n_clust)
message("\nRural districts with fewer than 50 households (", nrow(thin), "):")
if (nrow(thin) > 0) print(as.data.frame(thin), row.names = FALSE)

one_clust <- long %>%
  dplyr::filter(n_clust == 1) %>%
  dplyr::left_join(label, by = "district") %>%
  dplyr::select(survey, domain, district, dist_name, state_name, n_hh)
message("\nDistrict-survey cells resting on a single cluster (",
        nrow(one_clust), "):")
if (nrow(one_clust) > 0) print(as.data.frame(one_clust), row.names = FALSE)

# (ii) State-count reconciliation. The surveys' own state labels and the NFHS-4
#     (2011 census) geography need not give the same state count -- states that
#     split after 2011 collapse back into their parent under this geography.
#     Report both so the manuscript's state count can be stated unambiguously.
state_recon <- long %>%
  dplyr::left_join(label, by = "district") %>%
  dplyr::group_by(survey, domain) %>%
  dplyr::summarise(states_survey_label = dplyr::n_distinct(state),
                   states_nfhs4_geography = dplyr::n_distinct(state_name),
                   .groups = "drop")
message("\nState counts, survey label vs NFHS-4 geography:")
print(as.data.frame(state_recon), row.names = FALSE)

# ==============================================================================
# 7. Figures
# ==============================================================================
theme_ss <- theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 9, face = "plain"),
        legend.position = "bottom",
        legend.title = element_blank())

pal <- c("NFHS-4"    = "#1b6ca8", "NFHS-5" = "#3fa34d",
         "ACCESS W1" = "#e08a1e", "ACCESS W2" = "#8a5fbf",
         "IRES"      = "#d1495b")

# Explicit log breaks: ggplot's default log10 breaks drop or duplicate labels on
# a range this wide, which makes the axis unreadable.
lbreaks <- c(10, 30, 100, 300, 1000, 3000, 10000)
llabels <- format(lbreaks, big.mark = ",", trim = TRUE)

dom_lab <- c(rural = "Rural households",
             all   = "All households (rural + urban)")

# ---------------------------------------------------------- SUMMARY FIGURE ----
# One picture answering the reviewer: how much data stands behind a district
# estimate in each survey. Survey on the y-axis, log sample size on the x-axis,
# box = IQR, whiskers = range, points = individual districts, and the district
# count printed at the right edge of each row.
sf_dat <- long %>%
  dplyr::mutate(dom = factor(dom_lab[as.character(domain)],
                             levels = unname(dom_lab[c("rural", "all")])))
sf_lab <- sf_dat %>%
  dplyr::group_by(dom, survey) %>%
  dplyr::summarise(k = dplyr::n(), .groups = "drop") %>%
  dplyr::mutate(txt = paste0(k, " districts")) %>%
  dplyr::filter(k > 0)

# ACCESS has no rows in the "all households" panel. Say so on the panel rather
# than leaving two blank rows a reader has to interpret.
sf_none <- tidyr::expand_grid(dom = levels(sf_dat$dom), survey = SURVEYS) %>%
  dplyr::anti_join(sf_lab %>%
                     dplyr::mutate(dom = as.character(dom),
                                   survey = as.character(survey)),
                   by = c("dom", "survey")) %>%
  dplyr::mutate(dom = factor(dom, levels = levels(sf_dat$dom)),
                survey = factor(survey, levels = SURVEYS),
                xpos = min(lbreaks),
                txt = "not sampled (rural-only survey)")

box_panel <- function(v, xlab, title) {
  ggplot(sf_dat, aes(x = .data[[v]], y = survey, colour = survey,
                     fill = survey)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.20, linewidth = 0.35,
                 width = 0.6) +
    geom_jitter(height = 0.16, size = 0.5, alpha = 0.35, stroke = 0) +
    geom_text(data = sf_lab, aes(x = Inf, y = survey, label = txt),
              inherit.aes = FALSE, hjust = 1.05, vjust = -1.1, size = 2.4,
              colour = "grey30") +
    { if (nrow(sf_none))
        geom_text(data = sf_none, aes(x = xpos, y = survey, label = txt),
                  inherit.aes = FALSE, hjust = 0, size = 2.4,
                  colour = "grey55", fontface = "italic")
      else NULL } +
    scale_x_log10(breaks = lbreaks, labels = llabels) +
    scale_y_discrete(limits = rev(SURVEYS)) +
    scale_colour_manual(values = pal) + scale_fill_manual(values = pal) +
    facet_wrap(~ dom, ncol = 1) +
    labs(x = xlab, y = NULL, title = title) +
    theme_ss + theme(legend.position = "none")
}

fig_sum <- cowplot::plot_grid(
  box_panel("n_hh",    "Households per district (log scale)",
            "Households behind each district estimate"),
  box_panel("n_clust", "Sampling clusters per district (log scale)",
            "Independent clusters behind each district estimate"),
  ncol = 2, labels = c("a", "b"), label_size = 11)

fig_sum_path <- file.path(dir_maps, "SI_sample_size_summary.jpeg")
ggsave(fig_sum_path, fig_sum, width = 9.5, height = 5.6, dpi = 400)
message("\nWrote ", fig_sum_path)

# --------------------------------------------------------- DETAILED FIGURE ----
# (a) distribution of rural sample size per district, by survey
pa <- ggplot(dplyr::filter(long, domain == "rural"),
             aes(x = n_hh, fill = survey)) +
  geom_histogram(bins = 40, colour = NA, alpha = 0.85) +
  scale_x_log10(breaks = lbreaks, labels = llabels) +
  scale_fill_manual(values = pal) +
  facet_wrap(~ survey, ncol = 1, scales = "free_y") +
  labs(x = "Rural households per district (log scale)", y = "Districts",
       title = "Rural sample size per district") +
  theme_ss + theme(legend.position = "none")

# (b) state-level median with the district IQR, rural domain
pb_dat <- state_long %>%
  dplyr::filter(domain == "rural", !is.na(state_name))
state_order <- pb_dat %>%
  dplyr::group_by(state_name) %>%
  dplyr::summarise(m = stats::median(hh_median), .groups = "drop") %>%
  dplyr::arrange(m) %>% dplyr::pull(state_name)
pb_dat <- pb_dat %>%
  dplyr::mutate(state_name = factor(state_name, levels = state_order))

pb <- ggplot(pb_dat, aes(x = hh_median, y = state_name, colour = survey)) +
  geom_linerange(aes(xmin = hh_q25, xmax = hh_q75),
                 position = position_dodge(width = 0.7), linewidth = 0.4) +
  geom_point(position = position_dodge(width = 0.7), size = 1.1) +
  scale_x_log10(breaks = lbreaks, labels = llabels) +
  scale_colour_manual(values = pal) +
  labs(x = "Rural households per district: median [IQR] (log scale)", y = NULL,
       title = "By state") +
  theme_ss

# (c, d) the two pairs the calibration is actually estimated on.
# No 45-degree line: the surveys differ by more than an order of magnitude, so
# an identity reference falls outside the panel and would mislead. The
# informative quantity is the ratio, so it goes in the subtitle.
pair_panel <- function(xcol, ycol, xlab, ylab, flag, colour) {
  if (!all(c(xcol, ycol, flag) %in% names(district_tab)))
    return(ggplot() + theme_void())
  d <- district_tab %>%
    dplyr::filter(!is.na(.data[[flag]]), .data[[flag]] == 1)
  if (nrow(d) == 0) return(ggplot() + theme_void())
  rat <- stats::median(d[[xcol]] / d[[ycol]], na.rm = TRUE)
  ggplot(d, aes(x = .data[[xcol]], y = .data[[ycol]])) +
    geom_point(alpha = 0.7, size = 1.4, colour = colour) +
    scale_x_log10(breaks = lbreaks, labels = llabels) +
    scale_y_log10(breaks = lbreaks, labels = llabels) +
    labs(x = xlab, y = ylab,
         title = paste0("Linked districts (n = ", nrow(d), ")"),
         subtitle = paste0("median ratio = ", round(rat), " to 1")) +
    theme_ss + theme(plot.subtitle = element_text(size = 8))
}

pc <- pair_panel("NFHS4_rural_hh", "ACCESSW1_rural_hh",
                 "NFHS-4 rural households", "ACCESS W1 households",
                 "in_nfhs4_access_overlap", pal[["ACCESS W1"]])
pd <- pair_panel("NFHS5_rural_hh", "IRES_rural_hh",
                 "NFHS-5 rural households", "IRES rural households",
                 "in_nfhs5_ires_overlap", pal[["IRES"]])

fig <- cowplot::plot_grid(
  pa,
  cowplot::plot_grid(
    pb,
    cowplot::plot_grid(pc, pd, ncol = 2, labels = c("c", "d"), label_size = 11),
    ncol = 1, rel_heights = c(2, 1.15), labels = c("b", ""), label_size = 11),
  ncol = 2, rel_widths = c(1, 1.35), labels = c("a", ""), label_size = 11)

fig_path <- file.path(dir_maps, "SI_sample_size_by_district.jpeg")
ggsave(fig_path, fig, width = 10.5, height = 9, dpi = 400)
message("Wrote ", fig_path)

# ==============================================================================
# CHECKS
# ==============================================================================
if (exists("chk_header")) chk_header("24_si_sample_sizes") else
  cat("\n== CHECKS [24_si_sample_sizes] ==\n")
.chk  <- if (exists("chk"))      chk      else function(...) invisible(TRUE)
.warn <- if (exists("chk_warn")) chk_warn else function(...) invisible(TRUE)
S <- "24_si_sample_sizes.R"

# --- (1) recomputed counts must equal the counts stored by 01, 02 and 03 ------
# This is the check that matters: if it passes, the numbers in this table are
# literally the denominators behind the district estimates in the manuscript.
nfhs_d <- readRDS(file.path(dir_out, "nfhs_districts.rds")) %>%
  dplyr::mutate(district = as.character(district))
acc_d  <- readRDS(file.path(dir_out, "access_districts.rds")) %>%
  dplyr::mutate(district = as.character(district))
ires_d <- readRDS(file.path(dir_out, "ires_districts.rds")) %>%
  dplyr::mutate(district = as.character(district))

cmp_n <- function(stored_df, stored_col, survey_lab, domain_lab) {
  if (!stored_col %in% names(stored_df)) {
    .warn(S, paste0("n cross-check: ", stored_col), FALSE, "column absent")
    return(invisible(NULL))
  }
  a <- long %>%
    dplyr::filter(survey == survey_lab, domain == domain_lab) %>%
    dplyr::select(district, n_here = n_hh)
  b <- stored_df %>%
    dplyr::transmute(district, n_stored = as.numeric(.data[[stored_col]])) %>%
    dplyr::filter(!is.na(n_stored))
  j <- dplyr::full_join(a, b, by = "district")
  bad <- j %>% dplyr::filter(is.na(n_here) | is.na(n_stored) |
                             n_here != n_stored)
  .chk(S, paste0("n matches ", stored_col),
       nrow(bad) == 0 && nrow(j) > 0,
       sprintf("%d districts compared, %d mismatched%s", nrow(j), nrow(bad),
               if (nrow(bad) > 0)
                 paste0(" (e.g. district ", bad$district[1], ": here ",
                        bad$n_here[1], " vs stored ", bad$n_stored[1], ")")
               else ""))
  if (nrow(bad) > 0) {
    message("  mismatching districts for ", stored_col, ":")
    print(utils::head(as.data.frame(bad), 10))
  }
  invisible(bad)
}

cmp_n(nfhs_d, "n_2015",       "NFHS-4",    "all")
cmp_n(nfhs_d, "n_2015_rural", "NFHS-4",    "rural")
cmp_n(nfhs_d, "n_2019",       "NFHS-5",    "all")
cmp_n(nfhs_d, "n_2019_rural", "NFHS-5",    "rural")
cmp_n(acc_d,  "n_access_w1",  "ACCESS W1", "rural")
cmp_n(acc_d,  "n_access_w2",  "ACCESS W2", "rural")
cmp_n(ires_d, "n_ires",       "IRES",      "all")
cmp_n(ires_d, "n_ires_rural", "IRES",      "rural")

# --- (2) internal consistency -------------------------------------------------
rur_le_all <- long %>%
  tidyr::pivot_wider(id_cols = c(survey, district),
                     names_from = domain, values_from = n_hh) %>%
  dplyr::filter(!is.na(rural), !is.na(all), rural > all)
.chk(S, "rural n never exceeds all-household n",
     nrow(rur_le_all) == 0,
     sprintf("%d violating district-survey cells", nrow(rur_le_all)))

.chk(S, "clusters <= households in every cell",
     all(long$n_clust <= long$n_hh),
     sprintf("max clusters/households ratio = %.3f",
             max(long$n_clust / long$n_hh)))

.chk(S, "no missing district key in outputs",
     !any(is.na(district_tab$district)) && !any(district_tab$district == ""),
     sprintf("%d district rows", nrow(district_tab)))

dupe_state <- long %>%
  dplyr::distinct(survey, district, state) %>%
  dplyr::count(survey, district) %>% dplyr::filter(n > 1)
.chk(S, "district code maps to one state within each survey",
     nrow(dupe_state) == 0,
     sprintf("%d district codes spanning >1 state", nrow(dupe_state)))

# ACCESS should be rural-only and confined to the six ACCESS states.
acc_states <- long %>%
  dplyr::filter(survey %in% c("ACCESS W1", "ACCESS W2")) %>%
  dplyr::left_join(label, by = "district") %>%
  dplyr::pull(state_name)
acc_states <- sort(unique(acc_states[!is.na(acc_states)]))
.chk(S, "ACCESS confined to the six ACCESS states",
     length(acc_states) == 6,
     sprintf("%d states: %s", length(acc_states),
             paste(acc_states, collapse = "; ")))
.chk(S, "ACCESS reported for the rural domain only",
     !any(long$survey %in% c("ACCESS W1", "ACCESS W2") & long$domain == "all"),
     "no 'all households' rows for ACCESS")

# --- (3) totals against the pipeline's own diagnostics ------------------------
lk_path <- file.path(dir_out, "ires_linkage_diagnostics.csv")
if (file.exists(lk_path)) {
  lk <- readr::read_csv(lk_path, show_col_types = FALSE)
  getlk <- function(m) {
    v <- lk$value[lk$metric == m]
    if (length(v) == 1) as.numeric(v) else NA_real_
  }
  ires_all <- summary_tab %>% dplyr::filter(survey == "IRES", domain == "all")
  .chk(S, "IRES household total matches linkage diagnostics",
       isTRUE(ires_all$households_total == getlk("IRES households, total")),
       sprintf("here %s vs linkage %s", ires_all$households_total,
               getlk("IRES households, total")))
  .chk(S, "IRES district count matches linkage diagnostics",
       isTRUE(ires_all$districts == getlk("IRES distinct NFHS-4 districts")),
       sprintf("here %s vs linkage %s", ires_all$districts,
               getlk("IRES distinct NFHS-4 districts")))
  ires_rur <- summary_tab %>% dplyr::filter(survey == "IRES", domain == "rural")
  sh <- getlk("share of households rural")
  .warn(S, "IRES rural share consistent with linkage diagnostics",
        !is.na(sh) &&
          abs(ires_rur$households_total / ires_all$households_total - sh) < 0.01,
        sprintf("here %.4f vs linkage %.4f",
                ires_rur$households_total / ires_all$households_total, sh))
}

# The overlap flags must reproduce the n_districts already reported by 04 for
# the two head-to-head comparisons, or the table and the results disagree.
cmp_path <- file.path(dir_out, "comparison_table.csv")
if (file.exists(cmp_path)) {
  ct <- readr::read_csv(cmp_path, show_col_types = FALSE)
  getct <- function(pat) {
    v <- ct$n_districts[grepl(pat, ct$comparison, fixed = TRUE)]
    if (length(v) >= 1) as.numeric(v[1]) else NA_real_
  }
  n_acc  <- sum(district_tab$in_nfhs4_access_overlap == 1, na.rm = TRUE)
  n_ires <- sum(district_tab$in_nfhs5_ires_overlap  == 1, na.rm = TRUE)
  .chk(S, "NFHS-4/ACCESS overlap matches comparison_table.csv",
       isTRUE(n_acc == getct("NFHS-4 (rural) vs ACCESS W1")),
       sprintf("here %d vs 04 %s", n_acc, getct("NFHS-4 (rural) vs ACCESS W1")))
  .chk(S, "NFHS-5/IRES rural overlap matches comparison_table.csv",
       isTRUE(n_ires == getct("NFHS-5 (rural) vs IRES (rural)")),
       sprintf("here %d vs 04 %s", n_ires,
               getct("NFHS-5 (rural) vs IRES (rural)")))
}

# --- (4) labelling coverage ---------------------------------------------------
.warn(S, "district name resolved for every district",
      !any(is.na(district_tab$dist_name)),
      sprintf("%d of %d unnamed", sum(is.na(district_tab$dist_name)),
              nrow(district_tab)))
.warn(S, "state name resolved for every district",
      !any(is.na(district_tab$state_name)),
      sprintf("%d of %d unnamed", sum(is.na(district_tab$state_name)),
              nrow(district_tab)))

# IRES's own district name vs the shapefile name, where both exist: a low match
# rate would mean the code linkage put IRES households in the wrong polygon.
if (!is.null(xwalk) && nrow(ires_names) > 0) {
  nm <- ires_names %>%
    dplyr::inner_join(xwalk, by = "district") %>%
    dplyr::mutate(a = gsub("[^a-z]", "", tolower(ires_name)),
                  b = gsub("[^a-z]", "", tolower(dist_name)))
  agree <- mean(nm$a == nm$b, na.rm = TRUE)
  .warn(S, "IRES district name agrees with shapefile name",
        agree >= 0.80,
        sprintf("%.1f%% exact match over %d linked districts",
                100 * agree, nrow(nm)))
}

# --- (5) outputs on disk ------------------------------------------------------
for (f in c("si_sample_size_district.csv", "si_sample_size_state.csv",
            "si_sample_size_state_long.csv", "si_sample_size_summary.csv")) {
  p <- file.path(dir_out, f)
  .chk(S, paste0("wrote ", f), file.exists(p) && file.size(p) > 0,
       if (file.exists(p)) paste0(file.size(p), " bytes") else "missing")
}
for (p in c(fig_sum_path, fig_path)) {
  .chk(S, paste0("wrote ", basename(p)),
       file.exists(p) && file.size(p) > 0,
       if (file.exists(p)) paste0(file.size(p), " bytes") else "missing")
}

message("\n== 24_si_sample_sizes.R complete ==")
message("Tables : ", file.path(dir_out, "si_sample_size_district.csv"))
message("         ", file.path(dir_out, "si_sample_size_state.csv"))
message("         ", file.path(dir_out, "si_sample_size_summary.csv"))
message("Figures: ", fig_sum_path, "   <- summary")
message("         ", fig_path, "\n")
