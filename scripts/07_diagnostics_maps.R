# ==============================================================================
# 07_diagnostics_maps.R
# One-stop diagnostics + maps so you can eyeball whether everything looks right.
#
# Produces (in dir_out):
#   diagnostics_summary.txt      -- plain-text sanity checks & key metrics
#   maps/                        -- individual choropleth jpegs
#   maps_overview.pdf            -- all maps + diagnostic plots, one per page
#
# Maps:
#   1. NFHS-4 rural P(main LPG)        2. NFHS-5 rural P(main LPG)
#   3. Corrected 2015 (Bayes)          4. Corrected 2019 (Bayes)
#   5. Correction shift (corrected - raw), 2019
#   6. NFHS-4 vs ACCESS W1 difference (overlap districts)
#   7. NFHS-5 vs IRES difference (overlap districts)
#   8. Predicted stacking P(LPG main & solid use), NFHS-5
#   9. Predicted % any solid burning, NFHS-5
#  10. Predicted LPG kg/yr, NFHS-5
#  11. Training support flag (which districts rely on extrapolation)
#
# Inputs : outputs of 01-06 + district shapefile
# ==============================================================================

source("00_config.R")
need_inputs(c("nfhs_districts.rds"           = "01_prep_nfhs.R",
              "access_districts.rds"         = "02_prep_access.R",
              "ires_districts.rds"           = "03_prep_ires.R",
              "compare_pairs.rds"            = "04_compare.R",
              "corrected_nfhs_districts.rds" = "05_correction.R",
              "district_exposure_proxy.rds"  = "06_stacking_prediction.R"))

dir_maps <- file.path(dir_out, "maps")
dir.create(dir_maps, showWarnings = FALSE)

nfhs      <- readRDS(file.path(dir_out, "nfhs_districts.rds"))
access    <- readRDS(file.path(dir_out, "access_districts.rds"))
ires      <- readRDS(file.path(dir_out, "ires_districts.rds"))
pairs     <- readRDS(file.path(dir_out, "compare_pairs.rds"))
corrected <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))
proxy     <- readRDS(file.path(dir_out, "district_exposure_proxy.rds"))

shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(district = as.character(as.numeric(dist_code))) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

# ---- Assemble one district-level frame -----------------------------------------
proxy5 <- proxy %>% filter(survey == "NFHS5") %>%
  select(district, p_stack, p_stacking, p_solid_only, p_excl_lpg,
         p_other_nonsolid, p_any_solid_burning, e_lpg_kg_yr, in_support,
         # n_hh is carried so the extrapolation diagnostic below can report a
         # household-weighted support share alongside the district share.
         any_of("n_hh"))

d <- shp %>%
  left_join(corrected %>%
              select(district, lpg_2015_rural, lpg_2019_rural,
                     any_of(c("lpg_2015_rc", "lpg_2019_rc",
                              "lpg_2015_bayes", "lpg_2019_bayes",
                              "lpg_2015_bayes_lo", "lpg_2015_bayes_hi",
                              "lpg_2019_bayes_lo", "lpg_2019_bayes_hi"))),
            by = "district") %>%
  left_join(access %>% select(district, access_w1_mainlpg), by = "district") %>%
  left_join(ires   %>% select(district, ires_mainlpg_rural), by = "district") %>%
  left_join(proxy5, by = "district") %>%
  mutate(
    diff_nfhs4_access = lpg_2015_rural - access_w1_mainlpg,
    diff_nfhs5_ires   = lpg_2019_rural - ires_mainlpg_rural,
    corr_shift_2019   = if ("lpg_2019_bayes" %in% names(.))
                          lpg_2019_bayes - lpg_2019_rural else NA_real_
  )

# ---- Map helpers -----------------------------------------------------------------
map_fill <- function(data, var, title, limits = c(0, 1), diverging = FALSE,
                     unit = NULL) {
  g <- ggplot(data) +
    geom_sf(aes(fill = .data[[var]]), color = NA) +
    geom_sf(data = data, fill = NA, color = "grey70", linewidth = 0.05) +
    theme_void(base_size = 11) +
    labs(title = title, fill = unit %||% "") +
    theme(plot.title = element_text(face = "bold", size = 12),
          legend.position = "right")
  if (diverging) {
    lim <- suppressWarnings(max(abs(range(data[[var]], na.rm = TRUE))))
    if (!is.finite(lim) || lim == 0) lim <- 1   # all-NA/constant -> safe finite limit
    g + scale_fill_gradient2(low = "#2166AC", mid = "grey95", high = "#B2182B",
                             midpoint = 0, limits = c(-lim, lim),
                             na.value = "grey88")
  } else if (!is.null(limits)) {
    g + scale_fill_viridis_c(limits = limits, na.value = "grey88")
  } else {
    g + scale_fill_viridis_c(na.value = "grey88")
  }
}
`%||%` <- function(a, b) if (is.null(a)) b else a

save_map <- function(plot, name, w = 7, h = 7) {
  ggsave(file.path(dir_maps, paste0(name, ".jpeg")), plot,
         width = w, height = h, dpi = 250)
  plot
}

maps <- list()
maps$m1 <- save_map(map_fill(d, "lpg_2015_rural",
  "NFHS-4 (2015-16): P(main fuel = LPG), rural"), "01_nfhs4_rural_lpg")
maps$m2 <- save_map(map_fill(d, "lpg_2019_rural",
  "NFHS-5 (2019-21): P(main fuel = LPG), rural"), "02_nfhs5_rural_lpg")

if ("lpg_2015_bayes" %in% names(d))
  maps$m3 <- save_map(map_fill(d, "lpg_2015_bayes",
    "Corrected 2015 estimate (Bayesian ME model, ACCESS-calibrated)"),
    "03_corrected_2015_bayes")
if ("lpg_2019_bayes" %in% names(d))
  maps$m4 <- save_map(map_fill(d, "lpg_2019_bayes",
    "Corrected 2019 estimate (Bayesian ME model, IRES-calibrated)"),
    "04_corrected_2019_bayes")

maps$m5 <- save_map(map_fill(d, "corr_shift_2019",
  "Correction shift 2019 (corrected - raw NFHS-5 rural)", limits = NULL,
  diverging = TRUE), "05_correction_shift_2019")
maps$m6 <- save_map(map_fill(d, "diff_nfhs4_access",
  "NFHS-4 rural minus ACCESS W1 (overlap districts)", limits = NULL,
  diverging = TRUE), "06_diff_nfhs4_access")
maps$m7 <- save_map(map_fill(d, "diff_nfhs5_ires",
  "NFHS-5 rural minus IRES rural (overlap districts)", limits = NULL,
  diverging = TRUE), "07_diff_nfhs5_ires")
maps$m8 <- save_map(map_fill(d, "p_stack",
  "Predicted P(stacking | LPG main fuel), NFHS-5 households"),
  "08_pred_stacking_nfhs5")
maps$m9 <- save_map(map_fill(d, "p_any_solid_burning",
  "Predicted share burning any solid fuel, NFHS-5"),
  "09_pred_any_solid_nfhs5")
maps$m10 <- save_map(map_fill(d, "e_lpg_kg_yr",
  "Predicted household LPG use (kg/yr), NFHS-5", limits = NULL,
  unit = "kg/yr"), "10_pred_lpg_kg_yr_nfhs5")
maps$m11 <- save_map(
  ggplot(d) +
    geom_sf(aes(fill = in_support), color = NA) +
    scale_fill_viridis_c(limits = c(0, 1), na.value = "grey88",
                         option = "cividis") +
    theme_void(base_size = 11) +
    labs(title = "Share of district households in training-survey states\n(1 = interpolation, <1 = relies on extrapolation)",
         fill = "") +
    theme(plot.title = element_text(face = "bold", size = 12)),
  "11_training_support")

# ---- SI maps: survey linkage / coverage ----------------------------------------
# SI-1: which reference survey covers each NFHS-4 district
d_cov <- shp %>%
  mutate(in_access = district %in% access$district,
         in_ires   = district %in% ires$district,
         coverage  = case_when(in_access & in_ires ~ "ACCESS + IRES",
                               in_access ~ "ACCESS only",
                               in_ires   ~ "IRES only",
                               TRUE      ~ "Neither") %>%
           factor(levels = c("ACCESS + IRES", "ACCESS only", "IRES only", "Neither")))
maps$si1 <- save_map(
  ggplot(d_cov) +
    geom_sf(aes(fill = coverage), color = "grey70", linewidth = 0.05) +
    scale_fill_manual(values = c("ACCESS + IRES" = "#7B3294",
                                 "ACCESS only"   = "#C2A5CF",
                                 "IRES only"     = "#008837",
                                 "Neither"       = "grey90")) +
    theme_void(base_size = 11) +
    labs(title = NULL, fill = "") +
    theme(plot.title = element_text(face = "bold", size = 12)),
  "SI_1_reference_coverage")

# SI-2: NFHS-5 cluster assignment to NFHS-4 districts (requires 01 re-run to
# produce nfhs5_cluster_assignment.rds; skipped gracefully otherwise)
p_ca <- file.path(dir_out, "nfhs5_cluster_assignment.rds")
if (file.exists(p_ca)) {
  ca <- readRDS(p_ca)
  if (!"nearest_assigned" %in% names(ca)) ca$nearest_assigned <- FALSE
  # CRITICAL: project cluster points into the shapefile's CRS -- plotting raw
  # lon/lat over a projected basemap collapses the map extent (all-gray bug).
  ca_sf <- st_as_sf(ca, coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(st_crs(shp))
  maps$si2 <- save_map(
    ggplot() +
      geom_sf(data = shp, fill = "grey96", color = "grey75", linewidth = 0.05) +
      geom_sf(data = ca_sf %>% filter(assigned, !nearest_assigned),
              size = 0.35, alpha = 0.5, color = "#8FB4D6") +
      geom_sf(data = ca_sf %>% filter(nearest_assigned),
              size = 1.1, color = "#E08214") +
      geom_sf(data = ca_sf %>% filter(!assigned),
              size = 1.4, shape = 4, stroke = 0.6, color = "#B2182B") +
      theme_void(base_size = 11) +
      labs(title = NULL),
    "SI_2_nfhs5_cluster_assignment")
  # SI-2b: fallback clusters only, colored by snap distance -- lets you judge
  # whether they hug district borders/coastlines (expected under DHS GPS
  # displacement) or sit implausibly far from any polygon (data problem).
  if ("snap_km" %in% names(ca) && any(!is.na(ca$snap_km))) {
    fb    <- ca %>% filter(!is.na(snap_km))
    fb_sf <- ca_sf %>% filter(!is.na(snap_km))
    maps$si2b <- save_map(
      ggplot() +
        geom_sf(data = shp, fill = "grey96", color = "grey75", linewidth = 0.05) +
        geom_sf(data = fb_sf, aes(color = pmin(snap_km, 25),
                                  shape = assigned), size = 1.2) +
        scale_color_viridis_c(option = "inferno", direction = -1,
                              name = "km (capped 25)") +
        scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 4),
                           labels = c(`TRUE` = "snapped", `FALSE` = "excluded"),
                           name = "") +
        theme_void(base_size = 11) +
        labs(title = NULL),
      "SI_2b_fallback_snap_distance")
    si_hist <- ggplot(fb, aes(snap_km)) +
      geom_histogram(bins = 40, fill = "#E08214") +
      geom_vline(xintercept = 5, linetype = 2) +
      theme_bw() +
      labs(title = "Snap distance of nearest-district fallback clusters",
           subtitle = paste0(sum(fb$snap_km > 5, na.rm = TRUE),
                             " clusters exceed the 5 km DHS rural displacement ",
                             "radius -- inspect these in nfhs5_fallback_clusters.csv"),
           x = "distance to assigned district (km)", y = "clusters")
    ggsave(file.path(dir_maps, "SI_2c_snap_distance_hist.jpeg"), si_hist,
           width = 7, height = 5, dpi = 250)
    maps$si2c <- si_hist
  }

  # SI-2d: how much does each district's NFHS-5 sample rely on fallback
  # clusters? Districts with a high share are where the assignment choice
  # could move the estimate -- candidates for a sensitivity check.
  fb_share <- ca %>%
    filter(!is.na(district)) %>%
    group_by(district = as.character(district)) %>%
    summarise(share_fb = sum(n_hh[nearest_assigned]) / sum(n_hh),
              .groups = "drop")
  maps$si2d <- save_map(
    shp %>% left_join(fb_share, by = "district") %>%
      ggplot() +
      geom_sf(aes(fill = share_fb), color = "grey70", linewidth = 0.05) +
      scale_fill_viridis_c(option = "inferno", direction = -1,
                           limits = c(0, NA), na.value = "grey90") +
      theme_void(base_size = 11) +
      labs(title = NULL, fill = "share") +
      theme(plot.title = element_text(face = "bold", size = 11)),
    "SI_2d_fallback_share_by_district")
} else {
  message("nfhs5_cluster_assignment.rds not found -- re-run 01 to generate the ",
          "SI cluster-assignment map.")
}

# ---- Diagnostic plots --------------------------------------------------------------
pairA <- pairs$pairA; pairB <- pairs$pairB

diag_plots <- list(
  # calibration scatter: corrected vs reference in overlap districts
  ggplot(pairA, aes(access_w1_mainlpg, lpg_2015_rural)) +
    geom_abline(linetype = 2) + geom_point(alpha = .7, color = "#B2182B") +
    geom_point(data = pairA %>%
                 left_join(corrected %>% select(district, any_of("lpg_2015_bayes")),
                           by = "district"),
               aes(y = lpg_2015_bayes), alpha = .7, color = "#2166AC") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + theme_bw() +
    labs(title = "ACCESS W1 (x) vs NFHS-4 raw (red) and corrected (blue)",
         x = "ACCESS Wave 1", y = "NFHS-4 rural"),
  ggplot(pairB, aes(ires_mainlpg_rural, lpg_2019_rural)) +
    geom_abline(linetype = 2) + geom_point(alpha = .7, color = "#B2182B") +
    geom_point(data = pairB %>%
                 left_join(corrected %>% select(district, any_of("lpg_2019_bayes")),
                           by = "district"),
               aes(y = lpg_2019_bayes), alpha = .7, color = "#2166AC") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + theme_bw() +
    labs(title = "IRES rural (x) vs NFHS-5 raw (red) and corrected (blue)",
         x = "IRES rural", y = "NFHS-5 rural"),
  # distribution checks
  ggplot(proxy %>% filter(survey == "NFHS5"), aes(e_lpg_kg_yr)) +
    geom_histogram(bins = 40, fill = "#2166AC") + theme_bw() +
    labs(title = "Predicted district mean LPG kg/yr (NFHS-5) -- expect ~30-150",
         x = "kg/yr", y = "districts"),
  ggplot(proxy %>% filter(survey == "NFHS5") %>%
           pivot_longer(c(p_solid_only, p_stacking, p_excl_lpg, p_other_nonsolid)),
         aes(value, fill = name)) +
    geom_histogram(bins = 30, alpha = .7, position = "identity") + theme_bw() +
    labs(title = "Predicted 4-category shares across districts (should sum ~1)",
         x = "share", y = "districts", fill = "")
)

# ---- Funnel plots: is a district's NFHS-reference gap larger than sampling
# noise allows? Points outside the funnel disagree by more than reference-
# survey sampling error can explain (approximate limits, p(1-p)<=0.25).
funnel <- function(df, diffvar_x, diffvar_y, nvar, label) {
  d <- df %>%
    mutate(diff = .data[[diffvar_x]] - .data[[diffvar_y]],
           n = .data[[nvar]]) %>%
    filter(!is.na(diff), !is.na(n))
  md <- mean(d$diff)
  ns <- seq(max(10, min(d$n)), max(d$n), length.out = 200)
  lim <- tibble(n = ns, lo = md - 1.96 * sqrt(0.25 / ns),
                hi = md + 1.96 * sqrt(0.25 / ns))
  n_out <- with(d, sum(diff < md - 1.96 * sqrt(0.25 / n) |
                       diff > md + 1.96 * sqrt(0.25 / n)))
  ggplot(d, aes(n, diff)) +
    geom_hline(yintercept = md, linetype = 1, color = "grey40") +
    geom_line(data = lim, aes(n, lo), linetype = 2, inherit.aes = FALSE) +
    geom_line(data = lim, aes(n, hi), linetype = 2, inherit.aes = FALSE) +
    geom_point(alpha = 0.7, color = "#2166AC") +
    theme_bw() +
    labs(title = paste0("Funnel: ", label),
         subtitle = paste0(n_out, " of ", nrow(d),
                           " districts fall outside conservative 95% sampling limits ",
                           "-- disagreement beyond sampling noise"),
         x = "reference-survey households in district", y = "NFHS - reference")
}
diag_plots <- c(diag_plots, list(
  funnel(pairA, "lpg_2015_rural", "access_w1_mainlpg", "n_access_w1",
         "NFHS-4 rural vs ACCESS W1"),
  funnel(pairB, "lpg_2019_rural", "ires_mainlpg_rural", "n_ires_rural",
         "NFHS-5 rural vs IRES rural")
))

# ---- Per-state agreement table (which states drive disagreement?) --------------
state_agree <- pairB %>%
  filter(!is.na(lpg_2019_rural), !is.na(ires_mainlpg_rural)) %>%
  group_by(state_name) %>%
  summarise(districts = n(),
            r = ifelse(n() >= 3,
                       cor(lpg_2019_rural, ires_mainlpg_rural), NA),
            mean_diff = mean(lpg_2019_rural - ires_mainlpg_rural),
            .groups = "drop") %>%
  arrange(mean_diff)
write_csv(state_agree, file.path(dir_out, "state_agreement_nfhs5_ires.csv"))
diag_plots <- c(diag_plots, list(
  ggplot(state_agree,
         aes(mean_diff, reorder(state_name, mean_diff))) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    geom_point(aes(size = districts), color = "#B2182B") +
    theme_bw() +
    labs(title = "Mean NFHS-5 minus IRES difference by state (rural)",
         subtitle = "States far left drive the upward correction; check their IRES coding first",
         x = "mean difference (proportion)", y = NULL, size = "districts")
))

# ---- Combined PDF -------------------------------------------------------------------
pdf(file.path(dir_out, "maps_overview.pdf"), width = 9, height = 8)
for (m in maps) print(m)
for (g in diag_plots) print(g)
dev.off()

# ---- Plain-text diagnostics summary --------------------------------------------------
chk <- function(lbl, ok, detail = "") {
  sprintf("[%s] %s%s", ifelse(ok, "OK  ", "WARN"), lbl,
          ifelse(detail == "", "", paste0(" -- ", detail)))
}
comp <- tryCatch(read_csv(file.path(dir_out, "comparison_table.csv"),
                          show_col_types = FALSE), error = function(e) NULL)
mods <- tryCatch(readRDS(file.path(dir_out, "stacking_models.rds")),
                 error = function(e) NULL)

rng <- function(x) paste0(round(min(x, na.rm = TRUE), 3), " to ",
                          round(max(x, na.rm = TRUE), 3))
p5  <- proxy %>% filter(survey == "NFHS5")
sum3 <- with(p5, p_solid_only + p_stacking + p_excl_lpg + p_other_nonsolid)

lines <- c(
  "==================== DIAGNOSTICS SUMMARY ====================",
  paste0("Generated: ", format(Sys.time())), "",
  "--- District linkage (from 01-03; see *_linkage_diagnostics.csv) ---",
  { lk <- lapply(c("nfhs5", "access", "ires"), function(sv) {
      f <- file.path(dir_out, paste0(sv, "_linkage_diagnostics.csv"))
      if (file.exists(f)) {
        x <- read_csv(f, show_col_types = FALSE)
        paste0(toupper(sv), ": ", paste0(x$metric, " = ", x$value, collapse = "; "))
      } else paste0(toupper(sv), ": (diagnostics file not found -- re-run its prep script)")
    }); unlist(lk) }, "",
  "--- Coverage ---",
  chk("NFHS districts with 2015 estimate",
      sum(!is.na(corrected$lpg_2015_rural)) > 600,
      paste0(sum(!is.na(corrected$lpg_2015_rural)), " districts")),
  chk("NFHS districts with 2019 estimate",
      sum(!is.na(corrected$lpg_2019_rural)) > 600,
      paste0(sum(!is.na(corrected$lpg_2019_rural)), " districts")),
  chk("ACCESS overlap districts", nrow(pairA) >= 45, paste0(nrow(pairA))),
  chk("IRES overlap districts",   nrow(pairB) >= 130, paste0(nrow(pairB))), "",
  "--- Estimates in valid range ---",
  chk("All raw proportions in [0,1]",
      all(between(na.omit(c(corrected$lpg_2015_rural, corrected$lpg_2019_rural)), 0, 1))),
  chk("Corrected (Bayes) in [0,1]",
      all(between(na.omit(c(corrected[["lpg_2015_bayes"]],
                            corrected[["lpg_2019_bayes"]])), 0, 1))),
  chk("4-category shares sum to ~1",
      all(abs(na.omit(sum3) - 1) < 0.02),
      paste0("max abs dev = ", round(max(abs(sum3 - 1), na.rm = TRUE), 4))),
  chk("Predicted kg/yr plausible (5th-95th pctl in 10-200)",
      { q <- quantile(p5$e_lpg_kg_yr, c(.05, .95), na.rm = TRUE);
        q[1] > 10 && q[2] < 200 },
      paste0("district-mean range: ", rng(p5$e_lpg_kg_yr), " kg/yr")), "",
  "--- Agreement (from 04) ---",
  if (!is.null(comp)) capture.output(as.data.frame(comp)) else "(comparison_table.csv not found)", "",
  "--- Stacking model validation (from 06) ---",
  if (!is.null(mods)) c(
    sprintf("ACCESS leave-district-out: AUC %.3f | Brier %.3f | district-r %.3f",
            mods$validation$access$auc, mods$validation$access$brier,
            mods$validation$access$district_cor),
    sprintf("IRES rural leave-district-out: AUC %.3f | Brier %.3f | district-r %.3f",
            mods$validation$ires$auc, mods$validation$ires$brier,
            mods$validation$ires$district_cor),
    sprintf("Cross-survey (IRES model -> ACCESS districts): r %.3f",
            cor(mods$validation$cross_survey$obs, mods$validation$cross_survey$hat,
                use = "complete.obs"))
  ) else "(stacking_models.rds not found)", "",
  "--- Extrapolation ---",
  # in_support is a district-level 0/1 flag (every household in a district
  # shares a state), so an unweighted mean over p5 is the share of DISTRICTS,
  # not of households -- the old label said "households" and was wrong. Report
  # both, each named for what it actually measures.
  chk("Share of NFHS-5 DISTRICTS in IRES-state support",
      mean(p5$in_support, na.rm = TRUE) > 0.9,
      paste0(round(100 * mean(p5$in_support, na.rm = TRUE), 1), "%")),
  chk("Share of NFHS-5 sampled households in IRES-state support",
      { w <- proxy5$n_hh; f <- proxy5$in_support
        ok <- is.finite(w) & is.finite(f)
        sum(w[ok] * f[ok]) / sum(w[ok]) > 0.9 },
      { w <- proxy5$n_hh; f <- proxy5$in_support
        ok <- is.finite(w) & is.finite(f)
        paste0(round(100 * sum(w[ok] * f[ok]) / sum(w[ok]), 1), "%") }),
  "",
  "Interpretation notes:",
  "* Divergence maps (05-07): red = NFHS higher than reference/raw, blue = lower.",
  "* WARN on kg/yr usually means refill columns are per-month or mis-mapped.",
  "* WARN on 3-category sums points to multinomial prediction misalignment.",
  "=============================================================="
)
writeLines(unlist(lines), file.path(dir_out, "diagnostics_summary.txt"))
cat(paste(unlist(lines), collapse = "\n"), "\n")

## ---- CHECKS ------------------------------------------------------------------
# NOTE: this script defines its own local chk() for the diagnostics_summary text,
# so here we use only the shared helpers that do not collide (chk_header/chk_file/
# chk_warn). The 4-category sum-to-1 check is already in diagnostics_summary above.
chk_header("07_diagnostics_maps")
chk_file("07", "maps overview PDF written", "maps_overview.pdf")
chk_file("07", "diagnostics summary written", "diagnostics_summary.txt")
chk_warn("07", "4-category proxy shares sum to ~1 (NFHS-5)",
    all(abs(na.omit(sum3) - 1) < 0.02),
    sprintf("max |sum-1| = %.4f", max(abs(sum3 - 1), na.rm = TRUE)))

message("07_diagnostics_maps.R done. See maps_overview.pdf, maps/, ",
        "and diagnostics_summary.txt in ", dir_out)
