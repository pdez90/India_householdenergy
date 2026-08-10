# ==============================================================================
# H4_si_adult_health.R   (STANDALONE -- does not source 00_config.R)
#
# SUPPLEMENTARY analysis: adult cardio-metabolic outcomes vs the CORRECTED
# clean-cooking exposure, replicating deSouza et al. 2025 (Environ. Res. Health)
# but with the measurement-error-corrected LPG exposure.
#
#   (a) District prevalence of HYPERTENSION and DIABETES for NFHS-4 and NFHS-5,
#       from null four-level multilevel models (cluster/district/state), on the
#       dist_code geography used everywhere else.
#   (b) Prevalence maps (NFHS-5) for the SI.
#   (c) District change-on-change: change(prevalence, 2019-2015) ~ change(LPG)
#       with LPG measured raw / RC / Bayesian-corrected, adjusted for SES and
#       ambient covariates, population-agnostic, state-clustered SEs.
#
# Definitions (deSouza 2025, JNC7):
#   HYPERTENSION = mean(2nd,3rd systolic) >= 140 OR mean(2nd,3rd diastolic) >= 90
#                  OR currently on antihypertensive medication.
#   DIABETES     = self-reported "ever told had high blood sugar / diabetes"
#                  (secondary, self-report -- see paper section S2).
#
# Standalone inputs (files on disk only):
#   cvd_load.RData                  <- person-level NFHS with derived
#                                       hypertension*/diabetes + LATNUM/LONGNUM
#                                       (the same source behind deSouza 2025)
#   district_nfhs_shapefile/*.shp   <- dist_code geography
#   corrected_nfhs_districts.rds    <- from 05_correction.R (exposure)
# Optional (used for adjustment if present):
#   health_district_wide.rds        <- from H1 (SES change covariates)
#   df_wide_health.rds              <- from H3 (PM2.5 / temp / RH / drought / region)
#
# Output: si_adult_health_prevalence.rds/.csv, si_adult_health_effects.csv,
#         maps/SI_adult_prevalence.jpeg, si_adult_health_coefplot.jpeg
# ==============================================================================

## ---- CONFIG (edit paths / column names to match your machine) ---------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "H4_si_adult_health"


# Population: "rural" (MAIN; matches rural corrected exposure) or "all" (SI).
# Set by run_health.R; defaults to rural for a standalone run.
POP <- if (exists("POP")) POP else "rural"
sfx <- if (POP == "all") "_all" else ""
RURAL_ONLY <- (POP == "rural")
message("H4 population: ", POP)

path_cvd_load      <- "/Users/priyanka/Downloads/DHS_India/cvd_load.RData"
path_districts_shp <- "/Users/priyanka/Downloads/DHS_India/district_nfhs_shapefile/nfhs_data.shp"
path_corr          <- file.path(dir_out, "corrected_nfhs_districts.rds")
path_health        <- file.path(dir_out, paste0("health_district_wide", sfx, ".rds"))  # optional (SES)
path_dfwide        <- file.path(dir_out, "df_wide_health.rds")        # optional

# Names of the two person-level data frames inside cvd_load.RData.
OBJ_2015 <- "all_2015"
OBJ_2019 <- "all_2019"

# Derived indicators already present in cvd_load (confirmed via inspect_cvd.R).
# deSouza 2025 hypertension = systolic>=140 OR diastolic>=90 OR on medication,
# using the MEAN OF THE 2ND & 3RD readings (=> the "_23" family), INCLUDING the
# self-report medication component (=> NOT the "_nosr" family). "hypertension1"
# is the JNC7 stage-1 (>=140/90) threshold the paper uses.
#   Sensitivity variants you can switch to:
#     hypertension1_23_nosr  (drop self-report meds)
#     hypertension2_23       (stage-2, >=160/100)
COL_HYPERTENSION <- "hypertension1_23"
# Diabetes (secondary, self-report-inclusive). "diabetes" is the derived flag;
# "diabetes_report" is the pure self-report ("ever told high blood sugar").
COL_DIABETES     <- "diabetes"
# Sensitivity: pure self-reported diabetes (reported as a separate outcome).
COL_DIABETES_SR  <- "diabetes_report"

# --- OPTIONAL raw-construction fallback (used only if the COL_* above are "") --
COL_SYS_2ND <- ""; COL_SYS_3RD <- ""      # e.g. "systolic2" / "systolic3"
COL_DIA_2ND <- ""; COL_DIA_3RD <- ""      # e.g. "diastolic2" / "diastolic3"
COL_ANTIHTN <- ""                          # antihypertensive-med flag (1=yes)
COL_DIAB_SELFREP <- ""                     # self-report high blood sugar (1=yes)

## ---- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse); library(haven); library(sf); library(lme4)
  library(lmtest); library(sandwich)
})
options(stringsAsFactors = FALSE)
dir.create(file.path(dir_out, "maps"), showWarnings = FALSE, recursive = TRUE)

## ---- Helper: null 4-level district prevalence (cluster/district/state) --------
district_estimates_glmer <- function(data, outcome,
                                     state = "state", district = "district",
                                     cluster = "clust") {
  d <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[state]]),
           !is.na(.data[[district]]), !is.na(.data[[cluster]])) %>%
    mutate(across(all_of(c(state, district, cluster)), as.factor))
  if (nrow(d) == 0 || length(unique(d[[outcome]])) < 2) {
    warning("Outcome '", outcome, "' unusable (empty/constant); NA returned.")
    return(tibble(district = character(), p_hat = numeric(), n = integer()))
  }
  fml <- as.formula(paste0(outcome, " ~ (1|", state, ") + (1|", district,
                           ") + (1|", cluster, ")"))
  m <- glmer(fml, data = d, family = binomial, nAGQ = 0,
             control = glmerControl(optimizer = "nloptwrap"))
  if (exists("chk_record_fit"))
    chk_record_fit(.chk_tag("H4_si_adult_health"),
                   paste0("district_estimates_glmer:", outcome), m,
                   extra = sprintf("districts=%d", nlevels(d[[district]])))
  re_d <- ranef(m)[[district]] %>% rownames_to_column(district) %>% rename(v = `(Intercept)`)
  re_s <- ranef(m)[[state]]    %>% rownames_to_column(state)    %>% rename(f = `(Intercept)`)
  d %>% count(.data[[state]], .data[[district]], name = "n") %>%
    rename(state = 1, district = 2) %>%
    left_join(re_d, by = setNames(district, "district")) %>%
    left_join(re_s, by = setNames(state, "state")) %>%
    mutate(p_hat = plogis(fixef(m)[["(Intercept)"]] + coalesce(v, 0) + coalesce(f, 0))) %>%
    select(district, p_hat, n)
}

## ---- Helper: attach dist_code geography to a person-level frame --------------
shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.numeric(dist_code)) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

# all_2015/all_2019 already carry LATNUM/LONGNUM per record, so we overlay those
# points onto the pipeline's dist_code polygons directly (no GPS join needed),
# guaranteeing the district keys match corrected_nfhs_districts.rds.
# SNAP CAP. DHS displaces cluster coordinates (up to 2 km urban / 5 km rural, and
# 10 km for 1% of rural clusters), so a point can legitimately fall just outside
# its true district polygon. The nearest-district fallback repairs that, but ONLY
# up to a 10 km cap -- the same rule 01_prep_nfhs.R and H1_prep_mortality.R use.
# A point whose nearest district is further away is left unassigned (and dropped
# downstream) rather than forced into a district it plausibly does not belong to.
# Previously H4 snapped with NO cap, which was inconsistent with H1 and could
# attach records to arbitrarily distant districts.
SNAP_CAP_M <- 10000
snap_audit <- list()   # filled by attach_geo(); summarized in the CHECKS block

attach_geo <- function(df, tag = "") {
  df <- df %>%
    filter(!is.na(LATNUM), !is.na(LONGNUM), LATNUM != 0, LONGNUM != 0)
  df$.rid <- seq_len(nrow(df))
  pts <- st_as_sf(df, coords = c("LONGNUM", "LATNUM"),
                  crs = "+proj=longlat +datum=WGS84", remove = FALSE) %>%
    st_transform(st_crs(shp))
  j <- st_join(pts, shp %>% select(dist_code, state_name),
               join = st_within, left = TRUE) %>%
    st_drop_geometry() %>% arrange(.rid, dist_code) %>% distinct(.rid, .keep_all = TRUE)
  n_within_poly <- sum(!is.na(j$dist_code))
  miss <- j$.rid[is.na(j$dist_code)]
  aud <- list(tag = tag, n_points = nrow(j), n_inside_polygon = n_within_poly,
              n_outside = length(miss), n_snapped = 0L, n_dropped = 0L,
              median_km = NA_real_, max_km = NA_real_, p95_km = NA_real_,
              n_districts_receiving = 0L)
  if (length(miss)) {
    miss_pts <- pts %>% filter(.rid %in% miss)
    ni     <- st_nearest_feature(miss_pts, shp)
    dist_m <- as.numeric(st_distance(miss_pts, shp[ni, ], by_element = TRUE))
    within <- dist_m <= SNAP_CAP_M
    nn <- tibble(.rid          = miss_pts$.rid,
                 dist_code_nn  = ifelse(within, shp$dist_code[ni],  NA),
                 state_name_nn = ifelse(within, shp$state_name[ni], NA)) %>%
      distinct(.rid, .keep_all = TRUE)
    j <- j %>% left_join(nn, by = ".rid") %>%
      mutate(dist_code = coalesce(dist_code, dist_code_nn),
             state_name = coalesce(state_name, state_name_nn)) %>%
      select(-dist_code_nn, -state_name_nn)
    acc_km <- dist_m[within] / 1000
    aud$n_snapped  <- sum(within)
    aud$n_dropped  <- sum(!within)
    aud$median_km  <- if (length(acc_km)) median(acc_km) else NA_real_
    aud$p95_km     <- if (length(acc_km)) as.numeric(quantile(acc_km, 0.95)) else NA_real_
    aud$max_km     <- if (length(acc_km)) max(acc_km)    else NA_real_
    aud$n_districts_receiving <- length(unique(na.omit(shp$dist_code[ni][within])))
    message(sprintf(
      "  [%s] %d points outside every polygon: %d snapped within %.0f km (median %.2f km, p95 %.2f km, max %.2f km, %d districts affected); %d dropped (beyond cap).",
      tag, length(miss), aud$n_snapped, SNAP_CAP_M / 1000,
      aud$median_km, aud$p95_km, aud$max_km, aud$n_districts_receiving, aud$n_dropped))
    if (aud$n_dropped > 0)
      message(sprintf("  [%s] dropped-point distances: median %.1f km, max %.1f km.",
                      tag, median(dist_m[!within]) / 1000, max(dist_m[!within]) / 1000))
  } else {
    message(sprintf("  [%s] every point fell inside a district polygon; no snapping needed.", tag))
  }
  snap_audit[[length(snap_audit) + 1L]] <<- aud
  j %>% mutate(district = as.character(as.numeric(dist_code)), state = state_name) %>%
    filter(!is.na(dist_code)) %>%          # cap-dropped points leave the frame here
    select(-.rid)
}

## ---- Load person-level NFHS + derive outcomes --------------------------------
message("Loading cvd_load.RData ...")
e <- new.env(); load(path_cvd_load, envir = e)
pick_obj <- function(nm) {
  if (exists(nm, envir = e)) return(get(nm, envir = e))
  stop("Object '", nm, "' not found in cvd_load.RData. Available: ",
       paste(ls(e), collapse = ", "))
}
p15 <- as_tibble(pick_obj(OBJ_2015)); p19 <- as_tibble(pick_obj(OBJ_2019))
message("  ", OBJ_2015, ": ", nrow(p15), " rows | ", OBJ_2019, ": ", nrow(p19), " rows")

mean_reading <- function(df, c2, c3) {
  if (c2 == "" || c3 == "" || !all(c(c2, c3) %in% names(df))) return(rep(NA_real_, nrow(df)))
  rowMeans(cbind(as.numeric(df[[c2]]), as.numeric(df[[c3]])), na.rm = TRUE)
}
derive_outcomes <- function(df) {
  df <- zap_labels(df)
  # --- hypertension ---
  if (COL_HYPERTENSION != "" && COL_HYPERTENSION %in% names(df)) {
    df$hypertension <- as.integer(df[[COL_HYPERTENSION]] == 1)
  } else {
    sys <- mean_reading(df, COL_SYS_2ND, COL_SYS_3RD)
    dia <- mean_reading(df, COL_DIA_2ND, COL_DIA_3RD)
    med <- if (COL_ANTIHTN != "" && COL_ANTIHTN %in% names(df))
      as.integer(df[[COL_ANTIHTN]] == 1) else 0L
    df$hypertension <- as.integer((sys >= 140) | (dia >= 90) | (med == 1))
    df$hypertension[is.na(sys) & is.na(dia) & (is.na(med) | med == 0)] <- NA_integer_
  }
  # --- diabetes ---
  if (COL_DIABETES != "" && COL_DIABETES %in% names(df)) {
    df$diabetes <- as.integer(df[[COL_DIABETES]] == 1)
  } else if (COL_DIAB_SELFREP != "" && COL_DIAB_SELFREP %in% names(df)) {
    df$diabetes <- as.integer(df[[COL_DIAB_SELFREP]] == 1)
  } else df$diabetes <- NA_integer_
  # --- diabetes, pure self-report (sensitivity) ---
  df$diabetes_sr <- if (COL_DIABETES_SR != "" && COL_DIABETES_SR %in% names(df))
    as.integer(df[[COL_DIABETES_SR]] == 1) else NA_integer_
  df$rural <- if ("v025" %in% names(df)) as.integer(df$v025 == 2)
              else if ("hv025" %in% names(df)) as.integer(df$hv025 == 2) else NA_integer_
  df
}

p15 <- derive_outcomes(p15); p19 <- derive_outcomes(p19)
for (nm in c("hypertension","diabetes","diabetes_sr")) {
  message(sprintf("  %s -- 2015 nonmissing=%d mean=%.3f | 2019 nonmissing=%d mean=%.3f",
                  nm, sum(!is.na(p15[[nm]])), mean(p15[[nm]], na.rm = TRUE),
                  sum(!is.na(p19[[nm]])), mean(p19[[nm]], na.rm = TRUE)))
}

# keep only rows contributing to at least one outcome, then overlay onto polygons
keep_rows <- function(df) df %>%
  filter(!is.na(hypertension) | !is.na(diabetes) | !is.na(diabetes_sr))
cl <- function(df) if ("v001" %in% names(df)) df$v001 else df$clust
p15 <- keep_rows(p15); p19 <- keep_rows(p19)
p15$.cl <- cl(p15); p19$.cl <- cl(p19)
message("Overlaying person points onto dist_code polygons ...")
p15 <- attach_geo(p15, "NFHS-4 (2015)"); p19 <- attach_geo(p19, "NFHS-5 (2019)")
p15$clust <- paste0(p15$.cl, "_15")
p19$clust <- paste0(p19$.cl, "_19")
if (RURAL_ONLY) { p15 <- filter(p15, rural == 1); p19 <- filter(p19, rural == 1) }

## ---- District prevalence (both years, both outcomes) -------------------------
ADULT <- c("hypertension", "diabetes", "diabetes_sr")
est_year <- function(df, yr) {
  usable <- ADULT[vapply(ADULT, function(v)
    v %in% names(df) && length(unique(na.omit(df[[v]]))) >= 2, logical(1))]
  message("Year ", yr, " -- estimating: ", paste(usable, collapse = ", "))
  reduce(usable, function(acc, v) {
    e <- district_estimates_glmer(df, v) %>% select(district, !!paste0(v, "_", yr) := p_hat)
    if (nrow(acc) == 0) e else full_join(acc, e, by = "district")
  }, .init = tibble())
}
prev <- full_join(est_year(p15, 2015), est_year(p19, 2019), by = "district")
for (v in ADULT) {
  a <- paste0(v, "_2015"); b <- paste0(v, "_2019")
  if (all(c(a, b) %in% names(prev))) prev[[paste0("change_", v)]] <- 100 * (prev[[b]] - prev[[a]])
}
saveRDS(prev, file.path(dir_out, paste0("si_adult_health_prevalence", sfx, ".rds")))
write_csv(prev, file.path(dir_out, paste0("si_adult_health_prevalence", sfx, ".csv")))

## ---- Join corrected exposure + SES/ambient covariates ------------------------
corr <- readRDS(path_corr) %>%
  transmute(district = as.character(as.numeric(district)),
            lpg15_raw = lpg_2015_rural, lpg19_raw = lpg_2019_rural,
            lpg15_rc  = lpg_2015_rc,    lpg19_rc  = lpg_2019_rc,
            lpg15_b   = lpg_2015_bayes, lpg19_b   = lpg_2019_bayes)
df <- prev %>% left_join(corr, by = "district") %>%
  mutate(change_lpg_raw   = 100 * (lpg19_raw - lpg15_raw),
         change_lpg_rc    = 100 * (lpg19_rc  - lpg15_rc),
         change_lpg_bayes = 100 * (lpg19_b   - lpg15_b))

if (file.exists(path_health)) {
  h <- readRDS(path_health) %>% mutate(district = as.character(district))
  ses <- intersect(c("change_poor","change_mother_low_edu","change_electricity",
                     "change_muslim","change_improved_sanitation",
                     "change_improved_water"), names(h))
  df <- left_join(df, h %>% select(district, state, all_of(ses)) %>%
                    distinct(district, .keep_all = TRUE), by = "district")
}
if (file.exists(path_dfwide)) {
  dfw <- readRDS(path_dfwide) %>% mutate(district = as.character(district))
  # weighted_temperature_change is carried in for the alternative-temperature
  # sensitivity only; it is deliberately kept OUT of env_adj below.
  envc <- intersect(c("change_pm","temp_change","rh_change",
                      "weighted_temperature_change",
                      "droughtchange","region"), names(dfw))
  df <- left_join(df, dfw %>% select(district, all_of(envc)) %>%
                    distinct(district, .keep_all = TRUE), by = "district")
}

## ---- Change-on-change models -------------------------------------------------
dhs_adj <- intersect(c("change_poor","change_mother_low_edu","change_electricity",
                       "change_muslim","change_improved_sanitation",
                       "change_improved_water"), names(df))
# NOTE (adjustment-set harmonization). The primary ambient set adjusts for mean
# temperature change ONLY. The 0.7*tmax + 0.3*tmin composite (formerly and
# misleadingly called heat_index_change, now weighted_temperature_change) is a
# near-perfect linear function of the same two fields -- including both inflates
# the variance of each and makes neither interpretable. This set is now identical
# to H2's and H5's, so the three analyses are directly comparable.
env_adj <- intersect(c("change_pm","temp_change","rh_change",
                       "droughtchange"), names(df))
adj <- c(dhs_adj, env_adj)
stopifnot(!("weighted_temperature_change" %in% adj))
has_region <- "region" %in% names(df); has_state <- "state" %in% names(df)
message("Adjustment covariates: ", paste(adj, collapse = ", "),
        if (has_region) " + region FE" else "")

fit_one <- function(dat, outcome, exposure, adjusted = TRUE) {
  if (!outcome %in% names(dat)) return(NULL)
  rhs <- c(exposure, if (adjusted) adj, if (has_region) "factor(region)")
  m <- lm(reformulate(rhs, response = outcome), data = dat)
  rows <- as.numeric(rownames(model.frame(m)))
  vc <- if (has_state) sandwich::vcovCL(m, cluster = dat[["state"]][rows]) else
    sandwich::vcovHC(m, "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc); i <- match(exposure, rownames(ct))
  tibble(term = exposure, estimate = ct[i,1], se = ct[i,2], p = ct[i,4],
         conf.low = ct[i,1] - 1.96*ct[i,2], conf.high = ct[i,1] + 1.96*ct[i,2],
         n = nobs(m))
}
OUTCOMES <- c(hypertension = "change_hypertension", diabetes = "change_diabetes",
              `diabetes (self-report)` = "change_diabetes_sr")
grid <- expand_grid(outcome = names(OUTCOMES),
                    exposure = c("change_lpg_raw","change_lpg_rc","change_lpg_bayes"),
                    adjusted = c(FALSE, TRUE))
results <- pmap_dfr(grid, function(outcome, exposure, adjusted) {
  r <- fit_one(df, OUTCOMES[[outcome]], exposure, adjusted)
  if (is.null(r)) return(NULL)
  mutate(r, outcome = outcome, adjusted = adjusted, .before = 1)
}) %>% mutate(est_per10 = estimate*10, lo_per10 = conf.low*10, hi_per10 = conf.high*10,
              across(where(is.numeric), ~round(.x, 4)))
write_csv(results, file.path(dir_out, paste0("si_adult_health_effects", sfx, ".csv")))
cat("\n== Adult-health associations (change in prevalence per 10-pp rise in LPG) ==\n")
print(as.data.frame(results %>% filter(adjusted) %>%
        select(outcome, term, est_per10, lo_per10, hi_per10, p, n)), digits = 3)

## ---- Coefficient plot --------------------------------------------------------
plot_df <- results %>% filter(adjusted) %>%
  mutate(exposure = recode(term, change_lpg_raw = "Raw NFHS",
                           change_lpg_rc = "Regression-calibrated",
                           change_lpg_bayes = "Bayesian-corrected"),
         exposure = factor(exposure, levels = c("Raw NFHS","Regression-calibrated",
                                                "Bayesian-corrected")))
p <- ggplot(plot_df, aes(est_per10, exposure)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_pointrange(aes(xmin = lo_per10, xmax = hi_per10)) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Change in adult prevalence (pp) per 10-pp rise in LPG main-fuel prevalence",
       y = NULL, title = NULL, subtitle = NULL)
ggsave(file.path(dir_out, paste0("si_adult_health_coefplot", sfx, ".jpeg")), p, width = 8, height = 8.5, dpi = 300)

## ---- Prevalence maps (NFHS-5) ------------------------------------------------
suppressWarnings({
  d_map <- shp %>% mutate(district = as.character(as.numeric(dist_code))) %>%
    left_join(prev, by = "district")
  mk_map <- function(col, name, opt = "viridis") {
    if (!col %in% names(d_map)) return(invisible(NULL))
    p <- ggplot(d_map) + geom_sf(aes(fill = .data[[col]]), color = NA) +
      scale_fill_viridis_c(name = name, option = opt, na.value = "grey90") +
      theme_void(base_size = 9) + theme(legend.position = "right")
    ggsave(file.path(dir_out, "maps", paste0("SI_", col, sfx, ".jpeg")), p,
           width = 5.2, height = 4.6, dpi = 300)
  }
  mk_map("hypertension_2019", "Hypertension\n(2019)")
  mk_map("diabetes_2019",     "Diabetes\n(2019)", "magma")
})

## ---- CHECKS ------------------------------------------------------------------
chk_header(paste0("H4_si_adult_health", sfx))
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("H4", "H4_si_adult_health")

chk("H4", "adult effects table produced", exists("results") && nrow(results) > 0)
chk_file("H4", "adult prevalence table written",
    paste0("si_adult_health_prevalence", sfx, ".rds"))
chk_file("H4", "adult effects table written",
    paste0("si_adult_health_effects", sfx, ".csv"))

# --- spatial-assignment audit (10 km snap cap, matching H1 / 01_prep_nfhs) -----
snap_tbl <- dplyr::bind_rows(lapply(snap_audit, as_tibble))
write_csv(snap_tbl, file.path(dir_out, paste0("H4_snap_diagnostics", sfx, ".csv")))
cat("\n== H4 point-to-district assignment (10 km cap) ==\n")
print(as.data.frame(snap_tbl), row.names = FALSE, digits = 3)

chk("H4", "no point assigned to a district further than the 10 km cap",
    all(!is.finite(snap_tbl$max_km) | snap_tbl$max_km <= 10.0001),
    sprintf("max accepted snap distance = %.2f km", suppressWarnings(max(snap_tbl$max_km, na.rm = TRUE))))
chk("H4", "every retained record carries a district code",
    all(!is.na(p15$district)) && all(!is.na(p19$district)))
# Two problems with the previous version of this check, both fixed here.
#
# (1) The label stated a FACT ("fewer than 2% ... required snapping") rather than
#     a CRITERION, so when it WARNed -- as it did on 2026-07-30, at 2.20% and
#     2.05% -- the printed line asserted something false. The name now says what
#     is being tested and the detail carries the observed percentages, so a WARN
#     reads as "2.20% exceeded the 2.5% threshold? no -- here is the number".
# (2) The threshold itself was too tight for the mechanism. DHS displaces cluster
#     coordinates by up to 5 km (rural) / 2 km (urban) before release, so a small
#     share of points landing just outside their district polygon is EXPECTED,
#     not anomalous; 2% was an arbitrary round number below the displacement rate
#     the design implies. 2.5% still flags a genuine boundary-vintage mismatch
#     (which would run to double digits) while not flagging normal displacement.
#
# The check name carries `sfx` because H4 runs twice, once for the rural sample
# and once for all-population, and two rows with an identical name in
# pipeline_checks.csv cannot be told apart after the fact.
.snap_share <- snap_tbl$n_snapped / pmax(snap_tbl$n_points, 1)
chk_warn("H4", paste0("snapped share of person-records below 2.5%", sfx),
    all(.snap_share < 0.025),
    paste(sprintf("%s: %d/%d snapped = %.2f%%, %d dropped beyond cap",
                  snap_tbl$tag, snap_tbl$n_snapped, snap_tbl$n_points,
                  100 * .snap_share, snap_tbl$n_dropped),
          collapse = " | "))
chk("H4", "adjustment set excludes the weighted-temperature composite (collinear with temp_change)",
    !("weighted_temperature_change" %in% adj), paste(adj, collapse = ", "))
chk("H4", "adjustment set matches H2/H5 ambient covariates",
    setequal(env_adj, intersect(c("change_pm","temp_change","rh_change","droughtchange"), names(df))),
    paste(env_adj, collapse = ", "))

message("\nH4 done -> si_adult_health_prevalence.(rds/csv), si_adult_health_effects.csv, ",
        "si_adult_health_coefplot.jpeg, maps/SI_hypertension_2019.jpeg, ",
        "maps/SI_diabetes_2019.jpeg")
