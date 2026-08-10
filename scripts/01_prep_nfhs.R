# ==============================================================================
# 01_prep_nfhs.R
# Prepare NFHS-4 (2015-16) and NFHS-5 (2019-21) household data and produce
# district-level LPG-as-main-fuel estimates:
#   (a) all clusters,  (b) rural clusters only  (for comparison with ACCESS/IRES)
#   (i) unweighted glmer (main spec)
#   (ii) design-weighted direct estimates (sensitivity)
#   (iii) weighted mixed model via WeMix (sensitivity)
#
# Also builds the household-level prediction frame with covariates harmonized
# to ACCESS/IRES (used by 06_stacking_prediction.R).
#
# Inputs : cvd_load.RData, image2015/2019.RData, gps, district shapefile
# Outputs: nfhs_districts.rds, nfhs_hh_covariates.rds  (in dir_out)
# ==============================================================================

source("00_config.R")
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "01_prep_nfhs"


# ---- Load (identical to the original exploratory analysis) --------------------
load(path_cvd_load)      # all_2015, all_2019, india2015_hr, india2019_hr, ...
load(path_image2019); men_2019_hr <- men_hr; women_2019_hr <- women_hr
rm(india2019_ir, india2019_mr, men, women, men_hr, women_hr, gps)
load(path_image2015); men_2015_hr <- men_hr; women_2015_hr <- women_hr
rm(india2015_ir, india2015_mr, gps, men_hr, women_hr, men, women)

load(path_gps_2019)
india2019_hr <- merge(india2019_hr, gps, by.x = "hv001", by.y = "cluster", all.x = TRUE)
rm(gps)
india2019_hr$district <- india2019_hr$DHSREGCO
india2019_hr$state    <- india2019_hr$hv024
india2019_hr$clust    <- paste0(india2019_hr$hv001, india2019_hr$hv000, india2019_hr$survey)

load(path_gps_2015)
india2015_hr <- merge(india2015_hr, gps, by.x = "hv001", by.y = "cluster", all.x = TRUE)
rm(gps)
india2015_hr$district <- india2015_hr$DHSREGCO
india2015_hr$state    <- india2015_hr$hv024
india2015_hr$clust    <- paste0(india2015_hr$hv001, india2015_hr$hv000, india2015_hr$survey)

districts_2015 <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.numeric(dist_code)) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()
districts_2015 <- subset(districts_2015, select = -c(dist_cod_1))

# ---- Harmonized variables (NATIONAL, both surveys) ----------------------------
prep_nfhs <- function(hr, survey_year) {
  hr %>%
    mutate(
      lpg        = as.integer(haven::zap_labels(fuel) == 2),
      # 'Clean' per the DHS fuel categorisation: electricity(1), LPG(2), natural gas(4),
      # biogas(5)... keep both definitions:
      clean_fuel = as.integer(haven::zap_labels(fuel) %in% c(1, 2, 4, 5, 95)),
      rural      = as.integer(hv025 == 2),           # hv025: 1 urban, 2 rural
      wt         = hv005 / 1e6,                      # household weight
      psu        = hv021,                            # PSU (cluster)
      strat      = hv022,                            # sample stratum
      hh_size    = hv012,
      # Missing/unknown religion -> NA, not a real "Other" category (mirrors the
      # IRES fix; else missingness is silently absorbed into Other).
      hh_relig   = case_when(is.na(hh_religion)  ~ NA_character_,
                             hh_religion == 1    ~ "Hindu",
                             hh_religion == 2    ~ "Muslim",
                             TRUE ~ "Other"),
      # caste: sh34/sh36-style vars differ across rounds; both rounds carry
      # a harmonized caste var in the cvd_load extract if present:
      bank_account = ifelse(hv247 == 8, NA, hv247),
      # explicit 0/1 so nonstandard codes (e.g. 8/9 "don't know") become NA, not 0
      electricity  = dplyr::case_when(hv206 == 1 ~ 1L, hv206 == 0 ~ 0L,
                                      TRUE ~ NA_integer_),
      bpl          = if (survey_year == 2015) ifelse(sh58 == 8, NA, sh58)
                     else ifelse(sh75 == 8, NA, sh75),
      survey_year  = survey_year
    )
}

india2015_hr <- prep_nfhs(india2015_hr, 2015)
india2019_hr <- prep_nfhs(india2019_hr, 2019)

# Caste (household head) -- NFHS variable sh with SC/ST/OBC codes.
# NOTE: confirm the caste column name in the cvd_load extract
# (commonly sh36/sh49 in HR recodes: 1 SC, 2 ST, 3 OBC, 4 none/general).
add_caste <- function(hr, caste_col) {
  if (caste_col %in% names(hr)) {
    hr$caste <- factor(case_when(
      hr[[caste_col]] == 1 ~ "Scheduled Caste",
      hr[[caste_col]] == 2 ~ "Scheduled Tribe",
      hr[[caste_col]] == 3 ~ "Other Backward Class",
      hr[[caste_col]] == 4 ~ "General",
      TRUE ~ NA_character_))
  } else {
    warning("Caste column '", caste_col, "' not found; caste set NA.")
    hr$caste <- NA_character_
  }
  hr
}
india2015_hr <- add_caste(india2015_hr, "sh36")  # <-- check name for NFHS-4
india2019_hr <- add_caste(india2019_hr, "sh49")  # <-- check name for NFHS-5

# EDUCATION IS NOT DERIVED HERE, AND THAT IS DELIBERATE (removed 2026-08-01).
#
# The intended variable was "household head has primary schooling or less", to
# match ACCESS (edu_primary_or_less) and IRES (edu_low). The only NFHS column
# that could carry that contrast is the DHS attainment scale `eduatt`, and it is
# not present in the household extracts used here -- the run of 2026-08-01
# reported nonmissing = 0, distinct = 0 in both NFHS-4 and NFHS-5, i.e. 100%
# missing on 2.87M and 2.83M households respectively.
#
# A previous version fell back to `hh_college`, a binary college flag, for which
# `<= 2` is TRUE at both values; edu_low then came out CONSTANT rather than
# missing, which passes every structural check while being useless to every
# analysis. "Did not attend college" is also simply not the contrast ACCESS and
# IRES measure, so the substitution would have been wrong even had it varied.
#
# Because education cannot be harmonized across the three surveys, it is not a
# covariate, not a falsification benchmark, and not mapped. Carrying an all-NA
# edu_low forward only produced downstream checks that could never pass, so the
# derivation and every consumer of it have been removed. The manuscript still
# discloses the gap in the Table 1 comparability row ("Education of respondent /
# head -- not populated in extract"); that row is the honest record of this
# decision and should not be dropped while this comment stands.
#
# TO RESTORE: obtain an NFHS extract that carries `eduatt` (or another
# attainment scale), derive edu_low = as.integer(eduatt <= 2) here, then re-add
# it to the select() lists below, to the benchmark variable vectors in
# 08_si_benchmarks.R and 13_benchmark_maps.R, to the item list in
# 17_missingness.R, and to the candidate covariates in 06_stacking_prediction.R
# and 15_variable_importance.R.

# Wealth: prefer the continuous factor score (hv271) over the quintile (hv270);
# 06 converts this to within-state quintiles to align with ACCESS/IRES
# within-state expenditure quintiles.
add_wealth <- function(hr) {
  src <- intersect(c("hv271", "hv270", "wealthquin"), names(hr))[1]
  if (!is.na(src)) {
    hr$wealth_score <- suppressWarnings(as.numeric(hr[[src]]))
    message("wealth_score derived from '", src, "'.")
  } else {
    warning("No wealth column (hv271/hv270) found; wealth_score set NA.")
    hr$wealth_score <- NA_real_
  }
  hr
}
india2015_hr <- add_wealth(india2015_hr)
india2019_hr <- add_wealth(india2019_hr)

# ---- Attach 2015 district geography to NFHS-5 points (national) --------------
# Same overlay approach as the original analysis, but applied nationally so the corrected
# estimates in 05 can be produced for all districts on one consistent geography.
n_hh_2019_total   <- nrow(india2019_hr)
n_hh_2019_zerogps <- sum(india2019_hr$LATNUM == 0 | india2019_hr$LONGNUM == 0 |
                           is.na(india2019_hr$LATNUM), na.rm = TRUE)

india2019_pts <- india2019_hr %>%
  filter(LATNUM != 0 & LONGNUM != 0) %>%
  st_as_sf(coords = c("LONGNUM", "LATNUM"),
           crs = "+proj=longlat +datum=WGS84", remove = FALSE) %>%
  st_transform(st_crs(districts_2015))

india2019_pts$.rid <- seq_len(nrow(india2019_pts))
idx <- st_join(india2019_pts,
               districts_2015 %>%
                 select(Id, dist_code, state_name, dist_name, state_dist,
                        st_cd, censuscode, Districts),
               join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  # points inside two overlapping polygons would duplicate -> keep one per row
  arrange(.rid, dist_code) %>%
  distinct(.rid, .keep_all = TRUE)

# Nearest-district fallback for displaced clusters. DHS displaces cluster GPS
# by up to 2 km (urban) / 5 km (rural; 10 km for 1% of rural clusters), so a
# point just outside every polygon within ~10 km is explainable displacement
# and is snapped to the nearest district. Points
# farther than SNAP_MAX_KM cannot be displacement artifacts (bad coordinates,
# or territories absent from the NFHS-4 shapefile such as the island UTs) and
# are EXCLUDED, with their identities written to nfhs5_fallback_clusters.csv.
SNAP_MAX_KM <- 10
miss_rids <- idx$.rid[is.na(idx$dist_code)]
idx$nearest_assigned <- FALSE
if (length(miss_rids)) {
  miss_clusters <- india2019_pts %>%
    filter(.rid %in% miss_rids) %>%
    group_by(hv001) %>% slice(1) %>% ungroup()
  ni <- st_nearest_feature(miss_clusters, districts_2015)
  dd <- as.numeric(st_distance(miss_clusters, districts_2015[ni, ],
                               by_element = TRUE)) / 1000   # km
  nn <- tibble(hv001 = miss_clusters$hv001,
               dist_code_nn  = districts_2015$dist_code[ni],
               state_name_nn = districts_2015$state_name[ni],
               dist_name_nn  = districts_2015$dist_name[ni],
               snap_km = dd,
               snap_ok = dd <= SNAP_MAX_KM)
  idx <- idx %>%
    left_join(nn, by = "hv001") %>%
    mutate(nearest_assigned = is.na(dist_code) & !is.na(dist_code_nn) & snap_ok,
           dist_code  = if_else(nearest_assigned, dist_code_nn,  dist_code),
           state_name = if_else(nearest_assigned, state_name_nn, state_name),
           dist_name  = if_else(nearest_assigned, dist_name_nn,  dist_name)) %>%
    select(-dist_code_nn, -state_name_nn, -dist_name_nn, -snap_ok)
  message(sum(nn$snap_ok), " displaced clusters snapped to nearest district ",
          sprintf("(<= %d km; median %.1f km); ", SNAP_MAX_KM,
                  median(nn$snap_km[nn$snap_ok])),
          sum(!nn$snap_ok), " clusters beyond ", SNAP_MAX_KM,
          sprintf(" km EXCLUDED (max %.1f km) -- see nfhs5_fallback_clusters.csv.",
                  max(nn$snap_km)))
}
india2019_hr <- idx
india2019_hr$district <- india2019_hr$dist_code       # 2015 district geography

# ---- NFHS-5 -> NFHS-4 cluster-assignment diagnostics (for Methods/SI) ----------
if (!"snap_km" %in% names(india2019_hr)) india2019_hr$snap_km <- NA_real_
cluster_assign_2019 <- india2019_hr %>%
  group_by(hv001) %>%
  summarise(lon = dplyr::first(LONGNUM), lat = dplyr::first(LATNUM),
            district = dplyr::first(district),
            dist_name = dplyr::first(dist_name),
            state_name = dplyr::first(state_name),
            nearest_assigned = any(nearest_assigned),
            snap_km = dplyr::first(snap_km),
            n_hh = dplyr::n(), .groups = "drop") %>%
  mutate(assigned = !is.na(district))
saveRDS(cluster_assign_2019, file.path(dir_out, "nfhs5_cluster_assignment.rds"))

# Inspectable list of every fallback candidate (snapped AND excluded),
# sorted by snap distance:
cluster_assign_2019 %>%
  filter(!is.na(snap_km)) %>%
  arrange(desc(snap_km)) %>%
  readr::write_csv(file.path(dir_out, "nfhs5_fallback_clusters.csv"))

linkage_nfhs5 <- tibble(
  metric = c("NFHS-5 households, total",
             "households with unrecorded GPS (lat = lon = 0 in DHS release; excluded)",
             "clusters with GPS, total",
             "clusters assigned by point-in-polygon",
             paste0("clusters snapped to nearest district (<= ", SNAP_MAX_KM, " km)"),
             paste0("clusters excluded (nearest district > ", SNAP_MAX_KM, " km)"),
             "households retained via nearest-district snap",
             "households excluded (cluster > snap threshold)",
             "share of GPS households assigned"),
  value = c(n_hh_2019_total, n_hh_2019_zerogps,
            nrow(cluster_assign_2019),
            sum(cluster_assign_2019$assigned & !cluster_assign_2019$nearest_assigned),
            sum(cluster_assign_2019$nearest_assigned),
            sum(!cluster_assign_2019$assigned),
            sum(india2019_hr$nearest_assigned, na.rm = TRUE),
            sum(is.na(india2019_hr$district)),
            round(mean(!is.na(india2019_hr$district)), 4)))
readr::write_csv(linkage_nfhs5, file.path(dir_out, "nfhs5_linkage_diagnostics.csv"))
message("NFHS-5 -> NFHS-4 assignment: ",
        sum(cluster_assign_2019$nearest_assigned), " of ",
        nrow(cluster_assign_2019),
        " clusters via nearest-district fallback; ",
        sum(!cluster_assign_2019$assigned), " unassigned.")

# NFHS-4 districts already ARE the 2015 geography:
india2015_hr <- india2015_hr %>%
  left_join(districts_2015 %>% st_drop_geometry() %>%
              select(dist_code, state_name, dist_name),
            by = c("DHSREGCO" = "dist_code")) %>%
  mutate(district = DHSREGCO)

# ---- District estimates: function over subsets --------------------------------
make_estimates <- function(hr, label) {
  hr <- hr %>% mutate(state = factor(state_name), district = factor(district),
                      clust = factor(clust))

  est_all <- district_estimates_glmer(hr, "lpg") %>%
    rename(!!paste0("lpg_", label) := p_hat, !!paste0("n_", label) := n_hh)

  hr_rur <- hr %>% filter(rural == 1)
  est_rur <- district_estimates_glmer(hr_rur, "lpg") %>%
    rename(!!paste0("lpg_", label, "_rural") := p_hat,
           !!paste0("n_", label, "_rural") := n_hh) %>%
    select(-state)

  # Sensitivity: design-weighted direct district means (rural domain).
  # Design is built on ALL households, then restricted to rural inside the
  # design object -- proper domain estimation, avoids lonely-PSU strata.
  est_rur_wt <- district_estimates_weighted(
    hr, "lpg", ids = "psu", strata = "strat", weights = "wt",
    domain_col = "rural") %>%
    rename(!!paste0("lpg_", label, "_rural_wt") := p_wt,
           !!paste0("lpg_", label, "_rural_wt_se") := p_wt_se)

  est_all %>%
    left_join(est_rur,    by = "district") %>%
    left_join(est_rur_wt %>% mutate(district = factor(district)), by = "district")
}

nfhs4_districts <- make_estimates(india2015_hr, "2015")
nfhs5_districts <- make_estimates(india2019_hr, "2019")

# Optional third sensitivity: weighted multilevel model (can be slow nationally;
# run on the 6 ACCESS states if you want the ACCESS-comparison version)
run_wemix <- FALSE
if (run_wemix) {
  wemix_2015 <- india2015_hr %>%
    filter(rural == 1, state %in% ACCESS_STATES_2015 | state_name %in% ACCESS_STATE_NAMES) %>%
    district_estimates_wemix("lpg")
  nfhs4_districts <- left_join(nfhs4_districts,
                               wemix_2015 %>% select(-state), by = "district")
}

nfhs_districts <- full_join(
  nfhs4_districts %>% mutate(district = as.character(district)),
  nfhs5_districts %>% mutate(district = as.character(district)) %>% select(-state),
  by = "district")

saveRDS(nfhs_districts, file.path(dir_out, "nfhs_districts.rds"))

# ---- Household-level covariate frame for prediction (06) ----------------------
# Keep only predictors that ALSO exist in ACCESS/IRES (the crosswalk):
#   caste, religion, hh size, BPL card, bank account, electricity,
#   rural, state, district, weights. Education is NOT among them -- see the
#   comment at the head of this script.
for (v in c("month_interview", "yr_interview")) {
  if (!v %in% names(india2015_hr)) india2015_hr[[v]] <- NA_real_
  if (!v %in% names(india2019_hr)) india2019_hr[[v]] <- NA_real_
}
nfhs_hh <- bind_rows(
  india2015_hr %>% st_drop_geometry_safe() %>%
    transmute(survey = "NFHS4", survey_year, state = state_name,
              district = as.character(district), clust, wt, psu, strat,
              rural, lpg, clean_fuel, hh_size, hh_relig, caste,
              wealth_score, bpl, bank_account, electricity,
              month_interview = as.numeric(month_interview),
              yr_interview = as.numeric(yr_interview)),
  india2019_hr %>% st_drop_geometry_safe() %>%
    transmute(survey = "NFHS5", survey_year, state = state_name,
              district = as.character(district), clust, wt, psu, strat,
              rural, lpg, clean_fuel, hh_size, hh_relig, caste,
              wealth_score, bpl, bank_account, electricity,
              month_interview = as.numeric(month_interview),
              yr_interview = as.numeric(yr_interview))
)
saveRDS(nfhs_hh, file.path(dir_out, "nfhs_hh_covariates.rds"))

## ---- CHECKS ------------------------------------------------------------------
chk_header("01_prep_nfhs")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("01", "01_prep_nfhs")

chk("01", "district table non-empty", nrow(nfhs_districts) > 500,
    paste0(nrow(nfhs_districts), " districts"))
chk("01", "rural + design-weighted LPG columns present",
    chk_has_cols(nfhs_districts, c("lpg_2015_rural","lpg_2019_rural",
      "lpg_2015_rural_wt","lpg_2015_rural_wt_se",
      "lpg_2019_rural_wt","lpg_2019_rural_wt_se")))
chk("01", "2015/2019 rural prevalences in [0,1]",
    chk_in_range(nfhs_districts$lpg_2015_rural, 0, 1) &&
    chk_in_range(nfhs_districts$lpg_2019_rural, 0, 1))
chk("01", ">600 districts have a 2019 rural estimate",
    sum(is.finite(nfhs_districts$lpg_2019_rural)) > 600,
    paste0(sum(is.finite(nfhs_districts$lpg_2019_rural)), " districts"))
chk("01", "household frame carries lpg + covariates",
    chk_has_cols(nfhs_hh, c("lpg","hh_relig","caste","electricity")))
# Education was removed on 2026-08-01 because the NFHS extracts do not carry an
# attainment scale (100% missing in both rounds). Assert its ABSENCE, so that a
# future extract carrying a usable column fails here and forces a deliberate
# re-introduction rather than silently reviving a half-wired benchmark.
chk("01", "no education column is carried forward (see header note)",
    !any(grepl("^edu", names(nfhs_hh))),
    if (any(grepl("^edu", names(nfhs_hh))))
      paste("unexpected:", paste(grep("^edu", names(nfhs_hh), value = TRUE), collapse = ", "))
    else "none present, as intended")
chk_warn("01", "religion Other share plausible (<15%; missing is NA)",
    { s <- mean(nfhs_hh$hh_relig == "Other", na.rm = TRUE); is.finite(s) && s < 0.15 },
    paste0("Other = ", round(100 * mean(nfhs_hh$hh_relig == "Other", na.rm = TRUE), 1),
           "%, NA = ", chk_pct_na(nfhs_hh$hh_relig), "%"))
chk_warn("01", "month_interview populated (needed by 09)",
    mean(is.finite(nfhs_hh$month_interview)) > 0.5,
    paste0(chk_pct_na(nfhs_hh$month_interview), "% NA"))

message("01_prep_nfhs.R done: ",
        nrow(nfhs_districts), " districts; ",
        nrow(nfhs_hh), " households saved.")
