# ==============================================================================
# 02_prep_access.R
# Prepare ACCESS 2015 (Wave 1) & 2018 (Wave 2) and produce:
#   - household-level fuel-stacking outcomes:
#       stack_binary : main fuel LPG AND still uses any solid fuel
#       use3cat      : LPG, no solid fuel reported / LPG+solid stacking / solid-fuel-only
#       lpg_kg_yr    : annual LPG kg from cylinder refills (14.2 kg large, 5 kg small)
#   - district-level estimates of main-fuel-LPG (glmer, + weighted sensitivity)
#   - district-level estimates of stacking outcomes (for validation in 06)
#
# Inputs : CEEWACCESS20152018_Appended.dta, weights.dta, district shapefile
# Outputs: access_hh.rds, access_districts.rds
# ==============================================================================

source("00_config.R")
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "02_prep_access"


# ---- Load --------------------------------------------------------------------
pool <- read.dta13(path_access_appended, convert.factors = TRUE,
                   generate.factors = TRUE, nonint.factors = TRUE)
weights <- read.dta13(path_access_weights)   # year, m1_q11_village_code, weights

# The weights table must be unique at the join key, or the left_join silently
# duplicates household rows.
stopifnot(!anyDuplicated(weights[c("year", "m1_q11_village_code")]))
n_before <- nrow(pool)
pool <- pool %>%
  mutate(year = as.numeric(year)) %>%
  left_join(weights, by = c("year", "m1_q11_village_code"))
stopifnot(nrow(pool) == n_before)   # join must not add rows

# ---- Rename / derive ------------------------------------------------------------
# NOTE on LPG cylinder counts: the Gould et al. (2020) kg/month measure sums
# self-reported small (5 kg) and large (14.2 kg) cylinder purchases per year.
# The candidates in the local file are printed when this script runs -- pick the
# right ones from the message below and set the two names accordingly:
# Cylinder purchases per year, split by source (distributor vs market):
#   large: m4_q103_5_lpg_lcyl_dist + m4_q103_6_lpg_lcyl_mkt
#   small: m4_q103_8_lpg_scyl_dist + m4_q103_9_lpg_scyl_mkt
# (m4_q103_4_lpg_lcyl / m4_q103_7_lpg_scyl are the screener items;
#  *_price columns are prices, m4_q103_15_lpg_distance is travel distance.)
cyl_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
na_sum2 <- function(a, b) {
  # sum treating NA as 0, but NA when BOTH are missing
  out <- rowSums(cbind(a, b), na.rm = TRUE)
  out[is.na(a) & is.na(b)] <- NA
  out
}

pool <- pool %>%
  arrange(finalhhid, year) %>%
  dplyr::rename(
    age        = m1_q19_age,
    gender     = m1_q20_gender,
    state      = m1_q8_state_code,
    district   = m1_q9_district_code,
    village    = m1_q11_village_code,
    hh         = finalhhid,
    main_fuel  = m5_q118_main_cookfuel,
    month_exp  = m1_q32_month_expenditure,
    education  = m1_q23_edu
  ) %>%
  mutate(
    wave          = as.integer(year == 2018),
    main_fuel_lpg = as.integer(main_fuel == "LPG"),
    uselpg        = as.integer(m4_q103_lpg == "Yes"),
    usefirewood   = as.integer(m4_q109_firewood == "Yes"),
    usedung       = as.integer(m4_q113_dungcake == "Yes"),
    useagro       = as.integer(m4_q114_agro == "Yes"),
    any_solid     = row_any1(usefirewood, usedung, useagro),
    # read.dta13(nonint.factors=TRUE) turns these into labelled factors, so
    # match on EITHER the numeric code or the label text:
    caste_txt = tolower(trimws(as.character(m1_q25_caste))),
    caste = case_when(
      caste_txt == "1" | grepl("scheduled caste|\\bsc\\b", caste_txt) ~ "Scheduled Caste",
      caste_txt == "2" | grepl("scheduled tribe|\\bst\\b", caste_txt) ~ "Scheduled Tribe",
      caste_txt == "3" | grepl("backward|\\bobc\\b",       caste_txt) ~ "Other Backward Class",
      caste_txt == "4" | grepl("general|none",             caste_txt) ~ "General",
      TRUE ~ NA_character_),
    relig_txt = tolower(trimws(as.character(m1_q24_religion))),
    religion = case_when(
      relig_txt == "1" | grepl("hindu",  relig_txt) ~ "Hindu",
      relig_txt == "2" | grepl("muslim|islam", relig_txt) ~ "Muslim",
      is.na(m1_q24_religion) ~ NA_character_,
      TRUE ~ "Other"),
    hhsize      = m1_q27_no_adults + m1_q29_no_children,
    bplaay      = as.integer(m1_q26_ration %in% c("BPL", "Antyodaya")),
    bankaccount = as.integer(m1_q34_bank_acc == "Yes"),
    electricity = as.integer(m2_q55_grid == "Yes"),
    pmuy        = as.integer(m4_3n1_ujjwala_beneficiary == 1),
    # label-robust: match on text patterns OR numeric codes 1/2
    edu_primary_or_less = as.integer(
      grepl("no formal|up to 5th", tolower(as.character(education))) |
        trimws(as.character(education)) %in% c("1", "2"))
  )

# Sanity check the recodes -- both should show several non-empty categories:
message("ACCESS caste distribution:")
print(table(pool$caste, useNA = "ifany"))
message("ACCESS religion distribution:")
print(table(pool$religion, useNA = "ifany"))

# ---- Fuel stacking outcomes ----------------------------------------------------
pool <- pool %>%
  mutate(
    # (1) Binary stacking: LPG is the MAIN fuel but the household still burns
    #     solid fuels (the 'partial transition' most relevant to HAP)
    stack_binary = case_when(
      main_fuel_lpg == 1 & any_solid == 1 ~ 1L,
      main_fuel_lpg == 1 & any_solid == 0 ~ 0L,
      TRUE ~ NA_integer_),
    # (2) Fuel-use category (all households), REPORTING-BASED LABELS.
    #     The category names deliberately describe what the household REPORTED,
    #     not an inferred behavioural state. In particular there is no
    #     "exclusive LPG" category: the survey instruments do not establish that a
    #     household meets 100% of cooking needs with LPG (the IRES "all cooking
    #     needs" item was found unreliable in the metadata review, and ACCESS has
    #     no equivalent item at all). "LPG, no solid fuel reported" is exactly what
    #     the data support -- LPG use with no solid fuel named among the fuels used.
    #     Likewise a household with no LPG and no solid fuel (primary electricity,
    #     PNG, kerosene, ...) is "Neither LPG nor solid fuel reported" rather than
    #     being miscounted as a solid-fuel burner in p_any_solid downstream.
    #     Habib et al.'s own terminology is used ONLY in the external benchmark
    #     column of 08_si_benchmarks.R, never for our own derived categories.
    use3cat = case_when(
      uselpg == 1 & any_solid == 0 ~ "LPG, no solid fuel reported",
      uselpg == 1 & any_solid == 1 ~ "LPG and solid fuel reported",
      uselpg == 0 & any_solid == 1 ~ "Solid fuel reported, no LPG",
      uselpg == 0 & any_solid == 0 ~ "Neither LPG nor solid fuel reported",
      TRUE ~ NA_character_) %>%
      factor(levels = c("Solid fuel reported, no LPG", "LPG and solid fuel reported",
                        "LPG, no solid fuel reported", "Neither LPG nor solid fuel reported"))
  )
  stopifnot(all(pool$any_solid %in% c(0L, 1L, NA_integer_)))
  stopifnot(!any(is.infinite(pool$any_solid), na.rm = TRUE))

# (3) LPG kg per year from cylinder purchases (Gould et al. 2020: small +
#     large cylinders, from both distributors and the market, per year)
# Per-source cylinder counts (distributor vs market), kept separate so annual
# SPEND can price each source with its OWN price. A household that buys from both
# sources at different prices must not have every cylinder priced at whichever
# price coalesce() happened to pick first (the previous bug).
n_large_dist <- cyl_num(pool$m4_q103_5_lpg_lcyl_dist)
n_large_mkt  <- cyl_num(pool$m4_q103_6_lpg_lcyl_mkt)
n_small_dist <- cyl_num(pool$m4_q103_8_lpg_scyl_dist)
n_small_mkt  <- cyl_num(pool$m4_q103_9_lpg_scyl_mkt)
# Totals for the kg calc (kg per cylinder is the same regardless of source):
n_large <- na_sum2(n_large_dist, n_large_mkt)
n_small <- na_sum2(n_small_dist, n_small_mkt)
pool$lpg_kg_yr <- dplyr::case_when(
  pool$uselpg == 0 ~ 0,
  pool$uselpg == 1 ~ dplyr::coalesce(n_large, 0) * LPG_KG_LARGE +
                     dplyr::coalesce(n_small, 0) * LPG_KG_SMALL,
  TRUE ~ NA_real_
)
# LPG users with no refill info at all -> NA, not 0:
pool$lpg_kg_yr[pool$uselpg == 1 & is.na(n_large) & is.na(n_small)] <- NA
message("lpg_kg_yr among LPG users -- summary:")
print(summary(pool$lpg_kg_yr[pool$uselpg == 1]))

# (4) LPG affordability (adapted from the ACCESS 2015 report's <6%-of-monthly-
#     expenditure rule, Jain et al. 2015 -- ours covers LPG spending only, not
#     all cooking fuels, so it is an LPG-affordability variant):
#     annual LPG spend / annual household expenditure < 6%, among LPG users.
fill_med <- function(x, g) {
  med_g <- ave(x, g, FUN = function(v) median(v, na.rm = TRUE))
  out <- ifelse(is.na(x), med_g, x)
  ifelse(is.na(out), median(x, na.rm = TRUE), out)
}
# Price each source separately, imputing each source's missing prices by district
# median. Spend is then summed source-by-source, so a household's distributor and
# market cylinders are each valued at their own price rather than a single one.
price_l_dist <- fill_med(cyl_num(pool$m4_q103_10_lpg_lcyl_dist_price), pool$district)
price_l_mkt  <- fill_med(cyl_num(pool$m4_q103_12_lpg_lcyl_mkt_price),  pool$district)
price_s_dist <- fill_med(cyl_num(pool$m4_q103_11_lpg_scyl_dist_price), pool$district)
price_s_mkt  <- fill_med(cyl_num(pool$m4_q103_13_lpg_scyl_mkt_price),  pool$district)
mexp <- suppressWarnings(as.numeric(as.character(pool$month_exp)))
pool$lpg_spend_yr <-
  dplyr::coalesce(n_large_dist, 0) * price_l_dist +
  dplyr::coalesce(n_large_mkt,  0) * price_l_mkt  +
  dplyr::coalesce(n_small_dist, 0) * price_s_dist +
  dplyr::coalesce(n_small_mkt,  0) * price_s_mkt
# MISSING-vs-ZERO, matching the lpg_kg_yr rule above. An LPG user who reported no
# cylinder counts at all has UNKNOWN spend, not zero spend; coalesce(., 0) alone
# would silently record such a household as spending nothing. A non-user genuinely
# spends zero. Getting this wrong is what makes a "median LPG spend share" collapse
# toward 0 -- see the diagnostic block below.
.no_cyl_info <- is.na(n_large_dist) & is.na(n_large_mkt) &
                is.na(n_small_dist) & is.na(n_small_mkt)
pool$lpg_spend_yr[pool$uselpg == 1 & .no_cyl_info] <- NA_real_
pool$lpg_spend_yr[pool$uselpg == 0] <- 0
pool$lpg_afford6 <- ifelse(pool$uselpg == 1 & !is.na(mexp) & mexp > 0 &
                             !is.na(pool$lpg_spend_yr) & pool$lpg_spend_yr > 0,
                           as.integer(pool$lpg_spend_yr / (12 * mexp) < 0.06),
                           NA_integer_)

# ---- LPG expenditure-share diagnostics ---------------------------------------
# The previous version of this message took the median over EVERY pooled
# household, including the ~2/3 who do not use LPG and therefore have a spend
# share of exactly zero -- so the reported "median" was 0% by construction and
# said nothing about what LPG costs the households that actually buy it. The
# denominators are now stated explicitly at every step.
.share <- pool$lpg_spend_yr / (12 * mexp) * 100          # % of annual expenditure
.users        <- which(pool$uselpg == 1)
.users_valid  <- which(pool$uselpg == 1 & is.finite(.share) & mexp > 0)
.users_spend  <- which(pool$uselpg == 1 & is.finite(.share) & mexp > 0 &
                       pool$lpg_spend_yr > 0)
message("\n-- ACCESS LPG expenditure share (annual LPG spend / annual household expenditure) --")
message(sprintf("  pooled households                      : %d", nrow(pool)))
message(sprintf("  LPG users (uselpg == 1)                : %d (%.1f%% of pool)",
                length(.users), 100 * length(.users) / nrow(pool)))
message(sprintf("  ... with usable expenditure + spend     : %d (%.1f%% of users)",
                length(.users_valid), 100 * length(.users_valid) / max(length(.users), 1)))
message(sprintf("  ... reporting NONZERO cylinder purchases: %d (%.1f%% of users) <- the analytic base",
                length(.users_spend), 100 * length(.users_spend) / max(length(.users), 1)))
message(sprintf("  LPG users with NO cylinder info at all  : %d (spend set to NA, not 0)",
                sum(pool$uselpg == 1 & .no_cyl_info, na.rm = TRUE)))
if (length(.users_spend)) {
  .q <- quantile(.share[.users_spend], c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)
  message(sprintf(
    "  share among PURCHASING users (%%): p10 %.1f | p25 %.1f | MEDIAN %.1f | p75 %.1f | p90 %.1f | mean %.1f",
    .q[1], .q[2], .q[3], .q[4], .q[5], mean(.share[.users_spend], na.rm = TRUE)))
  message(sprintf("  median among ALL LPG users (zeros kept) : %.1f%%   <- lower, by construction",
                  median(.share[.users_valid], na.rm = TRUE)))
  message(sprintf("  median among ALL households            : %.1f%%   <- NOT a meaningful quantity",
                  median(.share[is.finite(.share)], na.rm = TRUE)))
  message(sprintf("  annual LPG spend among purchasing users : median Rs %.0f (IQR %.0f-%.0f)",
                  median(pool$lpg_spend_yr[.users_spend]),
                  quantile(pool$lpg_spend_yr[.users_spend], 0.25),
                  quantile(pool$lpg_spend_yr[.users_spend], 0.75)))
  message(sprintf("  affordable (<6%% of expenditure)         : %.1f%% of purchasing users",
                  100 * mean(pool$lpg_afford6 == 1, na.rm = TRUE)))
} else {
  message("  WARNING: no LPG user has both a positive spend and a positive expenditure; ",
          "the affordability variable is entirely missing.")
}
# Annual consumption, on the same explicit denominators (the manuscript quotes a
# kg/year figure; this is where that number comes from).
.kg_base <- which(pool$uselpg == 1 & is.finite(pool$lpg_kg_yr) & pool$lpg_kg_yr > 0)
if (length(.kg_base))
  message(sprintf(
    "  LPG kg/yr among users with a nonzero refill record (n = %d): median %.0f | mean %.0f | IQR %.0f-%.0f",
    length(.kg_base), median(pool$lpg_kg_yr[.kg_base]), mean(pool$lpg_kg_yr[.kg_base]),
    quantile(pool$lpg_kg_yr[.kg_base], 0.25), quantile(pool$lpg_kg_yr[.kg_base], 0.75)))

# ---- Attach district geography (dist_cod_1 crosswalk) ---------------------------
districts_shp <- st_read(path_districts_shp, quiet = TRUE)
districts_shp$Districts <- str_to_upper(districts_shp$dist_name)

pool <- merge(pool, districts_shp, by.x = "district", by.y = "dist_cod_1",
              all.x = TRUE)
# 'dist_code' in the shapefile is the NFHS-4 (DHSREGCO) district id -> use it
# as the common district key across all surveys:
pool$district_nfhs <- pool$dist_code

# ---- ACCESS -> NFHS-4 district linkage diagnostics (for Methods/SI) ------------
# ---- Analytic-frame counts for the Methods survey description (2.2.2) --------
# WHY these go to disk instead of being typed into the manuscript: the survey
# description used to carry a Wave 1 count of 8,568 households, transcribed from
# the published ACCESS 2015 report. It survived review because it was checked
# against that report rather than against the file analysed here. Three
# different Wave 1 counts exist in the source materials -- 8,568 in the 2015
# report, 8,566 records in the standalone 2015 microdata release, and the count
# derived below in the appended two-wave panel release that this script reads.
# The manuscript must describe the file it analyses, so these counts are derived
# here and interpolated by the manuscript builder; nothing is re-typed.
n_w1 <- sum(pool$wave == 0)
n_w2 <- sum(pool$wave == 1)
d_w1 <- n_distinct(pool$district[pool$wave == 0])
d_w2 <- n_distinct(pool$district[pool$wave == 1])

# Panel overlap on the survey's own household identifier (village code / serial).
# CAUTION: this is the overlap OF THIS FILE, not the fielded-sample retention
# rate the published reports quote. The appended release re-lists every Wave 1
# household in Wave 2, so the overlap is 1.00 by construction; it is written out
# so that any retention claim in the manuscript can be checked against it rather
# than taken on trust.
id1 <- unique(pool$hh[pool$wave == 0])
id2 <- unique(pool$hh[pool$wave == 1])
panel_overlap <- if (length(id1)) mean(id1 %in% id2) else NA_real_

linkage_access <- tibble(
  metric = c("ACCESS households (both waves)",
             "ACCESS Wave 1 households",
             "ACCESS Wave 2 households",
             "ACCESS Wave 1 districts",
             "ACCESS Wave 2 districts",
             "Wave 1 households re-listed in Wave 2 (share, this file)",
             "households matched to an NFHS-4 district (by census code)",
             "households unmatched (excluded from district estimates)",
             "ACCESS districts, total",
             "ACCESS districts matched",
             "share of households matched"),
  value = c(nrow(pool),
            n_w1,
            n_w2,
            d_w1,
            d_w2,
            round(panel_overlap, 4),
            sum(!is.na(pool$district_nfhs)),
            sum(is.na(pool$district_nfhs)),
            n_distinct(pool$district),
            n_distinct(pool$district_nfhs[!is.na(pool$district_nfhs)]),
            round(mean(!is.na(pool$district_nfhs)), 4)))
readr::write_csv(linkage_access, file.path(dir_out, "access_linkage_diagnostics.csv"))
message(sprintf(
  "ACCESS analytic frame: Wave 1 = %d households (%d districts), Wave 2 = %d (%d), panel = %d household-wave observations.",
  n_w1, d_w1, n_w2, d_w2, nrow(pool)))
message(sprintf(
  "  Wave 1 count provenance: published ACCESS 2015 report states 8,568 and the standalone 2015 microdata release holds 8,566; this appended panel release holds %d. The manuscript reports the file analysed.",
  n_w1))
message(sprintf(
  "  Wave 1 households re-listed in Wave 2 in this file: %.1f%% (file overlap, NOT the fielded-sample retention rate).",
  100 * panel_overlap))
message("ACCESS -> NFHS-4 linkage: ",
        sum(is.na(pool$district_nfhs)), " of ", nrow(pool),
        " households unmatched; ",
        n_distinct(pool$district_nfhs[!is.na(pool$district_nfhs)]), " of ",
        n_distinct(pool$district), " districts matched (all by census code).")

access_hh <- pool %>%
  st_drop_geometry_safe() %>%
  transmute(survey = ifelse(wave == 0, "ACCESS_W1", "ACCESS_W2"),
            year, wave, state, state_name, district_access = district,
            district = as.character(district_nfhs), village, hh,
            weights, main_fuel_lpg, uselpg, any_solid, usefirewood, usedung,
            useagro, stack_binary, use3cat, lpg_kg_yr, lpg_spend_yr,
            lpg_afford6, pmuy,
            caste, religion, hhsize, bplaay, bankaccount, electricity,
            education, edu_primary_or_less, month_exp, age, gender)
saveRDS(access_hh, file.path(dir_out, "access_hh.rds"))

# ---- District-level estimates ---------------------------------------------------
wave1 <- access_hh %>% filter(wave == 0) %>%
  mutate(state = factor(state), district = factor(district),
         village = factor(village))
wave2 <- access_hh %>% filter(wave == 1) %>%
  mutate(state = factor(state), district = factor(district),
         village = factor(village))

# Main spec: glmer with village as the cluster level (matches the other surveys)
w1_lpg <- district_estimates_glmer(wave1, "main_fuel_lpg", cluster = "village") %>%
  rename(access_w1_mainlpg = p_hat, n_access_w1 = n_hh)
w2_lpg <- district_estimates_glmer(wave2, "main_fuel_lpg", cluster = "village") %>%
  rename(access_w2_mainlpg = p_hat, n_access_w2 = n_hh) %>% select(-state)

# Stacking outcomes at district level (Wave 1 -- for NFHS-4-era validation; and
# Wave 2 for descriptives)
w1_stack <- district_estimates_glmer(
  wave1 %>% filter(!is.na(stack_binary)), "stack_binary", cluster = "village") %>%
  transmute(district, access_w1_stack = p_hat)
w1_excl <- wave1 %>%
  mutate(excl_lpg = as.integer(use3cat == "LPG, no solid fuel reported")) %>%
  district_estimates_glmer("excl_lpg", cluster = "village") %>%
  transmute(district, access_w1_excl_lpg = p_hat)
w1_kg <- wave1 %>%
  filter(!is.na(weights)) %>%
  as_survey_design(ids = village, weights = weights) %>%
  group_by(district) %>%
  summarise(access_w1_lpg_kg_yr = survey_mean(lpg_kg_yr, na.rm = TRUE))

# Sensitivity: design-weighted direct estimates (village weights from Zhang/
# Urpelainen replication archive, joined above)
w1_lpg_wt <- district_estimates_weighted(
  wave1, "main_fuel_lpg", ids = "village", weights = "weights") %>%
  rename(access_w1_mainlpg_wt = p_wt, access_w1_mainlpg_wt_se = p_wt_se)

# LPG affordability (<6% of expenditure) among LPG users, by district
w1_afford <- wave1 %>%
  filter(!is.na(lpg_afford6)) %>%
  district_estimates_glmer("lpg_afford6", cluster = "village") %>%
  transmute(district, access_w1_lpg_afford6 = p_hat)

access_districts <- w1_lpg %>%
  left_join(w2_lpg,   by = "district") %>%
  left_join(w1_stack, by = "district") %>%
  left_join(w1_afford, by = "district") %>%
  left_join(w1_excl,  by = "district") %>%
  left_join(w1_kg %>% mutate(district = factor(district)), by = "district") %>%
  left_join(w1_lpg_wt %>% mutate(district = factor(district)), by = "district") %>%
  mutate(district = as.character(district))

saveRDS(access_districts, file.path(dir_out, "access_districts.rds"))
## ---- CHECKS ------------------------------------------------------------------
chk_header("02_prep_access")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("02", "02_prep_access")

chk("02", "ACCESS district table non-empty", nrow(access_districts) >= 40,
    paste0(nrow(access_districts), " districts (expect ~51)"))
# The Methods survey description is built from these counts, so they are checked
# here rather than trusted. A count that is only ever checked against a published
# report -- and never against the file being analysed -- is how the wrong Wave 1
# household number survived into the draft.
chk("02", "frame counts written for the Methods survey description",
    all(c("ACCESS Wave 1 households", "ACCESS Wave 2 households",
          "ACCESS Wave 1 districts", "ACCESS Wave 2 districts") %in%
        linkage_access$metric),
    paste0(nrow(linkage_access), " linkage metrics written"))
chk("02", "ACCESS wave counts sum to the pooled frame",
    n_w1 + n_w2 == nrow(pool),
    sprintf("%d + %d = %d", n_w1, n_w2, nrow(pool)))
chk("02", "ACCESS wave sizes match the two-wave panel release",
    n_w1 > 8000 && n_w1 < 9000 && n_w2 > 8500 && n_w2 < 9500,
    sprintf("Wave 1 = %d, Wave 2 = %d", n_w1, n_w2))
chk("02", "ACCESS district counts match the published design (51 / 54)",
    d_w1 == 51 && d_w2 == 54,
    sprintf("Wave 1 = %d districts, Wave 2 = %d", d_w1, d_w2))
# NOT a pass/fail question -- the appended release re-lists every Wave 1
# household, so this overlap is 1.00 by construction and says nothing about
# fielded-sample retention. It is surfaced as a WARN so that any retention claim
# in the manuscript is checked against the file rather than taken on trust.
chk_warn("02", "panel overlap in this file is not the fielded retention rate",
    is.finite(panel_overlap) && panel_overlap >= 0.85,
    sprintf("%.1f%% of Wave 1 household ids recur in Wave 2 (file overlap)",
            100 * panel_overlap))
chk("02", "design-weighted LPG + SE columns present",
    chk_has_cols(access_districts, c("access_w1_mainlpg","access_w1_mainlpg_wt",
                                     "access_w1_mainlpg_wt_se","n_access_w1")))
chk("02", "W1 primary-LPG prevalence in [0,1]",
    chk_in_range(access_districts$access_w1_mainlpg, 0, 1),
    chk_rng(access_districts$access_w1_mainlpg))
chk("02", "use3cat has all four levels",
    setequal(levels(access_hh$use3cat),
             c("Solid fuel reported, no LPG","LPG and solid fuel reported","LPG, no solid fuel reported",
               "Neither LPG nor solid fuel reported")))
chk("02", "LPG spend non-negative (both-source pricing)",
    { s <- access_hh$lpg_spend_yr; all(s[is.finite(s)] >= 0) },
    chk_rng(access_hh$lpg_spend_yr))
# Missing-vs-zero discipline on the two derived LPG quantities. An LPG user with
# no cylinder information must be NA, never 0; a non-user must be 0, never NA.
chk("02", "non-LPG-users have zero (not missing) LPG spend and consumption",
    { nu <- access_hh$uselpg == 0 & !is.na(access_hh$uselpg)
      all(access_hh$lpg_spend_yr[nu] == 0, na.rm = TRUE) &&
      all(access_hh$lpg_kg_yr[nu]   == 0, na.rm = TRUE) &&
      !any(is.na(access_hh$lpg_spend_yr[nu])) && !any(is.na(access_hh$lpg_kg_yr[nu])) })
chk_warn("02", "LPG expenditure share among PURCHASING users is not degenerate",
    { u <- access_hh$uselpg == 1 & !is.na(access_hh$uselpg) &
           is.finite(access_hh$lpg_spend_yr) & access_hh$lpg_spend_yr > 0
      m <- suppressWarnings(as.numeric(as.character(access_hh$month_exp)))
      s <- access_hh$lpg_spend_yr[u] / (12 * m[u]) * 100
      sum(is.finite(s)) >= 100 && median(s, na.rm = TRUE) > 0.5 },
    { u <- access_hh$uselpg == 1 & !is.na(access_hh$uselpg) &
           is.finite(access_hh$lpg_spend_yr) & access_hh$lpg_spend_yr > 0
      m <- suppressWarnings(as.numeric(as.character(access_hh$month_exp)))
      s <- access_hh$lpg_spend_yr[u] / (12 * m[u]) * 100
      sprintf("n = %d purchasing users, median share = %.2f%% (a median at/near 0 means the denominator is wrong, not that LPG is free)",
              sum(is.finite(s)), median(s, na.rm = TRUE)) })
chk_warn("02", "LPG consumption among users is in a physically plausible band (20-200 kg/yr)",
    { k <- access_hh$lpg_kg_yr[access_hh$uselpg == 1 & is.finite(access_hh$lpg_kg_yr) &
                               access_hh$lpg_kg_yr > 0]
      length(k) > 0 && median(k) >= 20 && median(k) <= 200 },
    { k <- access_hh$lpg_kg_yr[access_hh$uselpg == 1 & is.finite(access_hh$lpg_kg_yr) &
                               access_hh$lpg_kg_yr > 0]
      sprintf("n = %d, median = %.1f kg/yr, mean = %.1f kg/yr", length(k),
              median(k), mean(k)) })
chk_warn("02", "ACCESS->NFHS-4 linkage share high",
    mean(!is.na(access_hh$district)) > 0.9,
    paste0(round(100 * mean(!is.na(access_hh$district)), 1), "% matched"))

message("02_prep_access.R done: ", nrow(access_districts), " districts.")
