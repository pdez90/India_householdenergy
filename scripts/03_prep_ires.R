# ==============================================================================
# 03_prep_ires.R
# Prepare IRES (2019-20; 152 district sampling units mapping to 151 distinct
# NFHS-4 districts, 21 states, urban + rural) and produce:
#   - household-level stacking outcomes (same three as ACCESS)
#   - district-level estimates of main-fuel-LPG:
#       (i) glmer main spec, (ii) design-weighted sensitivity (sw_dist)
#     ... produced for ALL households and RURAL-only (to match NFHS-5 rural)
#
# Inputs : IRES.dta, district shapefile
# Outputs: ires_hh.rds, ires_districts.rds
# ==============================================================================

source("00_config.R")
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "03_prep_ires"


ires <- read_dta(path_ires)

# ---- Rename + derive -------------------------------------------------------------
ires <- ires %>%
  dplyr::rename(
    bplaay    = q212_ration_card,
    caste_raw = q211_caste,
    age       = q202_resp_age,
    gender    = q201_resp_gender,
    religion_raw = q210_religion,
    hhno      = q213_no_members,
    lpguse    = q502_lpg_use_yn,
    lpg_only  = q514_lpg_use_all_needs_yn,
    firewood  = q515_firewood_cook_use_yn,
    dung      = q519_dung_cook_use_yn,
    kerosene  = q523_kero_cook_use_yn,
    solidfuel = q514_a_solidfuel_yn,
    agriresidue = q523_agro_cook_use_yn,
    coal      = q523_coal_cook_use_yn,
    mainfuel  = q529_prim_cook_fuel
  ) %>%
  mutate(
    State = tools::toTitleCase(tolower(s_name)),
    District_Name = trimws(tools::toTitleCase(tolower(d_name))),
    # Some IRES sampling units carry split codes ("336-2" = Nadia part 2):
    # strip the suffix so both parts map to the parent census district.
    dist_code_ires = suppressWarnings(as.numeric(sub("-.*$", "", as.character(d_code))))
  )

# IRES q529 primary cooking fuel codes -- CONFIRMED from the CEEW metadata
# workbook: 1 LPG, 2 PNG, 3 electricity, 4 firewood, 5 agri-residue, 6 dung cake,
# 7 coal/charcoal, 8 kerosene, 9 biogas, 10 other. Clean primary fuel = {1,2,3,9}.
# (main_fuel_clean is CLEAN PRIMARY cooking fuel, not "any clean-fuel use".)
IRES_LPG_CODE   <- 1L                 # confirmed: primary-fuel code 1 = LPG
IRES_CLEAN_CODES<- c(1L, 2L, 3L, 9L)  # LPG, PNG, electricity, biogas

# ---- Annual LPG mass from BOTH cylinder sizes (confirmed vars) -----------------
# q505_lpg_large_small_both: 1 large / 2 small / 3 both; q505_1_lpg_large_n large
# (14.2 kg) refills/yr; q506_lpg_small_n small refills/yr; q506_1_lpg_small_kg the
# household-specific small-cylinder size. Earlier code counted large cylinders
# only, undercounting small/both users and hurting cross-survey comparability with
# ACCESS (which counts small+large). Computed here, defensively, as a vector.
.gv <- function(nm) {
  if (nm %in% names(ires)) suppressWarnings(as.numeric(ires[[nm]]))
  else rep(NA_real_, nrow(ires))
}
.cyl_type <- .gv("q505_lpg_large_small_both")
.large_n  <- .gv("q505_1_lpg_large_n")
.small_n  <- .gv("q506_lpg_small_n")
.small_kg <- .gv("q506_1_lpg_small_kg")
.small_kg <- ifelse(is.finite(.small_kg) & .small_kg > 0, .small_kg, 5)  # default 5 kg
# Respect the size question where present: a "large only" household has 0 small.
.large_use <- ifelse(!is.na(.cyl_type) & .cyl_type == 2, 0, .large_n)
.small_use <- ifelse(!is.na(.cyl_type) & .cyl_type == 1, 0, .small_n)
ires$.lpg_kg_from_cyl <- dplyr::coalesce(.large_use, 0) * LPG_KG_LARGE +
                         dplyr::coalesce(.small_use, 0) * .small_kg
ires$.lpg_kg_from_cyl[is.na(.large_use) & is.na(.small_use)] <- NA_real_  # no info -> NA

ires <- ires %>%
  mutate(
    main_fuel_lpg  = as.integer(as.numeric(mainfuel) == IRES_LPG_CODE),
    main_fuel_clean= as.integer(as.numeric(mainfuel) %in% IRES_CLEAN_CODES),
    uselpg    = as.integer(as.numeric(lpguse) == 1),
    usefirewood = as.integer(as.numeric(firewood) == 1),
    usedung   = as.integer(as.numeric(dung) == 1),
    useagro   = as.integer(as.numeric(agriresidue) == 1),
    usecoal   = as.integer(as.numeric(coal) == 1),
    any_solid = row_any1(usefirewood, usedung, useagro, usecoal),
    # 'lpg_only' (q514) is a direct self-report of exclusive use -- keep both
    lpg_exclusive_self = as.integer(as.numeric(lpg_only) == 1),

    stack_binary = case_when(
      main_fuel_lpg == 1 & any_solid == 1 ~ 1L,
      main_fuel_lpg == 1 & any_solid == 0 ~ 0L,
      TRUE ~ NA_integer_),
    # REPORTING-BASED LABELS (see the matching note in 02_prep_access.R). The
    # IRES "meets all cooking needs" item was judged unreliable in the metadata
    # review, so no category claims exclusivity of use: "LPG, no solid fuel
    # reported" states only that LPG was used and no solid fuel was named.
    # "Solid fuel reported, no LPG" requires actual solid-fuel use; non-LPG
    # non-solid households (electricity/PNG/kerosene as primary) get their own
    # category rather than being miscounted as solid-fuel burners downstream.
    # lpg_exclusive_self (the raw self-report) is retained separately so the
    # unreliable item can be inspected but never drives the analytic category.
    use3cat = case_when(
      uselpg == 1 & any_solid == 0 ~ "LPG, no solid fuel reported",
      uselpg == 1 & any_solid == 1 ~ "LPG and solid fuel reported",
      uselpg == 0 & any_solid == 1 ~ "Solid fuel reported, no LPG",
      uselpg == 0 & any_solid == 0 ~ "Neither LPG nor solid fuel reported",
      TRUE ~ NA_character_) %>%
      factor(levels = c("Solid fuel reported, no LPG", "LPG and solid fuel reported",
                        "LPG, no solid fuel reported", "Neither LPG nor solid fuel reported")),

    # LPG kg/yr from BOTH cylinder sizes (see .lpg_kg_from_cyl above)
    lpg_kg_yr = ifelse(uselpg == 1, .lpg_kg_from_cyl, 0),
    # LPG affordability variant of the ACCESS <6%-of-expenditure rule:
    # annual spend = refills/yr x price paid at last refill (q508)
    lpg_spend_yr = suppressWarnings(as.numeric(q505_1_lpg_large_n)) *
                   suppressWarnings(as.numeric(q508_lpg_last_refill_pay)),
    lpg_afford6 = ifelse(uselpg == 1 &
                           !is.na(suppressWarnings(as.numeric(q234_month_exp))) &
                           suppressWarnings(as.numeric(q234_month_exp)) > 0 &
                           !is.na(lpg_spend_yr) & lpg_spend_yr > 0,
                         as.integer(lpg_spend_yr /
                           (12 * suppressWarnings(as.numeric(q234_month_exp))) < 0.06),
                         NA_integer_),
    pmuy = ifelse(q504_lpg_pmuy_yn == 99, NA, as.numeric(q504_lpg_pmuy_yn)),

    electricity = as.integer(as.numeric(q301_grid_yn) == 1),   # grid supply dummy
    # q208: education of primary income earner. CEEW scale assumed 1 = no
    # formal schooling, 2 = up to 5th std, ... -- VERIFY in codebook.
    edu_low = as.integer(suppressWarnings(as.numeric(q208_priminc_earner_edu)) <= 2),
    caste = case_when(as.numeric(caste_raw) == 1 ~ "Scheduled Caste",
                      as.numeric(caste_raw) == 2 ~ "Scheduled Tribe",
                      as.numeric(caste_raw) == 3 ~ "Other Backward Class",
                      as.numeric(caste_raw) == 4 ~ "General",
                      TRUE ~ NA_character_),
    # Missing / don't-know (99) religion -> NA, not "Other" (matches how caste
    # and the ACCESS religion recode treat unknowns; otherwise missingness is
    # silently absorbed into a real category).
    religion = case_when(is.na(religion_raw)              ~ NA_character_,
                         as.numeric(religion_raw) == 99   ~ NA_character_,
                         as.numeric(religion_raw) == 1    ~ "Hindu",
                         as.numeric(religion_raw) == 2    ~ "Muslim",
                         TRUE ~ "Other"),
    hhsize = as.numeric(hhno),
    month_exp = as.numeric(q234_month_exp)
  )

# Rural/urban flag: IRES has a sector/urbanity variable -- check its name.
# Candidates in the local file are printed below; set col_rural to the right one
# and check its coding (commonly 1 = rural, 2 = urban) in the CEEW codebook.
rural_candidates <- grep("sector|urban|rural|area|residence",
                         names(ires), value = TRUE, ignore.case = TRUE)
message("Possible IRES rural/urban columns: ",
        if (length(rural_candidates)) paste(rural_candidates, collapse = ", ")
        else "(none matched -- inspect names(ires))")

# q103_survey_type: "Survey Type (Rural-1; Urban-2)" per the IRES codebook
col_rural <- intersect(c("q103_survey_type", "sector", "urban_rural", "u_r"),
                       names(ires))[1]
if (!is.na(col_rural)) {
  # q103_survey_type CONFIRMED: rural = 1, urban = 2 (unexpected/missing -> NA,
  # not silently 0).
  .ru <- suppressWarnings(as.numeric(ires[[col_rural]]))
  ires$rural <- dplyr::case_when(.ru == 1 ~ 1L, .ru == 2 ~ 0L, TRUE ~ NA_integer_)
  message("Using '", col_rural, "' as the rural/urban flag. Rural share: ",
          round(mean(ires$rural, na.rm = TRUE), 3))
} else {
  warning("Rural/urban column not found; ires$rural set to NA. ",
          "Rural-only IRES estimates will be skipped -- edit col_rural above ",
          "using one of the candidates printed.")
  ires$rural <- NA_integer_
}

# ---- District key: map IRES d_code to the NFHS shapefile dist_code -------------
districts_shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.numeric(dist_code)) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

# Primary match on census district code; fall back to name matching WITHIN
# state (district names repeat across states, which previously caused a
# many-to-many join warning).
ires$district <- ifelse(ires$dist_code_ires %in% districts_shp$dist_code,
                        ires$dist_code_ires, NA)
n_matched_by_code <- sum(!is.na(ires$district))
d_matched_by_code <- n_distinct(ires$district[!is.na(ires$district)])
unmatched <- ires %>% filter(is.na(district)) %>% distinct(State, District_Name)
if (nrow(unmatched) > 0) {
  name_lut <- districts_shp %>% st_drop_geometry() %>%
    transmute(dist_code,
              state_name_t = trimws(tools::toTitleCase(tolower(state_name))),
              dist_name_t  = trimws(tools::toTitleCase(tolower(dist_name)))) %>%
    distinct(state_name_t, dist_name_t, .keep_all = TRUE)
  ires <- ires %>%
    left_join(name_lut,
              by = c("State" = "state_name_t",
                     "District_Name" = "dist_name_t")) %>%
    mutate(district = coalesce(district, dist_code)) %>%
    select(-dist_code)
  still_na <- sum(is.na(ires$district))
  message(nrow(unmatched), " IRES district(s) required name-based matching; ",
          still_na, " households remain unmatched",
          if (still_na > 0) " and are dropped from district estimates." else ".")
}

# ---- Analytic-frame counts for the Methods survey description (2.2.3) --------
# WHY these go to disk instead of being typed into the manuscript: a survey
# description that is re-typed drifts from the file it claims to describe, and
# that is exactly how a wrong ACCESS Wave 1 household count survived several
# rounds of review. Every count the Methods quotes for IRES is derived here and
# interpolated by the manuscript builder, so a re-run of this script updates the
# document and a missing value shows up as "NA" rather than as a stale number.
#
# NOTE on 152 vs 151. IRES fielded 152 district sampling units, identified by
# (State, District_Name). Two of them -- West Bengal's "Nadia-1" and "Nadia-2" --
# fall inside a single NFHS-4 district (code 336), so the survey maps onto 151
# distinct NFHS-4 districts. Both numbers are true of the same file and the
# Methods reports both; neither is a matching failure.
n_su_ires    <- nrow(dplyr::distinct(ires[c("State", "District_Name")]))
n_dist_nfhs4 <- dplyr::n_distinct(ires$district[!is.na(ires$district)])
n_states_ires <- dplyr::n_distinct(ires$State)
share_rural_ires <- mean(ires$rural, na.rm = TRUE)

# ---- IRES -> NFHS-4 district linkage diagnostics (for Methods/SI) --------------
linkage_ires <- tibble(
  metric = c("IRES households, total",
             "households matched by census district code",
             "households matched by state-aware district name",
             "households unmatched (excluded from district estimates)",
             "IRES districts, total",
             "IRES districts matched (code)",
             "IRES districts matched (name fallback)",
             "share of households matched",
             "IRES district sampling units (state x district name)",
             "IRES distinct NFHS-4 districts",
             "IRES states",
             "share of households rural"),
  value = c(nrow(ires),
            n_matched_by_code,
            sum(!is.na(ires$district)) - n_matched_by_code,
            sum(is.na(ires$district)),
            n_distinct(ires$dist_code_ires, na.rm = TRUE),
            d_matched_by_code,
            n_distinct(ires$district[!is.na(ires$district)]) - d_matched_by_code,
            round(mean(!is.na(ires$district)), 4),
            n_su_ires,
            n_dist_nfhs4,
            n_states_ires,
            round(share_rural_ires, 4)))
readr::write_csv(linkage_ires, file.path(dir_out, "ires_linkage_diagnostics.csv"))

message(sprintf(
  "IRES analytic frame: %d households, %d district sampling units mapping to %d distinct NFHS-4 districts in %d states; %.1f%% rural.",
  nrow(ires), n_su_ires, n_dist_nfhs4, n_states_ires, 100 * share_rural_ires))
if (n_su_ires != n_dist_nfhs4) {
  collapsed <- ires %>%
    dplyr::filter(!is.na(district)) %>%
    dplyr::distinct(State, District_Name, district) %>%
    dplyr::group_by(district) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(district, District_Name)
  message(sprintf(
    "  %d sampling unit(s) share an NFHS-4 district code, so %d units collapse to %d districts:",
    nrow(collapsed), n_su_ires, n_dist_nfhs4))
  print(as.data.frame(collapsed))
}

# Identify exactly WHICH districts remain unlinked (state, name, households):
unmatched_final <- ires %>%
  filter(is.na(district)) %>%
  count(State, District_Name, d_code_raw = as.character(d_code),
        name = "households")
if (nrow(unmatched_final) > 0) {
  message("Unlinked IRES district(s):")
  print(as.data.frame(unmatched_final))
  readr::write_csv(unmatched_final,
                   file.path(dir_out, "ires_unmatched_districts.csv"))
}
message("IRES -> NFHS-4 linkage: ",
        n_distinct(ires$district[!is.na(ires$district)]), " districts matched (",
        d_matched_by_code, " by code); ",
        sum(is.na(ires$district)), " of ", nrow(ires), " households unmatched.")

ires_hh <- ires %>%
  transmute(survey = "IRES", state = State, district = as.character(district),
            district_name = District_Name, hhid,
            village = village_ward_census_code, wt = sw_dist, rural,
            main_fuel_lpg, main_fuel_clean, uselpg, any_solid,
            stack_binary, use3cat, lpg_exclusive_self, lpg_kg_yr,
            lpg_spend_yr, lpg_afford6, pmuy,
            caste, religion, hhsize, bplaay = as.numeric(bplaay),
            electricity, edu_low,
            month_exp, age = as.numeric(age), gender = as.numeric(gender))
stopifnot(all(ires_hh$any_solid %in% c(0L, 1L, NA_integer_)))
stopifnot(!any(is.infinite(ires_hh$any_solid), na.rm = TRUE))
saveRDS(ires_hh, file.path(dir_out, "ires_hh.rds"))

# ---- District estimates ---------------------------------------------------------
ires_f <- ires_hh %>%
  mutate(state = factor(state), district = factor(district),
         village = factor(village))

est_all <- district_estimates_glmer(ires_f, "main_fuel_lpg", cluster = "village") %>%
  rename(ires_mainlpg = p_hat, n_ires = n_hh)

# Rural-only estimates: skipped gracefully if the rural flag is unavailable
if (sum(ires_f$rural == 1, na.rm = TRUE) > 0) {
  est_rural <- district_estimates_glmer(
    ires_f %>% filter(rural == 1), "main_fuel_lpg", cluster = "village") %>%
    transmute(district, ires_mainlpg_rural = p_hat, n_ires_rural = n_hh)
} else {
  message("No rural flag available -- ires_mainlpg_rural set to NA. ",
          "Set col_rural above and re-run to get rural-only estimates.")
  est_rural <- est_all %>%
    transmute(district, ires_mainlpg_rural = NA_real_, n_ires_rural = NA_real_)
}

est_stack <- district_estimates_glmer(
  ires_f %>% filter(!is.na(stack_binary)), "stack_binary", cluster = "village") %>%
  transmute(district, ires_stack = p_hat)

est_excl <- ires_f %>%
  mutate(excl_lpg = as.integer(use3cat == "LPG, no solid fuel reported")) %>%
  district_estimates_glmer("excl_lpg", cluster = "village") %>%
  transmute(district, ires_excl_lpg = p_hat)

est_afford <- ires_f %>%
  filter(!is.na(lpg_afford6)) %>%
  district_estimates_glmer("lpg_afford6", cluster = "village") %>%
  transmute(district, ires_lpg_afford6 = p_hat)

est_kg <- ires_f %>%
  filter(!is.na(wt)) %>%
  as_survey_design(ids = village, weights = wt) %>%
  group_by(district) %>%
  summarise(ires_lpg_kg_yr = survey_mean(lpg_kg_yr, na.rm = TRUE))

# Design-weighted sensitivity with IRES district weights (sw_dist), all households
est_wt <- district_estimates_weighted(
  ires_f, "main_fuel_lpg", ids = "village", weights = "wt") %>%
  rename(ires_mainlpg_wt = p_wt, ires_mainlpg_wt_se = p_wt_se)

# Design-weighted RURAL estimate (+ Taylor-linearized SE). This is the reference
# the Bayesian measurement-error correction now uses for NFHS-5: it matches the
# rural design-weighted NFHS-5 predictor (like against like), and its design SE
# supplies the reference-side measurement-error variance directly, instead of a
# simple-binomial approximation. Skipped gracefully if the rural flag is absent.
if (sum(ires_f$rural == 1, na.rm = TRUE) > 0) {
  # Build the design on the FULL sample and subset to the rural DOMAIN inside the
  # design object (domain_col), rather than pre-filtering the raw frame -- this is
  # the correct Taylor-linearized rural SE that then feeds the Bayesian correction.
  est_wt_rural <- district_estimates_weighted(
    ires_f, "main_fuel_lpg", ids = "village", weights = "wt", domain_col = "rural") %>%
    rename(ires_mainlpg_rural_wt = p_wt, ires_mainlpg_rural_wt_se = p_wt_se)
} else {
  message("No rural flag -- ires_mainlpg_rural_wt set to NA (correction will ",
          "fall back to the all-household design-weighted IRES reference).")
  est_wt_rural <- est_all %>%
    transmute(district, ires_mainlpg_rural_wt = NA_real_,
              ires_mainlpg_rural_wt_se = NA_real_)
}

ires_districts <- est_all %>%
  left_join(est_rural, by = "district") %>%
  left_join(est_stack, by = "district") %>%
  left_join(est_afford, by = "district") %>%
  left_join(est_excl,  by = "district") %>%
  left_join(est_kg %>% mutate(district = factor(district)), by = "district") %>%
  left_join(est_wt %>% mutate(district = factor(district)), by = "district") %>%
  left_join(est_wt_rural %>% mutate(district = factor(district)), by = "district") %>%
  mutate(district = as.character(district))

saveRDS(ires_districts, file.path(dir_out, "ires_districts.rds"))
## ---- CHECKS ------------------------------------------------------------------
chk_header("03_prep_ires")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("03", "03_prep_ires")

chk("03", "IRES district table non-empty", nrow(ires_districts) >= 130,
    paste0(nrow(ires_districts), " districts"))
# The Methods survey description is built from these four numbers, so they are
# checked here rather than trusted: if any of them moves, the manuscript moves
# with it and the reader should be told the frame changed.
chk("03", "frame counts written for the Methods survey description",
    all(c("IRES district sampling units (state x district name)",
          "IRES distinct NFHS-4 districts",
          "IRES states",
          "share of households rural") %in% linkage_ires$metric),
    paste0(nrow(linkage_ires), " linkage metrics written"))
chk("03", "IRES sampling units >= distinct NFHS-4 districts",
    n_su_ires >= n_dist_nfhs4,
    sprintf("%d sampling units -> %d NFHS-4 districts", n_su_ires, n_dist_nfhs4))
chk("03", "IRES frame size matches the reported survey scale",
    nrow(ires) > 14000 && nrow(ires) < 16000 &&
    n_states_ires >= 20 && n_states_ires <= 22,
    sprintf("%d households, %d states", nrow(ires), n_states_ires))
chk("03", "IRES rural share plausible for a 2/3-rural design",
    is.finite(share_rural_ires) && share_rural_ires > 0.55 && share_rural_ires < 0.75,
    sprintf("%.1f%% rural", 100 * share_rural_ires))
chk("03", "RURAL design-weighted columns present (feed 04/05 correction)",
    chk_has_cols(ires_districts, c("ires_mainlpg_rural","ires_mainlpg_rural_wt",
                                   "ires_mainlpg_rural_wt_se","n_ires_rural")))
chk("03", "rural design-weighted LPG in [0,1] and not all-NA",
    chk_in_range(ires_districts$ires_mainlpg_rural_wt, 0, 1) &&
    any(is.finite(ires_districts$ires_mainlpg_rural_wt)),
    chk_rng(ires_districts$ires_mainlpg_rural_wt))
chk("03", "rural design-weighted SE finite & positive",
    { s <- ires_districts$ires_mainlpg_rural_wt_se; any(is.finite(s)) && all(s[is.finite(s)] >= 0) },
    chk_rng(ires_districts$ires_mainlpg_rural_wt_se))
chk("03", "household rural flag present & 0/1",
    "rural" %in% names(ires_hh) && chk_in_range(ires_hh$rural, 0, 1))
chk("03", "use3cat has all four levels",
    setequal(levels(ires_hh$use3cat),
             c("Solid fuel reported, no LPG","LPG and solid fuel reported","LPG, no solid fuel reported",
               "Neither LPG nor solid fuel reported")))
chk_warn("03", "LPG kg/yr uses both cylinders (mean among users)",
    any(is.finite(ires_hh$lpg_kg_yr)),
    { u <- ires_hh$lpg_kg_yr[ires_hh$uselpg == 1]; sprintf("mean %.1f kg/yr", mean(u, na.rm = TRUE)) })
chk_warn("03", "religion Other share plausible (<15%; 99/NA -> NA)",
    { s <- mean(ires_hh$religion == "Other", na.rm = TRUE); is.finite(s) && s < 0.15 },
    paste0("Other = ", round(100 * mean(ires_hh$religion == "Other", na.rm = TRUE), 1), "%"))

message("03_prep_ires.R done: ", nrow(ires_districts), " districts.")
