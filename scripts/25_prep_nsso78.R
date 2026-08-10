# ==============================================================================
# 25_prep_nsso78.R
# Prepare the NSSO 78th round Multiple Indicator Survey (MIS; fieldwork
# Jan 2020 - Aug 2021, COVID-extended) and produce district-level estimates of
# main-fuel-LPG on the SAME NFHS-4 2015 district geography (census-2011
# dist_code) used everywhere else in this pipeline, so NSSO slots directly
# into the NFHS-5 / IRES comparisons:
#   (i)  glmer main spec (state / district / FSU random effects), and
#   (ii) design-weighted direct estimates (MULT/100 weights, FSU-clustered,
#        stratum-stratified) with Taylor-linearized SEs
#   ... each for ALL households and RURAL-only.
#
# Inputs : NSSO 78 unit-level CSV (household block, Level 03), the district
#          key nsso78_district_key.csv (provenance below), district shapefile
# Outputs: nsso78_hh.rds, nsso78_districts.rds,
#          nsso78_linkage_diagnostics.csv, nsso78_unmatched_districts.csv
#
# --- Data provenance ----------------------------------------------------------
# The MoSPI microdata-portal download arrives with UUID filenames. The file
# used here is the LEVEL 03 record (Questionnaire 5.1, Block 4, items 1-28:
# household characteristics), which carries
#   "primary source of energy used for cooking (code)"  [Block 4, item 16].
# The same records exist as fixed-width ms51l03.TXT inside the .rar archive.
#
# Fuel codes (Instructions to Field Staff, Vol-II, Schedule 5.1, item 16):
#   firewood, chips & crop residue-01, LPG-02, other natural gas-03,
#   dung cake-04, kerosene-05, coke/coal-06, gobar gas-07, other biogas-08,
#   charcoal-09, electricity (incl. solar/wind generated)-10, solar cooker-11,
#   no cooking arrangement-12, others-19
#
# Weights: wt = MULT/100 (combined-subsample final multiplier). VERIFIED:
#   sum(wt) = 265.1 million households, and the weighted share of households
#   using CLEAN fuel (codes 02,03,07,08,10,11) among households WITH a cooking
#   arrangement reproduces the MoSPI MIS press-note figures
#   (rural 49.8%, urban 92.0%, total 63.1%) to <0.2 pp. These are asserted in
#   the CHECKS block below so a wrong weight rule cannot survive silently.
#
# Denominator convention: main_fuel_lpg keeps ALL households in the
#   denominator ("no cooking arrangement" counts as not-LPG), matching how
#   01_prep_nfhs.R treats NFHS households (hv226 == 95 "no food cooked" is
#   simply not LPG there either). The no_cook flag is retained so the
#   press-note denominator can be reproduced (see CHECKS).
#
# District key: nsso78_district_key.csv maps every (NSS state, NSS district)
#   code pair in the data to a census-2011 district code (= dist_code in the
#   NFHS-4 shapefile). It was built from Appendix I ("List of NSS regions and
#   their composition") of the Vol-I field instructions, then post-2011
#   districts (47 of 685) were assigned to their dominant 2011 parent district
#   -- the same collapse logic as IRES's Nadia-1/Nadia-2 (see 03_prep_ires.R).
#   match_type in the key records how each row was resolved
#   (exact / alias / parent2011) and 'note' documents every judgment call,
#   including two Telangana districts (Mulugu, Narayanpet) that are absent
#   from Appendix I and whose codes were inferred from the NSS region carried
#   in the data itself. Multi-parent splits (e.g. Morbi, Botad, Mahisagar,
#   Siddipet) are assigned to the dominant parent and flagged in 'note'.
# ==============================================================================

source("00_config.R")
CHK_SCRIPT <- "25_prep_nsso78"

# ---- Paths ---------------------------------------------------------------------
# Override with environment variables rather than editing the script:
#   DIR_NSSO      folder holding the MoSPI unit-level download
#   NSSO_HH_CSV   full path to the Level-03 household CSV (the MoSPI download
#                 arrives with a UUID filename; identify it by its header,
#                 which contains 'primary source of energy used for cooking')
#   NSSO_KEY_CSV  full path to the district key. By default the key checked
#                 into this repository (data/nsso78_district_key.csv, relative
#                 to the scripts/ folder this is run from) is used, falling
#                 back to a copy placed next to the unit data.
dir_nsso <- Sys.getenv("DIR_NSSO", "")
if (!nzchar(dir_nsso)) {
  # Authors' local layout, used only if it actually exists on this machine;
  # any other user gets the actionable stop() below rather than a stranger's
  # path failing obscurely.
  .author_dir <- "/Users/priyanka/India_energy/NSSO78"
  if (dir.exists(.author_dir)) dir_nsso <- .author_dir
}
path_nsso_hh <- Sys.getenv("NSSO_HH_CSV", "")
if (!nzchar(path_nsso_hh) && nzchar(dir_nsso))
  path_nsso_hh <- file.path(dir_nsso, "d8fcc080-affe-445d-864a-f8822b9fb8d6.csv")
if (!nzchar(path_nsso_hh))
  stop("NSSO-78 unit data not configured. Set the DIR_NSSO environment ",
       "variable to the folder holding the MoSPI unit-level download, or ",
       "NSSO_HH_CSV to the Level-03 household CSV directly (the ~36 MB file ",
       "whose header contains 'primary source of energy used for cooking').",
       call. = FALSE)
.key_cand <- c(Sys.getenv("NSSO_KEY_CSV", ""),
               file.path("..", "data", "nsso78_district_key.csv"),
               file.path(dir_nsso, "nsso78_district_key.csv"))
.key_cand <- .key_cand[nzchar(.key_cand)]
path_nsso_key <- c(.key_cand[file.exists(.key_cand)], .key_cand[length(.key_cand)])[1]
message("NSSO district key: ", normalizePath(path_nsso_key, mustWork = FALSE))

for (p in c(path_nsso_hh, path_nsso_key)) {
  if (!file.exists(p))
    stop("Missing input: ", p,
         "\n  Set DIR_NSSO / NSSO_HH_CSV to the NSSO 78 Level-03 household CSV ",
         "(the ~36 MB file whose header contains 'primary source of energy ",
         "used for cooking'), and NSSO_KEY_CSV if the district key is not in ",
         "the repository's data/ folder.", call. = FALSE)
}

# ---- Read ----------------------------------------------------------------------
nsso_raw <- readr::read_csv(path_nsso_hh, col_types = readr::cols(.default = "c"),
                            progress = FALSE)
names(nsso_raw) <- trimws(names(nsso_raw))

# Column lookup by pattern rather than position, so a re-download with
# reordered columns fails loudly here instead of silently misreading.
find_col <- function(pattern) {
  hit <- grep(pattern, names(nsso_raw), ignore.case = TRUE, value = TRUE)
  if (length(hit) != 1)
    stop("Expected exactly 1 column matching '", pattern, "', found ",
         length(hit), ": ", paste(hit, collapse = " | "), call. = FALSE)
  hit
}
col_cook   <- find_col("^primary source of energy used for cooking")
col_relig  <- find_col("^religion")
col_social <- find_col("^social group")
col_hhsize <- find_col("^household size")
col_mpce   <- find_col("^Household usual monthly consumer expenditure")

nsso <- nsso_raw %>%
  transmute(
    fsu        = `FSU Serial No.`,
    state_code = as.integer(State),
    nss_district_code = as.integer(District),
    sector     = as.integer(Sector),            # 1 rural, 2 urban
    stratum    = Stratum,
    substratum = `Sub-Stratum`,
    ssu_no     = `Sample hhld. No.`,
    cook       = suppressWarnings(as.integer(.data[[col_cook]])),
    hhsize     = suppressWarnings(as.numeric(.data[[col_hhsize]])),
    religion_code = suppressWarnings(as.integer(.data[[col_relig]])),
    social_group_code = suppressWarnings(as.integer(.data[[col_social]])),
    mpce       = suppressWarnings(as.numeric(.data[[col_mpce]])),
    mult       = suppressWarnings(as.numeric(MULT)),
    nsc        = suppressWarnings(as.integer(NSC))
  ) %>%
  mutate(
    wt    = mult / 100,                          # final weight (see header note)
    rural = dplyr::case_when(sector == 1 ~ 1L, sector == 2 ~ 0L,
                             TRUE ~ NA_integer_),
    # LPG = code 02. ALL households in the denominator (see header note).
    main_fuel_lpg   = dplyr::if_else(is.na(cook), NA_integer_,
                                     as.integer(cook == 2L)),
    # Clean primary fuel per the MIS press-note definition:
    # LPG, other natural gas, gobar gas, other biogas, electricity, solar cooker.
    # (For strict symmetry with IRES_CLEAN_CODES -- LPG/PNG/electricity/biogas --
    #  drop code 11; the solar-cooker share is 0.003% so it cannot matter.)
    main_fuel_clean = dplyr::if_else(is.na(cook), NA_integer_,
                                     as.integer(cook %in% c(2L, 3L, 7L, 8L, 10L, 11L))),
    no_cook = dplyr::if_else(is.na(cook), NA_integer_, as.integer(cook == 12L)),
    # Design stratification: strata are formed within district x sector in the
    # NSS design, so the stratum identifier must nest all four components.
    strata_id = paste(state_code, sector, nss_district_code,
                      stratum, substratum, sep = "_")
  )

# ---- National benchmarks BEFORE any linkage (validates weights + fuel codes) ---
wtot <- sum(nsso$wt, na.rm = TRUE)
nat_share <- function(num, den) {
  sum(nsso$wt[num], na.rm = TRUE) / sum(nsso$wt[den], na.rm = TRUE)
}
has_cook  <- !is.na(nsso$cook) & nsso$cook != 12L      # press-note denominator
clean_ok  <- nsso$main_fuel_clean == 1L
nat <- c(
  clean_rural = nat_share(clean_ok & nsso$rural == 1L, has_cook & nsso$rural == 1L),
  clean_urban = nat_share(clean_ok & nsso$rural == 0L, has_cook & nsso$rural == 0L),
  clean_total = nat_share(clean_ok, has_cook),
  lpg_total_alldenom = nat_share(nsso$main_fuel_lpg == 1L, !is.na(nsso$main_fuel_lpg))
)
message(sprintf(
  "NSSO 78 national benchmarks: %.1f M households; clean-fuel (press-note denom) rural %.1f%% / urban %.1f%% / total %.1f%% (published: 49.8 / 92.0 / 63.1); LPG (all-hh denom) %.1f%%.",
  wtot / 1e6, 100 * nat["clean_rural"], 100 * nat["clean_urban"],
  100 * nat["clean_total"], 100 * nat["lpg_total_alldenom"]))

# ---- District key -> census-2011 dist_code (the pipeline's district id) --------
key <- readr::read_csv(path_nsso_key, col_types = readr::cols(.default = "c")) %>%
  mutate(nss_state_code    = as.integer(nss_state_code),
         nss_district_code = as.integer(nss_district_code),
         census2011_code   = as.integer(census2011_code))

stopifnot(!any(duplicated(key[c("nss_state_code", "nss_district_code")])))

nsso <- nsso %>%
  left_join(key %>% select(nss_state_code, nss_district_code, state_name,
                           nss_district_name, census2011_code, census2011_name,
                           match_type),
            by = c("state_code" = "nss_state_code",
                   "nss_district_code" = "nss_district_code")) %>%
  mutate(district = as.character(census2011_code))

# Which of those census codes actually exist in the shapefile the rest of the
# pipeline is keyed to? (They should all -- census-2011 codes -- but assert.)
districts_shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(dist_code = as.numeric(dist_code), state_name_shp = state_name,
            dist_name_shp = dist_name)
in_shp <- nsso$census2011_code %in% districts_shp$dist_code
if (any(!in_shp, na.rm = TRUE)) {
  message(sum(!in_shp, na.rm = TRUE), " households sit in census-2011 codes ",
          "absent from the shapefile -- see nsso78_unmatched_districts.csv.")
}

# ---- Linkage diagnostics (Methods/SI; mirrors 03_prep_ires.R) -------------------
n_pairs_data <- nrow(dplyr::distinct(nsso, state_code, nss_district_code))
linkage_nsso <- tibble(
  metric = c("NSSO-78 households, total (Level 03)",
             "sum of design weights (million households)",
             "NSS (state x district) code pairs in data",
             "pairs resolved by the district key",
             "households with a census-2011 district",
             "households whose census-2011 code is missing from the shapefile",
             "distinct census-2011 districts",
             "key rows resolved exact / alias / parent2011",
             "share of households rural (unweighted)"),
  value = c(nrow(nsso),
            round(wtot / 1e6, 2),
            n_pairs_data,
            n_pairs_data - nrow(dplyr::distinct(
              nsso %>% filter(is.na(census2011_code)),
              state_code, nss_district_code)),
            sum(!is.na(nsso$census2011_code)),
            sum(!in_shp, na.rm = TRUE),
            dplyr::n_distinct(nsso$census2011_code, na.rm = TRUE),
            paste(table(factor(key$match_type,
                               c("exact", "alias", "parent2011"))),
                  collapse = " / "),
            round(mean(nsso$rural, na.rm = TRUE), 4)))
readr::write_csv(linkage_nsso, file.path(dir_out, "nsso78_linkage_diagnostics.csv"))

unmatched_nsso <- nsso %>%
  filter(is.na(census2011_code) | !census2011_code %in% districts_shp$dist_code) %>%
  count(state_code, nss_district_code, nss_district_name, census2011_code,
        name = "households")
readr::write_csv(unmatched_nsso, file.path(dir_out, "nsso78_unmatched_districts.csv"))
if (nrow(unmatched_nsso) > 0) {
  message("Unlinked NSSO district(s):"); print(as.data.frame(unmatched_nsso))
}

# ---- Household frame -------------------------------------------------------------
nsso_hh <- nsso %>%
  filter(!is.na(census2011_code)) %>%
  transmute(survey = "NSSO78", state = state_name, district,
            district_name = census2011_name, nss_district_name,
            fsu, wt, rural, strata_id,
            main_fuel_lpg, main_fuel_clean, no_cook,
            hhsize, religion_code, social_group_code, mpce)
saveRDS(nsso_hh, file.path(dir_out, "nsso78_hh.rds"))

# ---- District estimates -----------------------------------------------------------
# Note on collapsing and counts. The key covers all 685 districts of the NSS
# 2020 frame; 684 (state, district) code pairs are actually observed in the
# unit data. The 47 parent2011 rows fold post-2011 splits back into their 2011
# parents (exactly as IRES's Nadia-1/2 fold into census 336), so the surveyed
# households map onto 639 distinct census-2011 districts (of 640; Pondicherry
# district was not surveyed). Grouping by `district` performs the collapse
# automatically for every estimator below.
nsso_f <- nsso_hh %>%
  mutate(state = factor(state), district = factor(district), fsu = factor(fsu),
         # factor, not character: some srvyr versions push character strata
         # through tapply()/as.numeric() and spray 'NAs introduced by coercion'
         # warnings (estimates unaffected -- asserted below -- but noisy).
         strata_id = factor(strata_id))

est_all <- district_estimates_glmer(nsso_f, "main_fuel_lpg", cluster = "fsu") %>%
  rename(nsso_mainlpg = p_hat, n_nsso = n_hh)

est_rural <- district_estimates_glmer(
  nsso_f %>% filter(rural == 1), "main_fuel_lpg", cluster = "fsu") %>%
  transmute(district, nsso_mainlpg_rural = p_hat, n_nsso_rural = n_hh)

# Design-weighted direct estimates (+ Taylor-linearized SEs). Full-sample
# design; rural via domain estimation (same rationale as 01/03).
est_wt <- district_estimates_weighted(
  nsso_f, "main_fuel_lpg", ids = "fsu", strata = "strata_id", weights = "wt") %>%
  rename(nsso_mainlpg_wt = p_wt, nsso_mainlpg_wt_se = p_wt_se)

est_wt_rural <- district_estimates_weighted(
  nsso_f, "main_fuel_lpg", ids = "fsu", strata = "strata_id", weights = "wt",
  domain_col = "rural") %>%
  rename(nsso_mainlpg_rural_wt = p_wt, nsso_mainlpg_rural_wt_se = p_wt_se)

# Clean-fuel analogue (design-weighted only; used for SI benchmarks)
est_clean_wt <- district_estimates_weighted(
  nsso_f, "main_fuel_clean", ids = "fsu", strata = "strata_id", weights = "wt") %>%
  rename(nsso_clean_wt = p_wt, nsso_clean_wt_se = p_wt_se)

nsso78_districts <- est_all %>%
  left_join(est_rural,   by = "district") %>%
  left_join(est_wt       %>% mutate(district = factor(district)), by = "district") %>%
  left_join(est_wt_rural %>% mutate(district = factor(district)), by = "district") %>%
  left_join(est_clean_wt %>% mutate(district = factor(district)), by = "district") %>%
  mutate(district = as.character(district)) %>%
  # Design-weighted proportions can land a floating-point epsilon outside
  # [0, 1] (e.g. 1 + 2e-16 in an all-LPG district), which then fails range
  # checks and would NaN a logit downstream. Clamp the PROPORTION columns
  # only (never the SEs).
  mutate(across(c(nsso_mainlpg_wt, nsso_mainlpg_rural_wt, nsso_clean_wt),
                ~ pmin(pmax(.x, 0), 1)))

saveRDS(nsso78_districts, file.path(dir_out, "nsso78_districts.rds"))

# ---- Independent recomputation of the design SEs (srvyr-version guard) --------
# Some srvyr versions emit 'NAs introduced by coercion' warnings from tapply()
# inside grouped survey_mean(). The estimates are unaffected, but assert that
# rather than assume it: recompute the three largest districts straight
# through survey::svyby on an identically specified design and require
# agreement to 1e-6. Because strata_id nests inside district, whole districts
# are unions of whole strata, so the subset design has the same variance
# structure as the full one.
big3 <- nsso_f %>% dplyr::count(district) %>% dplyr::slice_max(n, n = 3) %>%
  dplyr::pull(district) %>% as.character()
des3 <- survey::svydesign(
  ids = ~fsu, strata = ~strata_id, weights = ~wt, nest = TRUE,
  data = nsso_f %>% filter(as.character(district) %in% big3) %>% droplevels())
ref3 <- survey::svyby(~main_fuel_lpg, ~district, des3, survey::svymean,
                      na.rm = TRUE)
cmp3 <- nsso78_districts %>%
  filter(district %in% big3) %>%
  left_join(ref3 %>%
              transmute(district = as.character(district),
                        p_ref = main_fuel_lpg, se_ref = se),
            by = "district")
se_guard_ok <- nrow(cmp3) == 3 &&
  max(abs(cmp3$nsso_mainlpg_wt - cmp3$p_ref)) < 1e-6 &&
  max(abs(cmp3$nsso_mainlpg_wt_se - cmp3$se_ref)) < 1e-6

## ---- CHECKS ------------------------------------------------------------------
chk_header("25_prep_nsso78")
if (exists("chk_singular_summary")) chk_singular_summary("25", "25_prep_nsso78")

chk("25", "weight rule reproduces total households (250-285 M in 2020-21)",
    wtot / 1e6 > 250 && wtot / 1e6 < 285, sprintf("%.1f M", wtot / 1e6))
chk("25", "clean-fuel benchmarks match MIS press note to 0.5 pp (49.8/92.0/63.1)",
    abs(100 * nat["clean_rural"] - 49.8) < 0.5 &&
    abs(100 * nat["clean_urban"] - 92.0) < 0.5 &&
    abs(100 * nat["clean_total"] - 63.1) < 0.5,
    sprintf("rural %.2f / urban %.2f / total %.2f",
            100 * nat["clean_rural"], 100 * nat["clean_urban"],
            100 * nat["clean_total"]))
chk("25", "every (state, district) pair in the data resolves through the key",
    sum(is.na(nsso$census2011_code)) == 0,
    paste0(sum(is.na(nsso$census2011_code)), " unresolved households"))
chk("25", "all census-2011 codes exist in the pipeline shapefile",
    all(in_shp, na.rm = TRUE),
    paste0(sum(!in_shp, na.rm = TRUE), " households outside shapefile"))
chk("25", "district table covers (nearly) the full 2011 geography",
    nrow(nsso78_districts) >= 630, paste0(nrow(nsso78_districts), " districts"))
chk("25", "rural + design-weighted columns present (feed 26)",
    chk_has_cols(nsso78_districts,
                 c("nsso_mainlpg", "nsso_mainlpg_rural", "nsso_mainlpg_wt",
                   "nsso_mainlpg_wt_se", "nsso_mainlpg_rural_wt",
                   "nsso_mainlpg_rural_wt_se", "n_nsso", "n_nsso_rural")))
chk("25", "LPG prevalences in [0,1] and not all-NA",
    chk_in_range(nsso78_districts$nsso_mainlpg_wt, 0, 1) &&
    any(is.finite(nsso78_districts$nsso_mainlpg_wt)),
    chk_rng(nsso78_districts$nsso_mainlpg_wt))
chk("25", "srvyr design estimates/SEs match direct survey::svyby (3 largest districts)",
    isTRUE(se_guard_ok),
    sprintf("max |dp| %.1e, max |dse| %.1e",
            max(abs(cmp3$nsso_mainlpg_wt - cmp3$p_ref)),
            max(abs(cmp3$nsso_mainlpg_wt_se - cmp3$se_ref))))
chk("25", "design SE finite & positive where estimate exists",
    { s <- nsso78_districts$nsso_mainlpg_wt_se
      any(is.finite(s)) && all(s[is.finite(s)] >= 0) },
    chk_rng(nsso78_districts$nsso_mainlpg_wt_se))
chk_warn("25", "min district sample size worth reporting alongside estimates",
    min(nsso78_districts$n_nsso, na.rm = TRUE) >= 20,
    sprintf("min n_hh = %d (median %d)",
            min(nsso78_districts$n_nsso, na.rm = TRUE),
            round(median(nsso78_districts$n_nsso, na.rm = TRUE))))
chk_warn("25", "glmer and design-weighted estimates broadly agree (r > 0.9)",
    { r <- cor(nsso78_districts$nsso_mainlpg, nsso78_districts$nsso_mainlpg_wt,
               use = "pairwise.complete.obs"); is.finite(r) && r > 0.9 },
    sprintf("r = %.3f", cor(nsso78_districts$nsso_mainlpg,
                            nsso78_districts$nsso_mainlpg_wt,
                            use = "pairwise.complete.obs")))

message("25_prep_nsso78.R done: ", nrow(nsso78_districts), " districts, ",
        nrow(nsso_hh), " households.")
