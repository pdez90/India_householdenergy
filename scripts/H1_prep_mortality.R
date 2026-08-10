# ==============================================================================
# H1_prep_mortality.R   (STANDALONE -- does not source 00_config.R)
#
# Builds district-level child-mortality estimates from the NFHS-4/5 Children's
# Recode (KR) files, plus district SES/household covariates from the WOMEN's
# recode (IR) files (one row per woman, NOT birth-weighted; #17), on the SAME
# NFHS-4 district geography (dist_code) used by the energy pipeline, so the
# outputs join cleanly to corrected_nfhs_districts.rds from 05_correction.R.
#
# Standalone: reads only raw data files from disk. Run on its own with
#   Rscript H1_prep_mortality.R
#
# Mortality outcomes (completed-exposure cohort probabilities over the recent
# birth history; each birth censored to NA until observed for the full age
# interval): neonatal_death (died <1 mo), infant_death (died <12 mo). Estimated
# with a null 3-level model (PRIMARY) and, additionally, with DHS design weights
# (v005) as a direct-estimate SI sensitivity (#18).
# SES covariates (district prevalences, both years, from IR): poor (wealth Q1),
# mother_low_edu, electricity, muslim, improved_sanitation, improved_water.
# Also output: eligible-birth counts per round and their harmonic-mean district
# weight (for H2; #19), and a short-window nonoverlapping-cohort variant (#16).
#
# Outputs (one row per district; *_2015/_2019 columns):
#   health_district_wide.rds            RURAL, 120-mo window   (MAIN)
#   health_district_wide_all.rds        ALL households, 120-mo (SI sensitivity)
#   health_district_wide_nonoverlap.rds RURAL, short window    (SI; #16)
# ==============================================================================

## ---- CONFIG (edit paths to match your machine) ------------------------------
dir_out   <- "/Users/priyanka/Downloads/ACCESS_replica"

path_kr_2015 <- "/Users/priyanka/Downloads/DHS_India/IAKR74DT/IAKR74FL.DTA"
path_kr_2019 <- "/Users/priyanka/Downloads/DHS_India/India2019/IAKR7DDT/IAKR7DFL.DTA"

# IR (women's recode) files -- district SES/household covariates are estimated
# from these (one row per woman), NOT from the birth-level KR file. Paths follow
# the DHS naming convention (IAIR* alongside IAKR*); VERIFY/EDIT to match your
# disk. If absent, SES falls back to the birth-level KR file with a warning.
path_ir_2015 <- "/Users/priyanka/Downloads/DHS_India/IAIR74DT/IAIR74FL.DTA"
path_ir_2019 <- "/Users/priyanka/Downloads/DHS_India/India2019/IAIR7DDT/IAIR7DFL.DTA"

# Cluster GPS (same RData files used by 01_prep_nfhs.R): each has object `gps`
# with columns cluster, LATNUM, LONGNUM
path_gps_2015 <- "/Users/priyanka/Downloads/DHS_India/India2015/gps_india.RData"
path_gps_2019 <- "/Users/priyanka/Downloads/DHS_India/India2019/gps_india.RData"

# NFHS-4 district shapefile (the dist_code geography used throughout 01-07)
path_districts_shp <- "/Users/priyanka/Downloads/DHS_India/district_nfhs_shapefile/nfhs_data.shp"

# This script writes BOTH population versions in a single run:
#   health_district_wide.rds       -> RURAL births   (MAIN analysis; matches the
#                                      rural-only corrected exposure)
#   health_district_wide_all.rds   -> ALL-household births (SI sensitivity)
POPS <- c("rural", "all")

## ---- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse); library(haven); library(sf); library(lme4)
  library(survey); library(srvyr)   # design-weighted district mortality (SI)
})
options(stringsAsFactors = FALSE)

# Single-PSU strata: state the variance policy HERE as well as in 00_config.R.
# This script is standalone by design and does not source 00_config.R, so run on
# its own it would otherwise inherit the survey package default
# (survey.lonely.psu = "fail"), which STOPS on the first single-PSU stratum.
# Run through run_everything.R it happens to inherit "adjust" because the runner
# sources every step into one session and 01_prep_nfhs.R (which does source the
# config) runs first -- i.e. the correct behaviour was an accident of run order.
#
# Policy, stated once and deliberately: "adjust" centers a lonely PSU at the
# GRAND mean rather than its own stratum mean, which yields a conservative
# (upward-biased) variance contribution instead of dropping the stratum or
# treating it as a certainty unit with zero contribution. That matters here
# beyond tidiness, because these design SEs are the `y_se` / `x_se` inputs to the
# brms measurement-error model in 05_correction.R: a policy that quietly returned
# zero variance for lonely strata would feed the calibration overconfident
# reference values. survey.adjust.domain.lonely = TRUE applies the same treatment
# after subsetting (rural-only, single-round), which is where most lonely strata
# in this pipeline are created.
#
# The "Stratum (N) has only one PSU at stage 1" warnings this emits are the
# EXPECTED consequence of the policy, not a failure -- the survey package warns
# whenever it applies the adjustment. They are counted, not suppressed.
options(survey.lonely.psu = "adjust",
        survey.adjust.domain.lonely = TRUE)

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "H1_prep_mortality"


## ---- Helper: district predicted probability from a null 3-level model --------
district_estimates_glmer <- function(data, outcome,
                                     state = "state", district = "district",
                                     cluster = "clust") {
  d <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[state]]),
           !is.na(.data[[district]]), !is.na(.data[[cluster]])) %>%
    mutate(across(all_of(c(state, district, cluster)), as.factor))
  if (nrow(d) == 0 || length(unique(d[[outcome]])) < 2) {
    warning("Outcome '", outcome, "' unusable (empty or constant); returning NA.")
    return(tibble(district = character(), p_hat = numeric(), n = integer()))
  }
  fml <- as.formula(paste0(outcome, " ~ (1|", state, ") + (1|", district,
                           ") + (1|", cluster, ")"))
  m <- glmer(fml, data = d, family = binomial, nAGQ = 0,
             control = glmerControl(optimizer = "nloptwrap"))
  if (exists("chk_record_fit"))
    chk_record_fit(.chk_tag("H1_prep_mortality"),
                   paste0("district_estimates_glmer:", outcome), m,
                   extra = sprintf("districts=%d", nlevels(d[[district]])))
  re_d <- ranef(m)[[district]] %>% rownames_to_column(district) %>%
    rename(v = `(Intercept)`)
  re_s <- ranef(m)[[state]] %>% rownames_to_column(state) %>%
    rename(f = `(Intercept)`)
  d %>% count(.data[[state]], .data[[district]], name = "n") %>%
    rename(state = 1, district = 2) %>%
    left_join(re_d, by = setNames(district, "district")) %>%
    left_join(re_s, by = setNames(state, "state")) %>%
    mutate(p_hat = plogis(fixef(m)[["(Intercept)"]] + coalesce(v, 0) + coalesce(f, 0))) %>%
    select(district, p_hat, n)
}

## ---- Load KR, attach GPS, overlay onto NFHS-4 districts ----------------------
message("Reading KR files ...")
kr15 <- read_dta(path_kr_2015) %>% zap_labels()
kr19 <- read_dta(path_kr_2019) %>% zap_labels()

load(path_gps_2015); gps15 <- gps; rm(gps)
load(path_gps_2019); gps19 <- gps; rm(gps)

attach_geo <- function(kr, gps, shp) {
  kr <- kr %>%
    left_join(gps %>% select(cluster, LATNUM, LONGNUM),
              by = c("v001" = "cluster")) %>%
    filter(!is.na(LATNUM), !is.na(LONGNUM), LATNUM != 0, LONGNUM != 0)
  kr$.rid <- seq_len(nrow(kr))
  pts <- st_as_sf(kr, coords = c("LONGNUM", "LATNUM"),
                  crs = "+proj=longlat +datum=WGS84", remove = FALSE) %>%
    st_transform(st_crs(shp))

  # st_within can duplicate points that fall inside two overlapping polygons
  # -> deduplicate on the row id so exactly one district per point survives.
  j <- st_join(pts, shp %>% select(dist_code, state_name),
               join = st_within, left = TRUE) %>%
    st_drop_geometry() %>%
    arrange(.rid, dist_code) %>%
    distinct(.rid, .keep_all = TRUE)

  # nearest-district fallback for points just outside every polygon, but only up
  # to a 10 km cap (matching 01_prep_nfhs.R): a cluster whose nearest district is
  # more than 10 km away is left unassigned (dropped downstream) rather than
  # forced into a distant district.
  miss_ids <- j$.rid[is.na(j$dist_code)]
  if (length(miss_ids)) {
    miss_pts <- pts %>% filter(.rid %in% miss_ids)
    ni       <- st_nearest_feature(miss_pts, shp)
    dist_m   <- as.numeric(st_distance(miss_pts, shp[ni, ], by_element = TRUE))
    within   <- dist_m <= 10000
    nn <- tibble(.rid          = miss_pts$.rid,
                 dist_code_nn  = ifelse(within, shp$dist_code[ni],  NA),
                 state_name_nn = ifelse(within, shp$state_name[ni], NA)) %>%
      distinct(.rid, .keep_all = TRUE)
    j <- j %>%
      left_join(nn, by = ".rid") %>%
      mutate(dist_code  = coalesce(dist_code, dist_code_nn),
             state_name = coalesce(state_name, state_name_nn)) %>%
      select(-dist_code_nn, -state_name_nn)
    message("  ", sum(within), " of ", length(miss_ids),
            " displaced points assigned to nearest district within 10 km; ",
            sum(!within), " dropped (beyond cap).")
  }
  j %>%
    mutate(district = as.character(as.numeric(dist_code)),
           state = state_name,
           clust = paste0(v001, "_", v000)) %>%
    select(-.rid)
}

shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.numeric(dist_code)) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

message("Overlaying KR clusters onto NFHS-4 districts ...")
kr15 <- attach_geo(kr15, gps15, shp)
kr19 <- attach_geo(kr19, gps19, shp)

## ---- Recent-cohort windows ---------------------------------------------------
# MAIN: 120 months (10 yr) -- enough district-level events for the null multilevel
# smoother. Because NFHS-4 and NFHS-5 fieldwork are ~4 years apart, two 120-month
# windows overlap substantially in calendar time, so the "change" is a difference
# between two OVERLAPPING retrospective cohorts (reframed in the manuscript).
# SI (#16): a SHORT window whose two cohorts do NOT overlap in calendar time
# (chosen < the ~48-month inter-survey gap).
WIN_MONTHS       <- 120
WIN_MONTHS_SHORT <- 36

# Each mortality outcome is a COMPLETED-EXPOSURE COHORT PROBABILITY, not a raw
# death share: age_int = v008 - b3 is the child's age at interview (CMC dates); a
# birth counts toward an outcome only once observed for the full age interval
# (age_int >= 1 / 12) OR already dead within it, else it is NA-censored. The full
# birth history (living or dead, resident or not) is used to avoid co-residence
# selection bias. prep_kr computes the age/death fields WITHOUT the window; the
# window + its censoring are applied per-variant by apply_window(), so both the
# main and the nonoverlap cohorts can be built from the same frame.
prep_kr <- function(df) {
  df %>%
    mutate(
      rural     = as.integer(v025 == 2),
      age_int   = v008 - b3,
      .died_neo = as.integer(b5 == 0 & b7 == 0),
      .died_inf = as.integer(b5 == 0 & b7 <= 11),
      lpg          = as.integer(v161 == 2),
      hh_pollution = case_when(
        v161 %in% c(5, 6, 7, 8, 9, 10, 11) ~ 1L,
        v161 %in% c(1, 2, 4, 95) ~ 0L,
        TRUE ~ NA_integer_))
}
apply_window <- function(df, win) {
  df %>%
    mutate(
      in_window      = !is.na(age_int) & age_int >= 0 & age_int < win,
      neonatal_death = ifelse(in_window & (age_int >= 1  | .died_neo == 1), .died_neo, NA_integer_),
      infant_death   = ifelse(in_window & (age_int >= 12 | .died_inf == 1), .died_inf, NA_integer_)) %>%
    filter(in_window)
}
kr15 <- prep_kr(kr15)
kr19 <- prep_kr(kr19)

## ---- IR (women's file): district SES / household covariates (#17) -------------
# SES covariates come from the IR women's file (ONE row per woman), NOT the
# birth-level KR file -- so district prevalences are woman/household composition,
# not birth-weighted (which over-represents higher-parity women). Same validated
# recode codes as before; merged by district below. (HR would give strict
# household weighting; IR is used because its codes match the existing
# definitions and it carries the maternal-education item directly.)
ses_vars <- c("poor", "mother_low_edu", "electricity", "muslim",
              "improved_sanitation", "improved_water")
prep_ir <- function(df) {
  df %>% mutate(
    rural = as.integer(v025 == 2),
    poor            = if ("v190" %in% names(.)) as.integer(v190 == 1) else NA_integer_,
    mother_low_edu  = if ("v106" %in% names(.)) as.integer(v106 == 0) else NA_integer_,
    electricity     = if ("v119" %in% names(.)) as.integer(v119 == 1) else NA_integer_,
    muslim          = if ("v130" %in% names(.)) as.integer(v130 == 2) else NA_integer_,
    improved_sanitation = if ("v116" %in% names(.))
      as.integer(v116 %in% c(11,12,13,14,15,21,22)) else NA_integer_,
    improved_water = if ("v113" %in% names(.))
      as.integer(v113 %in% c(11,12,13,21,31,41,51,61,71)) else NA_integer_)
}
if (file.exists(path_ir_2015) && file.exists(path_ir_2019)) {
  message("Reading IR (women's) files for district SES covariates ...")
  ir15 <- prep_ir(attach_geo(zap_labels(read_dta(path_ir_2015)), gps15, shp))
  ir19 <- prep_ir(attach_geo(zap_labels(read_dta(path_ir_2019)), gps19, shp))
} else {
  warning("IR files not found -- SES covariates FALL BACK to the birth-level KR ",
          "file (birth-weighted). Edit path_ir_2015/2019 to use the woman-level ",
          "source (#17).")
  ir15 <- prep_ir(kr15); ir19 <- prep_ir(kr19)
}

## ---- Estimators --------------------------------------------------------------
# Multilevel district prevalence (PRIMARY), from a null 3-level model.
est_vars_year <- function(dat, vars, yr) {
  usable <- vars[vapply(vars, function(v)
    v %in% names(dat) && length(unique(na.omit(dat[[v]]))) >= 2, logical(1))]
  if (!length(usable)) return(tibble(district = character()))
  message("Year ", yr, " -- estimating: ", paste(usable, collapse = ", "))
  reduce(usable, function(acc, v) {
    e <- district_estimates_glmer(dat, v) %>%
      select(district, !!paste0(v, "_", yr) := p_hat)
    if (nrow(acc) == 0) e else full_join(acc, e, by = "district")
  }, .init = tibble())
}

# Eligible-birth counts per district per round -- the denominator that feeds the
# H2 district weights (#19).
birth_counts <- function(kr, yr)
  kr %>% filter(!is.na(district)) %>% count(district, name = paste0("n_births_", yr))

# DHS design-weighted DIRECT district mortality (SI sensitivity, #18): weights
# v005/1e6, PSU v001, Taylor-linearized SE. The multilevel estimates stay primary.
est_mort_designwt <- function(kr, yr) {
  d <- kr %>% mutate(.w = v005 / 1e6)
  out <- tibble(district = character())
  for (v in c("neonatal_death", "infant_death")) {
    dv <- d %>% filter(!is.na(.data[[v]]), !is.na(district), !is.na(v001),
                       is.finite(.w), .w > 0)
    if (nrow(dv) < 2) next
    dv$.y <- dv[[v]]
    e <- dv %>% as_survey_design(ids = v001, weights = .w) %>%
      group_by(district) %>% summarise(m = survey_mean(.y, na.rm = TRUE))
    names(e)[names(e) == "m"]    <- paste0(v, "_dw_", yr)
    names(e)[names(e) == "m_se"] <- paste0(v, "_dw_se_", yr)
    out <- if (nrow(out) == 0) e else full_join(out, e, by = "district")
  }
  out
}

mk_change <- function(df, stub) {
  a <- paste0(stub, "_2015"); b <- paste0(stub, "_2019")
  if (all(c(a, b) %in% names(df)))
    df[[paste0("change_", stub)]] <- 100 * (df[[b]] - df[[a]])
  df
}

## ---- Build + save one (population, window) variant ---------------------------
# Merges: KR multilevel mortality (primary) + IR-derived district SES + eligible-
# birth counts + design-weighted mortality (SI) + the harmonic-mean birth weight.
join_all <- function(parts) {
  parts <- Filter(function(x) "district" %in% names(x) && nrow(x) > 0, parts)
  if (!length(parts)) return(tibble(district = character()))
  Reduce(function(a, b) full_join(a, b, by = "district"), parts)
}
build_and_save <- function(pop, win, win_sfx = "") {
  sfx <- paste0(if (pop == "all") "_all" else "", win_sfx)
  keep_rural <- function(d) if (pop == "rural") dplyr::filter(d, rural == 1) else d
  k15 <- apply_window(keep_rural(kr15), win)
  k19 <- apply_window(keep_rural(kr19), win)
  i15 <- keep_rural(ir15); i19 <- keep_rural(ir19)
  message("\n== Population: ", pop, win_sfx, "  (win=", win, "mo; births ",
          nrow(k15), " / ", nrow(k19), ") ==")

  # Section 2.4.5 of the manuscript quotes these birth counts. Until now they
  # existed only in this message, which meant the paper carried a hand-typed
  # copy that a re-run could silently invalidate. Write them out instead, and
  # record BOTH the number of births in the observation window and the number
  # that a district assignment actually reached, because those differ (clusters
  # more than 10 km from any district polygon are left unassigned) and the two
  # are easy to conflate in the text.
  .assigned <- function(d) sum(!is.na(d$district))
  .bc <- tibble::tibble(
    population       = pop,
    window_months    = win,
    variant          = paste0(pop, win_sfx),
    year             = c(2015, 2019),
    n_births_window  = c(nrow(k15), nrow(k19)),
    n_births_district_assigned = c(.assigned(k15), .assigned(k19)),
    n_districts      = c(dplyr::n_distinct(stats::na.omit(k15$district)),
                         dplyr::n_distinct(stats::na.omit(k19$district))))
  .bc_file <- file.path(dir_out, "mortality_birth_counts.csv")
  if (file.exists(.bc_file) && exists(".bc_started", envir = globalenv())) {
    readr::write_csv(.bc, .bc_file, append = TRUE)
  } else {
    readr::write_csv(.bc, .bc_file)
    assign(".bc_started", TRUE, envir = globalenv())
  }

  hw <- join_all(list(
    est_vars_year(k15, c("neonatal_death", "infant_death", "lpg"), 2015),
    est_vars_year(k19, c("neonatal_death", "infant_death", "lpg"), 2019),
    est_vars_year(i15, ses_vars, 2015),
    est_vars_year(i19, ses_vars, 2019),
    birth_counts(k15, 2015), birth_counts(k19, 2019),
    est_mort_designwt(k15, 2015), est_mort_designwt(k19, 2019)))

  hw <- hw %>% left_join(distinct(k15, district, state), by = "district")
  for (s in c("neonatal_death", "infant_death", "lpg", ses_vars,
              "neonatal_death_dw", "infant_death_dw")) hw <- mk_change(hw, s)

  # District weight for H2 (#19): HARMONIC MEAN of eligible births across the two
  # rounds. This is a births-based analytic weight, NOT a population denominator.
  if (all(c("n_births_2015", "n_births_2019") %in% names(hw)))
    hw <- hw %>% mutate(w_births_hmean = ifelse(
      is.finite(n_births_2015) & is.finite(n_births_2019) &
        n_births_2015 > 0 & n_births_2019 > 0,
      2 / (1 / n_births_2015 + 1 / n_births_2019), NA_real_))

  saveRDS(hw, file.path(dir_out, paste0("health_district_wide", sfx, ".rds")))
  write_csv(hw, file.path(dir_out, paste0("health_district_wide", sfx, ".csv")))
  message("  H1 [", pop, win_sfx, "] -> health_district_wide", sfx, ".rds (",
          nrow(hw), " districts)")
}

## ---- Produce: rural main (+ all-hh SI), and rural nonoverlap SI ---------------
for (.p in POPS) build_and_save(.p, WIN_MONTHS)
build_and_save("rural", WIN_MONTHS_SHORT, "_nonoverlap")

## ---- CHECKS ------------------------------------------------------------------
chk_header("H1_prep_mortality")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("H1", "H1_prep_mortality")

.hw <- tryCatch(readRDS(file.path(dir_out, "health_district_wide.rds")),
                error = function(e) NULL)
chk("H1", "main health_district_wide.rds written", !is.null(.hw) && nrow(.hw) > 300,
    if (!is.null(.hw)) paste0(nrow(.hw), " districts") else "MISSING")
chk("H1", "mortality outcomes + change present",
    chk_has_cols(.hw, c("neonatal_death_2015","neonatal_death_2019",
      "infant_death_2015","infant_death_2019",
      "change_neonatal_death","change_infant_death")))
chk("H1", "SES covariates present (from IR women's file, #17)",
    chk_has_cols(.hw, c("poor_2019","mother_low_edu_2019","electricity_2019",
      "muslim_2019","improved_sanitation_2019","improved_water_2019")))
chk("H1", "eligible-birth counts + harmonic-mean weight present (#19)",
    chk_has_cols(.hw, c("n_births_2015","n_births_2019","w_births_hmean")))
# The birth counts quoted in Methods 2.4.5 must exist on disk for the manuscript
# builder to source them; a run that silently skipped this writer would leave the
# paper reading from its pinned fallback.
{
  .bcf <- file.path(dir_out, "mortality_birth_counts.csv")
  .bcd <- if (file.exists(.bcf)) readr::read_csv(.bcf, show_col_types = FALSE) else NULL
  chk("H1", "mortality birth-count table written",
      !is.null(.bcd) && nrow(.bcd) >= 4 &&
        all(c("n_births_window", "n_births_district_assigned") %in% names(.bcd)) &&
        all(.bcd$n_births_window > 0),
      if (is.null(.bcd)) "mortality_birth_counts.csv missing"
      else paste0(nrow(.bcd), " rows; variants: ",
                  paste(unique(.bcd$variant), collapse = ", ")))
}
chk("H1", "design-weighted mortality columns present (#18)",
    chk_has_cols(.hw, c("neonatal_death_dw_2015","infant_death_dw_2019")))
chk("H1", "nonoverlap-cohort SI file written (#16)",
    file.exists(file.path(dir_out, "health_district_wide_nonoverlap.rds")))
chk_warn("H1", "neonatal mortality prevalence plausible (<0.1)",
    chk_in_range(.hw$neonatal_death_2019, 0, 0.1),
    if (!is.null(.hw)) chk_rng(.hw$neonatal_death_2019) else "")
chk_warn("H1", "harmonic-mean birth weight positive & finite",
    chk_in_range(.hw$w_births_hmean, 1, 1e6),
    if (!is.null(.hw)) chk_rng(.hw$w_births_hmean) else "")
rm(.hw)
