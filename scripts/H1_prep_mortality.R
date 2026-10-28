# ==============================================================================
# H1_prep_mortality.R   (STANDALONE -- does not source 00_config.R)
#
# Builds district-level child-mortality estimates from the NFHS-4/5 Births
# Recode (BR) files (full birth histories), plus district SES/household covariates from the WOMEN's
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
if (!exists(".env_dir")) source("paths.R")   # path resolution; fails loudly if unconfigured
dir_out <- .dir_out()

# BIRTHS RECODE (BR): one row per birth in each woman's FULL birth history.
# The children's recode (KR) holds only births in the five years before the
# survey, so a 120-month window cannot be built from it (it silently becomes a
# ~59-month window). The object names below keep "kr" for continuity.
path_kr_2015 <- file.path(.dir_dhs(), "IABR74DT/IABR74FL.DTA")
path_kr_2019 <- file.path(.dir_dhs(), "India2019/IABR7DDT/IABR7DFL.DTA")
for (.p in c(path_kr_2015, path_kr_2019))
  if (!file.exists(.p)) stop("H1: births recode not found: ", .p,
                            " (the 120-month window needs the full birth history, not the KR file)", call. = FALSE)

# IR (women's recode) files -- district SES/household covariates are estimated
# from these (one row per woman), NOT from the birth-level KR file. Paths follow
# the DHS naming convention (IAIR* alongside IAKR*); VERIFY/EDIT to match your
# disk. If absent, SES falls back to the birth-level KR file with a warning.
path_ir_2015 <- file.path(.dir_dhs(), "IAIR74DT/IAIR74FL.DTA")
path_ir_2019 <- file.path(.dir_dhs(), "India2019/IAIR7DDT/IAIR7DFL.DTA")
# The DHS Program re-released the NFHS-5 IR recode as version 7E (2023); if the
# original 7D extract is absent but the 7E one is present, use it.
if (!file.exists(path_ir_2019)) {
  .ir7e <- c(file.path(.dir_dhs(), "India2019/IAIR7EDT/IAIR7EFL.DTA"),
             file.path(.dir_access(), "nfhs5_IR_7E/IAIR7EDT/IAIR7EFL.DTA"))
  .ir7e <- .ir7e[file.exists(.ir7e)]
  if (length(.ir7e)) path_ir_2019 <- .ir7e[1]
}

# Cluster GPS (same RData files used by 01_prep_nfhs.R): each has object `gps`
# with columns cluster, LATNUM, LONGNUM
path_gps_2015 <- file.path(.dir_dhs(), "India2015/gps_india.RData")
path_gps_2019 <- file.path(.dir_dhs(), "India2019/gps_india.RData")

# NFHS-4 district shapefile (the dist_code geography used throughout 01-07)
path_districts_shp <- file.path(.dir_dhs(), "district_pc11_shrug/nfhs_data.shp")   # SHRUG 2011 geometry; build_district_boundaries.R

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
# District prevalence = the share of the district's households (or births), i.e.
# the probability AVERAGED over the district's clusters, not the probability of
# a cluster whose random effect is exactly zero. On the logit scale these differ
# (the zero-effect value is the median cluster, pulled away from 0.5), and by how
# much depends on the survey's cluster variance -- so comparing two surveys'
# zero-effect values would mix a real prevalence gap with a difference in
# clustering. marg_plogis() integrates plogis(eta + u) over u ~ N(0, sd^2) by
# Gauss-Hermite quadrature (nodes from the Golub-Welsch eigenproblem, base R).
.gh_nodes <- local({
  n <- 40; i <- seq_len(n - 1)
  J <- matrix(0, n, n); J[cbind(i, i + 1)] <- J[cbind(i + 1, i)] <- sqrt(i / 2)
  e <- eigen(J, symmetric = TRUE)
  list(x = e$values, w = sqrt(pi) * e$vectors[1, ]^2)
})
# Per-fit record of cluster-averaged vs zero-cluster-effect prevalence (SI check),
# appended to the same file as the shared helper in 00_config.R writes.
log_marg <- function(outcome, cluster, sd_cluster, p_marg, p_cond) {
  try({
    rec <- data.frame(script = CHK_SCRIPT, outcome = outcome, cluster = cluster,
                      sd_cluster = sd_cluster, n_districts = length(p_marg),
                      mean_marginal = mean(p_marg, na.rm = TRUE),
                      mean_conditional = mean(p_cond, na.rm = TRUE),
                      mean_abs_diff = mean(abs(p_marg - p_cond), na.rm = TRUE))
    f <- file.path(dir_out, "glmer_cluster_marginalisation.csv")
    utils::write.table(rec, f, sep = ",", row.names = FALSE,
                       col.names = !file.exists(f), append = file.exists(f))
  }, silent = TRUE)
}
marg_plogis <- function(eta, sd) {
  if (!is.finite(sd) || sd <= 0) return(plogis(eta))
  P <- plogis(outer(eta, sqrt(2) * sd * .gh_nodes$x, "+"))
  as.numeric(P %*% .gh_nodes$w) / sqrt(pi)
}
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
    mutate(eta = fixef(m)[["(Intercept)"]] + coalesce(v, 0) + coalesce(f, 0)) %>%
    { .sd <- as.numeric(attr(lme4::VarCorr(m)[[cluster]], "stddev"))
      .o <- mutate(., p_hat = marg_plogis(eta, .sd))
      log_marg(outcome, cluster, .sd, .o$p_hat, plogis(.o$eta)); .o } %>%
    select(district, p_hat, n)
}

## ---- Load KR, attach GPS, overlay onto NFHS-4 districts ----------------------
message("Reading BR (births recode) files ...")
# Read only the columns this script uses. The full recodes are thousands of
# columns wide (the NFHS-5 IR file alone is 5.2 GB on disk) and reading them
# whole exhausts memory on a 32 GB laptop; haven's col_select reads just the
# named columns from disk.
DHS_KEEP <- c("caseid", "v000", "v001", "v002", "v003", "v005", "v008",
              "v021", "v022", "v023", "v024", "v025",
              "v106", "v113", "v116", "v119", "v130", "v161", "v190")
KR_BIRTH_PATTERN <- "^(b[0-9]+|bidx|bord|midx)$"   # birth-history items (BR/KR only)
read_dhs <- function(path, birth_cols = FALSE) {
  if (birth_cols)
    read_dta(path, col_select = c(any_of(DHS_KEEP), matches(KR_BIRTH_PATTERN))) %>% zap_labels()
  else
    read_dta(path, col_select = any_of(DHS_KEEP)) %>% zap_labels()
}
kr15 <- read_dhs(path_kr_2015, birth_cols = TRUE)
kr19 <- read_dhs(path_kr_2019, birth_cols = TRUE)

load(path_gps_2015); gps15 <- gps; rm(gps)
load(path_gps_2019); gps19 <- gps; rm(gps)

# District assignment uses the cluster -> NFHS-4 district crosswalk written by
# 01_prep_nfhs.R (nfhs_cluster_district_xwalk.csv; rule in geo_crosswalk.R), so
# births, women and households are assigned to districts exactly as the exposure
# is. Point-in-polygon on the shapefile is NOT used: it disagrees with DHS's own
# NFHS-4 district code for ~28% of clusters.
XWALK <- readr::read_csv(file.path(dir_out, "nfhs_cluster_district_xwalk.csv"),
                         show_col_types = FALSE)
attach_geo <- function(kr, gps, shp, survey) {
  xw <- XWALK[XWALK$survey == survey, c("cluster", "district")]
  st <- shp %>% sf::st_drop_geometry() %>%
    transmute(dist_code = as.numeric(dist_code), state_name)
  out <- kr %>%
    left_join(xw, by = c("v001" = "cluster")) %>%
    rename(dist_code = district) %>%
    left_join(st, by = "dist_code")
  miss <- mean(is.na(out$dist_code))
  message(sprintf("  [%s] %.2f%% of records have no district in the crosswalk (dropped).",
                  survey, 100 * miss))
  # the exclude_uncertain crosswalk variant removes clusters on purpose
  .miss_max <- if (tolower(Sys.getenv("CROSSWALK_VARIANT", "primary")) == "exclude_uncertain") 0.10 else 0.02
  if (miss > .miss_max) stop("H1: ", survey, ": more than " , 100 * .miss_max, "% of records unmatched to the district crosswalk", call. = FALSE)
  out %>%
    filter(!is.na(dist_code)) %>%
    mutate(district = as.character(as.numeric(dist_code)),
           state = state_name,
           clust = paste0(v001, "_", v000))
}

shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.numeric(dist_code)) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

message("Assigning births to NFHS-4 districts via the cluster crosswalk ...")
kr15 <- attach_geo(kr15, gps15, shp, "NFHS4")
kr19 <- attach_geo(kr19, gps19, shp, "NFHS5")

## ---- Recent-cohort windows ---------------------------------------------------
# MAIN: 120 months (10 yr) -- enough district-level events for the null multilevel
# smoother. Because NFHS-4 and NFHS-5 fieldwork are ~4 years apart, two 120-month
# windows overlap substantially in calendar time, so the "change" is a difference
# between two OVERLAPPING retrospective cohorts (reframed in the manuscript).
# SI (#16): a SHORT window, chosen shorter than the ~48-month inter-survey gap so
# the two cohorts overlap far less in calendar time; non-overlap is NOT enforced
# birth by birth.
WIN_MONTHS       <- 120
WIN_MONTHS_SHORT <- 36

# Each mortality outcome is a COMPLETED-EXPOSURE COHORT PROBABILITY, not a raw
# death share: age_int = v008 - b3 is the child's age at interview (CMC dates); a
# birth counts toward an outcome only once it has been exposed for the full age
# interval (age_int >= 2 for neonatal, >= 13 for infant, because CMC months are
# calendar months), whether it survived or
# died. Births still inside the interval at interview are censored (NA) whether
# alive or dead: keeping their deaths but not their survivors would bias the
# probability upward. The full
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
      # age_int is in whole calendar months (CMC), so age_int = 1 can mean a few
      # days old; >= 2 and >= 13 guarantee the full first month / year elapsed.
      neonatal_death = ifelse(in_window & age_int >= 2,  .died_neo, NA_integer_),
      infant_death   = ifelse(in_window & age_int >= 13, .died_inf, NA_integer_)) %>%
    filter(in_window)
}
kr15 <- prep_kr(kr15)
kr19 <- prep_kr(kr19)
# The file must reach back at least the main window, or the window is fictitious.
for (.k in list(kr15, kr19))
  if (max(.k$age_int, na.rm = TRUE) < WIN_MONTHS)
    stop("H1: the birth file reaches back only ", max(.k$age_int, na.rm = TRUE),
         " months, short of the ", WIN_MONTHS, "-month window", call. = FALSE)

## ---- IR (women's file): district SES / household covariates (#17) -------------
# SES covariates come from the IR women's file (ONE row per woman), NOT the
# birth-level KR file -- so district prevalences are woman/household composition,
# not birth-weighted (which over-represents higher-parity women). Same validated
# recode codes as before; merged by district below. (HR would give strict
# household weighting; IR is used because its codes match the existing
# definitions and it carries the maternal-education item directly.)
ses_vars <- c("poor", "mother_low_edu", "electricity", "muslim",
              "improved_sanitation", "improved_water")
# Code lists follow the NFHS improved-source definitions and each round's own
# recode map (IAIR74FL / IAIR7*FL .MAP), which number the water sources
# differently: NFHS-4 13 = public tap, 72 = community RO plant; NFHS-5 13 = piped
# to neighbour, 14 = public tap, 92 = community RO plant. Improved water: piped,
# public tap, tube well, protected well/spring, rainwater, tanker, cart with
# small tank, bottled water, community RO plant. Improved sanitation: flush to
# sewer/septic tank/pit or don't-know-where, VIP, pit latrine with slab,
# composting toilet (flush "somewhere else", 14, is unimproved). "Not a de jure
# resident" (97 / 7) and missing (99 / 9) are NA, not 0.
WATER_IMPROVED <- list(`2015` = c(11, 12, 13, 21, 31, 41, 51, 61, 62, 71, 72),
                       `2019` = c(11, 12, 13, 14, 21, 31, 41, 51, 61, 62, 71, 92))
SAN_IMPROVED   <- c(11, 12, 13, 15, 21, 22, 41)
code01 <- function(x, yes, na_codes) ifelse(is.na(x) | x %in% na_codes, NA_integer_,
                                            as.integer(x %in% yes))
prep_ir <- function(df, year) {
  yr <- as.character(year)
  df %>% mutate(
    rural = as.integer(v025 == 2),
    poor            = if ("v190" %in% names(.)) as.integer(v190 == 1) else NA_integer_,
    mother_low_edu  = if ("v106" %in% names(.)) code01(v106, 0, 9) else NA_integer_,
    electricity     = if ("v119" %in% names(.)) code01(v119, 1, c(7, 9)) else NA_integer_,
    muslim          = if ("v130" %in% names(.)) as.integer(v130 == 2) else NA_integer_,
    improved_sanitation = if ("v116" %in% names(.))
      code01(v116, SAN_IMPROVED, c(97, 99)) else NA_integer_,
    improved_water = if ("v113" %in% names(.))
      code01(v113, WATER_IMPROVED[[yr]], c(97, 99)) else NA_integer_)
}
if (file.exists(path_ir_2015) && file.exists(path_ir_2019)) {
  message("Reading IR (women's) files for district SES covariates ...")
  ir15 <- prep_ir(attach_geo(read_dhs(path_ir_2015), gps15, shp, "NFHS4"), 2015); invisible(gc())
  ir19 <- prep_ir(attach_geo(read_dhs(path_ir_2019), gps19, shp, "NFHS5"), 2019); invisible(gc())
} else {
  warning("IR files not found -- SES covariates FALL BACK to the birth-level KR ",
          "file (birth-weighted). Edit path_ir_2015/2019 to use the woman-level ",
          "source (#17).")
  ir15 <- prep_ir(kr15, 2015); ir19 <- prep_ir(kr19, 2019)
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
# Outcome-specific ELIGIBLE births: births in the window that were exposed for
# the outcome's full age interval (age_int >= 2 neonatal, >= 13 infant), i.e. the
# births that actually enter that outcome's probability. Births still inside the
# interval at interview are censored to NA by apply_window() and are NOT counted.
elig_counts <- function(kr, yr)
  kr %>% filter(!is.na(district)) %>% group_by(district) %>%
    summarise(!!paste0("n_elig_neonatal_", yr) := sum(!is.na(neonatal_death)),
              !!paste0("n_elig_infant_", yr)   := sum(!is.na(infant_death)),
              .groups = "drop")

# DHS design-weighted DIRECT district mortality (SI sensitivity, #18): weights
# v005/1e6, PSU v021 (v001 where absent), DHS sample stratum v022 (v023 where
# absent), Taylor-linearized SE. The multilevel estimates stay primary.
#
# The stratum used to be missing here. SI Section S1 describes the design-based
# estimator as carrying the DHS sampling strata with single-PSU strata centred at
# the overall mean, and 00_config.R's district_estimates_designwt() does exactly
# that on the EXPOSURE side -- but this function, the mortality side, declared
# only weights and PSUs. The paper therefore described one estimator and computed
# another, and the omitted stratification inflates the standard errors it
# reports (stratification removes between-stratum variance from the estimate of
# sampling variance). options(survey.lonely.psu = "adjust") is already set at the
# top of this script, which is the centring the SI describes; nest = TRUE because
# DHS PSU numbers restart within strata.
STRAT_COL <- NULL
est_mort_designwt <- function(kr, yr) {
  d <- kr %>% mutate(.w = v005 / 1e6)
  psu_col <- if ("v021" %in% names(d)) "v021" else "v001"
  str_col <- if ("v022" %in% names(d) && dplyr::n_distinct(d$v022, na.rm = TRUE) > 1) "v022"
             else if ("v023" %in% names(d) && dplyr::n_distinct(d$v023, na.rm = TRUE) > 1) "v023"
             else NA_character_
  STRAT_COL <<- str_col
  if (is.na(str_col))
    warning("H1: no usable DHS stratum (v022/v023) in the ", yr, " KR file, so the ",
            "design-weighted mortality sensitivity is PSU-clustered but UNSTRATIFIED. ",
            "SI Section S1 describes it as stratified -- fix one or the other.",
            call. = FALSE)
  else
    message("  design-weighted mortality ", yr, ": ids = ", psu_col,
            ", strata = ", str_col, ", weights = v005/1e6")
  out <- tibble(district = character())
  for (v in c("neonatal_death", "infant_death")) {
    dv <- d %>% filter(!is.na(.data[[v]]), !is.na(district), !is.na(.data[[psu_col]]),
                       is.finite(.w), .w > 0)
    if (!is.na(str_col)) dv <- dv %>% filter(!is.na(.data[[str_col]]))
    if (nrow(dv) < 2) next
    dv$.y <- dv[[v]]
    des <- if (is.na(str_col))
      dv %>% as_survey_design(ids = !!sym(psu_col), weights = .w)
    else
      dv %>% as_survey_design(ids = !!sym(psu_col), strata = !!sym(str_col),
                              weights = .w, nest = TRUE)
    e <- des %>%
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
  # that a district assignment actually reached, because those differ (births in
  # clusters that the district crosswalk, geo_crosswalk.R, does not assign are
  # left unassigned) and the two are easy to conflate in the text.
  .assigned <- function(d) sum(!is.na(d$district))
  .bc <- tibble::tibble(
    population       = pop,
    window_months    = win,
    variant          = paste0(pop, win_sfx),
    year             = c(2015, 2019),
    n_births_window  = c(nrow(k15), nrow(k19)),
    n_eligible_neonatal = c(sum(!is.na(k15$neonatal_death)), sum(!is.na(k19$neonatal_death))),
    n_eligible_infant   = c(sum(!is.na(k15$infant_death)),   sum(!is.na(k19$infant_death))),
    n_births_district_assigned = c(.assigned(k15), .assigned(k19)),
    n_districts      = c(dplyr::n_distinct(stats::na.omit(k15$district)),
                         dplyr::n_distinct(stats::na.omit(k19$district))))
  .bc_file <- file.path(dir_out, "mortality_birth_counts.csv")
  if (file.exists(.bc_file) && exists(".bc_started", envir = globalenv())) {
    # Replace, never duplicate, the rows of a variant re-run in the same session.
    .old <- readr::read_csv(.bc_file, show_col_types = FALSE)
    .old <- .old[!(.old$variant %in% .bc$variant), , drop = FALSE]
    readr::write_csv(dplyr::bind_rows(.old, .bc), .bc_file)
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
    elig_counts(k15, 2015), elig_counts(k19, 2019),
    est_mort_designwt(k15, 2015), est_mort_designwt(k19, 2019)))

  hw <- hw %>% left_join(distinct(k15, district, state), by = "district")
  for (s in c("neonatal_death", "infant_death", "lpg", ses_vars,
              "neonatal_death_dw", "infant_death_dw")) hw <- mk_change(hw, s)

  # District weights for H2 (#19): HARMONIC MEAN across the two rounds of the
  # births ELIGIBLE FOR EACH OUTCOME (w_hmean_neonatal, w_hmean_infant), so the
  # weight matches the denominator of the outcome it weights. A births-based
  # analytic (approximate precision) weight, NOT a population denominator.
  # w_births_hmean, the same quantity on ALL births in the window (including
  # births not yet exposed for the full interval), is kept for reference only;
  # no model uses it.
  hmean2 <- function(a, b) ifelse(is.finite(a) & is.finite(b) & a > 0 & b > 0,
                                  2 / (1 / a + 1 / b), NA_real_)
  hw <- hw %>% mutate(
    w_births_hmean   = hmean2(n_births_2015, n_births_2019),
    w_hmean_neonatal = hmean2(n_elig_neonatal_2015, n_elig_neonatal_2019),
    w_hmean_infant   = hmean2(n_elig_infant_2015,   n_elig_infant_2019))
  message(sprintf("  H1 [%s%s] weights: cor(all-births, neonatal-eligible) = %.4f; cor(all-births, infant-eligible) = %.4f; infant-eligible / all births = %.3f",
                  pop, win_sfx,
                  stats::cor(hw$w_births_hmean, hw$w_hmean_neonatal, use = "complete.obs"),
                  stats::cor(hw$w_births_hmean, hw$w_hmean_infant, use = "complete.obs"),
                  sum(hw$n_elig_infant_2015, hw$n_elig_infant_2019, na.rm = TRUE) /
                    sum(hw$n_births_2015, hw$n_births_2019, na.rm = TRUE)))

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
    chk_has_cols(.hw, c("n_births_2015","n_births_2019","w_births_hmean",
      "n_elig_neonatal_2015","n_elig_neonatal_2019","n_elig_infant_2015","n_elig_infant_2019",
      "w_hmean_neonatal","w_hmean_infant")))
chk("H1", "eligible births never exceed births in the window",
    !is.null(.hw) && all(.hw$n_elig_infant_2015 <= .hw$n_births_2015, na.rm = TRUE) &&
      all(.hw$n_elig_infant_2019 <= .hw$n_births_2019, na.rm = TRUE) &&
      all(.hw$n_elig_neonatal_2015 <= .hw$n_births_2015, na.rm = TRUE) &&
      all(.hw$n_elig_neonatal_2019 <= .hw$n_births_2019, na.rm = TRUE))
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
chk_warn("H1", "outcome-specific harmonic-mean birth weights positive & finite",
    chk_in_range(.hw$w_hmean_neonatal, 1, 1e6) && chk_in_range(.hw$w_hmean_infant, 1, 1e6),
    if (!is.null(.hw)) paste(chk_rng(.hw$w_hmean_neonatal), "|", chk_rng(.hw$w_hmean_infant)) else "")
rm(.hw)
