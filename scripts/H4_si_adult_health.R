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
#       ambient covariates, unweighted (each district counted once), state-
#       clustered CR2 / Satterthwaite inference.
#
# Definitions (carried over from the deSouza 2025 analysis file; checked by
# verify_hypertension_selfreport.R and check_cvd_definitions.R):
#   HYPERTENSION = mean(2nd,3rd systolic) >= 140 OR mean(2nd,3rd diastolic) >= 90
#                  OR self-reported diagnosis of high blood pressure OR currently
#                  on blood-pressure medication. Broader than JNC7 (it counts
#                  self-reported diagnosis); NA whenever any of the readings,
#                  the diagnosis item or the medication item is missing.
#   DIABETES     = composite: self-reported diabetes OR measured glucose > 126
#                  mg/dL fasting (> 8 h) / > 200 mg/dL otherwise.
#   DIABETES_SR  = self-reported item only (sensitivity).
#   Sample: women 15-49 and men 15-54, not currently pregnant, resident >= 5 y.
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
if (!exists(".env_dir")) source("paths.R")   # path resolution; fails loudly if unconfigured
dir_out <- .dir_out()
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

path_cvd_load      <- file.path(.dir_dhs(), "cvd_load.RData")
path_districts_shp <- file.path(.dir_dhs(), "district_nfhs_shapefile/nfhs_data.shp")
path_corr          <- file.path(dir_out, "corrected_nfhs_districts.rds")
path_health        <- file.path(dir_out, paste0("health_district_wide", sfx, ".rds"))  # optional (SES)
path_dfwide        <- file.path(dir_out, "df_wide_health.rds")        # optional

# Names of the two person-level data frames inside cvd_load.RData.
OBJ_2015 <- "all_2015"
OBJ_2019 <- "all_2019"

# Derived indicators already present in cvd_load (confirmed via inspect_cvd.R).
# hypertension1_23 = mean of the 2ND & 3RD readings with systolic>=140 OR
# diastolic>=90, OR told high BP (self-reported diagnosis), OR on BP medication;
# missing if ANY of these components is missing. Verified against the raw
# recodes by verify_hypertension_selfreport.R (records/adult_outcomes/).
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
    mutate(eta = fixef(m)[["(Intercept)"]] + coalesce(v, 0) + coalesce(f, 0)) %>%
    { .sd <- as.numeric(attr(lme4::VarCorr(m)[[cluster]], "stddev"))
      .o <- mutate(., p_hat = marg_plogis(eta, .sd))
      log_marg(outcome, cluster, .sd, .o$p_hat, plogis(.o$eta)); .o } %>%
    select(district, p_hat, n)
}

## ---- Helper: attach dist_code geography to a person-level frame --------------
shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.numeric(dist_code)) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

snap_audit <- list()   # filled by attach_geo(); summarized in the CHECKS block

# District assignment: the cluster -> NFHS-4 district crosswalk written by
# 01_prep_nfhs.R (rule in geo_crosswalk.R), so adult outcomes are assigned to
# districts exactly as the exposure is. Point-in-polygon on the shapefile is not
# used (it disagrees with DHS's own NFHS-4 district code for ~28% of clusters).
XWALK <- readr::read_csv(file.path(dir_out, "nfhs_cluster_district_xwalk.csv"),
                         show_col_types = FALSE)
attach_geo <- function(df, tag = "", survey) {
  cl_num <- suppressWarnings(as.numeric(df$.cl))
  if (all(is.na(cl_num)))
    stop("H4: no numeric DHS cluster id (v001/hv001) in the ", tag, " frame, ",
         "so it cannot be joined to the district crosswalk", call. = FALSE)
  xw <- XWALK[XWALK$survey == survey, c("cluster", "district")]
  st <- shp %>% sf::st_drop_geometry() %>%
    transmute(dist_code = as.numeric(dist_code), state_name)
  df$.clnum <- cl_num
  df <- df %>% select(-any_of(c("district", "dist_code", "state_name", "state")))
  out <- df %>% left_join(xw, by = c(".clnum" = "cluster")) %>%
    rename(dist_code = district) %>% left_join(st, by = "dist_code")
  miss <- mean(is.na(out$dist_code))
  snap_audit[[length(snap_audit) + 1L]] <<- list(tag = tag, n_points = nrow(out),
    n_unmatched = sum(is.na(out$dist_code)))
  message(sprintf("  [%s] %.2f%% of records unmatched to the district crosswalk (dropped).", tag, 100 * miss))
  if (miss > 0.02) stop("H4: ", tag, ": more than 2% of records unmatched to the crosswalk", call. = FALSE)
  out %>% filter(!is.na(dist_code)) %>%
    mutate(district = as.character(as.numeric(dist_code)), state = state_name) %>%
    select(-.clnum)
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
  # Urban/rural, defensively: the frame is a row-bind of women's and men's
  # person files, so residence could in principle sit in v025 (women), mv025
  # (men) or hv025 (household join). Use whichever of these exist, row by row:
  # each respondent's flag is the first non-missing value, after checking the
  # coding and that overlapping sources agree. (Checked 2026-10-21: men are
  # present in the rural frame in proportion, and the adult outputs are
  # byte-identical to those produced before this guard; see
  # records/adult_outcomes/README.md.)
  # All three use the DHS coding 1 = urban, 2 = rural; checked here row by row:
  # every non-missing value must be 1 or 2, and wherever two sources are both
  # present for the same respondent they must agree. The flag is then the
  # row-wise first non-missing value.
  rur_src <- intersect(c("v025", "mv025", "hv025"), names(df))
  if (!length(rur_src)) stop("H4: no residence variable (v025/mv025/hv025) in the person-level frame", call. = FALSE)
  rv <- lapply(rur_src, function(v) suppressWarnings(as.numeric(unclass(df[[v]]))))
  names(rv) <- rur_src
  for (v in rur_src) {
    bad <- sum(!is.na(rv[[v]]) & !(rv[[v]] %in% c(1, 2)))
    if (bad) stop(sprintf("H4: %s has %d values outside the 1/2 (urban/rural) coding", v, bad), call. = FALSE)
  }
  if (length(rur_src) > 1) for (i in seq_along(rur_src)[-1]) for (j in seq_len(i - 1)) {
    a <- rv[[i]]; b <- rv[[j]]; both <- !is.na(a) & !is.na(b)
    dis <- sum(a[both] != b[both])
    message(sprintf("  residence: %s vs %s -- both present for %d rows, disagree %d", rur_src[i], rur_src[j], sum(both), dis))
    if (dis) stop(sprintf("H4: %s and %s disagree for %d respondents", rur_src[i], rur_src[j], dis), call. = FALSE)
  }
  r <- Reduce(dplyr::coalesce, rv)
  if (any(is.na(r))) message(sprintf("  residence missing in every source for %d respondents (excluded from the rural frame)", sum(is.na(r))))
  df$rural <- as.integer(r == 2)
  df
}

p15 <- derive_outcomes(p15); p19 <- derive_outcomes(p19)
# Sample accounting by sex at each stage (written to diagnostics/adult_sample_by_sex*.csv).
SAMPLE_LOG <- list()
log_stage <- function(d, yr, stage) {
  k <- if ("kind" %in% names(d)) as.character(d$kind) else rep("all", nrow(d))
  for (kk in sort(unique(k))) {
    dd <- d[k == kk, , drop = FALSE]
    SAMPLE_LOG[[length(SAMPLE_LOG) + 1]] <<- data.frame(year = yr, stage = stage, sex = kk, n = nrow(dd),
      n_hypertension = sum(!is.na(dd$hypertension)), n_diabetes = sum(!is.na(dd$diabetes)),
      n_diabetes_sr = sum(!is.na(dd$diabetes_sr)))
  }
}
log_stage(p15, 2015, "1 all respondents in cvd_load"); log_stage(p19, 2019, "1 all respondents in cvd_load")
for (nm in c("hypertension","diabetes","diabetes_sr")) {
  message(sprintf("  %s -- 2015 nonmissing=%d mean=%.3f | 2019 nonmissing=%d mean=%.3f",
                  nm, sum(!is.na(p15[[nm]])), mean(p15[[nm]], na.rm = TRUE),
                  sum(!is.na(p19[[nm]])), mean(p19[[nm]], na.rm = TRUE)))
}

# keep only rows contributing to at least one outcome, then overlay onto polygons
keep_rows <- function(df) df %>%
  filter(!is.na(hypertension) | !is.na(diabetes) | !is.na(diabetes_sr))
# DHS cluster number: v001 / hv001 if carried, else the leading digits of the
# `clust` key (built as paste0(hv001, hv000, survey) in the source frames).
cl <- function(df) {
  cand <- list()
  for (v in c("v001", "mv001", "hv001", "cluster"))
    if (v %in% names(df)) cand[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  if ("clust" %in% names(df))
    cand$clust <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", df$clust)))
  if (!length(cand)) stop("H4: no cluster identifier (v001/mv001/hv001/clust) in the person-level frame", call. = FALSE)
  Reduce(dplyr::coalesce, cand)   # first non-missing across sources (men carry mv001)
}
p15 <- keep_rows(p15); p19 <- keep_rows(p19)
p15$.cl <- cl(p15); p19$.cl <- cl(p19)
message("Assigning person records to NFHS-4 districts via the cluster crosswalk ...")
p15 <- attach_geo(p15, "NFHS-4 (2015)", "NFHS4"); p19 <- attach_geo(p19, "NFHS-5 (2019)", "NFHS5")
p15$clust <- paste0(p15$.cl, "_15")
p19$clust <- paste0(p19$.cl, "_19")
log_stage(p15, 2015, "2 contributing to an outcome, district assigned"); log_stage(p19, 2019, "2 contributing to an outcome, district assigned")
if (RURAL_ONLY) { p15 <- filter(p15, rural == 1); p19 <- filter(p19, rural == 1) }
log_stage(p15, 2015, if (RURAL_ONLY) "3 rural analysis frame" else "3 analysis frame (all households)")
log_stage(p19, 2019, if (RURAL_ONLY) "3 rural analysis frame" else "3 analysis frame (all households)")
SAMPLE_TAB <- do.call(rbind, SAMPLE_LOG)
dir.create(file.path(dir_out, "diagnostics"), showWarnings = FALSE)
write.csv(SAMPLE_TAB, file.path(dir_out, "diagnostics", paste0("adult_sample_by_sex", sfx, ".csv")), row.names = FALSE)
cat("\n== Adult sample by sex (n = respondents; n_<outcome> = non-missing outcome) ==\n")
print(SAMPLE_TAB, row.names = FALSE)
# Who is in the analysis frame, by sex: the rural restriction must keep men.
if ("kind" %in% names(p15)) {
  .by_kind <- function(d) paste(names(table(d$kind)), table(d$kind), sep = "=", collapse = ", ")
  message("  analysis frame by sex -- 2015: ", .by_kind(p15), " | 2019: ", .by_kind(p19))
}
# Men's share of the analysis frame must be comparable to their share before the
# residence restriction (rural and urban men are not rare), not merely non-zero.
men_share <- function(stage, yr) {
  d <- SAMPLE_TAB[SAMPLE_TAB$stage == stage & SAMPLE_TAB$year == yr, ]
  if (!"men" %in% d$sex) return(0)
  d$n[d$sex == "men"] / sum(d$n)
}
H4_MEN_SHARE <- sapply(c(2015, 2019), function(yr) c(
  before = men_share("2 contributing to an outcome, district assigned", yr),
  after  = men_share(SAMPLE_TAB$stage[grepl("^3 ", SAMPLE_TAB$stage)][1], yr)))
message(sprintf("  men's share before/after residence restriction: 2015 %.3f/%.3f | 2019 %.3f/%.3f",
                H4_MEN_SHARE[1, 1], H4_MEN_SHARE[2, 1], H4_MEN_SHARE[1, 2], H4_MEN_SHARE[2, 2]))
H4_MEN_OK <- !("kind" %in% names(p15)) ||
  all(H4_MEN_SHARE["after", ] > 0.5 * H4_MEN_SHARE["before", ] & H4_MEN_SHARE["before", ] > 0)
if (!H4_MEN_OK) stop("H4: men are under-represented in the analysis frame after the residence restriction ",
                     "(see the sample-by-sex table above); the adult results would be wrong.", call. = FALSE)

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

# CR2 (Bell-McCaffrey) with Satterthwaite df, the primary inference for every
# state-clustered regression in this paper (see H2_health_models.R and SI
# Methods S6). The adult outcomes are a secondary analysis, but they are
# clustered on the same ~30 states as the mortality models and so inherit the
# same anticonservatism in the conventional clustered z; reporting them under a
# different rule would make the two sets of tables incomparable.
CR2_OK       <- requireNamespace("clubSandwich", quietly = TRUE)
CR2_FAILURES <- 0L
# ---- ONE reference distribution for the interval and the p-value ------------
# These have to come from the same distribution, and for the coeftest arm they
# did not. lmtest::coeftest() on an lm object reports a t test on the residual
# degrees of freedom -- its columns are literally "t value" and "Pr(>|t|)" --
# while the interval here was built from a Normal quantile. At n = 607 with 589
# residual df the two quantiles are 1.959964 and 1.964000, a gap of 0.2%. That
# is invisible in a printed bound and decisive at the threshold: the
# all-household infant row has |t| = 1.963, which is p = 0.0496 under the Normal
# and p = 0.0501 under t(589). The reported p was the t one and the interval was
# the Normal one, so a row of the released table showed an interval excluding
# zero beside p = 0.0502.
#
# An earlier attempt at this replaced a hard-coded 1.96 with qnorm(0.975). That
# was a real fix for a real (3.6e-5) discrepancy, but it was not THIS one, and
# the check below is what caught the difference.
#
# Both quantities are now computed here from the residual df rather than read
# out of coeftest, so the reference distribution is visible at the call site
# instead of being a package default that a reader has to know.
#
# The multiple-imputation arm further down is a separate estimator and stays on
# the Normal for BOTH its interval and its p-value -- internally consistent, and
# in any case superseded by the CR2/Barnard-Rubin columns reported alongside it.
Z975 <- qnorm(0.975)   # MI arm only; the coeftest arm uses qt() on residual df

# Interval and p-value must agree about the null, in every row. This is the
# check that would have caught the 1.96 discrepancy above without anyone
# noticing it by hand.
.ci_p_consistent <- function(tab, lo, hi, p, label) {
  d <- tab[is.finite(tab[[lo]]) & is.finite(tab[[hi]]) & is.finite(tab[[p]]), , drop = FALSE]
  if (!nrow(d)) return(invisible(NULL))
  excl <- d[[lo]] * d[[hi]] > 0
  sig  <- d[[p]] < 0.05
  bad  <- which(excl != sig)
  chk("H4", sprintf("%s: every interval agrees with its own p-value about 0.05", label),
      length(bad) == 0,
      if (length(bad) == 0) sprintf("%d row(s) checked", nrow(d))
      else paste(sprintf("row %d: CI (%.6f, %.6f) vs p = %.4f", bad,
                         d[[lo]][bad], d[[hi]][bad], d[[p]][bad]), collapse = " | "))
  invisible(NULL)
}


fit_one <- function(dat, outcome, exposure, adjusted = TRUE) {
  if (!outcome %in% names(dat)) return(NULL)
  rhs <- c(exposure, if (adjusted) adj, if (has_region) "factor(region)")
  m <- lm(reformulate(rhs, response = outcome), data = dat)
  rows <- as.numeric(rownames(model.frame(m)))
  cl <- if (has_state) dat[["state"]][rows] else NULL
  vc <- if (has_state) sandwich::vcovCL(m, cluster = cl) else
    sandwich::vcovHC(m, "HC1")
  ct <- lmtest::coeftest(m, vcov. = vc); i <- match(exposure, rownames(ct))
  dfres <- df.residual(m)          # coeftest's own reference df for an lm
  tcrit <- qt(0.975, dfres)
  pval  <- 2 * pt(-abs(ct[i,1] / ct[i,2]), dfres)
  out <- tibble(term = exposure, estimate = ct[i,1], se = ct[i,2], p = pval,
                conf.low = ct[i,1] - tcrit*ct[i,2], conf.high = ct[i,1] + tcrit*ct[i,2],
                n = nobs(m),
                n_clusters = if (has_state) length(unique(cl)) else NA_integer_,
                se_cr2 = NA_real_, df_satt = NA_real_, p_cr2 = NA_real_,
                ci_cr2_low = NA_real_, ci_cr2_high = NA_real_)
  if (has_state && CR2_OK) {
    ok <- try({
      ct2 <- clubSandwich::coef_test(m, vcov = "CR2", cluster = cl,
                                     test = "Satterthwaite", coefs = exposure)
      ci2 <- clubSandwich::conf_int(m, vcov = "CR2", cluster = cl,
                                    test = "Satterthwaite", coefs = exposure)
      out$se_cr2 <- ct2$SE[1]; out$df_satt <- ct2$df_Satt[1]
      out$p_cr2  <- ct2$p_Satt[1]
      out$ci_cr2_low <- ci2$CI_L[1]; out$ci_cr2_high <- ci2$CI_U[1]
    }, silent = TRUE)
    if (inherits(ok, "try-error")) CR2_FAILURES <<- CR2_FAILURES + 1L
  }
  out
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
              lo_cr2_per10 = ci_cr2_low*10, hi_cr2_per10 = ci_cr2_high*10)
# Unrounded copy for the CI/p consistency check below. The p-values are exported
# unrounded, but the other model quantities are rounded to 4 dp, and a check on
# rounded values (p = 0.04996 -> 0.0500) would manufacture a
# disagreement with an interval that legitimately excludes zero -- at precisely
# the boundary the check is there to police. See the same note in H2.
results_unrounded <- results
results <- results %>%
  # Round once: the display columns are rounded again, to 3 dp, by the manuscript
  # builder. See the same note in H2_health_models.R -- rounding in both places
  # printed one bound as 0.074 in the main text and -0.073 in an SI table.
  mutate(across(!dplyr::any_of(c("est_per10", "lo_per10", "hi_per10",
                                 "conf.low", "conf.high",
                                 "ci_cr2_low", "ci_cr2_high",
                                 "lo_cr2_per10", "hi_cr2_per10",
                                 # p-values unrounded too (see H2): rounded once, by the builder
                                 "p", "p_cr0", "p_cr2", "p_wild")) &
                where(is.numeric), ~round(.x, 4)))
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
# The plotted interval is the PRIMARY inference (CR2, Satterthwaite df), the one
# Table S5 reports; the conventional lo_per10/hi_per10 interval is not drawn.
# replot_cr2_coefplots.R draws the same figure from the saved CSV.
if (!all(c("lo_cr2_per10", "hi_cr2_per10") %in% names(plot_df)) ||
    any(!is.finite(plot_df$lo_cr2_per10) | !is.finite(plot_df$hi_cr2_per10)))
  stop("[H4] CR2 interval missing for some adjusted rows; the coefficient plot ",
       "draws CR2 intervals only. Install clubSandwich and re-run.", call. = FALSE)
p <- ggplot(plot_df, aes(est_per10, exposure)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  geom_pointrange(aes(xmin = lo_cr2_per10, xmax = hi_cr2_per10)) +
  facet_wrap(~ outcome, ncol = 1, scales = "free_x") +
  theme_bw() +
  labs(x = "Change in adult prevalence (pp) per 10-pp rise in LPG main-fuel prevalence\n(point estimate and 95% CR2 interval)",
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
chk("H4", "analysis frame keeps men in proportion (share after residence restriction > half the share before)", isTRUE(H4_MEN_OK))
chk_file("H4", "adult prevalence table written",
    paste0("si_adult_health_prevalence", sfx, ".rds"))
chk_file("H4", "adult effects table written",
    paste0("si_adult_health_effects", sfx, ".csv"))
.ci_p_consistent(as.data.frame(results_unrounded), "lo_per10", "hi_per10", "p",
                 "conventional interval")

# --- district-assignment audit (cluster crosswalk, as in 01 and H1) ----------
snap_tbl <- dplyr::bind_rows(lapply(snap_audit, as_tibble))
write_csv(snap_tbl, file.path(dir_out, paste0("H4_snap_diagnostics", sfx, ".csv")))
cat("\n== H4 record-to-district assignment (cluster crosswalk) ==\n")
print(as.data.frame(snap_tbl), row.names = FALSE, digits = 3)
chk("H4", "every retained record carries a district code",
    all(!is.na(p15$district)) && all(!is.na(p19$district)))
chk_warn("H4", paste0("share of person-records unmatched to the district crosswalk below 0.5%", sfx),
    all(snap_tbl$n_unmatched / pmax(snap_tbl$n_points, 1) < 0.005),
    paste(sprintf("%s: %d/%d unmatched", snap_tbl$tag, snap_tbl$n_unmatched, snap_tbl$n_points),
          collapse = " | "))
chk("H4", "adjustment set excludes the weighted-temperature composite (collinear with temp_change)",
    !("weighted_temperature_change" %in% adj), paste(adj, collapse = ", "))
# Compare against the documented set itself (Methods 2.4.5), not against the
# same intersect() that built env_adj, so a covariate missing from df fails here.
chk("H4", "ambient adjustment set matches the documented H2/H5 set exactly",
    setequal(env_adj, c("change_pm","temp_change","rh_change","droughtchange")),
    paste0("realized: ", paste(env_adj, collapse = ", "), " | missing: ",
           paste(setdiff(c("change_pm","temp_change","rh_change","droughtchange"), env_adj),
                 collapse = ", ")))
chk("H4", "no clubSandwich (CR2) failures across the adult fits",
    CR2_OK && CR2_FAILURES == 0L, sprintf("clubSandwich available = %s; failures = %d", CR2_OK, CR2_FAILURES))

message("\nH4 done -> si_adult_health_prevalence.(rds/csv), si_adult_health_effects.csv, ",
        "si_adult_health_coefplot.jpeg, maps/SI_hypertension_2019.jpeg, ",
        "maps/SI_diabetes_2019.jpeg")
