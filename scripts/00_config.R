# ==============================================================================
# 00_config.R
# Shared configuration, paths, libraries, and helper functions
#
# Project: Cross-survey validation and calibration of NFHS district
#          cooking-fuel estimates against dedicated energy surveys (ACCESS,
#          IRES) and the NSSO-78 Multiple Indicator Survey; prediction of
#          fuel-use detail absent from NFHS; and evaluation of the
#          consequences of exposure measurement error for district-level
#          health analyses.
#
# Run order: run_everything.R defines the canonical dependency order for the
# full pipeline (data prep -> cross-survey comparison incl. NSSO-78 ->
# correction -> augmentation -> health analyses -> figures/SI -> manuscript).
# Individual scripts can be re-run independently once their upstream outputs
# exist in `dir_out` (they save intermediate .rds files there).
# ==============================================================================

# ---- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)          # read_dta
  library(readstata13)    # read.dta13 (factor handling for ACCESS appended file)
  library(sf)
  library(lme4)           # glmer
  library(survey)
  library(srvyr)
  library(WeMix)          # weighted mixed models (sensitivity)
  library(broom)
  library(broom.mixed)
})
# Used only in specific scripts (loaded there, listed here for install):
#   05: brms
#   06: nnet (multinomial), pROC (AUC)

# Survey-design settings: strata with a single PSU (common after subsetting,
# e.g. rural-only) otherwise throw "Stratum has only one PSU at stage 1".
# 'adjust' centers the lonely PSU at the overall mean for variance estimation.
options(survey.lonely.psu = "adjust",
        survey.adjust.domain.lonely = TRUE)

# ---- Paths ---------------------------------------------------------------------
# Set via environment variables (preferred -- nothing to edit):
#   DHS_DIR     folder with the DHS/NFHS extracts and district shapefile
#   ACCESS_DIR  folder with the ACCESS replication archive and ires/ subfolder
#   OUTPUT_DIR  where every intermediate and final output is written
#               (defaults to ACCESS_DIR, matching the original layout)
# The literal defaults below are the authors' local layout, kept so the
# original runs remain reproducible; on any other machine set the variables.
if (!exists(".env_dir")) source("paths.R")   # path resolution; fails loudly if unconfigured
dir_dhs      <- .dir_dhs()
dir_access   <- .dir_access()
dir_out      <- Sys.getenv("OUTPUT_DIR", dir_access)

path_cvd_load        <- file.path(dir_dhs, "cvd_load.RData")
path_image2015       <- file.path(dir_dhs, "image2015.RData")
path_image2019       <- file.path(dir_dhs, "image2019.RData")
path_gps_2015        <- file.path(dir_dhs, "India2015/gps_india.RData")
path_gps_2019        <- file.path(dir_dhs, "India2019/gps_india.RData")
# District boundaries: SHRUG 2.2 2011 Census geometry with the original attribute
# table (built by build_district_boundaries.R). The original file
# district_nfhs_shapefile/nfhs_data.shp is kept only as the attribute source:
# its shapes do not match its labels (30_crosswalk_validation.R, part D).
path_districts_shp   <- file.path(dir_dhs, "district_pc11_shrug/nfhs_data.shp")
path_pc11_districts  <- Sys.getenv("PC11_DISTRICT_SHP",
                          file.path(dir_dhs, "shrug_pc11_district_polygons/district.shp"))
# NFHS-5 -> 2011 district crosswalk. "V2" (primary): documented district history
# + 2011 boundaries; "V1": the original name-match / nearest-neighbour rule.
CROSSWALK_VERSION    <- toupper(Sys.getenv("CROSSWALK_VERSION", "V2"))
# Version-2 sensitivity variants (geo_crosswalk.R): primary | alternative_assignment |
# exclude_uncertain | alternative_history | overlay_history. Run each into its own OUTPUT_DIR.
CROSSWALK_VARIANT    <- tolower(Sys.getenv("CROSSWALK_VARIANT", "primary"))
path_district_history <- normalizePath(file.path(getwd(), "..", "data", "district_history_nfhs5.csv"),
                                       mustWork = FALSE)

path_access_appended <- file.path(dir_access, "dataverse/CEEWACCESS20152018_Appended.dta")
path_access_weights  <- file.path(dir_access, "dataverse/weights.dta")   # year, m1_q11_village_code, weights
path_ires            <- file.path(dir_access, "ires/IRES.dta")

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# Warn early, with instructions, if an input folder is absent -- a missing
# path should name the environment variable that fixes it, not surface later
# as an unexplained read error deep inside a prep script.
.input_dirs <- c(DHS_DIR = dir_dhs, ACCESS_DIR = dir_access)
for (.v in names(.input_dirs)) {
  if (!dir.exists(path.expand(.input_dirs[[.v]])))
    warning("Input folder not found: ", .input_dirs[[.v]],
            "\n  Set the ", .v, " environment variable to your local copy ",
            "(see README).", call. = FALSE, immediate. = TRUE)
}

# ---- Global switches ----------------------------------------------------------
RURAL_ONLY_NFHS <- TRUE    # ACCESS is rural-only; restrict NFHS to rural clusters
                           # for the head-to-head comparison. District estimates
                           # are produced both ways in 01_prep_nfhs.R regardless.
LPG_KG_LARGE    <- 14.2    # kg per large cylinder
LPG_KG_SMALL    <- 5.0     # kg per small cylinder

# The six ACCESS states, by NFHS-4 and NFHS-5 state codes
ACCESS_STATES_2015 <- c(5, 15, 19, 26, 33, 35)   # NFHS-4 hv024 codes
ACCESS_STATES_2019 <- c(10, 20, 23, 21, 9, 19)   # NFHS-5 hv024 codes
ACCESS_STATE_NAMES <- c("Bihar", "Jharkhand", "Madhya Pradesh",
                        "Odisha", "Uttar Pradesh", "West Bengal")

# ==============================================================================
# Helper functions
# ==============================================================================

#' Check that required input files exist before a script starts; if not, stop
#' with a message naming the script that produces each missing file.
#' Usage: need_inputs(c("nfhs_districts.rds" = "01_prep_nfhs.R", ...))
need_inputs <- function(inputs) {
  paths <- file.path(dir_out, names(inputs))
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop("Missing input file(s) in ", dir_out, ":\n",
         paste0("  ", names(inputs)[missing], "  -> run ",
                inputs[missing], " first", collapse = "\n"),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Fit a null 3-level logistic model and return district-level predicted
#' probabilities (fixed intercept + state RE + district RE), i.e. exactly the
#' approach used throughout this project, wrapped so all surveys are processed identically.
#'
#' @param data     data.frame with outcome and grouping columns
#' @param outcome  character, name of 0/1 outcome column
#' @param state, district, cluster  character, names of grouping columns
#' Row-wise "any component == 1", returning NA only when ALL are NA.
#' (base pmax(..., na.rm = TRUE) returns -Inf when every component is NA, which
#'  silently breaks any_solid == 0/1 logic; this returns a clean 0/1/NA.)
row_any1 <- function(...) {
  M <- cbind(...)
  ifelse(rowSums(!is.na(M)) == 0, NA_integer_,
         as.integer(rowSums(M == 1L, na.rm = TRUE) > 0))
}

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
marg_plogis <- function(eta, sd) {
  if (!is.finite(sd) || sd <= 0) return(plogis(eta))
  P <- plogis(outer(eta, sqrt(2) * sd * .gh_nodes$x, "+"))
  as.numeric(P %*% .gh_nodes$w) / sqrt(pi)
}

#' @return tibble: district, state, p_hat (district-level probability), n_hh
district_estimates_glmer <- function(data, outcome,
                                     state = "state", district = "district",
                                     cluster = "clust") {
  data <- data %>%
    filter(!is.na(.data[[outcome]]),
           !is.na(.data[[state]]), !is.na(.data[[district]]),
           !is.na(.data[[cluster]])) %>%
    mutate(across(all_of(c(state, district, cluster)), as.factor))

  if (nrow(data) == 0)
    stop("district_estimates_glmer: 0 rows remain for outcome '", outcome,
         "' after dropping missing outcome/grouping values. ",
         "Check that the outcome and grouping columns are populated.")

  # Crossed random effects (1|district) are only valid if district codes are
  # globally unique (not reused across states). NFHS-4 dist_code is a national
  # 2011-census code, so this should hold; assert it rather than assume.
  dup_ds <- data %>% st_drop_geometry_safe() %>%
    distinct(.data[[district]], .data[[state]]) %>%
    count(.data[[district]]) %>% filter(n > 1)
  if (nrow(dup_ds) > 0)
    stop("district_estimates_glmer: ", nrow(dup_ds), " district code(s) appear ",
         "in more than one state; random effects would be misspecified. ",
         "Nest district within state (interaction) before fitting.")

  fml <- as.formula(paste0(outcome, " ~ (1|", state, ") + (1|", district,
                           ") + (1|", cluster, ")"))
  m <- glmer(fml, data = data, family = binomial,
             nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))

  # Record the fit (singularity, variance components, convergence messages) in
  # diagnostics/model_fits.csv. Attributed to the CALLING script via CHK_SCRIPT,
  # since this helper is shared by 01/02/03/08/09. Guarded so the helper still
  # works if checks.R was not sourced.
  if (exists("chk_record_fit"))
    chk_record_fit(.chk_tag(), paste0("district_estimates_glmer:", outcome), m,
                   extra = sprintf("districts=%d", nlevels(data[[district]])))

  sd_clust <- as.numeric(attr(lme4::VarCorr(m)[[cluster]], "stddev"))
  re_d <- ranef(m)[[district]] %>%
    rownames_to_column(district) %>% rename(v = `(Intercept)`)
  re_s <- ranef(m)[[state]] %>%
    rownames_to_column(state) %>% rename(f = `(Intercept)`)

  out <- data %>%
    st_drop_geometry_safe() %>%
    count(.data[[state]], .data[[district]], name = "n_hh") %>%
    rename(state = 1, district = 2) %>%
    left_join(re_d, by = setNames(district, "district")) %>%
    left_join(re_s, by = setNames(state, "state")) %>%
    mutate(eta = fixef(m)[["(Intercept)"]] + v + f,
           p_hat = marg_plogis(eta, sd_clust)) %>%
    select(state, district, n_hh, p_hat, eta)
  # conditional (cluster effect = 0) value kept as an attribute for the SI check
  attr(out, "p_hat_conditional") <- plogis(out$eta)
  attr(out, "sd_cluster") <- sd_clust
  # Record, per fit, how far the cluster-averaged prevalence sits from the
  # zero-cluster-effect value (reported in the SI; differs by survey).
  try({
    rec <- data.frame(script = if (exists(".chk_tag")) .chk_tag() else NA_character_,
                      outcome = outcome, cluster = cluster, sd_cluster = sd_clust,
                      n_districts = nrow(out),
                      mean_marginal = mean(out$p_hat, na.rm = TRUE),
                      mean_conditional = mean(plogis(out$eta), na.rm = TRUE),
                      mean_abs_diff = mean(abs(out$p_hat - plogis(out$eta)), na.rm = TRUE))
    f <- file.path(dir_out, "glmer_cluster_marginalisation.csv")
    utils::write.table(rec, f, sep = ",", row.names = FALSE,
                       col.names = !file.exists(f), append = file.exists(f))
  }, silent = TRUE)
  out$eta <- NULL

  attr(out, "model") <- m
  out
}

#' Design-weighted district estimates (sensitivity analysis).
#' Direct survey-weighted district means with SEs -- the design-based analogue
#' of the model-based estimates above. For small-sample districts this is
#' noisier than the glmer partial-pooling estimate, which is *why* we keep the
#' multilevel version as the main spec.
#'
#' @param data data.frame; @param outcome 0/1 outcome column name
#' @param ids,strata,weights column names for the survey design
#' @param domain_col optional 0/1 column: build the design on the FULL data,
#'   then restrict to domain_col == 1 *inside* the design (correct domain
#'   estimation; avoids creating single-PSU strata by pre-subsetting).
district_estimates_weighted <- function(data, outcome, ids, strata = NULL, weights,
                                        district = "district", nest = TRUE,
                                        domain_col = NULL) {
  d0 <- data %>%
    st_drop_geometry_safe() %>%
    filter(!is.na(.data[[weights]]), .data[[weights]] > 0)
  # strata optional: ACCESS/IRES are village-PSU designs with no household-level
  # stratum available, so we build an unstratified village-clustered design
  # rather than mis-declaring households as PSUs and villages as strata.
  des <- if (is.null(strata)) {
    d0 %>% as_survey_design(ids = !!sym(ids), weights = !!sym(weights))
  } else {
    d0 %>% as_survey_design(ids = !!sym(ids), strata = !!sym(strata),
                            weights = !!sym(weights), nest = nest)
  }
  if (!is.null(domain_col)) des <- des %>% filter(.data[[domain_col]] == 1)
  des %>%
    filter(!is.na(.data[[outcome]])) %>%
    group_by(district = .data[[district]]) %>%
    summarise(p_wt = survey_mean(.data[[outcome]], na.rm = TRUE, vartype = "se"))
}

#' Weighted mixed model district estimates via WeMix (second sensitivity).
#' WeMix needs level-specific weights; we follow the common approximation of
#' household weight at level 1 and weight 1 at higher levels.
district_estimates_wemix <- function(data, outcome, state = "state",
                                     district = "district", weights_col = "wt") {
  data <- data %>%
    st_drop_geometry_safe() %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[weights_col]])) %>%
    mutate(w1 = .data[[weights_col]], w2 = 1, w3 = 1,
           across(all_of(c(state, district)), as.factor))
  fml <- as.formula(paste0(outcome, " ~ (1|", district, ") + (1|", state, ")"))
  m <- WeMix::mix(fml, data = data, weights = c("w1", "w2", "w3"),
                  family = binomial(link = "logit"))
  # WeMix returns BLUPs in m$ranefMat
  re_d <- data.frame(district = rownames(m$ranefMat[[district]]),
                     v = m$ranefMat[[district]][, 1])
  re_s <- data.frame(state = rownames(m$ranefMat[[state]]),
                     f = m$ranefMat[[state]][, 1])
  data %>%
    distinct(state = .data[[state]], district = .data[[district]]) %>%
    left_join(re_d, by = "district") %>%
    left_join(re_s, by = "state") %>%
    mutate(p_hat_wt = plogis(m$coef[1] + v + f)) %>%
    select(state, district, p_hat_wt)
}

#' Drop sf geometry if present (lets helpers accept sf or plain data.frames)
st_drop_geometry_safe <- function(x) {
  if (inherits(x, "sf")) sf::st_drop_geometry(x) else x
}

#' Weighted Pearson correlation, used for district sample-size-weighted
#' agreement (districts weighted by the reference survey's household count)
weighted_cor <- function(x, y, w) {
  ok <- complete.cases(x, y, w)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  w <- w / sum(w)
  mx <- sum(w * x); my <- sum(w * y)
  sum(w * (x - mx) * (y - my)) /
    sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
}

#' Lin's concordance correlation coefficient (agreement, not just correlation)
ccc <- function(x, y) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  2 * cov(x, y) / (var(x) + var(y) + (mean(x) - mean(y))^2)
}

#' Bland-Altman plot for two sets of district proportions
bland_altman_plot <- function(df, est1, est2, label1, label2,
                              color_by = NULL, weight = NULL) {
  d <- df %>%
    mutate(avg  = (.data[[est1]] + .data[[est2]]) / 2,
           diff = .data[[est1]] - .data[[est2]])
  md  <- mean(d$diff, na.rm = TRUE)
  sdd <- sd(d$diff, na.rm = TRUE)
  p <- ggplot(d, aes(x = avg, y = diff)) +
    geom_hline(yintercept = md, linetype = 1) +
    geom_hline(yintercept = md + c(-1.96, 1.96) * sdd, linetype = 2) +
    labs(x = paste0("Mean of ", label1, " and ", label2),
         y = paste0(label1, " - ", label2),
         subtitle = sprintf("Mean diff = %.3f; 95%% LoA = [%.3f, %.3f]",
                            md, md - 1.96 * sdd, md + 1.96 * sdd)) +
    theme_bw()
  if (!is.null(color_by)) {
    p <- p + geom_point(aes(color = .data[[color_by]],
                            size = if (!is.null(weight)) .data[[weight]] else NULL),
                        alpha = 0.8) + labs(color = "State", size = "Households")
  } else {
    p <- p + geom_point(alpha = 0.8)
  }
  p
}

# Pipeline self-check helpers (chk / chk_warn / chk_header / chk_file / ...).
if (file.exists("checks.R")) source("checks.R")

message("00_config.R loaded. Outputs -> ", dir_out)
