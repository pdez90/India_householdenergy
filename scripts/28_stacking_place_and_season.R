# ==============================================================================
# 28_stacking_place_and_season.R -- two small SI analyses that turn claims from
# the stacking literature into evidence from this study's own data.
#
# (A) HOW MUCH OF STACKING IS "PLACE"?
#     Among primary-LPG households in the reference surveys (rural IRES 2019-20,
#     ACCESS Wave 1 2014-15 and Wave 2 2018), fit
#        M0: stack ~ 1 + (1|state) + (1|district) + (1|village)
#        M1: M0 + the household characteristics the three surveys share
#            (caste, religion, household size, BPL/AAY card, electricity,
#             within-state wealth quintile -- the stacking model's predictors)
#     and report, on the latent (logit) scale with the logistic residual
#     variance pi^2/3:
#        - the share of variation at the state, district and village levels (M0)
#        - the share explained by household characteristics (marginal R2, M1)
#        - how much of the place-level variance those characteristics absorb.
#     This quantifies "stacking is place-specific" (Perros et al. 2022; Mani et
#     al. 2020) for the covariates the surveys actually share.
#
# (B) DOES NFHS-5's MAIN-FUEL ANSWER SHIFT WITH THE SEASON?
#     Gould et al. (2022) report that most stacking households switch their
#     PRIMARY fuel across seasons. NFHS fieldwork spans the year, and all
#     households in a cluster are interviewed at about the same time, so the
#     comparison is made between clusters of the SAME district interviewed in
#     different seasons (district fixed effects), rural households, design-
#     weighted cluster shares, standard errors clustered on district. A second
#     specification adds a linear trend in interview month, so a season effect is
#     not just LPG rising over the fieldwork period. NFHS-4 (2015-16, before the
#     scale-up) is run the same way for comparison. Only districts whose clusters
#     were interviewed in more than one season contribute; their number is
#     reported, because it bounds what this design can show.
#
# Standalone; reads harmonized_frames.rds (06) and nfhs_hh_covariates.rds (01);
# writes aggregate CSVs only; does not touch the pipeline check registry.
#
#   cd scripts && OUTPUT_DIR=~/Downloads/ACCESS_energy/outputs Rscript 28_stacking_place_and_season.R
#
# OUTPUT  stacking_variance_partition.csv
#         nfhs_main_fuel_seasonality.csv
#         nfhs_main_fuel_seasonality_cells.csv   (clusters / districts per season)
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(lme4); library(sandwich) })
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()

# ------------------------------------------------------------------------------
# (A) variance partition
# ------------------------------------------------------------------------------
H <- readRDS(file.path(dir_out, "harmonized_frames.rds"))
pred_nc <- setdiff(H$predictors, "dist_lpg")
rhs_nc  <- H$rhs_nc

prep_vp <- function(d) {
  d <- d %>% filter(!is.na(stack_binary))
  d <- d[complete.cases(d[, pred_nc, drop = FALSE]), , drop = FALSE]
  vcol <- intersect(c("village", "clust", "psu"), names(d))[1]
  if (is.na(vcol)) stop("no village / cluster identifier in the harmonized frame")
  d %>% mutate(state = factor(state), district = factor(as.character(district)),
               village = factor(paste(district, .data[[vcol]], sep = "_")))
}
requint <- function(d) {   # wealth quintile within state over the rural frame (as in 21/27)
  if (!"month_exp" %in% names(d) || !"wealth_q" %in% names(d)) return(d)
  d %>% mutate(.w = suppressWarnings(as.numeric(month_exp))) %>% group_by(state) %>%
    mutate(wealth_q = ifelse(is.na(.w), NA_integer_, dplyr::ntile(.w, 5))) %>%
    ungroup() %>% select(-.w)
}

vc_of <- function(m, g) { v <- lme4::VarCorr(m)[[g]]; if (is.null(v)) 0 else as.numeric(v[1, 1]) }

partition <- function(d, label) {
  re <- "(1 | state) + (1 | district) + (1 | village)"
  ctl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  m0 <- glmer(as.formula(paste("stack_binary ~ 1 +", re)), data = d, family = binomial, control = ctl)
  m1 <- glmer(as.formula(paste("stack_binary ~", rhs_nc, "+", re)), data = d, family = binomial, control = ctl)
  r  <- pi^2 / 3
  s0 <- c(state = vc_of(m0, "state"), district = vc_of(m0, "district"), village = vc_of(m0, "village"))
  s1 <- c(state = vc_of(m1, "state"), district = vc_of(m1, "district"), village = vc_of(m1, "village"))
  vf <- stats::var(as.vector(model.matrix(m1) %*% lme4::fixef(m1)))   # variance of the fixed-effect predictor
  tot0 <- sum(s0) + r; tot1 <- vf + sum(s1) + r
  data.frame(
    survey = label, n_households = nrow(d), n_villages = nlevels(droplevels(d$village)),
    n_districts = nlevels(droplevels(d$district)), n_states = nlevels(droplevels(d$state)),
    stacking_share = mean(d$stack_binary),
    pct_state = 100 * s0[["state"]] / tot0, pct_district = 100 * s0[["district"]] / tot0,
    pct_village = 100 * s0[["village"]] / tot0,
    pct_place_total = 100 * sum(s0) / tot0,
    pct_household_residual = 100 * r / tot0,
    marginal_r2_household_pct = 100 * vf / tot1,
    place_variance_absorbed_pct = 100 * (1 - sum(s1) / sum(s0)),
    singular_m0 = lme4::isSingular(m0), singular_m1 = lme4::isSingular(m1))
}

frames <- list(
  "IRES rural (2019-20)"    = prep_vp(requint(H$ires %>% filter(rural == 1))),
  "ACCESS Wave 1 (2014-15)" = prep_vp(H$access_w1),
  "ACCESS Wave 2 (2018)"    = if (!is.null(H$access_w2)) prep_vp(H$access_w2) else NULL)
frames <- Filter(function(z) !is.null(z) && nrow(z) > 100, frames)
vp <- bind_rows(lapply(names(frames), function(k) {
  message("variance partition: ", k, " (", nrow(frames[[k]]), " households)")
  tryCatch(partition(frames[[k]], k), error = function(e) { message("  failed: ", conditionMessage(e)); NULL })
}))
write.csv(vp, file.path(dir_out, "stacking_variance_partition.csv"), row.names = FALSE)
cat("\n== (A) Where does stacking vary? (latent scale, % of total variance) ==\n")
print(vp %>% select(survey, n_households, n_villages, pct_state, pct_district, pct_village,
                    pct_place_total, marginal_r2_household_pct, place_variance_absorbed_pct) %>%
        mutate(across(where(is.numeric), ~ round(.x, 1))), row.names = FALSE)

# ------------------------------------------------------------------------------
# (B) NFHS main-fuel seasonality, within district
# ------------------------------------------------------------------------------
nh <- readRDS(file.path(dir_out, "nfhs_hh_covariates.rds"))
need <- c("survey", "rural", "lpg", "district", "clust", "wt", "month_interview")
miss <- setdiff(need, names(nh))
if (length(miss)) stop("nfhs_hh_covariates.rds lacks: ", paste(miss, collapse = ", "))
season_of <- function(m) factor(dplyr::case_when(
  m %in% c(12, 1, 2) ~ "Winter (Dec-Feb)", m %in% 3:5 ~ "Summer (Mar-May)",
  m %in% 6:9 ~ "Monsoon (Jun-Sep)", m %in% 10:11 ~ "Post-monsoon (Oct-Nov)"),
  levels = c("Winter (Dec-Feb)", "Summer (Mar-May)", "Monsoon (Jun-Sep)", "Post-monsoon (Oct-Nov)"))

season_fit <- function(svy) {
  cl <- nh %>%
    filter(survey == svy, rural == 1, !is.na(lpg), !is.na(month_interview), !is.na(district), wt > 0) %>%
    group_by(district, clust) %>%
    summarise(lpg = weighted.mean(lpg, wt), w = sum(wt), n = n(),
              # Median interview date as a month count, so a cluster split
              # between December and January lands in winter (the median of
              # the bare month numbers, 6.5, would put it in the monsoon).
              t = if ("yr_interview" %in% names(nh)) stats::median(12 * yr_interview + month_interview, na.rm = TRUE) else NA_real_,
              month = if (is.finite(t)) ((floor(t) - 1) %% 12) + 1 else round(stats::median(month_interview)),
              .groups = "drop") %>%
    mutate(season = season_of(month), district = factor(district)) %>%
    filter(!is.na(season))
  multi <- cl %>% group_by(district) %>% summarise(k = n_distinct(season), .groups = "drop")
  cells <- cl %>% group_by(season) %>%
    summarise(clusters = n(), districts = n_distinct(district), mean_lpg_pct = 100 * weighted.mean(lpg, w), .groups = "drop") %>%
    mutate(survey = svy)
  cl_m <- cl %>% filter(district %in% multi$district[multi$k > 1]) %>% droplevels()
  fit1 <- function(f, spec) {
    if (nrow(cl_m) < 50 || nlevels(cl_m$season) < 2) return(NULL)
    m <- lm(f, data = cl_m, weights = w)
    V <- sandwich::vcovCL(m, cluster = cl_m$district)
    tr <- grep("^season", names(coef(m)), value = TRUE)
    data.frame(survey = svy, specification = spec, term = sub("^season", "", tr),
               pp_vs_winter = 100 * coef(m)[tr], se_pp = 100 * sqrt(diag(V)[tr]),
               p = 2 * pnorm(-abs(coef(m)[tr] / sqrt(diag(V)[tr]))),
               n_clusters = nrow(cl_m), n_districts_multi_season = nlevels(cl_m$district),
               n_districts_total = nlevels(cl$district), row.names = NULL)
  }
  out <- list(fit1(lpg ~ season + district, "district fixed effects"))
  if (all(is.finite(cl_m$t)))
    out[[2]] <- fit1(lpg ~ season + t + district, "district fixed effects + linear time trend")
  list(est = bind_rows(out), cells = cells)
}

sres <- lapply(c("NFHS5", "NFHS4"), function(s) tryCatch(season_fit(s), error = function(e) {
  message("season fit ", s, " failed: ", conditionMessage(e)); NULL }))
est   <- bind_rows(lapply(sres, `[[`, "est"))
cells <- bind_rows(lapply(sres, `[[`, "cells"))
write.csv(est,   file.path(dir_out, "nfhs_main_fuel_seasonality.csv"), row.names = FALSE)
write.csv(cells, file.path(dir_out, "nfhs_main_fuel_seasonality_cells.csv"), row.names = FALSE)
cat("\n== (B) Rural households reporting LPG as main cooking fuel: season vs winter, within district (pp) ==\n")
print(est %>% mutate(across(c(pp_vs_winter, se_pp), ~ round(.x, 2)), p = signif(p, 2)), row.names = FALSE)
cat("\nClusters and districts by season:\n"); print(cells, row.names = FALSE)
message("wrote stacking_variance_partition.csv, nfhs_main_fuel_seasonality.csv, nfhs_main_fuel_seasonality_cells.csv")
