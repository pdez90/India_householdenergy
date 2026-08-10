# ==============================================================================
# 15_variable_importance.R   (standalone; fast, no brms / no 200-draw proxy)
#
# Quantifies how much each covariate contributes to the fuel-STACKING prediction
# model of 06_stacking_prediction.R, in two complementary senses:
#
#   (A) PREDICTIVE contribution -- drop-one leave-district-out (LDO) AUC.
#       Refit the model without each HOUSEHOLD covariate in turn; the fall in
#       cross-validated AUC (and district-level correlation) is that covariate's
#       unique out-of-sample contribution. Uses the same district-fold LDO scheme
#       as 06. The district-context covariate (dist_lpg) is a district aggregate
#       that cannot be honestly held out (its held-out value encodes the district
#       composition), so it is EXCLUDED from the drop-one CV and reported only via
#       its in-sample effect strength (B), with the gain from adding it to the
#       household-only model reported separately as the context contribution.
#
#   (B) EFFECT strength -- absolute z-statistic (|estimate/SE|) of each covariate
#       in the fitted logistic model with numeric predictors standardized, so the
#       magnitudes are comparable across covariates (factors summarized by their
#       largest-magnitude level).
#
# Both are computed in each training survey (ACCESS Wave 1; IRES rural), matching
# the two stacking models 06 actually fits.
#
# Inputs : access_hh.rds, ires_hh.rds, nfhs_hh_covariates.rds,
#          corrected_nfhs_districts.rds   (same as 06)
# Outputs: var_importance_stacking.csv (predictive contribution),
#          var_importance_effect.csv    (effect strength)
# ==============================================================================

source("00_config.R")
need_inputs(c("access_hh.rds"                = "02_prep_access.R",
              "ires_hh.rds"                  = "03_prep_ires.R",
              "nfhs_hh_covariates.rds"       = "01_prep_nfhs.R",
              "corrected_nfhs_districts.rds" = "05_correction.R"))
library(pROC)

access_hh <- readRDS(file.path(dir_out, "access_hh.rds"))
ires_hh   <- readRDS(file.path(dir_out, "ires_hh.rds"))
nfhs_hh   <- readRDS(file.path(dir_out, "nfhs_hh_covariates.rds"))
corrected <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))

# ---- Harmonization: copied verbatim from 06 so the predictor set is identical --
canon_state <- function(x) {
  x <- stringr::str_squish(tools::toTitleCase(tolower(as.character(x))))
  dplyr::recode(x,
    "Chattisgarh" = "Chhattisgarh", "Uttarakhaand" = "Uttarakhand",
    "Uttaranchal" = "Uttarakhand", "Nct of Delhi" = "Delhi",
    "Nct Of Delhi" = "Delhi", "Orissa" = "Odisha",
    "Pondicherry" = "Puducherry", "Jammu and Kashmir" = "Jammu & Kashmir",
    .default = x)
}
ACCESS_CODE_TO_NAME <- c("9" = "Uttar Pradesh", "10" = "Bihar",
                         "19" = "West Bengal", "20" = "Jharkhand",
                         "21" = "Odisha", "23" = "Madhya Pradesh")

harmonize <- function(df, source) {
  relig <- if ("religion" %in% names(df)) df$religion else df$hh_relig
  hhs   <- if ("hhsize"   %in% names(df)) df$hhsize   else df$hh_size
  bplv <- switch(source,
    ACCESS = as.numeric(df$bplaay),
    IRES   = { v <- suppressWarnings(as.numeric(df$bplaay)); v[v == 99] <- NA
               ifelse(is.na(v), NA_real_, as.numeric(v %in% c(1, 2))) },
    NFHS   = as.numeric(df$bpl == 1))
  wvar <- switch(source,
    ACCESS = "month_exp", IRES = "month_exp",
    NFHS   = if ("wealth_score" %in% names(df)) "wealth_score" else NA_character_)
  df <- df %>%
    mutate(
      caste3    = factor(caste, levels = c("Scheduled Caste", "Scheduled Tribe",
                                           "Other Backward Class", "General")),
      religion3 = factor(relig, levels = c("Hindu", "Muslim", "Other")),
      hhsize_c  = pmin(suppressWarnings(as.numeric(hhs)), 15),
      bpl_bin   = bplv,
      elec_bin  = suppressWarnings(as.numeric(
                    if ("electricity" %in% names(df)) df$electricity else NA)),
      state     = factor(canon_state(
        if (source == "ACCESS") {
          if ("state_name" %in% names(df) && !all(is.na(df$state_name)))
            dplyr::coalesce(as.character(df$state_name),
                            ACCESS_CODE_TO_NAME[as.character(df$state)])
          else ACCESS_CODE_TO_NAME[as.character(df$state)]
        } else as.character(df$state)))
    )
  if (!is.na(wvar)) {
    df <- df %>% mutate(.w = suppressWarnings(as.numeric(.data[[wvar]]))) %>%
      group_by(state) %>%
      mutate(wealth_q = ifelse(is.na(.w), NA_integer_, dplyr::ntile(.w, 5))) %>%
      ungroup() %>% select(-.w)
  } else df$wealth_q <- NA_integer_
  df
}
add_ctx <- function(df) df %>% group_by(district) %>%
  mutate(dist_lpg = mean(main_fuel_lpg, na.rm = TRUE)) %>% ungroup()

access_t <- add_ctx(harmonize(access_hh %>% filter(wave == 0), "ACCESS"))
ires_t   <- add_ctx(harmonize(ires_hh, "IRES"))
nfhs_t   <- harmonize(nfhs_hh, "NFHS")
corr_ctx <- corrected %>%
  transmute(district = as.character(district),
            d15 = dplyr::coalesce(lpg_2015_bayes, lpg_2015_rc, lpg_2015_rural),
            d19 = dplyr::coalesce(lpg_2019_bayes, lpg_2019_rc, lpg_2019_rural))
nfhs_t <- nfhs_t %>% left_join(corr_ctx, by = "district") %>%
  mutate(dist_lpg = ifelse(survey == "NFHS4", d15, d19)) %>% select(-d15, -d19)

usable <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(FALSE)
  if (is.factor(x) || is.character(x)) length(unique(as.character(x))) >= 2 else TRUE }
# Education was a candidate predictor until 2026-08-01; the NFHS extracts carry
# no attainment scale, so it was all-missing on the NFHS side. See the header
# note in 01_prep_nfhs.R. Kept out of `cand` and out of LAB below.
cand <- c("caste3", "religion3", "hhsize_c", "bpl_bin",
          "elec_bin", "wealth_q", "dist_lpg")
predictors <- cand[vapply(cand, function(v)
  usable(access_t[[v]]) && usable(ires_t[[v]]) && usable(nfhs_t[[v]]), logical(1))]
HH <- setdiff(predictors, "dist_lpg")   # household covariates (CV-droppable)
message("Predictors: ", paste(predictors, collapse = ", "))

LAB <- c(caste3 = "Caste category", religion3 = "Religion",
         hhsize_c = "Household size", bpl_bin = "BPL/ration card",
         elec_bin = "Household electricity",
         wealth_q = "Wealth quintile (within-state)",
         dist_lpg = "District LPG share (context)")

# ==============================================================================
# (A) Drop-one leave-district-out AUC + district correlation
# ==============================================================================
ldo_perf <- function(dat, vars, k = 10, seed = 42) {
  keep <- c(vars, "state", "district", "stack_binary")
  dat  <- dat %>% filter(!is.na(stack_binary)) %>%
    filter(complete.cases(.[, keep]))
  ds   <- unique(as.character(dat$district))
  set.seed(seed)
  fold <- setNames(sample(rep(1:k, length.out = length(ds))), ds)
  pr <- purrr::map_dfr(1:k, function(f) {
    tr <- dat %>% filter(fold[as.character(district)] != f)
    te <- dat %>% filter(fold[as.character(district)] == f)
    m  <- suppressWarnings(glm(
      as.formula(paste("stack_binary ~", paste(vars, collapse = " + "), "+ state")),
      data = tr, family = binomial))
    te$p <- predict(m, newdata = te, type = "response")
    te %>% select(district, stack_binary, p)
  })
  auc <- as.numeric(pROC::auc(pr$stack_binary, pr$p, quiet = TRUE))
  dr  <- pr %>% group_by(district) %>%
    summarise(o = mean(stack_binary), h = mean(p), .groups = "drop") %>%
    summarise(r = cor(o, h, use = "complete.obs")) %>% pull(r)
  list(auc = auc, district_r = dr)
}

drop_one <- function(dat, label) {
  base <- ldo_perf(dat, HH)
  # household + dist_lpg. WARNING: dist_lpg is a district-mean, so within each LDO
  # fold it still carries the held-out district's own aggregate -- this AUC is an
  # IN-SAMPLE/leaky number, NOT an honest out-of-district contribution. Reported
  # only with that caveat below; never treated as leave-district-out performance.
  ctx  <- ldo_perf(dat, predictors)
  rows <- purrr::map_dfr(HH, function(v) {
    red <- ldo_perf(dat, setdiff(HH, v))
    tibble(survey = label, covariate = unname(LAB[v]),
           auc_drop = round(base$auc - red$auc, 4),
           district_r_drop = round(base$district_r - red$district_r, 4),
           auc_without = round(red$auc, 4))
  }) %>% arrange(desc(auc_drop))
  message(sprintf("[%s] household-only LDO AUC = %.3f (district-r %.3f); +dist_lpg AUC = %.3f (IN-SAMPLE/leaky, not honest LDO)",
                  label, base$auc, base$district_r, ctx$auc))
  attr(rows, "base_auc") <- base$auc
  attr(rows, "ctx_auc")  <- ctx$auc
  rows
}

imp_access <- drop_one(access_t,                       "ACCESS W1")
imp_ires   <- drop_one(ires_t %>% filter(rural == 1),  "IRES rural")

# Context-covariate "contribution" (adding dist_lpg to the household-only model).
# FLAGGED AS LEAKY: dist_lpg is a district-mean built from the full district
# (including held-out outcomes), so this gain is an in-sample number that
# OVERSTATES what dist_lpg would add for a genuinely new district. It is NOT an
# honest out-of-district LDO contribution; column names and a note say so. The
# defensible statements about dist_lpg are its in-sample effect strength (below)
# and this caveat -- not a leave-district-out AUC gain.
ctx_gain <- tibble(
  survey = c("ACCESS W1", "IRES rural"),
  household_only_auc          = c(attr(imp_access, "base_auc"), attr(imp_ires, "base_auc")),
  plus_dist_lpg_auc_insample  = c(attr(imp_access, "ctx_auc"),  attr(imp_ires, "ctx_auc")),
  context_auc_gain_leaky      = round(c(attr(imp_access, "ctx_auc") - attr(imp_access, "base_auc"),
                                        attr(imp_ires,   "ctx_auc") - attr(imp_ires,   "base_auc")), 4),
  note = "dist_lpg leaks held-out district info; in-sample only, NOT honest out-of-district LDO gain")

imp_all <- bind_rows(imp_access, imp_ires)
write_csv(imp_all, file.path(dir_out, "var_importance_stacking.csv"))
write_csv(ctx_gain, file.path(dir_out, "var_importance_context_gain.csv"))

cat("\n===== (A) Predictive contribution: drop-one leave-district-out =====\n")
cat("    auc_drop = AUC points lost when the covariate is removed (larger = more important)\n")
print(as.data.frame(imp_all), row.names = FALSE, digits = 3)
cat("\n  District-context covariate (added to the household-only model) --",
    "IN-SAMPLE/LEAKY, not honest out-of-district LDO (see note column):\n")
print(as.data.frame(ctx_gain), row.names = FALSE, digits = 3)

# ==============================================================================
# (B) Effect strength: |z| from the fitted model, numeric predictors standardized
# ==============================================================================
eff_strength <- function(dat, label) {
  d <- dat %>% filter(!is.na(stack_binary)) %>%
    filter(complete.cases(.[, c(predictors, "state", "stack_binary")])) %>%
    mutate(across(any_of(c("hhsize_c", "wealth_q", "dist_lpg")),
                  ~ as.numeric(scale(.x))))
  m  <- suppressWarnings(glm(
    as.formula(paste("stack_binary ~", paste(predictors, collapse = " + "), "+ state")),
    data = d, family = binomial))
  co <- summary(m)$coefficients
  z  <- abs(co[, "z value"]); nm <- rownames(co)
  # map each coefficient to its covariate; summarize factor covars by max |z|
  purrr::map_dfr(predictors, function(v) {
    idx <- if (v %in% c("caste3", "religion3")) grepl(paste0("^", v), nm) else nm == v
    tibble(survey = label, covariate = unname(LAB[v]),
           abs_z = if (any(idx)) round(max(z[idx]), 2) else NA_real_)
  }) %>% arrange(desc(abs_z))
}
eff_all <- bind_rows(eff_strength(access_t,                      "ACCESS W1"),
                     eff_strength(ires_t %>% filter(rural == 1), "IRES rural"))
write_csv(eff_all, file.path(dir_out, "var_importance_effect.csv"))

cat("\n===== (B) Effect strength: |z| in the fitted model (standardized) =====\n")
cat("    factors summarized by their largest-magnitude level; larger |z| = stronger\n")
print(as.data.frame(eff_all), row.names = FALSE, digits = 3)

## ---- CHECKS ------------------------------------------------------------------
chk_header("15_variable_importance")
chk("15", "household-only LDO importance produced (honest)",
    exists("imp_all") && nrow(imp_all) > 0)
chk("15", "context gain relabelled as leaky/in-sample",
    "context_auc_gain_leaky" %in% names(ctx_gain) &&
    !"context_auc_gain" %in% names(ctx_gain))
chk_file("15", "stacking importance table written", "var_importance_stacking.csv")

message("\n15_variable_importance.R done -> var_importance_stacking.csv, ",
        "var_importance_context_gain.csv, var_importance_effect.csv")
