# ==============================================================================
# 06_stacking_prediction.R
# Train fuel-stacking models in ACCESS/IRES using ONLY predictors that exist in
# NFHS, validate them (leave-district-out + cross-survey), then apply to NFHS
# households to produce district-level stacking / exposure proxies.
#
# Outcomes:
#   (1) stack_binary : among main-fuel-LPG households, still uses solid fuel
#   (2) use3cat      : Solid fuel reported, no LPG / LPG+solid stacking / LPG, no solid fuel reported
#   (3) lpg_kg_yr    : two-part hurdle (uses LPG? -> kg | use), a la Gould 2020
#
# State handling: models with state fixed effects cannot predict for states
# outside the training survey, so each fixed-effect model is fit twice (with
# and without state); households in training-support states get the with-state
# prediction, all others get the no-state (population-average) prediction and
# are flagged via in_support.
#
# Inputs : access_hh.rds, ires_hh.rds, nfhs_hh_covariates.rds,
#          corrected_nfhs_districts.rds
# Outputs: stacking_models.rds, nfhs_predicted_stacking.rds,
#          district_exposure_proxy.rds/.csv, stacking_validation.jpeg
# ==============================================================================

source("00_config.R")
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "06_stacking_prediction"

need_inputs(c("access_hh.rds"                = "02_prep_access.R",
              "ires_hh.rds"                  = "03_prep_ires.R",
              "nfhs_hh_covariates.rds"       = "01_prep_nfhs.R",
              "corrected_nfhs_districts.rds" = "05_correction.R"))
library(nnet)    # multinomial
library(pROC)    # AUC

access_hh <- readRDS(file.path(dir_out, "access_hh.rds"))
ires_hh   <- readRDS(file.path(dir_out, "ires_hh.rds"))
nfhs_hh   <- readRDS(file.path(dir_out, "nfhs_hh_covariates.rds"))
corrected <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))

# ---- Canonical state names ------------------------------------------------------
# IRES misspells several states ("Chattisgarh", "Uttarakhaand", "Nct Of Delhi")
# and ACCESS stores numeric state codes -- without harmonization, NFHS states
# never match training states, so state effects and the in_support flag break.
canon_state <- function(x) {
  x <- stringr::str_squish(tools::toTitleCase(tolower(as.character(x))))
  dplyr::recode(x,
    "Chattisgarh"       = "Chhattisgarh",
    "Uttarakhaand"      = "Uttarakhand",
    "Uttaranchal"       = "Uttarakhand",
    "Nct of Delhi"      = "Delhi",
    "Nct Of Delhi"      = "Delhi",
    "Orissa"            = "Odisha",
    "Pondicherry"       = "Puducherry",
    "Jammu and Kashmir" = "Jammu & Kashmir",
    .default = x)
}
ACCESS_CODE_TO_NAME <- c("9" = "Uttar Pradesh", "10" = "Bihar",
                         "19" = "West Bengal", "20" = "Jharkhand",
                         "21" = "Odisha", "23" = "Madhya Pradesh")

# ---- Harmonize the shared predictor set --------------------------------------
# Each survey stores religion / household size / BPL under different names and
# codings; resolve them per-source OUTSIDE of mutate() so no survey's code path
# references a column that only exists in another survey.
harmonize <- function(df, source) {
  relig <- if ("religion" %in% names(df)) df$religion else df$hh_relig
  hhs   <- if ("hhsize"   %in% names(df)) df$hhsize   else df$hh_size

  bplv <- switch(source,
    ACCESS = as.numeric(df$bplaay),                       # already 0/1 (BPL/AAY)
    IRES   = { v <- suppressWarnings(as.numeric(df$bplaay))
               # q212 labels: 1 = Antyodaya, 2 = BPL, 3 = None, 99 = Don't know
               v[v == 99] <- NA
               ifelse(is.na(v), NA_real_, as.numeric(v %in% c(1, 2))) },
    NFHS   = as.numeric(df$bpl == 1))                     # sh58/sh75: 1 = yes

  # Wealth: within-state quintile of the best available SES gradient --
  # NFHS wealth-index score; ACCESS/IRES monthly expenditure. Comparable as a
  # relative (rank-based) measure, not as absolute wealth.
  wvar <- switch(source,
    ACCESS = "month_exp", IRES = "month_exp",
    NFHS   = if ("wealth_score" %in% names(df)) "wealth_score" else NA_character_)

  # Fixed, shared level sets across all surveys (missing stays NA -> those
  # households get NA predictions rather than breaking factor alignment):
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
          # prefer the merged shapefile state name; fall back to code->name map
          if ("state_name" %in% names(df) && !all(is.na(df$state_name)))
            dplyr::coalesce(as.character(df$state_name),
                            ACCESS_CODE_TO_NAME[as.character(df$state)])
          else ACCESS_CODE_TO_NAME[as.character(df$state)]
        } else as.character(df$state)))
    )
  if (!is.na(wvar)) {
    df <- df %>%
      mutate(.w = suppressWarnings(as.numeric(.data[[wvar]]))) %>%
      group_by(state) %>%
      mutate(wealth_q = ifelse(is.na(.w), NA_integer_, dplyr::ntile(.w, 5))) %>%
      ungroup() %>% select(-.w)
  } else df$wealth_q <- NA_integer_
  df
}

access_t <- harmonize(access_hh %>% filter(wave == 0), "ACCESS")  # W1 = NFHS-4 era
ires_t   <- harmonize(ires_hh, "IRES")
nfhs_t   <- harmonize(nfhs_hh, "NFHS")

# ---- District-context predictor: district share of LPG as main fuel ------------
# Training surveys: observed district share. NFHS: the CORRECTED district
# estimate from 05 (on the reference-survey scale, so it matches the context
# the models were trained on). Era-matched: 2015 correction for NFHS-4 rows,
# 2019 for NFHS-5 rows.
add_ctx <- function(df) df %>%
  group_by(district) %>%
  mutate(dist_lpg = mean(main_fuel_lpg, na.rm = TRUE)) %>%
  ungroup()
access_t <- add_ctx(access_t)
ires_t   <- add_ctx(ires_t)

corr_ctx <- corrected %>%
  transmute(district = as.character(district),
            d15 = dplyr::coalesce(.data[["lpg_2015_bayes"]],
                                  .data[["lpg_2015_rc"]], lpg_2015_rural),
            d19 = dplyr::coalesce(.data[["lpg_2019_bayes"]],
                                  .data[["lpg_2019_rc"]], lpg_2019_rural))
nfhs_t <- nfhs_t %>%
  left_join(corr_ctx, by = "district") %>%
  mutate(dist_lpg = ifelse(survey == "NFHS4", d15, d19)) %>%
  select(-d15, -d19)

# Keep a predictor only if it is usable (non-missing; >=2 observed levels for
# factors) in ALL THREE surveys -- otherwise drop it with a message rather
# than crash with "contrasts can be applied only to factors with 2+ levels".
# A predictor is usable only if it VARIES. The earlier version returned TRUE for
# any numeric with at least one non-missing value, which let a CONSTANT numeric
# through: in the run of 2026-07-30 the NFHS education indicator was constant
# (derived from a binary hh_college flag via `<= 2`, so every household coded 1)
# and it was fitted on IRES, where it varies, then applied to NFHS, where it does
# not. That does not fail loudly -- it silently applies the IRES coefficient as a
# uniform logit shift to every NFHS household. Distinctness is the property that
# matters, so distinctness is what is tested, for numerics as well as factors.
# (Education itself was removed from the pipeline on 2026-08-01 -- see the header
# note in 01_prep_nfhs.R -- but the rule it motivated stays, and guards the rest.)
usable <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(FALSE)
  if (is.factor(x) || is.character(x)) length(unique(as.character(x))) >= 2
  else length(unique(x)) >= 2
}
# Education was a candidate predictor until 2026-08-01. It is not one now: the
# NFHS extracts carry no attainment scale, so the NFHS side was all-missing and
# a transfer model fitted on ACCESS/IRES would have been applied to NFHS
# households with nothing to apply it to. See the header note in 01_prep_nfhs.R.
cand <- c("caste3", "religion3", "hhsize_c", "bpl_bin",
          "elec_bin", "wealth_q", "dist_lpg")
# Print what each survey actually carries for every candidate BEFORE selecting,
# so a dropped predictor can be traced to the survey and the reason without
# re-running anything.
.frames <- list(ACCESS = access_t, IRES = ires_t, NFHS = nfhs_t)
message("\n[predictors] candidate availability (n non-missing / distinct values):")
for (v in cand) {
  bits <- vapply(names(.frames), function(nm) {
    x <- .frames[[nm]][[v]]
    if (is.null(x)) return(paste0(nm, "=ABSENT"))
    xx <- x[!is.na(x)]
    nd <- if (is.factor(x) || is.character(x)) length(unique(as.character(xx)))
          else length(unique(xx))
    paste0(nm, "=", length(xx), "/", nd, if (nd < 2) " <-- NOT USABLE" else "")
  }, character(1))
  message("  ", format(v, width = 12), paste(bits, collapse = "  "))
}
predictors <- cand[vapply(cand, function(v)
  usable(access_t[[v]]) && usable(ires_t[[v]]) && usable(nfhs_t[[v]]),
  logical(1))]
dropped <- setdiff(cand, predictors)
if (length(dropped))
  message("Dropping predictor(s) unusable in at least one survey: ",
          paste(dropped, collapse = ", "),
          " -- check the harmonization if this is unexpected.")
if (length(predictors) < 2)
  stop("Too few usable shared predictors (", paste(predictors, collapse = ", "),
       "). Inspect the harmonized frames before proceeding.")
message("Predictors used: ", paste(predictors, collapse = ", "))
rhs <- paste(predictors, collapse = " + ")
# Context-free predictor set (drops the district-level dist_lpg): used for
# leave-district-out validation and cross-survey transfer, where a district-mean
# covariate would leak the held-out district's own aggregate or be undefined.
rhs_nc <- paste(setdiff(predictors, "dist_lpg"), collapse = " + ")

# ---- Export the harmonized frames for the transfer diagnostic (21) -------------
# 21_transfer_diagnostics.R asks WHY the IRES-trained model fails to transfer to
# ACCESS, by separating the two things that are confounded in the cross-survey
# test below: the ERA gap (2015 vs 2019-20, spanning PMUY) and the INSTRUMENT gap
# (different questionnaire, sampling and fieldwork). ACCESS wave 2 (2018) makes
# that separable -- it shares ACCESS's instrument with W1 but sits on the far
# side of PMUY, close to IRES in time:
#
#                     2015 era        2018-2020 era
#   ACCESS instrument  W1 (wave 0)     W2 (wave 1)
#   IRES instrument    --              IRES
#
#   W1 -> W2   : era effect, instrument HELD CONSTANT
#   W2 -> IRES : instrument effect, era approximately held constant
#   W1 -> IRES : both at once -- the test 06 currently reports
#
# We export the harmonized frames rather than let 21 re-derive them, because
# harmonize() encodes every survey-specific recode (BPL coding, the within-state
# wealth quintile) and a second copy of that logic would drift.
# W2 is harmonized here with the identical function and gets its own district
# LPG context (its 2018 share, not W1's).
access_t_w2 <- add_ctx(harmonize(access_hh %>% filter(wave == 1), "ACCESS"))
saveRDS(list(access_w1 = access_t, access_w2 = access_t_w2, ires = ires_t,
             predictors = predictors, rhs = rhs, rhs_nc = rhs_nc),
        file.path(dir_out, "harmonized_frames.rds"))
message(sprintf("Harmonized frames saved -> harmonized_frames.rds (W1 n=%d, W2 n=%d, IRES n=%d)",
                nrow(access_t), nrow(access_t_w2), nrow(ires_t)))

# ---- Prediction helpers --------------------------------------------------------
# NA-safe prediction: rows with any missing predictor get NA.
complete_rows <- function(nd) complete.cases(nd[, predictors, drop = FALSE])

# NA/weight-safe weighted mean: drops rows where the value OR the weight is
# non-finite, so a district with a few NA survey weights doesn't collapse the
# whole district mean to NA (base weighted.mean(na.rm=TRUE) still returns NA if
# any weight is NA).
safe_wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

predict_glmer <- function(m, nd) {
  ok  <- complete_rows(nd)
  out <- rep(NA_real_, nrow(nd))
  if (any(ok)) out[ok] <- predict(m, newdata = nd[ok, , drop = FALSE],
                                  type = "response", allow.new.levels = TRUE)
  out
}

# For fixed-effect models (multinom / glm): use the with-state model inside the
# training states, the no-state model elsewhere.
#
# IMPORTANT: use3cat has FOUR levels ("Solid fuel reported, no LPG", "LPG and solid fuel reported",
# "LPG, no solid fuel reported", "Neither LPG nor solid fuel reported"), so predict(type="probs") returns
# a 4-column matrix. We extract ALL FOUR probabilities BY LEVEL NAME (never by
# position) and carry the fourth (p_other_nonsolid) through, so the four reported
# shares sum to 1 by construction rather than to 1 - P(other). The degenerate
# binary case (predict() returns a bare vector) is handled too.
probs_named <- function(m, nd) {
  pr <- predict(m, newdata = nd, type = "probs")
  if (is.null(dim(pr))) {                 # binary response -> vector = P(2nd level)
    pr <- cbind(1 - pr, pr)
    colnames(pr) <- m$lev
  }
  pr
}
col_or0 <- function(pr, nm)
  if (nm %in% colnames(pr)) pr[, nm] else rep(0, nrow(pr))

predict_multinom2 <- function(m_state, m_nostate, nd, train_states) {
  ok  <- complete_rows(nd)
  ins <- ok & (as.character(nd$state) %in% train_states)
  oos <- ok & !(as.character(nd$state) %in% train_states)
  res <- tibble(p_solid_only     = rep(NA_real_, nrow(nd)),
                p_stacking       = rep(NA_real_, nrow(nd)),
                p_excl_lpg       = rep(NA_real_, nrow(nd)),
                p_other_nonsolid = rep(NA_real_, nrow(nd)))
  grab <- function(m, rows) {
    pr <- probs_named(m, nd[rows, , drop = FALSE])
    tibble(p_solid_only     = col_or0(pr, "Solid fuel reported, no LPG"),
           p_stacking       = col_or0(pr, "LPG and solid fuel reported"),
           p_excl_lpg       = col_or0(pr, "LPG, no solid fuel reported"),
           p_other_nonsolid = col_or0(pr, "Neither LPG nor solid fuel reported"))
  }
  if (any(ins)) res[ins, ] <- grab(m_state,   which(ins))
  if (any(oos)) res[oos, ] <- grab(m_nostate, which(oos))
  res
}

predict_glm2 <- function(m_state, m_nostate, nd, train_states, type = "response") {
  ok  <- complete_rows(nd)
  ins <- ok & (as.character(nd$state) %in% train_states)
  oos <- ok & !(as.character(nd$state) %in% train_states)
  out <- rep(NA_real_, nrow(nd))
  if (any(ins)) out[ins] <- predict(m_state,   newdata = nd[ins, , drop = FALSE], type = type)
  if (any(oos)) out[oos] <- predict(m_nostate, newdata = nd[oos, , drop = FALSE], type = type)
  out
}

# ==============================================================================
# (1) BINARY STACKING among main-fuel-LPG households
# ==============================================================================
# glmer with state & district RANDOM effects: new levels are fine at predict
# time (they get the population-level effect), so one model per survey suffices.
fit_stack <- function(dat) {
  glmer(as.formula(paste("stack_binary ~", rhs, "+ (1 | state) + (1 | district)")),
        data = dat %>% filter(!is.na(stack_binary)),
        family = binomial, nAGQ = 0,
        control = glmerControl(optimizer = "nloptwrap"))
}
m_stack_access   <- fit_stack(access_t)
m_stack_ires     <- fit_stack(ires_t %>% filter(rural == 1))
m_stack_ires_all <- fit_stack(ires_t)

# Record each stacking fit in diagnostics/model_fits.csv. These are the models
# whose predictions become the NFHS stacking surface, so a singular state or
# district variance here means the "multilevel" prediction is really a pooled
# fixed-effect prediction for that grouping -- worth knowing before the surface
# is interpreted as capturing district heterogeneity.
if (exists("chk_record_fit")) {
  chk_record_fit(.chk_tag("06_stacking_prediction"), "stack_binary:ACCESS",
                 m_stack_access,   extra = "training = ACCESS (rural)")
  chk_record_fit(.chk_tag("06_stacking_prediction"), "stack_binary:IRES_rural",
                 m_stack_ires,     extra = "training = IRES rural")
  chk_record_fit(.chk_tag("06_stacking_prediction"), "stack_binary:IRES_all",
                 m_stack_ires_all, extra = "training = IRES all-India")
}

# ---- Leave-district-out validation ---------------------------------------------
ldo_validate <- function(dat, k = 10) {
  dat  <- dat %>% filter(!is.na(stack_binary), complete_rows(.))
  ds   <- unique(as.character(dat$district))
  fold <- setNames(sample(rep(1:k, length.out = length(ds))), ds)
  # Use the context-FREE predictor set (no dist_lpg): dist_lpg is a district-level
  # mean, so under leave-district-out it is either unavailable for the held-out
  # district or leaks that district's own aggregate into the fit. Dropping it
  # makes the reported validation an honest out-of-district generalization test.
  preds <- map_dfr(1:k, function(f) {
    tr <- dat %>% filter(fold[as.character(district)] != f)
    te <- dat %>% filter(fold[as.character(district)] == f)
    m  <- glm(as.formula(paste("stack_binary ~", rhs_nc, "+ state")),
              data = tr, family = binomial)
    te$pred <- predict(m, newdata = te, type = "response")
    te %>% select(district, stack_binary, pred)
  })
  list(auc   = as.numeric(pROC::auc(preds$stack_binary, preds$pred, quiet = TRUE)),
       brier = mean((preds$pred - preds$stack_binary)^2, na.rm = TRUE),
       district_cor = preds %>% group_by(district) %>%
         summarise(obs = mean(stack_binary), hat = mean(pred), .groups = "drop") %>%
         summarise(r = cor(obs, hat, use = "complete.obs")) %>% pull(r))
}
set.seed(42)
val_access <- ldo_validate(access_t)
val_ires   <- ldo_validate(ires_t %>% filter(rural == 1))
message(sprintf(
  "Stacking LDO validation -- ACCESS: AUC %.3f, district-r %.3f | IRES rural: AUC %.3f, district-r %.3f",
  val_access$auc, val_access$district_cor, val_ires$auc, val_ires$district_cor))

# Persist the leave-district-out validation of the PRODUCTION stacking models.
# These figures are quoted in the manuscript (Section 3.4, SI Methods S2, the
# SI Figure S6 caption); writing them to disk keeps the document sourced from
# the analysis rather than from this script's console output.
ldo_val_tab <- tibble::tibble(
  survey      = c("ACCESS W1", "IRES rural"),
  auc         = c(val_access$auc,          val_ires$auc),
  brier       = c(val_access$brier,        val_ires$brier),
  district_r  = c(val_access$district_cor, val_ires$district_cor))
readr::write_csv(ldo_val_tab, file.path(dir_out, "stacking_ldo_validation.csv"))
if (exists("chk")) chk("06", "stacking LDO validation table written",
                       file.exists(file.path(dir_out, "stacking_ldo_validation.csv")))

# ---- Cross-survey validation: IRES-trained model scored in ACCESS districts ----
# NOTE: dist_lpg is era-specific (2019 levels in IRES vs 2015 in ACCESS), so a
# model containing it cannot be fairly scored across eras. Use the context-free
# predictor set (rhs_nc, defined above) for this test only.
m_stack_ires_nc <- glmer(
  as.formula(paste("stack_binary ~", rhs_nc, "+ (1 | state) + (1 | district)")),
  data = ires_t %>% filter(rural == 1, !is.na(stack_binary)),
  family = binomial, nAGQ = 0, control = glmerControl(optimizer = "nloptwrap"))
if (exists("chk_record_fit"))
  chk_record_fit(.chk_tag("06_stacking_prediction"), "stack_binary:IRES_rural_contextfree",
                 m_stack_ires_nc, extra = "cross-survey transfer model (rhs_nc)")
access_t$pred_ires_model <- {
  ok <- complete.cases(access_t[, setdiff(predictors, "dist_lpg"), drop = FALSE])
  out <- rep(NA_real_, nrow(access_t))
  out[ok] <- predict(m_stack_ires_nc, newdata = access_t[ok, , drop = FALSE],
                     type = "response", allow.new.levels = TRUE)
  out
}
cross_val <- access_t %>%
  filter(!is.na(stack_binary), !is.na(pred_ires_model)) %>%
  group_by(district) %>%
  summarise(obs = mean(stack_binary), hat = mean(pred_ires_model),
            .groups = "drop")
message("Cross-survey district-level r (IRES model -> ACCESS obs): ",
        round(cor(cross_val$obs, cross_val$hat, use = "complete.obs"), 3))

# ==============================================================================
# (2) 3-CATEGORY USE (all households) -- with-state + no-state variants
# ==============================================================================
fit_3cat <- function(dat, with_state = TRUE) {
  f <- paste("use3cat ~", rhs, if (with_state) "+ state" else "")
  nnet::multinom(as.formula(f), data = dat %>% filter(!is.na(use3cat)),
                 trace = FALSE, maxit = 400)
}
m_3cat_access    <- fit_3cat(access_t)
m_3cat_access_ns <- fit_3cat(access_t, with_state = FALSE)
m_3cat_ires_a    <- fit_3cat(ires_t)
m_3cat_ires_a_ns <- fit_3cat(ires_t, with_state = FALSE)

states_access <- levels(droplevels(access_t$state))
states_ires   <- levels(droplevels(ires_t$state))

# ==============================================================================
# (3) LPG kg/yr -- two-part hurdle (with-state + no-state variants)
# ==============================================================================
fit_hurdle <- function(dat, with_state = TRUE) {
  st  <- if (with_state) "+ state" else ""
  dat <- dat %>% filter(!is.na(lpg_kg_yr) | !is.na(uselpg))
  part1 <- glm(as.formula(paste("uselpg ~", rhs, st)),
               data = dat %>% filter(!is.na(uselpg)), family = binomial)
  part2 <- glm(as.formula(paste("log(lpg_kg_yr) ~", rhs, st)),
               data = dat %>% filter(uselpg == 1, lpg_kg_yr > 0),
               family = gaussian)
  list(adopt = part1, use = part2, smear = mean(exp(residuals(part2))))
}
h_ires    <- fit_hurdle(ires_t)
h_ires_ns <- fit_hurdle(ires_t, with_state = FALSE)

pred_kg <- function(h, h_ns, nd, train_states) {
  p_adopt <- predict_glm2(h$adopt, h_ns$adopt, nd, train_states, "response")
  mu_log  <- predict_glm2(h$use,   h_ns$use,   nd, train_states, "link")
  p_adopt * exp(mu_log) * h$smear
}

# ==============================================================================
# APPLY TO NFHS HOUSEHOLDS
# ==============================================================================
#   NFHS-4 rural: ACCESS W1 models (same era & rural setting; states outside the
#                 six ACCESS states use the population-average / no-state path)
#   NFHS-5      : IRES models (same era, national, urban + rural)
nfhs4_r <- nfhs_t %>% filter(survey == "NFHS4", rural == 1)
nfhs5   <- nfhs_t %>% filter(survey == "NFHS5")

nfhs4_r$p_stack <- predict_glmer(m_stack_access,   nfhs4_r)
nfhs5$p_stack   <- predict_glmer(m_stack_ires_all, nfhs5)

nfhs4_r <- bind_cols(nfhs4_r,
  predict_multinom2(m_3cat_access, m_3cat_access_ns, nfhs4_r, states_access))
nfhs5   <- bind_cols(nfhs5,
  predict_multinom2(m_3cat_ires_a, m_3cat_ires_a_ns, nfhs5, states_ires))

nfhs4_r$e_lpg_kg_yr <- pred_kg(h_ires, h_ires_ns, nfhs4_r, states_ires)
nfhs5$e_lpg_kg_yr   <- pred_kg(h_ires, h_ires_ns, nfhs5,   states_ires)

# Support flag: was this household's state inside the training survey?
nfhs4_r$in_support <- as.character(nfhs4_r$state) %in% states_access
nfhs5$in_support   <- as.character(nfhs5$state)   %in% states_ires

nfhs_pred <- bind_rows(nfhs4_r, nfhs5)
saveRDS(nfhs_pred, file.path(dir_out, "nfhs_predicted_stacking.rds"))

# ==============================================================================
# DISTRICT EXPOSURE PROXY
# ==============================================================================
district_proxy <- nfhs_pred %>%
  filter(!is.na(district)) %>%
  group_by(survey, state, district) %>%
  summarise(
    n_hh         = n(),
    in_support   = mean(in_support),
    # p_stack: predicted P(ALSO burns solid fuel | main fuel = LPG) -- the
    # CONDITIONAL stacking estimand. stack_binary is only defined for main-fuel-LPG
    # households, so this is aggregated over NFHS households reporting LPG as their
    # primary fuel (lpg == 1). Averaging the conditional propensity over ALL
    # households (including solid-only ones) would NOT estimate district stacking
    # among LPG-main households; that alternative is kept separately as
    # p_stack_std_all and must never be labelled "stacking among LPG households".
    # NOTE: the target population (lpg == 1) is defined by the error-prone NFHS
    # main-fuel item; a corrected-latent-LPG standardization would need a joint model.
    p_stack_std_all = safe_wmean(p_stack, wt),                       # over all hh
    p_stack      = safe_wmean(ifelse(lpg == 1, p_stack, NA_real_), wt),  # among lpg==1
    # p_stacking below is the UNCONDITIONAL LPG+solid share from the multinomial --
    # a different estimand from p_stack; both kept, labelled distinctly everywhere.
    p_solid_only     = safe_wmean(p_solid_only,     wt),
    p_stacking       = safe_wmean(p_stacking,       wt),
    p_excl_lpg       = safe_wmean(p_excl_lpg,       wt),
    p_other_nonsolid = safe_wmean(p_other_nonsolid, wt),
    e_lpg_kg_yr  = safe_wmean(e_lpg_kg_yr,  wt),
    .groups = "drop"
  ) %>%
  mutate(p_any_solid_burning = p_solid_only + p_stacking)

corrected <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))
district_proxy <- district_proxy %>%
  left_join(corrected %>%
              select(district, any_of(c("lpg_2015_rc", "lpg_2019_rc",
                                        "lpg_2015_bayes", "lpg_2019_bayes"))),
            by = "district")

saveRDS(district_proxy, file.path(dir_out, "district_exposure_proxy.rds"))
write_csv(district_proxy, file.path(dir_out, "district_exposure_proxy.csv"))

saveRDS(list(m_stack_access = m_stack_access, m_stack_ires = m_stack_ires,
             m_stack_ires_all = m_stack_ires_all,
             m_3cat_access = m_3cat_access, m_3cat_access_ns = m_3cat_access_ns,
             m_3cat_ires_a = m_3cat_ires_a, m_3cat_ires_a_ns = m_3cat_ires_a_ns,
             h_ires = h_ires, h_ires_ns = h_ires_ns,
             validation = list(access = val_access, ires = val_ires,
                               cross_survey = cross_val)),
        file.path(dir_out, "stacking_models.rds"))

# ---- Validation figure: predicted vs observed district stacking ----------------
# Predict with re.form = NA (population-level FIXED effects only -- NO district or
# state random effect). NFHS-4 districts share the ACCESS district codes, so the
# default prediction reuses the very ACCESS district random effects that were
# estimated from the outcomes we validate against, making the figure an in-sample
# reconstruction. re.form = NA removes that circularity; the honest out-of-district
# generalization test is the leave-district-out AUC/district-r reported above.
predict_glmer_noRE <- function(m, nd) {
  ok  <- complete_rows(nd)
  out <- rep(NA_real_, nrow(nd))
  if (any(ok)) out[ok] <- predict(m, newdata = nd[ok, , drop = FALSE],
                                  type = "response", re.form = NA,
                                  allow.new.levels = TRUE)
  out
}
nfhs4_r$p_stack_noRE <- predict_glmer_noRE(m_stack_access, nfhs4_r)
val_stack_df <- nfhs4_r %>% filter(!is.na(district)) %>%
  group_by(district) %>%
  summarise(p_stack_noRE = safe_wmean(ifelse(lpg == 1, p_stack_noRE, NA_real_), wt),
            .groups = "drop") %>%
  mutate(district = as.character(district))
obs_access <- access_t %>% filter(!is.na(stack_binary)) %>%
  group_by(district) %>%
  summarise(obs_stack = mean(stack_binary), .groups = "drop") %>%
  mutate(district = as.character(district))
val_plot_df <- val_stack_df %>% inner_join(obs_access, by = "district")
if (nrow(val_plot_df) > 0) {
  pv <- ggplot(val_plot_df, aes(obs_stack, p_stack_noRE)) +
    geom_abline(linetype = 2, color = "grey40") +
    geom_point(alpha = .7) +
    labs(x = "Observed district stacking (ACCESS W1)",
         y = "Predicted district stacking (NFHS-4 rural; population-level, no district RE)") +
    theme_bw()
  ggsave(file.path(dir_out, "stacking_validation.jpeg"), pv,
         width = 6, height = 6, dpi = 300)
}

# ==============================================================================
# MODEL-CONSISTENT EXPOSURE PROXY FOR HEALTH CHANGE-ON-CHANGE (+ uncertainty)
# ------------------------------------------------------------------------------
# The district_proxy above is ERA-MATCHED (ACCESS models -> NFHS-4, IRES models
# -> NFHS-5), which is right for the descriptive proxy surfaces but NOT for a
# change-on-change health analysis: because the ACCESS and IRES models do not
# transfer across the PMUY transition (see cross-survey validation above), the
# NFHS-4 -> NFHS-5 CHANGE in predicted composition would conflate real change
# with the switch between two different models.
#
# Here we build a MODEL-CONSISTENT proxy: the SAME (IRES) composition models are
# applied to BOTH NFHS rounds, so the only things that differ between years are
# the household covariates and the corrected district-LPG context. We also
# propagate the Bayesian correction's uncertainty using the ACTUAL saved posterior
# draws of the corrected district prevalence (correction_posterior_draws.rds from
# 05) -- NOT a normal approximation from the CrI width -- re-predicting composition
# under each draw (N_DRAWS times). Outputs feed H5_health_nuanced.R.
#
# SCOPE OF THE UNCERTAINTY BAND: these draws vary ONLY the corrected district-LPG
# CONTEXT. The composition-model coefficients, the hurdle-model coefficients and
# smearing factor, and the predictor harmonization are all held fixed. So
# district_proxy_consistent_draws.rds is a CONTEXT (correction) uncertainty band,
# NOT total exposure-model uncertainty; the model-form/coefficient uncertainty is
# not included. Document it as such wherever it is reported.
# ==============================================================================
DO_CONSISTENT_HEALTH <- TRUE
# Posterior draws for context uncertainty. 200 to match the mortality MI (H2);
# this is the slow step of the script -- lower it only if runtime is a problem,
# and note it in the SI if you do.
N_DRAWS <- 200

if (DO_CONSISTENT_HEALTH) {
  message("\nBuilding model-consistent exposure proxy (IRES models, both rounds) ",
          "with ", N_DRAWS, " uncertainty draws -- this can take a while ...")

  # district-mean composition from a predicted household frame
  agg_comp <- function(df) df %>% filter(!is.na(district)) %>%
    group_by(district) %>%
    summarise(any_solid = safe_wmean(p_solid_only + p_stacking, wt),
              excl_lpg  = safe_wmean(p_excl_lpg,               wt),
              stacking  = safe_wmean(p_stacking,               wt),
              kg        = safe_wmean(e_lpg_kg_yr,              wt),
              .groups = "drop") %>%
    mutate(district = as.character(district))

  # predict composition for a frame using the IRES models (both rounds)
  predict_ires_comp <- function(nd) {
    m3 <- predict_multinom2(m_3cat_ires_a, m_3cat_ires_a_ns, nd, states_ires)
    nd$p_solid_only <- m3$p_solid_only
    nd$p_stacking   <- m3$p_stacking
    nd$p_excl_lpg   <- m3$p_excl_lpg
    nd$e_lpg_kg_yr  <- pred_kg(h_ires, h_ires_ns, nd, states_ires)
    nd
  }

  # working frames: BOTH rounds restricted to RURAL households, to match the
  # rural-only calibration (ACCESS is rural; NFHS-5 is calibrated against IRES
  # rural) and NFHS-4 rural, so the change-on-change compares a consistent
  # (rural) population across rounds rather than rural-2015 vs all-2019.
  n4 <- nfhs_t %>% filter(survey == "NFHS4", rural == 1)
  n5 <- nfhs_t %>% filter(survey == "NFHS5", rural == 1)

  # ---- point estimate (context = corrected posterior mean) --------------------
  c15 <- agg_comp(predict_ires_comp(n4)) %>% rename_with(~paste0(., "_2015"), -district)
  c19 <- agg_comp(predict_ires_comp(n5)) %>% rename_with(~paste0(., "_2019"), -district)
  proxy_consistent <- full_join(c15, c19, by = "district") %>%
    mutate(change_any_solid = 100 * (any_solid_2019 - any_solid_2015),
           change_excl_lpg  = 100 * (excl_lpg_2019  - excl_lpg_2015),
           change_stacking  = 100 * (stacking_2019  - stacking_2015),
           change_kg        =        kg_2019        - kg_2015)
  saveRDS(proxy_consistent, file.path(dir_out, "district_exposure_proxy_consistent.rds"))
  write_csv(proxy_consistent, file.path(dir_out, "district_exposure_proxy_consistent.csv"))

  # ---- posterior draws: the ACTUAL saved brms posterior of the corrected
  #      district prevalence (05 -> correction_posterior_draws.rds), NOT a normal
  #      approximation from the CrI width. y2015/y2019 are (n_post x n_districts)
  #      probability-scale draw matrices; we name their columns by district and
  #      re-predict composition under each draw's district-LPG context.
  pd <- readRDS(file.path(dir_out, "correction_posterior_draws.rds"))
  draws15 <- pd$y2015; colnames(draws15) <- as.character(pd$districts_2015)
  draws19 <- pd$y2019; colnames(draws19) <- as.character(pd$districts_2019)
  n_post  <- min(nrow(draws15), nrow(draws19))
  # Deterministically thin to N_DRAWS evenly spaced posterior rows (no RNG, so the
  # draws are bit-reproducible given the saved posterior file).
  draw_rows <- unique(round(seq(1, n_post, length.out = min(N_DRAWS, n_post))))
  # NOTE: the 2015 and 2019 correction models are fit SEPARATELY, so pairing row r
  # of each is not a genuine joint across-year posterior draw (there is no
  # cross-year covariance to carry). Using the real per-year draws is nonetheless
  # far better than independent normal-from-CrI draws, which also discard the
  # within-year across-district covariance and the probability-scale skew.
  k4 <- as.character(n4$district); k5 <- as.character(n5$district)
  draw_one <- function(m, r) {
    dd15 <- draws15[r, ]; dd19 <- draws19[r, ]   # named by district
    n4$dist_lpg <- unname(dd15[k4]); n5$dist_lpg <- unname(dd19[k5])
    a15 <- agg_comp(predict_ires_comp(n4)) %>% rename_with(~paste0(., "_2015"), -district)
    a19 <- agg_comp(predict_ires_comp(n5)) %>% rename_with(~paste0(., "_2019"), -district)
    full_join(a15, a19, by = "district") %>%
      transmute(draw = m, district,
                change_any_solid = 100 * (any_solid_2019 - any_solid_2015),
                change_excl_lpg  = 100 * (excl_lpg_2019  - excl_lpg_2015),
                change_stacking  = 100 * (stacking_2019  - stacking_2015),
                change_kg        =        kg_2019        - kg_2015)
  }
  draws_df <- purrr::map_dfr(seq_along(draw_rows), function(m) {
    if (m %% 5 == 0) message("  draw ", m, "/", length(draw_rows))
    draw_one(m, draw_rows[m])
  })
  saveRDS(draws_df, file.path(dir_out, "district_proxy_consistent_draws.rds"))
  message("Model-consistent proxy done: district_exposure_proxy_consistent.rds (+ draws).")
}

## ---- CHECKS ------------------------------------------------------------------
chk_header("06_stacking_prediction")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("06", "06_stacking_prediction")

# The transfer diagnostic (21) consumes these; if W2 came back empty the era
# contrast silently degenerates to a one-frame comparison, so check it here.
chk_file("06", "harmonized frames exported for the transfer diagnostic",
         "harmonized_frames.rds")
# Which covariates the correction model was actually built on is a first-order
# fact about the estimate, and until now it lived only in a console message. A
# predictor silently dropped in one round and kept in another is a different
# model than the one the manuscript describes, so the set is recorded in the
# check trail and the drops are named.
chk("06", "at least four shared predictors survived the usability screen",
    length(predictors) >= 4,
    paste0("used (", length(predictors), "): ", paste(predictors, collapse = ", "),
           if (length(dropped))
             paste0("  |  DROPPED (", length(dropped), "): ",
                    paste(dropped, collapse = ", ")) else "  |  none dropped"))
chk_warn("06", "no candidate predictor dropped for lack of variation",
    length(dropped) == 0,
    if (length(dropped))
      paste0(paste(dropped, collapse = ", "),
             " -- constant or all-missing in at least one survey; ",
             "see the [predictors] table above for which one.")
    else "all 8 candidates usable in ACCESS, IRES and NFHS")

chk("06", "ACCESS wave 2 (2018) harmonized and non-empty",
    nrow(access_t_w2) > 0 && sum(!is.na(access_t_w2$stack_binary)) > 0,
    sprintf("W2 rows = %d, with observed stack_binary = %d",
            nrow(access_t_w2), sum(!is.na(access_t_w2$stack_binary))))

chk("06", "district proxy carries 4th composition category",
    chk_has_cols(district_proxy, c("p_solid_only","p_stacking","p_excl_lpg",
                                   "p_other_nonsolid")))
chk("06", "four composition shares sum to ~1",
    { s <- with(district_proxy, p_solid_only + p_stacking + p_excl_lpg + p_other_nonsolid)
      max(abs(s - 1), na.rm = TRUE) < 0.02 },
    { s <- with(district_proxy, p_solid_only + p_stacking + p_excl_lpg + p_other_nonsolid)
      sprintf("max |sum-1| = %.4f", max(abs(s - 1), na.rm = TRUE)) })
chk("06", "conditional stacking + standardized variant both present",
    chk_has_cols(district_proxy, c("p_stack","p_stack_std_all")))
chk("06", "all composition shares in [0,1]",
    chk_in_range(district_proxy$p_excl_lpg, 0, 1) &&
    chk_in_range(district_proxy$p_other_nonsolid, 0, 1))
chk("06", "model-consistent proxy + real posterior draws written",
    file.exists(file.path(dir_out, "district_exposure_proxy_consistent.rds")) &&
    file.exists(file.path(dir_out, "district_proxy_consistent_draws.rds")))
chk_warn("06", "stacking validation figure written",
    file.exists(file.path(dir_out, "stacking_validation.jpeg")))

message("06_stacking_prediction.R done.")
