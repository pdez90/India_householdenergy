# ==============================================================================
# 21_transfer_diagnostics.R   (STANDALONE -- does not source 00_config.R)
#
# QUESTION: 06 reports that an IRES-trained stacking model does not transfer to
# ACCESS. That is a single aggregate number and it does not say WHY, or whether
# ANY part of the covariate -> fuel-use relationship is stable. This script
# answers three questions the aggregate number cannot:
#
#   (Q1) Is the failure an ERA effect or an INSTRUMENT effect?
#   (Q2) Which individual predictors transfer, and which do not?
#   (Q3) Is the failure a shift in the PREDICTOR DISTRIBUTIONS (fixable by
#        reweighting) or in the PREDICTOR -> OUTCOME RELATIONSHIP (not fixable)?
#
# ---- (Q1) THE IDENTIFICATION IDEA -------------------------------------------
# The cross-survey test in 06 compares IRES (2019-20) against ACCESS wave 1
# (2014-15). Two things differ at once: the era (spanning PMUY, which added tens
# of millions of LPG connections from mid-2016) and the instrument (a different
# questionnaire, sample frame and fieldwork operation). A failure could be
# either, and the paper cannot say which.
#
# ACCESS wave 2 (2018) breaks that tie. It shares ACCESS's instrument with wave
# 1 but sits on the far side of PMUY, close to IRES in time:
#
#                       2015 era          2018-2020 era
#     ACCESS instrument  W1 (wave 0)       W2 (wave 1)
#     IRES instrument    --                IRES (2019-20)
#
#     W1 -> W2   : ERA contrast        -- instrument held constant
#     W2 -> IRES : INSTRUMENT contrast -- era approximately held constant
#     W1 -> IRES : BOTH                -- the comparison 06 reports
#
# If W1 -> W2 transfers poorly, the relationship genuinely moved across PMUY and
# no amount of harmonization will fix it -- the surveys are not measuring a
# stable quantity to transport. If W1 -> W2 transfers well but W2 -> IRES does
# not, the problem is instrument comparability, which harmonization CAN address.
#
# IMPORTANT CAVEAT, and it limits every conclusion below: "era approximately
# held constant" is approximate. W2 (2018) and IRES (2019-20) are 1-2 years
# apart during a period of continuing rapid change, so the instrument contrast
# still carries some residual era signal and will OVERSTATE the instrument
# effect. The era contrast (W1 -> W2) is clean by comparison, because the
# instrument really is identical. Read the era contrast as the sharper of the
# two.
#
# ---- SCOPE ------------------------------------------------------------------
# ACCESS is rural and covers 6 northern states. Every contrast is therefore run
# on the COMMON STATE SET (and reported alongside the unrestricted IRES-rural
# version as a sensitivity), so that "does not transfer" cannot be an artefact
# of comparing 6 northern states against 21 states of a different India.
#
# The district-context predictor dist_lpg is EXCLUDED throughout (rhs_nc). It is
# a district-level mean of the outcome's own parent variable and its scale is
# era-specific by construction, so including it would guarantee a spurious
# transfer failure. This is the same choice 06 makes for its cross-survey test.
#
# INPUT   harmonized_frames.rds   (written by 06; harmonize() is NOT duplicated
#                                  here, deliberately -- one definition only)
# OUTPUT  diagnostics/transfer_predictive.csv
#         diagnostics/transfer_coefficients.csv
#         diagnostics/transfer_covariate_shift.csv
#         diagnostics/transfer_loo_predictor.csv
#         diagnostics/transfer_summary.txt
#         diagnostics/fig_transfer_diagnostics.pdf
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(lme4); library(ggplot2); library(tidyr)
})

dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
if (file.exists("checks.R")) {
  source("checks.R")
} else {
  # Run from another working directory the checks helpers are simply absent. The
  # diagnostic itself is still valid, so degrade to no-op stubs and say so, rather
  # than erroring in the CHECKS block after all the modelling has already run.
  message("checks.R not found -- pipeline self-checks disabled for this run.")
  chk <- chk_warn <- function(script, label, ok, detail = "")
    cat(sprintf("  [%s] %-52s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label, detail))
  chk_header <- function(script) cat(sprintf("\n== CHECKS [%s] ==\n", script))
  chk_file   <- function(script, label, relpath)
    chk(script, label, file.exists(file.path(dir_out, relpath)), relpath)
}

# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "21_transfer_diagnostics"

diag_dir <- file.path(dir_out, "diagnostics")
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(2026)

f_in <- file.path(dir_out, "harmonized_frames.rds")
if (!file.exists(f_in))
  stop("harmonized_frames.rds not found in ", dir_out,
       ".\n  Run 06_stacking_prediction.R first -- it writes the harmonized ",
       "frames this script consumes.", call. = FALSE)
H <- readRDS(f_in)

predictors <- H$predictors
rhs_nc     <- H$rhs_nc
pred_nc    <- setdiff(predictors, "dist_lpg")

message("Transfer diagnostic -- context-free predictor set: ",
        paste(pred_nc, collapse = ", "))

# ---- Frames ------------------------------------------------------------------
# Keep only rows with an observed outcome and complete predictors, so every
# model below is fit and scored on exactly the same rows. Doing this once here
# (rather than per model) means the LOO comparison in section 5 is not
# contaminated by a changing complete-case frame as predictors are dropped.
prep <- function(d, tag) {
  d <- d %>% filter(!is.na(stack_binary))
  ok <- complete.cases(d[, pred_nc, drop = FALSE])
  d <- d[ok, , drop = FALSE]
  d$state    <- droplevels(factor(d$state))
  d$district <- droplevels(factor(as.character(d$district)))
  d$.frame   <- tag
  d
}
A1 <- prep(H$access_w1, "ACCESS_W1_2015")
A2 <- prep(H$access_w2, "ACCESS_W2_2018")
# ACCESS is rural-only, so IRES is restricted to rural to match. If 03 could not
# resolve the rural/urban flag it sets ires$rural to NA for every row, and a
# silent filter(rural == 1) would return zero rows and take the entire IRES side
# of this diagnostic down with it. Fall back to all IRES, loudly, and carry the
# fact into the frame tag so the output tables say which was used.
.ires_src <- H$ires
.n_rural  <- sum(.ires_src$rural == 1, na.rm = TRUE)
if (.n_rural > 0) {
  IRa <- prep(.ires_src %>% filter(rural == 1), "IRES_rural_all")
  IRES_SCOPE <- "rural"
} else {
  warning("IRES rural flag unusable (0 rows with rural == 1) -- using ALL IRES ",
          "households. The instrument contrast now also spans urban/rural.",
          call. = FALSE)
  IRa <- prep(.ires_src, "IRES_allsector_all")
  IRES_SCOPE <- "all sectors (rural flag unusable)"
}

# Common-state restriction: the primary comparison. ACCESS covers 6 northern
# states; scoring an all-India IRES model against them confounds "does not
# transfer across time/instrument" with "does not transfer across regions".
common_states <- sort(intersect(intersect(levels(A1$state), levels(A2$state)),
                                levels(IRa$state)))
restrict <- function(d, tag) {
  d <- d %>% filter(as.character(state) %in% common_states)
  d$state <- droplevels(factor(as.character(d$state)))
  d$.frame <- tag; d
}
A1c <- restrict(A1, "ACCESS_W1_2015")
A2c <- restrict(A2, "ACCESS_W2_2018")
IRc <- restrict(IRa, "IRES_rural_common")

message(sprintf("Common states (n=%d): %s", length(common_states),
                paste(common_states, collapse = ", ")))
message(sprintf("Frame sizes (common states) -- W1: %d, W2: %d, IRES: %d",
                nrow(A1c), nrow(A2c), nrow(IRc)))
if (nrow(A2c) == 0)
  warning("ACCESS wave 2 is empty after restriction -- the ERA contrast, the ",
          "whole point of this script, cannot be estimated. Check that ",
          "02_prep_access.R retained wave == 1 rows.")

# ==============================================================================
# 1. PREDICTIVE TRANSFER
# ==============================================================================
# Train on one frame, score on another, and report four things rather than one,
# because they fail independently and imply different remedies:
#
#   AUC              -- does the model get the ORDERING of households right?
#   Brier            -- overall probabilistic accuracy (ordering + calibration)
#   calibration slope-- regress observed on the predicted log-odds. Slope 1 =
#                       the relationship is on the right scale; slope < 1 = the
#                       model is over-confident in the target; slope near 0 =
#                       the predictors carry no usable signal in the target.
#   district-level r -- the quantity the pipeline actually needs, since the
#                       stacking surface is consumed as a DISTRICT aggregate.
#
# A model can transfer at the district level while failing at the household
# level, and vice versa. Reporting only one of them is how a transfer failure
# gets missed.
fit_nc <- function(dat, label) {
  m <- glmer(as.formula(paste("stack_binary ~", rhs_nc,
                              "+ (1 | state) + (1 | district)")),
             data = dat, family = binomial, nAGQ = 0,
             control = glmerControl(optimizer = "nloptwrap"))
  if (exists("chk_record_fit"))
    chk_record_fit(.chk_tag("21_transfer_diagnostics"),
                   paste0("transfer_stack:", label), m,
                   extra = sprintf("n=%d, districts=%d", nrow(dat),
                                   nlevels(dat$district)))
  m
}

score <- function(m, newdat, train_lab, test_lab) {
  p <- suppressWarnings(
    predict(m, newdata = newdat, type = "response", allow.new.levels = TRUE))
  y <- newdat$stack_binary
  ok <- is.finite(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]; dd <- droplevels(newdat$district[ok])
  if (!length(p) || length(unique(y)) < 2)
    return(data.frame(train = train_lab, test = test_lab, n = length(p),
                      auc = NA_real_, brier = NA_real_, cal_slope = NA_real_,
                      district_r = NA_real_, district_rmse = NA_real_,
                      n_districts = NA_integer_))
  pc <- pmin(pmax(p, 1e-6), 1 - 1e-6)          # clamp before qlogis
  auc <- tryCatch(as.numeric(pROC::auc(y, p, quiet = TRUE)),
                  error = function(e) NA_real_)
  cal <- tryCatch(unname(coef(glm(y ~ qlogis(pc), family = binomial))[2]),
                  error = function(e) NA_real_)
  agg <- data.frame(district = dd, y = y, p = p) %>%
    group_by(district) %>%
    summarise(obs = mean(y), hat = mean(p), .groups = "drop")
  data.frame(train = train_lab, test = test_lab, n = length(p),
             auc = auc, brier = mean((p - y)^2),
             cal_slope = cal,
             district_r = suppressWarnings(
               cor(agg$obs, agg$hat, use = "complete.obs")),
             district_rmse = sqrt(mean((agg$obs - agg$hat)^2, na.rm = TRUE)),
             n_districts = nrow(agg))
}

m_A1 <- fit_nc(A1c, "ACCESS_W1_common")
m_A2 <- if (nrow(A2c) > 0) fit_nc(A2c, "ACCESS_W2_common") else NULL
m_IR <- fit_nc(IRc, "IRES_rural_common")
m_IRall <- fit_nc(IRa, "IRES_rural_all")

# m_A2 is NULL when wave 2 did not survive the restriction, and any frame can be
# empty. score() would error on either, taking down the whole table -- including
# the contrasts that ARE estimable. Return an explicit all-NA row instead, so the
# output says "not estimable" rather than not existing.
score_safe <- function(m, newdat, train_lab, test_lab) {
  if (is.null(m) || is.null(newdat) || !nrow(newdat))
    return(data.frame(train = train_lab, test = test_lab, n = 0L,
                      auc = NA_real_, brier = NA_real_, cal_slope = NA_real_,
                      district_r = NA_real_, district_rmse = NA_real_,
                      n_districts = NA_integer_))
  tryCatch(score(m, newdat, train_lab, test_lab),
           error = function(e) {
             message("score(", train_lab, " -> ", test_lab, ") failed: ",
                     conditionMessage(e))
             data.frame(train = train_lab, test = test_lab, n = NA_integer_,
                        auc = NA_real_, brier = NA_real_, cal_slope = NA_real_,
                        district_r = NA_real_, district_rmse = NA_real_,
                        n_districts = NA_integer_)
           })
}

# In-sample rows are reported for reference only. They are optimistic by
# construction (the district random effect has seen these districts) and must
# never be read as transfer performance -- flagged in the `kind` column.
pred_rows <- list(
  cbind(score_safe(m_IR, A1c, "IRES_common",   "ACCESS_W1_2015"), kind = "ERA+INSTRUMENT"),
  cbind(score_safe(m_IR, A2c, "IRES_common",   "ACCESS_W2_2018"), kind = "INSTRUMENT"),
  cbind(score_safe(m_A1, A2c, "ACCESS_W1",     "ACCESS_W2_2018"), kind = "ERA"),
  cbind(score_safe(m_A2, A1c, "ACCESS_W2",     "ACCESS_W1_2015"), kind = "ERA (reverse)"),
  cbind(score_safe(m_A1, IRc, "ACCESS_W1",     "IRES_common"),    kind = "ERA+INSTRUMENT (reverse)"),
  cbind(score_safe(m_A2, IRc, "ACCESS_W2",     "IRES_common"),    kind = "INSTRUMENT (reverse)"),
  cbind(score_safe(m_IRall, A1c, "IRES_all",   "ACCESS_W1_2015"), kind = "ERA+INSTRUMENT, unrestricted IRES"),
  cbind(score_safe(m_IRall, A2c, "IRES_all",   "ACCESS_W2_2018"), kind = "INSTRUMENT, unrestricted IRES"),
  cbind(score_safe(m_A1, A1c, "ACCESS_W1",     "ACCESS_W1_2015"), kind = "in-sample (reference only)"),
  cbind(score_safe(m_A2, A2c, "ACCESS_W2",     "ACCESS_W2_2018"), kind = "in-sample (reference only)"),
  cbind(score_safe(m_IR, IRc, "IRES_common",   "IRES_common"),    kind = "in-sample (reference only)"))
pred_tab <- bind_rows(Filter(function(z) !is.null(z) && nrow(z) > 0, pred_rows))
write.csv(pred_tab, file.path(diag_dir, "transfer_predictive.csv"), row.names = FALSE)

cat("\n== Predictive transfer (context-free predictors, common states) ==\n")
print(pred_tab %>%
        mutate(across(c(auc, brier, cal_slope, district_r, district_rmse),
                      ~ round(.x, 3))), row.names = FALSE)

# ==============================================================================
# 2. PER-PREDICTOR COEFFICIENT STABILITY
# ==============================================================================
# Fit the SAME specification separately in each frame and compare coefficients
# term by term. A predictor "transfers" if its coefficient is statistically
# indistinguishable across frames, same sign, and similar magnitude.
#
# The test is the standard two-sample z on independent estimates:
#     z = (b1 - b2) / sqrt(se1^2 + se2^2)
#
# For the ERA contrast (W1 vs W2) the two estimates are NOT independent: ACCESS
# is a panel, so many households appear in both waves and the estimates are
# positively correlated. Ignoring that covariance OVERSTATES Var(b1 - b2), which
# makes the era test CONSERVATIVE -- it will under-reject, not over-reject. A
# significant era difference is therefore trustworthy; a null one is weaker
# evidence than it looks. The instrument contrast (W2 vs IRES) uses genuinely
# independent samples and needs no such caveat.
coef_of <- function(m, lab) {
  b  <- lme4::fixef(m)
  se <- sqrt(diag(as.matrix(vcov(m))))
  data.frame(term = names(b), est = as.numeric(b),
             se = as.numeric(se[names(b)]), frame = lab,
             stringsAsFactors = FALSE)
}
co <- bind_rows(coef_of(m_A1, "ACCESS_W1_2015"),
                if (!is.null(m_A2)) coef_of(m_A2, "ACCESS_W2_2018"),
                coef_of(m_IR, "IRES_rural_common"))

contrast <- function(co, f1, f2, label, note) {
  a <- co %>% filter(frame == f1) %>% select(term, e1 = est, s1 = se)
  b <- co %>% filter(frame == f2) %>% select(term, e2 = est, s2 = se)
  m <- inner_join(a, b, by = "term") %>% filter(term != "(Intercept)")
  if (!nrow(m)) return(NULL)
  m %>% mutate(
    contrast = label, note = note,
    diff = e1 - e2,
    se_diff = sqrt(s1^2 + s2^2),
    z = diff / se_diff,
    p = 2 * pnorm(-abs(z)),
    same_sign = sign(e1) == sign(e2),
    # "Transfers" is deliberately a conjunction, not just p > 0.05: a coefficient
    # can be non-significantly different simply because both estimates are noisy.
    # Require the signs to agree AND the difference to be small relative to the
    # effect itself, so that "no evidence of difference" is not read as
    # "evidence of no difference".
    transfers = same_sign & p > 0.05 &
                abs(diff) < 0.5 * pmax(abs(e1), abs(e2), 0.1))
}
cmp <- bind_rows(
  contrast(co, "ACCESS_W1_2015", "ACCESS_W2_2018", "ERA (W1 vs W2)",
           "same instrument; panel-correlated, so conservative"),
  contrast(co, "ACCESS_W2_2018", "IRES_rural_common", "INSTRUMENT (W2 vs IRES)",
           "independent samples; residual 1-2yr era drift remains"),
  contrast(co, "ACCESS_W1_2015", "IRES_rural_common", "ERA+INSTRUMENT (W1 vs IRES)",
           "the comparison 06 reports"))
if (!is.null(cmp) && nrow(cmp)) {
  cmp <- cmp %>% arrange(contrast, p)
  write.csv(cmp, file.path(diag_dir, "transfer_coefficients.csv"), row.names = FALSE)
  cat("\n== Per-predictor coefficient stability ==\n")
  print(cmp %>% mutate(across(c(e1, e2, diff, z), ~ round(.x, 3)),
                       p = signif(p, 3)) %>%
          select(contrast, term, e1, e2, diff, z, p, same_sign, transfers),
        row.names = FALSE)
  cat("\nPredictors whose coefficient is STABLE on the instrument contrast",
      "(the harmonization-fixable one):\n  ")
  st <- cmp %>% filter(contrast == "INSTRUMENT (W2 vs IRES)", transfers) %>% pull(term)
  cat(if (length(st)) paste(st, collapse = ", ") else "(none)", "\n")
}

# ==============================================================================
# 3. COVARIATE SHIFT vs CONCEPT SHIFT
# ==============================================================================
# Two different failures wear the same aggregate symptom, and they have
# different remedies:
#
#   COVARIATE SHIFT  P(X) differs between surveys -- the populations differ, but
#                    the relationship is intact. Fixable by reweighting or by
#                    restricting to the region of common support.
#   CONCEPT SHIFT    P(Y|X) differs -- the relationship itself moved. NOT fixable
#                    by reweighting; the model is simply not transportable.
#
# Section 2 measures concept shift. This section measures covariate shift, so
# the two can be told apart. A predictor that shows large covariate shift and no
# concept shift is a candidate for a reweighted transfer model; one that shows
# concept shift is not, no matter what you do to the weights.
shift_one <- function(v) {
  g <- function(d) {
    x <- d[[v]]
    if (is.factor(x) || is.character(x)) {
      tb <- prop.table(table(as.character(x)))
      setNames(as.numeric(tb), names(tb))
    } else c(mean = mean(as.numeric(x), na.rm = TRUE))
  }
  fr <- list(ACCESS_W1_2015 = A1c, ACCESS_W2_2018 = A2c, IRES_rural_common = IRc)
  fr <- fr[vapply(fr, nrow, integer(1)) > 0]
  vals <- lapply(fr, g)
  keys <- sort(unique(unlist(lapply(vals, names))))
  out <- data.frame(variable = v, level = keys, stringsAsFactors = FALSE)
  for (nm in names(vals)) out[[nm]] <- unname(vals[[nm]][keys])
  out
}
shift <- bind_rows(lapply(pred_nc, shift_one))
# Standardized shift on the two contrasts, so covariate shift is directly
# comparable with the concept shift in section 2.
if (all(c("ACCESS_W1_2015","ACCESS_W2_2018") %in% names(shift)))
  shift$shift_era <- shift$ACCESS_W2_2018 - shift$ACCESS_W1_2015
if (all(c("ACCESS_W2_2018","IRES_rural_common") %in% names(shift)))
  shift$shift_instrument <- shift$IRES_rural_common - shift$ACCESS_W2_2018
write.csv(shift, file.path(diag_dir, "transfer_covariate_shift.csv"), row.names = FALSE)
cat("\n== Covariate shift (predictor distributions by frame) ==\n")
print(shift %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)

# ==============================================================================
# 4. OUTCOME BASE RATES
# ==============================================================================
# Before blaming the predictors: if the stacking RATE itself moved sharply, an
# intercept shift alone will destroy calibration while leaving every slope
# intact. That is a recalibration problem, not a transportability problem, and
# it is worth separating out.
base_rates <- bind_rows(lapply(
  list(ACCESS_W1_2015 = A1c, ACCESS_W2_2018 = A2c, IRES_rural_common = IRc,
       IRES_rural_all = IRa),
  function(d) if (!nrow(d)) NULL else
    data.frame(n = nrow(d), stacking_rate = mean(d$stack_binary, na.rm = TRUE),
               n_districts = nlevels(droplevels(d$district)))),
  .id = "frame")
cat("\n== Outcome base rates ==\n")
print(base_rates %>% mutate(stacking_rate = round(stacking_rate, 3)), row.names = FALSE)

# ==============================================================================
# 5. LEAVE-ONE-PREDICTOR-OUT CONTRIBUTION TO TRANSFER
# ==============================================================================
# Sections 2-3 are about coefficients and distributions. This one is about what
# the pipeline actually cares about: does dropping a predictor make the
# cross-survey prediction BETTER or WORSE?
#
# Drop predictor j from the IRES training model, re-score in ACCESS, and compare
# the district-level r with the full model. A predictor with a NEGATIVE
# contribution is actively harming transfer -- the model is better off without
# it -- which is a concrete, actionable finding in a way that a coefficient
# p-value is not.
loo <- NULL
if (length(pred_nc) >= 2) {
  loo <- bind_rows(lapply(pred_nc, function(v) {
    keep <- setdiff(pred_nc, v)
    m <- tryCatch(
      glmer(as.formula(paste("stack_binary ~", paste(keep, collapse = " + "),
                             "+ (1 | state) + (1 | district)")),
            data = IRc, family = binomial, nAGQ = 0,
            control = glmerControl(optimizer = "nloptwrap")),
      error = function(e) NULL)
    if (is.null(m)) return(NULL)
    s1 <- score_safe(m, A1c, "IRES_common_minus", "ACCESS_W1_2015")
    s2 <- if (nrow(A2c) > 0) score_safe(m, A2c, "IRES_common_minus", "ACCESS_W2_2018") else NULL
    data.frame(dropped = v,
               district_r_W1 = s1$district_r,
               district_r_W2 = if (is.null(s2)) NA_real_ else s2$district_r,
               auc_W1 = s1$auc,
               auc_W2 = if (is.null(s2)) NA_real_ else s2$auc)
  }))
  full_W1 <- pred_tab$district_r[pred_tab$train == "IRES_common" &
                                 pred_tab$test == "ACCESS_W1_2015"][1]
  full_W2 <- pred_tab$district_r[pred_tab$train == "IRES_common" &
                                 pred_tab$test == "ACCESS_W2_2018"][1]
  if (!is.null(loo) && nrow(loo)) {
    # contribution = full - without. POSITIVE => the predictor HELPS transfer.
    loo$contrib_W1 <- full_W1 - loo$district_r_W1
    loo$contrib_W2 <- full_W2 - loo$district_r_W2
    write.csv(loo, file.path(diag_dir, "transfer_loo_predictor.csv"), row.names = FALSE)
    cat("\n== Leave-one-predictor-out: contribution to cross-survey district r ==\n")
    cat("   (positive contribution = predictor HELPS transfer; negative = it HURTS)\n")
    print(loo %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
  }
}

# ==============================================================================
# 6. A TRANSFERABLE-SUBSET MODEL
# ==============================================================================
# The constructive payoff. Keep only the predictors that (a) are coefficient-
# stable on the instrument contrast and (b) do not hurt transfer in section 5,
# refit on IRES, and score in ACCESS.
#
# Both outcomes are informative and both belong in the SI:
#   * If transfer r improves materially, the stacking surface CAN be made
#     transportable by restricting the predictor set, and the paper should say
#     which predictors survive.
#   * If it does not improve, the failure is GLOBAL rather than attributable to
#     a few bad predictors -- which is a stronger and more honest negative
#     result, and it directly supports the manuscript's decision to treat the
#     stacking surface as exploratory.
stable <- character(0)
if (!is.null(cmp) && nrow(cmp))
  stable <- cmp %>% filter(contrast == "INSTRUMENT (W2 vs IRES)", transfers) %>% pull(term)
# Coefficient names are not variable names for factors (caste3General etc.), so
# map back to the variable that generated each term before subsetting.
term_to_var <- function(terms, vars) {
  vapply(terms, function(t) {
    hit <- vars[vapply(vars, function(v) startsWith(t, v), logical(1))]
    if (!length(hit)) NA_character_ else hit[which.max(nchar(hit))]
  }, character(1))
}
stable_vars <- unique(na.omit(term_to_var(stable, pred_nc)))
if (!is.null(loo) && nrow(loo)) {
  harmful <- loo$dropped[which(loo$contrib_W1 < 0)]
  stable_vars <- setdiff(stable_vars, harmful)
}
sub_res <- NULL
if (length(stable_vars) >= 1 && length(stable_vars) < length(pred_nc)) {
  m_sub <- tryCatch(
    glmer(as.formula(paste("stack_binary ~", paste(stable_vars, collapse = " + "),
                           "+ (1 | state) + (1 | district)")),
          data = IRc, family = binomial, nAGQ = 0,
          control = glmerControl(optimizer = "nloptwrap")),
    error = function(e) NULL)
  if (!is.null(m_sub)) {
    if (exists("chk_record_fit"))
      chk_record_fit(.chk_tag("21_transfer_diagnostics"),
                     "transfer_stack:IRES_transferable_subset", m_sub,
                     extra = paste("vars:", paste(stable_vars, collapse = "+")))
    sub_res <- bind_rows(
      cbind(score_safe(m_sub, A1c, "IRES_subset", "ACCESS_W1_2015"), kind = "transferable subset"),
      if (nrow(A2c) > 0)
        cbind(score_safe(m_sub, A2c, "IRES_subset", "ACCESS_W2_2018"), kind = "transferable subset"))
    cat("\n== Transferable-subset model ==\n")
    cat("   predictors retained: ", paste(stable_vars, collapse = ", "), "\n")
    print(sub_res %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
    pred_tab <- bind_rows(pred_tab, sub_res)
    write.csv(pred_tab, file.path(diag_dir, "transfer_predictive.csv"), row.names = FALSE)
  }
} else {
  cat("\n== Transferable-subset model ==\n   Not fitted: ",
      if (!length(stable_vars)) "no predictor passed the stability screen"
      else "every predictor passed, so the subset equals the full model", "\n")
}

# ==============================================================================
# 7. FIGURE
# ==============================================================================
pdf(file.path(diag_dir, "fig_transfer_diagnostics.pdf"), width = 11, height = 8)
tryCatch({
  pl <- pred_tab %>%
    filter(!grepl("in-sample", kind)) %>%
    mutate(lab = paste0(train, " -> ", test)) %>%
    filter(is.finite(district_r))
  if (nrow(pl))
    print(ggplot(pl, aes(x = reorder(lab, district_r), y = district_r, fill = kind)) +
            geom_col() + coord_flip() +
            geom_hline(yintercept = 0, linewidth = 0.3) +
            labs(title = "Cross-survey transfer of the stacking model",
                 subtitle = paste("District-level correlation between predicted and",
                                  "observed stacking share.\nERA = instrument held",
                                  "constant (ACCESS W1 2015 vs W2 2018);",
                                  "INSTRUMENT = era approximately held constant."),
                 x = NULL, y = "District-level r (predicted vs observed)",
                 fill = "Contrast") +
            theme_minimal(base_size = 11))
}, error = function(e) message("Figure panel skipped: ", conditionMessage(e)))
tryCatch({
  if (!is.null(cmp) && nrow(cmp))
    print(ggplot(cmp, aes(x = e1, y = e2, colour = transfers)) +
            geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
            geom_point(size = 2) +
            facet_wrap(~ contrast, scales = "free") +
            labs(title = "Coefficient stability across surveys and eras",
                 subtitle = paste("Points on the dashed line have identical",
                                  "coefficients in the two frames.\nOff-diagonal =",
                                  "the predictor-outcome relationship moved."),
                 x = "Coefficient in first frame", y = "Coefficient in second frame",
                 colour = "Transfers") +
            theme_minimal(base_size = 11))
}, error = function(e) message("Coefficient panel skipped: ", conditionMessage(e)))
dev.off()

# ==============================================================================
# 8. PLAIN-LANGUAGE SUMMARY
# ==============================================================================
gv <- function(tr, te) {
  v <- pred_tab$district_r[pred_tab$train == tr & pred_tab$test == te]
  if (!length(v)) NA_real_ else v[1]
}
r_era   <- gv("ACCESS_W1", "ACCESS_W2_2018")
r_instr <- gv("IRES_common", "ACCESS_W2_2018")
r_both  <- gv("IRES_common", "ACCESS_W1_2015")

verdict <- if (any(is.na(c(r_era, r_instr, r_both)))) {
  "Not all three contrasts estimable -- inspect transfer_predictive.csv."
} else if (r_era > 0.5 && r_instr < 0.3) {
  paste0("INSTRUMENT-DOMINATED. The relationship is stable across the PMUY era ",
         "within ACCESS (r = ", round(r_era, 2), ") but does not survive the ",
         "move to IRES (r = ", round(r_instr, 2), "). This is a harmonization ",
         "problem, and the transferable-subset model above is the place to look ",
         "for a fix.")
} else if (r_era < 0.3) {
  paste0("ERA-DOMINATED. Transfer fails even with the instrument held constant ",
         "(ACCESS W1 -> W2, r = ", round(r_era, 2), "), so the covariate-outcome ",
         "relationship genuinely moved across the PMUY transition. No ",
         "harmonization of the two instruments can fix this, and the stacking ",
         "surface should not be transported across eras.")
} else {
  paste0("MIXED. Era contrast r = ", round(r_era, 2), ", instrument contrast r = ",
         round(r_instr, 2), ", combined r = ", round(r_both, 2),
         ". Both channels contribute; report all three.")
}

summary_txt <- c(
  "================ CROSS-SURVEY TRANSFER DIAGNOSTIC ================",
  paste0("Common states (", length(common_states), "): ",
         paste(common_states, collapse = ", ")),
  paste0("Context-free predictors: ", paste(pred_nc, collapse = ", ")),
  paste0("IRES scope: ", IRES_SCOPE, "  (ACCESS is rural-only by design)"),
  paste0("Frame sizes (common states): W1 = ", nrow(A1c), ", W2 = ", nrow(A2c),
         ", IRES = ", nrow(IRc)),
  "",
  "District-level r (predicted vs observed stacking share):",
  sprintf("  ERA contrast        ACCESS W1 2015 -> W2 2018   r = %s", round(r_era, 3)),
  sprintf("  INSTRUMENT contrast IRES -> ACCESS W2 2018      r = %s", round(r_instr, 3)),
  sprintf("  BOTH (as in 06)     IRES -> ACCESS W1 2015      r = %s", round(r_both, 3)),
  "",
  "VERDICT:", paste0("  ", verdict),
  "",
  "Coefficient-stable predictors on the instrument contrast:",
  paste0("  ", if (length(stable)) paste(stable, collapse = ", ") else "(none)"),
  "",
  "CAVEAT: the instrument contrast is not era-pure -- ACCESS W2 (2018) and IRES",
  "(2019-20) are 1-2 years apart during continuing rapid change, so it will",
  "overstate the instrument component. The era contrast is the clean one.",
  "CAVEAT: ACCESS is a panel, so the era contrast's coefficient tests are",
  "conservative (positively correlated estimates, covariance ignored).")
writeLines(summary_txt, file.path(diag_dir, "transfer_summary.txt"))
cat("\n"); cat(summary_txt, sep = "\n"); cat("\n")

## ---- CHECKS ------------------------------------------------------------------
chk_header("21_transfer_diagnostics")
if (exists("chk_singular_summary"))
  chk_singular_summary("21", "21_transfer_diagnostics")

chk("21", "harmonized frames loaded with all three eras",
    nrow(A1c) > 0 && nrow(IRc) > 0,
    sprintf("W1=%d, W2=%d, IRES=%d rows", nrow(A1c), nrow(A2c), nrow(IRc)))
chk_warn("21", "ACCESS wave 2 available (needed for the ERA contrast)",
         nrow(A2c) > 0,
         if (nrow(A2c) > 0) sprintf("%d rows", nrow(A2c))
         else "wave 2 empty -- era and instrument cannot be separated")
chk("21", "common state set non-empty", length(common_states) > 0,
    paste(common_states, collapse = ", "))
chk_warn("21", "all three transfer contrasts estimable",
         !any(is.na(c(r_era, r_instr, r_both))),
         sprintf("era=%s, instrument=%s, both=%s",
                 round(r_era, 3), round(r_instr, 3), round(r_both, 3)))
chk_file("21", "predictive transfer table written", "diagnostics/transfer_predictive.csv")
chk_file("21", "coefficient stability table written", "diagnostics/transfer_coefficients.csv")
chk_file("21", "transfer summary written", "diagnostics/transfer_summary.txt")
# Sanity: the combined contrast should not beat BOTH of its components, since it
# stacks the two sources of non-transportability. If it does, something is off.
chk_warn("21", "combined contrast is not better than both components",
         is.na(r_both) || is.na(r_era) || is.na(r_instr) ||
           !(r_both > r_era + 0.05 && r_both > r_instr + 0.05),
         sprintf("both=%s vs era=%s, instrument=%s",
                 round(r_both, 3), round(r_era, 3), round(r_instr, 3)))

message("21_transfer_diagnostics.R done -> diagnostics/transfer_*.csv, transfer_summary.txt")
