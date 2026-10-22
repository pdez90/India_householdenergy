# ==============================================================================
# 29_mime_validation.R -- prespecified simulation check of the three ways of
# using the corrected exposure in the health model, on THIS study's design.
#
#   fixed          the corrected point surface held fixed (posterior mean)
#   exposure-only  multiple imputation with each imputation one draw of the
#                  calibration posterior (the earlier method)
#   MIME           outcome-conditional multiple imputation (scripts/mime.R; the
#                  method now reported)
#
# Each replicate draws a TRUE exposure surface from the calibration posterior
# (one shared calibration draw plus each district's NFHS-side error, correlated
# across rounds), generates a mortality change from it with a known slope
# beta, the study's own covariate effects, a state random effect and
# weight-scaled residual noise of the size fitted to the real data, and then
# analyses the simulated outcome with each method, using the paper's CR2 /
# Satterthwaite inference (Barnard-Rubin pooled for the MI methods).
#
# Scenarios: beta in {0, the fixed-surface estimate, twice it}, crossed with
#   correct          the truth comes from the calibration posterior the methods use
#   too narrow       the truth deviates from the point surface 1.25 times as much
#                    as the posterior says (calibration uncertainty understated)
#   calibration bias the true calibration is 20% steeper than the fitted one: every
#                    district's true logit deviates from the round's mean 1.2
#                    times as far as the posterior draw does (a shared,
#                    systematic error the methods do not know about)
#   nonlinear        the exposure-mortality relation is not the fitted linear one:
#                    the effect acts only above the median change, with slope
#                    2 * beta. The target is then the best linear approximation
#                    (the slope of the true mean outcome on the true change and
#                    the covariates), computed per replicate.
# The simulated outcome has a state random effect (residuals correlated within
# state), residual variance inversely proportional to the birth weight (unequal
# district precision) and the analysis uses the actual weights.
# Reported per method: mean estimate, bias, empirical SD, mean CR2 SE, mean 95%
# interval width, coverage and rejection rate with their Monte Carlo SEs. The
# question is recovery, coverage and false-positive rate, not which method
# gives the smaller p-value.
#
# Inputs : health_analysis_frame.rds (H2), correction_posterior_components.rds (05)
# Output : mime_validation.csv, mime_validation_reps.csv
# Settings (env): SIM_REPS (default 60 per scenario), SIM_M (30), SIM_CORES.
# ==============================================================================
# ARCHIVAL (Oct 2026): this script belongs to the abandoned outcome-conditioned
# imputation (failed convergence -- not for inference; records/health_imputation/).
# It predates the outcome-specific birth weights (w_hmean_neonatal / w_hmean_infant)
# and is not part of the pipeline. It stops here unless explicitly re-enabled.
if (Sys.getenv("RUN_ARCHIVED_MIME", "") != "1")
  stop("ARCHIVED: 29_mime_validation.R belongs to the abandoned outcome-conditioned imputation and is not ",
       "maintained (it does not use the outcome-specific birth weights). Its recorded results are in ",
       "records/health_imputation/. Set RUN_ARCHIVED_MIME=1 only to reproduce the archived experiment ",
       "after updating its weight handling.", call. = FALSE)
suppressPackageStartupMessages({ library(dplyr); library(lme4) })
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()
source("mime.R")

R_REPS <- as.integer(Sys.getenv("SIM_REPS", "100"))
M_SIM  <- as.integer(Sys.getenv("SIM_M", "40"))
CORES  <- as.integer(Sys.getenv("SIM_CORES", max(1L, parallel::detectCores() - 1L)))
MIME_CHAINS <- 2L   # chains per MIME fit in the simulation (M_SIM must be a multiple)
if (.Platform$OS.type == "windows") CORES <- 1L

fr   <- readRDS(file.path(dir_out, "health_analysis_frame.rds"))
comp <- readRDS(file.path(dir_out, "correction_posterior_components.rds"))
PR15 <- mime_prior(comp$surfaces$lpg_2015, comp$rho)
PR19 <- mime_prior(comp$surfaces$lpg_2019, comp$rho)
df   <- fr$df; rhs <- fr$rhs; wcol <- fr$wcol
ocol <- fr$outcomes[["infant"]]

# Rows usable in every method (complete outcome, covariates, weight, exposure prior)
j15 <- match(df$district, PR15$ids); j19 <- match(df$district, PR19$ids)
Zf  <- model.matrix(reformulate(c("1", rhs)), model.frame(reformulate(c("1", rhs)), df, na.action = na.pass))
w   <- if (is.na(wcol)) rep(1, nrow(df)) else df[[wcol]]
ok  <- is.finite(df[[ocol]]) & !is.na(j15) & !is.na(j19) & is.finite(w) & w > 0 &
       complete.cases(Zf) & !is.na(df$state)
df  <- df[ok, , drop = FALSE]; rownames(df) <- NULL
w   <- w[ok]; j15 <- j15[ok]; j19 <- j19[ok]; n <- nrow(df)
wt  <- w / mean(w)
D0  <- 100 * (plogis(PR19$Lbar[j19]) - plogis(PR15$Lbar[j15]))   # point surface

# ---- data-generating model, fitted to the real outcome ------------------------
f_real <- reformulate(c("D0", rhs), response = ocol)
dd <- df; dd$D0 <- D0
m_real <- lm(f_real, data = dd, weights = wt)
beta_fixed <- unname(coef(m_real)["D0"])
eta_cov <- as.numeric(predict(m_real, newdata = transform(dd, D0 = 0)))   # covariate part
res <- residuals(m_real)
re  <- tryCatch(lme4::lmer(res ~ 1 + (1 | state), data = data.frame(res, state = df$state), weights = wt),
                error = function(e) NULL)
tau_state <- if (is.null(re)) 0 else as.numeric(attr(lme4::VarCorr(re)$state, "stddev"))
sig_e     <- if (is.null(re)) sigma(m_real) else sigma(re)
message(sprintf("DGP: beta(fixed) = %.5f; state SD %.4f; residual SD %.4f; n = %d districts, %d states",
                beta_fixed, tau_state, sig_e, n, length(unique(df$state))))

draw_truth <- function(infl, slope_bias = 1) {
  i15 <- sample.int(nrow(PR15$S), 1); i19 <- sample.int(nrow(PR19$S), 1)
  a <- rnorm(n); z15 <- a; z19 <- PR19$rho[j19] * a + sqrt(1 - PR19$rho[j19]^2) * rnorm(n)
  L15 <- PR15$S[i15, j15] + PR15$bk[i15] * PR15$sd[j15] * z15
  L19 <- PR19$S[i19, j19] + PR19$bk[i19] * PR19$sd[j19] * z19
  L15 <- PR15$Lbar[j15] + infl * (L15 - PR15$Lbar[j15])
  L19 <- PR19$Lbar[j19] + infl * (L19 - PR19$Lbar[j19])
  if (slope_bias != 1) { L15 <- mean(L15) + slope_bias * (L15 - mean(L15)); L19 <- mean(L19) + slope_bias * (L19 - mean(L19)) }
  100 * (plogis(L19) - plogis(L15))
}

fit_cr2 <- function(d, xcol) {
  m  <- lm(reformulate(c(xcol, rhs), response = "y_sim"), data = d, weights = d$.w)
  ct <- clubSandwich::coef_test(m, vcov = "CR2", cluster = d$state, test = "Satterthwaite", coefs = xcol)
  c(est = ct$beta[1], se = ct$SE[1], df = ct$df_Satt[1])
}
pool <- function(M) {   # M: rows of est, se, df
  pl <- mime_pool(M[, "est"], M[, "se"], M[, "se"], M[, "df"])
  c(est = pl[["estimate"]], se = pl[["se_cr2"]], df = pl[["df_br"]])
}
summ <- function(r, target) {
  lo <- r[["est"]] - qt(0.975, r[["df"]]) * r[["se"]]; hi <- r[["est"]] + qt(0.975, r[["df"]]) * r[["se"]]
  c(est = r[["est"]], se = r[["se"]], df = r[["df"]], width = hi - lo, target = target,
    cover = as.numeric(lo <= target & target <= hi), reject = as.numeric(lo > 0 | hi < 0))
}

SCEN <- list(correct = list(infl = 1, sb = 1, nl = FALSE), too_narrow = list(infl = 1.25, sb = 1, nl = FALSE),
             calibration_bias = list(infl = 1, sb = 1.2, nl = FALSE), nonlinear = list(infl = 1, sb = 1, nl = TRUE))
one_rep <- function(rep, beta, scen) {
  sc <- SCEN[[scen]]
  set.seed(1e5 * match(scen, names(SCEN)) + 1e3 * rep + round(abs(beta) * 1e5))
  Dt <- draw_truth(sc$infl, sc$sb)
  mu <- if (sc$nl) 2 * beta * pmax(Dt - stats::median(Dt), 0) else beta * Dt
  d  <- df; d$.w <- wt
  # the estimand: the slope of the true mean outcome on the true change, given covariates
  target <- if (sc$nl) { d$.mu <- mu; d$.dt <- Dt
    unname(coef(lm(reformulate(c(".dt", rhs), response = ".mu"), data = d, weights = d$.w))[".dt"]) } else beta
  d$y_sim <- eta_cov + mu + rnorm(length(unique(df$state)), 0, tau_state)[as.integer(factor(df$state))] +
             rnorm(n, 0, sig_e / sqrt(wt))
  beta <- target
  # Each method is run on its own: a failure is RECORDED (with its message) and
  # does not remove the other methods' results for this replicate.
  na_row <- c(est = NA_real_, se = NA_real_, df = NA_real_, width = NA_real_, target = target, cover = NA_real_, reject = NA_real_)
  # A result counts as a success only if the estimate, SE, degrees of freedom
  # and interval are all finite (and the SE and df positive); anything else is
  # recorded as a failure with the reason, even when no error was thrown.
  attempt <- function(expr) tryCatch({
      v <- expr
      bad <- c(estimate = !is.finite(v[["est"]]), SE = !(is.finite(v[["se"]]) && v[["se"]] > 0),
               df = !(is.finite(v[["df"]]) && v[["df"]] > 0), interval = !is.finite(v[["width"]]))
      if (any(bad)) list(ok = FALSE, v = na_row, err = paste("non-finite or invalid result:", paste(names(bad)[bad], collapse = ", ")))
      else list(ok = TRUE, v = v, err = "") },
    error = function(e) list(ok = FALSE, v = na_row, err = conditionMessage(e)))
  out <- list(); errs <- list(); dg <- NULL
  d$x_fix <- D0
  r <- attempt(summ(fit_cr2(d, "x_fix"), beta)); out$fixed <- r$v; errs$fixed <- r$err
  # exposure-only MI
  r <- attempt({ eo <- t(sapply(seq_len(M_SIM), function(m) {
    i15 <- sample.int(nrow(PR15$S), 1); i19 <- sample.int(nrow(PR19$S), 1)
    a <- rnorm(n); z19 <- PR19$rho[j19] * a + sqrt(1 - PR19$rho[j19]^2) * rnorm(n)
    d$x_mi <- 100 * (plogis(PR19$S[i19, j19] + PR19$bk[i19] * PR19$sd[j19] * z19) -
                     plogis(PR15$S[i15, j15] + PR15$bk[i15] * PR15$sd[j15] * a))
    fit_cr2(d, "x_mi") }))
    summ(pool(eo), beta) })
  out$exposure_only <- r$v; errs$exposure_only <- r$err
  # MIME, with its sampler diagnostics kept for this replicate (2 chains here)
  r <- attempt({ im <- mime_draws(transform(d, .y = y_sim), ".y", rhs, d$.w, PR15, PR19, M = M_SIM,
                                  burn = 500L, thin = 5L, seed = rep + 7L, chains = MIME_CHAINS)
    dg <- im$diag
    mm <- t(sapply(seq_len(M_SIM), function(m) { d$x_mi <- im$D[, m]; fit_cr2(d, "x_mi") }))
    summ(pool(mm), beta) })
  out$mime <- r$v; errs$mime <- r$err
  dgf <- if (is.null(dg)) data.frame(acc_z = NA, acc_idx = NA, rhat_max = NA, ess_min = NA, max_share_idx15 = NA, max_share_idx19 = NA)
         else data.frame(dg[c("acc_z", "acc_idx", "rhat_max", "ess_min", "max_share_idx15", "max_share_idx19")])
  bind_rows(lapply(names(out), function(k) data.frame(method = k, t(out[[k]]), failed = errs[[k]] != "", error = errs[[k]],
                                                     if (k == "mime") dgf else dgf[NA_integer_, ])))
}

grid <- expand.grid(beta = c(0, beta_fixed, 2 * beta_fixed), scen = names(SCEN), rep = seq_len(R_REPS), stringsAsFactors = FALSE)
message(sprintf("running %d replicates on %d cores (M = %d imputations each)", nrow(grid), CORES, M_SIM))
# A replicate that fails before any method runs (e.g. in data generation) is
# also recorded, as a failure of every method, never dropped.
runner <- function(k) tryCatch(cbind(grid[k, c("beta", "scen", "rep")], one_rep(grid$rep[k], grid$beta[k], grid$scen[k])),
  error = function(e) cbind(grid[k, c("beta", "scen", "rep")],
    data.frame(method = c("fixed", "exposure_only", "mime"), est = NA_real_, se = NA_real_, df = NA_real_, width = NA_real_,
               target = NA_real_, cover = NA_real_, reject = NA_real_, failed = TRUE, error = conditionMessage(e),
               acc_z = NA, acc_idx = NA, rhat_max = NA, ess_min = NA, max_share_idx15 = NA, max_share_idx19 = NA)))
reps <- if (CORES > 1) parallel::mclapply(seq_len(nrow(grid)), runner, mc.cores = CORES) else lapply(seq_len(nrow(grid)), runner)
reps <- bind_rows(reps)
write.csv(reps, file.path(dir_out, "mime_validation_reps.csv"), row.names = FALSE)

reps <- reps %>% mutate(scenario = scen,
  beta_label = ifelse(beta == 0, "0 (null)", ifelse(abs(beta - beta_fixed) < 1e-12, "fixed-surface estimate", "twice that")),
  poorly_mixed = method == "mime" & !failed & (!is.finite(rhat_max) | rhat_max > 1.10 | !is.finite(ess_min) | ess_min < 50))
cnt <- reps %>% group_by(scenario, beta_label, beta, method) %>%
  summarise(attempted = n(), failed = sum(failed), succeeded = attempted - failed,
            poorly_mixed = if (method[1] == "mime") sum(poorly_mixed) else NA_integer_, .groups = "drop")
st <- reps %>% filter(!failed) %>% group_by(scenario, beta_label, beta, method) %>%
  summarise(mean_target = mean(target), mean_est = mean(est), bias = mean(est - target),
            mcse_bias = sd(est - target) / sqrt(n()),
            rel_bias_pct = if (all(target != 0)) 100 * mean(est - target) / abs(mean(target)) else NA_real_,
            emp_sd = sd(est), mean_se_cr2 = mean(se), mean_ci_width = mean(width),
            coverage = mean(cover), mcse_coverage = sqrt(mean(cover) * (1 - mean(cover)) / n()),
            reject_rate = mean(reject), mcse_reject = sqrt(mean(reject) * (1 - mean(reject)) / n()),
            # approximate 95% Monte Carlo margins (1.96 MCSE), e.g. about +/- 4.3 points
            # for a 5% rate over 100 replicates
            margin95_coverage = 1.96 * sqrt(mean(cover) * (1 - mean(cover)) / n()),
            margin95_reject   = 1.96 * sqrt(mean(reject) * (1 - mean(reject)) / n()),
            # Diagnostic only: coverage among well-mixed MIME replicates. Report it
            # beside the overall coverage and the share excluded, never instead.
            coverage_well_mixed = if (method[1] == "mime") mean(cover[!poorly_mixed]) else NA_real_,
            share_excluded_poorly_mixed = if (method[1] == "mime") mean(poorly_mixed) else NA_real_,
            .groups = "drop")
val <- left_join(cnt, st, by = c("scenario", "beta_label", "beta", "method")) %>%
  mutate(flag = dplyr::case_when(succeeded == 0 ~ "NO SUCCESSFUL REPLICATES",
                                 failed > 0 ~ paste0("INCOMPLETE: ", failed, " of ", attempted, " failed"),
                                 !is.na(poorly_mixed) & poorly_mixed > 0 ~ paste0(poorly_mixed, " poorly mixed"),
                                 TRUE ~ "")) %>%
  arrange(scenario, beta, method)
errs <- reps %>% filter(failed) %>% count(scenario, beta_label, method, error, name = "n")
write.csv(errs, file.path(dir_out, "mime_validation_failures.csv"), row.names = FALSE)
if (any(val$failed > 0))
  message("!! ", sum(val$failed), " method-replicates FAILED across ", sum(val$failed > 0),
          " cells -- see the flag column and mime_validation_failures.csv; coverage in those cells is over the successful replicates only")
write.csv(val, file.path(dir_out, "mime_validation.csv"), row.names = FALSE)
cat("\n== Validation of the exposure-propagation methods (infant model, this design) ==\n")
print(as.data.frame(val %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)
message("wrote mime_validation.csv, mime_validation_reps.csv, mime_validation_failures.csv")
