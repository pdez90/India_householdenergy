# ==============================================================================
# 32_mime2_stageA.R -- Stage A of the version-2 pilot (docs/mime_v2_spec.md,
# Section 4.2): does the sampler CONVERGE, on synthetic data built on the real
# design? Standalone; never part of the pipeline or the document build.
#
# Synthetic data (spec 4.1): the real analysis frame (districts, states,
# covariates, birth weights), the real calibration posterior (the empirical
# joint draws of both fits, imputation_components_v2.rds), the real joint
# shrinkage moments; a "true" calibration draw chosen at random per dataset,
# true non-calibrated state effects tau * eta, eta ~ N(0,1), true NFHS errors
# z ~ N(0, R_d); mortality Y = mu(Delta_true) + Z gamma_hat + v_s + e with
# omega and psi fitted to the real outcome. Scenarios S1, S2, S3a at beta = 0
# and beta1 = 2 x the fixed-surface estimate, and S3b (heterogeneous slopes,
# non-zero mean). At beta = 0 every exposure slope is exactly zero.
#
# Each dataset is fitted with the 2 x 2 algorithm comparison -- state intercept
# (on/off) x eta update (statewise / block prior proposal) -- at two run
# lengths (short: burn 1000, thin 10; long: burn 4000, thin 40), 4 chains x 50
# retained imputations each. Monitored: beta, a/b/tau of both fits in use,
# omega, psi, mean and SD of Delta, mean |eta| of both fits, and the completed-
# data coefficient (WLS coefficient of each imputation; CR2 changes only its
# SE). Pass (spec 4.2): primary variant (state intercept, statewise), long run,
# every R-hat <= 1.05 and every bulk ESS >= 100, in >= 95% of datasets; and
# pooled estimate stable between run lengths (< 0.25 pooled CR2 SE).
#
# Every (dataset, variant, length) run is checkpointed to its own file under
# OUTPUT_DIR/mime2_stageA/ as soon as it finishes; re-running resumes.
#   cd scripts && OUTPUT_DIR=... STAGEA_CORES=12 Rscript 32_mime2_stageA.R
#   STAGEA_DATASETS (default 10 per cell), STAGEA_ONLY_PRIMARY=1 to skip the 2x2
# ==============================================================================
# ARCHIVAL (Oct 2026): this script belongs to the abandoned outcome-conditioned
# imputation (failed convergence -- not for inference; records/health_imputation/).
# It predates the outcome-specific birth weights (w_hmean_neonatal / w_hmean_infant)
# and is not part of the pipeline. It stops here unless explicitly re-enabled.
if (Sys.getenv("RUN_ARCHIVED_MIME", "") != "1")
  stop("ARCHIVED: 32_mime2_stageA.R belongs to the abandoned outcome-conditioned imputation and is not ",
       "maintained (it does not use the outcome-specific birth weights). Its recorded results are in ",
       "records/health_imputation/. Set RUN_ARCHIVED_MIME=1 only to reproduce the archived experiment ",
       "after updating its weight handling.", call. = FALSE)
suppressPackageStartupMessages({ library(dplyr) })
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()
source("mime2.R")

NDS    <- as.integer(Sys.getenv("STAGEA_DATASETS", "10"))
CORES  <- as.integer(Sys.getenv("STAGEA_CORES", max(1L, parallel::detectCores() - 1L)))
ONLY_P <- Sys.getenv("STAGEA_ONLY_PRIMARY", "0") == "1"
if (.Platform$OS.type == "windows") CORES <- 1L
# Sampler tuning allowed by spec Section 7 (each change recorded there):
#   STAGEA_NIDX  calibration-index proposals per fit per sweep (default 5)
#   STAGEA_TAG   label for this tuning round; its runs and summaries are kept
#                separately from earlier rounds (default: none = round 1)
NIDX <- as.integer(Sys.getenv("STAGEA_NIDX", "5"))
#   STAGEA_LONG  "burn,thin" of the long run (default "4000,40")
LONG <- as.integer(strsplit(Sys.getenv("STAGEA_LONG", "4000,40"), ",")[[1]])
TAG  <- Sys.getenv("STAGEA_TAG", "")
sfx  <- if (nzchar(TAG)) paste0("_", TAG) else ""
ck_dir <- file.path(dir_out, paste0("mime2_stageA", sfx)); dir.create(ck_dir, showWarnings = FALSE)

fr <- readRDS(file.path(dir_out, "health_analysis_frame.rds"))
cv <- readRDS(file.path(dir_out, "imputation_components_v2.rds"))
df <- fr$df; rhs <- fr$rhs; wcol <- fr$wcol; ocol <- fr$outcomes[["infant"]]
df$district <- as.character(df$district)
Zf <- model.matrix(reformulate(c("1", rhs)), model.frame(reformulate(c("1", rhs)), df, na.action = na.pass))
w  <- if (is.na(wcol)) rep(1, nrow(df)) else df[[wcol]]
ok <- is.finite(df[[ocol]]) & is.finite(w) & w > 0 & complete.cases(Zf) & !is.na(df$state) &
      df$district %in% cv$A$districts & df$district %in% cv$B$districts
df <- df[ok, , drop = FALSE]; rownames(df) <- NULL; w <- w[ok]; wt <- w / mean(w); n <- nrow(df)
FA <- m2_fit(cv$A, df$district); FB <- m2_fit(cv$B, df$district)
r  <- cv$rho$rho_post[match(df$district, as.character(as.numeric(cv$rho$district)))]; r[!is.finite(r)] <- 0
sidx <- as.integer(factor(df$state)); S <- max(sidx)

# data-generating values fitted to the real outcome on the point surface
D0 <- 100 * (plogis(FB$Lbar) - plogis(FA$Lbar))
dd <- df; dd$D0 <- D0
m_real <- lm(reformulate(c("D0", rhs), response = ocol), data = dd, weights = wt)
beta_fixed <- unname(coef(m_real)["D0"]); beta1 <- 2 * beta_fixed
eta_cov <- as.numeric(predict(m_real, newdata = transform(dd, D0 = 0)))
re <- lme4::lmer(res ~ 1 + (1 | st), data = data.frame(res = residuals(m_real), st = df$state), weights = wt)
omega <- as.numeric(attr(lme4::VarCorr(re)$st, "stddev")); psi <- sigma(re)
message(sprintf("Stage A design: n = %d districts, %d states; beta_fixed %.5f, beta1 %.5f; omega %.3f, psi %.3f",
                n, S, beta_fixed, beta1, omega, psi))

make_data <- function(scen, beta, seed) {
  set.seed(seed)
  kA <- sample.int(FA$K, 1); kB <- sample.int(FB$K, 1)
  eA <- rnorm(FA$n_eta); eB <- rnorm(FB$n_eta)
  a <- rnorm(n); z15 <- a; z19 <- r * a + sqrt(1 - r^2) * rnorm(n)
  L15 <- m2_logit(FA, kA, eA, z15); L19 <- m2_logit(FB, kB, eB, z19)
  if (scen == "S2") { L15 <- mean(L15) + 1.2 * (L15 - mean(L15)); L19 <- mean(L19) + 1.2 * (L19 - mean(L19)) }
  Dt <- 100 * (plogis(L19) - plogis(L15))
  mu <- switch(scen,
    S1 = beta * Dt, S2 = beta * Dt,
    S3a = 2 * beta * pmax(Dt - stats::median(Dt), 0),
    S3b = (beta + rnorm(S, 0, 0.5 * abs(beta1))[sidx]) * Dt)
  target <- if (scen %in% c("S3a", "S3b")) {
    x <- dd; x$.mu <- mu; x$.dt <- Dt
    unname(coef(lm(reformulate(c(".dt", rhs), response = ".mu"), data = x, weights = wt))[".dt"]) } else beta
  y <- eta_cov + mu + rnorm(S, 0, omega)[sidx] + rnorm(n, 0, psi / sqrt(wt))
  list(y = y, target = target)
}

cells <- rbind(expand.grid(scen = c("S1", "S2", "S3a"), beta = c(0, beta1), stringsAsFactors = FALSE),
               data.frame(scen = "S3b", beta = beta1))
datasets <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i)
  data.frame(cells[i, ], ds = seq_len(NDS), seed = 1e4 * i + seq_len(NDS), row.names = NULL)))
variants <- expand.grid(state_re = c(TRUE, FALSE), eta_update = c("statewise", "block"),
                        length = c("short", "long"), stringsAsFactors = FALSE)
if (ONLY_P) variants <- variants[variants$state_re & variants$eta_update == "statewise", ]
LENS <- strsplit(Sys.getenv("STAGEA_LENGTHS", "short,long"), ",")[[1]]
variants <- variants[variants$length %in% LENS, ]
CELLS <- Sys.getenv("STAGEA_CELLS", "")
if (nzchar(CELLS)) datasets <- datasets[datasets$scen %in% strsplit(CELLS, ",")[[1]], ]
tasks <- merge(datasets, variants)
tasks$file <- with(tasks, file.path(ck_dir, sprintf("%s_b%s_ds%02d_re%d_%s_%s.rds", scen,
                   ifelse(beta == 0, "0", "1"), ds, as.integer(state_re), eta_update, length)))
todo <- which(!file.exists(tasks$file))
message(sprintf("Stage A: %d runs (%d already checkpointed); %d cores", nrow(tasks), nrow(tasks) - length(todo), CORES))

Zk <- Zf[ok, , drop = FALSE]
wls_beta <- function(y, x, rows) {   # the completed-data point estimate (= the CR2 analysis's estimate)
  X <- cbind(x, Zk[rows, , drop = FALSE]); q <- qr(X); X <- X[, q$pivot[seq_len(q$rank)], drop = FALSE]
  if (q$pivot[1] != 1) stop("exposure column aliased")
  unname(lm.wfit(X, y, wt[rows])$coefficients[1])
}
run_task <- function(i) {
  tk <- tasks[i, ]; t0 <- Sys.time()
  res <- tryCatch({
    dat <- make_data(tk$scen, tk$beta, tk$seed)
    d <- df; d$.y <- dat$y
    L <- if (tk$length == "short") c(1000L, 10L) else LONG
    im <- mime2_draws(d, ".y", rhs, wt, cv$A, cv$B, cv$rho, M = 200, chains = 4L, burn = L[1], thin = L[2],
                      state_re = tk$state_re, eta_update = tk$eta_update, n_idx = NIDX,
                      index_move = Sys.getenv("STAGEA_INDEX_MOVE", "noncentered"), seed = tk$seed + 7L)
    cb <- apply(im$D[im$rows, , drop = FALSE], 2, function(x) wls_beta(dat$y[im$rows], x, im$rows))
    ce <- matrix(cb, ncol = 4)
    conv_est <- c(rhat = tryCatch(posterior::rhat(ce), error = function(e) NA_real_),
                  ess = tryCatch(posterior::ess_bulk(ce), error = function(e) NA_real_))
    # pooled CR2 SE only for the primary variant (needed for the stability rule)
    se_cr2 <- NA_real_
    if (tk$state_re && tk$eta_update == "statewise") {
      fits <- t(sapply(seq_len(ncol(im$D)), function(m) {
        x <- d; x$x_mi <- im$D[, m]
        mm <- lm(reformulate(c("x_mi", rhs), response = ".y"), data = x, weights = wt)
        cl <- x$state[as.numeric(rownames(model.frame(mm)))]
        ct <- clubSandwich::coef_test(mm, vcov = "CR2", cluster = cl, test = "Satterthwaite", coefs = "x_mi")
        c(ct$beta[1], ct$SE[1]) }))
      se_cr2 <- sqrt(mean(fits[, 2]^2) + (1 + 1 / nrow(fits)) * var(fits[, 1]))
    }
    c(tk[c("scen", "beta", "ds", "seed", "state_re", "eta_update", "length")],
      list(target = dat$target, pooled_est = mean(cb), pooled_se_cr2 = se_cr2,
           rhat_cr2_estimate = unname(conv_est["rhat"]), ess_cr2_estimate = unname(conv_est["ess"]),
           failed = FALSE, error = ""), im$diag[setdiff(names(im$diag), c("state_re", "eta_update"))])
  }, error = function(e) c(tk[c("scen", "beta", "ds", "seed", "state_re", "eta_update", "length")],
                           list(failed = TRUE, error = conditionMessage(e))))
  res$minutes <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  saveRDS(res, tasks$file[i])
  cat(sprintf("[%s] %s beta=%s ds%02d re=%d %s %s: %s (%.1f min)\n", format(Sys.time(), "%H:%M:%S"),
              tk$scen, ifelse(tk$beta == 0, "0", "b1"), tk$ds, as.integer(tk$state_re), tk$eta_update, tk$length,
              if (isTRUE(res$failed)) paste("FAILED:", res$error) else sprintf("rhat_max %.3f, ess_min %.0f, acc_idx %.3f",
              res$rhat_max, res$ess_min, res$acc_idx), res$minutes))
  invisible(NULL)
}
if (length(todo)) {
  if (CORES > 1) invisible(parallel::mclapply(todo, run_task, mc.cores = CORES, mc.preschedule = FALSE))
  else invisible(lapply(todo, run_task))
}

## ---- summary -------------------------------------------------------------------
runs <- bind_rows(lapply(tasks$file[file.exists(tasks$file)], function(f) {
  x <- readRDS(f); x[vapply(x, function(v) length(v) == 1, logical(1))] }))
write.csv(runs, file.path(dir_out, paste0("mime2_stageA_runs", sfx, ".csv")), row.names = FALSE)
runs <- runs %>% mutate(
  mixed = !failed & is.finite(rhat_max) & rhat_max <= 1.05 & is.finite(ess_min) & ess_min >= 100 &
          is.finite(rhat_cr2_estimate) & rhat_cr2_estimate <= 1.05 & is.finite(ess_cr2_estimate) & ess_cr2_estimate >= 100)
summ <- runs %>% group_by(state_re, eta_update, length) %>%
  summarise(datasets = n(), failed = sum(failed), share_mixed = mean(mixed), mixed = sum(mixed),
            median_rhat_max = median(rhat_max, na.rm = TRUE), median_ess_min = median(ess_min, na.rm = TRUE),
            median_acc_idx = median(acc_idx, na.rm = TRUE), median_acc_eta = median(acc_eta, na.rm = TRUE),
            median_minutes = median(minutes), .groups = "drop")
stab <- runs %>% filter(state_re, eta_update == "statewise", !failed) %>%
  select(scen, beta, ds, length, pooled_est, pooled_se_cr2) %>%
  tidyr::pivot_wider(names_from = length, values_from = c(pooled_est, pooled_se_cr2))
stab$diff_in_se <- if (all(c("pooled_est_long", "pooled_est_short") %in% names(stab)))
  abs(stab$pooled_est_long - stab$pooled_est_short) / stab$pooled_se_cr2_long else NA_real_
prim <- summ %>% filter(state_re, eta_update == "statewise", length == "long")
verdict <- tibble(
  criterion = c("primary variant, long run: share of datasets mixed (>= 0.95)",
                "pooled estimate stable between run lengths (< 0.25 SE) in >= 95% of datasets"),
  value = c(if (nrow(prim)) prim$share_mixed else NA_real_, mean(stab$diff_in_se < 0.25, na.rm = TRUE)),
  pass = c(if (nrow(prim)) prim$share_mixed >= 0.95 else FALSE, mean(stab$diff_in_se < 0.25, na.rm = TRUE) >= 0.95))
write.csv(summ, file.path(dir_out, paste0("mime2_stageA_summary", sfx, ".csv")), row.names = FALSE)
write.csv(stab, file.path(dir_out, paste0("mime2_stageA_stability", sfx, ".csv")), row.names = FALSE)
write.csv(verdict, file.path(dir_out, paste0("mime2_stageA_verdict", sfx, ".csv")), row.names = FALSE)
cat("\n== Stage A: convergence by algorithm variant ==\n"); print(as.data.frame(summ), digits = 3)
cat("\n== Stage A verdict (spec 4.2) ==\n"); print(as.data.frame(verdict), digits = 3)
