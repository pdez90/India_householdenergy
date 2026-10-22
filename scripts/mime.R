# ==============================================================================
# mime.R -- multiple imputation for a mismeasured exposure that CONDITIONS ON
# THE OUTCOME (measurement-error multiple imputation; Cole, Chu & Greenland 2006,
# Int J Epidemiol 35:1074; Freedman et al. 2008, Stat Med 27:5195).
#
# WHY. The corrected exposure of district d in round t is not observed; the
# calibration model gives its distribution given the survey data W,
#     L_dt = S_dt + b * sd_dt * z_dt,     z_dt ~ N(0, 1),
# where S_dt = a + b * E[x_dt | W] + u_s(d) carries the calibration parameters
# (shared across districts: intercept, slope, state effects) and b * sd_dt * z_dt
# the district's own NFHS sampling error. Imputing the exposure by drawing from
# p(L | W) alone -- as the earlier version did -- puts back exactly the kind of
# district-specific noise that measurement error consists of, and a regression on
# a noisier regressor is attenuated toward zero. Rubin's rules do not undo that:
# the imputation model omits the outcome, so it is not congenial with the
# analysis model and the pooled coefficient is biased toward the null. The
# remedy is to impute from p(L | W, Y): the calibration posterior as the prior,
# the outcome model as the likelihood.
#
# HOW. A Metropolis-within-Gibbs sampler over
#   (i15, i19)  index of the saved calibration draw (the shared part), one per
#               fitted calibration model (one shared index when both rounds come
#               from the same fit, as in the instrument-consistent sensitivity);
#   z15, z19    each district's standardised NFHS-side error;
#   theta       the outcome model: Y_d = beta * Delta_d + Z_d gamma + e_d,
#               e_d ~ N(0, sigma^2 / w_d), w the analysis weights (normalised),
#               flat prior on (beta, gamma), p(sigma^2) ~ 1/sigma^2,
# with Delta_d = 100 * (plogis(L_d,2019) - plogis(L_d,2015)) in percentage points.
#   theta | rest: exact conjugate draw (weighted normal regression);
#   z | rest:     independence Metropolis with the N(0,1) prior as proposal,
#                 district by district (acceptance = outcome-likelihood ratio);
#   i | rest:     independence Metropolis with a uniform draw from the saved
#                 calibration posterior as proposal (acceptance = likelihood
#                 ratio over all districts).
# Both Metropolis steps propose from the prior, so the stationary distribution
# is exactly p(i, z, theta | W, Y) under the model above. Imputations are taken
# every `thin` sweeps after `burn`. Each imputed exposure surface is then
# analysed with the paper's analysis model (weighted lm, CR2 / Satterthwaite)
# and pooled by Rubin's rules with Barnard-Rubin degrees of freedom, exactly as
# before; only the imputations change.
#
# The imputation model contains the analysis model's covariates, region effects
# and weights, but its likelihood treats district residuals as independent
# (variance proportional to 1 / birth weight); state clustering enters only
# afterwards, through CR2 in the analysis of each completed dataset. Matching
# covariates and weights therefore does NOT make the two models congenial in
# the strict sense, and nothing here guarantees that CR2 + Rubin pooling keeps
# nominal coverage. That is an empirical question, checked on this design by
# 29_mime_validation.R (state-correlated outcome errors, unequal precision,
# misspecified calibration and dose-response); the method should be read as
# awaiting that validation, not as validated by construction.
#
# KAPPA. The precision frontier rescales the calibration posterior around its
# mean Lbar (the corrected point surface): L(kappa) = Lbar + kappa (L - Lbar),
# i.e. S -> Lbar + kappa (S - Lbar) and the noise scale b -> kappa * b. At
# kappa = 0 there is nothing to impute and every imputation equals the point
# surface, so the frontier's kappa = 0 end reproduces the fixed-surface fit.
# ==============================================================================

# ---- priors -------------------------------------------------------------------
# A prior is list(S = K x n logit matrix of the shared part, bk = length-K noise
# multipliers (the slope draws), sd = length-n posterior SD of the district's
# true NFHS value, Lbar = length-n posterior mean of the corrected logit,
# ids = district ids as character).
# rho: data frame (district, rho_post), the posterior correlation of a district's
# true NFHS values in the two rounds (05_correction.R, joint shrinkage); the
# 2015 and 2019 NFHS-side errors are drawn with that correlation.
# sig = TRUE adds the calibration residual (sigma draw * e, independent by
# district and round): the posterior-PREDICTIVE estimand of 23_ppd_sensitivity.R.
# shrinkage = "separate" rebuilds the prior that each round's own (univariate)
# shrinkage would have given: S is linear in the posterior mean of the true NFHS
# value (S = a + b * E[X] + u), so S_sep = S + b * (E_sep[X] - E_joint[X]), with
# the univariate posterior SD and no correlation between rounds. This is the
# comparison the four-way table in H2 needs (separate vs joint shrinkage).
mime_prior <- function(comp, rho = NULL, sig = FALSE, shrinkage = c("joint", "separate")) {
  shrinkage <- match.arg(shrinkage)
  stopifnot(is.matrix(comp$S), length(comp$b) == nrow(comp$S),
            length(comp$x_sd) == ncol(comp$S), length(comp$Lbar) == ncol(comp$S))
  ids <- as.character(as.numeric(comp$districts))
  r <- if (is.null(rho)) rep(0, length(ids)) else
         rho$rho_post[match(ids, as.character(as.numeric(rho$district)))]
  r[!is.finite(r)] <- 0
  S <- comp$S; sd <- as.numeric(comp$x_sd); Lbar <- as.numeric(comp$Lbar)
  if (shrinkage == "separate") {
    if (is.null(comp$mu_sep) || is.null(comp$sd_sep) || is.null(comp$mu_joint))
      stop("components lack the separate-shrinkage moments (re-run 05_correction.R)")
    dmu  <- as.numeric(comp$mu_sep) - as.numeric(comp$mu_joint)
    S    <- S + outer(as.numeric(comp$b), dmu)
    Lbar <- Lbar + mean(comp$b) * dmu
    sd   <- as.numeric(comp$sd_sep); r[] <- 0
  }
  list(S = S, bk = as.numeric(comp$b), sd = sd, Lbar = Lbar, ids = ids, rho = r,
       sg = if (sig) as.numeric(comp$sigma) else 0, shrinkage = shrinkage)
}

# The earlier, outcome-independent imputation from the same prior: imputation m
# takes an evenly spaced calibration draw per round and a fresh district error.
# Kept for the comparison of propagation methods, not as a recommended method.
mime_exposure_only <- function(df, pr15, pr19, M = 200, seed = 1L, shared_index = FALSE) {
  set.seed(seed)
  j15 <- match(df$district, pr15$ids); j19 <- match(df$district, pr19$ids)
  ok  <- which(!is.na(j15) & !is.na(j19))
  # fixed processing order (by district id), so the imputations do not depend on
  # the row order of df: 23 rebuilds H2's frame and must reproduce its MI rows.
  ok  <- ok[order(suppressWarnings(as.numeric(df$district[ok])), df$district[ok])]
  n <- length(ok)
  i15s <- round(seq(1, nrow(pr15$S), length.out = M))
  if (shared_index && nrow(pr15$S) != nrow(pr19$S))
    stop("mime_exposure_only: shared_index = TRUE needs the same number of calibration draws in both rounds (",
         nrow(pr15$S), " vs ", nrow(pr19$S), ")")
  i19s <- if (shared_index) i15s else round(seq(1, nrow(pr19$S), length.out = M))
  pick <- function(v, i) v[min(i, length(v))]
  D <- matrix(NA_real_, nrow(df), M)
  rho <- pr19$rho[j19[ok]]
  for (m in seq_len(M)) {
    a <- rnorm(n); z19 <- rho * a + sqrt(1 - rho^2) * rnorm(n)
    L15 <- pr15$S[i15s[m], j15[ok]] + pick(pr15$bk, i15s[m]) * pr15$sd[j15[ok]] * a + pick(pr15$sg, i15s[m]) * rnorm(n)
    L19 <- pr19$S[i19s[m], j19[ok]] + pick(pr19$bk, i19s[m]) * pr19$sd[j19[ok]] * z19 + pick(pr19$sg, i19s[m]) * rnorm(n)
    D[ok, m] <- 100 * (plogis(L19) - plogis(L15))
  }
  D
}
mime_kappa <- function(pr, kap) {
  if (!is.finite(kap) || kap < 0) stop("kappa must be >= 0")
  if (kap == 0) {
    pr$S <- matrix(pr$Lbar, nrow = 1); pr$bk <- 0; pr$sg <- 0
  } else if (kap != 1) {
    Lb <- matrix(pr$Lbar, nrow(pr$S), ncol(pr$S), byrow = TRUE)
    pr$S <- Lb + kap * (pr$S - Lb); pr$bk <- kap * pr$bk; pr$sg <- kap * pr$sg
  }
  pr
}
# Calibration known (held at its posterior mean); only the NFHS-side error left.
mime_fixed <- function(pr, slope) {
  pr$S <- matrix(pr$Lbar, nrow = 1); pr$bk <- slope; pr$sg <- 0; pr
}

# ---- the sampler --------------------------------------------------------------
# df: analysis frame (one row per district, column `district` as character);
# ocol: outcome column; rhs: covariate terms exactly as in the analysis model
# (character, may include "factor(region)"); w: analysis weights aligned to df
# rows (NULL = unweighted); pr15, pr19: priors; shared_index: one calibration
# index for both rounds (both surfaces predicted from one fit).
# Returns list(D = nrow(df) x M matrix of imputed change (pp; NA outside the
# sampled rows), beta = M imputed-model slope draws, diag = list of diagnostics).
mime_draws <- function(df, ocol, rhs, w, pr15, pr19, M = 200, burn = 1000, thin = 10,
                       shared_index = FALSE, n_z = 2L, n_idx = 5L, seed = 1L, chains = 4L) {
  set.seed(seed)
  j15 <- match(df$district, pr15$ids); j19 <- match(df$district, pr19$ids)
  if (is.null(w)) w <- rep(1, nrow(df))
  f  <- reformulate(c("1", rhs))
  mf <- model.frame(f, data = df, na.action = na.pass)
  Zf <- model.matrix(f, mf)
  ok <- is.finite(df[[ocol]]) & !is.na(j15) & !is.na(j19) & is.finite(w) & w > 0 &
        stats::complete.cases(Zf)
  rows <- which(ok)
  # Process districts in a fixed order (by district id), so the random streams
  # -- and hence the imputations -- do not depend on the row order of `df`:
  # 22 / 23 rebuild H2's frame and must reproduce its imputations exactly.
  rows <- rows[order(suppressWarnings(as.numeric(df$district[rows])), df$district[rows])]
  n <- length(rows)
  Y  <- df[[ocol]][rows]; Z <- Zf[rows, , drop = FALSE]
  q  <- qr(Z); Z <- Z[, q$pivot[seq_len(q$rank)], drop = FALSE]      # drop aliased columns, as lm does
  wt <- w[rows] / mean(w[rows])
  S15 <- pr15$S[, j15[rows], drop = FALSE]; S19 <- pr19$S[, j19[rows], drop = FALSE]
  sd15 <- pr15$sd[j15[rows]];  sd19 <- pr19$sd[j19[rows]]
  rho  <- pr19$rho[j19[rows]]; rc <- sqrt(1 - rho^2)
  K15 <- nrow(S15); K19 <- nrow(S19)
  if (shared_index && K15 != K19) stop("shared_index needs equal numbers of calibration draws")
  p  <- ncol(Z) + 1L

  pick <- function(v, i) v[min(i, length(v))]
  # z = (z15, z19, e15, e19): NFHS-side errors (correlated across rounds by rho)
  # and, for the predictive estimand, the calibration residuals.
  lp  <- function(pr, S, i, sd, z, e) S[i, ] + pick(pr$bk, i) * sd * z + pick(pr$sg, i) * e
  dlt <- function(i15, i19, Zs)
    100 * (plogis(lp(pr19, S19, i19, sd19, Zs$z19, Zs$e19)) - plogis(lp(pr15, S15, i15, sd15, Zs$z15, Zs$e15)))
  draw_z <- function() { a <- rnorm(n)
    list(z15 = a, z19 = rho * a + rc * rnorm(n), e15 = rnorm(n), e19 = rnorm(n)) }
  ll  <- function(D, eta, beta, s2) -wt * (Y - eta - beta * D)^2 / (2 * s2)   # per district

  # ---- one chain ------------------------------------------------------------
  # Several chains, independently initialised (random calibration draw, random
  # district errors), are run and their imputations concatenated in chain order;
  # convergence is judged by comparing them (rank-normalised split R-hat and
  # bulk ESS, posterior package) on the imputation-model slope, the calibration
  # slopes in use, and the mean and SD of the imputed exposure change.
  run_chain <- function(Mc) {
    i15 <- sample.int(K15, 1); i19 <- if (shared_index) i15 else sample.int(K19, 1)
    Zs  <- draw_z()
    D   <- dlt(i15, i19, Zs)
    n_iter <- burn + Mc * thin
    Dc <- matrix(NA_real_, n, Mc)
    tr <- matrix(NA_real_, Mc, 5, dimnames = list(NULL, c("beta", "b15", "b19", "mean_change", "sd_change")))
    ic <- matrix(NA_integer_, Mc, 2)
    acc_z <- acc_i <- 0; prop_z <- prop_i <- 0; k <- 0L
    dof <- n - p
    for (it in seq_len(n_iter)) {
      ## theta | Delta: conjugate weighted regression
      X   <- cbind(D, Z)
      XtW <- t(X * wt)
      A   <- XtW %*% X
      R   <- chol(A)
      bh  <- backsolve(R, forwardsolve(t(R), XtW %*% Y))
      res <- Y - X %*% bh
      s2  <- sum(wt * res^2) / rchisq(1, dof)
      th  <- as.numeric(bh + backsolve(R, rnorm(p)) * sqrt(s2))
      beta <- th[1]; eta <- as.numeric(Z %*% th[-1])
      ## district errors | rest: independence Metropolis per district, the joint
      ## prior of (z15, z19, e15, e19) as proposal
      cur <- ll(D, eta, beta, s2)
      for (r in seq_len(n_z)) {
        Zp <- draw_z(); Dp <- dlt(i15, i19, Zp); llp <- ll(Dp, eta, beta, s2)
        a  <- log(runif(n)) < llp - cur
        for (nm in names(Zs)) Zs[[nm]][a] <- Zp[[nm]][a]
        D[a] <- Dp[a]; cur[a] <- llp[a]; acc_z <- acc_z + sum(a); prop_z <- prop_z + n
      }
      ## calibration index | rest: independence Metropolis over the saved draws
      tot <- sum(cur)
      for (r in seq_len(n_idx)) {
        if (shared_index) {
          ip <- sample.int(K15, 1); Dp <- dlt(ip, ip, Zs)
          lt <- sum(ll(Dp, eta, beta, s2)); prop_i <- prop_i + 1
          if (log(runif(1)) < lt - tot) { i15 <- i19 <- ip; D <- Dp; tot <- lt; acc_i <- acc_i + 1 }
        } else {
          ip <- sample.int(K15, 1); Dp <- dlt(ip, i19, Zs)
          lt <- sum(ll(Dp, eta, beta, s2)); prop_i <- prop_i + 1
          if (log(runif(1)) < lt - tot) { i15 <- ip; D <- Dp; tot <- lt; acc_i <- acc_i + 1 }
          ip <- sample.int(K19, 1); Dp <- dlt(i15, ip, Zs)
          lt <- sum(ll(Dp, eta, beta, s2)); prop_i <- prop_i + 1
          if (log(runif(1)) < lt - tot) { i19 <- ip; D <- Dp; tot <- lt; acc_i <- acc_i + 1 }
        }
      }
      if (it > burn && (it - burn) %% thin == 0) {
        k <- k + 1L; Dc[, k] <- D; ic[k, ] <- c(i15, i19)
        tr[k, ] <- c(beta, pick(pr15$bk, i15), pick(pr19$bk, i19), mean(D), stats::sd(D))
      }
    }
    list(D = Dc, tr = tr, ic = ic, acc_z = acc_z, prop_z = prop_z, acc_i = acc_i, prop_i = prop_i)
  }
  if (M %% chains != 0) stop("M must be a multiple of the number of chains")
  Mc <- M %/% chains
  ch <- lapply(seq_len(chains), function(cc) run_chain(Mc))
  Dout <- matrix(NA_real_, nrow(df), M)
  Dout[rows, ] <- do.call(cbind, lapply(ch, `[[`, "D"))
  i_out <- do.call(rbind, lapply(ch, `[[`, "ic"))
  trs   <- lapply(colnames(ch[[1]]$tr), function(v) sapply(ch, function(x) x$tr[, v]))   # Mc x chains each
  names(trs) <- colnames(ch[[1]]$tr)
  conv  <- mime_convergence(trs)
  bout  <- as.numeric(trs$beta)
  sumc  <- function(nm) sum(vapply(ch, `[[`, numeric(1), nm))
  list(D = Dout, beta = bout, rows = rows, chain = rep(seq_len(chains), each = Mc), traces = trs,
       diag = c(list(n = n, chains = chains, acc_z = sumc("acc_z") / max(sumc("prop_z"), 1),
                   acc_idx = sumc("acc_i") / max(sumc("prop_i"), 1),
                   distinct_idx15 = length(unique(i_out[, 1])), distinct_idx19 = length(unique(i_out[, 2])),
                   K15 = K15, K19 = K19,
                   beta_mean = mean(bout), beta_lo = unname(quantile(bout, .025)),
                   beta_hi = unname(quantile(bout, .975)),
                   # Feedback of the outcome on the calibration: the slope
                   # averaged over the calibration draws the imputations used,
                   # against its prior (calibration-only) mean.
                   b15_prior = mean(pr15$bk), b15_imputed = mean(trs$b15),
                   b19_prior = mean(pr19$bk), b19_imputed = mean(trs$b19),
                   # Concentration on the saved calibration draws: the largest share
                   # of the M imputations that used one single draw (1/M is ideal).
                   # High concentration can mean a concentrated posterior OR poor
                   # exploration; the between-chain comparison below separates them.
                   max_share_idx15 = max(table(i_out[, 1])) / M,
                   max_share_idx19 = max(table(i_out[, 2])) / M), conv))
}

# R-hat and bulk ESS of the completed-data coefficient across the chains
# (imputations are stored in chain order). Constant = 1 / M, as above.
mime_conv_est <- function(est, chains) {
  x <- matrix(est, ncol = chains)
  if (all(is.finite(x)) && diff(range(x)) < 1e-12) return(c(rhat = 1, ess = length(x)))
  c(rhat = tryCatch(posterior::rhat(x), error = function(e) NA_real_),
    ess  = tryCatch(posterior::ess_bulk(x), error = function(e) NA_real_))
}
# A diagnostics row passes when every R-hat is finite and <= rmax and every ESS
# is finite and >= emin, the completed-data coefficient included. Missing = fail.
mime_diag_ok <- function(d, rmax = 1.05, emin = 100)
  is.finite(d$rhat_max) & d$rhat_max <= rmax & is.finite(d$ess_min) & d$ess_min >= emin &
  is.finite(d$rhat_cr2_estimate) & d$rhat_cr2_estimate <= rmax &
  is.finite(d$ess_cr2_estimate) & d$ess_cr2_estimate >= emin

# Between-chain convergence summaries for each monitored quantity: rank-
# normalised split R-hat and bulk ESS (posterior package; Vehtari et al. 2021).
# A quantity that does not vary at all (kappa = 0, fixed calibration) gets
# R-hat 1 and ESS = number of draws, flagged as constant; any other non-finite
# value is returned as NA and treated as a FAILED diagnostic by the callers.
mime_convergence <- function(trs) {
  out <- list()
  for (v in names(trs)) {
    x <- as.matrix(trs[[v]])
    const <- all(is.finite(x)) && diff(range(x)) < 1e-12
    rh <- if (const) 1 else tryCatch(posterior::rhat(x), error = function(e) NA_real_)
    es <- if (const) length(x) else tryCatch(posterior::ess_bulk(x), error = function(e) NA_real_)
    out[[paste0("rhat_", v)]] <- rh; out[[paste0("ess_", v)]] <- es
  }
  out$rhat_max <- suppressWarnings(max(unlist(out[grep("^rhat_", names(out))])))
  out$ess_min  <- suppressWarnings(min(unlist(out[grep("^ess_", names(out))])))
  out
}

# Effective sample size of a chain (initial positive sequence, Geyer 1992).
mime_ess <- function(x) {
  n <- length(x); if (n < 4 || var(x) == 0) return(NA_real_)
  a <- stats::acf(x, lag.max = min(n - 1, 200), plot = FALSE)$acf[-1]
  s <- 0
  for (k in seq(1, length(a) - 1, by = 2)) { pr <- a[k] + a[k + 1]; if (pr < 0) break; s <- s + pr }
  n / (1 + 2 * s)
}

# Rubin / Barnard-Rubin pooling of per-imputation fits (est, se, se_cr2, df_satt).
mime_pool <- function(est, se, se2, dfs) {
  M <- length(est)
  qbar <- mean(est); ubar <- mean(se^2); bvar <- var(est)
  tvar <- ubar + (1 + 1/M) * bvar
  ok2  <- all(is.finite(se2)) && all(is.finite(dfs))
  tv2 <- df_br <- NA_real_
  if (ok2) {
    tv2 <- mean(se2^2) + (1 + 1/M) * bvar
    nu_com <- mean(dfs); lam <- (1 + 1/M) * bvar / tv2
    df_br <- if (!is.finite(lam) || lam <= 1e-12) nu_com else {
      nu_old <- (M - 1) / lam^2
      nu_obs <- (nu_com + 1) / (nu_com + 3) * nu_com * (1 - lam)
      1 / (1/nu_old + 1/nu_obs) }
  }
  t2 <- if (ok2) qbar / sqrt(tv2) else NA_real_
  c(estimate = qbar, se = sqrt(tvar), ubar = ubar, bvar = bvar, fmi = 1 - ubar / tvar,
    z = qbar / sqrt(tvar), p = 2 * pnorm(-abs(qbar / sqrt(tvar))),
    se_cr2 = if (ok2) sqrt(tv2) else NA_real_, df_br = df_br, t_cr2 = t2,
    p_cr2 = if (ok2) 2 * pt(-abs(t2), df_br) else NA_real_,
    crit_cr2 = if (ok2) qt(0.975, df_br) else NA_real_)
}
