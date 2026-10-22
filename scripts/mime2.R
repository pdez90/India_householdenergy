# ==============================================================================
# mime2.R -- outcome-conditioned imputation, version 2 (docs/mime_v2_spec.md).
#
# Target (spec Section 2.3):
#   p(phi_A, phi_B, eta_A, eta_B, z, beta, gamma, psi2, omega2, v | survey, Y)
#   phi_f  one saved draw of fit f's calibration posterior (a, b, tau, the
#          calibrated states' effects U), proposed as a whole -- its internal
#          dependence is never broken up
#   eta_fs standardised effect of a state NOT in fit f's calibration sample:
#          u_fs = tau_f * eta_fs, eta_fs ~ N(0, 1) a priori, independently
#   z_d    the district's standardised NFHS-side errors, (z15, z19) ~ N(0, R_d)
#   Y_d = beta * Delta_d + Z_d gamma + v_s(d) + e_d,
#          v_s ~ N(0, omega2), e_d ~ N(0, psi2 / w_d)          (state_re = TRUE)
#   flat (beta, gamma); p(psi2) ~ 1/psi2; omega2 ~ scaled-inv-chi2(1, s0^2)
#   Delta_d = 100 * (plogis(L_d19) - plogis(L_d15)),
#   L_dt = a_f + b_f * (m_dt + sd_dt * z_dt) + u_f,s(d)
#
# Sweep (spec Section 3): exact Gibbs draws for (beta, gamma), psi2, v, omega2;
# independence Metropolis for each district's (z15, z19) from its prior;
# eta either statewise random-walk Metropolis (acceptance includes the N(0,1)
# prior ratio; all states proposed at once, accepted state by state, which is
# valid because each eta_s enters only its own state's districts) or, for the
# algorithm comparison, one block independence proposal of all eta from the
# prior (acceptance = joint likelihood ratio); and n_idx independence
# proposals per fit of a whole saved calibration draw, eta held fixed
# (acceptance = likelihood ratio, the proposal being the prior factor).
#
# The imputation model includes state dependence (v), but it is still a
# parametric model while the analysis of each completed dataset is design-robust
# (CR2); their compatibility is checked by simulation (Stage B), not assumed.
# The state intercept accommodates clustering; it does not by itself remove
# state-level confounding (v is assumed independent of exposure and covariates).
# ==============================================================================

# Prepare one fit's components for the analysis rows. cf: one element of
# imputation_components_v2.rds; ids: district ids (character) of the rows.
m2_fit <- function(cf, ids) {
  j <- match(ids, cf$districts)
  if (anyNA(j)) stop("districts missing from calibration components")
  st <- cf$state[j]; cal <- cf$calibrated[j]
  ncs <- sort(unique(st[!cal]))
  list(a = cf$a, b = cf$b, tau = cf$tau, U = cf$U,
       ucol = ifelse(cal, match(st, colnames(cf$U)), NA_integer_),
       ecol = ifelse(cal, NA_integer_, match(st, ncs)), n_eta = length(ncs), eta_states = ncs,
       m = cf$m[j], sd = cf$sd[j], Lbar = cf$Lbar[j], K = length(cf$b))
}

# Corrected logit of every row for calibration draw k, eta vector and errors z.
m2_logit <- function(F, k, eta, z) {
  u <- numeric(length(F$m))
  c1 <- !is.na(F$ucol); u[c1] <- F$U[k, F$ucol[c1]]
  if (any(!c1)) u[!c1] <- F$tau[k] * eta[F$ecol[!c1]]
  F$a[k] + F$b[k] * (F$m + F$sd * z) + u
}

# The sampler. Returns list(D = nrow(df) x M imputed change (NA outside rows),
# traces = list of Mc x chains matrices, diag = list).
mime2_draws <- function(df, ocol, rhs, w, CA, CB, rho, M = 200, chains = 4L, burn = 1000L, thin = 10L,
                        state_re = TRUE, eta_update = c("statewise", "block"), n_idx = 5L, n_z = 2L,
                        index_move = c("noncentered", "both"),
                        s0sq = NULL, seed = 1L) {
  eta_update <- match.arg(eta_update); index_move <- match.arg(index_move)
  set.seed(seed)
  if (M %% chains != 0) stop("M must be a multiple of chains")
  if (is.null(w)) w <- rep(1, nrow(df))
  f  <- reformulate(c("1", rhs))
  Zf <- model.matrix(f, model.frame(f, data = df, na.action = na.pass))
  ok <- is.finite(df[[ocol]]) & is.finite(w) & w > 0 & stats::complete.cases(Zf) &
        df$district %in% CA$districts & df$district %in% CB$districts & !is.na(df$state)
  rows <- which(ok)
  rows <- rows[order(suppressWarnings(as.numeric(df$district[rows])), df$district[rows])]
  n <- length(rows); ids <- df$district[rows]
  Y  <- df[[ocol]][rows]; Z <- Zf[rows, , drop = FALSE]
  q  <- qr(Z); Z <- Z[, q$pivot[seq_len(q$rank)], drop = FALSE]
  wt <- w[rows] / mean(w[rows]); p <- ncol(Z) + 1L
  sidx <- as.integer(factor(df$state[rows])); S <- max(sidx)
  FA <- m2_fit(CA, ids); FB <- m2_fit(CB, ids)
  r  <- rho$rho_post[match(ids, as.character(as.numeric(rho$district)))]; r[!is.finite(r)] <- 0
  rc <- sqrt(1 - r^2)
  if (is.null(s0sq)) s0sq <- stats::var(Y) / 4
  nu0 <- 1
  delta <- function(kA, kB, eA, eB, z15, z19)
    100 * (plogis(m2_logit(FB, kB, eB, z19)) - plogis(m2_logit(FA, kA, eA, z15)))
  run_chain <- function(Mc) {
    kA <- sample.int(FA$K, 1); kB <- sample.int(FB$K, 1)
    eA <- rnorm(FA$n_eta); eB <- rnorm(FB$n_eta)
    a  <- rnorm(n); z15 <- a; z19 <- r * a + rc * rnorm(n)
    D  <- delta(kA, kB, eA, eB, z15, z19)
    v  <- numeric(S); omega2 <- s0sq; psi2 <- stats::var(Y)
    hA <- rep(1, FA$n_eta); hB <- rep(1, FB$n_eta)       # statewise RW scales (adapted in burn-in only)
    accA <- numeric(FA$n_eta); accB <- numeric(FB$n_eta)
    n_iter <- burn + Mc * thin
    Dc <- matrix(NA_real_, n, Mc)
    trn <- c("beta", "aA", "bA", "tauA", "aB", "bB", "tauB", "omega", "psi", "mean_change", "sd_change",
             "mean_abs_etaA", "mean_abs_etaB")
    tr <- matrix(NA_real_, Mc, length(trn), dimnames = list(NULL, trn)); k <- 0L
    cnt <- c(z = 0, zp = 0, eta = 0, etap = 0, idx = 0, idxp = 0)
    for (it in seq_len(n_iter)) {
      vd <- v[sidx]
      ## (beta, gamma) | rest
      X <- cbind(D, Z); XtW <- t(X * (wt / psi2)); A <- XtW %*% X; R <- chol(A)
      bh <- backsolve(R, forwardsolve(t(R), XtW %*% (Y - vd)))
      th <- as.numeric(bh + backsolve(R, rnorm(p)))
      beta <- th[1]; eta <- as.numeric(Z %*% th[-1])
      ## psi2 | rest
      e <- Y - beta * D - eta - vd
      psi2 <- sum(wt * e^2) / rchisq(1, n)
      ## v | rest and omega2 | v
      if (state_re) {
        e0 <- Y - beta * D - eta
        prec <- as.numeric(rowsum(wt, sidx)) / psi2 + 1 / omega2
        mn   <- as.numeric(rowsum(wt * e0, sidx)) / psi2 / prec
        v    <- mn + rnorm(S) / sqrt(prec)
        omega2 <- (nu0 * s0sq + sum(v^2)) / rchisq(1, nu0 + S)
        vd <- v[sidx]
      }
      res_fn <- function(Dx) -wt * (Y - eta - vd - beta * Dx)^2 / (2 * psi2)
      cur <- res_fn(D)
      ## district errors
      for (rr in seq_len(n_z)) {
        a <- rnorm(n); zp15 <- a; zp19 <- r * a + rc * rnorm(n)
        Dp <- delta(kA, kB, eA, eB, zp15, zp19); lp <- res_fn(Dp)
        acc <- log(runif(n)) < lp - cur
        z15[acc] <- zp15[acc]; z19[acc] <- zp19[acc]; D[acc] <- Dp[acc]; cur[acc] <- lp[acc]
        cnt["z"] <- cnt["z"] + sum(acc); cnt["zp"] <- cnt["zp"] + n
      }
      ## eta (states outside each calibration sample)
      for (fid in c("A", "B")) {
        Fx <- if (fid == "A") FA else FB
        if (Fx$n_eta == 0) next
        e_cur <- if (fid == "A") eA else eB
        h <- if (fid == "A") hA else hB
        if (eta_update == "statewise") {
          e_new <- e_cur + h * rnorm(Fx$n_eta)
          Dp <- if (fid == "A") delta(kA, kB, e_new, eB, z15, z19) else delta(kA, kB, eA, e_new, z15, z19)
          lp <- res_fn(Dp)
          aff <- !is.na(Fx$ecol)
          dll <- numeric(Fx$n_eta)
          dll_s <- rowsum((lp - cur)[aff], Fx$ecol[aff])
          dll[as.integer(rownames(dll_s))] <- dll_s[, 1]
          logr <- dll + (e_cur^2 - e_new^2) / 2                 # likelihood ratio x N(0,1) prior ratio
          accs <- log(runif(Fx$n_eta)) < logr
          e_cur[accs] <- e_new[accs]
          chg <- aff & accs[ifelse(is.na(Fx$ecol), 1L, Fx$ecol)]
          D[chg] <- Dp[chg]; cur[chg] <- lp[chg]
          cnt["eta"] <- cnt["eta"] + sum(accs); cnt["etap"] <- cnt["etap"] + Fx$n_eta
          if (it <= burn) {                                    # adapt per-state scale during burn-in only
            if (fid == "A") accA <- accA + accs else accB <- accB + accs
            if (it %% 50 == 0) {
              ac <- (if (fid == "A") accA else accB) / 50
              h  <- h * exp(ac - 0.4)
              if (fid == "A") { hA <- h; accA[] <- 0 } else { hB <- h; accB[] <- 0 }
            }
          }
        } else {                                               # block prior proposal
          e_new <- rnorm(Fx$n_eta)
          Dp <- if (fid == "A") delta(kA, kB, e_new, eB, z15, z19) else delta(kA, kB, eA, e_new, z15, z19)
          lp <- res_fn(Dp)
          if (log(runif(1)) < sum(lp) - sum(cur)) { e_cur <- e_new; D <- Dp; cur <- lp; cnt["eta"] <- cnt["eta"] + 1 }
          cnt["etap"] <- cnt["etap"] + 1
        }
        if (fid == "A") eA <- e_cur else eB <- e_cur
      }
      ## whole calibration draw, eta held fixed
      tot <- sum(cur)
      for (rr in seq_len(n_idx)) {
        kp <- sample.int(FA$K, 1); Dp <- delta(kp, kB, eA, eB, z15, z19); lp <- res_fn(Dp)
        if (log(runif(1)) < sum(lp) - tot) { kA <- kp; D <- Dp; cur <- lp; tot <- sum(lp); cnt["idx"] <- cnt["idx"] + 1 }
        kp <- sample.int(FB$K, 1); Dp <- delta(kA, kp, eA, eB, z15, z19); lp <- res_fn(Dp)
        if (log(runif(1)) < sum(lp) - tot) { kB <- kp; D <- Dp; cur <- lp; tot <- sum(lp); cnt["idx"] <- cnt["idx"] + 1 }
        cnt["idxp"] <- cnt["idxp"] + 2
        if (index_move == "both") {
          # DIAGNOSTIC (not in the approved spec): the same independence proposal
          # of a calibration draw, but holding the non-calibrated states' EFFECTS
          # u = tau * eta fixed instead of eta: eta' = eta * tau_k / tau_k'.
          # MH ratio = likelihood ratio x prod phi(eta') / phi(eta) x (tau_k / tau_k')^n_eta.
          for (fid in c("A", "B")) {
            Fx <- if (fid == "A") FA else FB
            kc <- if (fid == "A") kA else kB; kp <- sample.int(Fx$K, 1)
            e0 <- if (fid == "A") eA else eB; e1 <- e0 * Fx$tau[kc] / Fx$tau[kp]
            Dp <- if (fid == "A") delta(kp, kB, e1, eB, z15, z19) else delta(kA, kp, eA, e1, z15, z19)
            lp <- res_fn(Dp)
            lr <- sum(lp) - tot + sum(e0^2 - e1^2) / 2 + length(e0) * log(Fx$tau[kc] / Fx$tau[kp])
            if (log(runif(1)) < lr) {
              if (fid == "A") { kA <- kp; eA <- e1 } else { kB <- kp; eB <- e1 }
              D <- Dp; cur <- lp; tot <- sum(lp); cnt["idx"] <- cnt["idx"] + 1 }
            cnt["idxp"] <- cnt["idxp"] + 1
          }
        }
      }
      if (it > burn && (it - burn) %% thin == 0) {
        k <- k + 1L; Dc[, k] <- D
        tr[k, ] <- c(beta, FA$a[kA], FA$b[kA], FA$tau[kA], FB$a[kB], FB$b[kB], FB$tau[kB],
                     sqrt(omega2), sqrt(psi2), mean(D), stats::sd(D),
                     if (FA$n_eta) mean(abs(eA)) else 0, if (FB$n_eta) mean(abs(eB)) else 0)
      }
    }
    list(D = Dc, tr = tr, cnt = cnt)
  }
  Mc <- M %/% chains
  ch <- lapply(seq_len(chains), function(cc) run_chain(Mc))
  Dout <- matrix(NA_real_, nrow(df), M); Dout[rows, ] <- do.call(cbind, lapply(ch, `[[`, "D"))
  trs <- lapply(colnames(ch[[1]]$tr), function(vn) sapply(ch, function(x) x$tr[, vn]))
  names(trs) <- colnames(ch[[1]]$tr)
  if (!state_re) trs$omega <- NULL
  cnt <- Reduce(`+`, lapply(ch, `[[`, "cnt"))
  conv <- mime2_convergence(trs)
  list(D = Dout, rows = rows, chain = rep(seq_len(chains), each = Mc), traces = trs,
       diag = c(list(n = n, chains = chains, M = M, burn = burn, thin = thin, state_re = state_re,
                     eta_update = eta_update, acc_z = unname(cnt["z"] / cnt["zp"]),
                     acc_eta = unname(cnt["eta"] / max(cnt["etap"], 1)),
                     acc_idx = unname(cnt["idx"] / max(cnt["idxp"], 1)),
                     beta_mean = mean(trs$beta)), conv))
}

# Rank-normalised split R-hat and bulk ESS for each monitored quantity
# (constant quantities: R-hat 1, ESS = draws; anything else non-finite = NA).
mime2_convergence <- function(trs) {
  out <- list()
  for (v in names(trs)) {
    x <- as.matrix(trs[[v]])
    const <- all(is.finite(x)) && diff(range(x)) < 1e-12
    out[[paste0("rhat_", v)]] <- if (const) 1 else tryCatch(posterior::rhat(x), error = function(e) NA_real_)
    out[[paste0("ess_", v)]]  <- if (const) length(x) else tryCatch(posterior::ess_bulk(x), error = function(e) NA_real_)
  }
  out$rhat_max <- suppressWarnings(max(unlist(out[grep("^rhat_", names(out))])))
  out$ess_min  <- suppressWarnings(min(unlist(out[grep("^ess_", names(out))])))
  out
}
