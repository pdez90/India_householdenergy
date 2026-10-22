# ==============================================================================
# 05b_imputation_components.R -- the calibration posterior DECOMPOSED, for the
# outcome-conditioned imputation, version 2 (docs/mime_v2_spec.md).
#
# Nothing is refitted. From the cached brms fits written by 05_correction.R it
# extracts, on the same thinned draw indices 05 used (draw_index), for each fit:
#   a      intercept                (b_Intercept)
#   b      slope on the true NFHS logit (bsp_mex_obsx_se)
#   tau    state-effect SD          (sd_state_name__Intercept)
#   sigma  equation-error SD        (sigma)
#   U      K x |C_f| matrix of the calibrated states' effects (r_state_name[...])
# so the corrected logit of a district d is
#   L_d = a + b * X_d + u_s(d),  u_s = U[, s] if s is calibrated,
#                                u_s = tau * eta_s, eta_s ~ N(0, 1) otherwise,
# with X_d = m_d + sd_d * z_d the true NFHS value (joint two-round shrinkage of
# 05: m, sd and the between-round posterior correlation r_d).
#
# CHECK: for districts in calibrated states, a + b * m + U reproduces the shared
# part S that 05 saved (correction_posterior_components.rds) draw by draw; for
# the other districts the saved S used brms's new-level draws, so only the
# distribution can be compared (mean and SD of S - a - b m against 0 and tau).
#
# Inputs : brms_me_nfhs4_access.rds, brms_me_nfhs5_ires.rds,
#          correction_posterior_components.rds (05)
# Output : imputation_components_v2.rds, imputation_components_v2_check.csv
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr) })
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()

comp <- readRDS(file.path(dir_out, "correction_posterior_components.rds"))
fits <- list(A = "brms_me_nfhs4_access.rds", B = "brms_me_nfhs5_ires.rds")
surf <- list(A = "lpg_2015", B = "lpg_2019")
dmat <- function(fit, v) as.matrix(posterior::as_draws_matrix(fit)[, v, drop = FALSE])

out <- list(); chk <- list()
for (f in names(fits)) {
  fit <- readRDS(file.path(dir_out, fits[[f]]))
  sc  <- comp$surfaces[[surf[[f]]]]
  idx <- sc$draw_index
  vars <- posterior::variables(fit)
  rv   <- grep("^r_state_name\\[.*,Intercept\\]$", vars, value = TRUE)
  # brms writes factor levels with spaces as dots in variable names; map back
  # through the levels of the fitted data.
  lev  <- levels(factor(fit$data$state_name))
  lev_dot <- gsub("[^A-Za-z0-9]", ".", lev)
  st   <- lev[match(sub("^r_state_name\\[(.*),Intercept\\]$", "\\1", rv), lev_dot)]
  if (anyNA(st)) stop("could not map state-effect names for fit ", f)
  pars <- list(a = as.numeric(dmat(fit, "b_Intercept")[idx, 1]), b = as.numeric(dmat(fit, "bsp_mex_obsx_se")[idx, 1]),
               tau = as.numeric(dmat(fit, "sd_state_name__Intercept")[idx, 1]), sigma = as.numeric(dmat(fit, "sigma")[idx, 1]),
               U = { u <- unclass(dmat(fit, rv))[idx, , drop = FALSE]; u <- matrix(as.numeric(u), nrow(u)); colnames(u) <- st; u })
  stopifnot(all(abs(pars$b - sc$b) < 1e-12))     # same draws as 05's components
  # district map (states as in 05's prep; me_shrinkage carries state_name)
  sh <- read.csv(file.path(dir_out, "me_shrinkage.csv"), stringsAsFactors = FALSE)
  sh <- sh[sh$surface == surf[[f]], ]
  dist <- as.character(as.numeric(sc$districts))
  dstate <- sh$state_name[match(dist, as.character(as.numeric(sh$district)))]
  calib  <- dstate %in% st
  # reconstruction check
  rec <- pars$a + outer(pars$b, sc$mu_joint)
  ucal <- matrix(0, length(idx), length(dist)); ucal[, calib] <- pars$U[, match(dstate[calib], st)]
  dev  <- sc$S - rec
  chk[[f]] <- data.frame(fit = f, n_districts = length(dist), n_calibrated_districts = sum(calib),
    n_calibrated_states = length(st),
    max_abs_diff_calibrated = max(abs(dev[, calib] - ucal[, calib])),
    newstate_mean = mean(dev[, !calib]),
    newstate_sd_ratio = stats::sd(as.numeric(dev[, !calib])) / sqrt(mean(pars$tau^2)))
  out[[f]] <- c(pars, list(calibrated_states = st, districts = dist, state = dstate, calibrated = calib,
                          m = sc$mu_joint, sd = sc$x_sd, Lbar = sc$Lbar, draw_index = idx))
  rm(fit); gc(verbose = FALSE)
}
out$rho <- comp$rho
out$note <- "L = a + b*(m + sd*z) + u_state; u = U[,state] if calibrated else tau*eta; z correlated across rounds by rho"
saveRDS(out, file.path(dir_out, "imputation_components_v2.rds"))
ck <- bind_rows(chk)
write.csv(ck, file.path(dir_out, "imputation_components_v2_check.csv"), row.names = FALSE)
print(ck, digits = 4)
# Tolerance: 05 predicted with x_se = SE_FLOOR (1e-4), so brms added noise of
# SD b * 1e-4 to each saved S; the largest of ~10^6 such terms is ~5e-4.
stopifnot(all(ck$max_abs_diff_calibrated < 2e-3),            # reconstruction for calibrated states
          all(abs(ck$newstate_mean) < 0.05), all(abs(ck$newstate_sd_ratio - 1) < 0.1))
message("wrote imputation_components_v2.rds (reconstruction checks passed)")
