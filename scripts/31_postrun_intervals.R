# ==============================================================================
# 31_postrun_intervals.R -- reporting-only summaries computed AFTER the pipeline,
# from files it has already written. Nothing here refits a model or changes a
# reported estimate.
#
# (A) Between-round correlation of the latent NFHS district values (05's joint
#     shrinkage): a 500-replicate state-cluster bootstrap that keeps the
#     UNCAPPED estimate of every replicate as well as the capped one, and the
#     share of replicates at the +/-0.95 cap. 05's own interval is computed
#     from capped values, which is distorted if many replicates reach the cap.
#     Input: me_shrinkage.csv (05). Output: me_shrinkage_joint_bootstrap.csv
#     (per replicate) and me_shrinkage_joint_bootstrap_summary.csv.
#
# (B) Coverage and rejection rates of the validation simulation (29) with
#     Wilson and exact (Clopper-Pearson) 95% intervals, which stay informative
#     at 0% or 100%, where the normal-approximation margin collapses to zero.
#     Input: mime_validation_reps.csv. Output: mime_validation_intervals.csv.
#
#   cd scripts && OUTPUT_DIR=~/Downloads/ACCESS_energy/outputs Rscript 31_postrun_intervals.R
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()

## ---- (A) correlation bootstrap -----------------------------------------------
# Reuse 05's own estimator code (eb_shrink, eb_joint) verbatim.
src <- readLines(if (exists("DIR_SCRIPTS")) file.path(DIR_SCRIPTS, "05_correction.R") else "05_correction.R")
a <- grep("^eb_shrink <- function", src); b <- grep("^SHRINK_LOG <- list\\(\\)", src)
SE_CAP <- 2
eval(parse(text = src[a:(b - 1)]))
sh <- read.csv(file.path(dir_out, "me_shrinkage.csv"), stringsAsFactors = FALSE)
mk <- function(surf) sh %>% filter(surface == surf) %>%
  transmute(district = as.character(district), state_name, x_obs, x_se)
d15 <- mk("lpg_2015"); d19 <- mk("lpg_2019")
J0 <- eb_joint(d15, d19, boot = FALSE, quiet = TRUE)
set.seed(20261022L)
sts <- unique(as.character(d15$state_name)); B <- 500L
boot <- bind_rows(lapply(seq_len(B), function(k) {
  s <- sample(sts, length(sts), replace = TRUE)
  pick <- function(d) bind_rows(lapply(seq_along(s), function(i) {
    x <- d[as.character(d$state_name) == s[i], , drop = FALSE]
    x$state_name <- paste0(s[i], "#", i); x$district <- paste0(x$district, "#", i); x }))
  r <- tryCatch(eb_joint(pick(d15), pick(d19), boot = FALSE, quiet = TRUE), error = function(e) NULL)
  tibble(replicate = k, ok = !is.null(r),
         rho_uncapped = if (is.null(r)) NA_real_ else r$rho_raw,
         rho_capped   = if (is.null(r)) NA_real_ else r$rho_prior)
}))
write.csv(boot, file.path(dir_out, "me_shrinkage_joint_bootstrap.csv"), row.names = FALSE)
q <- function(v, p) unname(stats::quantile(v, p, na.rm = TRUE))
bs <- tibble(
  estimate_uncapped = J0$rho_raw, estimate_used = J0$rho_prior,
  replicates = B, failed = sum(!boot$ok),
  share_at_cap = mean(abs(boot$rho_capped[boot$ok]) >= 0.95 - 1e-9),
  ci_uncapped_lo = q(boot$rho_uncapped, .025), ci_uncapped_hi = q(boot$rho_uncapped, .975),
  ci_capped_lo = q(boot$rho_capped, .025), ci_capped_hi = q(boot$rho_capped, .975))
write.csv(bs, file.path(dir_out, "me_shrinkage_joint_bootstrap_summary.csv"), row.names = FALSE)
cat("\n== (A) Between-round correlation of latent district values ==\n"); print(as.data.frame(bs), digits = 3)

## ---- (B) Wilson / exact intervals for the validation rates -------------------
# (B) belongs to the validation of the abandoned outcome-conditioned imputation
# (29_mime_validation.R, no longer in the pipeline); it runs only on request.
pr <- file.path(dir_out, "mime_validation_reps.csv")
if (identical(Sys.getenv("MIME_VALIDATION", "0"), "1") && file.exists(pr)) {
  wilson <- function(x, n, z = 1.96) {
    if (n == 0) return(c(NA_real_, NA_real_))
    p <- x / n; den <- 1 + z^2 / n
    ctr <- (p + z^2 / (2 * n)) / den; hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
    c(ctr - hw, ctr + hw)
  }
  exact <- function(x, n) if (n == 0) c(NA_real_, NA_real_) else as.numeric(stats::binom.test(x, n)$conf.int)
  reps <- read.csv(pr, stringsAsFactors = FALSE)
  reps$failed <- as.logical(reps$failed)
  iv <- reps %>% group_by(scen, beta, method) %>% group_modify(function(g, k) {
    s <- g[!g$failed & is.finite(g$cover), , drop = FALSE]; n <- nrow(s)
    xc <- sum(s$cover); xr <- sum(s$reject)
    wc <- wilson(xc, n); wr <- wilson(xr, n); ec <- exact(xc, n); er <- exact(xr, n)
    tibble(attempted = nrow(g), succeeded = n, failed = sum(g$failed),
           coverage = if (n) xc / n else NA_real_, coverage_wilson_lo = wc[1], coverage_wilson_hi = wc[2],
           coverage_exact_lo = ec[1], coverage_exact_hi = ec[2],
           reject_rate = if (n) xr / n else NA_real_, reject_wilson_lo = wr[1], reject_wilson_hi = wr[2],
           reject_exact_lo = er[1], reject_exact_hi = er[2])
  }) %>% ungroup() %>% rename(scenario = scen) %>% arrange(scenario, beta, method)
  write.csv(iv, file.path(dir_out, "mime_validation_intervals.csv"), row.names = FALSE)
  cat("\n== (B) Validation rates with Wilson and exact 95% intervals ==\n")
  print(as.data.frame(iv %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))), row.names = FALSE)
} else message("(B) skipped: validation of the abandoned outcome-conditioned imputation (set MIME_VALIDATION=1 to run)")
message("wrote me_shrinkage_joint_bootstrap*.csv")
