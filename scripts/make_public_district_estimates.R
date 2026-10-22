## make_public_district_estimates.R -- the district-level LPG prevalence
## estimates, by every method the paper produces, as a publishable dataset.
##
##   cd <repository>/scripts
##   ACCESS_DIR=/path/to/data OUTPUT_DIR="$ACCESS_DIR/outputs" \
##     Rscript make_public_district_estimates.R
##
## WHY THIS IS NOT A PIPELINE STEP
##   It emits NO checks. Every pipeline script appends to
##   diagnostics/pipeline_checks.csv under the current CHK_RUN_ID, so running one
##   by hand against a finished output tree would drop that script's rows from
##   the canonical trail and re-add them under a new id -- turning a single-run
##   trail into a blend, and making release_build.csv report MIXED. This script
##   reads the outputs and writes a new folder. It touches nothing else.
##
## WHAT IT WRITES  (into <repository>/data/, to be committed)
##   district_lpg_estimates.csv             one row per district
##   district_lpg_estimates_dictionary.csv  what each column means
##   district_lpg_estimates_provenance.csv  which analysis run produced them
##
## The microdata behind these estimates cannot be redistributed. These are the
## MODEL OUTPUTS -- our own derived quantities -- which can be.

suppressPackageStartupMessages({library(dplyr); library(readr); library(tibble)})
if (!exists(".env_dir")) source("paths.R")
dir_out <- .dir_out()

## The files go into the REPOSITORY, not into the output folder, because the
## point of them is to be committed and citable -- CITATION.cff invites citation
## of "this code or the derived district estimates", and an estimate that lives
## only in an untracked output folder is not something a reader can reach.
## Writing them here rather than copying them by hand also means a re-run
## refreshes the committed copy: a hand copy would silently go stale the next
## time the pipeline ran, which is the same trap as a stale builder.
## Precedent: 00_snapshot_env.R already writes renv.lock and sessionInfo.txt
## into the repository for the same reason.
repo <- local({
  here <- tryCatch({
    a <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", a[grepl("^--file=", a)])
    if (length(f)) dirname(normalizePath(f[1])) else getwd()
  }, error = function(e) getwd())
  dirname(here)
})
if (!file.exists(file.path(repo, "README.md")))
  stop("expected the repository root at ", repo, " -- run this from scripts/",
       call. = FALSE)
pub <- file.path(repo, "data")
dir.create(pub, showWarnings = FALSE, recursive = TRUE)

src <- file.path(dir_out, "corrected_nfhs_districts.rds")
if (!file.exists(src)) stop("not found: ", src, call. = FALSE)
d <- readRDS(src)
message("read ", basename(src), ": ", nrow(d), " rows, ", ncol(d), " columns")

## District codes are character in the corrected surface and numeric in the SI
## sample-size table, so joining them raw fails. Both sides go through this.
## as.integer() first, because as.character(638) is "638" but as.character(638.0)
## read from a CSV can be "638" or "638.0" depending on the reader.
.key <- function(x) trimws(as.character(if (is.numeric(x)) as.integer(x) else x))

want <- c(
  district          = "district",
  raw_ml_2015       = "lpg_2015_rural",      raw_ml_2019       = "lpg_2019_rural",
  raw_wt_2015       = "lpg_2015_rural_wt",   raw_wt_2019       = "lpg_2019_rural_wt",
  raw_wt_se_2015    = "lpg_2015_rural_wt_se",raw_wt_se_2019    = "lpg_2019_rural_wt_se",
  rc_ml_2015        = "lpg_2015_rc",         rc_ml_2019        = "lpg_2019_rc",
  bayes_2015        = "lpg_2015_bayes",      bayes_2019        = "lpg_2019_bayes",
  bayes_lo_2015     = "lpg_2015_bayes_lo",   bayes_lo_2019     = "lpg_2019_bayes_lo",
  bayes_hi_2015     = "lpg_2015_bayes_hi",   bayes_hi_2019     = "lpg_2019_bayes_hi",
  bayes_iresonly_2015 = "lpg_2015_irescal_bayes",
  in_calib_state_2015 = "lpg_2015_in_support", in_calib_state_2019 = "lpg_2019_in_support",
  in_calib_range_2015 = "lpg_2015_cov_support", in_calib_range_2019 = "lpg_2019_cov_support")
have <- want[want %in% names(d)]
miss <- want[!want %in% names(d)]
if (length(miss))
  message("NOT FOUND (omitted from the public file, not silently blank):\n  ",
          paste(sprintf("%s <- %s", names(miss), miss), collapse = "\n  "))

out <- d[, unname(have), drop = FALSE]
names(out) <- names(have)
out$district <- .key(out$district)

## ---- the fifth surface: regression calibration on the DESIGN-WEIGHTED input --
## 18_correction_comparison.R computes this but stores only the national means
## and the between-estimator correlations, never the district column. The fit is
## a weighted lm on the overlap districts and is therefore deterministic, so it
## is recomputed here rather than left out -- and then CHECKED against the
## correlation 18 did store, so a silent divergence in the replication shows up
## as a failure instead of as a plausible extra column.
clamp   <- function(p, eps = 1e-3) pmin(pmax(p, eps), 1 - eps)
fit_rc  <- function(pair, ref, nfhs_in, wtcol) {
  f <- stats::as.formula(sprintf("qlogis(clamp(%s)) ~ qlogis(clamp(%s))", ref, nfhs_in))
  dd <- pair[stats::complete.cases(pair[[ref]], pair[[nfhs_in]], pair[[wtcol]]), , drop = FALSE]
  stats::lm(f, data = dd, weights = dd[[wtcol]])
}
rc_apply <- function(pair, ref, nfhs_in, wtcol, newx) {
  fit <- fit_rc(pair, ref, nfhs_in, wtcol)
  nd  <- stats::setNames(data.frame(newx), nfhs_in)
  as.numeric(plogis(predict(fit, newdata = nd)))
}
cp <- file.path(dir_out, "compare_pairs.rds")
if (file.exists(cp) && all(c("raw_wt_2015","raw_wt_2019") %in% names(out))) {
  pairs <- readRDS(cp)
  out$rc_wt_2015 <- rc_apply(pairs$pairA, "access_w1_mainlpg",  "lpg_2015_rural_wt",
                             "n_access_w1",  out$raw_wt_2015)
  out$rc_wt_2019 <- rc_apply(pairs$pairB, "ires_mainlpg_rural", "lpg_2019_rural_wt",
                             "n_ires_rural", out$raw_wt_2019)
  mixf <- file.path(dir_out, "estimator_mix_correlations.csv")
  if (file.exists(mixf) && "rc_ml_2015" %in% names(out)) {
    mc <- readr::read_csv(mixf, show_col_types = FALSE)
    for (yr in c("2015", "2019")) {
      stored <- suppressWarnings(as.numeric(
        mc$rc_wt[mc$year == as.numeric(yr) & mc$estimator == "rc_ml"]))
      here <- suppressWarnings(cor(out[[paste0("rc_ml_", yr)]],
                                   out[[paste0("rc_wt_", yr)]], use = "complete.obs"))
      if (length(stored) == 1 && is.finite(stored) && is.finite(here)) {
        okc <- abs(round(here, 3) - stored) <= 0.002
        message(sprintf("  [%s] rc_wt %s reproduces 18_correction_comparison.R: r = %.3f vs stored %.3f",
                        if (okc) "OK" else "MISMATCH", yr, here, stored))
        if (!okc) warning("rc_wt replication does not match the stored correlation for ",
                          yr, " -- do not publish this column until it does",
                          call. = FALSE, immediate. = TRUE)
      }
    }
  }
} else message("compare_pairs.rds or the design-weighted columns are absent -- rc_wt omitted")

## District and state names, from the SI sample-size table -- the same district
## key the analysis uses.
ss <- file.path(dir_out, "si_sample_size_district.csv")
if (file.exists(ss)) {
  nm <- readr::read_csv(ss, show_col_types = FALSE) %>%
    dplyr::select(district, dist_name, state_name) %>%
    dplyr::mutate(district = .key(district)) %>%
    dplyr::distinct(district, .keep_all = TRUE)
  before <- nrow(out)
  out <- dplyr::left_join(out, nm, by = "district")
  stopifnot(nrow(out) == before)
  message(sprintf("  names matched for %d of %d districts",
                  sum(!is.na(out$dist_name)), nrow(out)))
} else message("si_sample_size_district.csv absent -- no district/state names")

## ---- flags stated as the consequence, not the condition ---------------------
## These shipped as in_calib_state_* / in_calib_range_*, where TRUE meant "fine".
## A column named for the condition reads as metadata, and 62% of districts are
## extrapolated in 2015 -- so the single most important caveat about this file
## was the one a reader skimming the header was least likely to register. Named
## for the consequence and with the sense flipped, TRUE now means "treat with
## caution" in every flag, and the two flags no longer have opposite polarity.
## NA stays NA: a district with no estimate is not thereby un-extrapolated.
for (yr in c("2015", "2019")) {
  ic <- paste0("in_calib_state_", yr)
  if (ic %in% names(out)) {
    out[[paste0("extrapolated_", yr)]] <- !out[[ic]]
    out[[ic]] <- NULL
  }
  ir <- paste0("in_calib_range_", yr)
  if (ir %in% names(out)) {
    out[[paste0("outside_calib_range_", yr)]] <- !out[[ir]]
    # 05's support flag is FALSE (not NA) for a district with no raw estimate.
    rw <- paste0("raw_wt_", yr)
    if (rw %in% names(out))
      out[[paste0("outside_calib_range_", yr)]][is.na(out[[rw]])] <- NA
    out[[ir]] <- NULL
  }
}

if (all(c("bayes_2015","bayes_2019") %in% names(out)))
  out$change_bayes <- out$bayes_2019 - out$bayes_2015
if (all(c("raw_ml_2015","raw_ml_2019") %in% names(out)))
  out$change_raw_ml <- out$raw_ml_2019 - out$raw_ml_2015

out <- out %>% relocate(any_of(c("district","dist_name","state_name")))
readr::write_csv(out, file.path(pub, "district_lpg_estimates.csv"))
message("wrote ", file.path(pub, "district_lpg_estimates.csv"),
        " (", nrow(out), " districts, ", ncol(out), " columns)")

dict <- tibble::tribble(
 ~column, ~meaning,
 "district","2011 census district code, the key used throughout the analysis",
 "dist_name","district name as in the analysis shapefile",
 "state_name","state name as in the analysis shapefile",
 "raw_ml_2015/2019","RAW NFHS: multilevel (glmer) rural district estimate. The paper's primary raw specification.",
 "raw_wt_2015/2019","RAW NFHS: design-weighted direct rural estimate (Taylor-linearized).",
 "raw_wt_se_2015/2019","design-based standard error of the above, on the probability scale",
 "rc_ml_2015/2019","REGRESSION CALIBRATION fitted on the multilevel estimate. Transparent, carries no uncertainty.",
 "rc_wt_2015/2019","REGRESSION CALIBRATION refitted on the design-weighted estimate. The input-sensitivity variant; agrees with rc_ml at r = 0.98.",
 "bayes_2015/2019","BAYESIAN measurement-error correction: posterior mean of the district's latent logit prevalence, back-transformed. THE PAPER'S PRIMARY CORRECTED SURFACE.",
 "bayes_lo_2015/2019","2.5th percentile of the posterior for the corrected prevalence",
 "bayes_hi_2015/2019","97.5th percentile of the posterior (with the above, a 95% credible interval)",
 "bayes_iresonly_2015","2015 corrected with the IRES calibration instead of ACCESS: the instrument-consistent sensitivity",
 "extrapolated_2015/2019","TRUE means the district is OUTSIDE the states the calibration was estimated on, so its state effect was drawn from the fitted distribution rather than estimated. TREAT THESE ESTIMATES WITH CAUTION. For 2015 this is true of most of India, because ACCESS covers six states.",
 "outside_calib_range_2015/2019","TRUE means the district's raw design-weighted prevalence (raw_wt) lies OUTSIDE the range the calibration was fitted over, so the correction is an extrapolation on the prevalence scale too. Treat with caution.",
 "change_bayes","bayes_2019 - bayes_2015. This, not the levels, is the exposure in the health models.",
 "change_raw_ml","raw_ml_2019 - raw_ml_2015, for comparison")
keep <- vapply(dict$column, function(c) {
  stem <- sub("/.*$", "", c)
  any(names(out) == c) || any(startsWith(names(out), sub("_2015$", "", stem)))
}, logical(1))
readr::write_csv(dict[keep, ], file.path(pub, "district_lpg_estimates_dictionary.csv"))
message("wrote ", file.path(pub, "district_lpg_estimates_dictionary.csv"))

## Which analysis run these numbers came from. Without this the committed CSV
## is a set of numbers with no stated origin, and the first question anyone
## asks of a published dataset is which version of the analysis produced it.
prov <- local({
  trail <- file.path(dir_out, "diagnostics", "pipeline_checks.csv")
  rid <- NA_character_
  if (file.exists(trail)) {
    tr <- tryCatch(utils::read.csv(trail, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(tr) && "run_id" %in% names(tr)) {
      u <- unique(tr$run_id[!is.na(tr$run_id)])
      rid <- if (length(u) == 1L) u else paste0("MIXED(", length(u), ")")
    }
  }
  tibble::tibble(
    quantity = c("analysis_run_id", "source_file", "generated", "n_districts", "generated_by"),
    value = c(rid, "corrected_nfhs_districts.rds",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), as.character(nrow(out)),
              "scripts/make_public_district_estimates.R"))
})
readr::write_csv(prov, file.path(pub, "district_lpg_estimates_provenance.csv"))
message("wrote ", file.path(pub, "district_lpg_estimates_provenance.csv"),
        "  (analysis run ", prov$value[1], ")")

message("\nCAVEATS THAT TRAVEL WITH THIS FILE:")
message("  * The corrected estimates are calibrated to a better-instrumented")
message("    reference, not validated against measured truth.")
message("  * Where extrapolated_YYYY is TRUE the state effect was drawn, not")
message("    estimated. For 2015 that is most of India.")
message("  * The credible intervals cover correction uncertainty only.")
