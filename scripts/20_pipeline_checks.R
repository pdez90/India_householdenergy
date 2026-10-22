# ==============================================================================
# 20_pipeline_checks.R   (runs LAST; consolidates every stage's self-checks)
#
# Two jobs:
#   1. Read diagnostics/pipeline_checks.csv (the trail every script appended to
#      during the run) and print a per-script PASS/WARN/FAIL summary.
#   2. Independently RE-VERIFY the key cross-stage PROPAGATION paths by reading
#      the final on-disk outputs -- so a value that silently failed to flow from
#      one stage to the next is caught here even if an upstream check didn't fire.
#
# Writes diagnostics/pipeline_checks_summary.txt and ends with a clear banner.
# Non-fatal: reports failures loudly but does not stop (so you see everything).
# ==============================================================================

source("00_config.R")
diag_dir <- file.path(dir_out, "diagnostics")
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

rd <- function(f) tryCatch(readRDS(file.path(dir_out, f)), error = function(e) NULL)
has <- function(x, cols) is.data.frame(x) && all(cols %in% names(x))

## ---- what produced these outputs -------------------------------------------
## The manuscript states the R version used. It was TYPED, and it was wrong: the
## document said 4.5 while every run has been 4.3.3. The number audit missed it
## because a bare "4.5" is indistinguishable from a section reference. So it is
## derived, from the session that actually wrote the outputs, by the script that
## runs immediately before the document is built.
.envsnap <- data.frame(
  quantity = c("r_version", "r_version_short", "platform", "recorded"),
  value = c(paste(R.version$major, R.version$minor, sep = "."),
            paste0(R.version$major, ".", sub("\\..*$", "", R.version$minor)),
            R.version$platform,
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  stringsAsFactors = FALSE)
utils::write.csv(.envsnap, file.path(DIR_OUT, "environment_snapshot.csv"),
                 row.names = FALSE)

cat("\n########################  PIPELINE VERIFICATION  ########################\n")

## ---- 1. Consolidate the per-script check trail -------------------------------
log_f <- file.path(diag_dir, "pipeline_checks.csv")
if (file.exists(log_f)) {
  trail <- tryCatch(read.csv(log_f, stringsAsFactors = FALSE),
                    error = function(e) NULL)
  if (!is.null(trail) && nrow(trail)) {
    # Is this trail one run, or several? A targeted re-run legitimately leaves
    # rows from the previous run for every script it did not touch, and that is
    # fine -- but it must be VISIBLE, because "ALL CHECKS PASSED" over a
    # blended trail means less than it appears to. Rows written before run ids
    # existed have no run_id and are reported as unknown.
    if (!"run_id" %in% names(trail)) trail$run_id <- NA_character_
    .rids <- table(ifelse(is.na(trail$run_id) | trail$run_id == "",
                          "(no run id recorded)", trail$run_id))
    if (length(.rids) > 1) {
      cat("\n== NOTE: this trail blends ", length(.rids), " runs ==\n", sep = "")
      cat("  Scripts not re-run keep the rows from the run that last executed them.\n")
      cat("  That is expected after a targeted re-run; it is stated here so that\n")
      cat("  a PASS is not read as covering more than it does.\n")
      for (i in seq_along(.rids))
        cat(sprintf("    %-40s %4d row(s)  [%s]\n", names(.rids)[i], .rids[i],
                    paste(sort(unique(trail$script[
                      ifelse(is.na(trail$run_id) | trail$run_id == "",
                             "(no run id recorded)", trail$run_id) == names(.rids)[i]])),
                      collapse = ", ")))
    }
    cat("\n== Per-script check tally (from pipeline_checks.csv) ==\n")
    tally <- as.data.frame.matrix(table(trail$script,
                                        factor(trail$status, c("PASS","WARN","FAIL"))))
    tally$script <- rownames(tally); rownames(tally) <- NULL
    print(tally[, c("script","PASS","WARN","FAIL")], row.names = FALSE)
    fails <- trail[trail$status == "FAIL", ]
    warns <- trail[trail$status == "WARN", ]
    if (nrow(fails)) { cat("\n-- FAILURES --\n")
      for (i in seq_len(nrow(fails)))
        cat(sprintf("  [FAIL] %-6s %s  %s\n", fails$script[i], fails$check[i], fails$detail[i])) }
    if (nrow(warns)) { cat("\n-- WARNINGS --\n")
      for (i in seq_len(nrow(warns)))
        cat(sprintf("  [WARN] %-6s %s  %s\n", warns$script[i], warns$check[i], warns$detail[i])) }
  } else cat("(pipeline_checks.csv present but empty)\n")
} else {
  cat("(no pipeline_checks.csv found -- scripts may have run before checks.R existed)\n")
}

## ---- 2. Independent end-to-end propagation re-verification -------------------
chk_header("20_pipeline_checks (end-to-end propagation)")
ires <- rd("ires_districts.rds")
pairs<- rd("compare_pairs.rds")
corr <- rd("corrected_nfhs_districts.rds")
draws<- rd("correction_posterior_draws.rds")
prox <- rd("district_exposure_proxy.rds")
hw   <- rd("health_district_wide.rds")
eff  <- tryCatch(read.csv(file.path(dir_out, "health_effects_table.csv"),
                          stringsAsFactors = FALSE), error = function(e) NULL)

# 03 -> 04 -> 05 : rural design-weighted IRES reference
chk("20", "03: rural design-wt IRES estimate exists",
    has(ires, c("ires_mainlpg_rural_wt","ires_mainlpg_rural_wt_se")))
chk("20", "04: it propagated into compare_pairs$pairB",
    !is.null(pairs) && has(pairs$pairB, "ires_mainlpg_rural_wt"))
chk("20", "05: correction ran (Bayes surfaces exist)",
    has(corr, c("lpg_2019_bayes","lpg_2015_bayes")))

# 05 : instrument-consistent (IRES-cal 2015) surface + draws
chk("20", "05: IRES-cal 2015 surface present (option 3)",
    has(corr, "lpg_2015_irescal_bayes"))
chk("20", "05: IRES-cal 2015 posterior draws saved",
    !is.null(draws) && "y2015_irescal" %in% names(draws))

# 05 -> H2 : the instrument-consistent sensitivity reached the health table
chk("20", "H2: instrument-consistent sensitivity in effects table",
    !is.null(eff) && "change_lpg_bayes_iresonly" %in% eff$term)
chk("20", "H2: MI (uncertainty) rows in effects table",
    !is.null(eff) && any(grepl("_MI$", eff$term)))
chk("20", "H2: eligible-birth ('birth_hmean') weighting used",
    !is.null(eff) && "birth_hmean" %in% eff$weighting)

# 06 : four-category composition
chk("20", "06: 4th composition category present in district proxy",
    has(prox, "p_other_nonsolid"))

# H1 : SES from IR + birth weights + design-weighted mortality + nonoverlap
chk("20", "H1: SES covariates present (IR women's file)",
    has(hw, c("poor_2019","mother_low_edu_2019")))
chk("20", "H1: harmonic-mean birth weight present",
    has(hw, "w_births_hmean"))
chk("20", "H1: design-weighted mortality present",
    has(hw, "neonatal_death_dw_2019"))
chk("20", "H1: nonoverlap-cohort SI file present",
    file.exists(file.path(dir_out, "health_district_wide_nonoverlap.rds")))

# Diagnostics atlas produced
chk("20", "19: ACCESS/IRES atlas + QC maps produced",
    file.exists(file.path(diag_dir, "qc_verification.pdf")) &&
    file.exists(file.path(diag_dir, "atlas_IRES.pdf")))

## ---- Mixed-model fit registry (singular / boundary fits) ---------------------
# checks.R records EVERY glmer fit in the pipeline to diagnostics/model_fits.csv.
# A singular fit means a variance component was estimated at exactly zero, i.e.
# the partial pooling that justifies the small-area estimator collapsed to
# complete pooling for that grouping factor. That is a legitimate boundary MLE,
# not automatically an error -- but the district estimate then reverts to the
# pooled mean and its apparent precision is borrowed rather than earned, so the
# rate belongs in the SI rather than in an unread warning stream.
fits_f <- file.path(diag_dir, "model_fits.csv")
if (file.exists(fits_f)) {
  fits <- tryCatch(read.csv(fits_f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(fits) && nrow(fits)) {
    fits$is_sing <- fits$singular %in% c(TRUE, "TRUE")
    fits$is_unk  <- is.na(fits$singular) | fits$singular %in% c("NA", "")
    by_s <- aggregate(cbind(n = rep(1L, nrow(fits)), sing = as.integer(fits$is_sing),
                            unk = as.integer(fits$is_unk)) ~ script,
                      data = fits, FUN = sum)
    by_s$pct_singular <- round(100 * by_s$sing / by_s$n, 1)
    cat("\n-- Mixed-model fits recorded this run (diagnostics/model_fits.csv) --\n")
    print(by_s[order(-by_s$pct_singular), c("script", "n", "sing", "unk", "pct_singular")],
          row.names = FALSE)
    chk_warn("20", "no singular mixed-model fits anywhere in the pipeline",
             sum(fits$is_sing) == 0,
             sprintf("%d of %d fits singular (%.1f%%) across %d script(s)",
                     sum(fits$is_sing), nrow(fits),
                     100 * mean(fits$is_sing), length(unique(fits$script))))
    write.csv(by_s[, c("script", "n", "sing", "unk", "pct_singular")],
              file.path(diag_dir, "model_fits_summary.csv"), row.names = FALSE)
  }
} else {
  chk_warn("20", "mixed-model fit registry written", FALSE,
           "diagnostics/model_fits.csv absent -- no glmer fit was recorded")
}

## ---- Final banner ------------------------------------------------------------
trail2 <- tryCatch(read.csv(log_f, stringsAsFactors = FALSE), error = function(e) NULL)
nfail <- if (!is.null(trail2)) sum(trail2$status == "FAIL") else NA
nwarn <- if (!is.null(trail2)) sum(trail2$status == "WARN") else NA
npass <- if (!is.null(trail2)) sum(trail2$status == "PASS") else NA

summary_txt <- c(
  "==================== PIPELINE VERIFICATION SUMMARY ====================",
  paste0("PASS: ", npass, "   WARN: ", nwarn, "   FAIL: ", nfail),
  if (!is.null(trail2) && any(trail2$status == "FAIL"))
    c("", "FAILURES:", apply(trail2[trail2$status == "FAIL", c("script","check","detail")], 1,
                             function(r) paste0("  ", paste(r, collapse = " | ")))),
  if (!is.null(trail2) && any(trail2$status == "WARN"))
    c("", "WARNINGS:", apply(trail2[trail2$status == "WARN", c("script","check","detail")], 1,
                             function(r) paste0("  ", paste(r, collapse = " | ")))))
writeLines(summary_txt, file.path(diag_dir, "pipeline_checks_summary.txt"))

cat("\n########################################################################\n")
if (isTRUE(nfail == 0)) {
  cat("  ALL CHECKS PASSED",
      if (isTRUE(nwarn > 0)) paste0(" (", nwarn, " warnings -- review above)") else "", "\n")
} else if (is.na(nfail)) {
  cat("  Could not read the check trail; review per-stage output above.\n")
} else {
  cat("  ", nfail, " CHECK(S) FAILED -- see diagnostics/pipeline_checks_summary.txt\n")
}
cat("########################################################################\n")
# STRICT_CHECKS: a publication run must not be able to finish with a failed
# check. The default stays permissive because that is the right behaviour while
# developing -- you want the rest of the pipeline to run so you can see what else
# broke -- but the canonical run sets STRICT_CHECKS=TRUE and stops here.
#
# This only works because FAIL means "something is wrong", not "something is
# imperfect". Known, disclosed limitations are chk_warn(), not chk(): the 2019
# NFHS-side floor discrepancy is a factor of 1.8 that the manuscript reports and
# explains, so it warns rather than failing. The rule is that a FAIL is always
# actionable; if a check fires that you intend to publish anyway, downgrade the
# check and say why, rather than raising the tolerance until it passes.
STRICT_CHECKS <- toupper(Sys.getenv("STRICT_CHECKS", "FALSE")) == "TRUE"
if (isTRUE(nfail > 0)) {
  msg <- paste0(nfail, " pipeline check(s) FAILED -- see diagnostics/",
                "pipeline_checks_summary.txt")
  if (STRICT_CHECKS) {
    cat("\n  STRICT_CHECKS=TRUE: stopping.\n")
    stop(msg, call. = FALSE)
  }
  warning(msg)
} else if (STRICT_CHECKS) {
  cat("  STRICT_CHECKS=TRUE and no check failed.",
      if (isTRUE(nwarn > 0)) paste0(" ", nwarn, " warning(s) -- each is a known,")
      else "", if (isTRUE(nwarn > 0)) " documented limitation; read them." else "", "\n")
}

message("20_pipeline_checks.R done -> diagnostics/pipeline_checks_summary.txt")
