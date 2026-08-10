# ==============================================================================
# run_everything.R  --  ONE command for the whole pipeline, in dependency order.
#
#   data prep -> correction -> augmentation -> health analyses -> figures/atlas
#   -> (optionally) rebuild the Word manuscript from the fresh figures.
#
# WHY THIS EXISTS
#   The pipeline used to live in three disconnected pieces (run_all.R, run_health.R,
#   and a loose set of figure scripts 08-15). Running one but not the others left
#   the manuscript's tables and embedded figures lagging the data -- the classic
#   "figures don't match the numbers" trap. This script runs the ENTIRE chain in
#   the correct order, so all analytical outputs are regenerated from the same
#   pipeline version. Prefer this over run_all.R / run_health.R for any refresh.
#
#   Note the limit of that guarantee: running everything in order means no output
#   is STALE relative to another. It does NOT mean the estimands are correct, and
#   it does not mean the self-checks passed -- the checks are non-fatal by design
#   and are consolidated by 20_pipeline_checks.R. Read that consolidation, and the
#   warning summary this script prints at the end, before believing any number.
#
# USAGE (from the scripts folder):
#   Rscript run_everything.R
#   START_FROM=05_correction.R Rscript run_everything.R   # resume from a step
#   SKIP_STATIC=FALSE Rscript run_everything.R            # also re-run 01 and H3
#   BUILD_DOCX=FALSE Rscript run_everything.R             # stop before the docx
#
# WARNINGS
#   Every warning raised by any step is captured with its step tag, printed
#   immediately (options(warn = 1)), classified, and written to
#   diagnostics/pipeline_warnings.csv. A run that emits dozens of warnings is not
#   a clean run just because it exited 0.
#
# THE ANTI-STALENESS RULE
#   Whenever you edit ANY script, re-run this from that script onward
#   (START_FROM=<script>). Everything downstream then regenerates, so figures and
#   tables can never silently fall behind the numbers. The only steps skipped by
#   default are the two slow, essentially-static ones (01 NFHS prep; H3 ambient
#   covariates) -- set SKIP_STATIC=FALSE if you changed those or their raw inputs.
# ==============================================================================

## ---- run from this file's directory regardless of how it is invoked ----------
.args <- commandArgs(trailingOnly = FALSE)
.fa <- grep("^--file=", .args, value = TRUE)
if (length(.fa)) setwd(dirname(sub("^--file=", "", .fa[1])))
# Remember where the scripts live, as an absolute path. The docx build at the
# end setwd()s into DIR_OUT, so it cannot rely on the working directory still
# pointing here when it needs to source a sibling script.
DIR_SCRIPTS <- normalizePath(getwd())

## ---- warnings surface immediately, not as a "50 warnings" lump at the end -----
# warn = 1 prints each warning as it occurs, so it is attributable to the step
# (and the line) that raised it. The withCallingHandlers in run_step() below then
# also RECORDS it, because R's deferred warning list is capped at 50 and silently
# truncates -- which is exactly how a pipeline ends up with unread warnings.
options(warn = 1)
WARN_LOG <- new.env(parent = emptyenv())
WARN_LOG$rows <- list()

## ---- CONFIG --------------------------------------------------------------------
# DIR_OUT is read from 00_config.R (which itself honors the OUTPUT_DIR /
# ACCESS_DIR / DHS_DIR environment variables), so the output location is
# defined in exactly ONE place and cannot drift between the runner and the
# scripts it runs.
.cfg <- new.env()
source("00_config.R", local = .cfg)
DIR_OUT     <- .cfg$dir_out
START_FROM  <- Sys.getenv("START_FROM", "")                 # "" = from the beginning
SKIP_STATIC <- toupper(Sys.getenv("SKIP_STATIC", "TRUE"))  == "TRUE"
BUILD_DOCX  <- toupper(Sys.getenv("BUILD_DOCX",  "TRUE"))  == "TRUE"

## ---- required packages (union across all scripts) ----------------------------
required <- c("tidyverse","haven","readstata13","sf","lme4","survey","srvyr",
              "WeMix","broom","broom.mixed","brms","nnet","pROC","cowplot",
              "naniar","lmtest","sandwich",
              "terra","exactextractr","ncdf4","patchwork")  # H3 ambient + atlas
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing))
  stop("Missing packages -- install first:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))")

## ---- the pipeline, in strict dependency order --------------------------------
# pop    : NA (no POP), or "rural"/"all" for the population-specific health steps
# static : TRUE = slow & essentially fixed; skipped if `out` exists and SKIP_STATIC
steps <- list(
  # --- core exposure pipeline ---
  list(s = "01_prep_nfhs.R",           pop = NA, static = TRUE, out = "nfhs_districts.rds"),
  list(s = "02_prep_access.R",         pop = NA),
  list(s = "03_prep_ires.R",           pop = NA),
  list(s = "04_compare.R",             pop = NA),
  # NSSO-78 MIS external validation (manuscript SI section S8, Table S9.19,
  # Figure S10.16). 25 needs only the MoSPI unit data + the district key
  # (data/nsso78_district_key.csv, resolved automatically); 26 joins
  # nsso78_districts.rds to the 01/03 outputs, so both sit here with the
  # other cross-survey comparisons and BEFORE the docx build, which reads
  # the nsso78_* CSVs they write.
  list(s = "25_prep_nsso78.R",         pop = NA),
  list(s = "26_compare_nsso78.R",      pop = NA),
  list(s = "05_correction.R",          pop = NA),
  list(s = "06_stacking_prediction.R", pop = NA),
  # Why does the IRES-trained stacking model fail to transfer to ACCESS? 21 splits
  # that into an ERA gap and an INSTRUMENT gap using ACCESS wave 2 (2018), and
  # reports which individual predictors keep a stable relationship to the outcome.
  # Consumes harmonized_frames.rds, written by 06 -- so it must follow 06.
  list(s = "21_transfer_diagnostics.R", pop = NA),
  # --- health analyses (H3 makes the ambient covariates H2/H4 need) ---
  list(s = "H3_env_covariates.R",      pop = NA, static = TRUE, out = "df_wide_health.rds"),
  list(s = "H1_prep_mortality.R",      pop = NA),
  list(s = "H2_health_models.R",       pop = "rural"),
  list(s = "H4_si_adult_health.R",     pop = "rural"),
  list(s = "H2_health_models.R",       pop = "all"),
  list(s = "H4_si_adult_health.R",     pop = "all"),
  # SI (#16): re-run the mortality models on the short-window, non-overlapping
  # cohort produced by H1 (health_district_wide_nonoverlap.rds).
  list(s = "H2_health_models.R",       pop = "rural",
       env = list(HEALTH_VARIANT = "_nonoverlap")),
  list(s = "H5_health_nuanced.R",      pop = "rural"),
  list(s = "H6_si_health_energy_scatter.R", pop = "rural"),
  # How precise would a validation survey have to be for the change-on-change
  # design to return a conventionally significant answer? Traces the MI estimate
  # across calibration precision, splits the corrected-exposure posterior variance
  # into a calibration-side part (buyable) and an NFHS-side part (invariant), and
  # maps the result back to survey designs. Validates its endpoints against
  # health_effects_table.csv, so it must follow H2.
  list(s = "22_design_analysis.R",     pop = NA),

  # Which estimand is the corrected exposure? posterior_epred() carries the
  # calibration uncertainty but excludes the residual sigma. This script reports
  # the sigma-inclusive (posterior-predictive) sensitivity for readers who hold
  # the "district's true prevalence" estimand, and checks that averaging the
  # posterior on the probability scale rather than the logit scale does not move
  # the coefficient. Reads 22's sigma decomposition, so it must follow 22.
  list(s = "23_ppd_sensitivity.R",     pop = NA),
  # --- figures, atlas, diagnostics (run AFTER data + health so they never lag) --
  list(s = "07_diagnostics_maps.R",    pop = NA),
  list(s = "08_si_benchmarks.R",       pop = NA),
  list(s = "09_season_sensitivity.R",  pop = NA),
  list(s = "10_make_figures.R",        pop = NA),   # paper_figs/fig1,2,3
  list(s = "12_make_all_maps.R",       pop = NA),   # atlas (needs H1 mortality)
  list(s = "13_benchmark_maps.R",      pop = NA),
  list(s = "14_benchmark_side_by_side.R", pop = NA),
  list(s = "15_variable_importance.R", pop = NA),
  list(s = "16_ml_vs_designwt.R",      pop = NA),   # SI: multilevel vs design-weighted
  list(s = "17_missingness.R",         pop = NA),   # SI: missing-data accounting
  list(s = "18_correction_comparison.R", pop = NA),  # SI: RC (multilevel vs design-wt) + Bayes mix
  list(s = "19_ires_access_atlas.R",     pop = NA),  # detailed ACCESS/IRES atlas + per-stage QC maps
  list(s = "24_si_sample_sizes.R",       pop = NA),  # SI: per-district/state sample sizes (NFHS/ACCESS/IRES; NSSO counts come from 25's nsso78_hh.rds)
  list(s = "20_pipeline_checks.R",       pop = NA)   # consolidate self-checks + end-to-end verification
)

## ---- step runner: fresh env, optional POP, timing, clear failure -------------
run_step <- function(st) {
  tag <- if (is.na(st$pop)) st$s else paste0(st$s, "  [POP = ", st$pop, "]")
  if (isTRUE(st$static) && SKIP_STATIC && !is.null(st$out) &&
      file.exists(file.path(DIR_OUT, st$out))) {
    message("== SKIP ", tag, " (", st$out, " exists; SKIP_STATIC=TRUE) ==")
    return(invisible())
  }
  if (!is.null(st$env)) tag <- paste0(tag, "  [", paste(names(st$env), unlist(st$env),
                                                        sep = "=", collapse = ", "), "]")
  message("\n==== ", tag, "  [", format(Sys.time(), "%H:%M:%S"), "] ====")
  t0 <- Sys.time(); e <- new.env()
  if (!is.na(st$pop)) assign("POP", st$pop, envir = e)
  # Per-step environment variables (e.g. HEALTH_VARIANT for the nonoverlap SI run);
  # set before sourcing and cleared afterwards so they don't leak to later steps.
  if (!is.null(st$env)) {
    do.call(Sys.setenv, st$env)
    on.exit(Sys.unsetenv(names(st$env)), add = TRUE)
  }
  # withCallingHandlers records each warning WITHOUT muffling it, so warn = 1 still
  # prints it in place; invokeRestart("muffleWarning") would hide it from the log.
  nw0 <- length(WARN_LOG$rows)
  withCallingHandlers(
    tryCatch(source(st$s, echo = FALSE, local = e),
      error = function(err) stop("\n*** ", tag, " FAILED: ", conditionMessage(err),
        "\nFix the issue, then resume with:  START_FROM=", st$s,
        " Rscript run_everything.R", call. = FALSE)),
    warning = function(w) {
      cl <- tryCatch(paste(deparse(conditionCall(w)), collapse = " "),
                     error = function(e) NA_character_)
      WARN_LOG$rows[[length(WARN_LOG$rows) + 1L]] <- data.frame(
        step = st$s, pop = if (is.na(st$pop)) "" else st$pop,
        message = conditionMessage(w),
        call = if (is.null(cl)) NA_character_ else substr(cl, 1, 300),
        stringsAsFactors = FALSE)
    })
  nw <- length(WARN_LOG$rows) - nw0
  message("==== ", tag, " done in ",
          round(difftime(Sys.time(), t0, units = "mins"), 1), " min",
          if (nw > 0) paste0("  [", nw, " warning", if (nw > 1) "s" else "", "]") else "",
          " ====")
}

## ---- warning classification --------------------------------------------------
# Buckets the recurring, understood warning families so the tail of the log can be
# read in seconds. Anything that does not match lands in "UNCLASSIFIED -- READ IT",
# which is the only bucket that should ever require action.
classify_warning <- function(msg) {
  m <- tolower(msg)
  has <- function(p) grepl(p, m, fixed = FALSE)
  if (has("boundary \\(singular\\) fit|singular"))            "singular mixed-model fit"
  else if (has("failed to converge|convergence code|maxfun")) "mixed-model convergence"
  else if (has("nas introduced by coercion"))                 "NA coercion"
  else if (has("no non-missing arguments|argument is not numeric|-inf|returning inf"))
                                                                     "empty/degenerate input to a summary"
  else if (has("removed .* rows containing|missing values"))  "ggplot dropped missing rows"
  else if (has("attributes are not identical|vctrs|labelled")) "labelled/attribute mismatch on combine"
  else if (has("st_(centroid|point_on_surface|intersection)|assumes attributes|planar|longitude/latitude"))
                                                                     "sf planar-geometry assumption"
  else if (has("deprecat|superseded|renamed in"))             "deprecation"
  else if (has("collapsing to unique|duplicated|many-to-many")) "join/aggregation multiplicity"
  else if (has("weights|non-integer #successes|glm.fit"))     "glm weights / non-integer successes"
  else                                                               "UNCLASSIFIED -- READ IT"
}

## ---- write + summarise the captured warnings ---------------------------------
report_warnings <- function() {
  dir.create(file.path(DIR_OUT, "diagnostics"), showWarnings = FALSE, recursive = TRUE)
  path <- file.path(DIR_OUT, "diagnostics", "pipeline_warnings.csv")
  if (!length(WARN_LOG$rows)) {
    utils::write.csv(data.frame(step = character(), pop = character(),
                                message = character(), call = character(),
                                class = character()),
                     path, row.names = FALSE)
    message("\n==== WARNINGS: none raised by any step. ====")
    return(invisible(0L))
  }
  w <- do.call(rbind, WARN_LOG$rows)
  w$class <- vapply(w$message, classify_warning, character(1), USE.NAMES = FALSE)
  utils::write.csv(w, path, row.names = FALSE)
  message("\n==== WARNINGS: ", nrow(w), " raised across the run ====")
  message("  full list -> ", path)
  cat("\n-- by class --\n"); print(sort(table(w$class), decreasing = TRUE))
  cat("\n-- by step --\n");  print(sort(table(w$step),  decreasing = TRUE))
  unread <- w[w$class == "UNCLASSIFIED -- READ IT", , drop = FALSE]
  if (nrow(unread)) {
    cat("\n-- UNCLASSIFIED warnings (these are the ones to read) --\n")
    u <- unique(unread[, c("step", "message")])
    for (k in seq_len(nrow(u)))
      cat(sprintf("  [%s] %s\n", u$step[k], substr(u$message[k], 1, 220)))
  } else {
    message("  Every warning fell into a known, understood class; none unclassified.")
  }
  invisible(nrow(w))
}

## ---- resolve START_FROM ------------------------------------------------------
script_names <- vapply(steps, `[[`, character(1), "s")
start_i <- if (nzchar(START_FROM)) {
  hit <- which(script_names == START_FROM)[1]
  if (is.na(hit)) stop("START_FROM='", START_FROM,
                       "' is not a step. Valid: ", paste(unique(script_names), collapse = ", "))
  hit
} else 1L

## ---- fresh check trail on EVERY run ------------------------------------------
# The self-checks (checks.R) and the mixed-model registry both APPEND. If the
# previous run's rows are left in place, a resumed run silently mixes two runs'
# results in one file and 20_pipeline_checks.R consolidates the mixture -- the
# exact staleness trap this script exists to prevent, moved into the audit trail.
# So archive the old files and start clean every time, even on a START_FROM
# resume. The cost is that a resume's trail covers only the steps that actually
# ran; that is stated below, and is more honest than a blended file.
.diag <- file.path(DIR_OUT, "diagnostics")
suppressWarnings(dir.create(.diag, showWarnings = FALSE, recursive = TRUE))
for (.f in c("pipeline_checks.csv", "model_fits.csv", "pipeline_warnings.csv")) {
  .p <- file.path(.diag, .f)
  if (file.exists(.p)) {
    file.rename(.p, file.path(.diag, paste0("prev_", .f)))
  }
}
if (start_i > 1L)
  message("NOTE: resuming at ", START_FROM, " -- the check trail, model registry ",
          "and warning log will cover ONLY the steps from there onward. The ",
          "previous run's files are kept as diagnostics/prev_*.csv.")

t_all <- Sys.time()
message("Running ", length(steps) - start_i + 1L, " of ", length(steps),
        " steps (SKIP_STATIC=", SKIP_STATIC, ")")
for (i in seq(start_i, length(steps))) run_step(steps[[i]])
message("\nAll analysis steps complete in ",
        round(difftime(Sys.time(), t_all, units = "mins"), 1), " min. ",
        "All analytical outputs were regenerated from the same pipeline version.")
n_warn <- report_warnings()

## ---- rebuild the Word manuscript from the fresh figures ----------------------
# This step is REQUIRED, not best-effort, when BUILD_DOCX=TRUE. A silently skipped
# build is how a stale manuscript survives a "successful" pipeline run: the data
# regenerate, the docx does not, and the final message says DONE anyway. Every
# failure path below stops the script with the exact command needed to fix it.
DOCX_BUILT <- FALSE
if (BUILD_DOCX) {
  # The build lives in build_docx.R, sourced by both this runner and
  # build_docx_only.R, so a standalone document rebuild and a pipeline document
  # rebuild are literally the same code. Failures still stop() -- a silently
  # skipped build is how a stale manuscript survives a "successful" run.
  source(file.path(DIR_SCRIPTS, "build_docx.R"))
  build_manuscript_docx(
    DIR_OUT,
    resume_cmd = paste0("cd ", DIR_SCRIPTS, " && Rscript build_docx_only.R",
                        "   # rebuilds the docx only; no analysis re-run"))
  DOCX_BUILT <- TRUE
}

## ---- final status: say only what is actually true ----------------------------
message("\nDONE. Analysis outputs in ", DIR_OUT, ".")
if (BUILD_DOCX) {
  message("  Manuscript: ", if (DOCX_BUILT)
            "ACCESS_Health_main.docx and ACCESS_Health_SI.docx rebuilt from this run."
          else "NOT rebuilt.")
} else {
  message("  Manuscript: NOT rebuilt (BUILD_DOCX=FALSE). Any ACCESS_Health_*.docx ",
          "in ", DIR_OUT, " predates this run and may not match the numbers above.")
}
message("  Self-checks: read ", file.path(DIR_OUT, "diagnostics", "pipeline_checks.csv"),
        " -- checks are non-fatal, so a completed run is NOT a passed run.")
message("  Warnings: ", n_warn, " captured -> ",
        file.path(DIR_OUT, "diagnostics", "pipeline_warnings.csv"), ".")
