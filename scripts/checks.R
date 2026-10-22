# ==============================================================================
# checks.R  --  lightweight, NON-FATAL pipeline self-checks.
#
# source("checks.R") from any script (00_config.R already does; the standalone
# H-scripts source it themselves). Each check prints a [PASS]/[WARN]/[FAIL] line
# AND appends a row to <dir_out>/diagnostics/pipeline_checks.csv, so a full run
# leaves an auditable trail that every stage produced what the next stage needs.
#
# Design rules:
#   * Checks NEVER stop the pipeline. A failing check prints FAIL and moves on;
#     the condition is evaluated inside tryCatch so a broken check can't error a
#     long run. 20_pipeline_checks.R consolidates and flags failures at the end.
#   * dir_out is resolved from the CALLER's environment (parent.frame), so this
#     works whether a script runs standalone or is source()'d by run_everything.
# ==============================================================================

.chk_logfile <- function(env) {
  d <- tryCatch(get0("dir_out", envir = env, inherits = TRUE),
                error = function(e) NULL)
  if (is.null(d) || !is.character(d) || !nzchar(d)) d <- "."
  dd <- file.path(d, "diagnostics")
  suppressWarnings(dir.create(dd, showWarnings = FALSE, recursive = TRUE))
  file.path(dd, "pipeline_checks.csv")
}

# A full run_everything.R run truncates pipeline_checks.csv at the start, so the
# trail describes exactly that run. Re-running ONE script on its own does not:
# it appends, leaving the superseded rows in place beside the new ones, and
# 20_pipeline_checks.R then tallies a failure that has already been fixed (this
# is what reported 22_design_analysis.R's anchor check as FAIL at 0.893 next to
# the PASS at 1.040 that replaced it).
#
# Clear a script's OWN previous rows the first time it emits in this R session,
# and only then. The two properties that matters for are:
#   * a standalone re-run supersedes its earlier rows, and
#   * a script legitimately invoked several times WITHIN one run keeps all of
#     them. H2_health_models.R runs three times (rural, all, rural/_nonoverlap)
#     and H4 twice, emitting the same check labels each time with genuinely
#     different results, so deduplicating on (script, check) at read time would
#     silently drop the primary population's status in favour of the last SI
#     variant's. The session flag below is what keeps those.
# The flag lives in an option so it survives checks.R being source()d again by
# each step of run_everything.R, which all share one R session.
# ---- run identity -------------------------------------------------------------
# Which rows in the trail belong to THIS run? The first version of this keyed
# staleness on the R session, which is correct only when a script runs once per
# session. H2 runs three times (rural, all, non-overlapping cohort) and H4
# twice, and run_everything.R does that inside ONE session, so session-keying
# worked there. A targeted re-run script invokes each variant as a SEPARATE
# Rscript, and each new session then cleared the previous variant's rows: the
# trail silently kept only the last variant, and on 2026-09-16 that erased a
# genuine FAIL from the rural pass before 20_pipeline_checks.R consolidated.
# The summary said ALL CHECKS PASSED over a trail one of whose rows had failed.
#
# The key is therefore the RUN, not the session. A runner exports CHK_RUN_ID
# once and every Rscript it launches shares it, so repeated invocations of one
# script accumulate while a genuinely older run's rows are cleared. With no
# CHK_RUN_ID set, each session gets its own id, which reproduces the old
# behaviour for a single ad-hoc script run.
.CHK_RUN_ID <- local({
  v <- Sys.getenv("CHK_RUN_ID", "")
  if (nzchar(v)) v
  else paste0("session-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
              paste(sample(c(letters, 0:9), 6, TRUE), collapse = ""))
})

.chk_clear_stale <- function(script, f) {
  seen <- getOption("chk_cleared", character())
  if (script %in% seen) return(invisible())
  options(chk_cleared = c(seen, script))
  if (!file.exists(f)) return(invisible())
  old <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(old) || !"script" %in% names(old)) return(invisible())
  # A trail written before run ids existed has no run_id column. Two things
  # follow, and BOTH matter: every such row belongs to an older run, and the
  # file's schema is one column short of what .chk_emit is about to append.
  # Appending a 5-column row to a 4-column CSV corrupts the trail, so the
  # migration happens here -- unconditionally, before any append -- rather than
  # only on the path where rows are also dropped.
  migrated <- FALSE
  if (!"run_id" %in% names(old)) { old$run_id <- NA_character_; migrated <- TRUE }
  drop <- old$script == script &
          (is.na(old$run_id) | old$run_id != .CHK_RUN_ID)
  if (!any(drop) && !migrated) return(invisible())
  keep <- old[!drop, , drop = FALSE]
  tryCatch({
    utils::write.csv(keep, f, row.names = FALSE)
    if (any(drop))
      message(sprintf("[checks] dropped %d superseded row(s) for %s from %s (older run)",
                      sum(drop), script, basename(f)))
    if (migrated)
      message(sprintf("[checks] added the run_id column to %s (rows from before run ids are marked NA)",
                      basename(f)))
  }, error = function(e) invisible())
}

.chk_emit <- function(script, label, ok, detail, warn_only) {
  ok     <- tryCatch(isTRUE(ok), error = function(e) FALSE)
  status <- if (ok) "PASS" else if (warn_only) "WARN" else "FAIL"
  cat(sprintf("  [%s] %-52s %s\n", status, label, detail))
  tryCatch({
    f   <- .chk_logfile(parent.frame(2))
    .chk_clear_stale(script, f)
    new <- !file.exists(f)
    row <- data.frame(script = script, check = label, status = status,
                      detail = as.character(detail), run_id = .CHK_RUN_ID,
                      stringsAsFactors = FALSE)
    suppressWarnings(write.table(row, f, sep = ",", append = !new,
                                 col.names = new, row.names = FALSE))
  }, error = function(e) invisible())
  invisible(ok)
}

# Hard check (FAIL if not ok) and soft check (WARN if not ok).
chk      <- function(script, label, ok, detail = "") .chk_emit(script, label, ok, detail, FALSE)
chk_warn <- function(script, label, ok, detail = "") .chk_emit(script, label, ok, detail, TRUE)

chk_header <- function(script) cat(sprintf("\n== CHECKS [%s] ==\n", script))

# ---- Safe predicates (never error; return a plain logical) -------------------
chk_has_cols <- function(df, cols)
  is.data.frame(df) && all(cols %in% names(df))

# `tol` absorbs floating-point dust, NOT substantive slack. A design-weighted
# proportion in a saturated stratum is a ratio of weight sums, so a district
# where every sampled household reports the outcome lands at 1 + a few ULP
# rather than exactly 1 (IRES rural Rangareddy, n = 17, comes back as
# 1.0000000000000002). Rejecting that is a bug in the check, not a finding.
# 1e-8 is ~1e7 ULP at unit scale -- far above any accumulation of rounding
# error, far below any difference a check here is meant to catch. Do not widen
# it to make a substantively out-of-range value pass; fix the value instead.
chk_in_range <- function(x, lo, hi, tol = 1e-8) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  length(x) > 0 && all(x >= lo - tol & x <= hi + tol)
}

chk_pct_na <- function(x) round(100 * mean(is.na(x)), 2)

# Pretty numeric range string for the `detail` column.
chk_rng <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (!length(x)) "no finite values"
  else sprintf("range [%.3g, %.3g], n=%d", min(x), max(x), length(x))
}

# ---- Mixed-model fit registry ------------------------------------------------
# Every glmer/lmer fit in the pipeline is recorded here, whether or not it is
# problematic. WHY: lme4 reports "boundary (singular) fit" as a warning, and the
# small-area models in this pipeline fit hundreds of them across scripts. A
# warning stream that long is not read, so a singular fit -- a variance component
# estimated at exactly zero, meaning the partial pooling that justifies the
# multilevel estimator has collapsed to complete pooling for that grouping factor
# -- disappears into the noise. Recording every fit turns that into a countable
# rate you can report ("k of n district models were singular") instead of a
# warning you hoped was benign.
#
# A singular fit is NOT necessarily wrong: with binary outcomes and small
# clusters, a zero between-group variance is a legitimate (if boundary) MLE. It
# does mean the district random effect contributed nothing for that fit, so the
# estimate reverts to the pooled mean and its apparent precision is borrowed
# rather than earned. Report the rate; do not silently drop those districts.
#
# Writes <dir_out>/diagnostics/model_fits.csv (appended across the run).
#
# WHICH SCRIPT AM I? `district_estimates_glmer()` lives in 00_config.R but is
# called from 01/02/03/08/09, so the fit must be attributed to the CALLING
# script, not to the file the helper is defined in. Each analysis script sets
# `CHK_SCRIPT <- "<tag>"` near its top; .chk_tag() reads that global (walking up
# the search path, so it works whether the script is run standalone or source()'d
# by run_everything.R) and falls back to "unknown" if it was never set.
#
# run_everything.R source()s each step into its OWN environment (local = e), so
# CHK_SCRIPT is NOT in globalenv during a full run -- it is in that step's env.
# Standalone (`Rscript 13_benchmark_maps.R`) it IS in globalenv. Walk the active
# call frames innermost-first with inherits = TRUE, which reaches the step
# environment through the closure chain in the first case and globalenv in the
# second, then fall back to globalenv and finally to `default`.
## NOTE ON THE TWO TAG CONVENTIONS
##   model_fits.csv is keyed by the long script name ("06_stacking_prediction");
##   pipeline_checks.csv is keyed by the short one ("06"). CHK_SCRIPT carries the
##   long form for chk_record_fit(), so a chk() that reaches for .chk_tag()
##   silently files itself under a second tag and the per-script tally gains a
##   phantom script. chk() calls pass their short tag literally.
.chk_tag <- function(default = "unknown") {
  ok <- function(v) !is.null(v) && is.character(v) && length(v) && nzchar(v[1])
  frames <- sys.frames()
  if (length(frames)) for (i in rev(seq_along(frames))) {
    v <- tryCatch(get0("CHK_SCRIPT", envir = frames[[i]], inherits = TRUE),
                  error = function(e) NULL)
    if (ok(v)) return(as.character(v)[1])
  }
  v <- tryCatch(get0("CHK_SCRIPT", envir = globalenv(), inherits = TRUE),
                error = function(e) NULL)
  if (ok(v)) as.character(v)[1] else default
}

chk_record_fit <- function(script, label, m, extra = "") {
  # Resolve dir_out from the CALLER now, while parent.frame() is unambiguous --
  # not from inside a nested tryCatch handler, where the frame depth varies.
  .d <- tryCatch(get0("dir_out", envir = parent.frame(), inherits = TRUE),
                 error = function(e) NULL)
  row <- tryCatch({
    sing <- tryCatch(isTRUE(lme4::isSingular(m)), error = function(e) NA)
    # lme4 stashes optimizer convergence messages in @optinfo$conv$lme4$messages
    msg  <- tryCatch({
      mm <- m@optinfo$conv$lme4$messages
      if (is.null(mm) || !length(mm)) "" else paste(mm, collapse = "; ")
    }, error = function(e) "")
    ngrp <- tryCatch(paste(names(m@flist), vapply(m@flist, nlevels, integer(1)),
                           sep = "=", collapse = "; "),
                     error = function(e) NA_character_)
    vcdf <- tryCatch(as.data.frame(lme4::VarCorr(m)), error = function(e) NULL)
    vc <- tryCatch(paste(sprintf("%s:%.4g", vcdf$grp, vcdf$vcov), collapse = "; "),
                   error = function(e) NA_character_)
    # WHICH component collapsed, and how many groups did it have?
    #
    # `singular = TRUE` on its own is not actionable: it says a variance was
    # estimated at the boundary but not WHICH one, and the answer changes what
    # you should do. In this pipeline the district random effect IS the estimand
    # -- these are district-level small-area estimates and the district BLUPs are
    # the output -- so a collapsed DISTRICT variance would be a real problem
    # (complete pooling; every district reverts to the pooled mean). A collapsed
    # STATE variance is a different and much milder thing, and is what actually
    # occurs here: ACCESS covers six states, and six groups cannot distinguish a
    # small between-state variance from a zero one, so the MLE lands on the
    # boundary. The fit is then effectively (1|village) + (1|district) with a
    # common intercept, and the district estimates are shrunk toward the grand
    # mean rather than a state mean -- arguably the more honest target at k = 6.
    #
    # Recording the boundary TERM and its group count puts that distinction in
    # the diagnostic file instead of leaving a reader to reconstruct it from the
    # variance string. Random slopes (var2 non-NA) and the residual row are
    # excluded: neither is a grouping-factor intercept variance, and including
    # them would mislabel which factor collapsed.
    bt <- tryCatch({
      if (is.null(vcdf)) NA_character_ else {
        v <- vcdf[is.na(vcdf$var2) & vcdf$grp != "Residual", , drop = FALSE]
        z <- as.character(v$grp[is.finite(v$vcov) & v$vcov < 1e-8])
        if (!length(z)) "" else paste(z, collapse = "; ")
      }
    }, error = function(e) NA_character_)
    btn <- tryCatch({
      if (is.na(bt) || !nzchar(bt)) "" else {
        k <- vapply(m@flist, nlevels, integer(1))
        z <- strsplit(bt, "; ", fixed = TRUE)[[1]]
        paste(sprintf("%s=%s", z, ifelse(z %in% names(k), k[z], NA_integer_)),
              collapse = "; ")
      }
    }, error = function(e) NA_character_)
    # ACTION states what the pipeline DID, not what it could have done. Nothing
    # is auto-dropped: a fallback that removed a term would silently change the
    # estimand between runs depending on which fits happened to hit the boundary,
    # which is worse than a documented boundary fit. If this column ever needs to
    # say something other than "retained as fitted", that will be because a
    # deliberate, pre-specified change was made -- not because a helper guessed.
    act <- if (!isTRUE(sing)) "" else
      if (is.na(bt) || !nzchar(bt))
        "SINGULAR but no zero intercept variance identified -- inspect this fit manually"
      else if (any(grepl("district", strsplit(bt, "; ", fixed = TRUE)[[1]], fixed = TRUE)))
        paste0("SINGULAR on ", bt, " -- the DISTRICT variance collapsed. This is ",
               "the estimand of the small-area model: the district estimates are ",
               "completely pooled and carry no district-specific information. ",
               "Retained as fitted, but do NOT report these as district estimates ",
               "without saying so.")
      else
        paste0("SINGULAR on ", bt, " (groups: ", btn, "). Retained as fitted; no ",
               "term dropped. The collapsed component is not the district effect, ",
               "so the district estimates remain informative -- they are shrunk ",
               "toward the grand mean rather than a ", bt, " mean.")
    data.frame(script = script, model = label,
               n_obs = tryCatch(stats::nobs(m), error = function(e) NA_integer_),
               groups = ngrp, singular = sing, variances = vc,
               boundary_terms = bt, boundary_n_groups = btn, action = act,
               conv_message = msg, extra = as.character(extra),
               stringsAsFactors = FALSE)
  }, error = function(e)
    data.frame(script = script, model = label, n_obs = NA_integer_,
               groups = NA_character_, singular = NA, variances = NA_character_,
               boundary_terms = NA_character_, boundary_n_groups = NA_character_,
               action = "record failed",
               conv_message = paste("record failed:", conditionMessage(e)),
               extra = as.character(extra), stringsAsFactors = FALSE))
  tryCatch({
    d  <- .d
    if (is.null(d) || !is.character(d) || !nzchar(d)) d <- "."
    dd <- file.path(d, "diagnostics")
    suppressWarnings(dir.create(dd, showWarnings = FALSE, recursive = TRUE))
    f   <- file.path(dd, "model_fits.csv")
    new <- !file.exists(f)
    suppressWarnings(write.table(row, f, sep = ",", append = !new,
                                 col.names = new, row.names = FALSE))
  }, error = function(e) invisible())
  invisible(isTRUE(row$singular))
}

# Summarise the fits recorded so far by THIS script, and emit a WARN if any were
# singular. Call once in a script's CHECKS block, after its models have been fit.
chk_singular_summary <- function(script, script_filter = script) {
  d <- tryCatch(get0("dir_out", envir = parent.frame(), inherits = TRUE),
                error = function(e) ".")
  if (is.null(d) || !is.character(d) || !nzchar(d)) d <- "."
  f <- file.path(d, "diagnostics", "model_fits.csv")
  if (!file.exists(f)) {
    chk_warn(script, "mixed-model fit registry written", FALSE,
             "model_fits.csv absent -- no fits recorded")
    return(invisible(NULL))
  }
  fits <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                   error = function(e) NULL)
  if (is.null(fits) || !nrow(fits)) return(invisible(NULL))
  mine <- fits[fits$script == script_filter, , drop = FALSE]
  if (!nrow(mine)) return(invisible(NULL))
  # `singular` round-trips through CSV as the string "TRUE"/"FALSE", so accept both.
  # NA means the flag could not be read off the fit object at all -- that is not
  # evidence of non-singularity, so count and report it separately.
  ns <- sum(mine$singular %in% c(TRUE, "TRUE"))
  nu <- sum(is.na(mine$singular) | mine$singular %in% c("NA", ""))
  # Name the singular fits and the component that collapsed in the WARN detail.
  # A bare rate ("2 of 5 fits singular") tells a reader that something is wrong
  # but not whether it matters, and the answer depends entirely on WHICH term hit
  # the boundary -- so put the model names and the boundary terms in the line
  # itself rather than in a file the reader may not open.
  .bt <- if ("boundary_terms" %in% names(mine)) as.character(mine$boundary_terms)
         else rep(NA_character_, nrow(mine))
  .bt[is.na(.bt)] <- "?"
  .si <- which(mine$singular %in% c(TRUE, "TRUE"))
  chk_warn(script, "no singular (boundary) mixed-model fits", ns == 0 && nu == 0,
           sprintf("%d of %d fits singular (%.1f%%)%s%s; see diagnostics/model_fits.csv",
                   ns, nrow(mine), 100 * ns / nrow(mine),
                   if (nu > 0) sprintf(", %d undetermined", nu) else "",
                   if (length(.si))
                     paste0(" -- ", paste(sprintf("%s [zero: %s]",
                                                  mine$model[.si], .bt[.si]),
                                          collapse = " | "))
                   else ""))
  # A collapsed DISTRICT variance is categorically worse than a collapsed state
  # variance in this pipeline, because the district effect is what the small-area
  # models exist to estimate. Separate the two so the serious case cannot hide
  # inside an aggregate singularity rate that a reader has learned to tolerate.
  .dist <- length(.si) && any(grepl("district", .bt[.si], fixed = TRUE))
  # This check must not PASS on absence of evidence. Two situations leave it
  # genuinely unable to answer: a fit whose singularity status was never recorded
  # (nu > 0), and a singular fit whose boundary term could not be identified
  # (boundary_terms is NA/"?", which also covers reading an old-schema
  # model_fits.csv that has no boundary_terms column at all). In both cases a
  # collapsed district variance could be sitting right there unseen, so the
  # honest verdict is WARN-undetermined, not PASS.
  .unkbt <- if (length(.si)) sum(.bt[.si] == "?") else 0L
  .unk <- .unkbt > 0 || nu > 0
  chk_warn(script, "no DISTRICT variance component collapsed to zero",
           !.dist && !.unk,
           if (.dist)
             paste0("COMPLETE POOLING across districts in: ",
                    paste(mine$model[.si][grepl("district", .bt[.si], fixed = TRUE)],
                          collapse = ", "),
                    " -- these are not district-specific estimates.")
           else if (.unk)
             paste0("UNDETERMINED -- ",
                    if (nu > 0)
                      sprintf("%d fit(s) have no recorded singularity status; ", nu)
                    else "",
                    if (.unkbt > 0)
                      sprintf("%d singular fit(s) have no identified boundary term; ",
                              .unkbt)
                    else "",
                    "cannot confirm the district variance is non-zero in every fit. ",
                    "Inspect diagnostics/model_fits.csv by hand before treating any ",
                    "of these as district-specific estimates.")
           else if (length(.si))
             paste0("district variance non-zero in every fit; the ", length(.si),
                    " boundary fit(s) collapsed on other factors only (",
                    paste(unique(.bt[.si]), collapse = ", "), ")")
           else "no boundary fits")
  # read.csv collapses an all-empty character column to logical NA, and
  # nzchar(NA) is TRUE -- so coerce to character and blank the NAs first,
  # otherwise "no messages at all" reads as "every fit reported a message".
  cm <- as.character(mine$conv_message); cm[is.na(cm)] <- ""
  nc <- sum(nzchar(cm))
  chk_warn(script, "no optimizer convergence messages", nc == 0,
           sprintf("%d of %d fits reported a convergence message", nc, nrow(mine)))
  invisible(mine)
}

# File-exists check relative to dir_out (resolved from the caller).
chk_file <- function(script, label, relpath) {
  d <- tryCatch(get0("dir_out", envir = parent.frame(), inherits = TRUE),
                error = function(e) ".")
  p <- file.path(d, relpath)
  chk(script, label, file.exists(p),
      if (file.exists(p)) paste0(relpath, " (", file.info(p)$size, " B)")
      else paste0("MISSING: ", relpath))
}
