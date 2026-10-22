# ==============================================================================
# check_cvd_definitions.R  (read-only; writes nothing)
#
# H4's adult outcomes are flags stored in cvd_load.RData, which this repository
# does not build. Their construction was traced (2026-10-21) to code outside the
# repo, in the DHS_India folder:
#   cvd_load.RData        <- DHS_India/India_CVD.Rmd (save.image, line ~355):
#                            rbind of women + men person files, then
#                            timelived >= 5 & != 96, and currently_pregnant == 0
#   NFHS-5 person files   <- DHS_India/India2019.Rmd (women ~1670-1790,
#                            men ~1930-2055)
#   NFHS-4 person files   <- generating script NOT recovered; the same logic
#                            appears in DHS_India/india_DHS_precisionwt.Rmd
#                            (women ~495-600, men ~710-800), which writes to a
#                            different folder (New2015/), so it is not proven to
#                            be the code that built the files actually loaded.
# Definitions in that code:
#   hypertension1_23 = mean of BP readings 2 and 3: systolic >= 140 OR
#                      diastolic >= 90, OR told high BP (2015: sb18/smb18;
#                      2019: sb20/smb20), OR taking BP medicine (2015:
#                      sb19/smb19; 2019: sb21/smb21)
#   diabetes         = self-report (s723a/sm622a; s728a/sm627a) OR glucose
#                      > 126 mg/dL if > 8 h since food/drink, > 200 otherwise
# Missing-value rules differ between code versions (coalesce vs NA-if-any-
# component-missing), so this script recomputes BOTH and compares each with the
# stored flag. It also reports the age range by sex and the residence and
# pregnancy restrictions actually present in the file.
#
# Run:  Rscript check_cvd_definitions.R      (paste the whole output back)
# ==============================================================================
suppressPackageStartupMessages(library(dplyr))
if (!exists(".env_dir")) source("paths.R")
path_cvd_load <- file.path(.dir_dhs(), "cvd_load.RData")
e <- new.env(); load(path_cvd_load, envir = e)
cat("Objects in cvd_load.RData:", paste(ls(e), collapse = ", "), "\n")

# Columns arrive as haven_labelled (Stata value labels); drop the labels before
# converting, since vctrs refuses as.numeric() on a labelled vector.
num <- function(x) { x <- unclass(x); attributes(x) <- NULL
  if (is.factor(x) || is.character(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x)) }
col <- function(df, v) if (v %in% names(df)) num(df[[v]]) else rep(NA_real_, nrow(df))
first <- function(df, vs) { x <- rep(NA_real_, nrow(df)); for (v in vs) x <- dplyr::coalesce(x, col(df, v)); x }
agree <- function(re, st, lab) {
  cat("\n  ", lab, "\n", sep = "")
  print(table(recomputed = re, stored = st, useNA = "ifany"))
  ok <- !is.na(re) & !is.na(st)
  cat(sprintf("  agreement where both non-missing: %.5f (n = %d); NA mismatch: %d\n",
              mean(re[ok] == st[ok]), sum(ok), sum(is.na(re) != is.na(st))))
}

SPEC <- list(all_2015 = list(diag = c("sb18", "smb18"), med = c("sb19", "smb19")),
             all_2019 = list(diag = c("sb20", "smb20"), med = c("sb21", "smb21")))

for (nm in names(SPEC)) {
  if (!exists(nm, envir = e)) { cat("\n", nm, "not found\n"); next }
  df <- get(nm, envir = e); sp <- SPEC[[nm]]
  cat("\n=====================", nm, "(", nrow(df), "rows ) =====================\n")

  cat("\nSample restrictions present in the file:\n")
  if ("kind" %in% names(df)) {
    for (k in unique(df$kind)) {
      a <- col(df[df$kind == k, ], "age")
      cat(sprintf("  %-6s n=%-8d age %s-%s\n", k, sum(df$kind == k),
                  suppressWarnings(min(a, na.rm = TRUE)), suppressWarnings(max(a, na.rm = TRUE))))
    }
  }
  tl <- col(df, "timelived")
  cat(sprintf("  timelived: min %s, any 96: %s, any < 5: %s\n",
              suppressWarnings(min(tl, na.rm = TRUE)), any(tl == 96, na.rm = TRUE), any(tl < 5, na.rm = TRUE)))
  cat("  currently_pregnant values:", paste(sort(unique(col(df, "currently_pregnant"))), collapse = ","), "\n")
  cat("  components present:", paste(intersect(c("systolic23", "diastolic23", sp$diag, sp$med,
      "diabetes_report", "glucoselevel", "timesincelast", "diabetestest",
      "hypertension1_23", "diabetes"), names(df)), collapse = ", "), "\n")

  # ---- hypertension1_23 ----
  s <- col(df, "systolic23"); d <- col(df, "diastolic23")
  dg <- first(df, sp$diag); md <- first(df, sp$med)
  bp <- as.integer(s >= 140 | d >= 90)
  strict <- ifelse(is.na(s) | is.na(d) | is.na(dg) | is.na(md), NA,
                   as.integer(bp %in% 1 | dg %in% 1 | md %in% 1))
  loose <- as.integer(bp == 1 | dg == 1 | md == 1)          # ifelse(a|b|c) semantics
  loose <- dplyr::coalesce(dplyr::coalesce(loose, dg), md)
  st <- col(df, "hypertension1_23")
  agree(strict, st, "hypertension1_23 vs recomputed (NA if any component missing; India2019.Rmd rule)")
  agree(loose,  st, "hypertension1_23 vs recomputed (coalesce rule; india_*_precisionwt.Rmd)")
  agree(bp, st, "hypertension1_23 vs measured BP only (readings 2-3, 140/90)")

  # ---- diabetes composite ----
  g <- col(df, "glucoselevel"); g[g > 990] <- NA
  ts <- col(df, "timesincelast"); rep_ <- col(df, "diabetes_report")
  test <- ifelse(ts > 8, as.integer(g > 126), as.integer(g > 200))
  dz <- as.integer(rep_ == 1 | test == 1)
  dz2 <- ifelse((rep_ %in% 0 & is.na(test)) | (is.na(rep_) & test %in% 0), 0L, dz)
  sd <- col(df, "diabetes")
  agree(dz,  sd, "diabetes vs recomputed (no NA override)")
  agree(dz2, sd, "diabetes vs recomputed (NA override to 0 when the other component is 0)")
  agree(as.integer(rep_ == 1), col(df, "diabetes_report"), "diabetes_report vs self-report item")
}
cat("\nDone. Paste everything above back.\n")
