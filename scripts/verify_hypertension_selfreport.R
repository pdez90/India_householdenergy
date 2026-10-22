# ==============================================================================
# verify_hypertension_selfreport.R  (read-only; writes one aggregate CSV)
#
# check_cvd_definitions.R could verify the measured-BP part of the stored
# hypertension flag (hypertension1_23) but not its self-reported part, because
# the "told high BP" and "taking BP medication" items are not stored in
# cvd_load.RData. This script reads those two items from the raw DHS recodes,
# links them person by person to cvd_load.RData, recomputes
#   hypertension = mean(BP readings 2-3) >= 140/90  OR  told high BP  OR  on medication
# under both missing-value rules found in the construction code, and compares
# the result with the stored flag. It answers, for each round:
#   - is every respondent flagged despite normal measured BP told/medicated?
#   - is no respondent who was told/medicated left unflagged?
#
# Items: NFHS-4 women sb18/sb19, men smb18/smb19; NFHS-5 women sb20/sb21,
# men smb20/smb21 (raw recode MAP files: "Told had high BP on two or more
# occasions", "Currently taking a prescribed medicine to lower BP").
#
# Output: OUTPUT_DIR/diagnostics/adult_hypertension_verification.csv (counts
# only; no person-level data).
# Run:
#   DHS_DIR=~/Downloads/DHS_India ACCESS_DIR=~/Downloads/ACCESS_energy \
#   OUTPUT_DIR=~/Downloads/ACCESS_energy/outputs Rscript verify_hypertension_selfreport.R
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(haven) })
if (!exists(".env_dir")) source("paths.R")
dir_dhs <- .dir_dhs()
dir_out <- Sys.getenv("OUTPUT_DIR", "")
if (!nzchar(dir_out)) stop("Set OUTPUT_DIR.", call. = FALSE)

first_existing <- function(p) { p <- p[file.exists(p)]; if (length(p)) p[1] else NA_character_ }
RAW <- list(
  `2015` = list(ir = first_existing(file.path(dir_dhs, "IAIR74DT/IAIR74FL.DTA")),
                mr = first_existing(file.path(dir_dhs, "IAMR74DT/IAMR74FL.DTA")),
                told = "18", med = "19"),
  `2019` = list(ir = first_existing(c(file.path(dir_dhs, "India2019/IAIR7DDT/IAIR7DFL.DTA"),
                                      file.path(dir_dhs, "India2019/IAIR7EDT/IAIR7EFL.DTA"),
                                      file.path(Sys.getenv("ACCESS_DIR"), "nfhs5_IR_7E/IAIR7EDT/IAIR7EFL.DTA"))),
                mr = first_existing(c(file.path(dir_dhs, "India2019/IAMR7DDT/IAMR7DFL.DTA"),
                                      file.path(dir_dhs, "India2019/IAMR7EDT/IAMR7EFL.DTA"))),
                told = "20", med = "21"))

num <- function(x) { x <- unclass(x); attributes(x) <- NULL
  if (is.factor(x) || is.character(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x)) }
chr <- function(x) trimws(as.character(unclass(x)))

# One id for this run, written into both output files; the manuscript builder
# accepts the verification only if the two files carry the same id. Any status
# file from an earlier run is overwritten with passed = FALSE first, so a run
# that crashes part-way can never leave an older "passed" status behind.
VERIFY_RUN_ID <- paste0("verify-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())
dir.create(file.path(dir_out, "diagnostics"), showWarnings = FALSE)
fs <- file.path(dir_out, "diagnostics", "adult_hypertension_verification_status.csv")
write.csv(data.frame(passed = FALSE, n_failures = NA, failures = "run started but not completed",
                     checks = "", run_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                     run_id = VERIFY_RUN_ID), fs, row.names = FALSE)

e <- new.env(); load(file.path(dir_dhs, "cvd_load.RData"), envir = e)
out <- list()
FAIL <- character(0)                      # every failed check, reported and fatal at the end
fail <- function(msg) { FAIL <<- c(FAIL, msg); cat("  [FAIL]", msg, "\n") }

for (yr in names(RAW)) {
  R <- RAW[[yr]]; df <- get(paste0("all_", yr), envir = e)
  cat("\n=====================", yr, "(", nrow(df), "rows ) =====================\n")
  cat("raw IR:", R$ir, "\nraw MR:", R$mr, "\n")
  if (is.na(R$ir) || is.na(R$mr)) { fail(paste0(yr, ": raw recode not found")); next }
  idc <- intersect(c("caseid", "mcaseid", "v001", "v002", "v003", "mv001", "mv002", "mv003", "kind"), names(df))
  cat("id columns present in cvd_load:", paste(idc, collapse = ", "), "\n")

  ir <- read_dta(R$ir, col_select = any_of(c("caseid", paste0("sb", R$told), paste0("sb", R$med))))
  mr <- read_dta(R$mr, col_select = any_of(c("mcaseid", paste0("smb", R$told), paste0("smb", R$med))))
  raw <- bind_rows(
    tibble(key = chr(ir$caseid),  told = num(ir[[paste0("sb", R$told)]]),  med = num(ir[[paste0("sb", R$med)]]),  kind = "women"),
    tibble(key = chr(mr$mcaseid), told = num(mr[[paste0("smb", R$told)]]), med = num(mr[[paste0("smb", R$med)]]), kind = "men"))
  rm(ir, mr); invisible(gc())

  # person key in cvd_load: caseid for women; mcaseid for men if present, else caseid
  key <- if ("caseid" %in% names(df)) chr(df$caseid) else rep(NA_character_, nrow(df))
  if ("mcaseid" %in% names(df)) { mk <- chr(df$mcaseid); key[is.na(key) | key == ""] <- mk[is.na(key) | key == ""] }
  kind <- if ("kind" %in% names(df)) as.character(df$kind) else rep(NA_character_, nrow(df))
  rk <- paste(raw$kind, raw$key); ck <- paste(kind, key)
  if (anyDuplicated(rk)) fail(sprintf("%s: %d duplicated person keys in the raw recodes", yr, sum(duplicated(rk))))
  if (anyDuplicated(ck)) fail(sprintf("%s: %d duplicated person keys in cvd_load", yr, sum(duplicated(ck))))
  m <- match(ck, rk)
  cat(sprintf("linked to raw recode: %d of %d (women %d/%d, men %d/%d)\n", sum(!is.na(m)), nrow(df),
              sum(!is.na(m) & kind == "women"), sum(kind == "women", na.rm = TRUE),
              sum(!is.na(m) & kind == "men"), sum(kind == "men", na.rm = TRUE)))
  if (any(is.na(m))) fail(sprintf("%s: %d of %d rows not linked to the raw recodes", yr, sum(is.na(m)), nrow(df)))
  if (sum(!is.na(m)) == 0) next

  told <- raw$told[m]; med <- raw$med[m]
  told[told > 1] <- NA; med[med > 1] <- NA          # 8 = don't know -> missing
  s <- num(df$systolic23); d <- num(df$diastolic23); st <- num(df$hypertension1_23)
  bp <- ifelse((s >= 140 | d >= 90), 1, 0)           # R semantics: NA | TRUE = TRUE
  sr <- as.integer(told %in% 1 | med %in% 1)          # told or medicated (missing counts as no)
  h_or <- ifelse(bp == 1 | told == 1 | med == 1, 1, 0)
  h_coal <- dplyr::coalesce(dplyr::coalesce(h_or, told), med)
  h_strict <- ifelse(is.na(s) | is.na(d) | is.na(told) | is.na(med), NA, as.integer(bp %in% 1 | told %in% 1 | med %in% 1))

  cmp <- function(re, lab) {
    ok <- !is.na(re) & !is.na(st)
    cat(sprintf("\n  %s: agreement %.5f on %d rows; NA mismatch %d\n", lab,
                mean(re[ok] == st[ok]), sum(ok), sum(is.na(re) != is.na(st))))
    print(table(recomputed = re, stored = st, useNA = "ifany"))
    data.frame(year = yr, rule = lab, n_both = sum(ok), agree = sum(re[ok] == st[ok]),
               disagree = sum(re[ok] != st[ok]), na_mismatch = sum(is.na(re) != is.na(st)))
  }
  r1 <- cmp(h_coal, "BP or told or medicated (coalesce rule)")
  r2 <- cmp(h_strict, "BP or told or medicated (NA if any component missing)")

  bp0_flag <- which(bp %in% 0 & st %in% 1)            # flagged despite normal measured BP
  bp0_noflag <- which(bp %in% 0 & st %in% 0)
  cat(sprintf("\n  flagged despite normal measured BP: %d; of these told or medicated: %d (%.2f%%)\n",
              length(bp0_flag), sum(sr[bp0_flag]), 100 * mean(sr[bp0_flag])))
  cat(sprintf("  normal BP and NOT flagged: %d; of these told or medicated: %d (should be 0)\n",
              length(bp0_noflag), sum(sr[bp0_noflag])))
  res_yr <- rbind(r1, r2,
    data.frame(year = yr, rule = "flagged with normal BP: told or medicated", n_both = length(bp0_flag),
               agree = sum(sr[bp0_flag]), disagree = length(bp0_flag) - sum(sr[bp0_flag]), na_mismatch = NA),
    data.frame(year = yr, rule = "unflagged with normal BP: told or medicated", n_both = length(bp0_noflag),
               agree = length(bp0_noflag) - sum(sr[bp0_noflag]), disagree = sum(sr[bp0_noflag]), na_mismatch = NA))
  # Frame size and stored-flag missingness, so the manuscript can report how many
  # respondents the complete-component rule excludes in each round.
  res_yr$n_rows <- nrow(df)
  res_yr$n_stored_missing <- sum(is.na(st))
  cat(sprintf("\n  stored hypertension missing: %d of %d (%.1f%%)\n", sum(is.na(st)), nrow(df), 100 * mean(is.na(st))))
  out[[yr]] <- res_yr
  if (r2$disagree != 0) fail(sprintf("%s: %d classification disagreements under the strict rule", yr, r2$disagree))
  if (r2$na_mismatch != 0) fail(sprintf("%s: %d missing-value mismatches under the strict rule", yr, r2$na_mismatch))
  if (length(bp0_flag) - sum(sr[bp0_flag]) != 0) fail(sprintf("%s: %d flagged with normal BP but neither told nor medicated", yr, length(bp0_flag) - sum(sr[bp0_flag])))
  if (sum(sr[bp0_noflag]) != 0) fail(sprintf("%s: %d told or medicated with normal BP but not flagged", yr, sum(sr[bp0_noflag])))
}
if (length(out)) {
  dir.create(file.path(dir_out, "diagnostics"), showWarnings = FALSE)
  f <- file.path(dir_out, "diagnostics", "adult_hypertension_verification.csv")
  write.csv(transform(do.call(rbind, out), run_id = VERIFY_RUN_ID), f, row.names = FALSE)
  cat("\nwrote", f, "\n")
}
for (yr in names(RAW)) if (is.null(out[[yr]])) fail(paste0(yr, ": no verification result"))
# Overall status, written whether the run passed or failed, BEFORE exiting. The
# manuscript builder drops its "provisional" wording only if this file says
# passed = TRUE (a failed run still leaves the per-rule CSV above behind).
write.csv(data.frame(passed = length(FAIL) == 0, n_failures = length(FAIL),
                     failures = paste(FAIL, collapse = " | "),
                     checks = "linkage complete; unique keys; zero strict-rule classification disagreements; zero missing-value mismatches; normal-BP flags all told/medicated; no unflagged told/medicated",
                     run_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                     run_id = VERIFY_RUN_ID),
          fs, row.names = FALSE)
cat("wrote", fs, "\n")
if (length(FAIL)) {
  cat("\nVERIFICATION FAILED (", length(FAIL), " check(s)):\n  - ", paste(FAIL, collapse = "\n  - "), "\n", sep = "")
  quit(status = 1)
}
cat("\nVERIFICATION PASSED: complete linkage, unique keys, zero disagreements, zero missing-value mismatches in both rounds.\n")
cat("\nDone. Paste everything above back.\n")
