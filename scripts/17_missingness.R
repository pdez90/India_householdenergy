# ==============================================================================
# 17_missingness.R  (standalone; fast)
#
# Documents missing data at every stage of the analysis, for the SI:
#   A. Item (variable) missingness in each survey's analytic household frame --
#      the exposure (primary-LPG) and every harmonized covariate.
#   B. Cluster/household linkage exclusions (NFHS GPS + >10 km snap; ACCESS/IRES
#      census-code match) -- re-emitted from the *_linkage_diagnostics.csv files.
#   C. District analytic coverage -- how many districts carry an estimate at each
#      step (raw estimate -> corrected -> mortality overlap -> covariate-complete).
#
# Birth-level mortality "missingness" is by design (recent-cohort censoring; see
# H1_prep_mortality.R and Methods 2.4.5) and is summarized there, not here.
#
# Inputs : nfhs_hh_covariates.rds, access_hh.rds, ires_hh.rds,
#          nfhs_districts.rds, corrected_nfhs_districts.rds,
#          health_district_wide.rds, *_linkage_diagnostics.csv (optional)
# Outputs: missingness_items.csv, missingness_coverage.csv, missingness_linkage.csv
# ==============================================================================

source("00_config.R")
need_inputs(c("nfhs_hh_covariates.rds" = "01_prep_nfhs.R",
              "access_hh.rds"          = "02_prep_access.R",
              "ires_hh.rds"            = "03_prep_ires.R"))

nfhs_hh <- readRDS(file.path(dir_out, "nfhs_hh_covariates.rds"))
access  <- readRDS(file.path(dir_out, "access_hh.rds"))
ires    <- readRDS(file.path(dir_out, "ires_hh.rds"))

pick <- function(df, cands) { hit <- cands[cands %in% names(df)]; if (length(hit)) hit[1] else NA_character_ }

# roles -> candidate column names in each frame
miss_one <- function(df, survey, roles) {
  purrr::map_dfr(names(roles), function(role) {
    col <- pick(df, roles[[role]])
    if (is.na(col)) return(tibble(survey = survey, role = role, column = NA,
                                  n_total = nrow(df), n_missing = NA_integer_,
                                  pct_missing = NA_real_))
    x <- df[[col]]
    tibble(survey = survey, role = role, column = col,
           n_total = length(x), n_missing = sum(is.na(x)),
           pct_missing = round(100 * mean(is.na(x)), 2))
  })
}

# ---- A. item missingness -----------------------------------------------------
# Education was listed here until 2026-08-01, and its row reported 100% missing
# in both NFHS rounds against 0% in ACCESS and IRES. That row was the evidence
# for dropping education; having acted on it, the variable is gone from the
# pipeline (see the header note in 01_prep_nfhs.R) and there is nothing left to
# measure missingness on. The manuscript keeps the finding in the Table 1
# comparability row rather than in this table.
nfhs_roles <- list(
  `Primary-LPG exposure` = "lpg", Caste = "caste",
  Religion = c("hh_relig","religion"), `Household size` = c("hh_size","hhsize"),
  `BPL/ration card` = "bpl",
  Electricity = "electricity", `Wealth (index/expenditure)` = c("wealth_score","month_exp"))
access_roles <- list(
  `Primary-LPG exposure` = "main_fuel_lpg", Caste = "caste",
  Religion = c("religion","hh_relig"), `Household size` = c("hhsize","hh_size"),
  `BPL/ration card` = c("bplaay","bpl"),
  Electricity = "electricity", `Wealth (index/expenditure)` = c("month_exp","wealth_score"))
ires_roles <- access_roles

items <- dplyr::bind_rows(
  miss_one(dplyr::filter(nfhs_hh, survey == "NFHS4"), "NFHS-4", nfhs_roles),
  miss_one(dplyr::filter(nfhs_hh, survey == "NFHS5"), "NFHS-5", nfhs_roles),
  # NOTE: `access` is the POOLED two-wave panel frame (17,635 household-wave
  # observations), not Wave 1 alone (8,563). It was previously labelled
  # "ACCESS W1", which the manuscript then reported as a Wave 1 count. Label it
  # for the frame that is actually summarized.
  miss_one(access, "ACCESS (both waves)", access_roles),
  miss_one(ires,   "IRES",      ires_roles))
write_csv(items, file.path(dir_out, "missingness_items.csv"))
cat("\n== A. Item missingness in the analytic household frames (% missing) ==\n")
print(as.data.frame(items), row.names = FALSE)

# ---- B. linkage / exclusion cascade (re-emit the diagnostics files) ----------
link_files <- c(NFHS5 = "nfhs5_linkage_diagnostics.csv",
                ACCESS = "access_linkage_diagnostics.csv",
                IRES  = "ires_linkage_diagnostics.csv")
linkage <- purrr::imap_dfr(link_files, function(f, tag) {
  p <- file.path(dir_out, f)
  if (!file.exists(p)) return(NULL)
  x <- suppressMessages(readr::read_csv(p, show_col_types = FALSE))
  x$source <- tag; x
})
if (nrow(linkage)) {
  write_csv(linkage, file.path(dir_out, "missingness_linkage.csv"))
  cat("\n== B. Cluster/household linkage exclusions (from *_linkage_diagnostics.csv) ==\n")
  print(as.data.frame(linkage), row.names = FALSE)
} else message("No *_linkage_diagnostics.csv found; skipping section B.")

# ---- C. district analytic coverage -------------------------------------------
nd  <- readRDS(file.path(dir_out, "nfhs_districts.rds"))
cov_rows <- list(
  c("NFHS-4 districts with a raw multilevel estimate",
    sum(is.finite(nd[["lpg_2015_rural"]]))),
  c("NFHS-5 districts with a raw multilevel estimate",
    sum(is.finite(nd[["lpg_2019_rural"]]))))
if (file.exists(file.path(dir_out, "corrected_nfhs_districts.rds"))) {
  cr <- readRDS(file.path(dir_out, "corrected_nfhs_districts.rds"))
  cov_rows <- c(cov_rows, list(
    c("NFHS-4 districts with a corrected (Bayes) estimate", sum(is.finite(cr[["lpg_2015_bayes"]]))),
    c("NFHS-5 districts with a corrected (Bayes) estimate", sum(is.finite(cr[["lpg_2019_bayes"]])))))
}
if (file.exists(file.path(dir_out, "health_district_wide.rds"))) {
  hw <- readRDS(file.path(dir_out, "health_district_wide.rds"))
  cov_rows <- c(cov_rows, list(
    c("Rural districts with a neonatal-mortality estimate",
      if ("neonatal_death_2019" %in% names(hw)) sum(is.finite(hw[["neonatal_death_2019"]])) else NA),
    c("Rural districts with an infant-mortality estimate",
      if ("infant_death_2019" %in% names(hw)) sum(is.finite(hw[["infant_death_2019"]])) else NA)))
}
coverage <- tibble(quantity = vapply(cov_rows, `[`, character(1), 1),
                   n_districts = suppressWarnings(as.integer(vapply(cov_rows, `[`, character(1), 2))))
write_csv(coverage, file.path(dir_out, "missingness_coverage.csv"))
cat("\n== C. District analytic coverage ==\n")
print(as.data.frame(coverage), row.names = FALSE)

## ---- CHECKS ------------------------------------------------------------------
chk_header("17_missingness")
chk_file("17", "item missingness table written", "missingness_items.csv")
chk_file("17", "district coverage table written", "missingness_coverage.csv")
chk_warn("17", "exposure (primary-LPG) complete in all surveys",
    { e <- items$pct_missing[items$role == "Primary-LPG exposure"]
      length(e) > 0 && all(e[is.finite(e)] < 0.5) },
    { e <- items$pct_missing[items$role == "Primary-LPG exposure"]
      sprintf("max exposure missingness = %.2f%%", max(e, na.rm = TRUE)) })

message("\n17_missingness.R done -> missingness_items.csv, missingness_linkage.csv, ",
        "missingness_coverage.csv")
