# ==============================================================================
# 01_prep_nfhs.R
# Prepare NFHS-4 (2015-16) and NFHS-5 (2019-21) household data and produce
# district-level LPG-as-main-fuel estimates:
#   (a) all clusters,  (b) rural clusters only  (for comparison with ACCESS/IRES)
#   (i) unweighted glmer (main spec)
#   (ii) design-weighted direct estimates (sensitivity)
#   (iii) weighted mixed model via WeMix (sensitivity)
#
# Also builds the household-level prediction frame with covariates harmonized
# to ACCESS/IRES (used by 06_stacking_prediction.R).
#
# Inputs : cvd_load.RData, image2015/2019.RData, gps, district shapefile
# Outputs: nfhs_districts.rds, nfhs_hh_covariates.rds  (in dir_out)
# ==============================================================================

source("00_config.R")
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "01_prep_nfhs"


# ---- Load (identical to the original exploratory analysis) --------------------
# Only india2015_hr and india2019_hr are used below, and they live in the two
# whole-workspace images (image2015/2019.RData, which also hold IR, MR, men,
# women ...); cvd_load.RData holds all_2015/all_2019 for H4, not these. The
# original version load()ed cvd_load and both images into the global
# environment at once -- well over 30 GB expanded -- which exhausts memory on
# a laptop. Instead: open ONE image at a time in a private environment, keep
# its household frame, discard the rest, and cache the frame in a slim file
# next to the inputs so later runs never open the images at all.
# Columns this script reads (everything else in the ~hundreds-column HR recode
# is dead weight: 2.8 M member rows x 700 columns is ~10 GB per survey (the image
# objects are one row per household MEMBER -- see "ONE ROW PER HOUSEHOLD"), and the mixed
# models and the survey design each copy the frame). Trimming to these keeps
# the two frames at well under 1 GB.
HR_KEEP <- c("hv000", "hv001", "hv002", "hv005", "hv006", "hv007", "hv008", "hv012",
             "hv021", "hv022", "hv024", "hv025", "hv206", "hv226", "hv247",
             "hv270", "hv271", "wealthquin", "fuel", "hh_religion",
             "sh36", "sh49", "sh58", "sh75", "survey",
             "month_interview", "yr_interview",
             "DHSREGCO", "LATNUM", "LONGNUM", "district", "state", "clust")
trim_hr <- function(hr) {
  hr <- hr[, intersect(HR_KEEP, names(hr))]
  # 09_season_sensitivity.R needs the interview month; derive it from the DHS
  # date-of-interview fields if the extract does not carry it explicitly.
  if (!"month_interview" %in% names(hr) && "hv006" %in% names(hr))
    hr$month_interview <- as.numeric(hr$hv006)
  if (!"yr_interview" %in% names(hr) && "hv007" %in% names(hr))
    hr$yr_interview <- as.numeric(hr$hv007)
  hr
}

load_hr_slim <- function(year, path_image) {
  slim <- file.path(dir_dhs, sprintf("nfhs_hr_%d_slim.RData", year))
  obj  <- sprintf("india%d_hr", year)
  if (file.exists(slim)) {
    message("Loading cached ", obj, " from ", slim)
    e <- new.env(); load(slim, envir = e)
    hr <- trim_hr(get(obj, envir = e)); rm(e); invisible(gc())
    # A cache written before the household-unit fix (October 2026) has no hv002,
    # so households cannot be identified in it. Re-extract rather than proceed:
    # carrying on would silently restore the member-level frame.
    if ("hv002" %in% names(hr)) return(hr)
    message("Cached ", obj, " predates the household-unit fix (no hv002); ",
            "re-extracting from ", basename(path_image), " and overwriting the cache.")
    rm(hr); invisible(gc())
  }
  message("Extracting ", obj, " from ", basename(path_image), " (slow, once) ...")
  e <- new.env(); load(path_image, envir = e)
  if (!exists(obj, envir = e))
    stop(obj, " not found in ", path_image, ". Objects present: ",
         paste(ls(e), collapse = ", "), call. = FALSE)
  hr <- trim_hr(get(obj, envir = e))
  rm(e); invisible(gc())
  assign(obj, hr); save(list = obj, file = slim)
  message("Cached ", obj, " (", nrow(hr), " member rows; collapsed to households below) to ", slim)
  hr
}
india2019_hr <- load_hr_slim(2019, path_image2019)
india2015_hr <- load_hr_slim(2015, path_image2015)

## ---- ONE ROW PER HOUSEHOLD ------------------------------------------------------
## The india{year}_hr objects in the workspace images carry one row per household
## MEMBER, not per household: 2,869,043 rows for NFHS-4 against the 601,509
## households DHS interviewed, and 2,843,917 against 636,699 for NFHS-5 (about
## 100 rows per cluster where DHS samples about 21 households). The household
## variables are repeated on every member's row, so nothing looked wrong -- but
## every prevalence computed from the frame was the share of PEOPLE living in
## LPG-primary households, compared against ACCESS and IRES, which count
## HOUSEHOLDS, and the multilevel models treated the members of one household as
## independent observations. Because larger households are less likely to cook
## primarily with LPG, the member-level frame understated rural primary-LPG
## prevalence by 1.7 points (NFHS-4) and 2.3 points (NFHS-5). Found October 2026
## with scripts/check_nfhs_unit.R.
##
## Collapse to the DHS household key (hv001 cluster, hv002 household number).
## Before keeping the first member's row, require every kept column to be
## constant within each household: if one is not, it is a member-level variable
## and taking the first member's value would be arbitrary, so stop and name it.
NFHS_PUBLISHED_HH <- c(`2015` = 601509, `2019` = 636699)   # DHS final reports
to_households <- function(hr, year) {
  if (!"hv002" %in% names(hr))
    stop("NFHS ", year, " frame has no hv002, so households cannot be identified.",
         call. = FALSE)
  # A missing cluster or household number would silently merge households.
  if (anyNA(hr$hv001) || anyNA(hr$hv002))
    stop("NFHS ", year, ": hv001/hv002 has missing values, so households cannot ",
         "be identified safely.", call. = FALSE)
  key  <- paste(hr$hv001, hr$hv002, sep = "_")
  n_hh <- length(unique(key))
  hh_vars <- setdiff(names(hr), c("hv001", "hv002"))
  varying <- hh_vars[vapply(hh_vars, function(v)
    length(unique(paste(key, hr[[v]], sep = "\r"))) != n_hh, logical(1))]
  if (length(varying))
    stop("NFHS ", year, ": these columns vary within a household, so they are not ",
         "household-level and the frame cannot be collapsed safely: ",
         paste(varying, collapse = ", "), call. = FALSE)
  out <- hr[!duplicated(key), , drop = FALSE]
  # Fail here, before any model is fitted, if the household count is off; the
  # end-of-script check repeats this for the check trail.
  pub <- NFHS_PUBLISHED_HH[[as.character(year)]]
  if (abs(nrow(out) / pub - 1) > 0.005)
    stop(sprintf("NFHS %d: %s households after collapsing vs %s published by DHS (off by more than 0.5%%).",
                 year, format(nrow(out), big.mark = ","), format(pub, big.mark = ",")), call. = FALSE)
  message(sprintf("NFHS %d: %s member rows -> %s households (DHS published %s; %.2f members per household)",
                  year, format(nrow(hr), big.mark = ","), format(nrow(out), big.mark = ","),
                  format(NFHS_PUBLISHED_HH[[as.character(year)]], big.mark = ","),
                  nrow(hr) / nrow(out)))
  attr(out, "unit_audit") <- c(rows_in = nrow(hr), households = nrow(out),
                               published = NFHS_PUBLISHED_HH[[as.character(year)]])
  out
}
india2015_hr <- to_households(india2015_hr, 2015)
india2019_hr <- to_households(india2019_hr, 2019)
.unit_audit <- list(`2015` = attr(india2015_hr, "unit_audit"),
                    `2019` = attr(india2019_hr, "unit_audit"))

load(path_gps_2019)
india2019_hr <- merge(india2019_hr, gps, by.x = "hv001", by.y = "cluster", all.x = TRUE)
rm(gps)
india2019_hr$district <- india2019_hr$DHSREGCO
india2019_hr$state    <- india2019_hr$hv024
india2019_hr$clust    <- paste0(india2019_hr$hv001, india2019_hr$hv000, india2019_hr$survey)

load(path_gps_2015)
india2015_hr <- merge(india2015_hr, gps, by.x = "hv001", by.y = "cluster", all.x = TRUE)
rm(gps)
india2015_hr$district <- india2015_hr$DHSREGCO
india2015_hr$state    <- india2015_hr$hv024
india2015_hr$clust    <- paste0(india2015_hr$hv001, india2015_hr$hv000, india2015_hr$survey)

districts_2015 <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(dist_code = as.numeric(dist_code)) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()
districts_2015 <- subset(districts_2015, select = -c(dist_cod_1))

# ---- Harmonized variables (NATIONAL, both surveys) ----------------------------
prep_nfhs <- function(hr, survey_year) {
  hr %>%
    mutate(
      lpg        = as.integer(haven::zap_labels(fuel) == 2),
      # hv226 codes (NFHS-4 and NFHS-5 recode maps): 1 electricity, 2 LPG/natural
      # gas (NFHS-5 labels it "LPG"; its separate natural-gas code 3 is empty),
      # 4 biogas, 5 kerosene, 6-11 coal/charcoal/biomass, 95 no food cooked,
      # 96 other. Clean primary fuel = electricity, LPG/natural gas, biogas;
      # kerosene is not clean, and households that cook no food are NA.
      # Carried in the household frame for reference; no reported analysis uses it.
      clean_fuel = dplyr::case_when(
        haven::zap_labels(fuel) %in% c(1, 2, 3, 4) ~ 1L,
        haven::zap_labels(fuel) == 95              ~ NA_integer_,
        !is.na(fuel)                               ~ 0L,
        TRUE                                       ~ NA_integer_),
      rural      = as.integer(hv025 == 2),           # hv025: 1 urban, 2 rural
      wt         = hv005 / 1e6,                      # household weight
      psu        = hv021,                            # PSU (cluster)
      strat      = hv022,                            # sample stratum
      hh_size    = hv012,
      # Missing/unknown religion -> NA, not a real "Other" category (mirrors the
      # IRES fix; else missingness is silently absorbed into Other).
      hh_relig   = case_when(is.na(hh_religion)  ~ NA_character_,
                             hh_religion == 1    ~ "Hindu",
                             hh_religion == 2    ~ "Muslim",
                             TRUE ~ "Other"),
      # caste: sh34/sh36-style vars differ across rounds; both rounds carry
      # a harmonized caste var in the cvd_load extract if present:
      bank_account = ifelse(hv247 == 8, NA, hv247),
      # explicit 0/1 so nonstandard codes (e.g. 8/9 "don't know") become NA, not 0
      electricity  = dplyr::case_when(hv206 == 1 ~ 1L, hv206 == 0 ~ 0L,
                                      TRUE ~ NA_integer_),
      bpl          = if (survey_year == 2015) ifelse(sh58 == 8, NA, sh58)
                     else ifelse(sh75 == 8, NA, sh75),
      survey_year  = survey_year
    )
}

india2015_hr <- prep_nfhs(india2015_hr, 2015)
india2019_hr <- prep_nfhs(india2019_hr, 2019)

# Caste (household head) -- the NFHS HR-recode country-specific item, sh36 in
# NFHS-4 and sh49 in NFHS-5, coded 1 SC, 2 ST, 3 OBC, 4 none of them.
#
# These two names used to carry a "<-- check name" comment, which is not a
# reasonable thing to ship: caste is a covariate in every prediction model AND
# the Scheduled Caste / Scheduled Tribe district shares are the benchmark
# falsification the paper leans on to argue the NFHS-IRES discrepancy is specific
# to the cooking-fuel item. If the wrong column were read, that argument would be
# built on noise and nothing downstream would notice -- the pipeline would
# happily compare two meaningless district surfaces and report their agreement.
#
# So the name is now asserted rather than assumed, in three ways:
#   (1) the column must exist -- an absent one stops the run instead of setting
#       caste to NA and continuing with a covariate that is silently all-missing;
#   (2) its observed values must lie in {1, 2, 3, 4} (plus NA) -- a mis-named
#       column almost always carries some other code range, and this catches it;
#   (3) the resulting national distribution is printed every run and warns if it
#       leaves a deliberately wide plausible band. Indian SC and ST population
#       shares are roughly 17% and 9% (Census 2011) and the NFHS household
#       distribution is close to that, so a column that produced, say, 60% SC
#       would be flagged even though its codes were in range.
# (3) warns rather than fails because it is a plausibility heuristic, not a
# definition; (1) and (2) are definitional and stop the run.
CASTE_LEVELS <- c("Scheduled Caste", "Scheduled Tribe",
                  "Other Backward Class", "General")
add_caste <- function(hr, caste_col, round_label) {
  if (!caste_col %in% names(hr))
    stop("Caste column '", caste_col, "' is not in the ", round_label,
         " household extract. This is the SC/ST/OBC item used as a model ",
         "covariate AND as the benchmark falsification variable, so the run ",
         "stops rather than continuing with caste set to NA. Columns present ",
         "that look like candidates: ",
         paste(grep("^sh[0-9]+$", names(hr), value = TRUE), collapse = ", "),
         call. = FALSE)
  raw  <- haven::zap_labels(hr[[caste_col]])
  vals <- sort(unique(stats::na.omit(as.numeric(raw))))
  # 1 SC, 2 ST, 3 OBC, 4 none of them are the substantive codes. DHS also uses
  # 8 for "don't know" and 9 for "missing" on a one-digit item (98/99 on a
  # two-digit one), and NFHS-4's sh36 does carry 8s -- about a twentieth of
  # households say they do not know the household head's category. Those are
  # legitimately missing, not a sign that the wrong column was read, so they map
  # to NA and are reported. Only a code outside BOTH sets means the column is
  # not what this function thinks it is, and that still stops the run.
  MISSING_CODES <- c(8, 9, 98, 99)
  unknown <- setdiff(vals, c(1:4, MISSING_CODES))
  if (length(unknown))
    stop("Caste column '", caste_col, "' in ", round_label, " carries code(s) ",
         paste(unknown, collapse = ", "), ", which are neither the expected ",
         "1 SC / 2 ST / 3 OBC / 4 none nor a DHS missing code (",
         paste(MISSING_CODES, collapse = ", "), "). Either the column name is ",
         "wrong for this round or the coding changed; fix add_caste() rather ",
         "than letting the recode drop values silently.", call. = FALSE)
  n_dk <- sum(as.numeric(raw) %in% MISSING_CODES, na.rm = TRUE)
  hr$caste <- factor(dplyr::case_when(
    raw == 1 ~ CASTE_LEVELS[1],
    raw == 2 ~ CASTE_LEVELS[2],
    raw == 3 ~ CASTE_LEVELS[3],
    raw == 4 ~ CASTE_LEVELS[4],
    TRUE ~ NA_character_), levels = CASTE_LEVELS)
  shares <- prop.table(table(hr$caste))   # table() drops NA: shares are of the known
  message(sprintf(
    "  caste (%s, %s): SC %.1f%% | ST %.1f%% | OBC %.1f%% | General %.1f%% (of known); missing %.1f%% overall, of which %d rows were a don't-know/missing code",
    round_label, caste_col, 100*shares[1], 100*shares[2], 100*shares[3],
    100*shares[4], 100*mean(is.na(hr$caste)), n_dk))
  plausible <- shares[1] > 0.05 && shares[1] < 0.40 &&
               shares[2] > 0.02 && shares[2] < 0.30
  # "01", not .chk_tag(). CHK_SCRIPT is "01_prep_nfhs" because that is the
  # convention model_fits.csv uses, but every other chk() in this file writes
  # the short tag, so routing this one through .chk_tag() split the script
  # across two rows of the per-script tally -- 01 with 11 checks and
  # 01_prep_nfhs with 2, reading as 32 scripts where there are 31. Labels only;
  # no check, count or result changes.
  if (exists("chk"))
    chk("01",
        sprintf("caste column %s (%s) has in-range codes and a plausible distribution",
                caste_col, round_label),
        plausible,
        sprintf("SC %.1f%%, ST %.1f%% (census 2011: ~17%%, ~9%%)",
                100*shares[1], 100*shares[2]))
  else if (!plausible)
    warning("Caste distribution from '", caste_col, "' in ", round_label,
            " is outside the plausible band -- verify the column name.",
            call. = FALSE)
  hr
}
india2015_hr <- add_caste(india2015_hr, "sh36", "NFHS-4")
india2019_hr <- add_caste(india2019_hr, "sh49", "NFHS-5")

# EDUCATION IS NOT DERIVED HERE, AND THAT IS DELIBERATE (removed 2026-08-01).
#
# The intended variable was "household head has primary schooling or less", to
# match ACCESS (edu_primary_or_less) and IRES (edu_low). The only NFHS column
# that could carry that contrast is the DHS attainment scale `eduatt`, and it is
# not present in the household extracts used here -- the run of 2026-08-01
# reported nonmissing = 0, distinct = 0 in both NFHS-4 and NFHS-5, i.e. 100%
# missing on all 2.87M and 2.83M rows of the (then member-level) frames.
#
# A previous version fell back to `hh_college`, a binary college flag, for which
# `<= 2` is TRUE at both values; edu_low then came out CONSTANT rather than
# missing, which passes every structural check while being useless to every
# analysis. "Did not attend college" is also simply not the contrast ACCESS and
# IRES measure, so the substitution would have been wrong even had it varied.
#
# Because education cannot be harmonized across the three surveys, it is not a
# covariate, not a falsification benchmark, and not mapped. Carrying an all-NA
# edu_low forward only produced downstream checks that could never pass, so the
# derivation and every consumer of it have been removed. The manuscript still
# discloses the gap in the Table 1 comparability row ("Education of respondent /
# head -- not populated in extract"); that row is the honest record of this
# decision and should not be dropped while this comment stands.
#
# TO RESTORE: obtain an NFHS extract that carries `eduatt` (or another
# attainment scale), derive edu_low = as.integer(eduatt <= 2) here, then re-add
# it to the select() lists below, to the benchmark variable vectors in
# 08_si_benchmarks.R and 13_benchmark_maps.R, to the item list in
# 17_missingness.R, and to the candidate covariates in 06_stacking_prediction.R
# and 15_variable_importance.R.

# Wealth: prefer the continuous factor score (hv271) over the quintile (hv270);
# 06 converts this to within-state quintiles to align with ACCESS/IRES
# within-state expenditure quintiles.
add_wealth <- function(hr) {
  src <- intersect(c("hv271", "hv270", "wealthquin"), names(hr))[1]
  if (!is.na(src)) {
    hr$wealth_score <- suppressWarnings(as.numeric(hr[[src]]))
    message("wealth_score derived from '", src, "'.")
  } else {
    warning("No wealth column (hv271/hv270) found; wealth_score set NA.")
    hr$wealth_score <- NA_real_
  }
  hr
}
india2015_hr <- add_wealth(india2015_hr)
india2019_hr <- add_wealth(india2019_hr)

# ---- Cluster -> NFHS-4 (2011 Census) district crosswalk, both rounds ----------
# NOT by point-in-polygon on the district shapefile: checked against DHS's own
# district code for every NFHS-4 cluster, the shapefile polygons put only ~72%
# of clusters in their own district (and ~13% in the wrong state). See
# geo_crosswalk.R for the rule (NFHS-4: DHSREGCO; NFHS-5: its own district,
# matched to the 2011 district by name, or to its 2011 parent by the labels of
# the nearest NFHS-4 clusters in the same state). The shapefile is used only to
# draw maps and to read district/state names by code.
source("geo_crosswalk.R")
.e15 <- new.env(); load(path_gps_2015, envir = .e15)
.e19 <- new.env(); load(path_gps_2019, envir = .e19)
XW <- build_nfhs_xwalk(.e15$gps, .e19$gps,
  names4 = districts_2015 %>% st_drop_geometry() %>%
    transmute(code = as.numeric(dist_code), name = dist_name))
xw <- XW$xwalk
readr::write_csv(xw, file.path(dir_out, "nfhs_cluster_district_xwalk.csv"))

# Diagnostic kept for the SI: how often point-in-polygon would have agreed with
# DHS's own NFHS-4 district code.
.g4 <- .e15$gps %>% filter(is.finite(LATNUM), LATNUM != 0, LONGNUM != 0)
.p4 <- st_as_sf(.g4, coords = c("LONGNUM", "LATNUM"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(districts_2015))
poly_check4 <- st_join(.p4, districts_2015 %>% select(dist_code), join = st_within) %>%
  st_drop_geometry() %>% distinct(cluster, .keep_all = TRUE) %>%
  transmute(cluster, lon = LONGNUM, lat = LATNUM, dhs_district = as.numeric(DHSREGCO),
            polygon_district = as.numeric(dist_code),
            agree = !is.na(polygon_district) & polygon_district == dhs_district)
saveRDS(poly_check4, file.path(dir_out, "nfhs4_polygon_check.rds"))
rm(.p4, .g4); invisible(gc())

n_hh_2019_total   <- nrow(india2019_hr)
n_hh_2019_zerogps <- sum(india2019_hr$LATNUM == 0 | india2019_hr$LONGNUM == 0 |
                           is.na(india2019_hr$LATNUM), na.rm = TRUE)
xw5 <- xw %>% filter(survey == "NFHS5") %>% select(cluster, district, method)
nm_tbl <- districts_2015 %>% st_drop_geometry() %>%
  transmute(dist_code = as.numeric(dist_code), state_name, dist_name)
india2019_hr <- india2019_hr %>%
  select(-any_of(c("district", "state_name", "dist_name"))) %>%
  left_join(xw5, by = c("hv001" = "cluster")) %>%
  left_join(nm_tbl, by = c("district" = "dist_code"))

# ---- NFHS-5 -> NFHS-4 cluster-assignment diagnostics (for Methods/SI) ----------
cluster_assign_2019 <- XW$nfhs5_detail %>%
  left_join(.e19$gps %>% transmute(cluster = as.numeric(cluster), lon = LONGNUM, lat = LATNUM),
            by = "cluster") %>%
  left_join(india2019_hr %>% count(hv001, name = "n_hh"), by = c("cluster" = "hv001")) %>%
  mutate(assigned = !is.na(district),
         by_location = grepl("^location", method))
saveRDS(cluster_assign_2019, file.path(dir_out, "nfhs5_cluster_assignment.rds"))
rm(.e15, .e19); invisible(gc())

.dg <- function(m) XW$diagnostics$value[XW$diagnostics$metric == m]
linkage_nfhs5 <- tibble(
  metric = c("NFHS-5 households, total",
             "households with unrecorded GPS (lat = lon = 0 in DHS release)",
             XW$diagnostics$metric,
             "NFHS-5 households assigned by district name",
             "NFHS-5 households assigned by location",
             "households unassigned",
             "share of households assigned",
             "NFHS-4 clusters inside a shapefile polygon",
             "share of those in the polygon of their own DHS district"),
  value = c(n_hh_2019_total, n_hh_2019_zerogps,
            XW$diagnostics$value,
            sum(grepl("^name match", india2019_hr$method)),
            sum(grepl("^location", india2019_hr$method)),
            sum(is.na(india2019_hr$district)),
            round(mean(!is.na(india2019_hr$district)), 4),
            sum(!is.na(poly_check4$polygon_district)),
            round(mean(poly_check4$agree[!is.na(poly_check4$polygon_district)]), 4)))
readr::write_csv(linkage_nfhs5, file.path(dir_out, "nfhs5_linkage_diagnostics.csv"))
message("NFHS-5 -> NFHS-4 crosswalk: ",
        sum(cluster_assign_2019$by_location), " of ", nrow(cluster_assign_2019),
        " clusters assigned by location, the rest by district name; ",
        sum(!cluster_assign_2019$assigned), " unassigned. Shapefile polygons agree with ",
        "DHS's own NFHS-4 district code for ",
        sprintf("%.1f%%", 100 * mean(poly_check4$agree[!is.na(poly_check4$polygon_district)])),
        " of NFHS-4 clusters (not used).")

# NFHS-4 districts already ARE the 2015 geography:
india2015_hr <- india2015_hr %>%
  left_join(districts_2015 %>% st_drop_geometry() %>%
              select(dist_code, state_name, dist_name),
            by = c("DHSREGCO" = "dist_code")) %>%
  mutate(district = DHSREGCO)

# ---- District estimates: function over subsets --------------------------------
make_estimates <- function(hr, label) {
  hr <- hr %>% mutate(state = factor(state_name), district = factor(district),
                      clust = factor(clust))

  est_all <- district_estimates_glmer(hr, "lpg") %>%
    rename(!!paste0("lpg_", label) := p_hat, !!paste0("n_", label) := n_hh)

  hr_rur <- hr %>% filter(rural == 1)
  est_rur <- district_estimates_glmer(hr_rur, "lpg") %>%
    rename(!!paste0("lpg_", label, "_rural") := p_hat,
           !!paste0("n_", label, "_rural") := n_hh) %>%
    select(-state)

  # Sensitivity: design-weighted direct district means (rural domain).
  # Design is built on ALL households, then restricted to rural inside the
  # design object -- proper domain estimation, avoids lonely-PSU strata.
  est_rur_wt <- district_estimates_weighted(
    hr, "lpg", ids = "psu", strata = "strat", weights = "wt",
    domain_col = "rural") %>%
    rename(!!paste0("lpg_", label, "_rural_wt") := p_wt,
           !!paste0("lpg_", label, "_rural_wt_se") := p_wt_se)

  est_all %>%
    left_join(est_rur,    by = "district") %>%
    left_join(est_rur_wt %>% mutate(district = factor(district)), by = "district")
}

nfhs4_districts <- make_estimates(india2015_hr, "2015")
nfhs5_districts <- make_estimates(india2019_hr, "2019")

# Optional third sensitivity: weighted multilevel model (can be slow nationally;
# run on the 6 ACCESS states if you want the ACCESS-comparison version)
run_wemix <- FALSE
if (run_wemix) {
  wemix_2015 <- india2015_hr %>%
    filter(rural == 1, state %in% ACCESS_STATES_2015 | state_name %in% ACCESS_STATE_NAMES) %>%
    district_estimates_wemix("lpg")
  nfhs4_districts <- left_join(nfhs4_districts,
                               wemix_2015 %>% select(-state), by = "district")
}

nfhs_districts <- full_join(
  nfhs4_districts %>% mutate(district = as.character(district)),
  nfhs5_districts %>% mutate(district = as.character(district)) %>% select(-state),
  by = "district")

saveRDS(nfhs_districts, file.path(dir_out, "nfhs_districts.rds"))

# ---- Household-level covariate frame for prediction (06) ----------------------
# Keep only predictors that ALSO exist in ACCESS/IRES (the crosswalk):
#   caste, religion, hh size, BPL card, bank account, electricity,
#   rural, state, district, weights. Education is NOT among them -- see the
#   comment at the head of this script.
for (v in c("month_interview", "yr_interview")) {
  if (!v %in% names(india2015_hr)) india2015_hr[[v]] <- NA_real_
  if (!v %in% names(india2019_hr)) india2019_hr[[v]] <- NA_real_
}
nfhs_hh <- bind_rows(
  india2015_hr %>% st_drop_geometry_safe() %>%
    transmute(survey = "NFHS4", survey_year, state = state_name,
              district = as.character(district), clust, wt, psu, strat,
              rural, lpg, clean_fuel, hh_size, hh_relig, caste,
              wealth_score, bpl, bank_account, electricity,
              month_interview = as.numeric(month_interview),
              yr_interview = as.numeric(yr_interview)),
  india2019_hr %>% st_drop_geometry_safe() %>%
    transmute(survey = "NFHS5", survey_year, state = state_name,
              district = as.character(district), clust, wt, psu, strat,
              rural, lpg, clean_fuel, hh_size, hh_relig, caste,
              wealth_score, bpl, bank_account, electricity,
              month_interview = as.numeric(month_interview),
              yr_interview = as.numeric(yr_interview))
)
saveRDS(nfhs_hh, file.path(dir_out, "nfhs_hh_covariates.rds"))

## ---- CHECKS ------------------------------------------------------------------
chk_header("01_prep_nfhs")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("01", "01_prep_nfhs")

# The household-unit fix (see "ONE ROW PER HOUSEHOLD" above). The household
# count must match what DHS reports interviewing -- before GPS exclusions, which
# happen after the collapse. A member-level frame fails this by ~+370%, so it is
# the guard against the bug coming back; a "rows == households" check would be
# true by construction after the collapse and could never fail.
for (.y in names(.unit_audit)) {
  .a <- .unit_audit[[.y]]
  chk("01", sprintf("NFHS %s household count matches the DHS published sample (within 0.5%%)", .y),
      abs(.a[["households"]] / .a[["published"]] - 1) < 0.005,
      sprintf("%s households vs %s published (%+.2f%%)",
              format(.a[["households"]], big.mark = ","), format(.a[["published"]], big.mark = ","),
              100 * (.a[["households"]] / .a[["published"]] - 1)))
}
chk("01", "NFHS-5 households assigned to an NFHS-4 district (crosswalk) >= 99%",
    mean(!is.na(india2019_hr$district)) >= 0.99,
    sprintf("%.2f%% assigned", 100 * mean(!is.na(india2019_hr$district))))
chk("01", "location vote agrees with district-name match for >= 95% of name-matched NFHS-5 clusters",
    .dg("agreement of location vote with name match (name-matched clusters, before override)") >= 0.95,
    sprintf("agreement %.1f%%", 100 * .dg("agreement of location vote with name match (name-matched clusters, before override)")))
chk("01", "every NFHS-5 location vote used same-state NFHS-4 neighbours",
    .dg("NFHS-5 clusters whose location vote had no same-state reference cluster") == 0,
    sprintf("%d clusters fell back to a national search (state-name mismatch)",
            as.integer(.dg("NFHS-5 clusters whose location vote had no same-state reference cluster"))))
chk("01", "every NFHS-4 household has a DHS district code",
    !anyNA(india2015_hr$district), sprintf("%d missing", sum(is.na(india2015_hr$district))))
chk("01", "district table non-empty", nrow(nfhs_districts) > 500,
    paste0(nrow(nfhs_districts), " districts"))
chk("01", "rural + design-weighted LPG columns present",
    chk_has_cols(nfhs_districts, c("lpg_2015_rural","lpg_2019_rural",
      "lpg_2015_rural_wt","lpg_2015_rural_wt_se",
      "lpg_2019_rural_wt","lpg_2019_rural_wt_se")))
chk("01", "2015/2019 rural prevalences in [0,1]",
    chk_in_range(nfhs_districts$lpg_2015_rural, 0, 1) &&
    chk_in_range(nfhs_districts$lpg_2019_rural, 0, 1))
chk("01", ">600 districts have a 2019 rural estimate",
    sum(is.finite(nfhs_districts$lpg_2019_rural)) > 600,
    paste0(sum(is.finite(nfhs_districts$lpg_2019_rural)), " districts"))
chk("01", "household frame carries lpg + covariates",
    chk_has_cols(nfhs_hh, c("lpg","hh_relig","caste","electricity")))
# Education was removed on 2026-08-01 because the NFHS extracts do not carry an
# attainment scale (100% missing in both rounds). Assert its ABSENCE, so that a
# future extract carrying a usable column fails here and forces a deliberate
# re-introduction rather than silently reviving a half-wired benchmark.
chk("01", "no education column is carried forward (see header note)",
    !any(grepl("^edu", names(nfhs_hh))),
    if (any(grepl("^edu", names(nfhs_hh))))
      paste("unexpected:", paste(grep("^edu", names(nfhs_hh), value = TRUE), collapse = ", "))
    else "none present, as intended")
chk_warn("01", "religion Other share plausible (<15%; missing is NA)",
    { s <- mean(nfhs_hh$hh_relig == "Other", na.rm = TRUE); is.finite(s) && s < 0.15 },
    paste0("Other = ", round(100 * mean(nfhs_hh$hh_relig == "Other", na.rm = TRUE), 1),
           "%, NA = ", chk_pct_na(nfhs_hh$hh_relig), "%"))
chk_warn("01", "month_interview populated (needed by 09)",
    mean(is.finite(nfhs_hh$month_interview)) > 0.5,
    paste0(chk_pct_na(nfhs_hh$month_interview), "% NA"))

message("01_prep_nfhs.R done: ",
        nrow(nfhs_districts), " districts; ",
        nrow(nfhs_hh), " households saved.")
