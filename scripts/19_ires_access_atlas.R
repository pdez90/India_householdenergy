# ==============================================================================
# 19_ires_access_atlas.R   (standalone once 01-05 have run; fast, maps only)
#
# A DETAILED verification atlas for the ACCESS and IRES data, plus per-stage QC
# maps, so every processing step can be eyeballed for correctness. Purely
# descriptive: reads on-disk outputs, touches no analysis logic, changes no
# results. If a map looks wrong (a variable mapped to the wrong districts, an
# implausible prevalence, missing coverage), the bug is upstream.
#
# Produces, under <dir_out>/diagnostics/ :
#   atlas_ACCESS.pdf   every derived ACCESS (Wave 1) district variable, mapped
#   atlas_IRES.pdf     every derived IRES district variable, mapped (+ rural-only)
#   qc_verification.pdf per-stage checks: coverage, design-wt vs multilevel,
#                       IRES rural vs all, raw vs corrected NFHS-5, survey overlap
#   distributions.pdf  household-level distributions (consumption, expenditure, size)
#   recode_summary.txt weighted frequency tables for the categorical recodes
#
# Inputs : access_hh.rds, ires_hh.rds, access_districts.rds, ires_districts.rds,
#          nfhs_districts.rds, corrected_nfhs_districts.rds (optional), shapefile
# ==============================================================================

source("00_config.R")
need_inputs(c("access_hh.rds"        = "02_prep_access.R",
              "ires_hh.rds"          = "03_prep_ires.R",
              "access_districts.rds" = "02_prep_access.R",
              "ires_districts.rds"   = "03_prep_ires.R",
              "nfhs_districts.rds"   = "01_prep_nfhs.R"))

diag_dir <- file.path(dir_out, "diagnostics")
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

access_hh <- readRDS(file.path(dir_out, "access_hh.rds"))
ires_hh   <- readRDS(file.path(dir_out, "ires_hh.rds"))
acc_d     <- readRDS(file.path(dir_out, "access_districts.rds"))
ire_d     <- readRDS(file.path(dir_out, "ires_districts.rds"))
nfhs_d    <- readRDS(file.path(dir_out, "nfhs_districts.rds"))
corr_p    <- file.path(dir_out, "corrected_nfhs_districts.rds")
corr      <- if (file.exists(corr_p)) readRDS(corr_p) else NULL

# ---- District geography ------------------------------------------------------
shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  mutate(district = as.character(as.numeric(dist_code))) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

# ---- Helpers -----------------------------------------------------------------
# NA/weight-safe weighted mean.
dwmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w)
  if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
}

# Harmonized 0/1 indicator set from a household frame (guards missing columns).
add_ind <- function(hh, wcol) {
  has <- function(c) c %in% names(hh)
  fac <- function(c, v) if (has(c)) as.integer(hh[[c]] == v) else NA_real_
  num <- function(c) if (has(c)) suppressWarnings(as.numeric(hh[[c]])) else NA_real_
  tibble(
    district = as.character(hh$district),
    .w = suppressWarnings(as.numeric(hh[[wcol]])),
    i_lpg       = num("main_fuel_lpg"),
    i_clean     = num("main_fuel_clean"),
    i_uselpg    = num("uselpg"),
    i_anysolid  = num("any_solid"),
    i_stack_cond= num("stack_binary"),                       # among LPG-main hh
    i_excl      = fac("use3cat", "LPG, no solid fuel reported"),
    i_solidonly = fac("use3cat", "Solid fuel reported, no LPG"),
    i_stack3    = fac("use3cat", "LPG and solid fuel reported"),
    i_other     = fac("use3cat", "Neither LPG nor solid fuel reported"),
    i_sc        = fac("caste", "Scheduled Caste"),
    i_st        = fac("caste", "Scheduled Tribe"),
    i_obc       = fac("caste", "Other Backward Class"),
    i_gen       = fac("caste", "General"),
    i_hindu     = fac("religion", "Hindu"),
    i_muslim    = fac("religion", "Muslim"),
    i_elec      = num("electricity"),
    i_bpl       = if (has("bplaay")) num("bplaay") else num("bpl"),
    i_edu       = if (has("edu_low")) num("edu_low") else num("edu_primary_or_less"),
    i_afford    = num("lpg_afford6"),
    i_kg        = num("lpg_kg_yr"),
    i_pmuy      = num("pmuy"))
}

# District-level design-weighted summary (robust long/pivot form).
dist_sum <- function(hh, wcol) {
  x    <- add_ind(hh, wcol) %>% filter(!is.na(district))
  vars <- grep("^i_", names(x), value = TRUE)
  base <- x %>% count(district, name = "n_hh")
  wm <- x %>% select(district, .w, all_of(vars)) %>%
    pivot_longer(all_of(vars), names_to = "var", values_to = "val") %>%
    group_by(district, var) %>%
    summarise(m = dwmean(val, .w), .groups = "drop") %>%
    pivot_wider(names_from = var, values_from = m)
  left_join(base, wm, by = "district")
}

# Choropleth of one column of a district-summary table.
choro <- function(dsum, var, title, unit = "share", option = "D") {
  d <- left_join(shp, dplyr::select(dsum, district, dplyr::all_of(var)),
                 by = "district")
  ggplot(d) +
    geom_sf(aes(fill = .data[[var]]), color = NA) +
    scale_fill_viridis_c(na.value = "grey88", option = option) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12),
          legend.position = "right") +
    labs(title = title, fill = unit)
}

# Agreement scatter on the [0,1] square.
sc <- function(d, x, y, xl, yl, ttl) {
  d <- d %>% filter(is.finite(.data[[x]]), is.finite(.data[[y]]))
  if (nrow(d) < 3) return(NULL)
  r <- cor(d[[x]], d[[y]])
  ggplot(d, aes(.data[[x]], .data[[y]])) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
    geom_point(alpha = 0.5, color = "steelblue") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = xl, y = yl, title = ttl,
         subtitle = sprintf("r = %.3f  |  n = %d districts", r, nrow(d))) +
    theme_bw()
}

# What to map, in order. (unit is cosmetic; kg/count use their own scale below.)
SPEC <- tibble::tribble(
  ~var,          ~title,                                   ~unit,
  "i_lpg",       "Primary cooking fuel = LPG",             "share",
  "i_clean",     "Primary fuel clean (LPG/PNG/elec/biogas)","share",
  "i_uselpg",    "Any LPG use",                            "share",
  "i_anysolid",  "Any solid-fuel use",                     "share",
  "i_stack_cond","Stacking | primary fuel LPG (conditional)","share",
  "i_excl",      "LPG, no solid fuel reported (use3cat)",                "share",
  "i_stack3",    "LPG + solid stacking (use3cat)",         "share",
  "i_solidonly", "Solid fuel reported, no LPG (use3cat)",                   "share",
  "i_other",     "Other non-solid non-LPG (use3cat)",      "share",
  "i_kg",        "LPG consumption",                        "kg/yr",
  "i_afford",    "LPG affordable (<6% of expenditure)",    "share",
  "i_pmuy",      "PMUY beneficiary",                       "share",
  "i_sc",        "Scheduled Caste",                        "share",
  "i_st",        "Scheduled Tribe",                        "share",
  "i_obc",       "Other Backward Class",                   "share",
  "i_gen",       "General caste",                          "share",
  "i_hindu",     "Hindu",                                  "share",
  "i_muslim",    "Muslim",                                 "share",
  "i_elec",      "Household electricity",                  "share",
  "i_bpl",       "BPL / Antyodaya ration card",            "share",
  "i_edu",       "Low education (head)",                   "share")

atlas_pdf <- function(dsum, file, lab) {
  pdf(file, width = 8, height = 8.5); on.exit(dev.off(), add = TRUE)
  print(choro(dsum, "n_hh", paste0(lab, ": households per district (coverage)"),
              "n", option = "A"))
  for (i in seq_len(nrow(SPEC))) {
    v <- SPEC$var[i]
    if (v %in% names(dsum) && any(is.finite(dsum[[v]]))) {
      opt <- if (v == "i_kg") "G" else "D"
      print(choro(dsum, v, paste0(lab, ": ", SPEC$title[i]), SPEC$unit[i], opt))
    }
  }
}

message("Building ACCESS (Wave 1) district atlas ...")
acc_w1 <- if ("wave" %in% names(access_hh)) dplyr::filter(access_hh, wave == 0) else access_hh
acc_sum <- dist_sum(acc_w1, "weights")
atlas_pdf(acc_sum, file.path(diag_dir, "atlas_ACCESS.pdf"), "ACCESS W1")

message("Building IRES district atlas (+ rural-only) ...")
ire_sum <- dist_sum(ires_hh, "wt")
atlas_pdf(ire_sum, file.path(diag_dir, "atlas_IRES.pdf"), "IRES (all hh)")
ire_rural_sum <- if ("rural" %in% names(ires_hh))
  dist_sum(dplyr::filter(ires_hh, rural == 1), "wt") else NULL
if (!is.null(ire_rural_sum))
  atlas_pdf(ire_rural_sum, file.path(diag_dir, "atlas_IRES_rural.pdf"), "IRES (rural hh)")

# ---- QC / per-stage verification --------------------------------------------
message("Building per-stage QC verification maps ...")
qc <- file.path(diag_dir, "qc_verification.pdf")
pdf(qc, width = 9, height = 7)

# (A) Survey coverage + overlap on the NFHS district map
cov <- shp %>%
  left_join(acc_d %>% transmute(district, has_access = is.finite(n_access_w1)),
            by = "district") %>%
  left_join(ire_d %>% transmute(district, has_ires = is.finite(n_ires)),
            by = "district") %>%
  mutate(coverage = dplyr::case_when(
    coalesce(has_access, FALSE) & coalesce(has_ires, FALSE) ~ "ACCESS + IRES",
    coalesce(has_access, FALSE) ~ "ACCESS only",
    coalesce(has_ires, FALSE)   ~ "IRES only",
    TRUE ~ "Neither reference"))
print(ggplot(cov) + geom_sf(aes(fill = coverage), color = NA) +
        scale_fill_manual(values = c("ACCESS + IRES" = "#1B7837", "ACCESS only" = "#762A83",
                                     "IRES only" = "#2166AC", "Neither reference" = "grey85")) +
        theme_void(base_size = 11) +
        labs(title = "Reference-survey district coverage", fill = NULL))

# (B) Design-weighted vs multilevel (each survey) -- should track the identity line
print(sc(acc_d, "access_w1_mainlpg", "access_w1_mainlpg_wt",
         "ACCESS multilevel", "ACCESS design-weighted",
         "ACCESS W1 primary-LPG: multilevel vs design-weighted"))
print(sc(ire_d, "ires_mainlpg", "ires_mainlpg_wt",
         "IRES multilevel (all)", "IRES design-weighted (all)",
         "IRES primary-LPG (all hh): multilevel vs design-weighted"))
if (all(c("ires_mainlpg_rural", "ires_mainlpg_rural_wt") %in% names(ire_d)))
  print(sc(ire_d, "ires_mainlpg_rural", "ires_mainlpg_rural_wt",
           "IRES rural multilevel", "IRES rural design-weighted",
           "IRES primary-LPG (rural): multilevel vs design-weighted (feeds correction)"))

# (C) IRES rural vs all-household (should be similar but rural a touch lower)
if (all(c("ires_mainlpg_rural", "ires_mainlpg") %in% names(ire_d)))
  print(sc(ire_d, "ires_mainlpg", "ires_mainlpg_rural",
           "IRES all households", "IRES rural only",
           "IRES primary-LPG: all households vs rural only"))

# (D) NFHS-5: raw vs corrected surfaces (verifies the correction direction)
if (!is.null(corr)) {
  cc <- corr %>% mutate(district = as.character(as.numeric(district)))
  raw_map <- shp %>% left_join(cc %>% select(district, lpg_2019_rural), by = "district")
  print(ggplot(raw_map) + geom_sf(aes(fill = lpg_2019_rural), color = NA) +
          scale_fill_viridis_c(na.value = "grey88", limits = c(0, 1)) +
          theme_void(base_size = 11) +
          labs(title = "NFHS-5 raw rural primary-LPG (2019)", fill = "share"))
  if ("lpg_2019_bayes" %in% names(cc)) {
    b_map <- shp %>% left_join(cc %>% select(district, lpg_2019_bayes), by = "district")
    print(ggplot(b_map) + geom_sf(aes(fill = lpg_2019_bayes), color = NA) +
            scale_fill_viridis_c(na.value = "grey88", limits = c(0, 1)) +
            theme_void(base_size = 11) +
            labs(title = "NFHS-5 corrected (Bayes) rural primary-LPG (2019)", fill = "share"))
    # correction shift, diverging
    sh <- shp %>% left_join(cc %>% transmute(district,
                    shift = lpg_2019_bayes - lpg_2019_rural), by = "district")
    lim <- suppressWarnings(max(abs(range(sh$shift, na.rm = TRUE))))
    if (!is.finite(lim) || lim == 0) lim <- 0.1
    print(ggplot(sh) + geom_sf(aes(fill = shift), color = NA) +
            scale_fill_gradient2(low = "#2166AC", mid = "grey96", high = "#B2182B",
                                 midpoint = 0, limits = c(-lim, lim), na.value = "grey88") +
            theme_void(base_size = 11) +
            labs(title = "Correction shift (corrected - raw), NFHS-5", fill = "delta"))
  }
  if (!is.null(ire_rural_sum))
    print(sc(cc %>% left_join(ire_d %>% select(district, ires_mainlpg_rural), by = "district"),
             "lpg_2019_rural", "ires_mainlpg_rural",
             "NFHS-5 raw rural", "IRES rural (reference)",
             "The calibration target: NFHS-5 raw vs IRES rural"))
}
dev.off()

# ---- Household-level distributions -------------------------------------------
message("Building household distribution diagnostics ...")
pdf(file.path(diag_dir, "distributions.pdf"), width = 8, height = 5)
dist_hist <- function(hh, col, lab, xlab, xmax = NA, users_only = FALSE) {
  if (!col %in% names(hh)) return(invisible())
  if (users_only && "uselpg" %in% names(hh)) hh <- hh[which(hh$uselpg == 1), , drop = FALSE]
  d <- tibble(x = suppressWarnings(as.numeric(hh[[col]]))) %>% filter(is.finite(x))
  if (!is.na(xmax)) d <- d %>% filter(x <= xmax)
  if (nrow(d) == 0) return(invisible())
  print(ggplot(d, aes(x)) + geom_histogram(bins = 50, fill = "#2166AC") +
          theme_bw() + labs(title = lab, x = xlab, y = "households"))
}
dist_hist(acc_w1,  "lpg_kg_yr", "ACCESS W1: LPG kg/yr (users)", "kg/yr", 400, users_only = TRUE)
dist_hist(ires_hh, "lpg_kg_yr", "IRES: LPG kg/yr (users)", "kg/yr", 400, users_only = TRUE)
dist_hist(acc_w1, "month_exp", "ACCESS W1: monthly expenditure", "Rs/month", 50000)
dist_hist(ires_hh, "month_exp", "IRES: monthly expenditure", "Rs/month", 50000)
dist_hist(acc_w1, "hhsize", "ACCESS W1: household size", "members", 20)
dist_hist(ires_hh, "hhsize", "IRES: household size", "members", 20)
dev.off()

# ---- Recode sanity: weighted frequency tables --------------------------------
message("Writing recode sanity tables ...")
wtab <- function(hh, col, wcol) {
  if (!col %in% names(hh) || !wcol %in% names(hh)) return(NULL)
  hh %>% filter(!is.na(.data[[col]]), !is.na(.data[[wcol]])) %>%
    count(value = .data[[col]], wt = .data[[wcol]], name = "wn") %>%
    mutate(share = round(wn / sum(wn), 3)) %>% select(value, share)
}
sink(file.path(diag_dir, "recode_summary.txt"))
cat("== Weighted category shares (recode verification) ==\n")
for (cc in c("caste", "religion", "use3cat")) {
  cat("\n--- ACCESS W1:", cc, "---\n")
  print(as.data.frame(wtab(acc_w1, cc, "weights")), row.names = FALSE)
  cat("\n--- IRES:", cc, "---\n")
  print(as.data.frame(wtab(ires_hh, cc, "wt")), row.names = FALSE)
}
cat("\n== Missingness (% NA) in key household variables ==\n")
miss <- function(hh, lab) {
  vars <- intersect(c("main_fuel_lpg","use3cat","caste","religion","electricity",
                      "bplaay","bpl","edu_low","edu_primary_or_less","lpg_kg_yr",
                      "month_exp","rural"), names(hh))
  cat("\n---", lab, "---\n")
  for (v in vars)
    cat(sprintf("  %-22s %5.2f%%\n", v, 100 * mean(is.na(hh[[v]]))))
}
miss(acc_w1, "ACCESS W1"); miss(ires_hh, "IRES")
sink()

## ---- CHECKS ------------------------------------------------------------------
chk_header("19_ires_access_atlas")
for (f in c("atlas_ACCESS.pdf","atlas_IRES.pdf","qc_verification.pdf",
            "distributions.pdf","recode_summary.txt"))
  chk("19", paste0(f, " written"), file.exists(file.path(diag_dir, f)))

message("\n19_ires_access_atlas.R done -> ", diag_dir, "/ (atlas_ACCESS.pdf, ",
        "atlas_IRES.pdf, qc_verification.pdf, distributions.pdf, recode_summary.txt)")
