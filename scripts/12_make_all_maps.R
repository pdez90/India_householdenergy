# ==============================================================================
# 12_make_all_maps.R   (STANDALONE -- does not source 00_config.R)
#
# A district-level "atlas": one choropleth map for every quantity the paper
# discusses, so each number in the text has a figure a reader can look at.
# Covers, on the common NFHS-4 district geography:
#   * NFHS-4 / NFHS-5 raw prevalences   -- primary LPG, and the SES covariates
#       (poverty, low maternal education, electricity, Muslim share, improved
#       sanitation, improved water) and child mortality (neonatal/infant),
#       each for 2015 and 2019 and as the 2019-2015 change.
#   * Corrected LPG surfaces            -- regression-calibrated and Bayesian,
#       2015 and 2019.
#   * Predicted fuel-use composition    -- exclusive-LPG share, fuel-stacking
#       share, any-solid-fuel share, and LPG consumption (kg/yr), 2015 / 2019 / change.
#   * Reference-survey prevalences      -- ACCESS Wave 1 (& Wave 2) and IRES:
#       primary LPG, stacking, exclusive LPG, LPG consumption.
#   * Adult outcomes                    -- hypertension and diabetes, 2015 / 2019 / change.
#
# Each quantity is saved as an individual JPEG in <dir_out>/maps/atlas/, and
# thematically grouped multi-panel composites (SES, exposure, reference surveys,
# health, and change maps) are saved in <dir_out>/maps/atlas_panels/ for the paper.
#
# Standalone: reads only files already produced by the pipeline plus the NFHS-4
# district shapefile. Run on its own with:   Rscript 12_make_all_maps.R
#
# Inputs (each block is skipped with a message if its file is absent):
#   health_district_wide.csv                    <- H1_prep_mortality.R
#   district_exposure_proxy.csv                 <- 06_stacking_prediction.R (corrected LPG)
#   district_exposure_proxy_consistent.csv      <- 06_stacking_prediction.R (composition)
#   access_districts.rds, ires_districts.rds    <- 02_prep_access.R / 03_prep_ires.R
#   si_adult_health_prevalence.rds              <- H4_si_adult_health.R
#   NFHS-4 district shapefile (path below)
# ==============================================================================

## ---- CONFIG (edit paths to match your machine) ------------------------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
path_districts_shp <- "/Users/priyanka/Downloads/DHS_India/district_nfhs_shapefile/nfhs_data.shp"

# Optional fallback geometry (a GeoPackage carrying dist_code) if the shapefile
# above is not found; the pipeline writes wave1_nfhs4.gpkg into dir_out.
path_geom_fallback <- file.path(dir_out, "wave1_nfhs4.gpkg")

dir_atlas  <- file.path(dir_out, "maps", "atlas")
dir_panels <- file.path(dir_out, "maps", "atlas_panels")
dir.create(dir_atlas,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_panels, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(tidyverse); library(sf)
})
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (!has_patchwork)
  message("NOTE: 'patchwork' not installed -- individual maps will be written ",
          "but the multi-panel composites will be skipped. install.packages('patchwork') to enable them.")

## ---- Geometry ----------------------------------------------------------------
read_geom <- function() {
  if (file.exists(path_districts_shp)) {
    g <- st_read(path_districts_shp, quiet = TRUE)
  } else if (file.exists(path_geom_fallback)) {
    message("Shapefile not found; using fallback geometry ", path_geom_fallback)
    g <- st_read(path_geom_fallback, quiet = TRUE)
  } else stop("No district geometry found. Set path_districts_shp or provide ",
              path_geom_fallback)
  if (!"dist_code" %in% names(g))
    stop("Geometry has no 'dist_code' column; columns are: ",
         paste(names(g), collapse = ", "))
  g %>% mutate(district = as.character(as.numeric(dist_code))) %>%
    filter(!st_is_empty(.)) %>% st_make_valid()
}
shp <- read_geom()
message("Geometry loaded: ", nrow(shp), " district polygons.")

## ---- Load the district-level source tables (each guarded) --------------------
rd_csv <- function(f) { p <- file.path(dir_out, f)
  if (file.exists(p)) suppressMessages(readr::read_csv(p, show_col_types = FALSE)) else NULL }
rd_rds <- function(f) { p <- file.path(dir_out, f)
  if (file.exists(p)) readRDS(p) else NULL }

key <- function(df) if (is.null(df)) NULL else
  dplyr::mutate(df, district = as.character(as.numeric(district)))

health <- key(rd_csv("health_district_wide.csv"))
corr   <- key(rd_csv("district_exposure_proxy.csv"))
comp   <- key(rd_csv("district_exposure_proxy_consistent.csv"))
access <- key(rd_rds("access_districts.rds"))
ires   <- key(rd_rds("ires_districts.rds"))
adult  <- key(rd_rds("si_adult_health_prevalence.rds"))

# district_exposure_proxy.csv can carry one row per survey; keep the corrected
# LPG columns (identical across rows within a district) once per district.
if (!is.null(corr))
  corr <- corr %>% distinct(district, .keep_all = TRUE)

## ---- The atlas: one row per quantity to map ----------------------------------
# scale100 = TRUE for 0-1 proportions shown as percentages; FALSE for kg/yr.
# type = "seq" (viridis) for levels, "div" (blue-white-red at 0) for changes.
a <- function(src, var, label, group, unit = "%", type = "seq", scale100 = TRUE)
  tibble(src = src, var = var, label = label, group = group,
         unit = unit, type = type, scale100 = scale100)

atlas <- bind_rows(
  ## --- NFHS raw primary LPG + SES covariates, both rounds + change -----------
  pmap_dfr(list(
    v  = c("lpg","poor","mother_low_edu","electricity","muslim",
           "improved_sanitation","improved_water"),
    lb = c("Primary LPG","Poverty (wealth Q1)","Low maternal education",
           "Household electricity","Muslim share","Improved sanitation",
           "Improved water")),
    function(v, lb) bind_rows(
      a("health", paste0(v,"_2015"),      paste0("NFHS-4 (2015): ", lb), "SES_2015"),
      a("health", paste0(v,"_2019"),      paste0("NFHS-5 (2019): ", lb), "SES_2019"),
      a("health", paste0("change_",v),    paste0("Change 2015->2019: ", lb),
        "SES_change", unit = "pp", type = "div", scale100 = FALSE))),

  ## --- Child mortality, both rounds + change --------------------------------
  pmap_dfr(list(
    v  = c("neonatal_death","infant_death"),
    lb = c("Neonatal mortality","Infant mortality")),
    function(v, lb) bind_rows(
      a("health", paste0(v,"_2015"),   paste0("NFHS-4 (2015): ", lb), "MORT_2015",
        unit = "per 100 births"),
      a("health", paste0(v,"_2019"),   paste0("NFHS-5 (2019): ", lb), "MORT_2019",
        unit = "per 100 births"),
      a("health", paste0("change_",v), paste0("Change 2015->2019: ", lb),
        "MORT_change", unit = "per 100 births", type = "div", scale100 = FALSE))),

  ## --- Corrected LPG surfaces ------------------------------------------------
  a("corr", "lpg_2015_rc",    "NFHS-4 corrected (regression calibration): primary LPG", "EXP_corrected"),
  a("corr", "lpg_2019_rc",    "NFHS-5 corrected (regression calibration): primary LPG", "EXP_corrected"),
  a("corr", "lpg_2015_bayes", "NFHS-4 corrected (Bayesian meas.-error): primary LPG",   "EXP_corrected"),
  a("corr", "lpg_2019_bayes", "NFHS-5 corrected (Bayesian meas.-error): primary LPG",   "EXP_corrected"),

  ## --- Predicted fuel-use composition ---------------------------------------
  pmap_dfr(list(
    v  = c("excl_lpg","stacking","any_solid","kg"),
    lb = c("LPG, no solid fuel (share)","Fuel-stacking share","Any solid-fuel burning",
           "Predicted LPG consumption"),
    un = c("%","%","%","kg/yr"),
    s1 = c(TRUE, TRUE, TRUE, FALSE)),
    function(v, lb, un, s1) bind_rows(
      a("comp", paste0(v,"_2015"),   paste0("NFHS-4 (2015): ", lb), "COMP_2015", unit = un, scale100 = s1),
      a("comp", paste0(v,"_2019"),   paste0("NFHS-5 (2019): ", lb), "COMP_2019", unit = un, scale100 = s1),
      a("comp", paste0("change_",v), paste0("Change 2015->2019: ", lb), "COMP_change",
        unit = if (s1) "pp" else "kg/yr", type = "div", scale100 = FALSE))),

  ## --- ACCESS reference-survey prevalences ----------------------------------
  a("access", "access_w1_mainlpg",  "ACCESS Wave 1 (2015): primary LPG",        "REF_access"),
  a("access", "access_w2_mainlpg",  "ACCESS Wave 2 (2018): primary LPG",        "REF_access"),
  a("access", "access_w1_stack",    "ACCESS Wave 1 (2015): fuel stacking",      "REF_access"),
  a("access", "access_w1_excl_lpg", "ACCESS Wave 1 (2015): exclusive LPG",      "REF_access"),
  a("access", "access_w1_lpg_kg_yr","ACCESS Wave 1 (2015): LPG consumption",    "REF_access", unit = "kg/yr", scale100 = FALSE),

  ## --- IRES reference-survey prevalences ------------------------------------
  a("ires", "ires_mainlpg",       "IRES (2019-20): primary LPG (all households)", "REF_ires"),
  a("ires", "ires_mainlpg_rural", "IRES (2019-20): primary LPG (rural)",          "REF_ires"),
  a("ires", "ires_stack",         "IRES (2019-20): fuel stacking",                "REF_ires"),
  a("ires", "ires_excl_lpg",      "IRES (2019-20): exclusive LPG",                "REF_ires"),
  a("ires", "ires_lpg_kg_yr",     "IRES (2019-20): LPG consumption",              "REF_ires", unit = "kg/yr", scale100 = FALSE),

  ## --- Adult cardiometabolic outcomes ---------------------------------------
  pmap_dfr(list(
    v  = c("hypertension","diabetes","diabetes_sr"),
    lb = c("Hypertension (measured)","Diabetes (measured or self-report)",
           "Diabetes (self-report)")),
    function(v, lb) bind_rows(
      a("adult", paste0(v,"_2015"),   paste0("NFHS-4 (2015): ", lb), "ADULT_2015"),
      a("adult", paste0(v,"_2019"),   paste0("NFHS-5 (2019): ", lb), "ADULT_2019"),
      a("adult", paste0("change_",v), paste0("Change 2015->2019: ", lb),
        "ADULT_change", unit = "pp", type = "div", scale100 = FALSE)))
)

## ---- Keep only quantities whose source is present and column exists ---------
src_tbl <- list(health = health, corr = corr, comp = comp,
                access = access, ires = ires, adult = adult)
have <- map_lgl(atlas$src, ~ !is.null(src_tbl[[.x]]))
atlas <- atlas[have, ]
col_ok <- pmap_lgl(atlas, function(src, var, ...) var %in% names(src_tbl[[src]]))
dropped <- atlas[!col_ok, ]
if (nrow(dropped))
  message("Skipping ", nrow(dropped), " quantities not found in their source: ",
          paste(unique(dropped$var), collapse = ", "))
atlas <- atlas[col_ok, ]
message("Atlas: ", nrow(atlas), " district quantities to map.")

## ---- One map ----------------------------------------------------------------
safe_name <- function(s) gsub("[^A-Za-z0-9]+", "_", s)

build_one <- function(src, var, label, group, unit, type, scale100) {
  d <- src_tbl[[src]] %>% transmute(district, value = .data[[var]])
  if (isTRUE(scale100)) d$value <- 100 * d$value
  dat <- shp %>% left_join(d, by = "district")
  g <- ggplot(dat) +
    geom_sf(aes(fill = value), color = "grey80", linewidth = 0.03) +
    theme_void(base_size = 10) +
    labs(title = label, fill = unit) +
    theme(plot.title = element_text(size = 9, face = "bold"),
          legend.key.width = unit(0.35, "cm"))
  if (type == "div") {
    lim <- suppressWarnings(max(abs(dat$value), na.rm = TRUE))
    if (!is.finite(lim) || lim == 0) lim <- 1   # all-NA/constant -> safe finite limit
    g <- g + scale_fill_gradient2(low = "#2166AC", mid = "grey95", high = "#B2182B",
                                  midpoint = 0, limits = c(-lim, lim), na.value = "grey88")
  } else {
    g <- g + scale_fill_viridis_c(na.value = "grey88")
  }
  g
}

## ---- Write every individual map ---------------------------------------------
plots <- vector("list", nrow(atlas)); names(plots) <- atlas$var
for (i in seq_len(nrow(atlas))) {
  row <- atlas[i, ]
  p <- tryCatch(build_one(row$src, row$var, row$label, row$group, row$unit,
                          row$type, row$scale100),
                error = function(e) { message("  ! ", row$var, ": ", conditionMessage(e)); NULL })
  if (is.null(p)) next
  plots[[i]] <- p
  fn <- file.path(dir_atlas, paste0(safe_name(row$group), "__", safe_name(row$var), ".jpeg"))
  ggsave(fn, p, width = 4.2, height = 4.4, dpi = 200)
}
message("Wrote ", sum(!map_lgl(plots, is.null)), " individual maps to ", dir_atlas)

## ---- Thematic multi-panel composites ----------------------------------------
if (has_patchwork) {
  library(patchwork)
  panel_defs <- list(
    SES_levels_2019   = atlas$group == "SES_2019",
    SES_levels_2015   = atlas$group == "SES_2015",
    SES_change        = atlas$group == "SES_change",
    exposure_corrected= atlas$group == "EXP_corrected",
    composition_2019  = atlas$group == "COMP_2019",
    composition_change= atlas$group == "COMP_change",
    reference_ACCESS  = atlas$group == "REF_access",
    reference_IRES    = atlas$group == "REF_ires",
    mortality_levels  = atlas$group %in% c("MORT_2015","MORT_2019"),
    mortality_change  = atlas$group == "MORT_change",
    adult_levels      = atlas$group %in% c("ADULT_2015","ADULT_2019"),
    adult_change      = atlas$group == "ADULT_change"
  )
  for (nm in names(panel_defs)) {
    idx <- which(panel_defs[[nm]] & !map_lgl(plots, is.null))
    if (!length(idx)) next
    ncol <- if (length(idx) <= 3) length(idx) else if (length(idx) <= 8) 3 else 4
    comp <- wrap_plots(plots[idx], ncol = ncol)
    nr <- ceiling(length(idx) / ncol)
    ggsave(file.path(dir_panels, paste0(nm, ".jpeg")), comp,
           width = 4.0 * ncol, height = 4.2 * nr, dpi = 200, limitsize = FALSE)
  }
  message("Wrote ", length(panel_defs), " thematic composite panels to ", dir_panels)
}

## ---- CHECKS ------------------------------------------------------------------
chk_header("12_make_all_maps")
chk("12", "individual atlas maps written", dir.exists(dir_atlas) &&
    length(list.files(dir_atlas, pattern = "\\.(jpe?g|png)$")) > 0,
    paste0(length(list.files(dir_atlas, pattern = "\\.(jpe?g|png)$")), " files"))
chk("12", "composite atlas panels written", dir.exists(dir_panels) &&
    length(list.files(dir_panels, pattern = "\\.(jpe?g|png)$")) > 0,
    paste0(length(list.files(dir_panels, pattern = "\\.(jpe?g|png)$")), " files"))

message("\n12_make_all_maps.R done.\n",
        "  Individual maps: ", dir_atlas, "\n",
        "  Composite panels: ", dir_panels)
