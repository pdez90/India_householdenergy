# ==============================================================================
# H3_env_covariates.R   (STANDALONE -- does not source 00_config.R)
#
# Builds district-level AMBIENT environmental covariates on the SAME NFHS-4
# district geography (dist_code) used by 01-07 and H1/H2, so they join cleanly
# to health_district_wide.rds and corrected_nfhs_districts.rds. Produces the
# change-on-change covariates that H2_health_models.R looks for in
#   df_wide_health.rds  ->  change_pm, temp_change, rh_change,
#                           weighted_temperature_change, droughtchange, region.
#   (weighted_temperature_change = 0.7*tmax + 0.3*tmin, the variable the reference
#    code mislabelled a "heat index"; it contains no humidity term and is NOT used
#    in any primary adjustment set -- see the naming note in section 3.)
#
# Two data sources, BOTH netCDF, NEITHER needs rgee / Earth Engine:
#   * PM2.5  : van Donkelaar / ACAG V5GL03 HybridPM25 annual grids
#              (GWRPM25), read with ncdf4 exactly as in India_lpg2.Rmd.
#   * Temp + RH + drought : TerraClimate annual netCDFs (tmmx, tmmn, vap, vpd,
#              pdsi). Global monthly files; we crop to India, take the annual
#              mean, then the district mean. RH is DERIVED, not in the reference:
#              RH% = 100 * vap / (vap + vpd)     [vap, vpd both in kPa]
#
# Exposure windows (change = NFHS-5 period - NFHS-4 period), matching the
# 5-year averaging in India_lpg2.Rmd (with the pm_2015/pm_2019 labels put the
# right way round -- in the .Rmd pm_2015 averaged 2016-2020 and pm_2019 averaged
# 2011-2015, which is reversed):
#   NFHS-4 window (P4): 2011-2015     NFHS-5 window (P5): 2016-2020
#
# Standalone: run on its own with
#   Rscript H3_env_covariates.R
#
# Output: df_wide_health.rds / .csv  (one row per dist_code district)
# ==============================================================================

## ---- CONFIG (edit paths to match your machine) ------------------------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers

# NFHS-4 district shapefile (the dist_code geography used throughout 01-07/H1).
path_districts_shp <- "/Users/priyanka/Downloads/DHS_India/district_nfhs_shapefile/nfhs_data.shp"

# van Donkelaar / ACAG annual PM2.5 netCDFs, named
#   V5GL03.HybridPM25.Asia.<YYYY>01-<YYYY>12.nc   (same folder as India_lpg2.Rmd)
dir_pm25 <- "/Users/priyanka/Downloads/DHS_India/India2019/Annual"

# TerraClimate annual netCDFs, named TerraClimate_<var>_<YYYY>.nc.
# If a file is missing locally and TERRACLIMATE_DOWNLOAD = TRUE, it is fetched
# from the Climatology Lab THREDDS server (needs internet, ~a few hundred MB
# per variable-year). Set FALSE and pre-download if you prefer.
dir_tc <- "/Users/priyanka/Downloads/DHS_India/TerraClimate"
TERRACLIMATE_DOWNLOAD <- TRUE
TERRACLIMATE_BASE <- "https://climate.northwestknowledge.net/TERRACLIMATE-DATA"

# Exposure windows
P4 <- 2011:2015    # NFHS-4 period
P5 <- 2016:2020    # NFHS-5 period
ALL_YEARS <- sort(unique(c(P4, P5)))

# India bounding box (crop TerraClimate global grids before extraction)
INDIA_BBOX <- c(xmin = 67, xmax = 98, ymin = 6, ymax = 38)

## ---- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse); library(sf); library(terra)
  library(exactextractr); library(ncdf4)
})
options(stringsAsFactors = FALSE)
options(timeout = max(3600, getOption("timeout")))   # big netCDF downloads
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_tc,  showWarnings = FALSE, recursive = TRUE)

## ---- District polygons (dist_code geography) ---------------------------------
message("Reading district shapefile ...")
shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  filter(!st_is_empty(.)) %>% st_make_valid() %>%
  mutate(district = as.character(as.numeric(dist_code)))
shp_ll <- st_transform(shp, 4326)          # lon/lat for raster extraction

## ============================================================================
## 1. PM2.5  (van Donkelaar / ACAG, GWRPM25)  -- no rgee
## ============================================================================
message("\n== PM2.5 (ACAG V5GL03) ==")
# Orientation/units audit. A plausible national mean does NOT rule out a flipped,
# transposed or shifted grid, so the first successfully-read year records the
# actual geometry decisions and they are checked explicitly at the end.
pm_geom <- NULL
extract_pm <- function(year) {
  f <- file.path(dir_pm25,
                 sprintf("V5GL03.HybridPM25.Asia.%d01-%d12.nc", year, year))
  if (!file.exists(f)) { message("  PM2.5 ", year, ": file missing, skipped: ", f)
                         return(NULL) }
  nc  <- nc_open(f)
  lon <- ncvar_get(nc, "lon"); lat <- ncvar_get(nc, "lat")
  z   <- ncvar_get(nc, "GWRPM25")
  zunits <- tryCatch(ncatt_get(nc, "GWRPM25", "units")$value, error = function(e) NA)
  nc_close(nc)
  lon_asc <- !is.unsorted(lon); lat_asc <- !is.unsorted(lat)
  # z arrives as [lon, lat]; terra wants [row = lat, col = lon] with row 1 = NORTH,
  # so we transpose and then flip vertically (ncdf lat is ascending = south-first).
  r <- terra::rast(t(z), extent = terra::ext(min(lon), max(lon),
                                             min(lat), max(lat)),
                   crs = "EPSG:4326")
  r <- terra::flip(r, direction = "vertical")
  vals <- exact_extract(r, shp_ll, "mean")
  if (is.null(pm_geom)) {
    pm_geom <<- list(
      year = year, dim_z = dim(z), n_lon = length(lon), n_lat = length(lat),
      lon_ascending = lon_asc, lat_ascending = lat_asc,
      transpose_applied = TRUE, vflip_applied = TRUE,
      units = zunits,
      extent = as.vector(terra::ext(r)),
      grid_res_lon = signif(diff(range(lon)) / (length(lon) - 1), 4),
      grid_res_lat = signif(diff(range(lat)) / (length(lat) - 1), 4),
      dims_match = identical(dim(z), c(length(lon), length(lat))),
      district_coverage = mean(is.finite(vals)),
      val_range = range(vals[is.finite(vals)]))
    message("  [PM orientation audit] netCDF array dims = ",
            paste(dim(z), collapse = " x "), " (lon x lat = ", length(lon), " x ",
            length(lat), "); lon ascending = ", lon_asc, "; lat ascending = ", lat_asc,
            "; transpose applied = TRUE; vertical flip applied = TRUE")
    message("  [PM orientation audit] units = ", zunits,
            "; raster extent = [", paste(round(as.vector(terra::ext(r)), 3), collapse = ", "),
            "]; district extraction coverage = ",
            sprintf("%.1f%%", 100 * mean(is.finite(vals))))
    # North-south sanity: the Indo-Gangetic plain must be dirtier than the
    # peninsular south. A flipped raster reverses this ordering.
    ctr <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(shp_ll))))
    north <- mean(vals[is.finite(vals) & ctr[, 2] > 26], na.rm = TRUE)
    south <- mean(vals[is.finite(vals) & ctr[, 2] < 16], na.rm = TRUE)
    pm_geom$north_mean <<- north; pm_geom$south_mean <<- south
    message(sprintf("  [PM orientation audit] mean PM2.5 north of 26N = %.1f vs south of 16N = %.1f (north MUST be higher)",
                    north, south))
    # Diagnostic map for visual comparison against an external reference image.
    try({
      dir.create(file.path(dir_out, "diagnostics"), showWarnings = FALSE, recursive = TRUE)
      jpeg(file.path(dir_out, "diagnostics", sprintf("H3_pm25_orientation_%d.jpeg", year)),
           width = 1400, height = 1400, res = 200)
      terra::plot(terra::crop(r, terra::ext(INDIA_BBOX["xmin"], INDIA_BBOX["xmax"],
                                            INDIA_BBOX["ymin"], INDIA_BBOX["ymax"])),
                  main = sprintf("ACAG PM2.5 %d as read (check north-plain hotspot)", year))
      plot(sf::st_geometry(shp_ll), add = TRUE, border = "grey30", lwd = 0.2)
      dev.off()
    }, silent = TRUE)
  }
  tibble(district = shp_ll$district, !!paste0("pm_", year) := vals)
}
pm_list <- compact(map(ALL_YEARS, extract_pm))
pm_wide <- if (length(pm_list)) reduce(pm_list, full_join, by = "district") else
  tibble(district = shp_ll$district)

## ============================================================================
## 2. TerraClimate  (tmmx, tmmn, vap, vpd, pdsi)  -- no rgee
## ============================================================================
message("\n== TerraClimate (temp, humidity, drought) ==")
# The direct-download netCDF FILENAMES use tmax/tmin (and vap/vpd/pdsi), while
# the Google Earth Engine band names used in India_lpg2.Rmd are tmmx/tmmn. We
# key everything internally on the canonical stub (tmmx/tmmn) but try both
# remote spellings when downloading. Local files are stored under the stub.
TC_REMOTE_ALT <- list(tmmx = c("tmax", "tmmx"), tmmn = c("tmin", "tmmn"),
                      vap  = "vap", vpd = "vpd", pdsi = c("pdsi", "PDSI"))
tc_path <- function(var, year) file.path(dir_tc,
                                         sprintf("TerraClimate_%s_%d.nc", var, year))
tc_fetch <- function(var, year) {
  f <- tc_path(var, year)
  # accept an existing file only if it is a plausibly-complete netCDF (>1 MB; a
  # timed-out or 404 error-page download from libcurl leaves a tiny stub).
  if (file.exists(f) && file.info(f)$size > 1e6) return(f)
  if (file.exists(f)) file.remove(f)
  if (!TERRACLIMATE_DOWNLOAD) { message("  ", basename(f), " missing (download off)")
                                return(NA_character_) }
  alts <- TC_REMOTE_ALT[[var]]; if (is.null(alts)) alts <- var
  for (alt in alts) {
    url <- sprintf("%s/TerraClimate_%s_%d.nc", TERRACLIMATE_BASE, alt, year)
    for (attempt in 1:2) {
      message("  downloading TerraClimate_", alt, "_", year, ".nc (attempt ",
              attempt, ") ...")
      ok <- try(download.file(url, f, mode = "wb", quiet = TRUE), silent = TRUE)
      if (!inherits(ok, "try-error") && file.exists(f) && file.info(f)$size > 1e6)
        return(f)                                  # keep local name = stub
      if (file.exists(f)) file.remove(f)
    }
  }
  message("  FAILED to download ", basename(f), " (tried: ",
          paste(alts, collapse = "/"), ").")
  NA_character_
}
# Annual district mean of a TerraClimate variable (mean over 12 monthly layers).
# Units / scale-offset audit: terra applies netCDF scale_factor & add_offset
# automatically, so the logged ranges below are the values actually used. They
# are checked against physical expectations in the CHECKS block.
tc_audit <- list()
tc_year <- function(var, year) {
  f <- tc_fetch(var, year); if (is.na(f)) return(NULL)
  r <- try(terra::rast(f, subds = var), silent = TRUE)
  if (inherits(r, "try-error")) r <- terra::rast(f)      # single-var file
  # TerraClimate NetCDFs read through terra's fallback (non-multidim) path can
  # arrive with an EMPTY CRS, which makes exact_extract emit
  #   "Polygons transformed to raster CRS (EPSG:NA)"
  # and silently assume the polygons and the grid share an unnamed CRS. They do
  # in fact both sit on plain lon/lat WGS84 -- the TerraClimate grid is a
  # 1/24-degree global lon/lat grid -- so the extraction was correct, but it was
  # correct by luck rather than by declaration. Saying so explicitly turns an
  # unverifiable assumption into a stated one, and is a no-op when the reader
  # already supplied a CRS.
  if (is.na(terra::crs(r)) || !nzchar(terra::crs(r))) {
    terra::crs(r) <- "EPSG:4326"
    message("  [TerraClimate] ", var, " ", year,
            ": raster CRS was unset -> assigned EPSG:4326 ",
            "(TerraClimate ships on a global 1/24-deg WGS84 lon/lat grid).")
  }
  r <- terra::crop(r, terra::ext(INDIA_BBOX["xmin"], INDIA_BBOX["xmax"],
                                 INDIA_BBOX["ymin"], INDIA_BBOX["ymax"]))
  nlyr_in <- terra::nlyr(r)
  r <- terra::mean(r, na.rm = TRUE)                      # annual mean
  vals <- exact_extract(r, shp_ll, "mean")
  if (is.null(tc_audit[[var]])) {
    u <- tryCatch(terra::units(r)[1], error = function(e) NA_character_)
    tc_audit[[var]] <<- list(
      var = var, first_year = year, monthly_layers = nlyr_in, units = u,
      district_min = min(vals, na.rm = TRUE), district_max = max(vals, na.rm = TRUE),
      coverage = mean(is.finite(vals)))
    message(sprintf("  [TerraClimate audit] %-5s %d: %d monthly layers | units='%s' | district annual mean range [%.2f, %.2f] | coverage %.1f%%",
                    var, year, nlyr_in, ifelse(is.na(u) || !nzchar(u), "unset", u),
                    min(vals, na.rm = TRUE), max(vals, na.rm = TRUE),
                    100 * mean(is.finite(vals))))
  }
  tibble(district = shp_ll$district, !!paste0(var, "_", year) := vals)
}
tc_var_wide <- function(var) {
  lst <- compact(map(ALL_YEARS, ~tc_year(var, .x)))
  if (!length(lst)) { message("  ", var, ": no years available."); return(NULL) }
  reduce(lst, full_join, by = "district")
}
TC_VARS <- c("tmmx", "tmmn", "vap", "vpd", "pdsi")
tc_wide <- reduce(compact(map(TC_VARS, tc_var_wide)),
                  full_join, by = "district", .init = tibble(district = shp_ll$district))

## ============================================================================
## 3. Period means and change-on-change covariates
## ============================================================================
message("\n== Period averaging and change variables ==")
# helper: mean across the year-columns "<stub>_<yr>" for a set of years
period_mean <- function(df, stub, years) {
  cols <- paste0(stub, "_", years); cols <- cols[cols %in% names(df)]
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  rowMeans(as.matrix(df[cols]), na.rm = TRUE)
}
env <- tibble(district = shp_ll$district)

# --- PM2.5 ---
env$pm_p4 <- period_mean(pm_wide, "pm", P4)
env$pm_p5 <- period_mean(pm_wide, "pm", P5)
env$change_pm <- env$pm_p5 - env$pm_p4

# --- Temperature: the simple mean, and an asymmetrically weighted composite -----
tmmx_p4 <- period_mean(tc_wide, "tmmx", P4); tmmx_p5 <- period_mean(tc_wide, "tmmx", P5)
tmmn_p4 <- period_mean(tc_wide, "tmmn", P4); tmmn_p5 <- period_mean(tc_wide, "tmmn", P5)
env$temp_p4 <- (tmmx_p4 + tmmn_p4) / 2
env$temp_p5 <- (tmmx_p5 + tmmn_p5) / 2
env$temp_change <- env$temp_p5 - env$temp_p4
# NAMING. The India_lpg2 reference code called 0.7*tmmx + 0.3*tmmn a "heat index".
# It is NOT one: a heat index (apparent temperature) is a function of temperature
# AND humidity, whereas this is simply a max/min-weighted temperature average with
# no humidity term at all. Calling it a heat index would misdescribe the variable
# in the manuscript, so it is named for what it is: weighted_temperature_change.
# It is collinear with temp_change by construction (both are linear combinations
# of the same two monthly fields, r ~ 0.99), so the two must NEVER enter the same
# adjustment set -- H2/H4/H5 all adjust for temp_change only, and this column is
# retained purely for a clearly labelled alternative-specification sensitivity.
wt_p4 <- 0.7 * tmmx_p4 + 0.3 * tmmn_p4
wt_p5 <- 0.7 * tmmx_p5 + 0.3 * tmmn_p5
env$weighted_temperature_change <- wt_p5 - wt_p4

# --- Relative humidity (DERIVED: RH% = 100 * vap / (vap + vpd)) ---
vap_p4 <- period_mean(tc_wide, "vap", P4); vap_p5 <- period_mean(tc_wide, "vap", P5)
vpd_p4 <- period_mean(tc_wide, "vpd", P4); vpd_p5 <- period_mean(tc_wide, "vpd", P5)
env$rh_p4 <- 100 * vap_p4 / (vap_p4 + vpd_p4)
env$rh_p5 <- 100 * vap_p5 / (vap_p5 + vpd_p5)
env$rh_change <- env$rh_p5 - env$rh_p4

# --- Drought (PDSI) ---
env$pdsi_p4 <- period_mean(tc_wide, "pdsi", P4)
env$pdsi_p5 <- period_mean(tc_wide, "pdsi", P5)
env$droughtchange <- env$pdsi_p5 - env$pdsi_p4

## ============================================================================
## 4. Region fixed effect (from state name)
## ============================================================================
region_of <- function(s) {
  s <- tolower(as.character(s))
  dplyr::case_when(
    grepl("jammu|kashmir|ladakh|himachal|punjab|uttarakhand|uttaranchal|haryana|delhi|chandigarh|rajasthan", s) ~ "North",
    grepl("uttar pradesh|madhya pradesh|chhat|chhattisgarh|chattisgarh",   s) ~ "Central",
    grepl("bihar|jharkhand|odisha|orissa|west bengal|sikkim",              s) ~ "East",
    grepl("arunachal|assam|manipur|meghalaya|mizoram|nagaland|tripura",    s) ~ "Northeast",
    grepl("gujarat|maharashtra|goa|dadra|daman|diu",                       s) ~ "West",
    grepl("andhra|telangana|karnataka|kerala|tamil|puducherry|pondicherry|lakshadweep|andaman", s) ~ "South",
    TRUE ~ NA_character_)
}
region_tbl <- shp %>% st_drop_geometry() %>%
  transmute(district, region = region_of(state_name)) %>%
  distinct(district, .keep_all = TRUE)
env <- left_join(env, region_tbl, by = "district")

## ---- Save --------------------------------------------------------------------
df_wide_health <- env %>% distinct(district, .keep_all = TRUE)
saveRDS(df_wide_health, file.path(dir_out, "df_wide_health.rds"))
write_csv(df_wide_health, file.path(dir_out, "df_wide_health.csv"))

message("\nH3 done: ", nrow(df_wide_health), " districts -> df_wide_health.rds")
message("Change-covariate summary (district-level):")
print(summary(df_wide_health %>%
                select(change_pm, temp_change, rh_change,
                       weighted_temperature_change, droughtchange)))
message(sprintf(
  "temp_change vs weighted_temperature_change correlation: r = %.4f  (collinear by construction; never adjust for both)",
  suppressWarnings(cor(df_wide_health$temp_change,
                       df_wide_health$weighted_temperature_change,
                       use = "complete.obs"))))
message("Region counts:")
print(table(df_wide_health$region, useNA = "ifany"))

## ---- CHECKS ------------------------------------------------------------------
chk_header("H3_env_covariates")
chk("H3", "df_wide_health.rds written", file.exists(file.path(dir_out, "df_wide_health.rds")))
chk("H3", "ambient change covariates present",
    chk_has_cols(df_wide_health, c("change_pm","temp_change","rh_change","droughtchange")))
chk("H3", "region present for fixed effects",
    "region" %in% names(df_wide_health) && any(!is.na(df_wide_health$region)))
chk_warn("H3", "PM2.5 change in a sane range (ug/m3)",
    chk_in_range(df_wide_health$change_pm, -100, 100), chk_rng(df_wide_health$change_pm))

# --- RH: LEVELS and CHANGE are different quantities and need different tests ---
# (the previous single check applied a [0,100] bound to rh_change, which negative
#  changes trivially satisfy or violate for reasons unrelated to RH validity).
chk("H3", "RH LEVEL (NFHS-4 window) within [0,100]",
    chk_in_range(df_wide_health$rh_p4, 0, 100), chk_rng(df_wide_health$rh_p4))
chk("H3", "RH LEVEL (NFHS-5 window) within [0,100]",
    chk_in_range(df_wide_health$rh_p5, 0, 100), chk_rng(df_wide_health$rh_p5))
chk_warn("H3", "RH CHANGE within a plausible band (+/-30 pp)",
    chk_in_range(df_wide_health$rh_change, -30, 30), chk_rng(df_wide_health$rh_change))
# Band widened from [0, 45] to [-20, 45]. The old lower bound was wrong for what
# this variable actually is: a period mean of a TerraClimate temperature field
# over a district polygon. Himalayan districts (Leh, Lahaul & Spiti, Kargil,
# Tawang) have genuinely sub-zero period means, so the run of 2026-07-30 WARNed
# at an observed minimum of -5.46 C -- a correct value flagged as if it were an
# error, which is the kind of warning that trains a reader to ignore warnings.
# -20 C is below the coldest plausible Indian district mean and 45 C is above the
# hottest, so the band still catches a genuine unit or scaling error (e.g. Kelvin
# left unconverted, which would land near 300).
chk_warn("H3", "temperature LEVELS physically plausible (-20 to 45 C)",
    chk_in_range(c(df_wide_health$temp_p4, df_wide_health$temp_p5), -20, 45),
    chk_rng(c(df_wide_health$temp_p4, df_wide_health$temp_p5)))
chk_warn("H3", "PDSI LEVELS within the PDSI scale (+/-12)",
    chk_in_range(c(df_wide_health$pdsi_p4, df_wide_health$pdsi_p5), -12, 12),
    chk_rng(c(df_wide_health$pdsi_p4, df_wide_health$pdsi_p5)))

# --- Raster orientation / units audit (a plausible mean does not prove geometry) ---
if (!is.null(pm_geom)) {
  chk("H3", "PM netCDF array dims match (lon x lat)", isTRUE(pm_geom$dims_match),
      sprintf("array %s vs lon=%d lat=%d", paste(pm_geom$dim_z, collapse = "x"),
              pm_geom$n_lon, pm_geom$n_lat))
  chk("H3", "PM transpose + vertical flip applied",
      isTRUE(pm_geom$transpose_applied) && isTRUE(pm_geom$vflip_applied),
      sprintf("lon ascending=%s, lat ascending=%s, transpose=TRUE, vflip=TRUE",
              pm_geom$lon_ascending, pm_geom$lat_ascending))
  chk("H3", "PM raster NOT vertically flipped (north plain dirtier than south)",
      is.finite(pm_geom$north_mean) && is.finite(pm_geom$south_mean) &&
        pm_geom$north_mean > pm_geom$south_mean,
      sprintf("mean north of 26N = %.1f vs south of 16N = %.1f",
              pm_geom$north_mean, pm_geom$south_mean))
  chk("H3", "PM district extraction coverage >= 99%",
      isTRUE(pm_geom$district_coverage >= 0.99),
      sprintf("%.2f%% of districts extracted; value range [%.1f, %.1f]",
              100 * pm_geom$district_coverage, pm_geom$val_range[1], pm_geom$val_range[2]))
  chk_warn("H3", "PM raster extent covers India bbox",
      pm_geom$extent[1] <= INDIA_BBOX["xmin"] && pm_geom$extent[2] >= INDIA_BBOX["xmax"] &&
      pm_geom$extent[3] <= INDIA_BBOX["ymin"] && pm_geom$extent[4] >= INDIA_BBOX["ymax"],
      sprintf("extent [%s]", paste(round(pm_geom$extent, 2), collapse = ", ")))
  message("PM2.5 orientation diagnostic map: diagnostics/H3_pm25_orientation_",
          pm_geom$year, ".jpeg -- compare against a published India PM2.5 map.")
} else {
  chk_warn("H3", "PM orientation audit recorded", FALSE, "no PM year was read")
}
for (v in names(tc_audit)) {
  a <- tc_audit[[v]]
  chk_warn("H3", paste0("TerraClimate '", v, "' units/scale audit logged"),
      isTRUE(a$monthly_layers == 12) && isTRUE(a$coverage >= 0.99),
      sprintf("%d monthly layers, units='%s', district range [%.2f, %.2f], coverage %.1f%%",
              a$monthly_layers, ifelse(is.na(a$units) || !nzchar(a$units), "unset", a$units),
              a$district_min, a$district_max, 100 * a$coverage))
}
saveRDS(list(pm = pm_geom, terraclimate = tc_audit),
        file.path(dir_out, "diagnostics", "H3_raster_audit.rds"))
