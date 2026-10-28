# ==============================================================================
# build_district_boundaries.R -- the 2011 district boundary file used for maps
# and for the few polygon-based fallbacks (H3 environmental covariates for
# districts with no located cluster).
#
# Why. The boundary file the pipeline used until release .8
# (district_nfhs_shapefile/nfhs_data.shp) carries correct district codes and
# names, but its shapes do not match its labels: overlaid on it, only 70% of
# NFHS-4 clusters fall inside the polygon of their own DHS district, and about
# 36 districts' clusters sit wholly in another district's polygon
# (30_crosswalk_validation.R, part D). Overlaid on the 2011 Population Census
# district polygons published in SHRUG 2.2 (Development Data Lab; Asher, Lunt,
# Matsuura and Novosad 2021), 98% do, and the misses lie within DHS's
# coordinate-displacement distances.
#
# What. SHRUG geometry, keyed by pc11_d_id (= the Census 2011 district code =
# NFHS-4 DHSREGCO), with the old file's attribute table joined by code, so every
# downstream script that reads dist_code / dist_name / state_name / dist_cod_1
# works unchanged. Written in the old file's CRS (UTM zone 44N) so distance-based
# steps (the app's simplification tolerance) behave as before. SHRUG polygon 0
# (area outside the 640 districts) keeps the old background row's attributes.
#
# Licence. SHRUG data are CC BY-NC-SA 4.0. The output stays under DHS_DIR (not
# redistributed); anything published from it (the app's districts.geojson)
# carries the SHRUG attribution and licence.
# ==============================================================================
if (!exists(".env_dir")) source("paths.R")
suppressMessages(library(sf))
dir_dhs  <- .dir_dhs()
old_path <- file.path(dir_dhs, "district_nfhs_shapefile/nfhs_data.shp")
pc11_path <- Sys.getenv("PC11_DISTRICT_SHP",
                        file.path(dir_dhs, "shrug_pc11_district_polygons/district.shp"))
out_dir  <- file.path(dir_dhs, "district_pc11_shrug")
out_path <- file.path(out_dir, "nfhs_data.shp")
for (p in c(old_path, pc11_path))
  if (!file.exists(p)) stop("missing input: ", p,
    if (p == pc11_path) "\n  Download 'PC11 District Polygons' (SHP) from https://www.devdatalab.org/shrug_download/ and unzip it there." else "")

old <- st_read(old_path, quiet = TRUE)
attrs <- st_drop_geometry(old)
attrs$dist_code <- as.numeric(attrs$dist_code)
sh <- st_read(pc11_path, quiet = TRUE)
sh$dist_code <- as.numeric(sh$pc11_d_id)
sh$dist_code[sh$dist_code == 0] <- -100            # background area: old placeholder row
sh <- sh[, "dist_code"]
if (anyDuplicated(sh$dist_code)) stop("SHRUG polygons: duplicated district code")
new <- merge(sh, attrs, by = "dist_code", all.x = FALSE)
miss <- setdiff(1:640, new$dist_code)
if (length(miss)) stop("2011 districts with no SHRUG polygon: ", paste(miss, collapse = ", "))
new <- st_transform(st_make_valid(new), st_crs(old))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
suppressWarnings(st_write(new, out_path, delete_dsn = TRUE, quiet = TRUE))
message(sprintf("district boundaries: %d polygons (SHRUG 2.2 PC11 geometry, original attributes) -> %s",
                nrow(new), out_path))
