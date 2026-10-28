# ==============================================================================
# 33_crosswalk_audit_refresh.R  (standalone; a few minutes)
#
# Rebuilds the version-2 crosswalk with the current geo_crosswalk.R, checks that
# its assignments are identical to the crosswalk the run used
# (nfhs_cluster_district_xwalk.csv), and rewrites the per-cluster audit and its
# district summary with the uncertainty flags (fallback, no GPS, near boundary,
# low-confidence history) and each flagged cluster's alternative district.
# Used when a run's crosswalk was built before the flags existed; a fresh run
# writes the same files itself (01_prep_nfhs.R).
# ==============================================================================
source("00_config.R")
source("geo_crosswalk.R")
suppressMessages(library(sf))
need_inputs(c("nfhs_cluster_district_xwalk.csv" = "01_prep_nfhs.R"))
SCRIPT <- "33"
variant <- CROSSWALK_VARIANT
.e15 <- new.env(); load(path_gps_2015, envir = .e15)
.e19 <- new.env(); load(path_gps_2019, envir = .e19)
names4 <- st_read(path_districts_shp, quiet = TRUE) |> st_drop_geometry() |>
  dplyr::transmute(code = as.numeric(dist_code), name = dist_name)
XW <- build_nfhs_xwalk_v2(.e15$gps, .e19$gps, pc11 = st_read(path_pc11_districts, quiet = TRUE),
                          history = readr::read_csv(path_district_history, show_col_types = FALSE),
                          names4 = names4, variant = variant)
used <- readr::read_csv(file.path(dir_out, "nfhs_cluster_district_xwalk.csv"), show_col_types = FALSE)
a <- XW$xwalk[order(XW$xwalk$survey, XW$xwalk$cluster), c("survey", "cluster", "district")]
b <- as.data.frame(used)[order(used$survey, used$cluster), c("survey", "cluster", "district")]
same <- nrow(a) == nrow(b) && all(a$cluster == b$cluster) && all(a$district == b$district)
chk(SCRIPT, "rebuilt version-2 crosswalk assigns every cluster as in the run",
    same, sprintf("%d of %d rows identical", if (nrow(a) == nrow(b)) sum(a$district == b$district) else NA, nrow(b)))
if (!same) stop("the rebuilt crosswalk differs from the one the run used; rerun the pipeline instead")
readr::write_csv(XW$audit, file.path(dir_out, "nfhs5_crosswalk_audit.csv"))
au <- XW$audit
ds <- au |> dplyr::group_by(nfhs5_state, nfhs5_district, history_type, history_confidence) |>
  dplyr::summarise(clusters = dplyr::n(), with_gps = sum(has_gps),
                   outside_all_polygons = sum(overlay_status == "outside all polygons"),
                   uncertain = sum(uncertain), near_boundary = sum(uncertainty_flag == "near boundary"),
                   fallback = sum(uncertainty_flag == "fallback"), no_gps_multi_parent = sum(uncertainty_flag == "no GPS"),
                   low_confidence_history = sum(history_low_confidence), excluded_by_variant = sum(excluded_by_variant),
                   changed_from_v1 = sum(changed_from_v1),
                   overlay_disagrees_with_final = sum(overlay_disagrees_with_final, na.rm = TRUE),
                   assigned_2011 = paste(sort(unique(stats::na.omit(final_district))), collapse = ";"), .groups = "drop")
readr::write_csv(ds, file.path(dir_out, "crosswalk_v2_district_summary.csv"))
print(table(au$uncertainty_flag[au$uncertain]))
cat(sprintf("clusters flagged uncertain: %d of %d; in low-confidence-history districts: %d\n",
            sum(au$uncertain), nrow(au), sum(au$history_low_confidence)))
print(utils::head(as.data.frame(ds[order(-ds$uncertain), c("nfhs5_state", "nfhs5_district", "clusters",
      "uncertain", "near_boundary", "fallback", "no_gps_multi_parent")]), 15), row.names = FALSE)
