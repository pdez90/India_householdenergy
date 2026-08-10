# ==============================================================================
# 14_benchmark_side_by_side.R   (STANDALONE -- does not source 00_config.R)
#
# Spatial version of the falsification check (the map analogue of SI Figure S5's
# scatter): for each benchmark variable, the NFHS district surface and the
# reference-survey surface are drawn SIDE BY SIDE on the same overlap districts
# and the same colour scale, for each era-matched pair:
#     NFHS-4 (2015) vs ACCESS Wave 1     |     NFHS-5 (2019) vs IRES
# Close visual agreement on the demographic benchmarks, alongside the large
# NFHS-vs-reference gap for primary LPG, shows the discrepancy is specific to
# the fuel item rather than to sample composition.
#
# Reads the district tables written by 13_benchmark_maps.R (fast; no household
# reprocessing):
#     benchmark_district_wide.csv        (NFHS-4/5:  <var>_2015, <var>_2019)
#     benchmark_reference_district.csv   (ACCESS/IRES: <var>_access, <var>_ires)
#
# Outputs:
#   maps/atlas/BENCHSBS_<pair>__<var>.jpeg                 (per-variable pairs)
#   maps/atlas_panels/benchmark_sidebyside_<pair>.jpeg     (full composite/pair)
#   benchmark_sidebyside_agreement.csv                     (r and mean diff)
#
# Run with:   Rscript 14_benchmark_side_by_side.R   (after 13_benchmark_maps.R)
# ==============================================================================

## ---- CONFIG (edit paths to match your machine) ------------------------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
path_districts_shp <- "/Users/priyanka/Downloads/DHS_India/district_nfhs_shapefile/nfhs_data.shp"
path_geom_fallback <- file.path(dir_out, "wave1_nfhs4.gpkg")

dir_atlas  <- file.path(dir_out, "maps", "atlas")
dir_panels <- file.path(dir_out, "maps", "atlas_panels")
dir.create(dir_atlas,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_panels, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({ library(tidyverse); library(sf) })
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (!has_patchwork)
  message("NOTE: install.packages('patchwork') to also get the combined per-pair composites.")

## ---- Geometry ----------------------------------------------------------------
shp <- (if (file.exists(path_districts_shp)) st_read(path_districts_shp, quiet = TRUE)
        else st_read(path_geom_fallback, quiet = TRUE)) %>%
  mutate(district = as.character(as.numeric(dist_code))) %>%
  filter(!st_is_empty(.)) %>% st_make_valid()

## ---- District tables from 13_benchmark_maps.R --------------------------------
p_nfhs <- file.path(dir_out, "benchmark_district_wide.csv")
p_ref  <- file.path(dir_out, "benchmark_reference_district.csv")
for (p in c(p_nfhs, p_ref))
  if (!file.exists(p)) stop("Missing ", p, " -- run 13_benchmark_maps.R first.")
nfhs_wide <- suppressMessages(read_csv(p_nfhs, show_col_types = FALSE)) %>%
  mutate(district = as.character(as.numeric(district)))
ref_wide  <- suppressMessages(read_csv(p_ref,  show_col_types = FALSE)) %>%
  mutate(district = as.character(as.numeric(district)))

VARS  <- c("lpg","sc","st","scst","hindu","muslim","electricity","bpl")
LABEL <- c(lpg="Primary LPG", sc="Scheduled Caste", st="Scheduled Tribe",
           scst="Scheduled Caste or Tribe", hindu="Hindu share",
           muslim="Muslim share", electricity="Household electricity",
           bpl="BPL/Antyodaya ration card")

PAIRS <- list(
  list(tag = "NFHS4_ACCESS", nfhs_sfx = "2015", ref_sfx = "access",
       nfhs_name = "NFHS-4 (2015)", ref_name = "ACCESS W1 (2015)"),
  list(tag = "NFHS5_IRES",   nfhs_sfx = "2019", ref_sfx = "ires",
       nfhs_name = "NFHS-5 (2019)", ref_name = "IRES (2019-20)")
)

## ---- One choropleth on a shared scale (overlap districts colored) ------------
one_map <- function(values_df, title, subtitle, lim) {
  dat <- shp %>% left_join(values_df, by = "district")
  ggplot(dat) +
    geom_sf(aes(fill = value), color = "grey85", linewidth = 0.02) +
    scale_fill_viridis_c(limits = lim, na.value = "grey93") +
    theme_void(base_size = 10) +
    labs(title = title, subtitle = subtitle, fill = "%") +
    theme(plot.title = element_text(size = 9, face = "bold"),
          plot.subtitle = element_text(size = 8, color = "grey30"),
          legend.key.width = unit(0.3, "cm"))
}
safe <- function(s) gsub("[^A-Za-z0-9]+", "_", s)

## ---- Build side-by-side maps for every pair x variable -----------------------
agree <- list()
for (pr in PAIRS) {
  panels <- list()
  for (v in VARS) {
    ncol_nfhs <- paste0(v, "_", pr$nfhs_sfx)
    ncol_ref  <- paste0(v, "_", pr$ref_sfx)
    if (!ncol_nfhs %in% names(nfhs_wide) || !ncol_ref %in% names(ref_wide)) next

    ref_v <- ref_wide %>% transmute(district, value = .data[[ncol_ref]])
    overlap <- ref_v$district[!is.na(ref_v$value)]                 # ref-sampled districts
    nfhs_v <- nfhs_wide %>% transmute(district, value = .data[[ncol_nfhs]]) %>%
      mutate(value = ifelse(district %in% overlap, value, NA))     # restrict to same districts

    # shared colour scale + agreement stats over districts with both estimates
    both <- inner_join(
      nfhs_wide %>% transmute(district, n = .data[[ncol_nfhs]]),
      ref_v %>% rename(r = value), by = "district") %>%
      filter(district %in% overlap, !is.na(n), !is.na(r))
    lim <- range(100 * c(both$n, both$r), na.rm = TRUE)
    rr  <- if (nrow(both) > 2) cor(both$n, both$r, use = "pairwise.complete.obs") else NA
    md  <- mean(100 * (both$n - both$r), na.rm = TRUE)             # NFHS - ref, in pp
    agree[[length(agree)+1]] <- tibble(pair = pr$tag, variable = v,
      n_districts = nrow(both), pearson_r = round(rr, 3), mean_diff_pp = round(md, 2))

    sub <- sprintf("r = %.2f   mean diff = %+.1f pp", rr, md)
    gN <- one_map(nfhs_v %>% mutate(value = 100*value),
                  paste0(pr$nfhs_name, ": ", LABEL[[v]]), sub, lim)
    gR <- one_map(ref_v  %>% mutate(value = 100*value),
                  paste0(pr$ref_name, ": ", LABEL[[v]]), " ", lim)

    if (has_patchwork) {
      library(patchwork)
      ggsave(file.path(dir_atlas, paste0("BENCHSBS_", pr$tag, "__", safe(v), ".jpeg")),
             gN + gR + patchwork::plot_layout(ncol = 2),
             width = 8.4, height = 4.3, dpi = 200)
    } else {
      ggsave(file.path(dir_atlas, paste0("BENCHSBS_", pr$tag, "__", safe(v), "_NFHS.jpeg")),
             gN, width = 4.2, height = 4.3, dpi = 200)
      ggsave(file.path(dir_atlas, paste0("BENCHSBS_", pr$tag, "__", safe(v), "_REF.jpeg")),
             gR, width = 4.2, height = 4.3, dpi = 200)
    }
    panels[[length(panels)+1]] <- gN
    panels[[length(panels)+1]] <- gR
  }

  if (has_patchwork && length(panels)) {
    library(patchwork)
    # FOUR columns = TWO variable-pairs per row (NFHS | ref | NFHS | ref), because
    # panels are ordered gN,gR per variable. With 8 benchmark variables this is a
    # 4x4 grid at 17.2 x 13.6 in (3440 x 2720 px at 200 dpi, aspect 1.265), which
    # embeds on a single SI page undistorted. The former ncol = 2 produced an
    # 8.6 x 27.2 in strip that no page can hold: it was being embedded at a
    # different aspect ratio and rendered visibly stretched. Keep this layout and
    # the img() dimensions in build_manuscript.js (468 x 370) in agreement.
    comp <- patchwork::wrap_plots(panels, ncol = 4)
    ggsave(file.path(dir_panels, paste0("benchmark_sidebyside_", pr$tag, ".jpeg")),
           comp, width = 17.2, height = 3.4 * (length(panels)/4), dpi = 200,
           limitsize = FALSE)
    message("Wrote composite benchmark_sidebyside_", pr$tag, ".jpeg (",
            length(panels)/2, " variables)")
  }
}

agree_tbl <- bind_rows(agree)
write_csv(agree_tbl, file.path(dir_out, "benchmark_sidebyside_agreement.csv"))
cat("\nAgreement (NFHS vs reference on overlap districts):\n")
print(as.data.frame(agree_tbl), row.names = FALSE)

## ---- CHECKS ------------------------------------------------------------------
chk_header("14_benchmark_side_by_side")
chk_file("14", "side-by-side agreement table written", "benchmark_sidebyside_agreement.csv")
chk("14", "side-by-side composites written (embedded in the manuscript)",
    dir.exists(dir_panels) &&
    length(list.files(dir_panels, pattern = "benchmark_sidebyside")) > 0,
    paste0(length(list.files(dir_panels, pattern = "benchmark_sidebyside")), " files"))

message("\n14_benchmark_side_by_side.R done.\n",
        "  Per-variable pairs: ", dir_atlas, "/BENCHSBS_*\n",
        "  Composites:         ", dir_panels, "/benchmark_sidebyside_*\n",
        "  Agreement table:    benchmark_sidebyside_agreement.csv")
