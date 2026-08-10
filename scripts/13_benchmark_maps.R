# ==============================================================================
# 13_benchmark_maps.R   (STANDALONE -- does not source 00_config.R)
#
# Companion to 12_make_all_maps.R. Maps the falsification-benchmark variables
# (caste, religion, ration card, electricity) that are NOT saved as
# district files by the pipeline, by re-deriving their district-level prevalence
# from the household files with the same four-level multilevel model used
# everywhere else, then drawing the same choropleths.
#
# Benchmark variables (each a 0/1 household indicator, definitions matched to
# 08_si_benchmarks.R):
#   sc, st, scst  (Scheduled Caste / Tribe / either), hindu, muslim,
#   electricity, bpl (BPL/Antyodaya ration card),
#   and primary LPG for reference.
#
# Surfaces produced (rural households, to match the main analysis):
#   NFHS-4 (2015) and NFHS-5 (2019)  -- national, all districts
#   ACCESS Wave 1 (2015)             -- 51 districts, six states
#   IRES (2019-20)                   -- 151 districts, rural subsample
#
# Outputs:
#   benchmark_district_wide.csv                         (NFHS-4/5, one row/district)
#   benchmark_reference_district.csv                    (ACCESS W1 + IRES)
#   maps/atlas/BENCH_*__*.jpeg                           (individual maps)
#   maps/atlas_panels/benchmark_{NFHS4,NFHS5,ACCESS,IRES}.jpeg   (composite panels)
#
# Standalone: reads only the household files already produced by 01-03 plus the
# NFHS-4 district shapefile. Run with:   Rscript 13_benchmark_maps.R
#   Inputs: nfhs_hh_covariates.rds, access_hh.rds, ires_hh.rds
# NOTE: the national NFHS multilevel fits are the slow step (~10-25 min total).
#   Set FAST_MEANS <- TRUE to use survey-weighted district means instead (seconds),
#   which are adequate for descriptive maps though not partial-pooled.
# ==============================================================================

## ---- CONFIG (edit paths to match your machine) ------------------------------
dir_out <- "/Users/priyanka/Downloads/ACCESS_replica"
if (file.exists("checks.R")) source("checks.R")   # pipeline self-check helpers
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "13_benchmark_maps"

path_districts_shp <- "/Users/priyanka/Downloads/DHS_India/district_nfhs_shapefile/nfhs_data.shp"
path_geom_fallback <- file.path(dir_out, "wave1_nfhs4.gpkg")

FAST_MEANS <- TRUE    # TRUE = survey-weighted district means (fast, default);
                      # FALSE = null 4-level multilevel model (partial-pooled, slower;
                      # note the national NFHS household file is large, ~10-25 min).

dir_atlas  <- file.path(dir_out, "maps", "atlas")
dir_panels <- file.path(dir_out, "maps", "atlas_panels")
dir.create(dir_atlas,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_panels, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(tidyverse); library(sf); library(lme4)
})
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)

## ---- Geometry ----------------------------------------------------------------
read_geom <- function() {
  g <- if (file.exists(path_districts_shp)) st_read(path_districts_shp, quiet = TRUE)
       else if (file.exists(path_geom_fallback)) st_read(path_geom_fallback, quiet = TRUE)
       else stop("No district geometry found.")
  g %>% mutate(district = as.character(as.numeric(dist_code))) %>%
    filter(!st_is_empty(.)) %>% st_make_valid()
}
shp <- read_geom()
message("Geometry loaded: ", nrow(shp), " district polygons.")

## ---- District prevalence from a null 4-level model (same as H1) --------------
district_estimates_glmer <- function(data, outcome,
                                     state = "state", district = "district",
                                     cluster = "clust") {
  d <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[state]]),
           !is.na(.data[[district]]), !is.na(.data[[cluster]])) %>%
    mutate(across(all_of(c(state, district, cluster)), as.factor))
  if (nrow(d) == 0 || length(unique(d[[outcome]])) < 2)
    return(tibble(district = character(), p_hat = numeric()))
  fml <- as.formula(paste0(outcome, " ~ (1|", state, ") + (1|", district,
                           ") + (1|", cluster, ")"))
  m <- glmer(fml, data = d, family = binomial, nAGQ = 0,
             control = glmerControl(optimizer = "nloptwrap"))
  if (exists("chk_record_fit"))
    chk_record_fit(.chk_tag("13_benchmark_maps"),
                   paste0("district_estimates_glmer:", outcome), m,
                   extra = sprintf("districts=%d", nlevels(d[[district]])))
  re_d <- ranef(m)[[district]] %>% rownames_to_column(district) %>% rename(v = `(Intercept)`)
  re_s <- ranef(m)[[state]]    %>% rownames_to_column(state)    %>% rename(f = `(Intercept)`)
  d %>% distinct(.data[[state]], .data[[district]]) %>%
    rename(state = 1, district = 2) %>%
    left_join(re_d, by = setNames(district, "district")) %>%
    left_join(re_s, by = setNames(state, "state")) %>%
    mutate(p_hat = plogis(fixef(m)[["(Intercept)"]] + coalesce(v, 0) + coalesce(f, 0))) %>%
    select(district, p_hat)
}

## ---- Weighted district mean (fast alternative) -------------------------------
district_wmean <- function(data, outcome, weight = "wt") {
  d <- data %>% filter(!is.na(.data[[outcome]]))
  if (weight %in% names(d)) d <- d %>% filter(!is.na(.data[[weight]]))
  d %>% group_by(district) %>%
    summarise(p_hat = if (weight %in% names(data))
                weighted.mean(.data[[outcome]], .data[[weight]], na.rm = TRUE)
              else mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
}

estimate_var <- function(dat, v) {
  ok <- v %in% names(dat) && !all(is.na(dat[[v]])) && length(unique(na.omit(dat[[v]]))) >= 2
  if (!ok) { message("   '", v, "' unusable, skipped."); return(NULL) }
  message("   estimating ", v, " ...")
  if (FAST_MEANS) district_wmean(dat, v) else district_estimates_glmer(dat, v)
}

## ---- Harmonized benchmark indicators per survey (from 08_si_benchmarks.R) -----
# Education was a benchmark here until 2026-08-01 and is not one now: the NFHS
# household extracts carry no attainment scale (100% missing in both rounds), so
# there was never an NFHS side to map against ACCESS/IRES. See the header note in
# 01_prep_nfhs.R. The ACCESS/IRES education distributions are still described --
# in 19_ires_access_atlas.R, where no NFHS comparison is claimed.
VARS <- c("lpg","sc","st","scst","hindu","muslim","electricity","bpl")

# Household files carry haven_labelled columns (Stata import) that recent
# vctrs/dplyr refuse to compare or coerce; strip the labelled class to base
# numeric/character at load so all downstream comparisons are well-behaved.
strip_labelled <- function(df) {
  df[] <- lapply(df, function(col) {
    if (inherits(col, "haven_labelled")) {
      v <- unclass(col)
      attr(v, "labels") <- NULL; attr(v, "label") <- NULL
      attr(v, "format.stata") <- NULL
      if (is.character(v)) as.character(v) else as.numeric(v)
    } else col
  })
  df
}
nfhs_hh   <- strip_labelled(readRDS(file.path(dir_out, "nfhs_hh_covariates.rds")))
access_hh <- strip_labelled(readRDS(file.path(dir_out, "access_hh.rds")))
ires_hh   <- strip_labelled(readRDS(file.path(dir_out, "ires_hh.rds")))

# A note for anyone re-adding a benchmark: map it on the same footing in all
# three frames, and assert the OUTPUT column rather than its membership in the
# request vector. An earlier education check tested `"edu_low" %in% VARS_NFHS`,
# a tautology two lines below the assignment, and so passed while the benchmark
# was entirely absent from the table this script writes.
VARS_NFHS <- VARS
VARS_REF  <- VARS

# NFHS household columns arrive as haven_labelled (Stata import); coerce the
# numeric ones to plain doubles before comparison, and the categoricals to
# character, so dplyr's strict type checks do not reject them.
bench_nfhs <- function(df) df %>%
  transmute(state = as.character(state), district = as.character(district),
            clust = as.character(clust),
            wt = as.numeric(wt), lpg = as.numeric(lpg),
            sc  = as.integer(as.character(caste) == "Scheduled Caste"),
            st  = as.integer(as.character(caste) == "Scheduled Tribe"),
            scst= as.integer(as.character(caste) %in% c("Scheduled Caste","Scheduled Tribe")),
            hindu  = as.integer(as.character(hh_relig) == "Hindu"),
            muslim = as.integer(as.character(hh_relig) == "Muslim"),
            electricity = as.integer(as.numeric(electricity) == 1),
            bpl     = as.integer(as.numeric(bpl) == 1))

bench_access <- access_hh %>% filter(wave == 0) %>%
  transmute(state = as.character(state), district = as.character(district),
            clust = as.character(village),
            wt = as.numeric(weights), lpg = as.numeric(main_fuel_lpg),
            sc  = as.integer(as.character(caste) == "Scheduled Caste"),
            st  = as.integer(as.character(caste) == "Scheduled Tribe"),
            scst= as.integer(as.character(caste) %in% c("Scheduled Caste","Scheduled Tribe")),
            hindu  = as.integer(as.character(religion) == "Hindu"),
            muslim = as.integer(as.character(religion) == "Muslim"),
            electricity = as.integer(as.numeric(electricity) == 1),
            bpl     = as.integer(as.numeric(bplaay) == 1))

bench_ires <- ires_hh %>% filter(rural == 1) %>%
  transmute(state = as.character(state), district = as.character(district),
            clust = as.character(village),
            wt = as.numeric(wt), lpg = as.numeric(main_fuel_lpg),
            sc  = as.integer(as.character(caste) == "Scheduled Caste"),
            st  = as.integer(as.character(caste) == "Scheduled Tribe"),
            scst= as.integer(as.character(caste) %in% c("Scheduled Caste","Scheduled Tribe")),
            hindu  = as.integer(as.character(religion) == "Hindu"),
            muslim = as.integer(as.character(religion) == "Muslim"),
            electricity = as.integer(as.numeric(electricity) == 1),
            bpl     = ifelse(as.numeric(bplaay) == 99, NA_integer_,
                             as.integer(as.numeric(bplaay) %in% c(1, 2))))

## ---- Build district-wide tables ----------------------------------------------
build_wide <- function(dat, tag, vars = VARS) {
  message("== ", tag, " (", nrow(dat), " households) ==")
  out <- NULL
  for (v in vars) {
    e <- estimate_var(dat, v); if (is.null(e)) next
    e <- e %>% transmute(district = as.character(as.numeric(district)),
                         !!paste0(v, "_", tag) := p_hat)
    out <- if (is.null(out)) e else full_join(out, e, by = "district")
  }
  out
}

n4 <- bench_nfhs(nfhs_hh %>% filter(survey == "NFHS4", rural == 1))
n5 <- bench_nfhs(nfhs_hh %>% filter(survey == "NFHS5", rural == 1))
nfhs_wide <- full_join(build_wide(n4, "2015", VARS_NFHS),
                       build_wide(n5, "2019", VARS_NFHS), by = "district")
write_csv(nfhs_wide, file.path(dir_out, "benchmark_district_wide.csv"))

acc_wide  <- build_wide(bench_access, "access", VARS_REF)
ires_wide <- build_wide(bench_ires,   "ires",   VARS_REF)
ref_wide  <- full_join(acc_wide, ires_wide, by = "district")
write_csv(ref_wide, file.path(dir_out, "benchmark_reference_district.csv"))

## ---- Mapping (same style as 12_make_all_maps.R) ------------------------------
LABEL <- c(lpg="Primary LPG", sc="Scheduled Caste", st="Scheduled Tribe",
           scst="Scheduled Caste or Tribe", hindu="Hindu share", muslim="Muslim share",
           electricity="Household electricity", bpl="BPL/Antyodaya ration card")
safe <- function(s) gsub("[^A-Za-z0-9]+", "_", s)

map_one <- function(wide, v, colname, title) {
  if (!colname %in% names(wide)) return(NULL)
  d <- wide %>% transmute(district = as.character(as.numeric(district)),
                          value = 100 * .data[[colname]])
  dat <- shp %>% left_join(d, by = "district")
  ggplot(dat) +
    geom_sf(aes(fill = value), color = "grey80", linewidth = 0.03) +
    scale_fill_viridis_c(na.value = "grey88") +
    theme_void(base_size = 10) +
    labs(title = title, fill = "%") +
    theme(plot.title = element_text(size = 9, face = "bold"),
          legend.key.width = unit(0.35, "cm"))
}

# one surface = one (wide table, tag, pretty survey name)
surfaces <- list(
  list(wide = nfhs_wide, tag = "2015",   name = "NFHS-4 (2015)",   key = "NFHS4"),
  list(wide = nfhs_wide, tag = "2019",   name = "NFHS-5 (2019)",   key = "NFHS5"),
  list(wide = ref_wide,  tag = "access", name = "ACCESS W1 (2015)",key = "ACCESS"),
  list(wide = ref_wide,  tag = "ires",   name = "IRES (2019-20)",  key = "IRES")
)

for (s in surfaces) {
  plots <- list()
  for (v in VARS) {
    colname <- paste0(v, "_", s$tag)
    p <- map_one(s$wide, v, colname, paste0(s$name, ": ", LABEL[[v]]))
    if (is.null(p)) next
    plots[[v]] <- p
    ggsave(file.path(dir_atlas, paste0("BENCH_", s$key, "__", safe(v), ".jpeg")),
           p, width = 4.2, height = 4.4, dpi = 200)
  }
  if (has_patchwork && length(plots)) {
    library(patchwork)
    ncol <- if (length(plots) <= 3) length(plots) else 3
    nr <- ceiling(length(plots) / ncol)
    ggsave(file.path(dir_panels, paste0("benchmark_", s$key, ".jpeg")),
           patchwork::wrap_plots(plots, ncol = ncol),
           width = 4.0 * ncol, height = 4.2 * nr, dpi = 200, limitsize = FALSE)
  }
  message("Mapped ", length(plots), " benchmark surfaces for ", s$name)
}

## ---- CHECKS ------------------------------------------------------------------
chk_header("13_benchmark_maps")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("13", "13_benchmark_maps")

chk_file("13", "benchmark district table written", "benchmark_district_wide.csv")
chk("13", "benchmark composite panels written", dir.exists(dir_panels) &&
    length(list.files(dir_panels, pattern = "benchmark")) > 0,
    paste0(length(list.files(dir_panels, pattern = "benchmark")), " files"))
# Assert the OUTPUT, not the request: every requested benchmark must actually
# have reached the district table for BOTH rounds. This is the general form of
# the education-specific check removed on 2026-08-01, and it would have caught
# that failure without naming the variable.
.want <- as.vector(outer(VARS_NFHS, c("2015", "2019"), paste, sep = "_"))
.miss <- setdiff(.want, names(nfhs_wide))
chk("13", "every requested benchmark reached the NFHS district table",
    length(.miss) == 0,
    if (length(.miss) == 0)
      paste0(length(.want), " columns present in benchmark_district_wide.csv")
    else paste0("MISSING: ", paste(.miss, collapse = ", "),
                ". The variable is NA or constant in the NFHS frame -- check ",
                "the derivation in 01_prep_nfhs.R before re-adding it here."))

message("\n13_benchmark_maps.R done.\n",
        "  District tables: benchmark_district_wide.csv, benchmark_reference_district.csv\n",
        "  Individual maps: ", dir_atlas, "/BENCH_*\n",
        "  Composite panels: ", dir_panels, "/benchmark_*")
