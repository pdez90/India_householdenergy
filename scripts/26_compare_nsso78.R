# ==============================================================================
# 26_compare_nsso78.R
# Three-way district comparison of P(main cooking fuel = LPG):
#   NSSO-78 MIS (2020-21)  x  NFHS-5 (2019-21)  x  IRES (2019-20)
# on the common NFHS-4 2015 district geography, extending 04_compare.R
# (which this script deliberately does not touch).
#
# Pairs (each: all-household glmer, rural glmer, rural design-weighted):
#   C1. NSSO-78 vs NFHS-5      [~630+ districts, national]
#   C2. NSSO-78 vs IRES        [IRES's ~150 districts]
#   C3. NFHS-5 vs IRES         [same subset as C2, for context; matches 04]
#
# Inputs : nfhs_districts.rds (01), ires_districts.rds (03),
#          nsso78_districts.rds (25), district shapefile
# Outputs: nsso78_nfhs_ires_district_lpg.csv   <- the district spreadsheet
#          nsso78_comparison_table.csv          <- agreement metrics
#          nsso78_scatter_45deg.jpeg, nsso78_blandaltman.jpeg
#          nsso78_compare.rds, nsso78_comparison_summary.md
#
# Comparability notes (carry into any writeup):
#  * All three surveys ask for the household's PRIMARY/main cooking fuel, so
#    the estimand matches across surveys (unlike NFHS factsheet "clean fuel").
#    NSSO item 16 codes LPG separately (02) from other natural gas (03);
#    NFHS hv226 == 2 is "LPG/natural gas" in some DHS phase-7 recodes -- if so
#    the NFHS series is LPG-or-PNG. PNG is 0.5% of households nationally
#    (NSSO-78), concentrated in a few metros, so this cannot move district
#    estimates outside big cities, but say it in the limitations.
#  * Timing: IRES fieldwork ~late 2019 - early 2020; NFHS-5 Jun 2019 - Apr 2021
#    (two phases straddling COVID); NSSO-78 Jan 2020 - Aug 2021 (COVID-
#    extended). PMUY-era LPG growth means NSSO > NFHS-5 gaps are partly REAL
#    temporal change, not only measurement difference. The 04_compare.R
#    sensitivity (restrict NFHS-5 to 2019 interviews) applies here a fortiori.
#  * NSSO-78 district estimates: the MIS was designed to yield district-level
#    estimates for major indicators (districts are strata), but per-district
#    samples are modest (median ~320 households) -- report n and prefer the
#    partial-pooling glmer for maps, as elsewhere in this pipeline.
# ==============================================================================

source("00_config.R")
need_inputs(c("nfhs_districts.rds"   = "01_prep_nfhs.R",
              "ires_districts.rds"   = "03_prep_ires.R",
              "nsso78_districts.rds" = "25_prep_nsso78.R"))

nfhs <- readRDS(file.path(dir_out, "nfhs_districts.rds"))
ires <- readRDS(file.path(dir_out, "ires_districts.rds"))
nsso <- readRDS(file.path(dir_out, "nsso78_districts.rds"))

districts_shp <- st_read(path_districts_shp, quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(district = as.character(as.numeric(dist_code)),
            state_name, dist_name)

# ---- The district spreadsheet: one row per district, all three surveys --------
three_way <- districts_shp %>%
  left_join(nsso %>% select(district, nsso_mainlpg, nsso_mainlpg_rural,
                            nsso_mainlpg_wt, nsso_mainlpg_wt_se,
                            nsso_mainlpg_rural_wt, nsso_mainlpg_rural_wt_se,
                            n_nsso, n_nsso_rural),
            by = "district") %>%
  left_join(nfhs %>% select(district, lpg_2019, lpg_2019_rural,
                            lpg_2019_rural_wt, lpg_2019_rural_wt_se,
                            n_2019, n_2019_rural),
            by = "district") %>%
  left_join(ires %>% select(district, ires_mainlpg, ires_mainlpg_rural,
                            ires_mainlpg_wt, ires_mainlpg_wt_se,
                            ires_mainlpg_rural_wt, ires_mainlpg_rural_wt_se,
                            n_ires, n_ires_rural),
            by = "district") %>%
  arrange(state_name, dist_name)

readr::write_csv(three_way, file.path(dir_out, "nsso78_nfhs_ires_district_lpg.csv"))
saveRDS(three_way, file.path(dir_out, "nsso78_compare.rds"))

# ---- Agreement metrics (same battery as 04_compare.R) --------------------------
# agreement() is duplicated from 04_compare.R rather than sourced, so 04 stays
# untouched and each script remains independently re-runnable.
agreement <- function(df, x, y, w = NULL, label = "") {
  xv <- df[[x]]; yv <- df[[y]]
  wv <- if (!is.null(w)) df[[w]] else rep(1, nrow(df))
  tibble(
    comparison   = label,
    n_districts  = sum(complete.cases(xv, yv)),
    pearson      = cor(xv, yv, use = "pairwise.complete.obs"),
    spearman     = cor(xv, yv, use = "pairwise.complete.obs", method = "spearman"),
    pearson_ref_samplewt = weighted_cor(xv, yv, wv),
    ccc          = ccc(xv, yv),
    mean_diff    = mean(xv - yv, na.rm = TRUE),
    pearson_logit= cor(qlogis(pmin(pmax(xv, .001), .999)),
                       qlogis(pmin(pmax(yv, .001), .999)),
                       use = "pairwise.complete.obs")
  )
}

ires_sub <- three_way %>% filter(is.finite(ires_mainlpg))   # IRES's ~150 districts

comparison_table <- bind_rows(
  agreement(three_way, "nsso_mainlpg",          "lpg_2019",        "n_nsso",
            "NSSO-78 (all) vs NFHS-5 (all)"),
  agreement(three_way, "nsso_mainlpg_rural",    "lpg_2019_rural",  "n_nsso_rural",
            "NSSO-78 (rural) vs NFHS-5 (rural)"),
  agreement(three_way, "nsso_mainlpg_rural_wt", "lpg_2019_rural_wt", "n_nsso_rural",
            "NSSO-78 (rural, design-wt) vs NFHS-5 (rural, design-wt)"),
  agreement(ires_sub,  "nsso_mainlpg",          "ires_mainlpg",    "n_ires",
            "NSSO-78 (all) vs IRES (all)"),
  agreement(ires_sub,  "nsso_mainlpg_rural",    "ires_mainlpg_rural", "n_ires_rural",
            "NSSO-78 (rural) vs IRES (rural)"),
  agreement(ires_sub,  "nsso_mainlpg_rural_wt", "ires_mainlpg_rural_wt", "n_ires_rural",
            "NSSO-78 (rural, design-wt) vs IRES (rural, design-wt)"),
  agreement(ires_sub,  "lpg_2019",              "ires_mainlpg",    "n_ires",
            "NFHS-5 (all) vs IRES (all)  [context, IRES districts]"),
  agreement(ires_sub,  "lpg_2019_rural",        "ires_mainlpg_rural", "n_ires_rural",
            "NFHS-5 (rural) vs IRES (rural)  [context, IRES districts]")
)
readr::write_csv(comparison_table, file.path(dir_out, "nsso78_comparison_table.csv"))
print(comparison_table)

# ---- Scatterplots (45-degree line, sized by households) -------------------------
scatter_pair <- function(df, x, y, xlab, ylab, size_var) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]], color = state_name)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey40") +
    geom_point(aes(size = .data[[size_var]]), alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, inherit.aes = FALSE,
                aes(x = .data[[x]], y = .data[[y]])) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = xlab, y = ylab, color = "State", size = "Households") +
    theme_bw() +
    guides(color = "none")     # 30+ states: legend would swamp a 3-panel figure
}

p1 <- scatter_pair(three_way, "lpg_2019", "nsso_mainlpg",
                   "NFHS-5 (all), P(main fuel = LPG)",
                   "NSSO-78 (all), P(main fuel = LPG)", "n_nsso")
p2 <- scatter_pair(ires_sub, "ires_mainlpg", "nsso_mainlpg",
                   "IRES (all), P(main fuel = LPG)",
                   "NSSO-78 (all), P(main fuel = LPG)", "n_ires")
p3 <- scatter_pair(three_way, "lpg_2019_rural", "nsso_mainlpg_rural",
                   "NFHS-5 (rural), P(main fuel = LPG)",
                   "NSSO-78 (rural), P(main fuel = LPG)", "n_nsso_rural")
p4 <- scatter_pair(ires_sub, "ires_mainlpg_rural", "nsso_mainlpg_rural",
                   "IRES (rural), P(main fuel = LPG)",
                   "NSSO-78 (rural), P(main fuel = LPG)", "n_ires_rural")

jpeg(file.path(dir_out, "nsso78_scatter_45deg.jpeg"),
     res = 300, width = 4200, height = 4200)
print(cowplot::plot_grid(p1, p2, p3, p4,
                         labels = c("A)", "B)", "C)", "D)"), ncol = 2))
dev.off()

# ---- Bland-Altman -----------------------------------------------------------------
ba1 <- bland_altman_plot(three_way, "nsso_mainlpg_rural", "lpg_2019_rural",
                         "NSSO-78 rural", "NFHS-5 rural") +
       theme(legend.position = "none")
ba2 <- bland_altman_plot(ires_sub, "nsso_mainlpg_rural", "ires_mainlpg_rural",
                         "NSSO-78 rural", "IRES rural")
jpeg(file.path(dir_out, "nsso78_blandaltman.jpeg"),
     res = 300, width = 3000, height = 4500)
print(cowplot::plot_grid(ba1, ba2, labels = c("A)", "B)"), ncol = 1))
dev.off()

# ---- Written summary, derived not typed (same philosophy as 03's frame counts) ---
g <- function(lbl, col) {
  r <- comparison_table[comparison_table$comparison == lbl, ]
  if (nrow(r) != 1) return(NA_real_)
  r[[col]]
}
biggest <- three_way %>%
  filter(is.finite(nsso_mainlpg), is.finite(lpg_2019)) %>%
  mutate(diff = nsso_mainlpg - lpg_2019) %>%
  arrange(desc(abs(diff))) %>%
  slice(1:10) %>%
  transmute(line = sprintf("| %s | %s | %.2f | %.2f | %+.2f |",
                           state_name, dist_name, nsso_mainlpg, lpg_2019, diff))
summary_md <- c(
  "# NSSO-78 x NFHS-5 x IRES: district LPG-as-main-fuel comparison",
  "",
  sprintf("_Generated by 26_compare_nsso78.R on %s. All numbers recompute on re-run._",
          format(Sys.Date())),
  "",
  "## Agreement (P(main cooking fuel = LPG), district level)",
  "",
  "| comparison | districts | Pearson | CCC | mean diff |",
  "|---|---|---|---|---|",
  vapply(comparison_table$comparison, function(l) {
    sprintf("| %s | %d | %.3f | %.3f | %+.3f |",
            l, as.integer(g(l, "n_districts")), g(l, "pearson"),
            g(l, "ccc"), g(l, "mean_diff"))
  }, character(1)),
  "",
  "## Reading the table",
  "",
  sprintf(paste0(
    "* NSSO-78 sits ABOVE NFHS-5 on average (mean diff %+.3f all-household, ",
    "%+.3f rural): consistent with (i) the NFHS understatement of LPG this ",
    "project documents against ACCESS/IRES, and (ii) real PMUY-era growth -- ",
    "NSSO-78 fieldwork (Jan 2020 - Aug 2021) is centred ~1 year after NFHS-5's."),
    g("NSSO-78 (all) vs NFHS-5 (all)", "mean_diff"),
    g("NSSO-78 (rural) vs NFHS-5 (rural)", "mean_diff")),
  sprintf(paste0(
    "* Against IRES (mean diff %+.3f all, %+.3f rural), NSSO-78 is far closer ",
    "in level; correlation (r = %.3f all) is limited by IRES's ~12-hh-per-",
    "village district samples, as in the NFHS-IRES pair."),
    g("NSSO-78 (all) vs IRES (all)", "mean_diff"),
    g("NSSO-78 (rural) vs IRES (rural)", "mean_diff"),
    g("NSSO-78 (all) vs IRES (all)", "pearson")),
  "* All three surveys ask for the PRIMARY cooking fuel, so the estimand",
  "  matches; see the header of 26_compare_nsso78.R for the LPG-vs-PNG nuance",
  "  in NFHS hv226 and for timing caveats.",
  "",
  "## Largest NSSO-78 vs NFHS-5 divergences (all households, glmer)",
  "",
  "| state | district | NSSO-78 | NFHS-5 | diff |",
  "|---|---|---|---|---|",
  biggest$line,
  "",
  "## Provenance",
  "",
  "* NSSO-78: MIS unit data, Level 03; LPG = fuel code 02; weights MULT/100",
  "  (validated against the MoSPI press-note clean-fuel figures in 25).",
  "* District key: nsso78_district_key.csv (685 NSS districts -> 640 census-2011",
  "  codes; 47 post-2011 districts folded into 2011 parents).",
  "* NFHS-5 / IRES estimates: nfhs_districts.rds / ires_districts.rds from this",
  "  pipeline (01, 03), NFHS-5 spatially assigned to the 2015 geography."
)
writeLines(summary_md, file.path(dir_out, "nsso78_comparison_summary.md"))

## ---- CHECKS ------------------------------------------------------------------
chk_header("26_compare_nsso78")
chk("26", "three-way spreadsheet written with all three surveys populated",
    file.exists(file.path(dir_out, "nsso78_nfhs_ires_district_lpg.csv")) &&
    sum(complete.cases(three_way$nsso_mainlpg, three_way$lpg_2019)) > 550 &&
    sum(complete.cases(three_way$nsso_mainlpg, three_way$ires_mainlpg)) > 130,
    sprintf("NSSOxNFHS5 %d districts; NSSOxIRES %d districts",
            sum(complete.cases(three_way$nsso_mainlpg, three_way$lpg_2019)),
            sum(complete.cases(three_way$nsso_mainlpg, three_way$ires_mainlpg))))
chk("26", "agreement table has all 8 rows and finite Pearson everywhere",
    nrow(comparison_table) == 8 && all(is.finite(comparison_table$pearson)))
chk("26", "rural design-weighted comparisons present (like-for-like with 04/05)",
    all(c("NSSO-78 (rural, design-wt) vs NFHS-5 (rural, design-wt)",
          "NSSO-78 (rural, design-wt) vs IRES (rural, design-wt)") %in%
        comparison_table$comparison))
chk_file("26", "scatter figure written", "nsso78_scatter_45deg.jpeg")
chk_file("26", "Bland-Altman figure written", "nsso78_blandaltman.jpeg")
chk_file("26", "summary written", "nsso78_comparison_summary.md")
chk_warn("26", "NSSO-78 above NFHS-5 on average (expected direction)",
    g("NSSO-78 (all) vs NFHS-5 (all)", "mean_diff") > 0,
    sprintf("mean diff %+.3f", g("NSSO-78 (all) vs NFHS-5 (all)", "mean_diff")))

message("26_compare_nsso78.R done. See nsso78_* files in ", dir_out)
