# ==============================================================================
# 08_si_benchmarks.R
# SI falsification check: if NFHS and the energy surveys disagree on LPG
# because of FUEL measurement (questionnaire design), they should still AGREE
# on benchmark variables measured the same way in both -- caste composition,
# religion, electricity, BPL card. Large LPG disagreement
# alongside good benchmark agreement localizes the problem to the fuel item;
# poor benchmark agreement would instead point to sample-composition issues.
#
# Pairs (rural, era-matched, overlap districts):
#   A: NFHS-4 vs ACCESS W1 (six states)     B: NFHS-5 vs IRES
# Benchmarks: scst, muslim, electricity, bpl (+ LPG for reference)
#
# Education was a benchmark here until 2026-08-01. It was removed because the
# NFHS household extracts carry no attainment scale at all (100% missing in both
# rounds), so the NFHS side of the comparison did not exist; see the header note
# in 01_prep_nfhs.R. It is not a benchmark that failed -- it is one that could
# never be computed.
#
# Inputs : nfhs_hh_covariates.rds, access_hh.rds, ires_hh.rds (01-03)
# Outputs: si_benchmark_agreement.csv, maps/SI_3_benchmark_scatter.jpeg
# Runtime: ~10-25 min (national multilevel fits for each benchmark variable).
# ==============================================================================

source("00_config.R")
# Identifies this script in diagnostics/model_fits.csv (the mixed-model fit
# registry). district_estimates_glmer() is shared, so the fit must be
# attributed to the caller; .chk_tag() reads this.
CHK_SCRIPT <- "08_si_benchmarks"

need_inputs(c("nfhs_hh_covariates.rds" = "01_prep_nfhs.R",
              "access_hh.rds"          = "02_prep_access.R",
              "ires_hh.rds"            = "03_prep_ires.R"))

# LABELLED-COLUMN HANDLING (harmonized with 13_benchmark_maps.R).
# The NFHS frames arrive from Stata as haven_labelled. Comparisons such as
# `bpl == 1` against a haven_labelled column do NOT behave like a plain numeric
# comparison: the class is retained, and downstream `length(unique(...))` /
# `all(is.na(...))` guards could then judge the variable unusable and silently
# skip it. That is how a benchmark once appeared in 13_benchmark_maps.R (which
# strips labels) but not here. The two scripts use the SAME stripping helper, so
# the set of available benchmark variables is identical between them by
# construction.
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

# ---- Harmonized benchmark indicators (0/1) per survey ---------------------------
bench_nfhs <- function(df) df %>%
  transmute(state = factor(state), district = as.character(district),
            clust = factor(clust),
            lpg,
            sc      = as.integer(caste == "Scheduled Caste"),
            st      = as.integer(caste == "Scheduled Tribe"),
            scst    = as.integer(caste %in% c("Scheduled Caste", "Scheduled Tribe")),
            hindu   = as.integer(hh_relig == "Hindu"),
            muslim  = as.integer(hh_relig == "Muslim"),
            electricity = as.integer(electricity == 1),
            bpl     = as.integer(bpl == 1))

bench_access <- access_hh %>% filter(wave == 0) %>%
  transmute(state = factor(state), district = as.character(district),
            clust = factor(village),
            lpg = main_fuel_lpg,
            sc      = as.integer(caste == "Scheduled Caste"),
            st      = as.integer(caste == "Scheduled Tribe"),
            scst    = as.integer(caste %in% c("Scheduled Caste", "Scheduled Tribe")),
            hindu   = as.integer(religion == "Hindu"),
            muslim  = as.integer(religion == "Muslim"),
            electricity = as.integer(electricity == 1),
            bpl     = as.integer(bplaay == 1))

bench_ires <- ires_hh %>% filter(rural == 1) %>%
  transmute(state = factor(state), district = as.character(district),
            clust = factor(village),
            lpg = main_fuel_lpg,
            sc      = as.integer(caste == "Scheduled Caste"),
            st      = as.integer(caste == "Scheduled Tribe"),
            scst    = as.integer(caste %in% c("Scheduled Caste", "Scheduled Tribe")),
            hindu   = as.integer(religion == "Hindu"),
            muslim  = as.integer(religion == "Muslim"),
            electricity = as.integer(electricity == 1),
            # q212 labels: 1 = Antyodaya, 2 = BPL, 3 = None, 99 = Don't know
            bpl     = ifelse(bplaay == 99, NA_integer_,
                             as.integer(bplaay %in% c(1, 2))))

# NFHS sides, era- and setting-matched; pair A restricted to ACCESS states to
# keep the national fits from dominating runtime.
access_states_nfhs <- c("Bihar", "Jharkhand", "Madhya Pradesh",
                        "Odisha", "Uttar Pradesh", "West Bengal")
bench_nfhs4 <- bench_nfhs(nfhs_hh %>%
  filter(survey == "NFHS4", rural == 1, state %in% access_states_nfhs))
bench_nfhs5 <- bench_nfhs(nfhs_hh %>% filter(survey == "NFHS5", rural == 1))

VARS <- c("lpg", "sc", "st", "scst", "hindu", "muslim",
          "electricity", "bpl")

skipped_vars <- list()   # frame -> variables dropped as unusable (audited below)

est_all_vars <- function(dat, tag, frame = tag) {
  out <- NULL
  for (v in VARS) {
    n_ok <- sum(!is.na(dat[[v]])); n_lev <- length(unique(na.omit(dat[[v]])))
    ok <- n_ok > 0 && n_lev >= 2
    if (!ok) {
      message(sprintf("%s: '%s' unusable, skipped (nonmissing = %d, distinct values = %d).",
                      tag, v, n_ok, n_lev))
      skipped_vars[[frame]] <<- c(skipped_vars[[frame]], v)
      next
    }
    message(tag, ": estimating ", v, " ...")
    e <- district_estimates_glmer(dat, v) %>%
      transmute(district = as.character(district), !!paste0(v, "_", tag) := p_hat)
    out <- if (is.null(out)) e else full_join(out, e, by = "district")
  }
  out
}

# Report the raw usability of every benchmark variable in every frame BEFORE
# fitting, so a variable that quietly disappears from the agreement table can
# always be traced to its source frame rather than to a modelling failure.
cat("\n== Benchmark-variable availability by frame (nonmissing / distinct values) ==\n")
avail <- purrr::imap_dfr(
  list(`NFHS-4 (ACCESS states, rural)` = bench_nfhs4, `ACCESS W1` = bench_access,
       `NFHS-5 (rural)` = bench_nfhs5, `IRES (rural)` = bench_ires),
  function(d, nm) tibble(frame = nm, variable = VARS,
                         n_nonmissing = vapply(VARS, function(v) sum(!is.na(d[[v]])), integer(1)),
                         n_distinct   = vapply(VARS, function(v) length(unique(na.omit(d[[v]]))), integer(1)),
                         usable       = n_nonmissing > 0 & n_distinct >= 2))
print(as.data.frame(avail), row.names = FALSE)
write_csv(avail, file.path(dir_out, "si_benchmark_variable_availability.csv"))

eA_nfhs <- est_all_vars(bench_nfhs4,  "nfhs", "NFHS-4 (ACCESS states, rural)")
eA_ref  <- est_all_vars(bench_access, "ref",  "ACCESS W1")
eB_nfhs <- est_all_vars(bench_nfhs5,  "nfhs", "NFHS-5 (rural)")
eB_ref  <- est_all_vars(bench_ires,   "ref",  "IRES (rural)")

pairA <- inner_join(eA_nfhs, eA_ref, by = "district") %>% mutate(pair = "NFHS-4 vs ACCESS W1 (rural)")
pairB <- inner_join(eB_nfhs, eB_ref, by = "district") %>% mutate(pair = "NFHS-5 vs IRES (rural)")

# ---- Agreement table -------------------------------------------------------------
agree_row <- function(df, v, pairlab) {
  x <- df[[paste0(v, "_nfhs")]]; y <- df[[paste0(v, "_ref")]]
  if (is.null(x) || is.null(y)) return(NULL)
  tibble(pair = pairlab, variable = v,
         n = sum(complete.cases(x, y)),
         pearson = cor(x, y, use = "pairwise.complete.obs"),
         ccc = ccc(x, y),
         mean_diff = mean(x - y, na.rm = TRUE))
}
bench_table <- bind_rows(
  map_dfr(VARS, ~agree_row(pairA, .x, "NFHS-4 vs ACCESS W1 (rural)")),
  map_dfr(VARS, ~agree_row(pairB, .x, "NFHS-5 vs IRES (rural)"))
) %>% mutate(across(where(is.numeric) & !any_of("n"), ~round(.x, 6)))
write_csv(bench_table, file.path(dir_out, "si_benchmark_agreement.csv"))
print(as.data.frame(bench_table))

# ---- Scatter grid -----------------------------------------------------------------
long_pair <- function(df) {
  map_dfr(VARS, function(v) {
    xn <- paste0(v, "_nfhs"); yn <- paste0(v, "_ref")
    if (!(xn %in% names(df)) || !(yn %in% names(df))) return(NULL)
    tibble(pair = df$pair, variable = v, nfhs = df[[xn]], ref = df[[yn]])
  })
}
plot_df <- bind_rows(long_pair(pairA), long_pair(pairB)) %>%
  mutate(variable = factor(variable,
    levels = VARS,
    labels = c("Primary LPG", "SC", "ST", "SC/ST", "Hindu", "Muslim",
               "Electricity", "BPL card")))
p <- ggplot(plot_df, aes(ref, nfhs)) +
  geom_abline(linetype = 2, color = "grey50") +
  geom_point(alpha = 0.5, size = 0.8, color = "#2166AC") +
  facet_grid(pair ~ variable) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw(base_size = 10) +
  labs(x = "Reference survey (ACCESS / IRES) district prevalence",
       y = "NFHS district prevalence",
       title = NULL, subtitle = NULL)
# Width tracks the facet count: 9 benchmark variables at width 18 gave square
# panels, and dropping education to 8 without shrinking the canvas would stretch
# every panel and break coord_equal's visual 1:1 reading of the diagonal.
ggsave(file.path(dir_out, "maps", "SI_3_benchmark_scatter.jpeg"), p,
       width = 2 * length(VARS), height = 5.5, dpi = 250)

# ---- External check: national rural 3-category shares vs Habib et al. 2023 -----
# Habib et al. (pan-India survey + IRES validation) report rural India 2019:
# 28% exclusive LPG / 33% stacking / 39% exclusive solid fuel. Their three shares
# sum to 1 over the LPG-OR-SOLID universe, so we must use the SAME denominator:
# drop the fourth "Neither LPG nor solid fuel reported" category before computing shares,
# otherwise our three shares sum to <1 and are not comparable.
hab <- ires_hh %>%
  filter(rural == 1, !is.na(use3cat), !is.na(wt),
         use3cat != "Neither LPG nor solid fuel reported") %>%
  mutate(use3cat = droplevels(use3cat)) %>%
  count(use3cat, wt = wt) %>%
  mutate(share = round(n / sum(n), 3)) %>%
  select(use3cat, share) %>%
  mutate(habib_2023 = case_when(
    use3cat == "LPG, no solid fuel reported"          ~ 0.28,
    use3cat == "LPG and solid fuel reported" ~ 0.33,
    use3cat == "Solid fuel reported, no LPG"             ~ 0.39))
write_csv(hab, file.path(dir_out, "si_habib_comparison.csv"))
message("IRES rural 3-category shares (design-weighted) vs Habib et al. 2023:")
print(as.data.frame(hab))

## ---- CHECKS ------------------------------------------------------------------
chk_header("08_si_benchmarks")
# Singular (boundary) mixed-model fits: a zero between-group variance means
# partial pooling collapsed to complete pooling for that grouping factor, so
# the estimate reverts to the pooled mean and its precision is borrowed
# rather than earned. Report the rate rather than let it vanish into lme4's
# warning stream. Detail per fit is in diagnostics/model_fits.csv.
if (exists("chk_singular_summary")) chk_singular_summary("08", "08_si_benchmarks")

chk("08", "Habib 3-category shares sum to 1 (4th category dropped)",
    abs(sum(hab$share, na.rm = TRUE) - 1) < 0.01,
    sprintf("sum = %.3f", sum(hab$share, na.rm = TRUE)))
chk_file("08", "benchmark agreement table written", "si_benchmark_agreement.csv")
chk_file("08", "Habib comparison written", "si_habib_comparison.csv")
chk_file("08", "benchmark variable availability table written",
    "si_benchmark_variable_availability.csv")

# EDUCATION (removed 2026-08-01). Two checks here used to assert that the
# low-education benchmark was usable in every frame and present in both pairs.
# They could not pass: the NFHS extracts carry no attainment scale, so the NFHS
# side was 100% missing and the row was correctly skipped by est_all_vars(),
# whereupon the assertions FAILed. Deleting the variable rather than the checks
# is the honest fix -- a check that can never pass is not evidence of anything.
# What replaces them is the assertion that education has stayed out: if a future
# extract reintroduces it half-wired, this FAILs and forces a decision.
chk("08", "education is absent from the benchmark set (see 01_prep_nfhs.R)",
    !any(grepl("^edu", VARS)) && !any(grepl("^edu", bench_table$variable)),
    sprintf("benchmark variables = %s", paste(VARS, collapse = ", ")))
chk("08", "no benchmark variable silently dropped from any frame",
    length(skipped_vars) == 0,
    if (length(skipped_vars) == 0) "none dropped" else
      paste(sprintf("%s: %s", names(skipped_vars),
                    vapply(skipped_vars, paste, character(1), collapse = ",")),
            collapse = " | "))
chk("08", "no benchmark column is still haven_labelled after stripping",
    !any(vapply(c(bench_nfhs4, bench_nfhs5, bench_access, bench_ires),
                function(x) inherits(x, "haven_labelled"), logical(1))))

message("08_si_benchmarks.R done: si_benchmark_agreement.csv, si_habib_comparison.csv, SI_3_benchmark_scatter.jpeg")
