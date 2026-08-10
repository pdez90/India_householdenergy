# NFHS x ACCESS x IRES: comparison, correction, and fuel-stacking prediction

R scripts implementing the analysis pipeline:

1. Compare NFHS-4 vs ACCESS Wave 1 and NFHS-5 vs IRES district estimates
   (rural-restricted, with scatterplots, Bland-Altman, and sample-size-weighted
   Pearson correlations, weighting districts by the reference survey's household
   count — the four comparison questions: correlation, pattern of divergence,
   urban/rural restriction, and weighting).
2. Treat ACCESS/IRES as reference and correct the NFHS LPG estimates
   (regression calibration + Bayesian hierarchical measurement-error model).
3. Predict fuel stacking (which NFHS lacks) from ACCESS/IRES models applied to
   NFHS households, to build district HAP-exposure proxies.

## Reproducing everything (recommended)

```
Rscript run_everything.R
```

`run_everything.R` runs the WHOLE pipeline in dependency order — data prep →
correction → augmentation → health analyses → figures/atlas → (optionally) the
Word manuscript — so every data file, figure, and table is mutually consistent
by construction. Prefer it over the older `run_all.R` / `run_health.R`, which
each cover only part of the chain and are what let figures fall out of sync with
the numbers. If you edit any script, re-run from that step onward:
`START_FROM=05_correction.R Rscript run_everything.R`. Flags: `SKIP_STATIC=FALSE`
(also re-run the slow, static 01 and H3), `BUILD_DOCX=FALSE` (skip the docx).

## Run order (individual scripts, for reference)

`run_everything.R` is the canonical order; the list below mirrors its `steps`.

```
00_config.R                 <- paths, switches, shared helpers (EDIT PATHS HERE)
01_prep_nfhs.R              -> nfhs_districts.rds, nfhs_hh_covariates.rds
02_prep_access.R            -> access_hh.rds, access_districts.rds
03_prep_ires.R              -> ires_hh.rds, ires_districts.rds
04_compare.R                -> comparison_table.csv, scatter + Bland-Altman figs
25_prep_nsso78.R            -> nsso78_hh.rds, nsso78_districts.rds (+ linkage diagnostics)
26_compare_nsso78.R         -> nsso78_* comparison table, spreadsheet, figures, summary
05_correction.R             -> corrected_nfhs_districts.rds (RC + Bayes, w/ CrIs)
06_stacking_prediction.R    -> nfhs_predicted_stacking.rds,
                               district_exposure_proxy.rds/.csv
21_transfer_diagnostics.R   -> era-vs-instrument transfer decomposition (uses ACCESS W2)
H3, H1, H2, H4 (rural+all), H2 nonoverlap, H5, H6   -> health analyses
22_design_analysis.R        -> validation-survey precision frontier
23_ppd_sensitivity.R        -> posterior-predictive estimand sensitivity
07-19                       -> figures, maps, atlas, SI diagnostics
24_si_sample_sizes.R        -> per-district/state sample sizes (SI)
20_pipeline_checks.R        -> consolidated self-checks
build_docx.R (via runner)   -> manuscript + SI docx from the fresh outputs
```

Scripts 01–03 and 25 are independent of each other; 04/26 need 01/03/25;
05–06 need 01–04; everything downstream follows the runner's order. The
NSSO-78 module (25/26) is part of the canonical pipeline — the manuscript's
external-validation section (SI S8) is built from its outputs — and is
documented in README_NSSO78.md.

## Things to verify before first run (marked `<-- CHECK` in the code)

- **02_prep_access.R**: the ACCESS cylinder-refill column names
  (`col_lpg_large_n`, `col_lpg_small_n`). Run
  `grep("cyl|refill|lpg", names(pool), value = TRUE, ignore.case = TRUE)`
  and set them. Gould et al. (2020) sum small (5 kg) + large (14.2 kg)
  purchases per year.
- **01_prep_nfhs.R**: caste column names in the `cvd_load` NFHS extract
  (`sh36` NFHS-4 / `sh49` NFHS-5 are placeholders).
- **03_prep_ires.R**: the IRES primary-fuel code for LPG (`IRES_LPG_CODE`,
  default 1) and the rural/urban sector column (`col_rural`).

## Design decisions (and why)

- **Main spec stays unweighted glmer** (state/district/cluster REs), matching
  your existing Rmd, with two weighted sensitivities: design-weighted direct
  district means (survey/srvyr; ACCESS village weights from the Zhang &
  Urpelainen replication archive, IRES `sw_dist`, NFHS `hv005/1e6`) and an
  optional WeMix weighted mixed model (`run_wemix` switch in 01). This directly
  answers the weighting question directly while keeping continuity with the
  original exploratory results.
- **Rural restriction**: ACCESS is all-rural, so pair A uses rural-only NFHS-4
  clusters; IRES comparisons are run all-households AND rural-only.
- **Agreement, not just correlation**: the comparison table reports Pearson,
  Spearman, population-weighted Pearson, logit-scale Pearson, Lin's CCC, and
  mean difference. Low Pearson with high CCC (or vice versa) is informative —
  attenuation from small-sample noise in 12-hh-per-village district estimates
  will depress Pearson even if the surveys agree on average.
- **Correction (05)**: regression calibration on the logit scale (transparent,
  easy to present) plus a brms `me()` measurement-error model that accounts for
  sampling error in the NFHS district estimates and gives 95% credible
  intervals for every corrected district. IRES (21 states) is the better basis
  for a *national* correction; the ACCESS calibration (6 poor northern states)
  serves as replication. A leave-one-state-out check tests whether the
  calibration transports.
- **Stacking prediction (06)**: three outcomes — binary stacking among
  main-LPG households, four-category fuel use (solid fuel without LPG /
  LPG and solid fuel reported / LPG with no solid fuel reported / neither
  LPG nor solid fuel reported — `use3cat` in the code carries all four
  levels), and LPG kg/yr via a two-part hurdle (Gould et al. 2020 structure)
  with Duan smearing. Predictors are restricted to the NFHS-available
  crosswalk (caste, religion, household size, BPL card, electricity,
  within-state wealth quintile, and state) plus the corrected district
  primary-LPG prevalence, era-matched. Validation:
  leave-district-out CV (AUC, Brier, district-level r) and a cross-survey test
  (IRES-trained model scored against observed ACCESS district stacking).
- **Era matching**: NFHS-4 rural gets ACCESS-W1-trained models; NFHS-5 gets
  IRES-trained models. ACCESS W2 (2018) is not used as a calibration
  reference, but it is analytical, not merely descriptive: 21_transfer_
  diagnostics.R uses it as the pivot of the 2x2 era-versus-instrument
  transfer decomposition (W1->W2 holds the instrument fixed across eras;
  IRES->W2 approximately holds the era fixed across instruments).

## Caveats to carry into the writeup

- Part of NFHS-vs-reference disagreement could in principle be *real temporal
  change* (PMUY rollout from 2016; COVID-era NFHS-5 fieldwork). The
  pre-COVID (2019-interview) and winter-season sensitivities have been run
  (09_season_sensitivity.R) and yield nearly identical gaps, so timing does
  not explain the discrepancy; the NSSO-78 validation (25/26) reinforces
  this, since its later fieldwork would widen, not create, the NFHS gap.
- IRES provides the primary national corrected surface because of its
  geographic support (21 states covering ~97% of the population); the
  corrected NFHS-4 surface outside the six ACCESS states is an extrapolation,
  and ACCESS serves mainly as independent corroboration of the direction and
  approximate magnitude of the correction where an era-matched reference
  exists.
- District identifiers: everything is keyed to the NFHS-4 2015 district
  geography (`dist_code` in the NFHS-4 district shapefile), with NFHS-5 points
  spatially overlaid and IRES matched by census code with a name-based
  fallback (unmatched districts are reported).

## Suggested next steps

- Cross district exposure classes with your UNC PM2.5 product to get
  population-weighted HAP+ambient exposure by district.
- Consider ordering exposure as exclusive-solid > stacking > exclusive-LPG and
  validating against NFHS's own kitchen/ventilation variables (hv241/hv242,
  sh52/sh55) which you already derived in the Rmd.
- If the Bayesian correction and calibration diverge, look first at
  small-n_hh districts (partial pooling matters most there).
