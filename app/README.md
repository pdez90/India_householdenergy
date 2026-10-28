# App: cooking fuel, surveys and health in India

A guided walkthrough of the analysis, built with R/Shiny. It reads only the published
aggregate results: no household records, GPS coordinates or cluster identifiers.

## Pages
1. **The story**: the four steps with one key number each.
2. **Surveys disagree**: NFHS vs ACCESS (2015) and NFHS vs IRES (2019), district by district;
   checks for sample composition, season and states.
3. **Correction**: an interactive district map (raw, corrected, change, uncertainty); a leave-one-state-out check; the calibration model.
4. **Health (exploratory)**: the mortality association with the corrected surface held fixed (conditional on the correction) and the exposure-only imputation (sensitivity); why no validated way to carry the correction's uncertainty was available; the small-cluster simulation.
5. **Uncertainty**: where the corrected exposure's uncertainty comes from (calibration versus the health survey's own district estimates; descriptive).
6. **Explore a district**; **Data & methods** (provenance, downloads, glossary).
7. **Fuel stacking (experimental)** (a top-level page): validation first (household AUC, district r within and across surveys, a state-level leave-one-state-out check against a no-skill benchmark); then the reference surveys' own measured district stacking shares; then, behind an explicit opt-in, the model-based national surface, which fails validation and must not be used to rank or target districts. The state check and the measured shares come from `scripts/27_stacking_state_validation.R`; run it once after the pipeline, before `prepare_data.R`.
8. **Under the hood**:
   - **Correction models**: regression calibration and the Bayesian measurement-error model side by side; every fitted parameter, including the design-weighted ACCESS sensitivity; sampler settings and diagnostics; input SE bounds; empirical-Bayes shrinkage; the leave-one-state-out check by state.
   - **Compare surfaces**: any district surface against any other (raw multilevel or design-weighted, RC on either input, Bayesian, Bayesian with IRES calibration for 2015), with r and mean differences.
   - **Health sensitivity explorer**: every mortality specification, filterable by outcome, population (rural / all births / rural 36-month window with reduced cohort overlap), adjustment, weighting and calibration-support stratum. Shows CR2, conventional and wild-bootstrap inference, plus estimand (expectation vs predictive), leg-A reference and alternative exposure definitions.
   - **Run checks**: every pipeline check (PASS/WARN/FAIL) and mixed-model fit diagnostics.

Each step opens with a plain-language summary. "How we know" and "Technical details"
panels hold the evidence and methods. The summaries are built from the results files, and
sentences whose direction could change between runs are conditional on the numbers.

## Run locally
```r
install.packages(c("shiny", "bslib", "plotly", "readr", "jsonlite", "sf", "dplyr"))
setwd("app"); shiny::runApp()
```

## Refresh after a pipeline run
After `scripts/export_results_tables.R` has updated `results/`:
```
cd app
Rscript prepare_data.R
```
This copies the needed CSVs into `app/data/` and builds `districts.geojson` from the district
shapefile (polygons only, simplified; set `DISTRICT_SHP` if it is not at
`$DHS_DIR/district_pc11_shrug/nfhs_data.shp`, built by `scripts/build_district_boundaries.R` from the SHRUG 2.2 2011 Census district polygons).

District boundaries: SHRUG 2.2, Development Data Lab (Asher, Lunt, Matsuura and Novosad 2021, *World Bank Economic Review* 35(4)), licensed CC BY-NC-SA 4.0. `data/districts.geojson` is derived from them and is distributed under the same licence.

## Deploy to Posit Connect Cloud (from GitHub)
Connect Cloud needs a `manifest.json` next to `app.R`. Write it with only the app's own files,
so the data-preparation script (which needs `sf`) is not recorded as a dependency:

```r
setwd("~/Downloads/ACCESS_energy/India_householdenergy/app")
install.packages(c("rsconnect", "shiny", "bslib", "plotly", "readr", "jsonlite", "htmltools"))
rsconnect::writeManifest(appFiles = c("app.R", list.files("R", full.names = TRUE),
                                      list.files("www", full.names = TRUE),
                                      list.files("data", full.names = TRUE)))
```

Commit `app/` (including `manifest.json` and `data/`) and push. In Connect Cloud: Publish, then
Shiny, then the repository, branch `main`, primary file `app/app.R`.

To update after a new pipeline run: `Rscript prepare_data.R`, then regenerate `manifest.json`
with the `writeManifest` call above (its file checksums go stale whenever `data/` or the code
changes), then commit and push. Pushing updates the deployed app only if automatic publishing
on push is turned on for this content in Connect Cloud; otherwise republish it by hand there.

The map is drawn as SVG and needs no map tiles. The equations are typeset with MathJax, which
`withMathJax()` loads from a CDN, so the maths panels need internet access in the viewer's browser.

If the exported files combine runs (the provenance label reads `MIXED(n)`), the banner names the
main run and lists which scripts' checks came from a later targeted rerun.
