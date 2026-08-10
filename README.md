# India Household Energy: comparing, calibrating, and using district LPG estimates

Code for the paper *"Correcting environmental exposure measurement error in national
health surveys using specialized surveys: evidence from India's clean-cooking
transition"* — comparing NFHS district-level primary-LPG
estimates against dedicated energy surveys (ACCESS, IRES) and the NSSO 78th-round
Multiple Indicator Survey, correcting the NFHS estimates with regression calibration and
a Bayesian measurement-error model, predicting fuel stacking, and tracing the
consequences of exposure measurement error for district child-mortality analysis.

## Repository layout

```
scripts/     The full analysis pipeline (R). See scripts/README.md for the
             run order and design decisions, and scripts/README_NSSO78.md for
             the NSSO-78 external-validation module (25/26).
manuscript/  Node.js builder that regenerates the manuscript and SI Word
             documents from the analysis outputs (numbers are interpolated
             from the pipeline's CSVs at build time, never typed by hand).
data/        Small derived data artifacts that are inputs to the pipeline:
             nsso78_district_key.csv maps every NSSO-78 (state, district)
             code pair to its census-2011 district code (with provenance
             and match_type documented per row). 25_prep_nsso78.R resolves
             this file automatically when run from scripts/ (override with
             NSSO_KEY_CSV); it does not need to be copied elsewhere.
```

## Reproducing the analysis

```bash
cd scripts
Rscript run_everything.R
```

`run_everything.R` runs the whole pipeline in dependency order (data prep →
cross-survey comparison, including the NSSO-78 external validation (25/26) →
correction → augmentation → health analyses → figures/atlas/SI diagnostics →
manuscript build). Before the first run, edit the paths at the top of
`scripts/00_config.R` to point at your local copies of the survey microdata,
set `DIR_NSSO`/`NSSO_HH_CSV` for the NSSO-78 unit data (see
`scripts/README_NSSO78.md`), and see the `<-- CHECK` markers listed in
`scripts/README.md`.

The manuscript build additionally requires Node.js with the `docx` package
(`cd <output-dir> && npm install docx`).

## Data access (not redistributed here)

The survey microdata are not redistributable and must be obtained from their
providers:

- **NFHS-4 (2015-16) and NFHS-5 (2019-21)** — DHS Program household recodes and
  cluster GPS, https://dhsprogram.com (registration required).
- **ACCESS Waves 1-2 (2015, 2018)** — Zhang & Urpelainen replication archive /
  CEEW-Aklin dataverse releases.
- **IRES (2020)** — CEEW India Residential Energy Survey, Harvard Dataverse
  (doi:10.7910/DVN/U8NYUP).
- **NSSO 78th round Multiple Indicator Survey (2020-21)** — unit-level data from
  the MoSPI microdata portal, https://microdata.gov.in. The household record
  used is the Level-03 file (Questionnaire 5.1, Block 4 items 1-28).
- **District geography** — NFHS-4 (2011 Census) district shapefile.

## Verification philosophy

Every script ends in a `CHECKS` block (see `scripts/checks.R`): weight rules are
validated against published totals (e.g., the NSSO-78 weights reproduce the MoSPI
press-note clean-fuel shares to <0.2 pp), district linkages write diagnostics and
unmatched lists to disk, and the manuscript builder reads every quoted number
back out of the analysis outputs so a re-run updates the documents.
