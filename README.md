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
ACCESS_DIR=/path/to/your/data bash run_pipeline.sh
```

`run_pipeline.sh` is the only supported entry point. It sets every path the
scripts need, locates the NSSO-78 unit-data CSV by its header, installs the Node
`docx` module if it is missing, and then runs `scripts/run_everything.R`, which
executes the pipeline in dependency order: data prep → cross-survey comparison
including the NSSO-78 external validation (25/26) → correction → augmentation →
health analyses → figures/atlas/SI diagnostics → manuscript build.

**Do not start from `Rscript run_everything.R`, and do not configure the run by
editing `scripts/00_config.R`.** Several steps (`H1`, `H2`, `H3`, `12`, `22`) are
deliberately standalone and do not source `00_config.R`; they read the same paths
from the environment, via `scripts/paths.R`. Editing `00_config.R` therefore
configures some of the pipeline and leaves the rest unconfigured. Those steps no
longer fall back to a path on the original author's machine — `paths.R` stops
with an actionable message instead — so a partly-configured run now fails rather
than reading the wrong data, but it still fails. `run_pipeline.sh` sets the
variables in one place so that cannot happen.

The variables it sets, all overridable:

| variable | default | what it points at |
|---|---|---|
| `ACCESS_DIR` | parent directory of this repo | ACCESS, IRES and NSSO-78 inputs, and `outputs/` |
| `DHS_DIR` | `~/Downloads/DHS_India` | NFHS RData images, GPS, IR/KR recodes, district shapefile |
| `OUTPUT_DIR` | `$ACCESS_DIR/outputs` | every `.rds`/`.csv`/`.jpeg`/`.docx` the run produces |
| `DIR_NSSO` | `$ACCESS_DIR/NSSO78` | NSS 78th-round unit data (see `scripts/README_NSSO78.md`) |
| `NSSO_HH_CSV` | found by header | the Level-03 household CSV, if auto-detection fails |
| `STRICT_CHECKS` | `FALSE` | `TRUE` stops the run on any failed self-check |
| `START_FROM` | *(none)* | resume at a named script, e.g. `05_correction.R` |
| `BUILD_DOCX` | `TRUE` | `FALSE` stops before the manuscript build |
| `SKIP_STATIC` | `TRUE` | `FALSE` also re-runs `01` (NFHS prep) and `H3` (ambient covariates) |

### The canonical (publication) run

A run intended to produce the submitted manuscript should start from an empty
output directory, regenerate everything including the two slow static steps, and
refuse to finish if any self-check fails:

```bash
ACCESS_DIR=/path/to/your/data
mv "$ACCESS_DIR/outputs" "$ACCESS_DIR/outputs.$(date +%Y%m%d)" 2>/dev/null || true
SKIP_STATIC=FALSE STRICT_CHECKS=TRUE ACCESS_DIR="$ACCESS_DIR" bash run_pipeline.sh
```

The output directory is `$ACCESS_DIR/outputs` (or `OUTPUT_DIR`, if you set it),
which is generally NOT a directory called `outputs` beside this README — so move
the one the pipeline actually writes to, not a same-named folder in the
repository, or the run will append to the previous one's outputs.

Expect roughly six hours, dominated by the Bayesian calibration (`05`), the
stacking models (`06`), the mortality prep (`H1`) and the SI benchmarks (`08`).
At the end, `outputs/diagnostics/pipeline_checks_summary.txt` gives the PASS /
WARN / FAIL tally. Under `STRICT_CHECKS=TRUE` a FAIL stops the run, so reaching
the end means none fired; warnings still need reading, and each one is a known
limitation the manuscript discusses (for example the factor-1.8 disagreement
between the two routes to the 2019 NFHS-side variance).

The manuscript build additionally requires Node.js with the `docx` package;
`run_pipeline.sh` installs it into the output directory if `npm` is available.

### Software environment

Install the R packages from the recorded lockfile rather than by hand, so you get
the versions that produced the published numbers:

```r
install.packages("renv")   # if needed
renv::restore()            # from the repository root
```

`clubSandwich` is REQUIRED, not optional. CR2 with Satterthwaite degrees of
freedom is the primary inference for every state-clustered regression in the
paper, and the manuscript builder reads those columns directly;
`run_everything.R` refuses to start without the package.

`scripts/00_snapshot_env.R` records the exact package versions a run used, and
`run_everything.R` invokes it automatically at the end of every run — so
`renv.lock` and `sessionInfo.txt` are products of the run rather than something
typed by hand. Commit them after a canonical run. The package list it records is
the same vector the preflight enforces (`scripts/required_packages.R`, sourced by
both), so the lockfile cannot drift out of step with what the pipeline demands.

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
