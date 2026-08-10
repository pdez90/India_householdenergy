## build_docx_only.R -- rebuild the manuscript from analysis outputs already on
## disk. Runs NO analysis, reads no raw data, and touches no diagnostics file.
##
##     cd /Users/priyanka/India_energy/scripts
##     Rscript build_docx_only.R
##
## Use this when a pipeline run completed its analysis but failed at the docx
## step -- the outputs in DIR_OUT are current, only the two .docx files lag.
##
## Why this exists rather than START_FROM=20_pipeline_checks.R:
## run_everything.R archives diagnostics/pipeline_checks.csv, model_fits.csv and
## pipeline_warnings.csv to prev_*.csv at the START of every run, including a
## resume, so that two runs' rows can never be blended into one audit trail.
## That is the right behaviour for a resumed ANALYSIS, but resuming purely to
## rebuild a document would demote a complete check trail to prev_* and replace
## it with the handful of rows one step emits. The manuscript build needs none
## of that machinery -- so it is available on its own, and the audit trail from
## the run that produced these numbers stays exactly where it is.
##
## The build itself is NOT duplicated here: both this script and
## run_everything.R call build_manuscript_docx() from build_docx.R, so the
## figure table, the build command and the pass conditions cannot drift apart.

DIR_OUT <- Sys.getenv("DIR_OUT", "/Users/priyanka/Downloads/ACCESS_replica")

.self <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
}, error = function(e) getwd())
source(file.path(.self, "build_docx.R"))

if (!dir.exists(DIR_OUT))
  stop("\n[docx] output folder not found: ", DIR_OUT,
       "\n  Set it explicitly:  DIR_OUT=/path/to/outputs Rscript build_docx_only.R",
       call. = FALSE)

## State plainly what this script does and does not guarantee. It rebuilds the
## document from whatever is in DIR_OUT; it cannot verify that those outputs
## came from a complete run. That is pipeline_checks.csv's job, and the run that
## wrote it is named below so the provenance is visible rather than assumed.
message("[docx] Rebuilding the manuscript from analysis outputs in ", DIR_OUT)
.chk <- file.path(DIR_OUT, "diagnostics", "pipeline_checks.csv")
if (file.exists(.chk)) {
  message("[docx] Analysis outputs last verified by the run that wrote ",
          "diagnostics/pipeline_checks.csv (", format(file.info(.chk)$mtime), ").")
} else {
  message("[docx] NOTE: no diagnostics/pipeline_checks.csv found -- this build ",
          "cannot confirm the outputs it is reading came from a complete run.")
}

build_manuscript_docx(DIR_OUT, resume_cmd = "Rscript build_docx_only.R")

message("\nDONE. ACCESS_Health_main.docx and ACCESS_Health_SI.docx in ", DIR_OUT,
        "\n  now reflect the analysis outputs currently in that folder.",
        "\n  No analysis was re-run and no diagnostics file was modified.")
