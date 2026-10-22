#!/usr/bin/env bash
# Targeted rerun of the supplementary adult analysis (H4) after the
# residence-flag fix, then rebuild, export and app refresh. Stops at the first
# failure, so nothing downstream runs on stale or partial results. Does NOT
# commit: review the sample-by-sex table and Table S5 first.
set -euo pipefail
cd "$(dirname "$0")"
export DHS_DIR="${DHS_DIR:-$HOME/Downloads/DHS_India}"
export ACCESS_DIR="${ACCESS_DIR:-$HOME/Downloads/ACCESS_energy}"
export OUTPUT_DIR="${OUTPUT_DIR:-$ACCESS_DIR/outputs}"
export R_MAX_VSIZE="${R_MAX_VSIZE:-100Gb}"   # as run_pipeline.sh sets it; H4 needs it standalone
# One check-trail id for every Rscript below (as run_pipeline.sh does), so the
# all-household H4 run does not replace the rural H4 run's check rows.
export CHK_RUN_ID="${CHK_RUN_ID:-adult-$(date +%Y%m%d%H%M%S)-$$}"

echo "== 1/6 hypertension verification =="
Rscript verify_hypertension_selfreport.R
cp "$OUTPUT_DIR/diagnostics/adult_hypertension_verification.csv" \
   "$OUTPUT_DIR/diagnostics/adult_hypertension_verification_status.csv" ../records/adult_outcomes/

echo "== 2/6 H4 rural =="
Rscript -e 'POP <- "rural"; source("H4_si_adult_health.R")'
echo "== 3/6 H4 all households =="
Rscript -e 'POP <- "all"; source("H4_si_adult_health.R")'
echo "== sample by sex (rural; all households) =="
cat "$OUTPUT_DIR/diagnostics/adult_sample_by_sex.csv"
cat "$OUTPUT_DIR/diagnostics/adult_sample_by_sex_all.csv"

echo "== 4/6 rebuild manuscript =="
Rscript build_docx_only.R
echo "== 5/6 export results =="
Rscript export_results_tables.R
echo "== 6/6 app data =="
(cd ../app && Rscript prepare_data.R)

echo
echo "ADULT RERUN COMPLETE. Review the sample-by-sex tables and Table S5, then commit."
