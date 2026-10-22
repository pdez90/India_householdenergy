#!/usr/bin/env bash
# =============================================================================
# rerun_frame_counts.sh
#
# Re-runs the two prep scripts whose analytic-frame counts the manuscript now
# reads from disk, then rebuilds the manuscript from the refreshed CSVs.
#
# Only 02 and 03 changed, and neither changes access_hh.rds / ires_hh.rds, so
# nothing downstream (04, 05, 06, H1-H6) needs re-running.
#
# Usage:  bash rerun_frame_counts.sh
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")"

REPLICA=/Users/priyanka/Downloads/ACCESS_replica
LOG=/tmp/frame_counts_rerun

echo "== 1/5  Clearing the previous 02/03 rows from the check trail =========="
# checks.R appends unconditionally, so without this the SI pipeline-checks table
# would list every 02/03 check twice.
Rscript -e 'source("00_config.R"); f <- file.path(dir_out, "diagnostics", "pipeline_checks.csv"); if (file.exists(f)) { d <- read.csv(f, stringsAsFactors = FALSE); n0 <- nrow(d); d <- d[!d$script %in% c("02", "03"), , drop = FALSE]; write.csv(d, f, row.names = FALSE); cat(sprintf("  pipeline_checks.csv: %d -> %d rows\n", n0, nrow(d))) } else cat("  no check trail yet\n")'

echo
echo "== 2/5  02_prep_access.R ==============================================="
Rscript 02_prep_access.R 2>&1 | tee "${LOG}_02.log"

echo
echo "== 3/5  03_prep_ires.R ================================================="
Rscript 03_prep_ires.R 2>&1 | tee "${LOG}_03.log"

echo
echo "== 4/5  Confirming the new frame counts reached the linkage CSVs ========"
grep -E 'Wave 1 households|Wave 2 households|Wave 1 districts|Wave 2 districts|re-listed' "$REPLICA/access_linkage_diagnostics.csv" || { echo "  MISSING -- 02 did not write them"; exit 1; }
grep -E 'sampling units|distinct NFHS-4 districts|IRES states|households rural'        "$REPLICA/ires_linkage_diagnostics.csv"   || { echo "  MISSING -- 03 did not write them"; exit 1; }

echo
echo "== 5/5  20_pipeline_checks.R, then rebuilding the manuscript ==========="
Rscript 20_pipeline_checks.R 2>&1 | tee "${LOG}_20.log"
node "$REPLICA/build_manuscript.js"

echo
echo "Done. Any line beginning '[build] MISSING' above means a count did not"
echo "reach the document and it will read NA there. '[build] PINNED' lines are"
echo "expected: those are the values that legitimately are not CSV-derived."
echo "Logs: ${LOG}_02.log, ${LOG}_03.log, ${LOG}_20.log"
