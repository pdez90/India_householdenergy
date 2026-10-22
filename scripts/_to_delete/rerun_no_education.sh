#!/usr/bin/env bash
# =============================================================================
# rerun_no_education.sh
#
# Re-runs the pipeline after the low-education benchmark was removed (2026-08-01).
#
# WHY A FULL RE-RUN AND NOT A SUBSET
#   Six scripts changed: 01, 06, 08, 13, 15, 17.
#
#   01_prep_nfhs.R no longer derives edu_low, so nfhs_hh_covariates.rds must be
#   rewritten -- otherwise the frame on disk still carries the column and 01's
#   new absence check would be asserting something the file contradicts.
#
#   06_stacking_prediction.R dropped edu_low_bin from the CANDIDATE PREDICTOR
#   SET. That changes the fitted models, so it changes harmonized_frames.rds,
#   the stacking predictions, and everything that reads them: 21 (transfer
#   diagnostics), 15 (variable importance), and SI Tables S9, S12, S13, S14.
#   This is not a cosmetic edit and cannot be re-run in isolation.
#
#   Working the dependency chain by hand is exactly the staleness trap
#   run_everything.R exists to prevent, so this script defers to it.
#
# WHAT IS SKIPPED
#   H3_env_covariates.R (the ambient rasters). It is static, slow, and nothing
#   about education touches it. 01 is ALSO marked static, so this script moves
#   its gate file aside to force it to re-run while leaving H3 skipped.
#
# Usage:  bash rerun_no_education.sh
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")"

REPLICA=/Users/priyanka/Downloads/ACCESS_replica
STAMP=$(date +%Y%m%d_%H%M%S)
LOG=/tmp/no_education_rerun_${STAMP}.log

echo "== 1/5  Confirming the edited scripts are in place ====================="
# A re-run against the OLD scripts would look successful and change nothing.
# Each marker is text that exists only in the post-edit version of that file.
fail=0
check_marker () {  # $1 = file, $2 = marker
  if grep -q -- "$2" "$1"; then
    printf '  OK    %-28s\n' "$1"
  else
    printf '  STALE %-28s  (missing: %s)\n' "$1" "$2"; fail=1
  fi
}
check_marker 01_prep_nfhs.R           "no education column is carried forward"
check_marker 06_stacking_prediction.R "Education was a candidate predictor until"
check_marker 08_si_benchmarks.R       "education is absent from the benchmark set"
check_marker 13_benchmark_maps.R      "every requested benchmark reached the NFHS district table"
check_marker 15_variable_importance.R "Education was a candidate predictor until"
check_marker 17_missingness.R         "Education was listed here until"
check_marker "$REPLICA/build_manuscript.js" "The eduMiss helper lived here until"
if [ "$fail" -ne 0 ]; then
  echo "  ABORTING: at least one file is the pre-edit version." >&2
  exit 1
fi

echo
echo "== 2/5  Forcing 01_prep_nfhs.R to re-run =============================="
# run_everything.R skips a static step when its output already exists. Moving
# the gate file aside is how you ask for 01 without also asking for H3.
mkdir -p "$REPLICA/_superseded_$STAMP"
for f in nfhs_districts.rds nfhs_hh_covariates.rds; do
  if [ -f "$REPLICA/$f" ]; then
    mv "$REPLICA/$f" "$REPLICA/_superseded_$STAMP/$f"
    echo "  moved aside: $f"
  else
    echo "  absent already: $f"
  fi
done
echo "  previous copies kept in _superseded_$STAMP/ -- delete once you are happy."

echo
echo "== 3/5  Full pipeline in dependency order ============================="
echo "  This re-runs 01 through 20 plus the health analyses and rebuilds the"
echo "  manuscript. Expect a long run. Logging to $LOG"
echo
Rscript run_everything.R 2>&1 | tee "$LOG"

echo
echo "== 4/5  Confirming education is gone from the analytic outputs ========"
# 19_ires_access_atlas.R still describes ACCESS/IRES education on purpose (it
# claims no NFHS comparison), so this checks only the files education was
# wrongly propagating through.
edufail=0
for f in missingness_items.csv var_importance_stacking.csv var_importance_effect.csv \
         si_benchmark_agreement.csv benchmark_district_wide.csv; do
  p="$REPLICA/$f"
  if [ ! -f "$p" ]; then printf '  MISSING FILE  %s\n' "$f"; edufail=1; continue; fi
  if head -1 "$p" | grep -qi 'edu' || grep -qi '[,"]\(low \)\?education' "$p"; then
    printf '  STILL PRESENT %s\n' "$f"; edufail=1
  else
    printf '  clean         %s\n' "$f"
  fi
done

echo
echo "  -- the three checks that could never pass, and the two that replace them --"
CHK="$REPLICA/diagnostics/pipeline_checks.csv"
if [ -f "$CHK" ]; then
  echo "  any remaining education-related check rows:"
  grep -i 'educat' "$CHK" || echo "    (none reference education by name)"
  echo
  echo "  the new absence assertions:"
  grep -E 'no education column is carried forward|education is absent from the benchmark set|every requested benchmark reached' "$CHK" \
    || { echo "    NOT FOUND -- 01/08/13 did not run"; edufail=1; }
  echo
  echo "  FAIL rows across the whole run:"
  awk -F',' 'NR>1 && $0 ~ /FAIL/ {print "    " $0}' "$CHK" | cut -c1-160 || true
  echo "  total FAIL rows: $(grep -c 'FAIL' "$CHK" || true)"
else
  echo "  NO CHECK TRAIL -- 20_pipeline_checks.R did not run"; edufail=1
fi

echo
echo "== 5/5  Confirming the manuscript rebuilt without education ==========="
for d in ACCESS_Health_main.docx ACCESS_Health_SI.docx; do
  p="$REPLICA/$d"
  if [ ! -f "$p" ]; then printf '  MISSING %s\n' "$d"; edufail=1; continue; fi
  printf '  %s  %s  modified %s\n' "$d" "$(md5 -q "$p" 2>/dev/null || md5sum "$p" | cut -d' ' -f1)" \
    "$(date -r "$p" '+%Y-%m-%d %H:%M:%S')"
done
echo
echo "  'Low education' occurrences in the rebuilt documents (expect 0 in each):"
for d in ACCESS_Health_main.docx ACCESS_Health_SI.docx; do
  # grep exits 1 on no match, which is the GOOD case here, so || true is load-bearing.
  n=$(unzip -p "$REPLICA/$d" word/document.xml 2>/dev/null | grep -o 'Low education' | wc -l | tr -d ' ' || true)
  printf '    %-24s %s\n' "$d" "$n"
  if [ "$n" != "0" ]; then edufail=1; fi
done
echo
# Table 1 is emitted into the SI document, not the main text -- the main
# documents carry no tables at all. An earlier version of this check grepped
# the main file and reported a false "LOST".
echo "  Table 1 disclosure row must SURVIVE (expect 1 in the SI):"
n=$(unzip -p "$REPLICA/ACCESS_Health_SI.docx" word/document.xml 2>/dev/null \
    | grep -o 'Education of respondent' | wc -l | tr -d ' ' || true)
echo "    Education of respondent / head (in SI): $n"
if [ "$n" = "0" ]; then
  echo "    LOST -- the disclosure row was deleted by mistake"
  edufail=1
fi

echo
if [ "$edufail" -eq 0 ]; then
  echo "== DONE: education is out of the pipeline and the disclosure survived. =="
else
  echo "== DONE WITH PROBLEMS: read the lines marked STILL PRESENT / MISSING. ==" >&2
  exit 1
fi
echo "Full log: $LOG"
