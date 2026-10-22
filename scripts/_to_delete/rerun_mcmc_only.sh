#!/usr/bin/env bash
# =============================================================================
# rerun_mcmc_only.sh
#
# Re-fits ONLY the two Bayesian calibration models in 05_correction.R, after
# the sampler settings were changed (iter 6000 -> 12000, adapt_delta 0.995 ->
# 0.999) to clear the three MCMC checks that failed on 2026-08-01:
#
#   no divergent transitions   nfhs4_access = 1      (want 0)
#   max Rhat < 1.01            nfhs5_ires   = 1.020  (want < 1.01)
#   min bulk ESS > 400         nfhs5_ires   =  304.3 (want > 400)
#
# THIS IS A DIAGNOSTIC RUN, NOT A COMPLETE PIPELINE RUN.
#   05 rewrites corrected_nfhs_districts.rds, so every script downstream of it
#   is stale the moment this finishes. Nothing here is publishable on its own.
#   If the checks pass, run the full pipeline from 05 onward (the command is
#   printed at the end). If they do not, restore the backup, also printed.
#
# WHY THE CACHED FITS ARE MOVED ASIDE
#   brms's file_refit = "on_change" compares the Stan code, the Stan data and
#   the algorithm. It does NOT compare iter, chains or control. Leaving the
#   cached .rds in place would silently reload the OLD fit and reprint the OLD
#   diagnostics -- the run would look successful and change nothing.
#
# Usage:  bash rerun_mcmc_only.sh
# =============================================================================
set -euo pipefail
cd "$(dirname "$0")"

REPLICA=/Users/priyanka/Downloads/ACCESS_replica
STAMP=$(date +%Y%m%d_%H%M%S)
LOG=/tmp/mcmc_rerun_${STAMP}.log
CHK="$REPLICA/diagnostics/pipeline_checks.csv"

echo "== 1/4  Confirming the new sampler settings are in place =============="
fail=0
check_marker () {  # $1 = marker, $2 = human description
  if grep -q -- "$1" 05_correction.R; then
    printf '  OK    %s\n' "$2"
  else
    printf '  STALE %s  (missing: %s)\n' "$2" "$1"; fail=1
  fi
}
check_marker "iter = 12000"             "iter = 12000"
check_marker "adapt_delta = 0.999"      "adapt_delta = 0.999"
if [ "$fail" -ne 0 ]; then
  echo "  ABORTING: 05_correction.R is the pre-edit version." >&2
  exit 1
fi

echo
echo "== 2/4  Moving the cached brms fits aside ============================="
# See the header: without this the sampler changes have no effect.
mkdir -p "$REPLICA/_superseded_$STAMP"
moved=0
for f in brms_me_nfhs4_access.rds brms_me_nfhs5_ires.rds brms_me_nfhs4_access_wt.rds; do
  if [ -f "$REPLICA/$f" ]; then
    sz=$(du -h "$REPLICA/$f" | cut -f1)
    mv "$REPLICA/$f" "$REPLICA/_superseded_$STAMP/$f"
    echo "  moved aside: $f  ($sz)"
    moved=$((moved+1))
  else
    echo "  absent already: $f"
  fi
done
if [ "$moved" -eq 0 ]; then
  echo "  WARNING: neither cached fit was found. If 05 has never run in this"
  echo "  folder that is fine; otherwise check REPLICA is the right path." >&2
fi
echo "  previous fits kept in _superseded_$STAMP/ -- delete once you are happy."
echo
echo "  NOTE: the new fits will be roughly TWICE the size of the old ones"
echo "  (the pair was about 1.2 GB), and adapt_delta 0.999 makes each"
echo "  transition slower. Budget appreciably longer than the last 05 run."

# Row count before the run, so step 4 can show ONLY this run's checks.
# A standalone script APPENDS to pipeline_checks.csv; only run_everything.R
# archives it and starts clean.
# ([ -f ] && x=... would return 1 when the file is absent, and set -e would
# abort on it. The if block is load-bearing, not style.)
before=0
if [ -f "$CHK" ]; then before=$(wc -l < "$CHK" | tr -d ' '); fi

echo
echo "== 3/4  Re-fitting =================================================="
echo "  Logging to $LOG"
echo
Rscript 05_correction.R 2>&1 | tee "$LOG"

echo
echo "== 4/4  The three MCMC checks ========================================"
if [ ! -f "$CHK" ]; then
  echo "  NO CHECK TRAIL -- 05 did not reach its check block." >&2
  exit 1
fi
after=$(wc -l < "$CHK" | tr -d ' ')
echo "  check rows appended by this run: $((after - before))"
echo
awk -v b="$before" 'NR > b' "$CHK" \
  | grep -E 'divergent|Rhat|bulk ESS' \
  | sed 's/^/    /' | cut -c1-160
echo
mcmcfail=$(awk -v b="$before" 'NR > b' "$CHK" \
           | grep -E 'divergent|Rhat|bulk ESS' | grep -c 'FAIL' || true)
echo "  --- everything else 05 checked this run ---"
# Captured into a variable rather than piped to `|| echo`: cut exits 0 on empty
# input, so the fallback branch of a pipeline would never fire.
others=$(awk -v b="$before" 'NR > b' "$CHK" \
         | grep -Ev 'divergent|Rhat|bulk ESS' \
         | grep -E 'FAIL|WARN' | cut -c1-160 || true)
if [ -n "$others" ]; then
  echo "$others" | sed 's/^/    /'
else
  echo "    (no other FAIL or WARN rows)"
fi

echo
if [ "$mcmcfail" = "0" ]; then
  cat <<MSG
== CLEARED: all three MCMC checks pass. ==

Downstream outputs are now STALE. Finish the job with:

  cd /Users/priyanka/India_energy/scripts && START_FROM=05_correction.R Rscript run_everything.R

That re-runs 05 (instantly -- it reloads the fits this run just cached) and
everything after it, and rebuilds both documents.
MSG
else
  cat <<MSG
== NOT CLEARED: $mcmcfail of the three MCMC checks still fail. ==

Send me the block above before running anything else. Do NOT run the full
pipeline yet -- 05 has already rewritten corrected_nfhs_districts.rds, so the
folder is mid-change either way.

To put the sampler settings back exactly as they were:

  cp /Users/priyanka/India_energy/scripts/_to_delete/_mcmc_bak/05_correction.R.bak \\
     /Users/priyanka/India_energy/scripts/05_correction.R

MSG
fi
echo "Full log: $LOG"
