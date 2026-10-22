#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# run_pipeline.sh -- the canonical entry point for the whole analysis.
#
#   bash run_pipeline.sh                         # data root = parent of this repo
#   ACCESS_DIR=/path/to/data bash run_pipeline.sh
#   STRICT_CHECKS=TRUE bash run_pipeline.sh      # publication run: stop on any FAIL
#   START_FROM=05_correction.R bash run_pipeline.sh
#   BUILD_DOCX=FALSE bash run_pipeline.sh
#
# WHY THIS FILE EXISTS
#   Every path the R scripts need is an environment variable with a fallback, and
#   the fallbacks point at one author's laptop. Several scripts (H1, H2, H3, 12,
#   22) are deliberately standalone and do NOT source 00_config.R, so editing
#   that file -- which earlier versions of the README told you to do -- does not
#   reach them. The result was an undocumented dependency on environment
#   variables that nothing in the repository set. This script sets all of them,
#   in one place, and is the only supported way to start a run.
#
# EXPECTED DATA LAYOUT under $ACCESS_DIR (none of it is redistributable; see the
# "Data access" section of README.md for where each piece comes from):
#   dataverse/CEEWACCESS20152018_Appended.dta   ACCESS waves 1-2
#   dataverse/weights.dta                       ACCESS village weights
#   ires/IRES.dta                               IRES 2019-20
#   NSSO78/                                     NSS 78th-round unit data (any layout)
#   outputs/                                    created if absent; everything lands here
# and under $DHS_DIR:
#   the NFHS-4/NFHS-5 RData images, GPS files, IR/KR recodes and the district
#   shapefile (see scripts/README.md).
# ------------------------------------------------------------------------------
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Data root: $ACCESS_DIR if set, else the directory containing this repo. That
# default matches the documented layout without hard-coding anyone's home.
export ACCESS_DIR="${ACCESS_DIR:-$(cd "$REPO/.." && pwd)}"
export DHS_DIR="${DHS_DIR:-$HOME/Downloads/DHS_India}"

# One run id shared by every Rscript this run launches. checks.R keys check-trail
# staleness on the RUN rather than the R session, because a script that runs more
# than once (H2 three times, H4 twice) would otherwise have each invocation clear
# the previous one's rows -- which silently reduced the trail to the last variant
# and, on 2026-09-16, erased a genuine FAIL before the summary consolidated.
# run_everything.R happens to do all of them in one session, so this is belt and
# braces here; it is load-bearing for any script that launches Rscript per step.
export CHK_RUN_ID="${CHK_RUN_ID:-$(date +%Y%m%d%H%M%S)-$$}"
export OUTPUT_DIR="${OUTPUT_DIR:-$ACCESS_DIR/outputs}"
export DIR_OUT="$OUTPUT_DIR"          # build_docx_only.R spelling
export DIR_NSSO="${DIR_NSSO:-$ACCESS_DIR/NSSO78}"

# macOS R caps its own heap well below physical RAM; 01_prep_nfhs.R loads ~3.5 GB
# of RData that expands several-fold. This is a ceiling, not an allocation.
export R_MAX_VSIZE="${R_MAX_VSIZE:-100Gb}"

# NSSO-78 Level-03 household CSV: NSSO_HH_CSV if set, else found by header. Both
# the original descriptive-header download and the 2026 MoSPI short-name
# re-release ("b4q1".."b4q28") are recognised.
if [ -z "${NSSO_HH_CSV:-}" ] && [ -d "$DIR_NSSO" ]; then
  while IFS= read -r -d '' f; do
    if head -c 20000 "$f" | grep -qi "primary source of energy used for cooking" \
       || head -c 20000 "$f" | grep -q "b4q16.*b4q28"; then
      export NSSO_HH_CSV="$f"; break
    fi
  done < <(find "$DIR_NSSO" -iname "*.csv" -print0 | sort -z)
fi
if [ -z "${NSSO_HH_CSV:-}" ]; then
  echo "!! No NSSO-78 Level-03 household CSV found under $DIR_NSSO"
  echo "   Scripts 25/26 and SI Section S8 will fail. See scripts/README_NSSO78.md."
fi

mkdir -p "$OUTPUT_DIR"

# The docx builder must sit in OUTPUT_DIR beside a node_modules/docx.
# Kept, though scripts/build_docx.R now stages these itself before every
# build. This used to be the ONLY place they were copied, which meant
# build_docx_only.R rebuilt from whatever builder the last full run left
# behind. Belt and braces: the copy is cheap and the failure it prevents is
# silent.
cp "$REPO/manuscript/build_manuscript.js" "$OUTPUT_DIR/"
cp "$REPO/manuscript/package.json"        "$OUTPUT_DIR/"
# `npm install docx` (with the package NAME) resolves the newest release and
# ignores the version in package.json -- so a clean run would silently upgrade
# the document renderer. `npm install` with no argument installs exactly what
# package.json pins, which is the whole point of pinning it.
if [ ! -d "$OUTPUT_DIR/node_modules/docx" ] && command -v npm >/dev/null; then
  (cd "$OUTPUT_DIR" && npm install --silent)
fi

echo "ACCESS_DIR    = $ACCESS_DIR"
echo "DHS_DIR       = $DHS_DIR"
echo "OUTPUT_DIR    = $OUTPUT_DIR"
echo "DIR_NSSO      = $DIR_NSSO"
echo "NSSO_HH_CSV   = ${NSSO_HH_CSV:-<not set>}"
echo "STRICT_CHECKS = ${STRICT_CHECKS:-FALSE}"

cd "$REPO/scripts"
exec Rscript run_everything.R 2>&1 | tee "$OUTPUT_DIR/run_log.txt"
