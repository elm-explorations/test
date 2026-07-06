#!/usr/bin/env bash
# Usage: ./run-effectiveness-parallel.sh <workers> <totalSeeds>
# Runs <workers> instances of the effectiveness runner in parallel (addends 0..workers-1),
# merges "  Seed " lines into max_{totalSeeds}_runs.txt, extracts minimal fuzz numbers
# into max_{totalSeeds}.txt, runs the two Python analysis scripts, renames outputs to
# max_{totalSeeds}_histogram.png and max_{totalSeeds}_fit.png.
# Run from tests/ directory.

set -e

if [ $# -lt 2 ]; then
  echo "Usage: $0 <workers> <totalSeeds>" >&2
  exit 1
fi

WORKERS="$1"
TOTAL_SEEDS="$2"
PREFIX="max_${TOTAL_SEEDS}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ANALYSIS_DIR="${SCRIPT_DIR}/src/effectiveness-analysis"
TMPDIR="${TMPDIR:-/tmp}"
WORK_DIR=$(mktemp -d "${TMPDIR}/effectiveness-parallel.XXXXXXXX")
trap 'rm -rf "$WORK_DIR"' EXIT

# Run workers in parallel, each writing to a temp file
echo "Running ${WORKERS} workers in parallel"
for ADDEND in $(seq 0 $((WORKERS - 1))); do
  node "${SCRIPT_DIR}/run-effectiveness.js" "$TOTAL_SEEDS" "$WORKERS" "$ADDEND" > "${WORK_DIR}/out_${ADDEND}.txt" 2>&1 &
done
wait

# Merge outputs and keep only "  Seed " lines
echo "Merging outputs"
cat "${WORK_DIR}"/out_*.txt | grep '  Seed ' > "${ANALYSIS_DIR}/${PREFIX}_runs.txt"

# Extract last number (minimal fuzz) from each line; only "minimal fuzz" lines have a number
echo "Extracting min --fuzz numbers"
grep 'minimal fuzz' "${ANALYSIS_DIR}/${PREFIX}_runs.txt" | awk -F': ' '{print $NF}' > "${ANALYSIS_DIR}/${PREFIX}.txt"

# Run Python scripts from effectiveness-analysis directory (use uv run if available for deps)
echo "Making charts"
cd "${ANALYSIS_DIR}"
uv run python3 runs_histogram.py "${PREFIX}.txt" >/dev/null
uv run python3 runs_histogram_fitted.py "${PREFIX}.txt" >/dev/null

# Rename PNGs to use common prefix
mv -f tries_histogram.png "${PREFIX}_histogram.png"
mv -f tries_fit.png "${PREFIX}_fit.png"

echo "Done. Outputs:"
echo "${ANALYSIS_DIR}/${PREFIX}.txt"
echo "${ANALYSIS_DIR}/${PREFIX}_runs.txt"
echo "${ANALYSIS_DIR}/${PREFIX}_histogram.png"
echo "${ANALYSIS_DIR}/${PREFIX}_fit.png"
