#!/usr/bin/env bash
set -euo pipefail

# One-command Linux build for the final high-resolution extraction run.
export CORE_WORKERS="${CORE_WORKERS:-16}"
export CORE_FORCE="${CORE_FORCE:-1}"
export REPRO_SITES="${REPRO_SITES:-TUM}"

mkdir -p logs

R_VERSION="$(Rscript -e 'cat(as.character(getRversion()))')"
if [[ "${R_VERSION}" != "4.5.0" ]]; then
  echo "ERROR: this build requires R 4.5.0; found ${R_VERSION}" >&2
  exit 1
fi

echo "[1/5] Restore/pin R 4.5.0 environment"
Rscript scripts/00_setup.R

echo "[2/5] Refresh local MeLiDos inventory"
Rscript scripts/02_inventory.R

echo "[3/5] Reproduce upstream metric pipeline on ${REPRO_SITES}"
Rscript scripts/03_reproduce_upstream.R

echo "[4/5] Validate upstream reproduction"
Rscript scripts/04_validate_reproduction.R

echo "[5/5] Build core + ERA5 artifacts with ${CORE_WORKERS} workers"
CORE_WORKERS="${CORE_WORKERS}" CORE_FORCE="${CORE_FORCE}" Rscript scripts/09_build_core_artifacts.R

echo "Artifact build complete:"
echo "  data/derived/core/metric_cube.csv.gz"
echo "  data/derived/core/unit_context.csv.gz"
echo "  data/derived/core/weather_1min.csv.gz"
echo "Diagnostics:"
echo "  logs/core_artifact_summary.csv"
echo "  logs/era5_qc.csv"
echo "  logs/era5_missing_study_dates.csv"
