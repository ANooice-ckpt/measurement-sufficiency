#!/usr/bin/env bash
set -euo pipefail

# One-command Linux build for the final high-resolution extraction run.
export CORE_WORKERS="${CORE_WORKERS:-16}"
export CORE_FORCE="${CORE_FORCE:-1}"
export REPRO_SITES="${REPRO_SITES:-TUM}"
# Ubuntu's V8 package otherwise defaults to downloading a static libv8 build,
# which is slow/unreliable from this ECS. Bootstrap installs libv8-dev.
export DISABLE_STATIC_LIBV8="${DISABLE_STATIC_LIBV8:-1}"

mkdir -p logs

# Use --vanilla for the version probe so project-level renv startup messages
# cannot contaminate the captured version string before the environment restore.
R_VERSION="$(Rscript --vanilla -e 'cat(as.character(getRversion()))')"
if [[ "${R_VERSION}" != "4.5.0" ]]; then
  echo "ERROR: this build requires R 4.5.0; found ${R_VERSION}" >&2
  exit 1
fi

echo "[1/7] Restore/pin R 4.5.0 environment"
Rscript scripts/00_setup.R

echo "[2/7] Refresh local MeLiDos inventory"
Rscript scripts/02_inventory.R

echo "[3/7] Reproduce upstream metric pipeline on ${REPRO_SITES}"
Rscript scripts/03_reproduce_upstream.R

echo "[4/7] Validate upstream reproduction"
Rscript scripts/04_validate_reproduction.R

echo "[5/7] Validate all ERA5 payloads and date coverage"
Rscript scripts/04b_validate_era5_inputs.R

echo "[6/7] Preserve raw near-corneal recording spans"
Rscript scripts/04c_prepare_raw_eye_spans.R

echo "[7/7] Build core + ERA5 artifacts with ${CORE_WORKERS} workers"
CORE_WORKERS="${CORE_WORKERS}" CORE_FORCE="${CORE_FORCE}" Rscript scripts/09_build_core_artifacts.R

echo "Artifact build complete:"
echo "  data/derived/core/metric_cube.csv.gz"
echo "  data/derived/core/unit_context.csv.gz"
echo "  data/derived/core/weather_1min.csv.gz"
echo "Diagnostics:"
echo "  logs/core_artifact_summary.csv"
echo "  logs/era5_input_inventory.csv"
echo "  logs/era5_qc.csv"
echo "  logs/era5_missing_study_dates.csv"
echo "  logs/raw_eye_recording_spans.csv"
