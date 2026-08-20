#!/usr/bin/env bash
set -euo pipefail

# One-command Linux build for the high-resolution extraction run.
# Versioned core caches make CORE_FORCE=0 safe after scientific-operator changes.
export CORE_WORKERS="${CORE_WORKERS:-16}"
export CORE_FORCE="${CORE_FORCE:-0}"
export REPRO_SITES="${REPRO_SITES:-TUM}"
export DISABLE_STATIC_LIBV8="${DISABLE_STATIC_LIBV8:-1}"
export MAKEFLAGS="${MAKEFLAGS:--j24}"

mkdir -p logs
R_VERSION="$(Rscript --vanilla -e 'cat(as.character(getRversion()))')"
if [[ "${R_VERSION}" != "4.5.0" ]]; then
  echo "ERROR: this build requires R 4.5.0; found ${R_VERSION}" >&2
  exit 1
fi

echo "[preflight] Parse all changed scientific and figure entry points before expensive work"
Rscript --vanilla -e 'files <- c(
  "scripts/utils/melidos_io.R", "scripts/utils/protocol_windows.R",
  "scripts/utils/core_artifacts.R", "scripts/utils/core_temporal_sampling.R", "scripts/utils/core_context.R",
  "scripts/utils/figure_style.R",
  "scripts/01_download_melidos.R", "scripts/04c_prepare_raw_eye_spans.R",
  "scripts/09_build_core_artifacts.R", "scripts/10_rq1_analysis.R", "scripts/11_plot_fig1.R",
  "scripts/12_rq2_analysis.R", "scripts/13_plot_rq2.R",
  "scripts/14_rq3_analysis.R", "scripts/15_plot_rq3.R"
); invisible(lapply(files, parse)); cat("R parse preflight passed for", length(files), "files\n")'

echo "[1/8] Restore/pin R 4.5.0 environment"
Rscript scripts/00_setup.R

echo "[2/8] Download/validate MeLiDos inputs, including trial_times"
Rscript scripts/01_download_melidos.R

echo "[3/8] Refresh local MeLiDos inventory"
Rscript scripts/02_inventory.R

echo "[4/8] Reproduce upstream metric pipeline on ${REPRO_SITES}"
Rscript scripts/03_reproduce_upstream.R

echo "[5/8] Validate upstream reproduction"
Rscript scripts/04_validate_reproduction.R

echo "[6/8] Validate all ERA5 payloads and date coverage"
Rscript scripts/04b_validate_era5_inputs.R

echo "[7/8] Preserve raw spans and protocol trial metadata"
Rscript scripts/04c_prepare_raw_eye_spans.R

echo "[8/8] Build versioned core artifacts with ${CORE_WORKERS} workers"
CORE_WORKERS="${CORE_WORKERS}" CORE_FORCE="${CORE_FORCE}" Rscript scripts/09_build_core_artifacts.R

echo "Artifact build complete:"
echo "  data/derived/core/metric_cube.csv.gz"
echo "  data/derived/core/unit_context.csv.gz"
echo "  data/derived/core/weather_1min.csv.gz"
echo "  data/derived/core/core_manifest.csv"
echo "Diagnostics: logs/core_artifact_summary.csv; logs/protocol_participant_metadata.csv"
