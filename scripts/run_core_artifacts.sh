#!/usr/bin/env bash
set -euo pipefail

# One-command Linux build for the high-resolution extraction run.
# Versioned core caches make CORE_FORCE=0 safe after scientific-operator changes.
export CORE_WORKERS="${CORE_WORKERS:-16}"
export CORE_FORCE="${CORE_FORCE:-0}"
export REPRO_SITES="${REPRO_SITES:-TUM}"
export DISABLE_STATIC_LIBV8="${DISABLE_STATIC_LIBV8:-1}"
export MAKEFLAGS="${MAKEFLAGS:--j24}"

mkdir -p results/logs results/core results/core/cache
R_VERSION="$(Rscript --vanilla -e 'cat(as.character(getRversion()))')"
if [[ "${R_VERSION}" != "4.5.0" ]]; then
  echo "ERROR: this build requires R 4.5.0; found ${R_VERSION}" >&2
  exit 1
fi

echo "[preflight] Parse all scientific and figure entry points before expensive work"
Rscript --vanilla -e 'files <- c(
  "scripts/utils/analysis_design.R",
  "scripts/utils/melidos_io.R", "scripts/utils/protocol_windows.R",
  "scripts/utils/paths.R", "scripts/utils/duration_artifacts.R", "scripts/utils/parallel_runtime.R",
  "scripts/utils/core_artifacts.R", "scripts/utils/core_temporal_sampling.R", "scripts/utils/core_context.R",
  "scripts/utils/figure_style.R", "scripts/utils/rq1_pairwise_artifacts.R",
  "scripts/utils/rq_context.R", "scripts/utils/rq2_context_features.R",
  "scripts/01_download_melidos.R", "scripts/04c_prepare_raw_eye_spans.R",
  "scripts/09_build_core_artifacts.R", "scripts/09b_validate_core_design.R",
  "scripts/10_rq1_analysis.R", "scripts/11_plot_fig1.R",
  "scripts/12_rq2_analysis.R", "scripts/12_rq2_analysis_v5.R", "scripts/12c_rq2_context_models.R",
  "scripts/13_plot_rq2.R", "scripts/13_plot_rq2_v5.R",
  "scripts/14_rq3_analysis.R", "scripts/14_rq3_analysis_v5.R",
  "scripts/15_plot_rq3.R", "scripts/15_plot_rq3_v5.R",
  "scripts/resume_rq3_v5_after_joint.R"
); invisible(lapply(files, parse)); cat("R parse preflight passed for", length(files), "files\n")'
python3 -m py_compile scripts/utils/build_downstream_v5_runtime.py

echo "[1/8] Restore/pin R 4.5.0 environment"
Rscript scripts/00_setup.R

echo "[2/8] Download/validate MeLiDos inputs, including exercise diary and trial_times"
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

echo "[audit] Validate core against the frozen measurement design"
Rscript scripts/09b_validate_core_design.R

echo "Artifact build complete:"
echo "  results/core/metric_cube.csv.gz"
echo "  results/core/unit_context.csv.gz"
echo "  results/core/weather_1min.csv.gz"
echo "  results/core/duration_metric_cube.rds"
echo "Diagnostics: results/diagnostics/core_artifact_summary.csv; results/diagnostics/core_design_audit.csv; results/diagnostics/duration_cohort_audit.csv"
