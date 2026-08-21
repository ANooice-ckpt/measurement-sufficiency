#!/usr/bin/env bash
set -euo pipefail

# One-command Linux build for the high-resolution extraction run.
# Versioned core caches make CORE_FORCE=0 safe after scientific-operator changes.
export CORE_WORKERS="${CORE_WORKERS:-16}"
export CORE_FORCE="${CORE_FORCE:-0}"
export REPRO_SITES="${REPRO_SITES:-TUM}"
export DISABLE_STATIC_LIBV8="${DISABLE_STATIC_LIBV8:-1}"
export MAKEFLAGS="${MAKEFLAGS:--j24}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${PROJECT_ROOT}"

ZAUNER_DIR="external/zauner_position"
ZAUNER_URL="https://github.com/tscnlab/ZaunerDeVriesEtAl_bioRxiv_2026.git"
ZAUNER_TAG="v0.9.9"
ZAUNER_COMMIT="a74ec2acc84258ce87cc85b196f71b3a651522c4"

ensure_zauner_reference() {
  local metric_types="${ZAUNER_DIR}/data/metric_types.xlsx"
  local prepared_metrics="${ZAUNER_DIR}/data/prepared_metrics.RData"
  if [[ -f "${metric_types}" && -f "${prepared_metrics}" && -d "${ZAUNER_DIR}/.git" ]]; then
    local existing_commit
    existing_commit="$(git -C "${ZAUNER_DIR}" rev-parse HEAD 2>/dev/null || true)"
    if [[ "${existing_commit}" == "${ZAUNER_COMMIT}" ]]; then
      return 0
    fi
  fi

  echo "[preflight] Prepare Zauner reference ${ZAUNER_TAG}"
  if [[ -d "${ZAUNER_DIR}/.git" ]]; then
    git -C "${ZAUNER_DIR}" fetch --tags --force origin "${ZAUNER_TAG}"
    git -C "${ZAUNER_DIR}" checkout --detach "${ZAUNER_TAG}"
  else
    local extra
    extra="$(find "${ZAUNER_DIR}" -mindepth 1 -maxdepth 1 ! -name .gitkeep -print -quit 2>/dev/null || true)"
    if [[ -n "${extra}" ]]; then
      echo "ERROR: ${ZAUNER_DIR} exists but is not the expected upstream checkout: ${extra}" >&2
      exit 1
    fi
    local tmp
    tmp="$(mktemp -d)"
    git clone --filter=blob:none --branch "${ZAUNER_TAG}" --depth 1 "${ZAUNER_URL}" "${tmp}/zauner_position"
    mkdir -p "${ZAUNER_DIR}"
    cp -a "${tmp}/zauner_position/." "${ZAUNER_DIR}/"
    rm -rf "${tmp}"
  fi

  local observed
  observed="$(git -C "${ZAUNER_DIR}" rev-parse HEAD)"
  if [[ "${observed}" != "${ZAUNER_COMMIT}" ]]; then
    echo "ERROR: Zauner reference is ${observed}; expected ${ZAUNER_COMMIT}" >&2
    exit 1
  fi
  [[ -f "${metric_types}" && -f "${prepared_metrics}" ]] || {
    echo "ERROR: Zauner reference checkout is missing required data files" >&2
    exit 1
  }
}

ensure_zauner_reference
mkdir -p results/logs results/core results/core/cache
R_VERSION="$(Rscript --vanilla -e 'cat(as.character(getRversion()))')"
if [[ "${R_VERSION}" != "4.5.0" ]]; then
  echo "ERROR: this build requires R 4.5.0; found ${R_VERSION}" >&2
  exit 1
fi

echo "[preflight] Parse all changed scientific and figure entry points before expensive work"
Rscript --vanilla -e 'files <- c(
  "scripts/utils/melidos_io.R", "scripts/utils/protocol_windows.R",
  "scripts/utils/paths.R", "scripts/utils/duration_artifacts.R", "scripts/utils/parallel_runtime.R",
  "scripts/utils/core_artifacts.R", "scripts/utils/core_temporal_sampling.R", "scripts/utils/core_context.R",
  "scripts/utils/figure_style.R", "scripts/utils/rq1_pairwise_artifacts.R",
  "scripts/00_setup.R", "scripts/01_download_melidos.R", "scripts/02_inventory.R",
  "scripts/03_reproduce_upstream.R", "scripts/04_validate_reproduction.R",
  "scripts/04b_validate_era5_inputs.R", "scripts/04c_prepare_raw_eye_spans.R",
  "scripts/09_build_core_artifacts.R", "scripts/10_rq1_analysis.R", "scripts/11_plot_fig1.R",
  "scripts/12_rq2_analysis.R", "scripts/13_plot_rq2.R",
  "scripts/14_rq3_analysis.R", "scripts/15_plot_rq3.R"
); invisible(lapply(files, parse)); cat("R parse preflight passed for", length(files), "files\n")'

echo "[1/8] Restore/pin R 4.5.0 environment"
Rscript scripts/00_setup.R

echo "[2/8] Download/validate MeLiDos inputs and optional metadata"
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
echo "  results/core/metric_cube.csv.gz"
echo "  results/core/unit_context.csv.gz"
echo "  results/core/weather_1min.csv.gz"
echo "  results/core/duration_metric_cube.rds"
echo "Diagnostics: results/diagnostics/core_artifact_summary.csv; results/diagnostics/duration_cohort_audit.csv"
