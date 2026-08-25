#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"

mkdir -p results/logs results/runtime results/core results/core/cache
LOG="results/logs/full_run_optimized.log"

export DISABLE_STATIC_LIBV8="${DISABLE_STATIC_LIBV8:-1}"
export MAKEFLAGS="${MAKEFLAGS:--j24}"
export RENV_CONFIG_REPOS_OVERRIDE="${RENV_CONFIG_REPOS_OVERRIDE:-https://mirrors.tuna.tsinghua.edu.cn/CRAN/}"
export R_DEFAULT_INTERNET_TIMEOUT="${R_DEFAULT_INTERNET_TIMEOUT:-600}"
export REPRO_SITES="${REPRO_SITES:-TUM}"

export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="${VECLIB_MAXIMUM_THREADS:-1}"
export NUMEXPR_NUM_THREADS="${NUMEXPR_NUM_THREADS:-1}"

# Defaults tuned for the 48-physical-core / ~185-GiB usable ECS node.
export CORE_WORKERS="${CORE_WORKERS:-48}"
export CORE_DURATION_WORKERS="${CORE_DURATION_WORKERS:-32}"
export CORE_FORCE="${CORE_FORCE:-0}"
export RQ1_BOOT="${RQ1_BOOT:-1000}"
export RQ1_STARTUP_WORKERS="${RQ1_STARTUP_WORKERS:-24}"
export RQ1_PART_WORKERS="${RQ1_PART_WORKERS:-44}"
export RQ1_BOOT_WORKERS="${RQ1_BOOT_WORKERS:-40}"
export RQ1_FRAGMENT_WORKERS="${RQ1_FRAGMENT_WORKERS:-12}"
export RQ1_PART_COMPRESSION="${RQ1_PART_COMPRESSION:-gzip}"
export RQ2_WORKERS="${RQ2_WORKERS:-40}"
export RQ2_CV_FOLDS="${RQ2_CV_FOLDS:-5}"
export RQ2_RUN_MODELS="${RQ2_RUN_MODELS:-1}"
export RQ2_KEEP_MODEL_INPUTS="${RQ2_KEEP_MODEL_INPUTS:-0}"
export RQ3_WORKERS="${RQ3_WORKERS:-32}"
export RQ3_PART_WORKERS="${RQ3_PART_WORKERS:-8}"

{
  echo "===== OPTIMIZED FULL RUN START: $(date --iso-8601=seconds) ====="
  echo "Repository: ${ROOT}"
  echo "Host: $(hostname)"
  echo "Commit: $(git rev-parse HEAD)"
  echo "Workers: core=${CORE_WORKERS}; core_duration=${CORE_DURATION_WORKERS}; rq1_startup=${RQ1_STARTUP_WORKERS}; rq1_parts=${RQ1_PART_WORKERS}; rq1_boot=${RQ1_BOOT_WORKERS}; rq1_fragments=${RQ1_FRAGMENT_WORKERS}; rq2=${RQ2_WORKERS}; rq3=${RQ3_WORKERS}; rq3_parts=${RQ3_PART_WORKERS}"
  echo

  echo "===== PREFLIGHT ====="
  command -v python3 >/dev/null || { echo "ERROR: python3 is required" >&2; exit 1; }
  R_VERSION="$(Rscript --vanilla -e 'cat(as.character(getRversion()))')"
  [[ "${R_VERSION}" == "4.5.0" ]] || { echo "ERROR: R 4.5.0 required; found ${R_VERSION}" >&2; exit 1; }
  [[ -s external/zauner_position/data/prepared_metrics.RData ]] || { echo "ERROR: missing Zauner prepared_metrics.RData" >&2; exit 1; }
  [[ -s external/zauner_position/data/metric_types.xlsx ]] || { echo "ERROR: missing Zauner metric_types.xlsx" >&2; exit 1; }
  [[ -d data/raw ]] && [[ -n "$(find data/raw -type f -print -quit 2>/dev/null)" ]] || { echo "ERROR: data/raw is missing or empty" >&2; exit 1; }

  Rscript -e '
    required <- c(LightLogR = "0.10.3", melidosData = "1.0.6")
    for (pkg in names(required)) {
      if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing project package: ", pkg)
      observed <- as.character(packageVersion(pkg))
      if (!identical(observed, required[[pkg]])) stop(pkg, " must be ", required[[pkg]], "; found ", observed)
    }
    cat("Project package preflight passed\n")
  '

  echo "Generate runtime-only optimized RQ1 and corrected downstream v5 entrypoints"
  python3 scripts/utils/build_runtime_optimized.py
  python3 scripts/utils/patch_rq1_memory_safe.py
  python3 scripts/utils/build_downstream_v5_runtime.py
  Rscript --vanilla -e '
    fs <- c(
      "results/runtime/duration_artifacts.optimized.R",
      "results/runtime/09_build_core_artifacts.optimized.R",
      "results/runtime/10_rq1_analysis.optimized.R",
      "results/runtime/12_rq2_analysis_v5.runtime.R",
      "results/runtime/14_rq3_analysis_v5.runtime.R",
      "scripts/utils/rq_context.R",
      "scripts/utils/rq2_context_features.R",
      "scripts/09b_validate_core_design.R",
      "scripts/11_plot_fig1.R",
      "scripts/12_rq2_analysis.R",
      "scripts/12c_rq2_context_models.R",
      "scripts/13_plot_rq2.R",
      "scripts/13_plot_rq2_v5.R",
      "scripts/15_plot_rq3.R",
      "scripts/15_plot_rq3_v5.R"
    )
    invisible(lapply(fs, parse)); cat("Optimized/corrected analysis, layered RQ2 and canonical plotting entry points parse successfully\n")
  '
  echo

  echo "===== CORE PIPELINE ====="
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
  echo "[8/8] Build versioned core artifacts with ${CORE_WORKERS} workers; duration workers=${CORE_DURATION_WORKERS}"
  CORE_WORKERS="${CORE_WORKERS}" CORE_DURATION_WORKERS="${CORE_DURATION_WORKERS}" CORE_FORCE="${CORE_FORCE}" Rscript results/runtime/09_build_core_artifacts.optimized.R
  echo "[audit] Validate core against the frozen measurement design"
  Rscript scripts/09b_validate_core_design.R

  echo "===== RQ1 ====="
  Rscript results/runtime/10_rq1_analysis.optimized.R
  echo "===== FIGURE 1 ====="
  Rscript scripts/11_plot_fig1.R

  echo "===== RQ2 V5 + LAYERED CONTEXT ====="
  # Canonical wrapper sources the corrected runtime and the layered extension in
  # one R process, so all context models reuse the same transition objects.
  Rscript scripts/12_rq2_analysis.R
  echo "===== RQ2 V5 FIGURES ====="
  Rscript scripts/13_plot_rq2.R
  echo "===== RQ3 V5 ====="
  Rscript results/runtime/14_rq3_analysis_v5.runtime.R
  echo "===== RQ3 V5 FIGURES ====="
  Rscript scripts/15_plot_rq3.R

  echo "===== PROVENANCE ====="
  git rev-parse HEAD > results/logs/git_commit.txt
  git status --short > results/logs/git_status_after_run.txt
  Rscript -e 'sessionInfo()' > results/logs/sessionInfo_server.txt
  lscpu > results/logs/server_lscpu.txt
  free -h > results/logs/server_memory.txt
  df -h > results/logs/server_disk.txt
  echo "Results size: $(du -sh results | cut -f1)"
  echo "===== OPTIMIZED FULL RUN COMPLETE: $(date --iso-8601=seconds) ====="
} 2>&1 | tee -a "${LOG}"
