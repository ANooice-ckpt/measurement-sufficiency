#!/usr/bin/env bash
set -euo pipefail

# Full Linux server entry point: raw validation -> core artifacts -> RQ1/RQ2/RQ3
# -> figures -> provenance. Defaults target the 48-physical-core / 192-GiB ECS
# node; every worker count remains environment-overridable.
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"

mkdir -p results/logs
LOG="results/logs/full_run.log"

export DISABLE_STATIC_LIBV8="${DISABLE_STATIC_LIBV8:-1}"
export MAKEFLAGS="${MAKEFLAGS:--j24}"
export RENV_CONFIG_REPOS_OVERRIDE="${RENV_CONFIG_REPOS_OVERRIDE:-https://mirrors.tuna.tsinghua.edu.cn/CRAN/}"
export R_DEFAULT_INTERNET_TIMEOUT="${R_DEFAULT_INTERNET_TIMEOUT:-600}"
export REPRO_SITES="${REPRO_SITES:-TUM}"

# Never nest BLAS/OpenMP parallelism inside fork/PSOCK workers.
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="${VECLIB_MAXIMUM_THREADS:-1}"
export NUMEXPR_NUM_THREADS="${NUMEXPR_NUM_THREADS:-1}"

export CORE_WORKERS="${CORE_WORKERS:-48}"
export CORE_DURATION_WORKERS="${CORE_DURATION_WORKERS:-32}"
export CORE_FORCE="${CORE_FORCE:-0}"
export RQ1_BOOT="${RQ1_BOOT:-1000}"
export RQ1_STARTUP_WORKERS="${RQ1_STARTUP_WORKERS:-24}"
export RQ1_PART_WORKERS="${RQ1_PART_WORKERS:-44}"
export RQ1_FRAGMENT_WORKERS="${RQ1_FRAGMENT_WORKERS:-12}"
export RQ1_BOOT_WORKERS="${RQ1_BOOT_WORKERS:-40}"
export RQ1_PART_COMPRESSION="${RQ1_PART_COMPRESSION:-gzip}"
export RQ2_WORKERS="${RQ2_WORKERS:-40}"
export RQ2_CV_FOLDS="${RQ2_CV_FOLDS:-5}"
export RQ2_RUN_MODELS="${RQ2_RUN_MODELS:-1}"
export RQ2_KEEP_MODEL_INPUTS="${RQ2_KEEP_MODEL_INPUTS:-0}"
export RQ3_WORKERS="${RQ3_WORKERS:-32}"
export RQ3_PART_WORKERS="${RQ3_PART_WORKERS:-8}"

{
  echo "===== FULL RUN START: $(date --iso-8601=seconds) ====="
  echo "Repository: ${ROOT}"
  echo "Host: $(hostname)"
  echo "Commit: $(git rev-parse HEAD)"
  echo "Physical/logical cores reported by R: $(Rscript -e 'cat(parallel::detectCores(logical=FALSE), "/", parallel::detectCores(logical=TRUE))')"
  echo "Workers: core=${CORE_WORKERS}; duration=${CORE_DURATION_WORKERS}; rq1_parts=${RQ1_PART_WORKERS}; rq1_fragments=${RQ1_FRAGMENT_WORKERS}; rq1_boot=${RQ1_BOOT_WORKERS}; rq2=${RQ2_WORKERS}; rq3=${RQ3_WORKERS}; rq3_parts=${RQ3_PART_WORKERS}"
  echo

  echo "===== PREFLIGHT: PROJECT R ENVIRONMENT ====="
  Rscript -e '
    required_r <- package_version("4.5.0")
    if (getRversion() != required_r) stop("R 4.5.0 required; found ", getRversion())
    required <- c(LightLogR = "0.10.3", melidosData = "1.0.6")
    for (pkg in names(required)) {
      if (!requireNamespace(pkg, quietly = TRUE)) stop("Missing project package: ", pkg)
      observed <- as.character(packageVersion(pkg))
      if (!identical(observed, required[[pkg]])) stop(pkg, " must be ", required[[pkg]], "; found ", observed)
    }
    cat("R/project package preflight passed\n")
  '

  if [[ ! -d data/raw ]] || [[ -z "$(find data/raw -type f -print -quit 2>/dev/null)" ]]; then
    echo "ERROR: data/raw is missing or empty" >&2
    exit 1
  fi
  echo "Raw input size: $(du -sh data/raw | cut -f1)"
  df -h "${ROOT}"
  echo

  echo "===== CORE PIPELINE ====="
  bash scripts/run_core_artifacts.sh

  echo "===== RQ1 ====="
  Rscript scripts/10_rq1_analysis.R

  echo "===== DOWNSTREAM ====="
  bash scripts/run_downstream_server.sh

  echo "===== FULL RUN COMPLETE: $(date --iso-8601=seconds) ====="
} 2>&1 | tee -a "${LOG}"
