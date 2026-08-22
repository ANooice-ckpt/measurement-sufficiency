#!/usr/bin/env bash
set -euo pipefail

# Full Linux server entry point: raw validation -> core artifacts -> RQ1/RQ2/RQ3
# -> figures -> provenance. Designed for an Alibaba Cloud 48-physical-core /
# 192-GiB node, while every worker count remains environment-overridable.
#
# Run from anywhere with:
#   bash scripts/run_full_server.sh
#
# Recommended for long jobs:
#   tmux new -s fullrun
#   bash scripts/run_full_server.sh

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"

mkdir -p results/logs
LOG="results/logs/full_run.log"

# China-friendly package/network defaults. Override before launch if needed.
export DISABLE_STATIC_LIBV8="${DISABLE_STATIC_LIBV8:-1}"
export MAKEFLAGS="${MAKEFLAGS:--j24}"
export RENV_CONFIG_REPOS_OVERRIDE="${RENV_CONFIG_REPOS_OVERRIDE:-https://mirrors.tuna.tsinghua.edu.cn/CRAN/}"
export R_DEFAULT_INTERNET_TIMEOUT="${R_DEFAULT_INTERNET_TIMEOUT:-600}"

# Never nest BLAS/OpenMP parallelism inside PSOCK workers.
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="${VECLIB_MAXIMUM_THREADS:-1}"
export NUMEXPR_NUM_THREADS="${NUMEXPR_NUM_THREADS:-1}"

# Defaults tuned for 48 physical cores / 192 GiB. All are overridable.
export CORE_WORKERS="${CORE_WORKERS:-36}"
export CORE_FORCE="${CORE_FORCE:-0}"
export RQ1_BOOT="${RQ1_BOOT:-1000}"
export RQ1_PART_WORKERS="${RQ1_PART_WORKERS:-36}"
export RQ1_BOOT_WORKERS="${RQ1_BOOT_WORKERS:-24}"
export RQ1_PART_COMPRESSION="${RQ1_PART_COMPRESSION:-gzip}"
export RQ2_WORKERS="${RQ2_WORKERS:-36}"
export RQ2_CV_FOLDS="${RQ2_CV_FOLDS:-5}"
export RQ2_RUN_MODELS="${RQ2_RUN_MODELS:-1}"
export RQ3_WORKERS="${RQ3_WORKERS:-24}"

{
  echo "===== FULL RUN START: $(date --iso-8601=seconds) ====="
  echo "Repository: ${ROOT}"
  echo "Host: $(hostname)"
  echo "Commit: $(git rev-parse HEAD)"
  echo "Physical/logical cores reported by R: $(Rscript -e 'cat(parallel::detectCores(logical=FALSE), "/", parallel::detectCores(logical=TRUE))')"
  echo "Workers: core=${CORE_WORKERS}; rq1_parts=${RQ1_PART_WORKERS}; rq1_boot=${RQ1_BOOT_WORKERS}; rq2=${RQ2_WORKERS}; rq3=${RQ3_WORKERS}"
  echo

  echo "===== PREFLIGHT: PROJECT R ENVIRONMENT ====="
  # Deliberately do NOT use --vanilla here: the repository .Rprofile activates renv.
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
  # Includes setup, MeLiDos validation/inventory, upstream reproduction,
  # ERA5 validation, raw-span preparation and versioned core construction.
  bash scripts/run_core_artifacts.sh

  echo
  echo "===== RQ1 ====="
  # Normal Rscript invocation is intentional so .Rprofile activates renv.
  Rscript scripts/10_rq1_analysis.R

  echo
  echo "===== FIGURE 1 ====="
  Rscript scripts/11_plot_fig1.R

  echo
  echo "===== RQ2 ====="
  Rscript scripts/12_rq2_analysis.R

  echo
  echo "===== RQ2 FIGURES ====="
  Rscript scripts/13_plot_rq2.R

  echo
  echo "===== RQ3 ====="
  Rscript scripts/14_rq3_analysis.R

  echo
  echo "===== RQ3 FIGURES ====="
  Rscript scripts/15_plot_rq3.R

  echo
  echo "===== PROVENANCE ====="
  git rev-parse HEAD > results/logs/git_commit.txt
  git status --short > results/logs/git_status_after_run.txt
  Rscript -e 'sessionInfo()' > results/logs/sessionInfo_server.txt
  lscpu > results/logs/server_lscpu.txt
  free -h > results/logs/server_memory.txt
  df -h > results/logs/server_disk.txt

  echo "Results size: $(du -sh results | cut -f1)"
  echo "===== FULL RUN COMPLETE: $(date --iso-8601=seconds) ====="
} 2>&1 | tee -a "${LOG}"
