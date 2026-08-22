#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"
mkdir -p results/logs results/runtime results/rq2 results/rq3

export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="${VECLIB_MAXIMUM_THREADS:-1}"
export NUMEXPR_NUM_THREADS="${NUMEXPR_NUM_THREADS:-1}"

export RQ2_WORKERS="${RQ2_WORKERS:-40}"
export RQ2_CV_FOLDS="${RQ2_CV_FOLDS:-5}"
export RQ2_RUN_MODELS="${RQ2_RUN_MODELS:-1}"
# Temporary row-level model shards are resumable during RQ2 but removed after a
# successful run by default to keep the server image compact.
export RQ2_KEEP_MODEL_INPUTS="${RQ2_KEEP_MODEL_INPUTS:-0}"
export RQ3_WORKERS="${RQ3_WORKERS:-32}"
export RQ3_PART_WORKERS="${RQ3_PART_WORKERS:-8}"

LOG="results/logs/downstream_v5.log"
{
  echo "===== DOWNSTREAM V5 START: $(date --iso-8601=seconds) ====="
  echo "Commit: $(git rev-parse HEAD)"
  echo "Workers: rq2=${RQ2_WORKERS}; rq2_cv=${RQ2_CV_FOLDS}; rq3=${RQ3_WORKERS}; rq3_parts=${RQ3_PART_WORKERS}"

  echo "===== BUILD/PARSE CORRECTED RUNTIMES ====="
  python3 scripts/utils/build_downstream_v5_runtime.py
  Rscript --vanilla -e '
    fs <- c(
      "results/runtime/12_rq2_analysis_v5.runtime.R",
      "results/runtime/13_plot_rq2_v5.runtime.R",
      "results/runtime/14_rq3_analysis_v5.runtime.R",
      "results/runtime/15_plot_rq3_v5.runtime.R"
    )
    invisible(lapply(fs, parse))
    cat("All downstream v5 runtime scripts parse successfully\n")
  '

  # Remove only stale final products. Versioned v5 checkpoints/shards are kept
  # so an interrupted RQ2 run remains resumable.
  rm -f \
    results/rq2/rq2_condition_long.rds \
    results/rq2/rq2_conditional_geometry.csv \
    results/rq2/rq2_model_coefficients.csv \
    results/rq2/rq2_model_performance.csv \
    results/rq2/rq2_model_artifact_manifest.csv \
    results/rq2/rq2_gamma_long.rds \
    results/rq2/rq2_gamma_summary.csv \
    results/rq2/rq2_conditional_geometry_gamma.csv \
    results/rq2/rq2_interaction_scope.csv \
    results/rq2/RQ2_RUN_REPORT.md
  rm -rf results/rq2/figures

  rm -f \
    results/rq3/rq3_observed_stability.csv \
    results/rq3/rq3_sufficiency_long.rds \
    results/rq3/rq3_sufficiency_long.csv \
    results/rq3/rq3_single_dimension_requirement.csv \
    results/rq3/rq3_unordered_substitutability.csv \
    results/rq3/rq3_unordered_coverage_curves.csv \
    results/rq3/rq3_convergence_profile.csv \
    results/rq3/rq3_joint_pair_summary.csv \
    results/rq3/rq3_joint_stability.rds \
    results/rq3/rq3_joint_summary.csv \
    results/rq3/rq3_pareto_occupancy.csv \
    results/rq3/rq3_pareto_frontiers.csv \
    results/rq3/rq3_pareto_ever.csv \
    results/rq3/rq3_pareto_frequency.csv \
    results/rq3/RQ3_RUN_REPORT.md
  rm -rf results/rq3/figures

  echo "===== FIGURE 1 (FROZEN RQ1) ====="
  Rscript scripts/11_plot_fig1.R

  echo "===== RQ2 V5 ====="
  Rscript results/runtime/12_rq2_analysis_v5.runtime.R

  echo "===== RQ2 V5 FIGURES ====="
  Rscript results/runtime/13_plot_rq2_v5.runtime.R

  echo "===== RQ3 V5 ====="
  Rscript results/runtime/14_rq3_analysis_v5.runtime.R

  echo "===== RQ3 V5 FIGURES ====="
  Rscript results/runtime/15_plot_rq3_v5.runtime.R

  echo "===== PROVENANCE ====="
  git rev-parse HEAD > results/logs/git_commit.txt
  git status --short > results/logs/git_status_after_run.txt
  Rscript -e 'sessionInfo()' > results/logs/sessionInfo_server.txt
  lscpu > results/logs/server_lscpu.txt
  free -h > results/logs/server_memory.txt
  df -h > results/logs/server_disk.txt
  du -sh results
  echo "===== DOWNSTREAM V5 COMPLETE: $(date --iso-8601=seconds) ====="
} 2>&1 | tee -a "${LOG}"
