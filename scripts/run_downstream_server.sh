#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"
mkdir -p results/logs results/rq2 results/rq3

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

  echo "===== PARSE CANONICAL DOWNSTREAM SOURCES ====="
  Rscript --vanilla -e '
    fs <- c(
      "scripts/utils/analysis_design.R",
      "scripts/utils/rq_context.R",
      "scripts/utils/rq2_context_features.R",
      "scripts/utils/plot_rq1_common.R",
      "scripts/utils/plot_rq2_common.R",
      "scripts/utils/plot_rq3_common.R",
      "scripts/11_plot_fig1.R",
      "scripts/12_rq2_analysis.R",
      "scripts/12c_rq2_context_models.R",
      "scripts/13a_plot_fig2.R",
      "scripts/13b_plot_fig3.R",
      "scripts/14_rq3_analysis.R",
      "scripts/15a_plot_fig4.R",
      "scripts/15b_plot_fig5.R",
      "scripts/16_plot_supplementary.R"
    )
    invisible(lapply(fs, parse))
    cat("All canonical downstream analysis and plotting sources parse successfully\n")
  '

  echo "===== STRUCTURAL PREFLIGHT ====="
  Rscript -e '
    suppressPackageStartupMessages(library(tidyverse))
    source("scripts/utils/analysis_design.R")
    source("scripts/utils/rq1_pairwise_artifacts.R")
    manifest <- readRDS("results/rq1/rq1_pairwise_change_long.rds")
    if (!is.list(manifest) || !identical(manifest$artifact_type, "partitioned_rq1_pairwise_change")) {
      stop("RQ1 pairwise artifact is not the required partitioned manifest")
    }
    if (is.null(manifest$analysis_design_id) ||
        !identical(as.character(manifest$analysis_design_id[[1]]), ms_analysis_design_id())) {
      stop("RQ1 pairwise artifact does not match the frozen analysis design")
    }
    paths <- file.path(manifest$part_dir, manifest$parts)
    if (!length(paths) || any(!file.exists(paths))) stop("One or more canonical RQ1 parts are missing")
    s <- readr::read_csv("results/rq1/rq1_pairwise_summary.csv", show_col_types = FALSE, progress = FALSE)
    rq1_assert_summary_version(manifest, s)
    d <- s |> filter(dimension == "duration")
    if (!nrow(d)) stop("RQ1 duration summary missing")
    if (any(grepl("__to__", d$comparison_pair_id, fixed = TRUE))) stop("Concrete duration window ids remain in RQ1 summary")
    duration_days <- ms_primary_duration_days()
    expected_duration_types <- choose(length(duration_days), 2L)
    expected_adjacent_duration <- length(duration_days) - 1L
    if (dplyr::n_distinct(d$comparison_pair_id) != expected_duration_types) {
      stop("Expected ", expected_duration_types, " duration comparison types")
    }
    da <- d |> filter(adjacent_transition)
    if (dplyr::n_distinct(da$comparison_pair_id) != expected_adjacent_duration) {
      stop("Expected ", expected_adjacent_duration, " adjacent duration comparison types")
    }
    t <- s |> filter(dimension == "temporal")
    expected_temporal_types <- choose(length(ms_primary_temporal_s()), 2L)
    expected_adjacent_temporal <- length(ms_primary_temporal_s()) - 1L
    if (dplyr::n_distinct(t$comparison_pair_id) != expected_temporal_types) {
      stop("Expected ", expected_temporal_types, " temporal comparison types")
    }
    ta <- t |> filter(adjacent_transition)
    if (dplyr::n_distinct(ta$comparison_pair_id) != expected_adjacent_temporal) {
      stop("Expected ", expected_adjacent_temporal, " adjacent temporal comparison types")
    }
    if (any(s$A_mean_absolute + 1e-12 < abs(s$B_mean_signed), na.rm = TRUE)) stop("RQ1 A >= |B| invariant failed")
    cat(
      "Structural preflight passed: design=", ms_analysis_design_id(),
      "; parts=", length(paths),
      "; temporal types=", expected_temporal_types,
      "; duration types=", expected_duration_types, "\n", sep = ""
    )
  '

  # Every figure is regenerated from frozen RQ outputs. Clear the actual
  # RQ-specific figure directories so retired filenames cannot survive a run.
  rm -rf results/rq1/figures results/rq2/figures results/rq3/figures
  mkdir -p results/rq1/figures results/rq2/figures results/rq3/figures

  # Remove only stale final products. Versioned v5 checkpoints/shards are kept
  # so an interrupted RQ2 run remains resumable.
  rm -f \
    results/rq2/rq2_condition_long.rds \
    results/rq2/rq2_conditional_geometry.csv \
    results/rq2/rq2_model_coefficients.csv \
    results/rq2/rq2_model_performance.csv \
    results/rq2/rq2_model_artifact_manifest.csv \
    results/rq2/rq2_layered_context_model_coefficients.csv \
    results/rq2/rq2_layered_context_model_performance.csv \
    results/rq2/rq2_layered_context_model_manifest.csv \
    results/rq2/rq2_layered_context_model_input_shard_manifest.csv \
    results/rq2/rq2_layered_context_run_report.txt \
    results/rq2/rq2_gamma_long.rds \
    results/rq2/rq2_gamma_summary.csv \
    results/rq2/rq2_conditional_geometry_gamma.csv \
    results/rq2/rq2_interaction_scope.csv \
    results/rq2/RQ2_RUN_REPORT.md

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

  echo "===== FIGURE 1 (FROZEN RQ1) ====="
  Rscript scripts/11_plot_fig1.R

  echo "===== RQ2 + LAYERED CONTEXT ====="
  # The canonical RQ2 entrypoint and layered extension share one R process and
  # therefore one set of validated canonical transition objects.
  Rscript scripts/12_rq2_analysis.R

  echo "===== FIGURE 2 ====="
  Rscript scripts/13a_plot_fig2.R

  echo "===== FIGURE 3 ====="
  Rscript scripts/13b_plot_fig3.R

  echo "===== RQ3 ====="
  Rscript scripts/14_rq3_analysis.R

  echo "===== FIGURE 4 ====="
  Rscript scripts/15a_plot_fig4.R

  echo "===== FIGURE 5 ====="
  Rscript scripts/15b_plot_fig5.R

  echo "===== SUPPLEMENTARY FIGURES ====="
  Rscript scripts/16_plot_supplementary.R

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
