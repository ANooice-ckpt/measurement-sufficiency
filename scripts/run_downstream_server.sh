#!/usr/bin/env bash
set -euo pipefail

# Server-oriented downstream runner.  RQ1 is the expensive pairwise stage;
# its duration parts are immutable and resumable.  RQ2 models are checkpointed
# by upstream RQ1 version, so an interrupted run does not refit completed
# groups.  Plot scripts only consume frozen outputs.
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
export VROOM_TEMP_PATH="${VROOM_TEMP_PATH:-${TMPDIR:-/tmp}}"
export RQ1_BOOT="${RQ1_BOOT:-1000}"
export RQ1_PART_WORKERS="${RQ1_PART_WORKERS:-16}"
export RQ1_BOOT_WORKERS="${RQ1_BOOT_WORKERS:-8}"
export RQ1_PART_COMPRESSION="${RQ1_PART_COMPRESSION:-gzip}"
export RQ2_WORKERS="${RQ2_WORKERS:-12}"
export RQ2_CV_FOLDS="${RQ2_CV_FOLDS:-5}"
export RQ2_RUN_MODELS="${RQ2_RUN_MODELS:-1}"
export RQ3_WORKERS="${RQ3_WORKERS:-8}"

mkdir -p results/logs

echo "[preflight] R parse"
Rscript --vanilla -e 'files <- c(list.files("scripts/utils", pattern="[.]R$", full.names=TRUE), file.path("scripts", c("09_build_core_artifacts.R", "10_rq1_analysis.R", "11_plot_fig1.R", "12_rq2_analysis.R", "13_plot_rq2.R", "14_rq3_analysis.R", "15_plot_rq3.R"))); invisible(lapply(files[file.exists(files)], parse)); cat("R parse preflight passed\n")'

echo "[1/6] RQ1 partitioned pairwise: parts=${RQ1_PART_WORKERS}, bootstrap=${RQ1_BOOT_WORKERS}"
Rscript --vanilla scripts/10_rq1_analysis.R
echo "[2/6] Fig. 1"
Rscript --vanilla scripts/11_plot_fig1.R
echo "[3/6] RQ2 conditionality/gamma/models: workers=${RQ2_WORKERS}"
Rscript --vanilla scripts/12_rq2_analysis.R
echo "[4/6] RQ2 figures"
Rscript --vanilla scripts/13_plot_rq2.R
echo "[5/6] RQ3 stability/sufficiency/Pareto"
Rscript --vanilla scripts/14_rq3_analysis.R
echo "[6/6] RQ3 figures"
Rscript --vanilla scripts/15_plot_rq3.R

echo "Downstream analysis complete; copy results/ including checkpoints and manifests."
