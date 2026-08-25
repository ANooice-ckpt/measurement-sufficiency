#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"
mkdir -p results/logs results/runtime

export RQ1_BOOT="${RQ1_BOOT:-1000}"
export RQ1_STARTUP_WORKERS="${RQ1_STARTUP_WORKERS:-24}"
export RQ1_PART_WORKERS="${RQ1_PART_WORKERS:-44}"
# Fragment checkpoints are deliberately kept below the main pairwise worker count
# because each worker reads and summarizes a large immutable RDS part.
export RQ1_FRAGMENT_WORKERS="${RQ1_FRAGMENT_WORKERS:-12}"
export RQ1_BOOT_WORKERS="${RQ1_BOOT_WORKERS:-40}"
export RQ1_PART_COMPRESSION="${RQ1_PART_COMPRESSION:-gzip}"

export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export MKL_NUM_THREADS="${MKL_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="${VECLIB_MAXIMUM_THREADS:-1}"
export NUMEXPR_NUM_THREADS="${NUMEXPR_NUM_THREADS:-1}"

LOG="results/logs/rq1_memory_safe.log"
{
  echo "===== RQ1 MEMORY-SAFE RESUME: $(date --iso-8601=seconds) ====="
  echo "Commit: $(git rev-parse HEAD)"
  echo "Workers: startup=${RQ1_STARTUP_WORKERS}; parts=${RQ1_PART_WORKERS}; fragments=${RQ1_FRAGMENT_WORKERS}; boot=${RQ1_BOOT_WORKERS}"

  python3 scripts/utils/build_runtime_optimized.py
  python3 scripts/utils/patch_rq1_memory_safe.py
  Rscript --vanilla -e 'parse("results/runtime/10_rq1_analysis.optimized.R"); cat("Memory-safe RQ1 runtime parses successfully\n")'

  Rscript results/runtime/10_rq1_analysis.optimized.R
  echo "===== RQ1 MEMORY-SAFE COMPLETE: $(date --iso-8601=seconds) ====="
} 2>&1 | tee -a "${LOG}"
