#!/usr/bin/env bash
set -euo pipefail

# Run from repository root on the Linux compute server.
# Defaults are intentionally modest; override CORE_WORKERS for the instance size.
export CORE_WORKERS="${CORE_WORKERS:-16}"
export CORE_FORCE="${CORE_FORCE:-1}"
export REPRO_SITES="${REPRO_SITES:-TUM}"

echo "[1/5] Restore/pin R environment"
Rscript scripts/00_setup.R

echo "[2/5] Refresh local data inventory"
Rscript scripts/02_inventory.R

echo "[3/5] Reproduce upstream metric pipeline on ${REPRO_SITES}"
Rscript scripts/03_reproduce_upstream.R

echo "[4/5] Validate upstream reproduction"
Rscript scripts/04_validate_reproduction.R

echo "[5/5] Build core artifacts with ${CORE_WORKERS} workers"
CORE_WORKERS="${CORE_WORKERS}" CORE_FORCE="${CORE_FORCE}" Rscript scripts/09_build_core_artifacts.R

echo "Core artifacts complete:"
echo "  data/derived/core/metric_cube.csv.gz"
echo "  data/derived/core/unit_context.csv.gz"
