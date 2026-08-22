#!/usr/bin/env bash
set -euo pipefail

# Canonical server downstream entrypoint. The corrected v5 runner contains the
# duration-type RQ2 projection, full-row grouped CV, streamed model inputs,
# type-level RQ3 stability, nested joint duration comparisons and corrected
# Pareto burden direction. Keep one public entrypoint so future runs cannot
# accidentally fall back to the retired v4 scripts.
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"
exec bash scripts/run_downstream_v5_server.sh
