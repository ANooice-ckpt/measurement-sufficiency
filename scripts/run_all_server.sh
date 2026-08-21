#!/usr/bin/env bash
set -euo pipefail

# Fresh-server entry point: build core artifacts, then run every downstream
# analysis and figure stage in the documented order.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${PROJECT_ROOT}"

bash scripts/run_core_artifacts.sh
bash scripts/run_downstream_server.sh

echo "Full server build complete; copy results/ and logs/ back from the server."
