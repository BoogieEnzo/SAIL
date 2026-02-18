#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "[check_min] validating control files"
python3 -m json.tool docs/AGENT_STATE.json >/dev/null
python3 -m json.tool docs/TASK_QUEUE.json >/dev/null

echo "[check_min] ensuring docs are on Zabha mainline"
if rg -n "Ziccrse" docs >/dev/null; then
  echo "[check_min] found unexpected 'Ziccrse' in docs"
  exit 1
fi

echo "[check_min] checking stage acceptance doc exists"
test -f docs/阶段验收标准.md
test -f docs/RUN_LOG.md

echo "[check_min] ok"
