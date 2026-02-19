#!/usr/bin/env bash
set -euo pipefail

ROOT="/home/fengde/SAIL"
RAT="$ROOT/riscv-arch-test"
WORK="$RAT/work-zabha"
STAMP="$(date +%Y%m%d_%H%M%S)"
LOG="$RAT/work-zabha-run-$STAMP.log"

echo "[retry] root=$ROOT"
echo "[retry] log=$LOG"

if [[ -d "$WORK" ]]; then
  echo "[retry] archiving old work dir -> ${WORK}.bak.$STAMP"
  mv "$WORK" "${WORK}.bak.$STAMP"
fi

mkdir -p "$WORK"

echo "[retry] start run_riscof_zabha.sh"
bash "$ROOT/scripts/run_riscof_zabha.sh" | tee "$LOG"
rc=${PIPESTATUS[0]}

echo "[retry] exit_code=$rc"
echo "[retry] log=$LOG"
exit "$rc"
