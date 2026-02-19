#!/usr/bin/env bash
set -euo pipefail
# Optional: use real gcc (bypass ccache) for builds
export PATH="${ROOT_DIR:-/home/fengde/SAIL}/tools/no_ccache_bin:${PATH:-}"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RAT_DIR="${ROOT_DIR}/riscv-arch-test"
WORK_DIR="${RAT_DIR}/work-zabha"

if [[ ! -x "${ROOT_DIR}/.venv/bin/riscof" ]]; then
  echo "missing ${ROOT_DIR}/.venv/bin/riscof"
  exit 1
fi

if [[ ! -x "/home/fengde/SAIL/tools/spike/bin/spike" ]]; then
  echo "missing spike binary at /home/fengde/SAIL/tools/spike/bin/spike"
  exit 1
fi

if [[ ! -x "/home/fengde/SAIL/sail-riscv/build/c_emulator/sail_riscv_sim" ]]; then
  echo "missing sail_riscv_sim at /home/fengde/SAIL/sail-riscv/build/c_emulator/sail_riscv_sim"
  exit 1
fi

export PATH="/home/fengde/SAIL/tools/spike/bin:/home/fengde/SAIL/sail-riscv/build/c_emulator:${PATH}"

cd "${RAT_DIR}"
mkdir -p "${WORK_DIR}"

echo "[run_riscof_zabha] generate testlist (rv64)"
"${ROOT_DIR}/.venv/bin/riscof" testlist \
  --config riscof-plugins/rv64/config.ini \
  --suite riscv-test-suite \
  --env riscv-test-suite/env \
  --work-dir "${WORK_DIR}" \
  --no-browser

echo "[run_riscof_zabha] run suite (rv64)"
"${ROOT_DIR}/.venv/bin/riscof" run \
  --config riscof-plugins/rv64/config.ini \
  --suite riscv-test-suite \
  --env riscv-test-suite/env \
  --work-dir "${WORK_DIR}" \
  --no-browser

echo "[run_riscof_zabha] done"
