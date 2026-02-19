#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="/home/fengde/SAIL"
RAT_DIR="${ROOT_DIR}/riscv-arch-test"
CFG_DIR="${RAT_DIR}/riscof-plugins/rv64"
WORK_DIR="${RAT_DIR}/work-zabha"
STATE_FILE="${WORK_DIR}/.auto_stage_state"
STAGE1_TESTLIST="${WORK_DIR}/test_list_stage1.yaml"
STAGE2_TESTLIST="${WORK_DIR}/test_list_stage2.yaml"
STAGE3_TESTLIST="${WORK_DIR}/test_list_stage3.yaml"
STAGE4_TESTLIST="${WORK_DIR}/test_list_stage4.yaml"
PROBE_LOG="${WORK_DIR}/zabha_probe.log"

if [[ ! -x "${ROOT_DIR}/.venv/bin/riscof" ]]; then
  echo "missing ${ROOT_DIR}/.venv/bin/riscof"
  exit 1
fi
if [[ ! -x "${ROOT_DIR}/tools/spike/bin/spike" ]]; then
  echo "missing ${ROOT_DIR}/tools/spike/bin/spike"
  exit 1
fi
if [[ ! -x "${ROOT_DIR}/sail-riscv/build/c_emulator/sail_riscv_sim" ]]; then
  echo "missing ${ROOT_DIR}/sail-riscv/build/c_emulator/sail_riscv_sim"
  exit 1
fi

export PATH="${ROOT_DIR}/tools/spike/bin:${ROOT_DIR}/sail-riscv/build/c_emulator:${PATH}"

mkdir -p "${WORK_DIR}"
cd "${CFG_DIR}"

probe_zabha_support() {
  local src="${WORK_DIR}/.zabha_probe.S"
  local obj="${WORK_DIR}/.zabha_probe.o"
  cat > "${src}" <<'EOF'
.text
.globl _start
_start:
  amoadd.b x1, x2, (x3)
EOF
  if riscv64-unknown-elf-gcc -c -march=rv64imafd_zicsr_zabha -mabi=lp64 "${src}" -o "${obj}" >"${PROBE_LOG}" 2>&1; then
    rm -f "${src}" "${obj}"
    return 0
  fi
  rm -f "${src}" "${obj}"
  return 1
}

stage="${1:-auto}"
if [[ "${stage}" == "auto" ]]; then
  if [[ -f "${STATE_FILE}" ]] && grep -q "^phase4_done$" "${STATE_FILE}"; then
    stage="phase5"
  elif [[ -f "${STATE_FILE}" ]] && grep -q "^phase3_done$" "${STATE_FILE}"; then
    stage="phase4"
  elif [[ -f "${STATE_FILE}" ]] && grep -q "^phase2_done$" "${STATE_FILE}"; then
    stage="phase3"
  elif [[ -f "${STATE_FILE}" ]] && grep -q "^phase1_done$" "${STATE_FILE}"; then
    stage="phase2"
  else
    stage="phase1"
  fi
fi

run_with_testfile() {
  local testfile="$1"
  "${ROOT_DIR}/.venv/bin/riscof" run \
    --config config.ini \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}" \
    --testfile "${testfile}" \
    --no-browser
}

if [[ "${stage}" == "phase1" ]]; then
  echo "[auto] stage1: quick subset (toolchain-aware)"
  "${ROOT_DIR}/.venv/bin/riscof" testlist \
    --config config.ini \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}"

  if probe_zabha_support; then
    FILTER_MODE="zabha"
    echo "[auto] probe: toolchain supports Zabha"
  else
    FILTER_MODE="smoke"
    echo "[auto] probe: toolchain does NOT support Zabha; fallback to smoke subset"
    echo "[auto] probe log: ${PROBE_LOG}"
  fi

  export FILTER_MODE
  python3 - <<'PY'
import yaml
import os
from pathlib import Path

work = Path("/home/fengde/SAIL/riscv-arch-test/work-zabha")
src = work / "test_list.yaml"
dst = work / "test_list_stage1.yaml"
mode = os.environ.get("FILTER_MODE", "zabha")

data = yaml.safe_load(src.read_text()) or {}
filtered = {}
items = list(data.items())

if mode == "zabha":
    for name, entry in items:
        test_path = str(entry.get("test_path", ""))
        if "/Zabha/" in test_path or "/Zacas/" in test_path:
            filtered[name] = entry
else:
    # Fallback smoke set: pick a small number of broadly-supported AMO tests.
    for name, entry in items:
        test_path = str(entry.get("test_path", ""))
        if "/rv64i_m/A/src/" in test_path and "/Zabha/" not in test_path and "/Zacas/" not in test_path:
            filtered[name] = entry
            if len(filtered) >= 24:
                break

if not filtered:
    raise SystemExit(f"stage1 filter produced 0 tests (mode={mode}); aborting")

dst.write_text(yaml.safe_dump(filtered, sort_keys=True))
print(f"stage1_mode={mode}")
print(f"stage1_selected={len(filtered)}")
PY

  run_with_testfile "${STAGE1_TESTLIST}"

  echo "phase1_done" > "${STATE_FILE}"
  echo "[auto] stage1 passed."
  echo "[auto] next: run the same command again for stage2 (compat-core)."
  exit 0
fi

if [[ "${stage}" == "phase2" ]]; then
  echo "[auto] stage2: compat-core subset"
  "${ROOT_DIR}/.venv/bin/riscof" testlist \
    --config config.ini \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}"

  if probe_zabha_support; then
    FILTER_MODE="core_with_zabha"
  else
    FILTER_MODE="core_compat"
    echo "[auto] probe: no Zabha support, using compat-core set"
  fi
  export FILTER_MODE
  python3 - <<'PY'
import os
import yaml
from pathlib import Path

work = Path("/home/fengde/SAIL/riscv-arch-test/work-zabha")
src = work / "test_list.yaml"
dst = work / "test_list_stage2.yaml"
mode = os.environ.get("FILTER_MODE", "core_compat")

data = yaml.safe_load(src.read_text()) or {}
filtered = {}

for name, entry in data.items():
    p = str(entry.get("test_path", ""))
    keep = False
    if "/rv64i_m/A/src/" in p or "/rv64i_m/Zicond/src/" in p:
        keep = True
    if mode == "core_with_zabha" and ("/Zabha/" in p or "/Zacas/" in p):
        keep = True
    if keep:
        filtered[name] = entry

if not filtered:
    raise SystemExit("stage2 filter produced 0 tests")
dst.write_text(yaml.safe_dump(filtered, sort_keys=True))
print(f"stage2_mode={mode}")
print(f"stage2_selected={len(filtered)}")
PY

  run_with_testfile "${STAGE2_TESTLIST}"
  echo "phase2_done" > "${STATE_FILE}"
  echo "[auto] stage2 passed."
  echo "[auto] next: run the same command again for stage3 (compat-wide)."
  exit 0
fi

if [[ "${stage}" == "phase3" ]]; then
  echo "[auto] stage3: compat-wide sample subset"
  "${ROOT_DIR}/.venv/bin/riscof" testlist \
    --config config.ini \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}"

  if probe_zabha_support; then
    FILTER_MODE="wide_all"
  else
    FILTER_MODE="wide_compat"
    echo "[auto] probe: no Zabha support, filtering unsupported extensions"
  fi
  export FILTER_MODE
  python3 - <<'PY'
import os
import yaml
from pathlib import Path

work = Path("/home/fengde/SAIL/riscv-arch-test/work-zabha")
src = work / "test_list.yaml"
dst = work / "test_list_stage3.yaml"
mode = os.environ.get("FILTER_MODE", "wide_compat")
sample_cap = 240

data = yaml.safe_load(src.read_text()) or {}
filtered = {}
blocked_tokens = ("/Zabha/", "/Zacas/", "/Zimop/", "/Zcmop/")

for name, entry in data.items():
    p = str(entry.get("test_path", ""))
    if mode == "wide_compat":
        if any(t in p for t in blocked_tokens):
            continue
    filtered[name] = entry
    if mode == "wide_compat" and len(filtered) >= sample_cap:
        break

if not filtered:
    raise SystemExit("stage3 filter produced 0 tests")
dst.write_text(yaml.safe_dump(filtered, sort_keys=True))
print(f"stage3_mode={mode}")
print(f"stage3_selected={len(filtered)}")
PY

  run_with_testfile "${STAGE3_TESTLIST}"
  echo "phase3_done" > "${STATE_FILE}"
  echo "[auto] stage3 passed."
  echo "[auto] next: run the same command again for stage4 (full-compatible)."
  exit 0
fi

if [[ "${stage}" == "phase4" ]]; then
  echo "[auto] stage4: full-compatible (exclude unsupported extensions when needed)"
  "${ROOT_DIR}/.venv/bin/riscof" testlist \
    --config config.ini \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}"

  if probe_zabha_support; then
    cp "${WORK_DIR}/test_list.yaml" "${STAGE4_TESTLIST}"
    echo "[auto] probe: Zabha supported, stage4 includes all tests"
  else
    python3 - <<'PY'
import yaml
from pathlib import Path

work = Path("/home/fengde/SAIL/riscv-arch-test/work-zabha")
src = work / "test_list.yaml"
dst = work / "test_list_stage4.yaml"
data = yaml.safe_load(src.read_text()) or {}
blocked_tokens = ("/Zabha/", "/Zacas/", "/Zimop/", "/Zcmop/")
filtered = {k: v for k, v in data.items() if not any(t in str(v.get("test_path", "")) for t in blocked_tokens)}
if not filtered:
    raise SystemExit("stage4 filter produced 0 tests")
dst.write_text(yaml.safe_dump(filtered, sort_keys=True))
print(f"stage4_selected={len(filtered)}")
PY
    echo "[auto] probe: no Zabha support, stage4 excludes unsupported extension tests"
  fi

  run_with_testfile "${STAGE4_TESTLIST}"
  echo "phase4_done" > "${STATE_FILE}"
  echo "[auto] stage4 passed."
  echo "[auto] next: run the same command again for stage5 (true full Zabha)."
  exit 0
fi

if [[ "${stage}" == "phase5" ]]; then
  if ! probe_zabha_support; then
    echo "[auto] stage5 blocked: local riscv64-unknown-elf-gcc/binutils do not support Zabha opcodes."
    echo "[auto] detail: see ${PROBE_LOG}"
    exit 3
  fi
  echo "[auto] stage5: full Zabha validation"
  bash "${ROOT_DIR}/scripts/run_riscof_zabha_retry.sh"
  echo "phase5_done" > "${STATE_FILE}"
  echo "[auto] stage5 passed. full flow complete."
  exit 0
fi

echo "unknown stage: ${stage}"
echo "usage: bash scripts/run_riscof_zabha_auto.sh [auto|phase1|phase2|phase3|phase4|phase5]"
exit 2
