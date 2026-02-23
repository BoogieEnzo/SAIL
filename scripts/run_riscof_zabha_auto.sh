#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${SAIL_ROOT:-/home/fengde/SAIL}"
export SAIL_ROOT="${ROOT_DIR}"
RAT_DIR="${ROOT_DIR}/riscv-arch-test"
CFG_DIR="${RAT_DIR}/riscof-plugins/rv64"
WORK_DIR="${RAT_DIR}/work-zabha"
STATE_FILE="${WORK_DIR}/.auto_stage_state"
BASE_CFG_FILE="${CFG_DIR}/config.ini"
RUNTIME_CFG_FILE="${CFG_DIR}/config.runtime.ini"
STAGE1_TESTLIST="${WORK_DIR}/test_list_stage1.yaml"
STAGE2_TESTLIST="${WORK_DIR}/test_list_stage2.yaml"
STAGE3_TESTLIST="${WORK_DIR}/test_list_stage3.yaml"
STAGE4_TESTLIST="${WORK_DIR}/test_list_stage4.yaml"
STAGE5_TESTLIST="${WORK_DIR}/test_list_stage5.yaml"
PROBE_LOG="${WORK_DIR}/zabha_probe.log"
PHASE4_CHUNKS=48
PHASE5_CHUNKS=48
RISCOF_JOBS="${RISCOF_JOBS:-2}"
LOCAL_TOOLCHAIN_BIN="${LOCAL_TOOLCHAIN_BIN:-${ROOT_DIR}/tools/riscv-zabha/bin}"

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
if [[ -d "${LOCAL_TOOLCHAIN_BIN}" ]]; then
  export PATH="${LOCAL_TOOLCHAIN_BIN}:${PATH}"
  echo "[auto] using local toolchain bin: ${LOCAL_TOOLCHAIN_BIN}"
fi

mkdir -p "${WORK_DIR}"
cd "${CFG_DIR}"

prepare_runtime_config() {
  python3 - <<PY
from pathlib import Path

src = Path("${BASE_CFG_FILE}")
dst = Path("${RUNTIME_CFG_FILE}")
jobs = "${RISCOF_JOBS}"

lines = src.read_text(encoding="utf-8").splitlines()

def upsert_jobs(section_name: str):
    header = f"[{section_name}]"
    start = -1
    for i, line in enumerate(lines):
        if line.strip() == header:
            start = i
            break
    if start < 0:
        return
    end = len(lines)
    for i in range(start + 1, len(lines)):
        if lines[i].strip().startswith("[") and lines[i].strip().endswith("]"):
            end = i
            break
    for i in range(start + 1, end):
        if lines[i].strip().startswith("jobs="):
            lines[i] = f"jobs={jobs}"
            return
    lines.insert(end, f"jobs={jobs}")

upsert_jobs("spike_simple")
upsert_jobs("sail_cSim")
dst.write_text("\n".join(lines) + "\n", encoding="utf-8")
PY
}

prepare_runtime_config

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
  state_val=""
  if [[ -f "${STATE_FILE}" ]]; then
    state_val="$(cat "${STATE_FILE}")"
  fi
  if [[ "${state_val}" == "phase5_done" ]]; then
    stage="done"
  elif [[ "${state_val}" =~ ^phase5_([0-9]+)_done$ ]]; then
    idx="${BASH_REMATCH[1]}"
    if (( idx >= PHASE5_CHUNKS )); then
      stage="done"
    else
      stage="phase5_$((idx + 1))"
    fi
  elif [[ "${state_val}" == "phase4_done" ]]; then
    stage="phase5_1"
  elif [[ "${state_val}" =~ ^phase4_([0-9]+)_done$ ]]; then
    idx="${BASH_REMATCH[1]}"
    if (( idx >= PHASE4_CHUNKS )); then
      stage="phase5_1"
    else
      stage="phase4_$((idx + 1))"
    fi
  elif [[ "${state_val}" == "phase3_done" ]]; then
    stage="phase4_1"
  elif [[ "${state_val}" == "phase2_done" ]]; then
    stage="phase3"
  elif [[ "${state_val}" == "phase1_done" ]]; then
    stage="phase2"
  else
    stage="phase1"
  fi
fi

run_with_testfile() {
  local testfile="$1"
  "${ROOT_DIR}/.venv/bin/riscof" run \
    --config "${RUNTIME_CFG_FILE}" \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}" \
    --testfile "${testfile}" \
    --no-browser
}

prepare_stage4_subtasks() {
  "${ROOT_DIR}/.venv/bin/riscof" testlist \
    --config "${RUNTIME_CFG_FILE}" \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}"

  if probe_zabha_support; then
    cp "${WORK_DIR}/test_list.yaml" "${STAGE4_TESTLIST}"
    echo "[auto] probe: Zabha supported, stage4 includes all tests"
  else
    python3 - <<'PY'
import os
import yaml
from pathlib import Path

work = Path(os.environ.get("SAIL_ROOT", "/home/fengde/SAIL")) / "riscv-arch-test" / "work-zabha"
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

  python3 - <<'PY'
import os
import math
import yaml
from pathlib import Path

work = Path(os.environ.get("SAIL_ROOT", "/home/fengde/SAIL")) / "riscv-arch-test" / "work-zabha"
src = work / "test_list_stage4.yaml"
data = yaml.safe_load(src.read_text()) or {}
items = sorted(data.items(), key=lambda kv: kv[0])
total = len(items)
if total == 0:
    raise SystemExit("stage4 split source has 0 tests")
chunk_count = 48
chunk_size = math.ceil(total / chunk_count)

for idx in range(chunk_count):
    start = idx * chunk_size
    end = min((idx + 1) * chunk_size, total)
    chunk = dict(items[start:end])
    out = work / f"test_list_stage4_{idx + 1}.yaml"
    out.write_text(yaml.safe_dump(chunk, sort_keys=True))
    print(f"stage4_{idx + 1}_selected={len(chunk)}")
PY
}

prepare_stage5_subtasks() {
  if ! probe_zabha_support; then
    echo "[auto] stage5 blocked: local riscv64-unknown-elf-gcc/binutils do not support Zabha opcodes."
    echo "[auto] detail: see ${PROBE_LOG}"
    return 3
  fi

  "${ROOT_DIR}/.venv/bin/riscof" testlist \
    --config "${RUNTIME_CFG_FILE}" \
    --suite "${RAT_DIR}/riscv-test-suite" \
    --env "${RAT_DIR}/riscv-test-suite/env" \
    --work-dir "${WORK_DIR}"

  python3 - <<'PY'
import os
import math
import yaml
from pathlib import Path

work = Path(os.environ.get("SAIL_ROOT", "/home/fengde/SAIL")) / "riscv-arch-test" / "work-zabha"
src = work / "test_list.yaml"
dst = work / "test_list_stage5.yaml"
data = yaml.safe_load(src.read_text()) or {}
filtered = {
    k: v for k, v in data.items()
    if "/Zabha/" in str(v.get("test_path", "")) or "/Zacas/" in str(v.get("test_path", ""))
}
if not filtered:
    raise SystemExit("stage5 filter produced 0 tests")
dst.write_text(yaml.safe_dump(filtered, sort_keys=True))

items = sorted(filtered.items(), key=lambda kv: kv[0])
total = len(items)
chunk_size = math.ceil(total / 48)
print(f"stage5_selected={total}")
for idx in range(48):
    start = idx * chunk_size
    end = min((idx + 1) * chunk_size, total)
    chunk = dict(items[start:end])
    out = work / f"test_list_stage5_{idx + 1}.yaml"
    out.write_text(yaml.safe_dump(chunk, sort_keys=True))
    print(f"stage5_{idx + 1}_selected={len(chunk)}")
PY
}

if [[ "${stage}" == "phase4" ]]; then
  stage="phase4_1"
  if [[ -f "${STATE_FILE}" ]]; then
    state_val="$(cat "${STATE_FILE}")"
    if [[ "${state_val}" =~ ^phase4_([0-9]+)_done$ ]]; then
      idx="${BASH_REMATCH[1]}"
      if (( idx < PHASE4_CHUNKS )); then
        stage="phase4_$((idx + 1))"
      else
        stage="phase5_1"
      fi
    elif [[ "${state_val}" == "phase4_done" ]]; then
      stage="phase5_1"
    fi
  fi
fi

if [[ "${stage}" == "phase5" ]]; then
  stage="phase5_1"
  if [[ -f "${STATE_FILE}" ]]; then
    state_val="$(cat "${STATE_FILE}")"
    if [[ "${state_val}" =~ ^phase5_([0-9]+)_done$ ]]; then
      idx="${BASH_REMATCH[1]}"
      if (( idx < PHASE5_CHUNKS )); then
        stage="phase5_$((idx + 1))"
      else
        stage="done"
      fi
    elif [[ "${state_val}" == "phase5_done" ]]; then
      stage="done"
    fi
  fi
fi

if [[ "${stage}" == "phase1" ]]; then
  echo "[auto] stage1: quick subset (toolchain-aware)"
  "${ROOT_DIR}/.venv/bin/riscof" testlist \
    --config "${RUNTIME_CFG_FILE}" \
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

work = Path(os.environ.get("SAIL_ROOT", "/home/fengde/SAIL")) / "riscv-arch-test" / "work-zabha"
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
    --config "${RUNTIME_CFG_FILE}" \
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

work = Path(os.environ.get("SAIL_ROOT", "/home/fengde/SAIL")) / "riscv-arch-test" / "work-zabha"
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
    --config "${RUNTIME_CFG_FILE}" \
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

work = Path(os.environ.get("SAIL_ROOT", "/home/fengde/SAIL")) / "riscv-arch-test" / "work-zabha"
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

if [[ "${stage}" =~ ^phase4_([0-9]+)$ ]]; then
  chunk_idx="${BASH_REMATCH[1]}"
  if (( chunk_idx < 1 || chunk_idx > PHASE4_CHUNKS )); then
    echo "unknown stage: ${stage}"
    echo "usage: bash scripts/run_riscof_zabha_auto.sh [auto|phase1|phase2|phase3|phase4|phase4_1..phase4_${PHASE4_CHUNKS}|phase5|phase5_1..phase5_${PHASE5_CHUNKS}]"
    exit 2
  fi
  echo "[auto] ${stage}: full-compatible chunk ${chunk_idx}/${PHASE4_CHUNKS}"
  prepare_stage4_subtasks

  chunk_file="${WORK_DIR}/test_list_stage4_${chunk_idx}.yaml"
  chunk_count="$(python3 - <<PY
import yaml
from pathlib import Path
p = Path("${chunk_file}")
d = yaml.safe_load(p.read_text()) if p.exists() else {}
print(len(d or {}))
PY
)"
  if [[ "${chunk_count}" == "0" ]]; then
    echo "[auto] ${stage}: chunk empty, skip run"
  else
    run_with_testfile "${chunk_file}"
  fi

  echo "phase4_${chunk_idx}_done" > "${STATE_FILE}"
  if (( chunk_idx == PHASE4_CHUNKS )); then
    echo "phase4_done" > "${STATE_FILE}"
    echo "[auto] stage4 all chunks passed."
    echo "[auto] next: run the same command again for stage5 (true full Zabha)."
  else
    next_chunk=$((chunk_idx + 1))
    echo "[auto] ${stage} passed."
    echo "[auto] next: run the same command again for phase4_${next_chunk}."
  fi
  exit 0
fi

if [[ "${stage}" =~ ^phase5_([0-9]+)$ ]]; then
  chunk_idx="${BASH_REMATCH[1]}"
  if (( chunk_idx < 1 || chunk_idx > PHASE5_CHUNKS )); then
    echo "unknown stage: ${stage}"
    echo "usage: bash scripts/run_riscof_zabha_auto.sh [auto|phase1|phase2|phase3|phase4|phase4_1..phase4_${PHASE4_CHUNKS}|phase5|phase5_1..phase5_${PHASE5_CHUNKS}]"
    exit 2
  fi
  echo "[auto] ${stage}: full Zabha chunk ${chunk_idx}/${PHASE5_CHUNKS}"
  prepare_stage5_subtasks || exit $?

  chunk_file="${WORK_DIR}/test_list_stage5_${chunk_idx}.yaml"
  chunk_count="$(python3 - <<PY
import yaml
from pathlib import Path
p = Path("${chunk_file}")
d = yaml.safe_load(p.read_text()) if p.exists() else {}
print(len(d or {}))
PY
)"
  if [[ "${chunk_count}" == "0" ]]; then
    echo "[auto] ${stage}: chunk empty, skip run"
  else
    run_with_testfile "${chunk_file}"
  fi

  echo "phase5_${chunk_idx}_done" > "${STATE_FILE}"
  if (( chunk_idx == PHASE5_CHUNKS )); then
    echo "phase5_done" > "${STATE_FILE}"
    echo "[auto] stage5 all chunks passed. full flow complete."
  else
    next_chunk=$((chunk_idx + 1))
    echo "[auto] ${stage} passed."
    echo "[auto] next: run the same command again for phase5_${next_chunk}."
  fi
  exit 0
fi

if [[ "${stage}" == "done" ]]; then
  echo "[auto] all phases already marked done."
  exit 0
fi

echo "unknown stage: ${stage}"
echo "usage: bash scripts/run_riscof_zabha_auto.sh [auto|phase1|phase2|phase3|phase4|phase4_1..phase4_${PHASE4_CHUNKS}|phase5|phase5_1..phase5_${PHASE5_CHUNKS}]"
exit 2
