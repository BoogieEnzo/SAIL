#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="/home/fengde/SAIL"
STATE_FILE="${ROOT_DIR}/riscv-arch-test/work-zabha/.auto_stage_state"
RUNNER="${ROOT_DIR}/scripts/run_riscof_zabha_auto.sh"
LOG_DIR="${ROOT_DIR}/docs/auto_phase4_logs"
HEARTBEAT_FILE="${ROOT_DIR}/docs/PH4_HEARTBEAT.md"
TOTAL=48
MAX_STEPS="${1:-}"

mkdir -p "${LOG_DIR}"
cd "${ROOT_DIR}"

update_heartbeat() {
  local now="$1"
  local phase_state="$2"
  local stage_hint="$3"
  local selected="$4"
  local ran="$5"
  local last_log="$6"
  local status="$7"

  local done=0
  if [[ "${phase_state}" =~ ^phase4_([0-9]+)_done$ ]]; then
    done="${BASH_REMATCH[1]}"
  elif [[ "${phase_state}" == "phase4_done" ]]; then
    done="${TOTAL}"
  fi
  local remain=$((TOTAL - done))
  local percent=$((done * 100 / TOTAL))

  cat > "${HEARTBEAT_FILE}" <<EOF
# PH4 Heartbeat

- updated_at: ${now}
- status: ${status}
- total_chunks: ${TOTAL}
- done_chunks: ${done}
- remaining_chunks: ${remain}
- progress: ${percent}%
- current_or_next: ${stage_hint}
- state_file_value: ${phase_state:-missing}
- last_chunk_selected: ${selected}
- last_chunk_result: ${ran}
- last_log: ${last_log}
EOF
}

step=0
while true; do
  if [[ -n "${MAX_STEPS}" ]] && [[ "${MAX_STEPS}" =~ ^[0-9]+$ ]] && (( step >= MAX_STEPS )); then
    echo "[auto-loop] reached max steps=${MAX_STEPS}, stop"
    exit 0
  fi

  state=""
  if [[ -f "${STATE_FILE}" ]]; then
    state="$(cat "${STATE_FILE}")"
  fi

  if [[ "${state}" == "phase4_done" || "${state}" =~ ^phase5_ || "${state}" == "phase5_done" ]]; then
    echo "[auto-loop] phase4 already complete, state=${state}"
    exit 0
  fi

  expected="unknown"
  if [[ "${state}" =~ ^phase4_([0-9]+)_done$ ]]; then
    expected="phase4_$((BASH_REMATCH[1]+1))"
  elif [[ "${state}" == "phase3_done" || -z "${state}" ]]; then
    expected="phase4_1"
  fi

  update_heartbeat "$(date -Iseconds)" "${state}" "${expected}" "n/a" "running" "n/a" "running"

  ts="$(date -Iseconds)"
  run_log_file="${LOG_DIR}/run_${ts//:/-}.log"

  set +e
  bash "${RUNNER}" >"${run_log_file}" 2>&1
  rc=$?
  set -e

  new_state=""
  if [[ -f "${STATE_FILE}" ]]; then
    new_state="$(cat "${STATE_FILE}")"
  fi

  if (( rc != 0 )); then
    err_line="$(tail -n 5 "${run_log_file}" | tr '\n' ' ' | sed 's/[[:space:]]\+/ /g')"

    python3 - <<PY
import json
from pathlib import Path

root = Path("${ROOT_DIR}")
agent = root / "docs/AGENT_STATE.json"
queue = root / "docs/TASK_QUEUE.json"

with agent.open("r", encoding="utf-8") as f:
    a = json.load(f)
a["updated_at"] = "${ts}"
a["current_stage"] = "M3"
a["current_task"] = "M3-004"
a["notes"] = "M3-004 blocked during phase4 auto-loop. failed stage=${expected}, rc=${rc}. check docs/RUN_LOG.md and docs/auto_phase4_logs/."
with agent.open("w", encoding="utf-8") as f:
    json.dump(a, f, ensure_ascii=False, indent=2)
    f.write("\n")

with queue.open("r", encoding="utf-8") as f:
    q = json.load(f)
for t in q.get("tasks", []):
    if t.get("id") == "M3-004":
        t["status"] = "blocked"
        t["notes"] = "blocked in phase4 auto-loop: stage=${expected}, rc=${rc}; see docs/RUN_LOG.md and docs/auto_phase4_logs/."
with queue.open("w", encoding="utf-8") as f:
    json.dump(q, f, ensure_ascii=False, indent=2)
    f.write("\n")
PY

    cat >> docs/RUN_LOG.md <<EOR

## ${ts} - phase4 auto-loop blocked

- Failure command:
  - bash scripts/run_riscof_zabha_auto.sh
- Expected stage:
  - ${expected}
- Exit code:
  - ${rc}
- Key error (tail):
  - ${err_line}
- Next:
  1. inspect ${run_log_file}
  2. fix cause and rerun scripts/auto_phase4_loop.sh
EOR

    update_heartbeat "${ts}" "${new_state}" "${expected}" "n/a" "blocked" "${run_log_file}" "blocked"

    git add docs/AGENT_STATE.json docs/TASK_QUEUE.json docs/RUN_LOG.md
    git commit -m "test(riscof): block in phase4 auto-loop (${expected})" || true
    echo "[auto-loop] blocked at ${expected}, rc=${rc}"
    exit ${rc}
  fi

  chunk=""
  if [[ "${new_state}" =~ ^phase4_([0-9]+)_done$ ]]; then
    chunk="${BASH_REMATCH[1]}"
  elif [[ "${new_state}" == "phase4_done" ]]; then
    chunk="48"
  fi

  if [[ -z "${chunk}" ]]; then
    echo "[auto-loop] unexpected state transition: ${state} -> ${new_state}"
    exit 1
  fi

  selected="$(rg -n "phase4_${chunk}_selected=" "${run_log_file}" 2>/dev/null | tail -n1 | awk -F= '{print $2}' || true)"
  if [[ -z "${selected}" ]]; then
    selected="unknown"
  fi

  ran="$(rg -n "Following [0-9]+ tests have been run" "${run_log_file}" 2>/dev/null | tail -n1 | sed -E 's/.*Following ([0-9]+) tests.*/\1/' || true)"
  if [[ -z "${ran}" ]]; then
    ran="${selected}"
  fi

  next="phase4_$((chunk+1))"
  if (( chunk >= TOTAL )); then
    next="phase5_1"
  fi

  python3 - <<PY
import json
from pathlib import Path

root = Path("${ROOT_DIR}")
agent = root / "docs/AGENT_STATE.json"
queue = root / "docs/TASK_QUEUE.json"

with agent.open("r", encoding="utf-8") as f:
    a = json.load(f)
a["updated_at"] = "${ts}"
a["current_stage"] = "M3"
a["current_task"] = "M3-004"
a["notes"] = "M3-004 phase4 auto-loop active (48 chunks, jobs=2 runtime). phase4_${chunk} passed (${ran}/${ran}), checkpoint phase4_${chunk}_done, next ${next}."
with agent.open("w", encoding="utf-8") as f:
    json.dump(a, f, ensure_ascii=False, indent=2)
    f.write("\n")

with queue.open("r", encoding="utf-8") as f:
    q = json.load(f)
for t in q.get("tasks", []):
    if t.get("id") == "M3-004":
        t["status"] = "in_progress"
        t["notes"] = "phase4 auto-loop active (48 chunks, jobs=2 runtime): latest phase4_${chunk} passed (${ran}/${ran}), checkpoint phase4_${chunk}_done, next ${next}; phase5 remains blocked on non-Zabha toolchain."
with queue.open("w", encoding="utf-8") as f:
    json.dump(q, f, ensure_ascii=False, indent=2)
    f.write("\n")
PY

  cat >> docs/RUN_LOG.md <<EOR

## ${ts} - phase4_${chunk} Result (auto-loop)

- Command:
  - bash scripts/run_riscof_zabha_auto.sh
- Selection/result:
  - phase4_${chunk}_selected=${selected}
  - Test outcome: ${ran}/${ran} Passed
  - Script exit code: 0
- State update:
  - riscv-arch-test/work-zabha/.auto_stage_state = ${new_state}
- Next action:
  1. Continue auto-loop to ${next}.
EOR

  cat >> docs/提示词.md <<EOP

## $(date +%Y-%m-%d) phase4_${chunk} 自动进度
- 已完成：phase4_${chunk}（48 分块）。
- 结果：${ran}/${ran} 通过，状态文件为 ${new_state}。
- 下一步：自动继续到 ${next}。
EOP

  update_heartbeat "${ts}" "${new_state}" "${next}" "${selected}" "${ran}/${ran} Passed" "${run_log_file}" "running"

  python3 -m json.tool docs/AGENT_STATE.json >/dev/null
  python3 -m json.tool docs/TASK_QUEUE.json >/dev/null

  git add docs/AGENT_STATE.json docs/TASK_QUEUE.json docs/RUN_LOG.md docs/提示词.md
  git commit -m "test(riscof): phase4_${chunk} pass in 48-way auto loop" || true

  echo "[auto-loop] phase4_${chunk} done (${ran}/${ran}), next=${next}"

  step=$((step+1))

done
