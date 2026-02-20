#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="/home/fengde/SAIL"
STATE_FILE="${ROOT_DIR}/riscv-arch-test/work-zabha/.auto_stage_state"
RUNNER="${ROOT_DIR}/scripts/run_riscof_zabha_auto.sh"
LOG_DIR="${ROOT_DIR}/docs/auto_phase5_logs"
HEARTBEAT_FILE="${ROOT_DIR}/docs/PH5_HEARTBEAT.md"
TOTAL=48
MAX_STEPS="${1:-}"

mkdir -p "${LOG_DIR}"
cd "${ROOT_DIR}"

update_heartbeat() {
  local now="$1"
  local phase_state="$2"
  local stage_hint="$3"
  local ran="$4"
  local last_log="$5"
  local status="$6"

  local done=0
  if [[ "${phase_state}" =~ ^phase5_([0-9]+)_done$ ]]; then
    done="${BASH_REMATCH[1]}"
  elif [[ "${phase_state}" == "phase5_done" ]]; then
    done="${TOTAL}"
  fi
  local remain=$((TOTAL - done))
  local percent=$((done * 100 / TOTAL))

  cat > "${HEARTBEAT_FILE}" <<EOF
# PH5 Heartbeat

- updated_at: ${now}
- status: ${status}
- total_chunks: ${TOTAL}
- done_chunks: ${done}
- remaining_chunks: ${remain}
- progress: ${percent}%
- current_or_next: ${stage_hint}
- state_file_value: ${phase_state:-missing}
- last_chunk_result: ${ran}
- last_log: ${last_log}
EOF
}

step=0
while true; do
  if [[ -n "${MAX_STEPS}" ]] && [[ "${MAX_STEPS}" =~ ^[0-9]+$ ]] && (( step >= MAX_STEPS )); then
    echo "[auto-phase5] reached max steps=${MAX_STEPS}, stop"
    exit 0
  fi

  state=""
  if [[ -f "${STATE_FILE}" ]]; then
    state="$(cat "${STATE_FILE}")"
  fi

  if [[ "${state}" == "phase5_done" ]]; then
    update_heartbeat "$(date -Iseconds)" "${state}" "done" "phase5 complete" "n/a" "done"
    echo "[auto-phase5] already complete"
    exit 0
  fi

  expected="unknown"
  if [[ "${state}" == "phase4_done" ]]; then
    expected="phase5_1"
  elif [[ "${state}" =~ ^phase5_([0-9]+)_done$ ]]; then
    idx="${BASH_REMATCH[1]}"
    if (( idx >= TOTAL )); then
      expected="done"
    else
      expected="phase5_$((idx + 1))"
    fi
  else
    update_heartbeat "$(date -Iseconds)" "${state}" "phase4_done required" "blocked" "n/a" "blocked"
    echo "[auto-phase5] blocked: current state '${state:-missing}' is not ready for phase5"
    exit 2
  fi

  ts="$(date -Iseconds)"
  run_log_file="${LOG_DIR}/run_${ts//:/-}.log"
  update_heartbeat "${ts}" "${state}" "${expected}" "running" "${run_log_file}" "running"

  set +e
  bash "${RUNNER}" auto >"${run_log_file}" 2>&1
  rc=$?
  set -e

  new_state=""
  if [[ -f "${STATE_FILE}" ]]; then
    new_state="$(cat "${STATE_FILE}")"
  fi

  if (( rc != 0 )); then
    err_line="$(tail -n 5 "${run_log_file}" | tr '\n' ' ' | sed 's/[[:space:]]\+/ /g')"
    update_heartbeat "${ts}" "${new_state}" "${expected}" "blocked (rc=${rc}): ${err_line}" "${run_log_file}" "blocked"
    echo "[auto-phase5] blocked at ${expected}, rc=${rc}"
    exit "${rc}"
  fi

  chunk=""
  if [[ "${new_state}" =~ ^phase5_([0-9]+)_done$ ]]; then
    chunk="${BASH_REMATCH[1]}"
  elif [[ "${new_state}" == "phase5_done" ]]; then
    chunk="${TOTAL}"
  fi

  if [[ -z "${chunk}" ]]; then
    update_heartbeat "${ts}" "${new_state}" "${expected}" "blocked: unexpected state transition" "${run_log_file}" "blocked"
    echo "[auto-phase5] unexpected state transition: ${state} -> ${new_state}"
    exit 1
  fi

  ran="$(rg -n "Following [0-9]+ tests have been run" "${run_log_file}" 2>/dev/null | tail -n1 | sed -E 's/.*Following ([0-9]+) tests.*/\1/' || true)"
  if [[ -z "${ran}" ]]; then
    ran="unknown"
  fi

  next="phase5_$((chunk+1))"
  status="running"
  if (( chunk >= TOTAL )); then
    next="done"
    status="done"
  fi

  update_heartbeat "${ts}" "${new_state}" "${next}" "${ran}/${ran} Passed" "${run_log_file}" "${status}"
  echo "[auto-phase5] phase5_${chunk} done (${ran}/${ran}), next=${next}"

  if [[ "${new_state}" == "phase5_done" ]]; then
    exit 0
  fi

  step=$((step+1))
done
