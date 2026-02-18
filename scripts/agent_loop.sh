#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MAX_STEPS="${1:-20}"
LOG_FILE="docs/RUN_LOG.md"

now() {
  date -u +"%Y-%m-%dT%H:%M:%SZ"
}

append_log() {
  local msg="$1"
  {
    echo ""
    echo "## $(now) - Loop Event"
    echo ""
    echo "- ${msg}"
  } >>"$LOG_FILE"
}

state_mode="$(python3 - <<'PY'
import json
from pathlib import Path
p=Path("docs/AGENT_STATE.json")
print(json.loads(p.read_text(encoding="utf-8"))["execution_mode"])
PY
)"

allow_dev="$(python3 - <<'PY'
import json
from pathlib import Path
p=Path("docs/AGENT_STATE.json")
print("true" if json.loads(p.read_text(encoding="utf-8"))["allow_development"] else "false")
PY
)"

if [[ "$state_mode" != "run" ]]; then
  echo "[agent_loop] execution_mode=$state_mode, stop."
  append_log "Loop skipped: execution_mode=$state_mode"
  exit 0
fi

for ((i=1; i<=MAX_STEPS; i++)); do
  set +e
  if [[ "$allow_dev" == "true" ]]; then
    next_task="$(scripts/taskctl.py next --allow-development 2>/dev/null)"
  else
    next_task="$(scripts/taskctl.py next 2>/dev/null)"
  fi
  rc=$?
  set -e

  if [[ $rc -ne 0 ]]; then
    echo "[agent_loop] no runnable task."
    append_log "Loop stopped: no runnable task"
    exit 0
  fi

  echo "[agent_loop] step=$i task=$next_task"
  scripts/taskctl.py set-status "$next_task" doing >/dev/null
  scripts/taskctl.py set-state current_task "$next_task" >/dev/null

  cmd="$(python3 - <<'PY' "$next_task"
import json,sys
from pathlib import Path
tid=sys.argv[1]
q=json.loads(Path("docs/TASK_QUEUE.json").read_text(encoding="utf-8"))
t=next(x for x in q["tasks"] if x["id"]==tid)
print(t.get("command",""))
PY
)"

  ok=1
  if [[ -n "$cmd" ]]; then
    if ! bash -lc "$cmd"; then
      ok=0
    fi
  fi

  mapfile -t checks < <(python3 - <<'PY' "$next_task"
import json,sys
from pathlib import Path
tid=sys.argv[1]
q=json.loads(Path("docs/TASK_QUEUE.json").read_text(encoding="utf-8"))
t=next(x for x in q["tasks"] if x["id"]==tid)
for c in t.get("acceptance",[]):
    print(c)
PY
)

  for c in "${checks[@]:-}"; do
    [[ -z "$c" ]] && continue
    if ! bash -lc "$c"; then
      ok=0
      break
    fi
  done

  if [[ "$ok" -eq 1 ]]; then
    scripts/taskctl.py set-status "$next_task" done >/dev/null
    append_log "Task ${next_task} marked done by loop"
  else
    scripts/taskctl.py set-status "$next_task" blocked --note "auto-loop acceptance failed" >/dev/null
    append_log "Task ${next_task} marked blocked by loop"
    exit 1
  fi

  scripts/taskctl.py set-state current_task null >/dev/null
done

append_log "Loop reached max steps=${MAX_STEPS}"
