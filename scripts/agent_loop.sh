#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MAX_STEPS="${1:-20}"
LOG_FILE="docs/RUN_LOG.md"
CONTEXT_BRIEF="docs/CONTEXT_BRIEF.md"

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

refresh_context_brief() {
  local ts
  ts="$(now)"
  {
    echo "# Context Brief"
    echo ""
    echo "- refreshed_at: ${ts}"
    echo ""
    echo "## Agent State"
    cat docs/AGENT_STATE.json
    echo ""
    echo "## Pending Tasks"
    python3 - <<'PY'
import json
from pathlib import Path
q=json.loads(Path("docs/TASK_QUEUE.json").read_text(encoding="utf-8"))
for t in q["tasks"]:
    if t["status"] in ("pending","doing","blocked"):
        print(f"- {t['id']} [{t['status']}] {t['title']}")
PY
    echo ""
    echo "## Recent Run Log (tail 80 lines)"
    tail -n 80 "$LOG_FILE" || true
  } > "$CONTEXT_BRIEF"
}

commit_if_dirty() {
  local repo="$1"
  local msg="$2"
  if ! git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    return 0
  fi
  if git -C "$repo" diff --quiet && git -C "$repo" diff --cached --quiet && [[ -z "$(git -C "$repo" ls-files --others --exclude-standard)" ]]; then
    return 0
  fi
  git -C "$repo" add -A
  if git -C "$repo" diff --cached --quiet; then
    return 0
  fi
  git -C "$repo" commit -m "$msg" >/dev/null
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
  refresh_context_brief
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
    commit_if_dirty "$ROOT_DIR" "loop: ${next_task} done (state/docs sync)"
    commit_if_dirty "$ROOT_DIR/riscv-arch-test" "loop: ${next_task} done (zabha progress)"
  else
    scripts/taskctl.py set-status "$next_task" blocked --note "auto-loop acceptance failed" >/dev/null
    append_log "Task ${next_task} marked blocked by loop"
    exit 1
  fi

  scripts/taskctl.py set-state current_task null >/dev/null
done

append_log "Loop reached max steps=${MAX_STEPS}"
