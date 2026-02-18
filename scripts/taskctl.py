#!/usr/bin/env python3
import argparse
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List

ROOT = Path(__file__).resolve().parent.parent
STATE_PATH = ROOT / "docs" / "AGENT_STATE.json"
QUEUE_PATH = ROOT / "docs" / "TASK_QUEUE.json"


def now_iso() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat()


def read_json(path: Path) -> Dict[str, Any]:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def write_json(path: Path, data: Dict[str, Any]) -> None:
    with path.open("w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
        f.write("\n")


def list_tasks() -> int:
    queue = read_json(QUEUE_PATH)
    tasks: List[Dict[str, Any]] = queue["tasks"]
    print("ID        STAGE  KIND     STATUS    TITLE")
    for t in tasks:
        print(f"{t['id']:<9} {t['stage']:<6} {t['kind']:<8} {t['status']:<9} {t['title']}")
    return 0


def show_state() -> int:
    state = read_json(STATE_PATH)
    print(json.dumps(state, ensure_ascii=False, indent=2))
    return 0


def set_state(key: str, value: str) -> int:
    state = read_json(STATE_PATH)
    if key not in state:
        raise SystemExit(f"Unknown state key: {key}")
    if value.lower() == "null":
        state[key] = None
    elif isinstance(state[key], bool):
        if value.lower() in ("true", "1", "yes"):
            state[key] = True
        elif value.lower() in ("false", "0", "no"):
            state[key] = False
        else:
            raise SystemExit(f"Invalid bool value: {value}")
    elif state[key] is None:
        state[key] = value
    else:
        state[key] = value
    state["updated_at"] = now_iso()
    write_json(STATE_PATH, state)
    print(f"Updated {key} -> {state[key]}")
    return 0


def set_status(task_id: str, status: str, note: str) -> int:
    queue = read_json(QUEUE_PATH)
    tasks: List[Dict[str, Any]] = queue["tasks"]
    if status not in ("pending", "doing", "done", "blocked"):
        raise SystemExit(f"Invalid status: {status}")
    for t in tasks:
        if t["id"] == task_id:
            t["status"] = status
            if note:
                t["notes"] = note
            write_json(QUEUE_PATH, queue)
            print(f"{task_id} -> {status}")
            return 0
    raise SystemExit(f"Task not found: {task_id}")


def next_task(allow_development: bool) -> int:
    state = read_json(STATE_PATH)
    queue = read_json(QUEUE_PATH)
    if state["execution_mode"] != "run":
        print("NO_TASK: execution_mode is not run")
        return 2
    for t in queue["tasks"]:
        if t["status"] != "pending":
            continue
        if t["kind"] == "develop" and not allow_development:
            continue
        print(t["id"])
        return 0
    print("NO_TASK: no runnable pending task")
    return 3


def main() -> int:
    parser = argparse.ArgumentParser(description="Task and state control")
    sub = parser.add_subparsers(dest="cmd", required=True)

    sub.add_parser("list")
    sub.add_parser("show-state")

    p_set_state = sub.add_parser("set-state")
    p_set_state.add_argument("key")
    p_set_state.add_argument("value")

    p_set_status = sub.add_parser("set-status")
    p_set_status.add_argument("task_id")
    p_set_status.add_argument("status")
    p_set_status.add_argument("--note", default="")

    p_next = sub.add_parser("next")
    p_next.add_argument("--allow-development", action="store_true")

    args = parser.parse_args()
    if args.cmd == "list":
        return list_tasks()
    if args.cmd == "show-state":
        return show_state()
    if args.cmd == "set-state":
        return set_state(args.key, args.value)
    if args.cmd == "set-status":
        return set_status(args.task_id, args.status, args.note)
    if args.cmd == "next":
        return next_task(args.allow_development)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
