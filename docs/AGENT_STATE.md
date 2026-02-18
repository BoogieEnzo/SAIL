# Agent State

- Execution Mode: `hold`
- Development Allowed: `false`
- Current Stage: `M2`
- Current Task: `none`
- Rule: 在你明确下达“开始开发”之前，不执行开发类任务，只允许状态检查和流程维护。

## Control Commands

- 查看状态: `scripts/taskctl.py show-state`
- 切换为运行模式: `scripts/taskctl.py set-state execution_mode run`
- 允许开发任务: `scripts/taskctl.py set-state allow_development true`
- 切换回暂停: `scripts/taskctl.py set-state execution_mode hold`

## Loop Discipline

- 每次 alpha loop 开始前，必须先读取并刷新以下文件：
  - `docs/AGENT_STATE.json`
  - `docs/TASK_QUEUE.json`
  - `docs/RUN_LOG.md`
- 每轮结束后应更新 `docs/RUN_LOG.md`，确保下一轮可无上下文恢复。
- 采用相对频繁 commit 策略，方便必要时回滚：
  - 至少每完成一个任务一次 commit
  - 发生结构性调整时立即额外 commit
