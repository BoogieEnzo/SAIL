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
