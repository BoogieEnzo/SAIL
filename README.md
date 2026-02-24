SAIL / ACT Zabha 测试支持仓库
==============================

本仓库是我在 SAIL / ACT 实习方向下的本地工作目录，核心目标是：**在 `riscv-arch-test (ACT)` 中为 Zabha 扩展补充测试，并完成分阶段（ph1–ph4）本地验证，尤其是第 4 阶段的完整回归**。老师可以通过本 README 快速了解：

- 我主要修改 / 新增的代码位于哪些目录；
- ph1–ph4（尤其是 ph4）已经做到什么程度，有哪些可直接查看的证据；
- 当前剩余的阻塞点（phase5 工具链能力不足）。

> 注：完整的实习总结与方向说明见 `实习汇报.md`、`docs/实习方向.md`。

一、代码改动位置
----------------

- `riscv-arch-test/`：上游 ACT 仓库的 fork，**所有 Zabha 相关改动都在这里**。
  - `riscv-test-suite/rv32i_m/Zabha/src/*.S`：我新增的 RV32 Zabha 测试汇编文件。
  - `riscv-test-suite/rv64i_m/Zabha/src/*.S`：我新增的 RV64 Zabha 测试汇编文件。
  - `coverage/zabha/rv32zabha.cgf`、`coverage/zabha/rv64zabha.cgf`：我新增/修改的 Zabha 覆盖率文件。
  - `riscv-test-suite/rv{32,64}i_m/Zacas/src/amocas.{b,h}-01.S`：我补充的 Zabha+Zacas 组合测试。
- `sail-riscv/`：参考模型 `sail_riscv_sim` 源码及构建结果（用于 RISCOF 回归）。
 - `scripts/`：本地自动化脚本（阶段拆分、心跳、工具链安装等），主要由我新增/维护，例如：
   - `check_min.sh` / `check_full.sh`：用于自检文档与 Zabha 测试/覆盖率的一致性。
   - `run_riscof_zabha_auto.sh`：分阶段（phase1–5）调用 RISCOF 跑 Zabha 测试的主脚本。
   - `auto_phase4_loop.sh`：自动循环跑 phase4 并更新心跳文件。
   - `bootstrap_local_zabha_toolchain.sh`、`install_phase5_deps_and_toolchain.sh`：构建支持 Zabha 的 GNU 工具链（供 phase5 使用）。
 - `docs/`：文档与运行记录。
   - `PH4_HEARTBEAT.md`：phase4 的进度心跳文件（当前为 48/48，100%）。
   - `automation-logs/auto_phase4_logs/`：phase4 自动循环的详细运行日志。
   - `实习方向.md`、`阶段验收标准.md`、`RUN_LOG.md` 等：任务说明与阶段记录。
 - `tools/`：
   - `riscv-gnu-toolchain/`：上游 RISC-V GNU 工具链源码（供脚本使用）。
   - `riscv-zabha/`：通过脚本构建出的、**支持 Zabha** 的本地工具链安装前缀。
   - `spike/`：本地 Spike 模拟器构建结果。

二、分阶段验证概览（ph1–ph4，重点 ph4）
------------------------------------

> 老师**不需要在本机重跑**，本节只是说明我在本机已经完成了哪些阶段，以及对应的证据文件在哪里。ph4 是最「完整」的一步，这部分我做得最重。

- **phase1：小规模「探路」测试**  
  用 `run_riscof_zabha_auto.sh` 从 ACT 的标准 test_list 中筛出一小部分与 Zabha 相关或兼容的测试，验证本机环境（RISCOF、Spike、sail-riscv 等）是否正常。  
  结果记录在 `riscv-arch-test/work-zabha/` 下的中间文件和 `docs/RUN_LOG.md` 中。

- **phase2：兼容核心子集**  
  在 phase1 通过的基础上，扩大到「核心指令 + 可选 Zabha」的子集，验证 ACT 与参考模型的基本一致性。  
  过滤与选取逻辑体现在 `scripts/run_riscof_zabha_auto.sh` 中，对应的 test_list 与日志同样保存在 `work-zabha/` 和 `docs/RUN_LOG.md`。

- **phase3：更大范围的兼容采样子集**  
  进一步扩展到更大规模的测试集（过滤掉当前工具链不支持的扩展），确保在不依赖 Zabha 工具链的前提下，环境与 ACT 集成本身是稳定的。  
  这一步的目的是为 phase4/phase5 做铺垫，避免在大规模回归时才暴露基础问题。

- **phase4：兼容全集，拆分 48 个 chunk（重点）**  
  这是我本阶段最「重」的一步，也是老师最关心的部分：  
  - 将兼容全集拆分为 **48 个 chunk**，通过 `auto_phase4_loop.sh` 自动从 `phase4_1` 跑到 `phase4_48`；  
  - 进度与结果汇总在 `docs/PH4_HEARTBEAT.md` 中，目前 `done_chunks: 48`、`progress: 100%`；  
  - 每一次运行的详细日志保存在 `docs/automation-logs/auto_phase4_logs/`，对应 `PH4_HEARTBEAT.md` 里的 `last_log` 路径；  
  - 除了 HEARTBEAT 和日志外，`docs/RUN_LOG.md` 中也有阶段性的文字记录，方便老师按时间线回顾。

三、老师可以直接看的证据
------------------------

如果老师只想「看结果、不跑命令」，可以重点看以下位置：

1. **我改了哪些测试与覆盖率**  
   - `riscv-arch-test/riscv-test-suite/rv32i_m/Zabha/src/*.S`  
   - `riscv-arch-test/riscv-test-suite/rv64i_m/Zabha/src/*.S`  
   - `riscv-arch-test/coverage/zabha/rv32zabha.cgf`  
   - `riscv-arch-test/coverage/zabha/rv64zabha.cgf`  
   - `riscv-arch-test/riscv-test-suite/rv{32,64}i_m/Zacas/src/amocas.{b,h}-01.S`

2. **phase4 是否完整跑完**  
   - 查看 `docs/PH4_HEARTBEAT.md`：  
     - `done_chunks: 48`  
     - `progress: 100%`  
     - `last_log: .../auto_phase4_logs/run_*.log`  
   - 如需细看某次运行细节，可打开 `docs/automation-logs/auto_phase4_logs/` 中对应的 log。

3. **阶段性运行记录**  
   - `docs/RUN_LOG.md` 中有各阶段（尤其是 phase4 自动循环）的摘要记录，可以从时间轴角度理解「我具体跑了什么」。

四、phase5 现状（简要说明）
------------------------

完整的「全量 Zabha 验收」需要 phase5：**真正只跑带 Zabha/Zacas 的测试全集**。这一阶段依赖本机的 GNU 工具链（`riscv64-unknown-elf-gcc/binutils`）支持 Zabha 指令。  

当前默认工具链不支持 Zabha，所以 `scripts/run_riscof_zabha_auto.sh` 会在探测时直接标记 phase5 为 blocked：这也是实习汇报中提到的「第五步被工具链能力阻塞」——**并非 Zabha 测试本身有错误，而是环境能力不够**。后续只需按脚本说明在本机或 CI 上换用支持 Zabha 的工具链，就可以继续往下推进。

