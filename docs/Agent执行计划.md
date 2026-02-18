# Agent执行计划（AI内部）

## 执行模式说明

**串行执行**：Agent按顺序启动，非并行
**里程碑汇报**：每个里程碑完成后向用户汇报
**代理类型**：根据任务需要启动不同类型的代理

---

## M1阶段：规范与参考分析

### Agent 1: 规范搜索专员（librarian）
**启动时机**：M1开始时
**任务**：
- 搜索Ziccrse规范文档（RISC-V官网、GitHub riscv-CMOs、邮件列表）
- 确认Ziccrse是否已批准
- 提取需要实现的CSR列表（地址、名称、功能描述）
- 如果找不到，搜索Ziccamoa作为备选

**交付物**：
- 规范文档链接或PDF路径
- CSR列表（含地址和功能）
- 或"规范找不到"报告

**预计用时**：2-4小时

---

### Agent 2: 代码分析专员（explore）
**启动时机**：M1开始时（与Agent 1同时）
**任务**：
- 分析Ssqosid扩展实现（model/extensions/Ssqosid/）
- 理解CSR扩展的实现模式
- 提取代码模板（CSR定义、读写函数、注册流程）
- 分析其他简单CSR扩展（如需要）

**交付物**：
- Ssqosid代码结构分析
- 可复用的代码模板
- CSR实现模式总结

**预计用时**：2-3小时

---

### Agent 3: 冲突检查专员（explore）
**启动时机**：M1开始时（与Agent 1、2同时）
**任务**：
- 检查sail-riscv仓库中是否有Ziccrse相关代码
- 搜索GitHub Issues和PRs，确认是否有重复工作
- 检查分配者Prashanth Mundkur的近期活动
- 确认实现状态

**交付物**：
- "无冲突"确认或冲突报告
- 相关PR/Issue列表（如有）
- 建议行动（继续/等待/换方案）

**预计用时**：1-2小时

---

### M1整合（主AI执行）
**时机**：所有Agent完成后
**任务**：
- 整合所有Agent结果
- 准备M1汇报内容
- 如规范找不到，准备备选方案建议
- **向用户汇报M1**

---

## M2阶段：框架与CSR实现

### Agent 4: 框架搭建专员（quick）
**启动时机**：用户确认M1后继续
**任务**：
- 创建目录结构 model/extensions/Ziccrse/
- 修改 model/core/extensions.sail 注册扩展
- 修改 model/riscv.sail_project 添加模块
- 修改 config/config.json.in 添加配置
- 确保编译通过

**交付物**：
- 目录结构已创建
- 所有配置文件已修改
- 编译通过确认

**预计用时**：2-3小时

---

### Agent 5: CSR实现专员（deep）
**启动时机**：Agent 4完成后
**任务**：
- 根据M1的CSR列表，实现CSR定义
- 实现read_CSR clause
- 实现write_CSR clause
- 处理只读位、副作用
- 确保代码风格符合规范

**交付物**：
- ziccrse_regs.sail（CSR实现）
- 编译通过确认

**预计用时**：4-6小时

---

### M2整合（主AI执行）
**时机**：所有Agent完成后
**任务**：
- 验证所有代码
- 准备M2汇报内容（代码展示）
- **向用户汇报M2**

---

## M3阶段：集成与测试

### Agent 6: 页表集成专员（deep）
**启动时机**：用户确认M2后继续
**任务**：
- 分析页表遍历代码（model/sys/vmem_ptw.sail）
- 确定CSR检查插入点
- 实现集成逻辑
- 如太复杂，准备简化方案说明

**交付物**：
- 修改后的页表代码（或简化版本说明）
- 集成逻辑说明

**预计用时**：4-8小时（取决于复杂度）

---

### Agent 7: 测试验证专员（unspecified-high）
**启动时机**：Agent 6完成后
**任务**：
- 运行 make 编译整个项目
- 运行 make test 执行测试套件
- 修复编译错误
- 修复测试失败
- 创建简单功能测试（如需要）

**交付物**：
- 测试通过报告
- 或测试失败记录及修复建议

**预计用时**：2-4小时

---

### M3整合（主AI执行）
**时机**：所有Agent完成后
**任务**：
- 验证所有测试通过
- 准备M3汇报内容
- **向用户汇报M3**

---

## M4阶段：PR提交

### Agent 8: PR准备专员（writing）
**启动时机**：用户确认M3后继续
**任务**：
- 准备PR描述（实现内容、参考规范、测试结果）
- 检查代码风格（CODE_STYLE.md）
- 准备commit message
- 整理文件修改列表

**交付物**：
- PR描述草稿
- Commit message建议
- 代码风格检查报告

**预计用时**：1-2小时

---

### Agent 9: 学习材料准备专员（writing）
**启动时机**：Agent 8同时
**任务**：
- 准备技术概念解释（通俗版，用rCore类比）
- 准备PPT大纲
- 准备Q&A回答
- 整理实现总结

**交付物**：
- 技术概念解释文档
- PPT大纲
- Q&A准备

**预计用时**：2-3小时

---

### M4整合（主AI执行）
**时机**：所有Agent完成后
**任务**：
- 准备最终汇报
- **询问用户GitHub信息**
- 提交PR（用户确认后）
- **向用户汇报M4**

---

## Agent调度顺序

```
M1
├── Agent 1 (规范搜索) ──┐
├── Agent 2 (代码分析) ──┼──> 主AI整合 ──> 汇报M1 ──> 用户确认
└── Agent 3 (冲突检查) ──┘

M2
├── Agent 4 (框架搭建) ──> 
└── Agent 5 (CSR实现) ───> 主AI整合 ──> 汇报M2 ──> 用户确认

M3
├── Agent 6 (页表集成) ──>
└── Agent 7 (测试验证) ──> 主AI整合 ──> 汇报M3 ──> 用户确认

M4
├── Agent 8 (PR准备) ────┐
└── Agent 9 (学习材料) ──┼──> 主AI整合 ──> 询问GitHub信息 ──> 提交PR ──> 汇报M4
```

---

## 启动指令模板

### 启动Agent 1（规范搜索）
```
task(
  category="deep",
  load_skills=["git-master"],
  description="搜索Ziccrse规范",
  prompt="""
  [CONTEXT] 需要为sail-riscv项目实现Ziccrse扩展，首先需要找到官方规范文档。
  
  [GOAL] 找到Ziccrse的RISC-V规范文档，确认需要实现哪些CSR。
  
  [REQUEST] 
  1. 搜索RISC-V官方规范库中的Ziccrse文档
  2. 搜索GitHub riscv-CMOs仓库
  3. 搜索RISC-V邮件列表存档
  4. 确认Ziccrse是否已批准
  5. 提取CSR列表（地址、名称、功能）
  6. 如果找不到Ziccrse，搜索Ziccamoa作为备选
  
  [DELIVERABLE]
  - 规范文档链接或PDF路径
  - CSR详细列表
  - 或"找不到规范"的报告
  """
)
```

### 启动Agent 2（代码分析）
```
task(
  category="explore",
  load_skills=["git-master"],
  description="分析Ssqosid参考实现",
  prompt="""
  [CONTEXT] 需要实现Ziccrse扩展（CSR扩展），需要学习sail-riscv中现有CSR扩展的实现模式。
  
  [GOAL] 分析Ssqosid扩展，提取可复用的代码模板。
  
  [REQUEST]
  1. 分析 model/extensions/Ssqosid/ 目录结构
  2. 分析Ssqosid的CSR定义方式
  3. 分析read_CSR和write_CSR的实现
  4. 分析扩展注册流程（extensions.sail, riscv.sail_project等）
  5. 提取代码模板（填空即可使用的代码片段）
  6. 总结CSR扩展的实现模式
  
  [DELIVERABLE]
  - Ssqosid代码结构分析文档
  - 可复用代码模板
  - 实现模式总结
  """
)
```

### 启动Agent 3（冲突检查）
```
task(
  category="explore",
  load_skills=["git-master"],
  description="检查Ziccrse实现冲突",
  prompt="""
  [CONTEXT] 计划实现Ziccrse扩展，需要确认没有重复工作。
  
  [GOAL] 检查sail-riscv中Ziccrse的实现状态。
  
  [REQUEST]
  1. 搜索sail-riscv代码库中是否有Ziccrse相关代码
  2. 搜索GitHub Issues中是否有Ziccrse相关讨论
  3. 搜索GitHub PRs中是否有Ziccrse实现
  4. 检查分配者Prashanth Mundkur的近期GitHub活动
  5. 评估继续实现的风险
  
  [DELIVERABLE]
  - "无冲突"确认或详细冲突报告
  - 相关PR/Issue列表
  - 行动建议（继续/等待/换方案）
  """
)
```

---

## 主AI职责

1. **调度Agent**：按顺序启动Agent，等待完成
2. **整合结果**：收集所有Agent输出，形成统一汇报
3. **向用户汇报**：每个里程碑完成后汇报
4. **决策**：在Agent结果不一致时做出判断
5. **代码最终审核**：确保代码质量符合要求

---

## 风险应对

| 风险 | Agent | 应对 |
|------|-------|------|
| 规范找不到 | Agent 1 | 启动Ziccamoa搜索作为备选 |
| 代码分析不充分 | Agent 2 | 要求分析更多参考实现 |
| 发现冲突 | Agent 3 | 建议用户换方案或等待 |
| 框架搭建失败 | Agent 4 | 检查依赖关系，重试或询问用户 |
| CSR实现复杂 | Agent 5 | 分步骤实现，简化功能 |
| 页表集成太难 | Agent 6 | 只做CSR部分，PR中说明 |
| 测试失败 | Agent 7 | 修复或记录问题询问用户 |

---

## 下一步

**准备启动**：M1阶段

**立即执行**：
1. 启动Agent 1（规范搜索）
2. 启动Agent 2（代码分析）
3. 启动Agent 3（冲突检查）
4. 等待所有Agent完成
5. 整合结果
6. **向你汇报M1**

**是否现在开始执行？**
