# Ziccrse 扩展实施计划（AI内部工作指南）

## 任务概述

**扩展名称**：Ziccrse - Cache Control Register Set Extension  
**目标**：在 sail-riscv 中实现 Ziccrse 扩展支持  
**用户选择依据**：中等复杂度、已批准、分配者可能未开始、用户有内核/CSR经验  
**预期时间**：1-2周  

---

## 阶段划分与AI行动项

### Phase 1: 规范研究与信息收集（第1天）

**AI任务**：
1. **搜索Ziccrse规范文档**
   - 查询RISC-V官方规范库
   - 查找Ziccrse的PDF或Asciidoc文档
   - 确认扩展包含哪些CSR寄存器
   - 记录每个CSR的地址、字段、访问权限

2. **验证实现状态**
   - 在sail-riscv仓库中搜索Ziccrse关键词
   - 确认完全没有相关代码或PR
   - 检查是否有类似命名冲突

3. **准备实施清单**
   - 列出所有需要实现的CSR
   - 记录每个CSR的读取/写入行为
   - 标记与其他模块的交互点（如页表遍历）

**交付物**：
- Ziccrse规范文档链接或摘要
- 需要实现的CSR列表（含地址、字段定义）
- 与其他模块的依赖关系图

---

### Phase 2: 参考实现分析（第2天）

**AI任务**：
1. **分析Ssqosid扩展**（最佳参考）
   - 研究 `model/extensions/Ssqosid/` 目录结构
   - 理解CSR扩展的注册模式
   - 学习如何在sys_regs.sail中添加CSR

2. **研究现有CSR实现模式**
   - 分析 `model/core/sys_regs.sail` 中的CSR定义
   - 理解 `readCSR` 和 `writeCSR` 函数
   - 学习CSR访问权限控制（如某些位只读）

3. **提取代码模板**
   - CSR类型定义模板
   - CSR读写函数模板
   - 扩展注册模板

**交付物**：
- 参考代码文件列表
- CSR实现模式总结
- 可直接复用的代码模板

---

### Phase 3: 基础框架搭建（第3-4天）

**AI任务**：
1. **创建扩展目录结构**
   ```
   model/extensions/Ziccrse/
   ├── ziccrse_regs.sail      # CSR定义
   └── ziccrse_insts.sail     # 指令实现（如需要）
   ```

2. **注册扩展**
   - 在 `model/core/extensions.sail` 添加：
     - `enum clause extension = Ext_Ziccrse`
     - `mapping clause extensionName = Ext_Ziccrse <-> "ziccrse"`
     - `function clause hartSupports(Ext_Ziccrse)`
     - 添加到 `extensions_ordered_for_isa_string` 数组

3. **添加扩展依赖**
   - 在 `model/riscv.sail_project` 中添加模块定义
   - 确保依赖关系正确（通常依赖core, sys）

4. **添加配置支持**
   - 在 `config/config.json.in` 中添加：
     ```json
     "Ziccrse": {
       "supported": true
     }
     ```

**交付物**：
- 扩展目录和空文件
- 所有注册代码
- 编译测试通过

---

### Phase 4: CSR实现（第5-7天）

**AI任务**：
1. **定义CSR寄存器类型**
   - 根据规范定义每个CSR的字段结构
   - 使用Sail的位向量类型（如`bits(64)`）
   - 定义字段的访问掩码

2. **实现读取函数**
   - 在 `readCSR` 匹配中添加新CSR
   - 处理特权级检查（如某些CSR只在S/M模式可读）
   - 返回正确的字段值

3. **实现写入函数**
   - 在 `writeCSR` 匹配中添加新CSR
   - 处理写入掩码（某些位只读）
   - 处理副作用（如某些CSR写入会触发cache操作）

4. **添加存储状态**
   - 如有需要，在寄存器状态中添加新字段
   - 确保reset时正确初始化

**交付物**：
- 完整的CSR实现代码
- 读写函数实现
- 编译测试通过

---

### Phase 5: 页表遍历集成（第8-9天）

**AI任务**：
1. **分析页表遍历代码**
   - 研究 `model/core/ptw.sail`（页表遍历）
   - 找到页表项(PTE)检查点
   - 理解cache控制如何影响页表遍历

2. **集成Ziccrse逻辑**
   - 在页表遍历中添加CSR值检查
   - 根据CSR设置调整cache行为
   - 确保硬件行为和规范一致

3. **处理边界情况**
   - 不同特权级的行为差异
   - 与现有cache控制扩展的交互

**交付物**：
- 修改后的页表遍历代码
- 集成逻辑说明
- 编译测试通过

---

### Phase 6: 测试与验证（第10-11天）

**AI任务**：
1. **编译测试**
   - 运行 `./build_simulator.sh`
   - 确保无编译错误
   - 解决类型不匹配等问题

2. **功能测试**
   - 创建简单的测试用例（如直接读写CSR）
   - 验证CSR读写正确
   - 验证页表遍历行为正确

3. **回归测试**
   - 运行 `make test`
   - 确保现有测试不失败
   - 修复可能的回归问题

4. **代码风格检查**
   - 检查2空格缩进
   - 检查命名规范
   - 检查文件末尾空行

**交付物**：
- 测试通过报告
- 修复记录
- 代码风格符合规范

---

### Phase 7: PR准备（第12天）

**AI任务**：
1. **准备PR描述**
   - 说明实现内容：支持Ziccrse扩展
   - 参考规范：RISC-V Ziccrse规范（附上链接）
   - 实现细节：CSR列表、功能说明
   - 测试情况：编译通过、测试通过

2. **整理提交历史**
   - 确保提交信息清晰
   - 建议用户按逻辑分割提交（如先框架、后CSR、再集成）

3. **检查清单**
   - [ ] CI测试通过
   - [ ] 代码风格符合规范
   - [ ] 无重复PR
   - [ ] 提交信息清晰

**交付物**：
- PR描述草稿
- 提交历史建议
- 最终代码检查

---

### Phase 8: Review响应（第13-14天）

**AI任务**：
1. **监控PR状态**
   - 每日检查PR是否有评论
   - 分析review意见

2. **快速响应**
   - 简单问题：立即修复（如代码风格）
   - 复杂问题：与用户讨论后再决定
   - 记录修改原因

3. **迭代优化**
   - 根据反馈调整实现
   - 确保每次修改后重新测试

**交付物**：
- Review响应记录
- 修改后的代码
- 最终合并

---

## 技术要点

### 1. CSR实现模式

```sail
// 1. 定义寄存器状态
register mcustomcsr : bits(64)

// 2. 初始化函数
function init_custom_csr() = {
  mcustomcsr = zeros()
}

// 3. 读取函数
function clause readCSR(0x7C0) = {
  // 返回CSR值
  mcustomcsr
}

// 4. 写入函数
function clause writeCSR(0x7C0, value) = {
  // 应用写入掩码
  let mask : bits(64) = 0x0000_0000_0000_00FF;
  mcustomcsr = (mcustomcsr & ~mask) | (value & mask);
  true
}
```

### 2. 扩展注册模式

```sail
// extensions.sail
enum clause extension = Ext_Ziccrse
mapping clause extensionName = Ext_Ziccrse <-> "ziccrse"
function clause hartSupports(Ext_Ziccrse) = config extensions.Ziccrse.supported

// 添加到有序列表
let extensions_ordered_for_isa_string = [
  // ... 其他扩展
  Ext_Ziccrse,
  // ...
]
```

### 3. 配置注册

```sail
// riscv.sail_project
extensions {
  // ...
  Ziccrse {
    requires core, sys
    files extensions/Ziccrse/ziccrse_regs.sail
          extensions/Ziccrse/ziccrse_insts.sail
  }
}
```

---

## 潜在风险与应对

### 风险1：规范找不到或不完整
- **应对**：在RISC-V官方库中搜索，或询问社区
- **备选**：如果规范太难找，考虑换Ziccamoa

### 风险2：分配者Prashanth已开始工作
- **应对**：先观察2-3天，如果他push代码，立即切换备选
- **备选**：Ziccamoa、修复bug

### 风险3：页表遍历集成太复杂
- **应对**：先实现CSR基本功能，页表部分简化或后做
- **备选**：只实现CSR部分，也算完整实现

### 风险4：CI失败
- **应对**：先本地解决所有编译问题，确保无警告
- **检查**：lean/rocq导致的失败可以忽略

---

## 关键时间节点

| 天数 | 关键里程碑 | 检查点 |
|------|-----------|--------|
| 2 | 找到规范，确认实现范围 | 有CSR列表 |
| 4 | 框架搭建完成 | 编译通过 |
| 7 | CSR实现完成 | 读写测试通过 |
| 9 | 页表集成完成 | 功能测试通过 |
| 11 | 所有测试通过 | make test通过 |
| 12 | PR提交 | PR创建 |
| 14 | Review完成 | PR合并或接近合并 |

---

## 每日检查清单

**AI每天需要检查**：
1. 前一天的任务是否完成
2. 是否有编译错误需要解决
3. 用户是否有新的指令或问题
4. 是否需要调整计划

**每周检查**：
1. 进度是否符合预期
2. 是否需要加班追赶
3. PR状态是否正常
4. 是否需要寻求帮助

---

## 成功标准

✅ **任务完成标准**：
1. Ziccrse扩展完整实现（CSR + 页表集成）
2. 代码通过CI测试
3. PR提交并获得至少一个approval
4. 用户理解所有实现细节（能讲清楚）

✅ **技术报告准备**：
- 用户能解释Ziccrse的作用
- 用户能解释CSR实现模式
- 用户能解释遇到的难点和解决方法

---

**计划创建时间**：2026-02-18  
**预计完成时间**：2026-03-04（2周）  
**状态**：准备开始实施  
