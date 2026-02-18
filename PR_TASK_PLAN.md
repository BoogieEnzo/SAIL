# SAIL/ACT 实习 PR 任务规划

> 针对丁冯德背景定制的 RISC-V Smaia 扩展实现方案

---

## 个人背景匹配分析

| 你的经验 | 与 Smaia 的关联 |
|---------|----------------|
| NEMU 模拟器开发 | IMSIC/APLIC 寄存器模拟机制类似 |
| rCore 中断处理 | 理解 mstatus/mie/mip 等 CSR |
| Sv32/Sv39 页表 | 理解内存映射设备（APLIC 是 MMIO）|
| 设备驱动（UART/块设备）| APLIC 类似平台级中断控制器 |
| C/Rust/汇编 | Sail 语言类似函数式 C+OCaml |

**结论：Smaia 与你的背景高度匹配，2-3 周可完成 MVP**

---

## 推荐任务：Smaia 扩展实现

### 什么是 Smaia？

Smaia（Supervisor-level Advanced Interrupt Architecture）是 RISC-V 的高级中断架构，包含：

- **IMSIC**：内存映射中断文件（每核一个，用于 MSI 中断）
- **APLIC**：平台级中断控制器（传统中断路由）
- **中断虚拟化支持**：供 Hypervisor 使用

**为什么适合你？**
- 涉及中断控制器，你在 NEMU/rCore 中已实现过 PLIC/CLINT
- 需要添加大量 CSRs，你在 OS 实习中熟悉 CSR 操作
- 无人认领（Extension Roadmap 显示未分配）

---

## 详细时间线

### Week 1：环境搭建与理解规范

#### Day 1-2：环境准备
```bash
# 1. 克隆仓库
cd /home/fengde/SAIL
git clone https://github.com/riscv/sail-riscv.git
cd sail-riscv
git checkout -b feature/smaia-extension

# 2. 安装 Sail（推荐 binary release）
wget https://github.com/rems-project/sail/releases/download/0.20.1/sail-0.20.1-linux-x86_64.tar.gz
tar xzf sail-0.20.1-linux-x86_64.tar.gz
export PATH=$PWD/sail-0.20.1/bin:$PATH

# 3. 验证构建
./build_simulator.sh
# 预期：生成 build/c_emulator/sail_riscv_sim
```

**成功标准：** `./build/c_emulator/sail_riscv_sim --help` 有输出

---

#### Day 3-4：阅读规范

**必读文档：**
1. [RISC-V AIA 规范 v1.0](https://github.com/riscv/riscv-aia/releases)（下载 PDF）
   - 重点章节：
     - Chapter 2: Interrupt Delivery（理解 IMSIC）
     - Chapter 3: APLIC（理解平台级中断控制器）
     - Chapter 5: CSRs（miselect, mireg, mtopei, mtopi 等）

2. Sail 语言入门
   - 阅读 `doc/ReadingGuide.md`
   - 理解 `model/extensions/Zicond/zicond_insts.sail` 结构

**笔记任务：**
- 列出 Smaia 涉及的所有 CSRs（约 10-15 个）
- 理解 IMSIC 内存映射布局
- 理解 APLIC 寄存器空间

---

#### Day 5-7：代码结构分析

**研究以下文件：**

```
model/
├── core/
│   ├── extensions.sail          # 看扩展如何注册
│   ├── csrs.sail               # 看 CSR 读写框架
│   └── types.sail              # 看类型定义
├── extensions/
│   ├── Zicond/                 # 简单扩展示例
│   ├── Sstc/                   # 特权级扩展示例
│   └── Smcntrpmf/              # Machine 模式扩展示例
└── sys/
    └── csr_utils.sail          # CSR 辅助函数
```

**任务：**
- 理解 `enum clause extension` 语法
- 理解 `scattered function read_CSR/write_CSR`
- 理解 `bitfield` 定义 CSR 位域

---

### Week 2：核心实现

#### Day 8-9：扩展框架搭建

**文件 1：model/extensions/Smaia/smaia_types.sail**
```sail
// Smaia 类型定义

// miselect 的索引枚举
enum mieindex = {
  // Topi 相关
  MTOPI,
  // EIP0-EIP63（每 64 个一组）
  MEIP0, MEIP1, ..., MEIP63,
  // EIE0-EIE63
  MEIE0, MEIE1, ..., MEIE63,
}

// 中断优先级结构
bitfield interrupt_priority : bits(64) = {
  priority : 55..0,
  reserved : 63..56
}
```

**文件 2：model/extensions/Smaia/smaia_regs.sail**
```sail
// CSR 寄存器定义

register miselect : bits(64)  // 间接寄存器选择器
register mireg : bits(64)     // 间接寄存器数据
register mtopei : bits(64)    // 顶部外部中断信息
register mtopi : bits(64)     // 顶部中断信息（只读）

// IMSIC 相关
register meie : vector(64, bits(64))   // 外部中断使能（每个中断一个位）
register meip : vector(64, bits(64))   // 外部中断待处理
```

**文件 3：model/core/extensions.sail（修改）**
```sail
// 在适当位置添加：
enum clause extension = Ext_Smaia
mapping clause extensionName = Ext_Smaia <-> "smaia"
function clause hartSupports(Ext_Smaia) = config extensions.Smaia.supported
function clause currentlyEnabled(Ext_Smaia) = hartSupports(Ext_Smaia)
```

---

#### Day 10-11：CSR 读写实现

**文件 4：model/extensions/Smaia/smaia_csrs.sail**
```sail
// miselect 读写（直接）
function clause read_CSR(0x350) = miselect
function clause write_CSR(0x350, value) = {
  miselect = value;
  miselect
}

// mireg 间接读写（根据 miselect 决定访问哪个寄存器）
function clause read_CSR(0x351) = {
  match decode_mieindex(miselect) {
    MTOPI => mtopi.bits(),
    MEIP0 => meip[0],
    MEIE0 => meie[0],
    // ... 其他索引
    _ => zeros()  // 未定义索引返回 0
  }
}

function clause write_CSR(0x351, value) = {
  match decode_mieindex(miselect) {
    MEIE0 => { meie[0] = value; meie[0] },
    // ... 其他可写寄存器
    _ => zeros()  // 只读寄存器写入被忽略
  }
}

// mtopei - 只读，返回最高优先级待处理中断
function clause read_CSR(0x35C) = mtopei.bits()

// mtopi - 只读，简化版（不含 claim 功能）
function clause read_CSR(0x35D) = mtopi.bits()
```

---

#### Day 12-14：IMSIC 基础支持

**文件 5：model/extensions/Smaia/smaia_imsic.sail**

IMSIC 是内存映射的，需要模拟内存访问：

```sail
// IMSIC 内存映射区域（每核一个）
// 基址 + hart_id * stride

// 中断设置寄存器（MSI 写入触发中断）
val imsic_write : (xlenbits, xlenbits) -> unit
function imsic_write(addr, data) = {
  // 检查地址是否在 IMSIC 范围内
  // 提取中断号
  let interrupt_id : nat = ...;
  // 设置对应的 MEIP 位
  set_meip_bit(interrupt_id);
  // 更新 mtopi
  update_mtopi();
}

// 计算最高优先级中断
val update_mtopi : unit -> unit
function update_mtopi() = {
  // 遍历所有 MEIP & MEIE，找最高优先级
  // 更新 mtopi 寄存器
}
```

---

### Week 3：测试与优化

#### Day 15-17：基础测试

**创建测试：** `test/first_party/smaia/`（如果不存在则创建）

```assembly
# test_smaia_csr.S
# 测试基本 CSR 访问

RVTEST_RV64M
RVTEST_CODE_BEGIN

  # 测试 miselect 读写
  li t0, 0
  csrw miselect, t0
  csrr t1, miselect
  bne t0, t1, fail

  # 测试 mireg 间接访问（MEIE0）
  li t0, 1
  csrw miselect, t0    # 选择 MEIE0
  li t1, 0xFFFFFFFF
  csrw mireg, t1       # 使能所有外部中断
  csrr t2, mireg
  bne t1, t2, fail

  RVTEST_PASS

fail:
  RVTEST_FAIL

RVTEST_CODE_END
```

**运行测试：**
```bash
./build/c_emulator/sail_riscv_sim \
  --enable-experimental-extensions \
  test/first_party/smaia/test_smaia_csr.elf
```

---

#### Day 18-19：APLIC 基础支持（可选 MVP）

如果时间不够，可以先跳过 APLIC，只完成 IMSIC 部分。

**简化版 APLIC：**
- 只实现单核模式
- 只支持 MSI 模式（不兼容传统 wired 中断）

---

#### Day 20-21：代码清理与文档

**检查清单：**
- [ ] 所有新文件有 BSD-2-Clause 许可证头
- [ ] 代码符合 CODE_STYLE.md（2空格缩进）
- [ ] 函数命名使用 snake_case
- [ ] 类型命名使用 PascalCase
- [ ] 无尾随空格

**运行检查：**
```bash
# 格式检查
find model/extensions/Smaia -name "*.sail" -exec clang-format -i {} \;

# 拼写检查
codespell model/extensions/Smaia/

# 构建测试
make clean && ./build_simulator.sh
```

---

## PR 提交清单

### 提交前必须完成

1. **功能验证**
   ```bash
   # 基础构建通过
   ./build_simulator.sh
   
   # 测试通过
   make test
   
   # 启用实验扩展测试
   ./build/c_emulator/sail_riscv_sim \
     --enable-experimental-extensions \
     test.elf
   ```

2. **代码质量**
   - [ ] 通过 `pre-commit` 检查
   - [ ] 无编译警告
   - [ ] Sail 类型检查通过（`sail -c ...`）

3. **文档更新**
   - [ ] README.md 中 Supported Extensions 列表添加 Smaia
   - [ ] 更新 Extension Roadmap（如果维护 Wiki）

### PR 描述模板

```markdown
## Summary
Add Smaia (Advanced Interrupt Architecture) extension support

## Changes
- Add Smaia extension framework (model/extensions/Smaia/)
- Implement IMSIC (Interrupt Message Signaled Interrupt Controller) CSRs:
  - miselect, mireg, mtopei, mtopi
  - MEIP/MEIE 寄存器组（外部中断待处理/使能）
- Add indirect CSR access mechanism
- Add basic MSI delivery support

## Testing
- Added test/first_party/smaia/test_smaia_csr.S
- Verified CSR read/write operations
- Tested indirect register access via miselect/mireg

## Notes
- This is MVP implementation, APLIC support planned for follow-up PR
- Requires --enable-experimental-extensions flag

## Checklist
- [x] Code follows CODE_STYLE.md
- [x] All tests pass
- [x] Documentation updated
```

---

## 风险与应对

| 风险 | 概率 | 应对策略 |
|------|------|---------|
| Sail 编译器安装失败 | 中 | 使用 binary release，避免源码编译 |
| 规范理解错误 | 中 | 在开发过程中多查 Sail 模型已有实现 |
| 时间不够 | 中 | Week 2 末评估，如不够则提交简化版（仅核心 CSRs）|
| 与已有 PR 冲突 | 低 | 每 2 天检查一次 upstream 更新 |

---

## 每日 Check-in 模板

每天结束时问自己：

1. 今天完成了什么？（具体到文件/函数）
2. 遇到什么问题？（卡住超过 2 小时必须寻求帮助）
3. 明天计划做什么？
4. 是否还需要调整时间线？

---

## 资源链接

- **AIA 规范**: https://github.com/riscv/riscv-aia/releases
- **Sail 手册**: https://alasdair.github.io/manual.html
- **现有扩展**: `model/extensions/` 目录
- **SAIL 实习方向**: `/home/fengde/SAIL/SAIL_ACT实习方向.md`
- **导师联系方式**: 邮件/腾讯会议（周三 3PM）

---

## 下一步行动

**今天就做：**

```bash
cd /home/fengde/SAIL
git clone https://github.com/riscv/sail-riscv.git
cd sail-riscv
# 安装 Sail 编译器
# 运行 ./build_simulator.sh
```

**遇到问题立即联系：**
- 技术问题 → 周三 3PM 技术报告会议
- 方向问题 → 导师邮件
- Sail 语法 → 查看 model/extensions/ 已有代码

---

**祝实习顺利！记得每周三准备技术报告 PPT。**
