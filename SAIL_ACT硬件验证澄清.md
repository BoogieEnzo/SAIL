# SAIL/ACT 硬件验证（你这个“1+2=3”理解版）

## 结论（一句话）

可以非常直白地理解为：**把“物理硬件/实现”当成黑盒跑程序，检查它给出的结果（寄存器/内存/异常…）是不是和 RISC-V 规范定义的语义一致**。  
就像测计算器一样：输入 `1+2`，输出必须是 `3`。

## 验证流程（最小版）

```
写一个能触发某条语义的测试程序（通常是汇编/二进制）
   ↓
在待测实现上跑（DUT：芯片/RTL/仿真器/模型…）得到结果 A
   ↓
用参考语义跑（常用 Sail 模型）得到结果 B
   ↓
比较 A 与 B：一致=通过，不一致=实现有问题/测试有问题
```

---

## 🔍 具体验证什么（保留有价值例子）

### 1) 指令语义（Instruction Semantics）

验证点就是你说的“1+2=3”：**同一条指令、同一组输入状态，执行后输出状态必须符合规范**。

**例子：ADD 指令**

**RISC-V ISA 规范说**：
- `ADD rd, rs1, rs2` 应该执行：`rd = rs1 + rs2`
- 如果溢出怎么办？RISC-V 说：**截断**（wrap around）

**参考语义（示意：SAIL 风格）**：
```sail
function clause execute (ADD(rs1, rs2, rd)) = {
  X(rd) = X(rs1) + X(rs2);
  RETIRE_SUCCESS
}
```

**ACT 测试**：
```assembly
# 测试 ADD 指令
addi x1, x0, 100
addi x2, x0, 200
add x3, x1, x2
# 检查 x3 是否等于 300
```

---

### 2) 寄存器/CSR 状态（Register/CSR State）

验证执行指令后，**寄存器、内存、CSR（控制状态寄存器）**的可见状态是否符合规范。

**例子：CSR 访问**

**RISC-V ISA 规范说**：
- `CSRRW rd, csr, rs1` 应该：
  1. 读取 CSR 的值到 rd
  2. 将 rs1 的值写入 CSR

**ACT 测试**：
```assembly
# 测试 CSRRW
li x1, 0x123
csrrw x2, mstatus, x1
# 检查 x2 是否等于旧的 mstatus 值
# 检查 mstatus 是否等于 0x123
```

---

### 3) 异常和中断（Exceptions & Interrupts）

验证硬件在异常情况下的行为是否符合规范。

**例子：非法指令异常**

**RISC-V ISA 规范说**：
- 如果遇到未定义的指令编码，应该触发 `Illegal Instruction` 异常

**ACT 测试**：
```assembly
# 测试非法指令
.word 0x00000000  # 未定义的指令编码
# 检查是否触发异常，异常类型是否正确
```

---

### 4) 内存模型/原子性（Memory Model & Atomics）

验证内存访问的顺序、一致性是否符合 RISC-V 内存模型（RVWMO）。

**例子：原子操作**

**RISC-V ISA 规范说**：
- `AMOADD.W` 应该原子地执行：`mem[addr] = mem[addr] + rs2`

**ACT 测试**：
```assembly
# 测试原子操作
li x1, 0x1000
li x2, 100
amoadd.w x3, x2, (x1)
# 检查内存[0x1000]是否增加了100
# 检查 x3 是否等于旧的内存值
```

---

---

## 📝 一句话总结

**是的，你可以把它当成“验证物理实现的输出是否符合 RISC-V 语义”**：输入状态固定，执行后输出状态必须和规范一致；SAIL 常用作“参考答案”，ACT 常用作“出题/判卷的一套测试题库与框架”。  
