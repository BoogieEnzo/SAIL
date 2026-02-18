# AGENTS.md

> Guidelines for AI agents working on SAIL/ACT RISC-V verification projects

## Project Context

This directory contains research notes for RISC-V processor verification using:
- **SAIL**: ISA specification language (OCaml-based)
- **ACT**: Architecture Compatibility Tests
- **ISLA**: Formal verification tools

Related repositories:
- https://github.com/riscv/sail-riscv
- https://github.com/rems-project/sail
- https://github.com/riscv-non-isa/riscv-arch-test (ACT4 branch)
- https://github.com/rems-project/isla

## Build/Test Commands (for sail-riscv)

```bash
# Build sail-riscv
make

# Build specific architecture
make ARCH=RV64

# Run tests
make test

# Run specific test
./emulator_riscv64_RV64 < test.elf

# Clean build
make clean
```

## Build/Test Commands (for ACT)

```bash
# Run all tests
make

# Run specific extension tests
make RISCV_DEVICE=I

# Run single test
riscv64-unknown-elf-gcc -march=rv64i -mabi=lp64 -T link.ld test.S -o test.elf

# Verify against reference
make verify
```

## Code Style Guidelines

### SAIL Language
- Use 2-space indentation
- Function names: `snake_case`
- Type names: `PascalCase`
- Scattered definitions should be grouped logically
- Use `function` keyword for pure functions, `val` for monadic
- Prefer pattern matching over nested if-else

### OCaml (SAIL implementation)
- Use `ocamlformat` if available
- Follow existing module structure
- Prefix internal functions with underscore

### C/Assembly (ACT tests)
- Indent with spaces (4 for C, 8 for assembly)
- Use RVTEST_ macros for test framework
- Include proper signature and data sections
- Follow existing test naming convention: `TEST_NAME.S`

### General
- Max line length: 100 characters
- Trailing whitespace: remove
- End files with newline
- UTF-8 encoding only

## Naming Conventions

- SAIL functions: `execute_`, `read_`, `write_` prefixes for operations
- ACT tests: descriptive with extension suffix (e.g., `addi-01.S`)
- Branches: `feature/description` or `fix/description`
- Commits: imperative mood, reference issue numbers

## Error Handling

- SAIL: Use `throw` for architectural exceptions
- Return `option` or `result` types for recoverable errors
- Log with appropriate verbosity levels

## PR Requirements

- Must pass CI (unless blocked by lean/rocq issues)
- Code quality standards apply
- Cannot be trivial (typo fixes don't count)
- Check for duplicate PRs before submitting
- Include technical report (Wed 3PM meeting)

## Resources

- Sail manual: https://alasdair.github.io/manual.html
- RISC-V specs: https://riscv.org/technical/specifications/
- ACT dev guide: https://github.com/riscv-non-isa/riscv-arch-test/blob/act4/docs/DeveloperGuide.md
