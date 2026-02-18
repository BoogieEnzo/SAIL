#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

bash scripts/check_min.sh

echo "[check_full] validating Zabha core test file counts"
rv32_dir="riscv-arch-test/riscv-test-suite/rv32i_m/Zabha/src"
rv64_dir="riscv-arch-test/riscv-test-suite/rv64i_m/Zabha/src"

test -d "$rv32_dir"
test -d "$rv64_dir"

rv32_count="$(find "$rv32_dir" -maxdepth 1 -type f -name '*.S' | wc -l | tr -d ' ')"
rv64_count="$(find "$rv64_dir" -maxdepth 1 -type f -name '*.S' | wc -l | tr -d ' ')"

if [[ "$rv32_count" != "18" ]]; then
  echo "[check_full] rv32 Zabha count expected 18, got $rv32_count"
  exit 1
fi
if [[ "$rv64_count" != "18" ]]; then
  echo "[check_full] rv64 Zabha count expected 18, got $rv64_count"
  exit 1
fi

echo "[check_full] validating ISA/regex tags"
if rg -n 'RVTEST_ISA\("RV(32|64)IA,RV(32|64)IZaamo"\)' "$rv32_dir" "$rv64_dir" >/dev/null; then
  echo "[check_full] found files missing Zabha ISA tag"
  exit 1
fi
if rg -n 'Zaamo\.\*' "$rv32_dir" "$rv64_dir" >/dev/null; then
  echo "[check_full] found old Zaamo gate regex in Zabha tests"
  exit 1
fi
if rg -n '\.w-01\.S|\.w covergroup|TEST_AMO_OP\([a-z0-9]+\.w,' "$rv32_dir" "$rv64_dir" >/dev/null; then
  echo "[check_full] found unexpected .w residue in Zabha .b/.h tests"
  exit 1
fi

echo "[check_full] validating Zabha+Zacas amocas.b/h integration tests"
for f in \
  "riscv-arch-test/riscv-test-suite/rv32i_m/Zacas/src/amocas.b-01.S" \
  "riscv-arch-test/riscv-test-suite/rv32i_m/Zacas/src/amocas.h-01.S" \
  "riscv-arch-test/riscv-test-suite/rv64i_m/Zacas/src/amocas.b-01.S" \
  "riscv-arch-test/riscv-test-suite/rv64i_m/Zacas/src/amocas.h-01.S"; do
  test -f "$f"
done

if ! rg -n 'RVTEST_ISA\("RV32IA,RV32IZabha,RV32IZacas"\)' \
  riscv-arch-test/riscv-test-suite/rv32i_m/Zacas/src/amocas.b-01.S \
  riscv-arch-test/riscv-test-suite/rv32i_m/Zacas/src/amocas.h-01.S >/dev/null; then
  echo "[check_full] rv32 amocas.b/h ISA tag mismatch"
  exit 1
fi
if ! rg -n 'RVTEST_ISA\("RV64IA,RV64IZabha,RV64IZacas"\)' \
  riscv-arch-test/riscv-test-suite/rv64i_m/Zacas/src/amocas.b-01.S \
  riscv-arch-test/riscv-test-suite/rv64i_m/Zacas/src/amocas.h-01.S >/dev/null; then
  echo "[check_full] rv64 amocas.b/h ISA tag mismatch"
  exit 1
fi
if ! rg -n 'regex\(\.\*Zabha\.\*Zacas\.\*\)' \
  riscv-arch-test/riscv-test-suite/rv32i_m/Zacas/src/amocas.b-01.S \
  riscv-arch-test/riscv-test-suite/rv32i_m/Zacas/src/amocas.h-01.S \
  riscv-arch-test/riscv-test-suite/rv64i_m/Zacas/src/amocas.b-01.S \
  riscv-arch-test/riscv-test-suite/rv64i_m/Zacas/src/amocas.h-01.S >/dev/null; then
  echo "[check_full] amocas.b/h case gate missing Zabha+Zacas regex"
  exit 1
fi

echo "[check_full] validating coverage files for rv32/rv64 Zabha"
test -f riscv-arch-test/coverage/zabha/rv32zabha.cgf
test -f riscv-arch-test/coverage/zabha/rv64zabha.cgf
if ! rg -n '^amocas\.b:|^amocas\.h:' \
  riscv-arch-test/coverage/zabha/rv32zabha.cgf \
  riscv-arch-test/coverage/zabha/rv64zabha.cgf >/dev/null; then
  echo "[check_full] missing amocas.b/h covergroups in zabha coverage files"
  exit 1
fi

echo "[check_full] ok"
