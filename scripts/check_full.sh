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

echo "[check_full] ok"
