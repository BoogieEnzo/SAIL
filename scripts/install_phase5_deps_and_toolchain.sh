#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${SAIL_ROOT:-/home/fengde/SAIL}"
export SAIL_ROOT="${ROOT_DIR}"
TC_SRC_DIR="${ROOT_DIR}/tools/riscv-gnu-toolchain"
TC_PREFIX="${ROOT_DIR}/tools/riscv-zabha"
JOBS="${JOBS:-2}"
AUTO_RUN_PHASE5="${AUTO_RUN_PHASE5:-0}"

echo "[phase5-setup] root=${ROOT_DIR}"
echo "[phase5-setup] toolchain_src=${TC_SRC_DIR}"
echo "[phase5-setup] toolchain_prefix=${TC_PREFIX}"
echo "[phase5-setup] jobs=${JOBS}"

if [[ ! -d "${TC_SRC_DIR}" ]]; then
  echo "[phase5-setup] missing ${TC_SRC_DIR}"
  echo "[phase5-setup] clone first:"
  echo "  git clone https://github.com/riscv-collab/riscv-gnu-toolchain.git ${TC_SRC_DIR}"
  exit 1
fi

echo "[phase5-setup] installing host dependencies (apt)..."
sudo apt-get update
sudo apt-get install -y \
  autoconf automake autotools-dev curl python3 python3-pip \
  libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex \
  texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev ninja-build

echo "[phase5-setup] preparing source..."
cd "${TC_SRC_DIR}"
git submodule update --init --recursive

echo "[phase5-setup] configuring riscv-gnu-toolchain (newlib, no gdb)..."
./configure --prefix="${TC_PREFIX}" --disable-gdb

echo "[phase5-setup] building toolchain..."
make -j"${JOBS}" newlib MAKEINFO=true

echo "[phase5-setup] probing Zabha support..."
cat > /tmp/zabha_probe_local.S <<'EOF'
.text
.globl _start
_start:
  amoadd.b x1, x2, (x3)
EOF
"${TC_PREFIX}/bin/riscv64-unknown-elf-gcc" -c \
  -march=rv64imafd_zicsr_zabha -mabi=lp64 \
  /tmp/zabha_probe_local.S -o /tmp/zabha_probe_local.o

echo "[phase5-setup] Zabha probe passed."
echo "[phase5-setup] export for this shell:"
echo "  export LOCAL_TOOLCHAIN_BIN=${TC_PREFIX}/bin"

if [[ "${AUTO_RUN_PHASE5}" == "1" ]]; then
  echo "[phase5-setup] starting phase5 auto-loop..."
  cd "${ROOT_DIR}"
  export LOCAL_TOOLCHAIN_BIN="${TC_PREFIX}/bin"
  bash scripts/auto_phase5_loop.sh
fi

echo "[phase5-setup] done."
