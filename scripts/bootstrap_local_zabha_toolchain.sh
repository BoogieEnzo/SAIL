#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="/home/fengde/SAIL"
SRC_DIR="${ROOT_DIR}/tools/riscv-gnu-toolchain"
INSTALL_DIR="${ROOT_DIR}/tools/riscv-zabha"
JOBS="${JOBS:-2}"

echo "[zabha-bootstrap] root: ${ROOT_DIR}"
echo "[zabha-bootstrap] source: ${SRC_DIR}"
echo "[zabha-bootstrap] install: ${INSTALL_DIR}"

if [[ ! -d "${SRC_DIR}" ]]; then
  echo "[zabha-bootstrap] cloning riscv-gnu-toolchain..."
  git clone --depth=1 https://github.com/riscv-collab/riscv-gnu-toolchain.git "${SRC_DIR}"
fi

cd "${SRC_DIR}"
if [[ -x "./configure" ]]; then
  :
else
  echo "[zabha-bootstrap] preparing submodules..."
  git submodule update --init --recursive
fi

mkdir -p "${INSTALL_DIR}"

echo "[zabha-bootstrap] configuring..."
./configure --prefix="${INSTALL_DIR}"

echo "[zabha-bootstrap] building newlib toolchain (jobs=${JOBS})..."
make -j"${JOBS}" newlib

echo "[zabha-bootstrap] probing Zabha support..."
cat > /tmp/zabha_probe_local.S <<'EOF'
.text
.globl _start
_start:
  amoadd.b x1, x2, (x3)
EOF

"${INSTALL_DIR}/bin/riscv64-unknown-elf-gcc" -c \
  -march=rv64imafd_zicsr_zabha \
  -mabi=lp64 \
  /tmp/zabha_probe_local.S \
  -o /tmp/zabha_probe_local.o

echo "[zabha-bootstrap] success."
echo "[zabha-bootstrap] use local toolchain only in this repo via:"
echo "  export LOCAL_TOOLCHAIN_BIN=${INSTALL_DIR}/bin"
echo "  bash scripts/run_riscof_zabha_auto.sh phase5"
