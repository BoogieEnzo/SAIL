#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-/home/fengde/SAIL}"
LOG_DIR="${ROOT_DIR}/automation-logs"
LOG_FILE="${LOG_DIR}/for_me-$(date +%Y%m%d_%H%M%S).log"

mkdir -p "${LOG_DIR}"

run() {
  echo ""
  echo ">>> $*" | tee -a "${LOG_FILE}"
  eval "$@" 2>&1 | tee -a "${LOG_FILE}"
}

echo "Using ROOT_DIR=${ROOT_DIR}" | tee -a "${LOG_FILE}"

if [[ ! -d "${ROOT_DIR}" ]]; then
  echo "Root dir not found: ${ROOT_DIR}" | tee -a "${LOG_FILE}"
  exit 1
fi

cd "${ROOT_DIR}"

if [[ ! -d ".venv" ]]; then
  run "python3 -m venv .venv"
fi

run "${ROOT_DIR}/.venv/bin/python -m pip install -U pip"
run "${ROOT_DIR}/.venv/bin/python -m pip install -U riscof"

# 1) Spike
if [[ ! -d "riscv-isa-sim" ]]; then
  run "git clone https://github.com/riscv-software-src/riscv-isa-sim.git"
fi
run "cd riscv-isa-sim && mkdir -p build && cd build && ../configure --prefix=${ROOT_DIR}/tools/spike && make -j\$(nproc) && make install"
run "${ROOT_DIR}/tools/spike/bin/spike --version"

# 2) Sail compiler via opam
run "sudo apt-get update"
run "sudo apt-get install -y opam m4 pkg-config zlib1g-dev libgmp-dev"
run "opam init -y --disable-sandboxing || true"
run "eval \"\$(opam env)\" && opam install -y sail"
run "eval \"\$(opam env)\" && sail --version"

# 3) Build sail_riscv_sim (disable ccache launcher)
run "cd sail-riscv && rm -rf build && eval \"\$(opam env)\" && CC=/usr/bin/gcc CXX=/usr/bin/g++ CMAKE_C_COMPILER_LAUNCHER= CMAKE_CXX_COMPILER_LAUNCHER= PATH=\"\$(opam var bin):\$PATH\" ./build_simulator.sh"
run "find ${ROOT_DIR}/sail-riscv/build -type f -name sail_riscv_sim | head -n 1"

# 4) RISC-V toolchain with Zabha
if [[ ! -d "riscv-gnu-toolchain" ]]; then
  run "git clone https://github.com/riscv-collab/riscv-gnu-toolchain.git"
fi
run "cd riscv-gnu-toolchain && git pull"
run "cd riscv-gnu-toolchain && ./configure --prefix=${ROOT_DIR}/tools/riscv --with-arch=rv64gc_zabha_zacas --with-abi=lp64d"
run "cd riscv-gnu-toolchain && make -j\$(nproc)"

# Verification outputs
run "${ROOT_DIR}/tools/riscv/bin/riscv64-unknown-elf-as --version | head -n 1"
run "echo 'amoadd.b x1,x2,(x3)' | ${ROOT_DIR}/tools/riscv/bin/riscv64-unknown-elf-as -march=rv64ia_zabha -o /tmp/t.o -"
run "${ROOT_DIR}/.venv/bin/riscof --version"

echo ""
echo "Done. Log: ${LOG_FILE}"
