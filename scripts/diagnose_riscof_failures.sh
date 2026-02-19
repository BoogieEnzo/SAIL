#!/usr/bin/env bash
set -u

ROOT="/home/fengde/SAIL"
WORK="$ROOT/riscv-arch-test/work-zabha"
OUT="$WORK/diagnose_$(date +%Y%m%d_%H%M%S).log"

ZCMOP_DIR="$WORK/rv64i_m/Zcmop/src/c.mop.11-01.S/dut"
VM_DIR="$WORK/rv64i_m/vm_sv57/src/sv57_sum_unset_S_mode.S/dut"

log() {
  echo "[$(date +%H:%M:%S)] $*" | tee -a "$OUT"
}

run_and_log() {
  local title="$1"
  shift
  log "=== $title ==="
  echo "+ $*" | tee -a "$OUT"
  "$@" >>"$OUT" 2>&1
  local rc=$?
  log "exit_code=$rc"
  return 0
}

mkdir -p "$WORK"
touch "$OUT"
log "output_file=$OUT"

log "=== Tool Versions ==="
{
  echo "+ riscv64-unknown-elf-gcc --version | head -n 1"
  riscv64-unknown-elf-gcc --version | head -n 1
  echo "+ $ROOT/tools/spike/bin/spike --help | head -n 3"
  "$ROOT/tools/spike/bin/spike" --help | head -n 3
} >>"$OUT" 2>&1

if [[ -d "$ZCMOP_DIR" ]]; then
  log "=== Reproduce Zcmop compile (c.mop.11-01.S) ==="
  (
    cd "$ZCMOP_DIR" || exit 1
    rm -f my.elf
    echo "+ compile in $PWD" >>"$OUT"
    riscv64-unknown-elf-gcc \
      -march=rv64imafdcv_zicsr_zicond_zicboz_zimop_zfa_zfh_zca_zcb_zcmop_zba_zbb_zbc_zbs_zbkb_zbkc_zknd_zkne_zbkx_zknh_zksh_zksed \
      -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles -g \
      -T "$ROOT/riscv-arch-test/riscof-plugins/rv64/spike_simple/env/link.ld" \
      -I "$ROOT/riscv-arch-test/riscof-plugins/rv64/spike_simple/env/" \
      -I "$ROOT/riscv-arch-test/riscv-test-suite/env" \
      "$ROOT/riscv-arch-test/riscv-test-suite/rv64i_m/Zcmop/src/c.mop.11-01.S" \
      -o my.elf -DTEST_CASE_1=True -DXLEN=64 -DFLEN=64 -mabi=lp64 >>"$OUT" 2>&1
    echo "gcc_exit=$?" >>"$OUT"
    ls -l my.elf >>"$OUT" 2>&1 || true
  )
else
  log "WARN: missing dir $ZCMOP_DIR"
fi

if [[ -d "$VM_DIR" ]]; then
  log "=== Reproduce vm_sv57 runtime (60s timeout) ==="
  (
    cd "$VM_DIR" || exit 1
    rm -f quick.signature
    echo "+ runtime in $PWD" >>"$OUT"
    timeout 60 "$ROOT/tools/spike/bin/spike" \
      --misaligned \
      --isa=rv64imafdcv_Zicsr_Zicond_Zicboz_Zimop_Zfa_Zfh_Zca_Zcb_Zcmop_Zba_Zbb_Zbc_Zbs_Zbkb_Zbkc_Zknd_Zkne_Zbkx_Zknh_Zksh_Zksed \
      +signature=quick.signature +signature-granularity=8 my.elf >>"$OUT" 2>&1
    echo "spike_exit=$?" >>"$OUT"
    ls -l quick.signature >>"$OUT" 2>&1 || true
  )
else
  log "WARN: missing dir $VM_DIR"
fi

log "=== Tail Existing Logs ==="
if [[ -f "$ZCMOP_DIR/DUT-spike.log" ]]; then
  {
    echo "--- tail: $ZCMOP_DIR/DUT-spike.log ---"
    tail -n 80 "$ZCMOP_DIR/DUT-spike.log"
  } >>"$OUT" 2>&1
fi
if [[ -f "$VM_DIR/DUT-spike.log" ]]; then
  {
    echo "--- tail: $VM_DIR/DUT-spike.log ---"
    tail -n 80 "$VM_DIR/DUT-spike.log"
  } >>"$OUT" 2>&1
fi

log "done"
echo "diagnose_log=$OUT"
