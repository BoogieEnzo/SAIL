#!/usr/bin/env bash
# 强力清理脚本 v3：Docker/Snap/Flatpak/日志/缓存/旧内核/大型构建目录
# 说明：
# 1) 危险操作会先询问 [y/N]
# 2) 默认保守，优先清理缓存和可重建文件
# 3) 仅清理当前用户可访问目录，系统目录用 sudo

set -euo pipefail

BEFORE_KB="$(df -k / | awk 'NR==2 {print $3}')"

confirm() {
  local msg="$1"
  echo
  read -r -p "${msg} [y/N] " -n 1 r
  echo
  [[ "${r:-}" =~ ^[yY]$ ]]
}

human_kb_to_gb() {
  local kb="$1"
  awk -v v="${kb}" 'BEGIN { printf "%.2f", v/1024/1024 }'
}

safe_rm_dir_contents() {
  local dir="$1"
  [[ -d "${dir}" ]] || return 0
  find "${dir}" -mindepth 1 -maxdepth 1 -exec rm -rf {} + 2>/dev/null || true
}

echo "=== 清理前 ==="
df -h /
echo

# =============================================================================
# 一、无需确认的清理（相对安全）
# =============================================================================

if command -v docker >/dev/null 2>&1; then
  echo "=== Docker 清理 ==="
  docker system df 2>/dev/null || true
  docker system prune -f 2>/dev/null || true
  docker image prune -a -f 2>/dev/null || true
  docker volume prune -f 2>/dev/null || true
  docker builder prune -a -f 2>/dev/null || true
  docker system df 2>/dev/null || true
  echo
fi

if command -v podman >/dev/null 2>&1; then
  echo "=== Podman 清理 ==="
  podman system prune -a -f 2>/dev/null || true
  echo
fi

echo "=== 清理 /tmp、/var/tmp（安全方式，不触碰 . ..）==="
sudo find /tmp -mindepth 1 -maxdepth 1 -exec rm -rf {} + 2>/dev/null || true
sudo find /var/tmp -mindepth 1 -maxdepth 1 -exec rm -rf {} + 2>/dev/null || true
echo

echo "=== APT 清理 ==="
sudo apt-get clean 2>/dev/null || true
sudo apt-get autoclean 2>/dev/null || true
sudo apt-get autoremove -y 2>/dev/null || true
echo

echo "=== 裁剪 journal（保留 3 天）==="
sudo journalctl --vacuum-time=3d 2>/dev/null || true
echo

if command -v snap >/dev/null 2>&1; then
  echo "=== 删除 Snap 旧版本 ==="
  (sudo snap list --all 2>/dev/null || true) | awk '/disabled/{print $1, $3}' | while read -r pkg rev; do
    sudo snap remove "${pkg}" --revision="${rev}" 2>/dev/null || true
  done
  echo
fi

echo "=== 用户缓存 ==="
safe_rm_dir_contents "${HOME}/.cache/pip"
safe_rm_dir_contents "${HOME}/.cache/matplotlib"
safe_rm_dir_contents "${HOME}/.cache/huggingface"
safe_rm_dir_contents "${HOME}/.cache/npm"
safe_rm_dir_contents "${HOME}/.cache/yarn"
safe_rm_dir_contents "${HOME}/.cache/go-build"
safe_rm_dir_contents "${HOME}/.cache/thumbnails"
safe_rm_dir_contents "${HOME}/.cache/fontconfig"
safe_rm_dir_contents "${HOME}/.gradle/caches"
echo

echo "=== Cursor 缓存 ==="
safe_rm_dir_contents "${HOME}/.cursor/Cache"
safe_rm_dir_contents "${HOME}/.cursor/CachedData"
safe_rm_dir_contents "${HOME}/.cursor/CachedExtensions"
safe_rm_dir_contents "${HOME}/.cursor/CachedExtensionVSIXs"
echo

echo "=== 系统缓存（字体、man）==="
sudo find /var/cache/fontconfig -mindepth 1 -exec rm -rf {} + 2>/dev/null || true
sudo find /var/cache/man -mindepth 1 -exec rm -rf {} + 2>/dev/null || true
echo

# =============================================================================
# 二、需要确认的清理
# =============================================================================

if [[ -d "${HOME}/.local/share/Trash" ]]; then
  TRASH_SIZE="$(du -sh "${HOME}/.local/share/Trash" 2>/dev/null | awk '{print $1}' || true)"
  if [[ -n "${TRASH_SIZE:-}" ]]; then
    echo ">>> 回收站占用约: ${TRASH_SIZE}"
    if confirm "清空回收站?"; then
      safe_rm_dir_contents "${HOME}/.local/share/Trash/files"
      safe_rm_dir_contents "${HOME}/.local/share/Trash/info"
      echo "已清空回收站."
    fi
  fi
fi

if command -v flatpak >/dev/null 2>&1; then
  UNUSED="$(flatpak list --unused 2>/dev/null | wc -l | tr -d ' ')"
  if [[ "${UNUSED}" -gt 0 ]]; then
    echo ">>> 未使用 Flatpak 项数量: ${UNUSED}"
    flatpak list --unused 2>/dev/null || true
    if confirm "卸载未使用 Flatpak 运行时/应用?"; then
      flatpak uninstall --unused -y 2>/dev/null || true
      echo "已卸载."
    fi
  fi
fi

if [[ -d /var/crash ]]; then
  CRASH_SIZE="$(sudo du -sh /var/crash 2>/dev/null | awk '{print $1}' || true)"
  if [[ -n "${CRASH_SIZE:-}" && "${CRASH_SIZE}" != "0" ]]; then
    echo ">>> /var/crash 占用约: ${CRASH_SIZE}"
    if confirm "清空 /var/crash ?"; then
      sudo find /var/crash -mindepth 1 -exec rm -rf {} +
      echo "已清空 /var/crash."
    fi
  fi
fi

BIG_LOGS="$(sudo find /var/log -type f -size +100M 2>/dev/null || true)"
if [[ -n "${BIG_LOGS}" ]]; then
  echo ">>> /var/log 中 >100MB 的文件:"
  while IFS= read -r f; do
    sudo du -sh "${f}" 2>/dev/null || true
  done <<< "${BIG_LOGS}"
  if confirm "截断这些大日志到 0 字节?"; then
    while IFS= read -r f; do
      sudo truncate -s 0 "${f}" 2>/dev/null || true
    done <<< "${BIG_LOGS}"
    echo "已截断."
  fi
fi

CURRENT="$(uname -r)"
CURRENT_PKG="linux-image-${CURRENT}"
OLD_IMAGES="$(dpkg -l 'linux-image-*' 2>/dev/null | awk '/^ii/{print $2}' | grep -v "^${CURRENT_PKG}$" || true)"
if [[ -n "${OLD_IMAGES}" ]]; then
  echo ">>> 当前内核: ${CURRENT}（保留 ${CURRENT_PKG}）"
  echo ">>> 可删除旧内核包:"
  echo "${OLD_IMAGES}"
  OLD_HDR="$(echo "${OLD_IMAGES}" | sed 's/linux-image/linux-headers/g')"
  if confirm "删除上述旧内核及对应 headers?"; then
    echo "${OLD_IMAGES}" | xargs -r sudo apt-get purge -y || true
    echo "${OLD_HDR}" | xargs -r sudo apt-get purge -y || true
    echo "已删除旧内核包."
  fi
fi

if [[ -d "${HOME}/.cargo/registry" ]]; then
  CARGO_SIZE="$(du -sh "${HOME}/.cargo/registry" 2>/dev/null | awk '{print $1}' || true)"
  if [[ -n "${CARGO_SIZE:-}" ]]; then
    echo ">>> Cargo registry 占用约: ${CARGO_SIZE}"
    if confirm "清理 Cargo 缓存 (registry/cache/src + git/db)?"; then
      rm -rf "${HOME}/.cargo/registry/cache" "${HOME}/.cargo/registry/src" 2>/dev/null || true
      rm -rf "${HOME}/.cargo/git/db" 2>/dev/null || true
      echo "已清理 Cargo 缓存."
    fi
  fi
fi

# 额外：清理大型源码构建目录（与你当前 phase5 工具链场景相关）
if [[ -d "${HOME}/SAIL/tools/riscv-gnu-toolchain" ]]; then
  echo
  echo ">>> 检测到工具链源码目录: ${HOME}/SAIL/tools/riscv-gnu-toolchain"
  du -sh "${HOME}/SAIL/tools/riscv-gnu-toolchain"/build-* 2>/dev/null || true
  if confirm "删除 riscv-gnu-toolchain 的 build-* 中间构建目录（可大幅释放空间）?"; then
    rm -rf "${HOME}/SAIL/tools/riscv-gnu-toolchain"/build-* 2>/dev/null || true
    echo "已删除 build-* 中间目录."
  fi
fi

if [[ -d "${HOME}/SAIL/tools/riscv-zabha/share" ]]; then
  SHARE_SIZE="$(du -sh "${HOME}/SAIL/tools/riscv-zabha/share" 2>/dev/null | awk '{print $1}' || true)"
  echo ">>> 本地工具链文档目录占用: ${SHARE_SIZE:-unknown}"
  if confirm "删除 ${HOME}/SAIL/tools/riscv-zabha/share (文档/locale，不影响编译器执行)?"; then
    rm -rf "${HOME}/SAIL/tools/riscv-zabha/share" 2>/dev/null || true
    echo "已删除 share."
  fi
fi

echo
echo "=== 清理后 ==="
df -h /

AFTER_KB="$(df -k / | awk 'NR==2 {print $3}')"
FREED_KB="$(( BEFORE_KB - AFTER_KB ))"
if (( FREED_KB < 0 )); then
  FREED_KB=0
fi
echo
echo "释放约: $(human_kb_to_gb "${FREED_KB}") GB"
