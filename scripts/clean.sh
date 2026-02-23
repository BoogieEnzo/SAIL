#!/bin/bash
# 强力清理脚本 v3：全局（Docker/Snap/缓存等）+ SAIL 项目目录
# 有风险的操作会先列出并询问 [y/N]，确认后再执行

set -e
BEFORE=$(df / | tail -1 | awk '{print $3}')

# SAIL 根目录：优先环境变量，否则按脚本位置推断（脚本在 SAIL/scripts/clean.sh）
SAIL_ROOT="${SAIL_ROOT:-}"
if [[ -z "$SAIL_ROOT" ]] && [[ -f "${BASH_SOURCE[0]}" ]]; then
  _script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  if [[ -d "${_script_dir}/../sail-riscv" ]] && [[ -d "${_script_dir}/../docs" ]]; then
    SAIL_ROOT="$(cd "${_script_dir}/.." && pwd)"
  fi
fi

confirm() {
  local msg="$1"
  echo ""
  read -p "$msg [y/N] " -n 1 r
  echo
  [[ $r =~ ^[yY]$ ]]
}

echo "=== 清理前 ==="
df -h /
echo ""

# =============================================================================
# 一、无需确认的清理（安全）
# =============================================================================

# ---------------------------------------------------------------------------
# 1. Docker 深度清理
# ---------------------------------------------------------------------------
if command -v docker &>/dev/null; then
  echo "=== Docker 磁盘占用（清理前）==="
  docker system df 2>/dev/null || true
  echo ">>> 执行: system prune, image prune -a, volume prune, builder prune"
  docker system prune -f 2>/dev/null || true
  docker image prune -a -f 2>/dev/null || true
  docker volume prune -f 2>/dev/null || true
  docker builder prune -a -f 2>/dev/null || true
  echo "Docker 清理完成."
  docker system df 2>/dev/null || true
  echo ""
fi

# ---------------------------------------------------------------------------
# 2. Podman（若存在，与 Docker 类似）
# ---------------------------------------------------------------------------
if command -v podman &>/dev/null; then
  echo "=== Podman 清理 ==="
  podman system prune -a -f 2>/dev/null || true
  echo ""
fi

# ---------------------------------------------------------------------------
# 3. 系统临时目录
# ---------------------------------------------------------------------------
echo "=== 清理 /tmp、/var/tmp ==="
sudo rm -rf /tmp/* /tmp/.* 2>/dev/null || true
sudo rm -rf /var/tmp/* /var/tmp/.* 2>/dev/null || true
echo ""

# ---------------------------------------------------------------------------
# 4. APT 缓存
# ---------------------------------------------------------------------------
echo "=== APT 清理 ==="
sudo apt-get clean 2>/dev/null || true
sudo apt-get autoclean 2>/dev/null || true
sudo apt-get autoremove -y 2>/dev/null || true
echo ""

# ---------------------------------------------------------------------------
# 5. 系统 journal 日志（保留 3 天）
# ---------------------------------------------------------------------------
echo "=== 裁剪 journal（保留 3 天）==="
sudo journalctl --vacuum-time=3d 2>/dev/null || true
echo ""

# ---------------------------------------------------------------------------
# 6. Snap 旧版本
# ---------------------------------------------------------------------------
if command -v snap &>/dev/null; then
  echo "=== 删除 Snap 旧版本 ==="
  sudo snap list --all 2>/dev/null | awk '/disabled/{print $1, $3}' | while read pkg rev; do sudo snap remove "$pkg" --revision="$rev" 2>/dev/null; done || true
  echo ""
fi

# ---------------------------------------------------------------------------
# 7. 用户缓存与 Cursor
# ---------------------------------------------------------------------------
echo "=== 用户缓存 ==="
rm -rf ~/.cache/pip ~/.cache/matplotlib ~/.cache/huggingface 2>/dev/null || true
rm -rf ~/.cache/npm ~/.cache/yarn ~/.cache/go-build 2>/dev/null || true
rm -rf ~/.cache/thumbnails/* ~/.cache/fontconfig 2>/dev/null || true
rm -rf ~/.gradle/caches 2>/dev/null || true
echo "=== Cursor 缓存 ==="
rm -rf ~/.cursor/Cache/* ~/.cursor/CachedData/* 2>/dev/null || true
rm -rf ~/.cursor/CachedExtensions/* ~/.cursor/CachedExtensionVSIXs/* 2>/dev/null || true
echo ""

# ---------------------------------------------------------------------------
# 8. 系统缓存目录（字体、man 等）
# ---------------------------------------------------------------------------
echo "=== 系统缓存（字体、man）==="
sudo rm -rf /var/cache/fontconfig/* 2>/dev/null || true
sudo rm -rf /var/cache/man/* 2>/dev/null || true
echo ""

# =============================================================================
# 二、需要确认的清理（执行前会询问）
# =============================================================================

# ---------------------------------------------------------------------------
# 9. 回收站
# ---------------------------------------------------------------------------
TRASH_SIZE=$(du -sh ~/.local/share/Trash 2>/dev/null | cut -f1 || echo "0")
if [[ -n "$TRASH_SIZE" && "$TRASH_SIZE" != "0" ]]; then
  echo ">>> 回收站占用约: $TRASH_SIZE"
  if confirm "清空回收站?"; then
    rm -rf ~/.local/share/Trash/files/* ~/.local/share/Trash/info/* 2>/dev/null || true
    echo "已清空回收站."
  fi
fi

# ---------------------------------------------------------------------------
# 10. Flatpak 未使用的运行时/应用
# ---------------------------------------------------------------------------
if command -v flatpak &>/dev/null; then
  UNUSED=$(flatpak list --unused 2>/dev/null | wc -l)
  if [[ "$UNUSED" -gt 0 ]]; then
    echo ">>> 未使用的 Flatpak 项数量: $UNUSED"
    flatpak list --unused 2>/dev/null || true
    if confirm "卸载上述未使用的 Flatpak 运行时/应用?"; then
      flatpak uninstall --unused -y 2>/dev/null || true
      echo "已卸载."
    fi
  fi
fi

# ---------------------------------------------------------------------------
# 11. 崩溃报告与核心转储（/var/crash、core dump）
# ---------------------------------------------------------------------------
if [[ -d /var/crash ]]; then
  CRASH_SIZE=$(sudo du -sh /var/crash 2>/dev/null | cut -f1 || echo "0")
  if [[ -n "$CRASH_SIZE" && "$CRASH_SIZE" != "0" ]]; then
    echo ">>> /var/crash 占用约: $CRASH_SIZE"
    if confirm "清空崩溃报告目录 /var/crash?"; then
      sudo rm -rf /var/crash/*
      echo "已清空."
    fi
  fi
fi

# ---------------------------------------------------------------------------
# 12. /var/log 中的大文件（>100MB）
# ---------------------------------------------------------------------------
BIG_LOGS=$(sudo find /var/log -type f -size +100M 2>/dev/null || true)
if [[ -n "$BIG_LOGS" ]]; then
  echo ">>> /var/log 中大于 100MB 的文件:"
  echo "$BIG_LOGS" | while read f; do sudo du -sh "$f" 2>/dev/null; done
  if confirm "截断上述大日志文件（保留 0 字节，需 sudo）?"; then
    echo "$BIG_LOGS" | while read f; do sudo truncate -s 0 "$f" 2>/dev/null; done
    echo "已截断."
  fi
fi

# ---------------------------------------------------------------------------
# 13. 旧内核（保留当前内核，列出可删的由你确认）
# ---------------------------------------------------------------------------
CURRENT=$(uname -r)
CURRENT_PKG="linux-image-${CURRENT}"
OLD_IMAGES=$(dpkg -l 'linux-image-*' 2>/dev/null | awk '/^ii/{print $2}' | grep -v "^${CURRENT_PKG}$" || true)
if [[ -n "$OLD_IMAGES" ]]; then
  echo ">>> 当前内核: $CURRENT （保留 $CURRENT_PKG）"
  echo ">>> 可删除的旧内核包:"
  echo "$OLD_IMAGES"
  OLD_HDR=$(echo "$OLD_IMAGES" | sed 's/linux-image/linux-headers/g' | tr '\n' ' ')
  if confirm "删除上述旧内核包（约可释放数百 MB）?"; then
    echo "$OLD_IMAGES" | xargs -r sudo apt-get purge -y 2>/dev/null || true
    echo "$OLD_HDR" | xargs -r sudo apt-get purge -y 2>/dev/null || true
    echo "已删除. 重启后请从当前内核启动."
  fi
fi

# ---------------------------------------------------------------------------
# 14. Cargo 缓存（Rust 编译缓存，删除后下次编译会重新下载）
# ---------------------------------------------------------------------------
if [[ -d ~/.cargo/registry ]]; then
  CARGO_SIZE=$(du -sh ~/.cargo/registry 2>/dev/null | cut -f1 || echo "0")
  if [[ -n "$CARGO_SIZE" && "$CARGO_SIZE" != "0" ]]; then
    echo ">>> Cargo registry 约: $CARGO_SIZE（删除后 Rust 编译会重新下载依赖）"
    if confirm "清理 Cargo 缓存（registry/cache、git/db）?"; then
      rm -rf ~/.cargo/registry/cache ~/.cargo/registry/src 2>/dev/null || true
      rm -rf ~/.cargo/git/db 2>/dev/null || true
      echo "已清理."
    fi
  fi
fi

# =============================================================================
# 三、SAIL 项目目录（可选，仅在检测到 SAIL 根目录时执行）
# =============================================================================
if [[ -n "$SAIL_ROOT" ]] && [[ -d "$SAIL_ROOT" ]]; then
  echo "=== SAIL 项目目录清理（根目录: $SAIL_ROOT）==="
  echo ""

  # ---------------------------------------------------------------------------
  # 15. docs/automation-logs 超过 7 天的日志
  # ---------------------------------------------------------------------------
  if [[ -d "${SAIL_ROOT}/docs/automation-logs" ]]; then
    OLD_SAIL_LOGS=$(find "${SAIL_ROOT}/docs/automation-logs" -maxdepth 1 -type f -name "*.log" -mtime +7 2>/dev/null || true)
    if [[ -n "$OLD_SAIL_LOGS" ]]; then
      _cnt=$(echo "$OLD_SAIL_LOGS" | wc -l)
      echo ">>> docs/automation-logs 中超过 7 天的日志文件: $_cnt 个"
      if confirm "删除上述旧日志?"; then
        echo "$OLD_SAIL_LOGS" | xargs -r rm -f 2>/dev/null || true
        echo "已删除."
      fi
    fi
  fi

  # ---------------------------------------------------------------------------
  # 16. tools/riscv-gnu-toolchain（源码+构建目录，约数 GB；删除后需重新 clone 才能再编工具链）
  # ---------------------------------------------------------------------------
  if [[ -d "${SAIL_ROOT}/tools/riscv-gnu-toolchain" ]]; then
    TC_SIZE=$(du -sh "${SAIL_ROOT}/tools/riscv-gnu-toolchain" 2>/dev/null | cut -f1 || echo "?")
    echo ">>> tools/riscv-gnu-toolchain 约: $TC_SIZE（仅构建工具链用；若已装好 tools/riscv-zabha 且不需重编可删）"
    if confirm "删除 tools/riscv-gnu-toolchain（释放空间，需时再 clone）?"; then
      rm -rf "${SAIL_ROOT}/tools/riscv-gnu-toolchain"
      echo "已删除."
    fi
  fi

  # ---------------------------------------------------------------------------
  # 17. tools/riscv-zabha/share（文档与 locale，不影响 gcc/as 执行）
  # ---------------------------------------------------------------------------
  if [[ -d "${SAIL_ROOT}/tools/riscv-zabha/share" ]]; then
    SHARE_SIZE=$(du -sh "${SAIL_ROOT}/tools/riscv-zabha/share" 2>/dev/null | cut -f1 || echo "?")
    echo ">>> tools/riscv-zabha/share 约: $SHARE_SIZE（文档/locale，删除不影响编译器）"
    if confirm "删除 tools/riscv-zabha/share?"; then
      rm -rf "${SAIL_ROOT}/tools/riscv-zabha/share"
      echo "已删除."
    fi
  fi

  # ---------------------------------------------------------------------------
  # 18. sail-riscv/build（C 模拟器构建产物；删除后需重新编译 sail_riscv_sim）
  # ---------------------------------------------------------------------------
  if [[ -d "${SAIL_ROOT}/sail-riscv/build" ]]; then
    BUILD_SIZE=$(du -sh "${SAIL_ROOT}/sail-riscv/build" 2>/dev/null | cut -f1 || echo "?")
    echo ">>> sail-riscv/build 约: $BUILD_SIZE（删除后需在 sail-riscv 下重新执行 build_simulator.sh）"
    if confirm "删除 sail-riscv/build?"; then
      rm -rf "${SAIL_ROOT}/sail-riscv/build"
      echo "已删除."
    fi
  fi

  echo ""
fi

# =============================================================================
# 汇总
# =============================================================================
echo ""
echo "=== 清理后 ==="
df -h /
AFTER=$(df / | tail -1 | awk '{print $3}')
echo ""
echo "释放约: $(( (BEFORE - AFTER) / 1024 )) GB"
