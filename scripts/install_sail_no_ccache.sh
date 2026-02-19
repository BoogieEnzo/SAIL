#!/usr/bin/env bash
# Install dune and sail with ccache bypass: cc/gcc -> real gcc
set -euo pipefail
ROOT=/home/fengde/SAIL
export OPAMROOT=$ROOT/.opam
export PATH="$ROOT/tools/no_ccache_bin:$PATH"
unset CC CXX
export CCACHE_DISABLE=1
eval "$(opam env --root $OPAMROOT --switch 5.1.1)"
opam install -y dune
opam install -y sail
which sail && sail --version
