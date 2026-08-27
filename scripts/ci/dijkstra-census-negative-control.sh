#!/usr/bin/env bash
# Negative control for scripts/ci/dijkstra-stub-gate.sh — cardano-wallet #5406.
#
# A green census proves nothing until someone shows it can go RED. This file
# does exactly that: seed exactly one throwaway Dijkstra stub as a new .hs file
# under <tree-root>/lib/, run the census gate against that same tree, and
# require exit 1.
#
# Usage:
#   dijkstra-census-negative-control.sh [tree-root]    # tree-root defaults to "."
# Exit:
#   0   the gate went RED on the seeded tree    the gate is able to fail
#   1   the gate did NOT go red, or this harness could not run
#
# No stub count and no ratchet value appear in this file: the assertion is
# "one more Dijkstra stub than this tree holds turns the gate red", which stays
# true after later tickets retire stubs and lower the ratchet.
set -uo pipefail

tree=${1:-.}
gate="$tree/scripts/ci/dijkstra-stub-gate.sh"
seed="$tree/lib/DijkstraCensusNegativeControlSeed.hs"

say() { printf '%s\n' "$*"; }
die() { printf 'negative-control: %s\n' "$*" >&2; exit 1; }

# Every exit path removes the seed — normal exit, failure, and signal.
# Inline commands (not a function reference): see evidence note about this
# host's shellcheck build flagging trap-referenced functions as never invoked.
trap 'rm -f -- "$seed"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM HUP

[ -x "$gate" ] ||
  die "census gate '$gate' is missing or not executable — the census is not installed, so there is nothing to falsify"

[ -e "$seed" ] &&
  die "refusing to overwrite existing file at seed path '$seed'"

cat >"$seed" <<'EOF'
module DijkstraCensusNegativeControlSeed where

-- Throwaway seed written and removed by
-- scripts/ci/dijkstra-census-negative-control.sh. It exists solely to add
-- exactly one counted shape (`error "...Dijkstra..."`) to the tree.
dijkstraCensusNegativeControlSeed :: ()
dijkstraCensusNegativeControlSeed =
  error "Dijkstra negative-control seed stub"
EOF

out=$("$gate" "$tree" 2>&1)
status=$?
printf '%s\n' "$out"

observed="negative-control: observed — seed 'lib/DijkstraCensusNegativeControlSeed.hs' under '$tree'; gate exit status: $status"

if [ "$status" -eq 1 ]; then
  say "$observed"
  say "negative-control: PASS — one extra Dijkstra stub turned the gate RED; the census is able to fail."
  exit 0
fi

say "$observed" >&2
case $status in
  0) die "the census did NOT notice the added stub (exit 0) — its green is meaningless" ;;
  2) die "the census failed its own self-check instead of going red (exit 2)" ;;
  *) die "unexpected gate exit status (expected 1)" ;;
esac
