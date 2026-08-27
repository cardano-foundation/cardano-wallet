#!/usr/bin/env bash
# Negative control for scripts/ci/dijkstra-stub-gate.sh — cardano-wallet #5406.
# Implements the control contract, version 2:
#   specs/5406-dijkstra-census-ratchet/functions-model.md ("Version 2")
#
# A census nobody can make go red proves nothing — and neither does a red whose
# cause was never measured. This harness seeds exactly one counted shape as a
# new .hs file under <tree-root>/lib/, reads the census total from the census's
# own stdout before and after seeding, and succeeds only when the measured
# effect is exactly one added stub and the census went red because of it.
# Its verdict is bound to those two totals; a red unrelated to any counted
# delta is rejected as a vacuous pass.
#
# Usage:
#   dijkstra-census-negative-control.sh [tree-root]     # tree-root defaults to "."
# Exit:
#   0   pristine census exited 0 AND measured delta == 1 AND gate_exit == 1
#   1   anything else (including delta != 1: that is this control failing,
#       not the census)
#
# Machine-readable result lines on stdout, exactly these five keys:
#   seed=<path relative to tree-root>
#   pristine_total=<integer>    total the census reports on the untouched tree
#   seeded_total=<integer>      total with the seed present
#   delta=<integer>             seeded_total - pristine_total
#   gate_exit=<integer>         census exit status on the seeded tree
#
# No ratchet value and no count appear in this file, so later tickets can lower
# the ratchet without invalidating it.
set -uo pipefail

seed_rel="lib/DijkstraCensusNegativeControlSeed.hs"
tree=${1:-.}
gate="$tree/scripts/ci/dijkstra-stub-gate.sh"
seed="$tree/$seed_rel"

total_from() { sed -n 's/^total = \([0-9][0-9]*\) across \([0-9][0-9]*\) files.*/\1/p' | tail -n 1; }
files_from() { sed -n 's/^total = [0-9][0-9]* across \([0-9][0-9]*\) files.*/\1/p' | tail -n 1; }
is_int() { case $1 in '' | *[!0-9]*) return 1 ;; *) return 0 ;; esac; }

note() { printf 'negative-control: %s\n' "$*" >&2; }
emit() { printf '%s=%s\n' "$1" "$2"; }

[ -x "$gate" ] || {
  note "census gate '$gate' is missing or not executable — there is nothing to falsify"
  exit 1
}

# Cleanup runs on every exit path but stays INERT until this invocation owns
# the artifact it created. Inlined trap commands rather than a helper function:
# see evidence/shellcheck-sc2329-repro.log — this host's shellcheck build flags
# functions referenced solely through traps.
owned=0
trap 'if [ "$owned" -eq 1 ]; then rm -f -- "$seed"; fi' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM HUP

if [ -e "$seed" ]; then
  note "refusing to overwrite existing file at seed path '$seed_rel' — collision rejected"
  exit 1
fi

p_out=$("$gate" "$tree")
p_status=$?
p_total=$(printf '%s\n' "$p_out" | total_from)
p_files=$(printf '%s\n' "$p_out" | files_from)

# Exactly one counted shape lives in the heredoc below, on its last line; the
# surrounding prose deliberately avoids every counted keyword, because the
# instrument collapses whitespace across the whole file before matching and a
# comment is not a hiding place.
seed_body() {
  cat <<'EOF'
module DijkstraCensusNegativeControlSeed where

-- Throwaway seed created and removed by
-- scripts/ci/dijkstra-census-negative-control.sh to measure whether the
-- census notices exactly one added stub, namely the one below.
dijkstraCensusNegativeControlSeed :: ()
dijkstraCensusNegativeControlSeed =
  error "Dijkstra census negative-control seed"
EOF
}

# Create atomically: noclobber fails the redirect if the path appeared since
# the collision check; only after a successful create does the invocation own
# the artifact and allow cleanup to act on it.
if ! (set -o noclobber; seed_body >"$seed"); then
  note "refusing to overwrite existing file at seed path '$seed_rel' — collision rejected"
  exit 1
fi
owned=1

s_out=$("$gate" "$tree")
s_status=$?
s_total=$(printf '%s\n' "$s_out" | total_from)
s_files=$(printf '%s\n' "$s_out" | files_from)

printf '%s\n' "$p_out" | sed 's/^/census-pristine| /'
printf '%s\n' "$s_out" | sed 's/^/census-seeded|   /'

delta=""
if is_int "$p_total" && is_int "$s_total"; then
  delta=$((s_total - p_total))
fi
file_delta=""
if is_int "$p_files" && is_int "$s_files"; then
  file_delta=$((s_files - p_files))
fi

emit seed "$seed_rel"
emit pristine_total "$p_total"
emit seeded_total "$s_total"
emit delta "$delta"
emit gate_exit "$s_status"

reasons=""
ok=1
[ "$p_status" -eq 0 ] || { ok=0; reasons="$reasons pristine-run-exited-$p_status-not-0"; }
is_int "$p_total" || { ok=0; reasons="$reasons pristine-total-unparseable"; }
is_int "$s_total" || { ok=0; reasons="$reasons seeded-total-unparseable"; }
if [ -n "$delta" ]; then
  [ "$delta" -eq 1 ] || { ok=0; reasons="$reasons measured-delta-$delta-not-1"; }
else
  ok=0; reasons="$reasons delta-uncomputable"
fi
# A second instrument property, same class: one counted shape lands in exactly
# one new file.
if [ -n "$file_delta" ]; then
  [ "$file_delta" -eq 1 ] || { ok=0; reasons="$reasons measured-file-delta-$file_delta-not-1"; }
fi
[ "$s_status" -eq 1 ] || { ok=0; reasons="$reasons gate-exit-$s_status-not-1"; }

if [ "$ok" -ne 1 ]; then
  note "FAIL —$reasons ; measurements are emitted above"
  exit 1
fi

printf 'negative-control: PASS — census moved total=%s->%s across %s->%s files (measured delta=1) and exited 1 because of the single added stub at %s\n' \
  "$p_total" "$s_total" "$p_files" "$s_files" "$seed_rel"
exit 0
