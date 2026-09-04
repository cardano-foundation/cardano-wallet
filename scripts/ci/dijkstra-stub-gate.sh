#!/usr/bin/env bash
# ACCEPTANCE CRITERION for cardano-wallet issue #5209 (re-cut).
#
# RATCHET over Dijkstra era `error` stubs and silently-skipped Dijkstra era
# tests under lib/. Fails when the count RISES above the ratchet. Counts, never matching a
# hand-listed set of message strings: the stubs carry 29 distinct literals and
# 4 of them use Haskell string gaps, so any single-line grep undercounts.
#
# Usage: dijkstra-stub-gate.sh [tree-root]     (default: repo root = $PWD)
# Exit:  0 = at or below ratchet, 1 = regression above ratchet, 2 = self-check failed.
set -uo pipefail
root=${1:-.}
lib="$root/lib"
[ -d "$lib" ] || { echo "gate: no lib/ under $root" >&2; exit 2; }

count() { # $1=file $2=keyword(error|pendingWith)
  KW="$2" perl -0777 -ne '
    my $kw = $ENV{KW};
    my $c=0;
    while (/\Q$kw\E\s*(?:\$\s*)?"((?:[^"\\]|\\.)*)"/gs) {
      my $s=$1; $s=~s/\s+/ /g; $c++ if $s=~/Dijkstra/i
    }
    print $c
  ' "$1"
}

# --- instrument self-check: it must be able to count and able to return zero.
selfdir=$(mktemp -d); trap 'rm -rf "$selfdir"' EXIT
printf 'f = error "DijkstraEra not yet supported"\ng = pendingWith "TODO: Dijkstra"\n' > "$selfdir/pos.hs"
printf 'f = error "ConwayEra not yet supported"\ng = "Dijkstra"\n' > "$selfdir/neg.hs"
[ "$(count "$selfdir/pos.hs" error)" = 1 ] && [ "$(count "$selfdir/pos.hs" pendingWith)" = 1 ] \
  || { echo "gate: POSITIVE CONTROL FAILED - instrument cannot count" >&2; exit 2; }
[ "$(count "$selfdir/neg.hs" error)" = 0 ] && [ "$(count "$selfdir/neg.hs" pendingWith)" = 0 ] \
  || { echo "gate: NEGATIVE CONTROL FAILED - instrument cannot return zero" >&2; exit 2; }

tot_e=0; tot_p=0; files=0
while IFS= read -r f; do
  e=$(count "$f" error); p=$(count "$f" pendingWith)
  if [ "$e" -gt 0 ] || [ "$p" -gt 0 ]; then
    files=$((files+1)); tot_e=$((tot_e+e)); tot_p=$((tot_p+p))
    printf '  %2d error  %2d pendingWith  %s\n' "$e" "$p" "${f#"$root"/}"
  fi
done < <(find "$lib" -name '*.hs' -type f | sort)

echo "controls: positive=PASS negative=PASS"
echo "error stubs mentioning Dijkstra (multi-line-aware) = $tot_e   (ratchet target: 0)"
echo "pendingWith mentioning Dijkstra                    = $tot_p   (ratchet target: 0)"
total=$((tot_e+tot_p))

# --- ratchet ------------------------------------------------------------
# MAX is the number of Dijkstra stubs currently tolerated. It only ever goes
# DOWN. Each child that retires stubs lowers it in the same PR; its terminal
# value is 0, which is issue #5209's acceptance criterion.
MAX=${DIJKSTRA_STUB_MAX:-39}

echo "total = $total across $files files   (ratchet MAX=$MAX)"

if [ "$total" -gt "$MAX" ]; then
    echo "GATE RED: $total Dijkstra stubs > ratchet MAX=$MAX — a stub was ADDED."
    echo "  Fix the regression. Do NOT raise MAX; the ratchet only goes down."
    exit 1
fi

if [ "$total" -lt "$MAX" ]; then
    echo "RATCHET SLACK: $total < MAX=$MAX — $((MAX-total)) stub(s) retired but"
    echo "  the ratchet was not tightened. Lower it in this same PR:"
    echo "      DIJKSTRA_STUB_MAX=$total"
    if [ "${DIJKSTRA_STUB_STRICT:-0}" = "1" ]; then
        echo "GATE RED (strict): ratchet not tightened."
        exit 1
    fi
fi

if [ "$total" -eq 0 ]; then
    echo "GATE GREEN: no Dijkstra stubs remain — #5209 acceptance criterion met."
else
    echo "GATE GREEN: $total stubs, at or below the ratchet."
fi
