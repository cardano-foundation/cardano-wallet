#!/usr/bin/env bash
set -euo pipefail

git diff --check

# The Lean badge/link must be fully removed from README.md, not just edited.
if grep -qi 'lean' README.md; then
  echo "FAIL: README.md still references Lean"
  exit 1
fi

# Sanity: badge block must still be well-formed (equal <a> open/close count).
opens=$(grep -c '<a href=' README.md)
closes=$(grep -c '</a>' README.md)
if [ "$opens" -ne "$closes" ]; then
  echo "FAIL: README.md badge block has mismatched <a>/</a> tags ($opens opens, $closes closes)"
  exit 1
fi

echo "gate OK"
