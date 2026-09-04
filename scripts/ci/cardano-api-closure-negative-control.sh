#!/usr/bin/env bash
# Negative control for the cardano-api closure ratchet — cardano-wallet #5423.
#
# Seeds one violation per reported row, reads the gate's own counters before
# and after, and accepts only the exact expected delta together with exit 1.
# Seed paths are collision-checked and removed on every exit path.

set -uo pipefail

tree=${1:-.}
gate="$tree/scripts/ci/cardano-api-closure-gate.sh"
failures=0
owned_dirs=()

note() { printf 'negative-control: %s\n' "$*" >&2; }
is_nonnegative_integer() {
    case ${1-} in
        '' | *[!0-9]*) return 1 ;;
        *) return 0 ;;
    esac
}
cleanup() {
    local dir
    for dir in "${owned_dirs[@]}"; do
        rm -rf -- "$dir"
    done
    owned_dirs=()
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM HUP

row_value() {
    local row=$1
    sed -nE "s/^${row} = ([0-9]+)[[:space:]]+\(MAX=[0-9]+\)$/\1/p" | head -1
}
read_rows() {
    local output=$1
    closure_lib=$(printf '%s\n' "$output" | row_value closure-lib)
    closure_any=$(printf '%s\n' "$output" | row_value closure-any)
    suppressions=$(printf '%s\n' "$output" | row_value suppressions)
}
rows_are_valid() {
    is_nonnegative_integer "$closure_lib" &&
        is_nonnegative_integer "$closure_any" &&
        is_nonnegative_integer "$suppressions"
}
run_gate() {
    gate_output=$("$gate" "$tree" 2>&1)
    gate_status=$?
    read_rows "$gate_output"
}
claim_dir() {
    local relative=$1
    local absolute="$tree/$relative"
    if ! mkdir -- "$absolute" 2>/dev/null; then
        note "refusing seed collision at '$relative'"
        return 1
    fi
    owned_dirs+=("$absolute")
}
seed_closure_lib() {
    local relative=lib/cardano-api-closure-control-lib
    claim_dir "$relative" || return 1
    (set -o noclobber; cat >"$tree/$relative/control-lib.cabal" <<'EOF'
cabal-version: 3.4
name:          cardano-api-closure-control-lib
version:       0.0.0
build-type:    Simple

library
  build-depends:
    , base
    , cardano-api
EOF
    )
}
seed_closure_any() {
    local relative=lib/cardano-api-closure-control-any
    claim_dir "$relative" || return 1
    (set -o noclobber; cat >"$tree/$relative/control-any.cabal" <<'EOF'
cabal-version: 3.4
name:          cardano-api-closure-control-any
version:       0.0.0
build-type:    Simple

benchmark control
  type: exitcode-stdio-1.0
  build-depends:
    , base
    , cardano-api
EOF
    )
}
seed_suppressions() {
    local relative=lib/cardano-api-closure-control-suppression
    claim_dir "$relative" || return 1
    (set -o noclobber; cat >"$tree/$relative/Control.hs" <<'EOF'
{-# OPTIONS_GHC -Wno-deprecations #-}
module Control where
EOF
    )
}

[ -x "$gate" ] || {
    note "gate '$gate' is missing or not executable"
    exit 1
}

run_gate
pristine_status=$gate_status
pristine_lib=$closure_lib
pristine_any=$closure_any
pristine_suppressions=$suppressions
printf 'pristine closure-lib=%s closure-any=%s suppressions=%s exit=%s\n' \
    "$pristine_lib" "$pristine_any" "$pristine_suppressions" "$pristine_status"

if [ "$pristine_status" -ne 0 ] || ! rows_are_valid; then
    note "pristine run is not a parseable green baseline"
    exit 1
fi

run_seed() {
    local label=$1 seed_function=$2 seed_path=$3
    local want_lib=$4 want_any=$5 want_suppressions=$6
    local delta_lib delta_any delta_suppressions reason

    cleanup
    if ! "$seed_function"; then
        printf 'seed %s path=%s\n' "$label" "$seed_path"
        printf 'seeded %s closure-lib= closure-any= suppressions= exit=1\n' "$label"
        printf 'delta %s closure-lib= closure-any= suppressions=\n' "$label"
        printf 'verdict %s = FAIL seed-collision-or-create-failure\n' "$label"
        failures=$((failures + 1))
        return
    fi

    printf 'seed %s path=%s\n' "$label" "$seed_path"
    run_gate
    printf 'seeded %s closure-lib=%s closure-any=%s suppressions=%s exit=%s\n' \
        "$label" "$closure_lib" "$closure_any" "$suppressions" "$gate_status"

    if rows_are_valid; then
        delta_lib=$((closure_lib - pristine_lib))
        delta_any=$((closure_any - pristine_any))
        delta_suppressions=$((suppressions - pristine_suppressions))
    else
        delta_lib=''
        delta_any=''
        delta_suppressions=''
    fi
    printf 'delta %s closure-lib=%s closure-any=%s suppressions=%s\n' \
        "$label" "$delta_lib" "$delta_any" "$delta_suppressions"

    reason=
    [ "$delta_lib" = "$want_lib" ] || reason="$reason closure-lib-delta"
    [ "$delta_any" = "$want_any" ] || reason="$reason closure-any-delta"
    [ "$delta_suppressions" = "$want_suppressions" ] || reason="$reason suppressions-delta"
    [ "$gate_status" -eq 1 ] || reason="$reason gate-exit-$gate_status-not-1"
    if [ -z "$reason" ]; then
        printf 'verdict %s = PASS measured-delta-and-gate-exit\n' "$label"
    else
        printf 'verdict %s = FAIL%s\n' "$label" "$reason"
        failures=$((failures + 1))
    fi
    cleanup
}

run_seed closure-lib seed_closure_lib \
    lib/cardano-api-closure-control-lib/control-lib.cabal 1 1 0
run_seed closure-any seed_closure_any \
    lib/cardano-api-closure-control-any/control-any.cabal 0 1 0
run_seed suppressions seed_suppressions \
    lib/cardano-api-closure-control-suppression/Control.hs 0 0 1

if [ "$failures" -ne 0 ]; then
    note "FAIL — $failures seeded row(s) did not cause the required measured RED"
    exit 1
fi

note "PASS — all three independently seeded rows caused their exact measured delta and exit 1"
