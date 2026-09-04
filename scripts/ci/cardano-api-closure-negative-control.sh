#!/usr/bin/env bash
# Negative control for the cardano-api closure ratchet — cardano-wallet #5423.
#
# Asserts the pristine stdout contract and both ratchet directions, then seeds
# one violation per row and accepts only the exact delta, named RED, and exit 1.
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
line_count() {
    local pattern=$1 output=$2
    printf '%s\n' "$output" | grep -Ec "$pattern" || true
}
require_one_line() {
    local reason=$1 pattern=$2 output=$3
    if [ "$(line_count "$pattern" "$output")" -ne 1 ]; then
        note "pristine-contract-$reason"
        failures=$((failures + 1))
    fi
}
validate_pristine_contract() {
    local output=$1 row cells

    require_one_line packages '^packages = [1-9][0-9]*$' "$output"
    require_one_line excluded-build-tool-depends '^excluded build-tool-depends = [0-9]+$' "$output"
    for row in closure-lib closure-any suppressions; do
        require_one_line "row-$row" "^$row = [0-9]+   \\(MAX=[0-9]+\\)$" "$output"
        require_one_line "licence-$row" "^licence $row: .+" "$output"
    done
    require_one_line witness-cardano-wallet-read \
        '^witness cardano-wallet-read: in-closure-dependents=[0-9]+ closure-lib=(yes|no) closure-any=(yes|no)$' "$output"
    require_one_line witness-cardano-wallet-blackbox-benchmarks \
        '^witness cardano-wallet-blackbox-benchmarks: closure-lib=(yes|no) closure-any=(yes|no)$' "$output"
    require_one_line self-check \
        '^self-check: fixture=PASS population=PASS cells=[0-9]+$' "$output"
    cells=$(printf '%s\n' "$output" | sed -nE \
        's/^self-check: fixture=PASS population=PASS cells=([0-9]+)$/\1/p' | head -1)
    if ! is_nonnegative_integer "$cells" || [ "$cells" -lt 6 ]; then
        note "pristine-contract-self-check-cells-at-least-6"
        failures=$((failures + 1))
    fi
    require_one_line gate-green '^GATE GREEN: .+' "$output"
    require_one_line note-green-not-current \
        '^NOTE: GATE GREEN does not mean the ratchet is current\. .+' "$output"
    if printf '%s\n' "$output" | grep -q '^GATE RED:'; then
        note "pristine-contract-unexpected-gate-red"
        failures=$((failures + 1))
    fi
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

validate_pristine_contract "$gate_output"
if [ "$pristine_status" -ne 0 ]; then
    note "pristine-gate-exit-$pristine_status-not-0"
    failures=$((failures + 1))
fi
if ! rows_are_valid; then
    note "pristine-rows-not-parseable"
    failures=$((failures + 1))
fi
if [ "$failures" -ne 0 ]; then
    note "FAIL — pristine gate violated $failures baseline contract clause(s)"
    exit 1
fi

run_fall() {
    local row=$1 value=$2 maximum_var=$3 raised reason
    raised=$((value + 1))
    gate_output=$(env "$maximum_var=$raised" "$gate" "$tree" 2>&1)
    gate_status=$?
    reason=
    [ "$gate_status" -eq 0 ] || reason="$reason gate-exit-$gate_status-not-0"
    printf '%s\n' "$gate_output" |
        grep -qE "^RATCHET SLACK: $row $value < MAX=$raised " ||
        reason="$reason missing-ratchet-slack-$row"
    if printf '%s\n' "$gate_output" | grep -q '^GATE RED:'; then
        reason="$reason unexpected-gate-red"
    fi
    printf 'fall %s value=%s max=%s exit=%s\n' "$row" "$value" "$raised" "$gate_status"
    if [ -z "$reason" ]; then
        printf 'verdict fall-%s = PASS advisory-slack-and-gate-exit\n' "$row"
    else
        printf 'verdict fall-%s = FAIL%s\n' "$row" "$reason"
        failures=$((failures + 1))
    fi
}

run_fall closure-lib "$pristine_lib" CARDANO_API_CLOSURE_LIB_MAX
run_fall closure-any "$pristine_any" CARDANO_API_CLOSURE_ANY_MAX
run_fall suppressions "$pristine_suppressions" CARDANO_API_SUPPRESSIONS_MAX

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
    printf '%s\n' "$gate_output" | grep -qE "^GATE RED: $label " ||
        reason="$reason missing-gate-red-$label"
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
    note "FAIL — $failures ratchet contract check(s) failed"
    exit 1
fi

note "PASS — pristine contract, per-row falls, and independently seeded rises all held"
