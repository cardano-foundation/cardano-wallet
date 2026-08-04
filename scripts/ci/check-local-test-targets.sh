#!/usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail

root=${1:-.}
project_file="$root/cabal.project"
justfile="$root/justfile"

fail() {
    echo "Local test target check failed: $*" >&2
    exit 1
}

[ -f "$project_file" ] || fail "missing $project_file"
[ -f "$justfile" ] || fail "missing $justfile"

declared_local_targets() {
    local package_dir cabal_file package_name

    awk '
        /^[[:space:]]*packages[[:space:]]*:/ {
            in_packages = 1
            line = $0
            sub(/^[^:]*:/, "", line)
            gsub(/^[[:space:],]+|[[:space:],]+$/, "", line)
            if (line != "") print line
            next
        }
        in_packages && /^[[:space:]]*$/ { exit }
        in_packages && /^[^[:space:]]/ { exit }
        in_packages {
            line = $0
            sub(/[[:space:]]*--.*/, "", line)
            gsub(/^[[:space:],]+|[[:space:],]+$/, "", line)
            if (line != "") print line
        }
    ' "$project_file" | while IFS= read -r package_dir; do
        for cabal_file in "$root/$package_dir"/*.cabal; do
            [ -f "$cabal_file" ] || continue
            package_name=$(
                awk 'tolower($1) == "name:" { print $2; exit }' "$cabal_file"
            )
            [ -n "$package_name" ] || continue
            awk -v package="$package_name" '
                tolower($1) == "test-suite" && NF >= 2 {
                    print package ":" $2
                }
            ' "$cabal_file"
        done
    done | sort -u
}

selected_targets() {
    awk '
        /^unit-tests-cabal-match[[:space:]].*:$/ {
            in_recipe = 1
            next
        }
        in_recipe && /^[^[:space:]]/ { exit }
        in_recipe {
            for (i = 1; i <= NF; i++) {
                target = $i
                sub(/\\$/, "", target)
                if (target ~ /^[A-Za-z][A-Za-z0-9_-]*:[A-Za-z][A-Za-z0-9_-]*$/) {
                    print target
                }
            }
        }
    ' "$justfile" | sort -u
}

declared=$(declared_local_targets)
selected=$(selected_targets)

[ -n "$declared" ] || fail "parsed zero locally declared test suites"
[ -n "$selected" ] || fail "parsed zero selected targets from unit-tests-cabal-match"

nonlocal=$(
    comm -23 \
        <(printf '%s\n' "$selected") \
        <(printf '%s\n' "$declared")
)

if [ -n "$nonlocal" ]; then
    echo "Targets not declared by local project packages:" >&2
    while IFS= read -r target; do
        echo "  $target" >&2
    done <<< "$nonlocal"
    fail "unit-tests-cabal-match selects targets Cabal cannot test locally"
fi

echo "Local test targets are valid: $(printf '%s\n' "$selected" | wc -l) selected, $(printf '%s\n' "$declared" | wc -l) locally declared."
