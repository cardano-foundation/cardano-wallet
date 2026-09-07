#!/usr/bin/env bash
# Ratchet the transitive cardano-api package closures and deprecation
# suppressions in cardano-wallet — issue #5423.

set -uo pipefail

tree_root=${1:-.}
target_package=cardano-api

die_instrument() {
    printf 'instrument error: %s\n' "$*" >&2
    exit 2
}
is_nonnegative_integer() {
    case ${1-} in
        '' | *[!0-9]*) return 1 ;;
        *) return 0 ;;
    esac
}
yes_no_member() {
    local kind=$1 package=$2 closure_file=$3
    if grep -Fqx "$kind"$'\t'"$package" "$closure_file"; then
        printf 'yes'
    else
        printf 'no'
    fi
}

discover_packages() {
    local root=$1 cabal name
    [ -d "$root/lib" ] || return 0
    while IFS= read -r cabal; do
        name=$(awk '
            /^[[:space:]]*[Nn][Aa][Mm][Ee][[:space:]]*:/ {
                line=$0
                sub(/^[[:space:]]*[Nn][Aa][Mm][Ee][[:space:]]*:[[:space:]]*/, "", line)
                sub(/[[:space:]]*--.*/, "", line)
                sub(/[[:space:]]+$/, "", line)
                print line
                exit
            }
        ' "$cabal")
        printf '%s\t%s\n' "$name" "$cabal"
    done < <(find "$root/lib" -mindepth 2 -maxdepth 2 -type f -name '*.cabal' -print | sort)
}

extract_edges() {
    local cabal_path=$1 package_name=$2
    awk -v package="$package_name" '
        function trim(value) {
            sub(/^[[:space:],]+/, "", value)
            sub(/[[:space:]]+$/, "", value)
            return value
        }
        function remember_values(stanza, field, value,    n, values, i, item, dep) {
            gsub(/:\{[^}]*\}/, "", value)
            n=split(value, values, ",")
            for (i=1; i<=n; i++) {
                item=trim(values[i])
                if (field == "import") {
                    if (match(item, /^[A-Za-z0-9][A-Za-z0-9_.+-]*/)) {
                        dep=substr(item, RSTART, RLENGTH)
                        imports[stanza, dep]=1
                    }
                } else if (match(item, /^[A-Za-z0-9][A-Za-z0-9_.+-]*/)) {
                    dep=substr(item, RSTART, RLENGTH)
                    direct[stanza, field, dep]=1
                }
            }
        }
        function begin_stanza(line,    words) {
            if (line ~ /^library([[:space:]]+[A-Za-z0-9_.+-]+)?[[:space:]]*$/) {
                sid++; kind[sid]="LIB"; active=""; return 1
            }
            if (line ~ /^(test-suite|benchmark|executable|foreign-library)[[:space:]]+[A-Za-z0-9_.+-]+[[:space:]]*$/) {
                sid++; kind[sid]="ANY"; active=""; return 1
            }
            if (line ~ /^common[[:space:]]+[A-Za-z0-9_.+-]+[[:space:]]*$/) {
                split(line, words, /[[:space:]]+/)
                sid++; kind[sid]="COMMON"; common_name[sid]=words[2]
                common_id[words[2]]=sid; active=""; return 1
            }
            return 0
        }
        {
            line=$0
            sub(/\r$/, "", line)
            sub(/[[:space:]]*--.*/, "", line)
            if (line ~ /^[^[:space:]]/ && begin_stanza(line)) next
            if (sid == 0) next

            if (match(line, /^[[:space:]]*(build-depends|build-tool-depends|import)[[:space:]]*:/)) {
                head=substr(line, RSTART, RLENGTH)
                active=head
                sub(/^[[:space:]]*/, "", active)
                sub(/[[:space:]]*:$/, "", active)
                indent=line
                sub(/[^[:space:]].*$/, "", indent)
                active_indent=length(indent)
                value=substr(line, RSTART + RLENGTH)
                remember_values(sid, active, value)
                next
            }
            if (line ~ /^[[:space:]]*[A-Za-z][A-Za-z0-9-]*[[:space:]]*:/) {
                indent=line
                sub(/[^[:space:]].*$/, "", indent)
                if (length(indent) <= active_indent) {
                    active=""
                    next
                }
            }
            if (line ~ /^[[:space:]]*(if|else)([[:space:]]|$)/) {
                indent=line
                sub(/[^[:space:]].*$/, "", indent)
                if (length(indent) <= active_indent) {
                    active=""
                    next
                }
            }
            if (active != "" && line ~ /[^[:space:]]/) {
                remember_values(sid, active, line)
            }
        }
        END {
            bad=0
            for (key in imports) {
                split(key, part, SUBSEP)
                stanza=part[1]; common=part[2]
                if (!(common in common_id)) {
                    printf "unresolved common stanza %s in %s\n", common, package > "/dev/stderr"
                    bad=1
                } else if (kind[stanza] != "COMMON") {
                    reachable[stanza, common_id[common]]=1
                }
            }
            if (bad) exit 2

            changed=1
            while (changed) {
                changed=0
                for (key in reachable) {
                    split(key, part, SUBSEP)
                    owner=part[1]; common_stanza=part[2]
                    for (import_key in imports) {
                        split(import_key, imported, SUBSEP)
                        if (imported[1] == common_stanza) {
                            nested=common_id[imported[2]]
                            if (!((owner SUBSEP nested) in reachable)) {
                                reachable[owner, nested]=1
                                changed=1
                            }
                        }
                    }
                }
            }

            for (stanza=1; stanza<=sid; stanza++) {
                if (kind[stanza] == "COMMON") continue
                for (key in direct) {
                    split(key, part, SUBSEP)
                    if (part[1] == stanza)
                        print package "\t" kind[stanza] "\t" part[2] "\t" part[3]
                }
                for (reach_key in reachable) {
                    split(reach_key, reached, SUBSEP)
                    if (reached[1] != stanza) continue
                    for (key in direct) {
                        split(key, part, SUBSEP)
                        if (part[1] == reached[2])
                            print package "\t" kind[stanza] "\t" part[2] "\t" part[3]
                    }
                }
            }
        }
    ' "$cabal_path" | sort -u
}

closure_rows() {
    local target=$1
    awk -F '\t' -v target="$target" '
        $3 == "build-depends" {
            n++
            source[n]=$1
            kind[n]=$2
            dependency[n]=$4
            packages[$1]=1
        }
        END {
            library_closure[target]=1
            changed=1
            while (changed) {
                changed=0
                for (i=1; i<=n; i++) {
                    if (kind[i] == "LIB" && dependency[i] in library_closure &&
                        !(source[i] in library_closure)) {
                        library_closure[source[i]]=1
                        changed=1
                    }
                }
            }
            for (package in library_closure) {
                if (package != target && package in packages) any_closure[package]=1
            }
            for (i=1; i<=n; i++) {
                if (dependency[i] in library_closure) any_closure[source[i]]=1
            }
            library_count=0
            any_count=0
            for (package in library_closure)
                if (package != target && package in packages) library_count++
            for (package in any_closure)
                if (package in packages) any_count++
            print "closure-lib-count\t" library_count
            print "closure-any-count\t" any_count
            for (package in library_closure)
                if (package != target && package in packages)
                    print "closure-lib-member\t" package
            for (package in any_closure)
                if (package in packages)
                    print "closure-any-member\t" package
        }
    ' | sort
}

count_suppressions() {
    local root=$1 file
    {
        while IFS= read -r file; do
            case $file in
                *.hs)
                    awk '
                        /^\{-# OPTIONS_GHC/ &&
                        /(^|[[:space:]])(-Wno-deprecations|-fno-warn-deprecations)([[:space:]#-]|$)/ {
                            print FILENAME ":" FNR
                        }
                    ' "$file"
                    ;;
                *.cabal | */cabal.project*)
                    awk '
                        /^[[:space:]]*ghc-options[[:space:]]*:/ &&
                        /(^|[[:space:]])(-Wno-deprecations|-fno-warn-deprecations)([[:space:]]|$)/ {
                            print FILENAME ":" FNR
                        }
                    ' "$file"
                    ;;
            esac
        done < <(find "$root" -type f \( -name '*.hs' -o -name '*.cabal' -o -name 'cabal.project*' \) -print | sort)
    } | awk '{ found[++count]=$0 } END { print "count\t" count; for (i=1; i<=count; i++) print found[i] }'
}

suppression_file_classes=(hs cabal project)
suppression_spellings=(-Wno-deprecations -fno-warn-deprecations)

build_fixture() {
    local root=$1 file_class spelling cell_id
    mkdir -p "$root/lib"/{direct,any-only,neutral,common-lib,prefix,cycle-a,cycle-b,self}
    cat >"$root/lib/direct/direct.cabal" <<'EOF'
cabal-version: 3.4
name: direct
version: 0
library
  build-depends:
    base,
    cardano-api,
    neutral
EOF
    cat >"$root/lib/any-only/any-only.cabal" <<'EOF'
cabal-version: 3.4
name: any-only
version: 0
benchmark check
  build-depends: cardano-api, neutral
EOF
    cat >"$root/lib/neutral/neutral.cabal" <<'EOF'
cabal-version: 3.4
name: neutral
version: 0
library
  build-depends: base
EOF
    cat >"$root/lib/common-lib/common-lib.cabal" <<'EOF'
cabal-version: 3.4
name: common-lib
version: 0
common inherited
  build-depends:
    cardano-api:{lib,internal}
library
  import: inherited
EOF
    cat >"$root/lib/prefix/prefix.cabal" <<'EOF'
cabal-version: 3.4
name: prefix
version: 0
description: prose ghc-options: -Wno-deprecations is not a field
library
  build-depends: cardano-api-extra
  build-tool-depends: cardano-api:tool
EOF
    cat >"$root/lib/cycle-a/cycle-a.cabal" <<'EOF'
cabal-version: 3.4
name: cycle-a
version: 0
library
  build-depends: cycle-b
EOF
    cat >"$root/lib/cycle-b/cycle-b.cabal" <<'EOF'
cabal-version: 3.4
name: cycle-b
version: 0
library
  build-depends: cycle-a, cardano-api
EOF
    cat >"$root/lib/self/self.cabal" <<'EOF'
cabal-version: 3.4
name: self
version: 0
library
  build-depends: self, cardano-api
EOF
    : >"$root/suppression-cells"
    cell_id=0
    for file_class in "${suppression_file_classes[@]}"; do
        for spelling in "${suppression_spellings[@]}"; do
            cell_id=$((cell_id + 1))
            case $file_class in
                hs)
                    printf '{-# OPTIONS_GHC %s #-}\nmodule Suppression%s where\n' \
                        "$spelling" "$cell_id" \
                        >"$root/lib/direct/Suppression${cell_id}.hs"
                    ;;
                cabal)
                    printf '  ghc-options: %s\n' "$spelling" \
                        >>"$root/lib/prefix/prefix.cabal"
                    ;;
                project)
                    printf 'package *\n  ghc-options: %s\n' "$spelling" \
                        >>"$root/cabal.project.fixture"
                    ;;
                *) return 1 ;;
            esac
            printf '%s\t%s\n' "$file_class" "$spelling" >>"$root/suppression-cells"
        done
    done
    cat >"$root/README" <<'EOF'
Prose mentioning {-# OPTIONS_GHC -Wno-deprecations #-} and
ghc-options: -fno-warn-deprecations must not be counted.
EOF
}

valid_stanza_count() {
    awk '
        /^library([[:space:]]+[A-Za-z0-9_.+-]+)?[[:space:]]*$/ ||
        /^(test-suite|benchmark|executable|foreign-library|common)[[:space:]]+[A-Za-z0-9_.+-]+[[:space:]]*$/ { count++ }
        END { print count+0 }
    ' "$1"
}

prepare_population() {
    local root=$1 packages_file=$2 edges_file=$3
    local cabal_files records unique_names parse_fail name cabal
    discover_packages "$root" >"$packages_file"
    : >"$edges_file"
    cabal_files=0
    if [ -d "$root/lib" ]; then
        cabal_files=$(find "$root/lib" -mindepth 2 -maxdepth 2 -type f -name '*.cabal' -print | wc -l)
    fi
    records=$(wc -l <"$packages_file")
    unique_names=$(cut -f1 "$packages_file" | sed '/^$/d' | sort -u | wc -l)
    [ "$cabal_files" -gt 0 ] || return 1
    [ "$records" -eq "$cabal_files" ] || return 1
    [ "$unique_names" -eq "$records" ] || return 1

    parse_fail=0
    while IFS=$'\t' read -r name cabal; do
        [ -n "$name" ] && [ -r "$cabal" ] && [ "$(valid_stanza_count "$cabal")" -gt 0 ] || {
            parse_fail=1
            continue
        }
        extract_edges "$cabal" "$name" >>"$edges_file" || parse_fail=1
    done <"$packages_file"
    [ "$parse_fail" -eq 0 ] || return 1
    sort -u -o "$edges_file" "$edges_file"
}

self_check() {
    local scratch=$1
    local fixture="$scratch/fixture" empty="$scratch/empty"
    local fixture_result=PASS population_result=PASS
    local packages="$scratch/fixture-packages" edges="$scratch/fixture-edges"
    local closures="$scratch/fixture-closures" suppressions="$scratch/fixture-suppressions"
    local package_count lib_count any_count suppression_count tool_count
    local fixture_cells expected_fixture_cells

    build_fixture "$fixture" || fixture_result=FAIL
    prepare_population "$fixture" "$packages" "$edges" || population_result=FAIL
    package_count=$(wc -l <"$packages")
    closure_rows "$target_package" <"$edges" >"$closures"
    count_suppressions "$fixture" >"$suppressions"
    lib_count=$(awk -F '\t' '$1 == "closure-lib-count" { print $2 }' "$closures")
    any_count=$(awk -F '\t' '$1 == "closure-any-count" { print $2 }' "$closures")
    suppression_count=$(awk -F '\t' '$1 == "count" { print $2 }' "$suppressions")
    tool_count=$(awk -F '\t' -v target="$target_package" \
        '$3 == "build-tool-depends" && $4 == target { count++ } END { print count+0 }' "$edges")
    fixture_cells=$(wc -l <"$fixture/suppression-cells")
    expected_fixture_cells=$((${#suppression_file_classes[@]} * ${#suppression_spellings[@]}))

    [ "$package_count" -eq 8 ] || population_result=FAIL
    mkdir -p "$empty/lib/no-cabal"
    [ -z "$(discover_packages "$empty")" ] || population_result=FAIL

    [ "$lib_count" = 5 ] && [ "$any_count" = 6 ] && [ "$tool_count" = 1 ] || fixture_result=FAIL
    [ "$expected_fixture_cells" -ge 6 ] &&
        [ "$fixture_cells" -eq "$expected_fixture_cells" ] &&
        [ "$suppression_count" -eq "$fixture_cells" ] || fixture_result=FAIL
    for package in direct common-lib cycle-a cycle-b self; do
        grep -Fqx "closure-lib-member"$'\t'"$package" "$closures" || fixture_result=FAIL
    done
    grep -Fqx "closure-any-member"$'\t'any-only "$closures" || fixture_result=FAIL
    for package in any-only neutral prefix; do
        grep -Fqx "closure-lib-member"$'\t'"$package" "$closures" && fixture_result=FAIL
    done
    for package in neutral prefix; do
        grep -Fqx "closure-any-member"$'\t'"$package" "$closures" && fixture_result=FAIL
    done

    case ${CARDANO_API_CLOSURE_SELFTEST_BREAK:-} in
        '') ;;
        fixture) fixture_result=FAIL ;;
        population) population_result=FAIL ;;
        *) fixture_result=FAIL; population_result=FAIL ;;
    esac
    printf '%s\t%s\t%s\n' "$fixture_result" "$population_result" "$fixture_cells"
}

[ -d "$tree_root" ] || die_instrument "tree root '$tree_root' is not a directory"

scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
packages_file="$scratch/packages"
edges_file="$scratch/edges"
closures_file="$scratch/closures"
suppressions_file="$scratch/suppressions"

read -r fixture_check population_check fixture_cells < <(self_check "$scratch/self-check")
if ! prepare_population "$tree_root" "$packages_file" "$edges_file"; then
    population_check=FAIL
fi

packages=$(wc -l <"$packages_file")
if [ "$population_check" = PASS ]; then
    closure_rows "$target_package" <"$edges_file" >"$closures_file"
    count_suppressions "$tree_root" >"$suppressions_file"
    closure_lib=$(awk -F '\t' '$1 == "closure-lib-count" { print $2 }' "$closures_file")
    closure_any=$(awk -F '\t' '$1 == "closure-any-count" { print $2 }' "$closures_file")
    suppressions=$(awk -F '\t' '$1 == "count" { print $2 }' "$suppressions_file")
else
    closure_lib=0 closure_any=0 suppressions=0
    : >"$closures_file"
fi
excluded_build_tool_depends=$(awk -F '\t' -v target="$target_package" \
    '$3 == "build-tool-depends" && $4 == target { count++ } END { print count+0 }' "$edges_file")

closure_lib_max=${CARDANO_API_CLOSURE_LIB_MAX:-12}
closure_any_max=${CARDANO_API_CLOSURE_ANY_MAX:-13}
suppressions_max=${CARDANO_API_SUPPRESSIONS_MAX:-9}
is_nonnegative_integer "$closure_lib_max" || die_instrument "CARDANO_API_CLOSURE_LIB_MAX is not a non-negative integer"
is_nonnegative_integer "$closure_any_max" || die_instrument "CARDANO_API_CLOSURE_ANY_MAX is not a non-negative integer"
is_nonnegative_integer "$suppressions_max" || die_instrument "CARDANO_API_SUPPRESSIONS_MAX is not a non-negative integer"

printf 'packages = %s\n' "$packages"
printf 'excluded build-tool-depends = %s\n' "$excluded_build_tool_depends"
printf 'closure-lib = %s   (MAX=%s)\n' "$closure_lib" "$closure_lib_max"
printf 'closure-any = %s   (MAX=%s)\n' "$closure_any" "$closure_any_max"
printf 'suppressions = %s   (MAX=%s)\n' "$suppressions" "$suppressions_max"
printf '%s\n' 'licence closure-lib: zero licenses declaring that no production library depends on cardano-api.'
printf '%s\n' 'licence closure-any: zero licenses deleting the cardano-api pin from cabal.project.'
printf '%s\n' 'licence suppressions: zero licenses declaring that no deprecation suppression remains.'

read_dependents=$(awk -F '\t' '$3 == "build-depends" && $4 == "cardano-wallet-read" { print $1 }' "$edges_file" | sort -u)
in_closure_dependents=0
while IFS= read -r package; do
    [ -n "$package" ] || continue
    if grep -Fqx "closure-lib-member"$'\t'"$package" "$closures_file" ||
        grep -Fqx "closure-any-member"$'\t'"$package" "$closures_file"; then
        in_closure_dependents=$((in_closure_dependents + 1))
    fi
done <<<"$read_dependents"
printf 'witness cardano-wallet-read: in-closure-dependents=%s closure-lib=%s closure-any=%s\n' \
    "$in_closure_dependents" \
    "$(yes_no_member closure-lib-member cardano-wallet-read "$closures_file")" \
    "$(yes_no_member closure-any-member cardano-wallet-read "$closures_file")"
printf 'witness cardano-wallet-blackbox-benchmarks: closure-lib=%s closure-any=%s\n' \
    "$(yes_no_member closure-lib-member cardano-wallet-blackbox-benchmarks "$closures_file")" \
    "$(yes_no_member closure-any-member cardano-wallet-blackbox-benchmarks "$closures_file")"
printf 'self-check: fixture=%s population=%s cells=%s\n' \
    "$fixture_check" "$population_check" "$fixture_cells"

if [ "$fixture_check" != PASS ] || [ "$population_check" != PASS ]; then
    printf '%s\n' 'instrument error: a self-check failed; reported counts must not be trusted' >&2
    exit 2
fi

red=0
ratchet_row() {
    local row=$1 value=$2 maximum=$3 thing=$4
    if [ "$value" -gt "$maximum" ]; then
        printf 'GATE RED: %s %s > MAX=%s — a %s was ADDED.\n' "$row" "$value" "$maximum" "$thing"
        red=1
    elif [ "$value" -lt "$maximum" ]; then
        printf 'RATCHET SLACK: %s %s < MAX=%s — lower MAX to %s when landing this change.\n' \
            "$row" "$value" "$maximum" "$value"
    fi
}
ratchet_row closure-lib "$closure_lib" "$closure_lib_max" 'library closure member'
ratchet_row closure-any "$closure_any" "$closure_any_max" 'package closure member'
ratchet_row suppressions "$suppressions" "$suppressions_max" 'deprecation suppression'

if [ "$red" -ne 0 ]; then
    exit 1
fi
printf '%s\n' 'GATE GREEN: nothing was added above the configured cardano-api ratchets.'
printf '%s\n' 'NOTE: GATE GREEN does not mean the ratchet is current. Lower any MAX named by RATCHET SLACK when landing the change.'
