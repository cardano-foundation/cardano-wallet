#!/usr/bin/env bash

######################################################################
# Functions for querying Cabal projects

# shellcheck disable=SC2034

builddir=dist-newstyle
cabal_opts=("--builddir=$builddir")
plan_json=$builddir/cache/plan.json

list_cabal_files() {
    # Exclude prototypes dir because it's a different project.
    git ls-files '*.cabal' | grep -v prototypes/
}

list_packages() {
    list_cabal_files | xargs basename -a | sed 's/\.cabal//'
}

get_cabal_version() {
    awk '/^version:/ { print $2; }' "$(git ls-files '*.cabal' | head -n1)"
}

list_sources() {
    # Exclude lib/wallet/extra. Those files are Plutus scripts intended
    # to be serialised for use in the tests. They are not intended to be built
    # with the project.
    # Exclude prototypes dir because it's a different project.
    git ls-files 'lib/**/*.hs' | grep -v Main.hs | grep -v prototypes/ | grep -v lib/wallet/extra | grep -v lib/wallet-e2e
}

# usage: query_plan_json PACKAGE COMP:NAME KEY
query_plan_json() {
    jq -r '.["install-plan"][]|select(."pkg-name"=="'"$1"'")|select(.["component-name"]=="'"$2"'")["'"$3"'"]' <$plan_json
}

# usage: get_dist_dir PACKAGE
get_dist_dir() {
    relpath "$(query_plan_json "$1" lib dist-dir)"
}

# usage: get_bin_dir PACKAGE exe:NAME
get_bin_dir() {
    dirname "$(relpath "$(query_plan_json "$1" "$2" bin-file)")"
}

relpath() {
    # path must exist
    # realpath "--relative-to=$(pwd)" "$1"
    # shellcheck disable=SC2001
    echo "$1" | sed "s=$(pwd)/=="
}

setup_cabal_plan() {
    if [ ! -f $plan_json ]; then
        echo "$0: Running Cabal to generate build plan $plan_json"
        cabal "${cabal_opts[@]}" update
        cabal "${cabal_opts[@]}" configure
    fi
}

######################################################################
# Config file generation

ghci_flags() {
    cat <<EOF
-XOverloadedStrings
-XNoImplicitPrelude
-XTypeApplications
-XDataKinds
-fwarn-unused-binds
-fwarn-unused-imports
-fwarn-orphans
-fprint-potential-instances
-Wno-missing-home-modules
-Wredundant-constraints
EOF

    # Add all source directories to ghci command-line
    mapfile -t sources < <(list_sources)
    dirname "${sources[@]}" | sed -e 's/^\([^A-Z]*\).*$/-i\1/' | sed 's=/$==' | sort -u

    # Add Cabal-generated Paths sources
    list_packages | while read -r pkg; do
        echo "-i$(get_dist_dir "$pkg")/build/autogen"
    done
}

# List all modules for ghci command-line
list_modules() {
    list_sources | sed -e 's/^[^A-Z]*\(.*\)\.hs$/\1/' | grep -v '^[[:space:]]*$' | sed 'y=/=.='
    list_packages | sed -e 'y/-/_/' -e 's/^/Paths_/'
}
