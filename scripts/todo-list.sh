#!/usr/bin/env bash

if type -p lentil > /dev/null; then
    if [ -n "${1:-}" ]; then
        target="$1"
        shift
    else
        cd "$(dirname "$0")/.." || exit
        target=lib
    fi

    echo "+++ TODO list"
    exec lentil "$target" "$@"
else
    echo "The lentil command was not found."
    echo "Either run this in nix-shell, or cabal install lentil."
    exit 1
fi
