#!/usr/bin/env bash

######################################################################
# Generates a cradle config for haskell-language-server,
# This script is used by ../hie-direnv.yaml.
######################################################################

set -euo pipefail

# See https://github.com/haskell/hie-bios#bios
make_hie_bios() {
  ghci_flags
  list_modules
}

# See https://github.com/haskell/hie-bios#cradle-dependencies
make_hie_bios_deps() {
  list_cabal_files

  cat<<EOF
cabal.project
default.nix
shell.nix
EOF
}

: "${HIE_BIOS_OUTPUT:=/dev/stdout}"
: "${HIE_BIOS_ARG:=${1:-}}"
: "${HIE_BIOS_DEPS:=/dev/null}"

cd "$(dirname "$0")"/.. || exit

source_file=${HIE_BIOS_ARG//$(pwd)\//}

if [[ "$source_file" =~ ^lib/.*\.hs$ || -z "$source_file" ]]; then
  echo "$0: Generating hie-bios for source file $source_file."
  . "$(dirname "$0")/cabal-lib.sh"

  setup_cabal_plan

  echo "Writing cradle config to $HIE_BIOS_OUTPUT"
  make_hie_bios > "$HIE_BIOS_OUTPUT"

  echo "Writing dependencies to $HIE_BIOS_DEPS"
  make_hie_bios_deps > "$HIE_BIOS_DEPS"

  echo "Finished."
else
  echo "Ignoring $source_file."
fi
