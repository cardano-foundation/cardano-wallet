#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$0")/../cabal-lib.sh"

hie-bios check lib/core/src/Cardano/Wallet.hs || true

hie-bios check lib/core/src/Cardano/Wallet.hs

printf '%s\0' $(list_sources) | xargs -0 haskell-language-server
