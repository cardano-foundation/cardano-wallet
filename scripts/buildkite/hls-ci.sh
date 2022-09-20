#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$0")/../cabal-lib.sh"

# hie-bios occasionally segfaults. Re-running is usually enough to overcome the
# segfault.
hie-bios check lib/wallet/src/Cardano/Wallet.hs || true

hie-bios check lib/wallet/src/Cardano/Wallet.hs

if [ -z ${IS_NIGHTLY+x} ]; then
    # If not nightly, just check one file.
    haskell-language-server lib/wallet/src/Cardano/Wallet.hs
else
    # If nightly, execute haskell-language-server on every file in the project.
    mapfile -t srcs < <(list_sources)
    haskell-language-server "${srcs[@]}"
fi
