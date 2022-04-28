#!/usr/bin/env bash

set -euo pipefail

. "$(dirname "$0")/../cabal-lib.sh"

# hie-bios occasionally segfaults. Re-running is usually enough to overcome the
# segfault.
hie-bios check lib/core/src/Cardano/Wallet.hs || true

hie-bios check lib/core/src/Cardano/Wallet.hs

haskell-language-server lib/core/src/Cardano/Wallet.hs
