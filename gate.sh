#!/usr/bin/env bash
set -euo pipefail

git diff --check

echo "+++ build (matches CI Build Gate (Linux))"
nix build --quiet .#cardano-wallet .#unit-cardano-wallet-unit

echo "+++ focused unit tests (RndState / checkpoint roundtrip, matches CI wallet-unit/DB shard style)"
nix run --quiet .#unit-cardano-wallet-unit -- \
  --fail-on=empty --match "RndState" -j1 +RTS -M2G -s -RTS

echo "+++ format (fourmolu, touched files only — the repo-wide just check-fmt"
echo "    recipe ends in 'git diff --exit-code', which fails on ANY uncommitted"
echo "    diff, not just formatting drift, so it's unusable pre-commit)"
nix develop --quiet -c fourmolu --mode check \
  lib/wallet/src/Cardano/Wallet/DB/Store/Checkpoints/Store.hs \
  lib/unit/test/unit/Cardano/Wallet/DB/Store/Checkpoints/StoreSpec.hs

echo "+++ hlint"
nix develop --quiet -c just hlint

echo "gate OK"
