#!/usr/bin/env bash
set -euo pipefail
git diff --check
# Matches CI's "wallet-unit / *" jobs exactly (.github/workflows/ci.yml)
# rather than `just unit-tests-*`/raw `cabal test`: the latter needs a
# from-scratch cabal-install TUF bootstrap of the CHaP secure repo, which
# the nix-substituted local mirror can't satisfy (no 00-index.tar.gz).
# `nix run` resolves the build plan via haskell.nix at eval time instead,
# bypassing cabal-install's runtime index entirely.
(cd lib/unit && nix run --quiet ../../.#unit-cardano-wallet-unit -- \
  --match "/Cardano.Wallet.Api.Types" \
  --match "/Cardano.WalletSpec" \
  --match "/LayerSpec" \
  --fail-on=empty -j1 +RTS -M2G -s -RTS)
nix develop --quiet -c just check-fmt
nix develop --quiet -c just hlint
