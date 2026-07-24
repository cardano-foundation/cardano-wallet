#!/usr/bin/env bash
set -euo pipefail
git diff --check
nix develop --quiet -c just unit-tests-local-cluster-match "TypesSpec|WalletSpec|LayerSpec"
nix develop --quiet -c just check-fmt
nix develop --quiet -c just hlint
