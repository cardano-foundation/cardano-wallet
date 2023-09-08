default:
  @just --list

# build wallet-e2e suite with cabal
build:
  cabal build all

# run wallet-e2e suite against the preprod network
e2e-preprod:
  nix run '.#cardano-wallet-e2e' -- preprod \
    -s lib/wallet-e2e/test-state \
    -c lib/wallet-e2e/config/cardano-node/preprod

# run wallet-e2e suite against the local test cluster
e2e-local:
  nix run '.#cardano-wallet-e2e' -- local \
    -s lib/wallet-e2e/test-state \
    -c lib/wallet-e2e/config/cardano-node/local

# run wallet-e2e suite against the manually started node/wallet
e2e-manual:
  nix run '.#cardano-wallet-e2e' -- manual
