default:
  @just --list

# check that the code is formatted with stylish-haskell
syntax:
  ./.buildkite/check-code-format.sh

# build wallet-e2e suite with cabal
build:
  cabal build all

# run a nix shell with `cardano-wallet` in scope
wallet:
  nix shell '.#cardano-wallet'

# run a benchmark: api | latency | memory | db | restore
bench target:
  ./.buildkite/bench-{{target}}.sh

# run a local test cluster
local-cluster:
  nix shell '.#local-cluster' '.#cardano-node' '.#cardano-wallet' \
    -c "local-cluster" \
    --cluster-configs lib/local-cluster/test/data/cluster-configs

# run unit tests
unit:
  cabal run cardano-wallet:test:unit \
    --test-options '--cluster-configs lib/local-cluster/test/data/cluster-configs'

# run integration tests
integration:
  LOCAL_CLUSTER_CONFIGS=lib/local-cluster/test/data/cluster-configs \
  nix shell '.#cardano-wallet' -c cabal test integration

# run wallet-e2e suite against the preprod network
e2e-preprod:
  nix run '.#cardano-wallet-e2e' -- preprod \
    -s lib/wallet-e2e/test-state/preprod \
    -c lib/wallet-e2e/config/cardano-node/preprod \
    -t lib/wallet-e2e/test-output/preprod


# run wallet-e2e suite against the local test cluster
e2e-local:
  nix shell \
    '.#local-cluster' '.#cardano-node' '.#cardano-wallet' '.#cardano-wallet-e2e' \
    -c wallet-e2e local \
    -s lib/wallet-e2e/test-state/local \
    -c lib/local-cluster/test/data/cluster-configs \
    -t lib/wallet-e2e/test-output/local

# run wallet-e2e suite against the manually started node/wallet
e2e-manual:
  nix run '.#cardano-wallet-e2e' -- manual

#run integration tests locally via babbage-integration-exe
babbage-integration-tests:
  LOCAL_CLUSTER_CONFIGS=lib/local-cluster/test/data/cluster-configs \
  CARDANO_WALLET_TEST_DATA=./lib/wallet/test/data \
  LOCAL_CLUSTER_ERA=babbage \
  TESTS_RETRY_FAILED=1 \
  nix shell \
    '.#cardano-node' \
    '.#cardano-cli' \
    '.#cardano-wallet' \
    '.#integration-exe' \
    -c integration-exe -j 3

# run integration tests locally via conway-integration-exe
conway-integration-tests:
  LOCAL_CLUSTER_CONFIGS=lib/local-cluster/test/data/cluster-configs \
  CARDANO_WALLET_TEST_DATA=./lib/wallet/test/data \
  LOCAL_CLUSTER_ERA=conway \
  TESTS_RETRY_FAILED=1 \
  nix shell \
    '.#local-cluster' \
    '.#cardano-node' \
    '.#cardano-cli' \
    '.#cardano-wallet' \
    '.#integration-exe' \
    -c integration-exe -j 3

hlint:
  nix develop --command bash -c 'hlint lib'