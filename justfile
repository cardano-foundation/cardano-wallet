default:
  @just --list

# check that the code is formatted with stylish-haskell
check target="stylish":
  ./.buildkite/check-{{target}}.sh

# build wallet-e2e suite with cabal
build:
  cabal build all

# run a nix shell with `cardano-wallet` in scope
shell:
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
    -s lib/wallet-e2e/test-state \
    -c lib/wallet-e2e/config/cardano-node/preprod

# run wallet-e2e suite against the local test cluster
e2e-local:
  nix shell \
    '.#local-cluster' '.#cardano-node' '.#cardano-wallet' '.#cardano-wallet-e2e' '.#local-cluster' \
    -c wallet-e2e local -s lib/wallet-e2e/test-state -c lib/local-cluster/test/data/cluster-configs

# run wallet-e2e suite against the manually started node/wallet
e2e-manual:
  nix run '.#cardano-wallet-e2e' -- manual
