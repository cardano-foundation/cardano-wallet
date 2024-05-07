LC_ALL := 'C.UTF-8'

default:
  @just --list

# check that the code is formatted with stylish-haskell
syntax:
  ./.buildkite/check-code-format.sh

hlint:
  nix develop --command bash -c 'hlint lib'

# build wallet
build:
  cabal build all  --enable-benchmarks --enable-tests --minimize-conflict-set -O0 -v0 --ghc-options="-Werror "

# build after clean
clean-build:
  cabal clean
  just build

# run a nix shell with `cardano-wallet` in scope
wallet:
  nix shell '.#cardano-wallet'

# run a benchmark: api | latency | memory | db | restore
bench target:
  ./.buildkite/bench-{{target}}.sh

# run a local test cluster
local-cluster:
  nix shell '.#local-cluster' '.#cardano-node' \
    -c "local-cluster" \
    --cluster-configs lib/local-cluster/test/data/cluster-configs \
    --cluster-logs ignore-me/cluster.logs \
    --socket-path ignore-me/cluster.socket \
    --monitoring-port 12788

# run unit tests on a match
unit-tests-cabal-match match:
  LOCAL_CLUSTER_CONFIGS=../../lib/local-cluster/test/data/cluster-configs \
  cabal test \
    local-cluster:test \
    cardano-wallet-unit:unit \
    cardano-wallet-read:test \
    -O0 -v0 \
    --test-options '--match="{{match}}"'

unit-tests-local-cluster-match match:
    nix shell '.#local-cluster' 'nixpkgs#just' \
    -c just unit-tests-cabal-match {{match}}
# run unit tests
unit-tests-cabal:
    just unit-tests-cabal-match ""

# run wallet-e2e suite against the preprod network
e2e-preprod:
  nix run '.#cardano-wallet-e2e' -- preprod \
    -s lib/wallet-e2e/test-state/preprod \
    -c lib/wallet-e2e/config/cardano-node/preprod \
    -t lib/wallet-e2e/test-output/preprod

add_missing_json_goldens:
    CREATE_MISSING_GOLDEN=1 just unit-tests-cabal-match "JSON"

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

# run any integration test matching the given pattern via cabal
integration-tests-cabal-match match:
    just integration-tests-cabal-options '--match="{{match}}"'

# run any integration test matching the given pattern via cabal
integration-tests-cabal-options options:
  TESTS_TRACING_MIN_SEVERITY=Warning \
  LOCAL_CLUSTER_NODE_OUTPUT_FILE=/dev/null \
  LOCAL_CLUSTER_CONFIGS=../../lib/local-cluster/test/data/cluster-configs \
  CARDANO_WALLET_TEST_DATA=../../lib/integration/test/data \
  cabal test integration -O0 -v0 \
    --test-options '{{options}}'

# run babbage integration tests matching the given pattern via cabal
babbage-integration-tests-cabal-match match:
  LOCAL_CLUSTER_ERA=babbage \
  just integration-tests-cabal-match "{{match}}"

# run conway integration tests matching the given pattern via cabal
conway-integration-tests-cabal-match match:
  LOCAL_CLUSTER_ERA=conway \
  just integration-tests-cabal-match "{{match}}"

# run babbage integration tests via cabal
babbage-integration-tests-cabal:
  just babbage-integration-tests-cabal-match ""

# run conway integration tests via cabal
conway-integration-tests-cabal:
  just conway-integration-tests-cabal-match ""


# run any integration test matching the given pattern via nix
integration-tests match:
  LOCAL_CLUSTER_CONFIGS=lib/local-cluster/test/data/cluster-configs \
  CARDANO_WALLET_TEST_DATA=lib/integration/test/data \
  TESTS_RETRY_FAILED=1 \
  nix shell \
    '.#cardano-node' \
    '.#cardano-cli' \
    '.#cardano-wallet' \
    '.#integration-exe' \
    -c integration-exe -j 2 --match="{{match}}"

# run babbage integration tests matching the given pattern via nix
babbage-integration-tests-match match:
  LOCAL_CLUSTER_ERA=babbage \
  just integration-tests "{{match}}"

# run conway integration tests matching the given pattern via nix
conway-integration-tests-match match:
  LOCAL_CLUSTER_ERA=conway \
  just integration-tests "{{match}}"

# run babbage integration tests via nix
babbage-integration-tests:
  just babbage-integration-tests-match ""

# run conway integration tests via nix
conway-integration-tests:
  just conway-integration-tests-match ""

latency-bench:
   cabal run -O2 -v0 cardano-wallet-benchmarks:latency -- \
   --cluster-configs lib/local-cluster/test/data/cluster-configs

test-local-cluster:
    LOCAL_CLUSTER_CONFIGS=lib/local-cluster/test/data/cluster-configs \
    nix shell \
        '.#local-cluster' \
        '.#test-local-cluster-exe' \
        '.#cardano-cli' \
        '.#cardano-node' \
        '.#cardano-wallet' \
        -c test-local-cluster-exe
