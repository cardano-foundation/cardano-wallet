# shellcheck shell=bash

LC_ALL := 'C.UTF-8'

default:
  @just --list

# check that the code is formatted with stylish-haskell
syntax:
  scripts/buildkite/main/check-code-format.sh

hlint:
  nix develop --command bash -c 'hlint lib'

# build wallet
build target='all':
  # shellcheck disable=SC1083
  cabal build {{target}} --enable-benchmarks --enable-tests \
    --minimize-conflict-set -O0 -v0 --ghc-options="-Werror "


run target +args='':
    just build "{{target}}"
    # shellcheck disable=SC1083
    cabal run "{{target}}" \
        --minimize-conflict-set \
        --enable-benchmarks \
        --enable-tests \
        -O0 -v0 --ghc-options="-Werror " {{args}}

# build after clean
clean-build:
  cabal clean
  just build

# run a nix shell with `cardano-wallet` in scope
wallet:
  nix shell '.#cardano-wallet'

# run a benchmark: api | latency | memory | db | restore
bench target:
  ./.buildkite/bench-"{{target}}".sh

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
    cardano-wallet-application-tls:unit \
    cardano-balance-tx:test \
    cardano-numeric:unit \
    cardano-wallet-blackbox-benchmarks:unit \
    cardano-wallet-launcher:unit \
    cardano-wallet-network-layer:unit \
    cardano-wallet-primitive:test \
    cardano-wallet-secrets:test \
    cardano-wallet-test-utils:unit \
    cardano-wallet-unit:unit \
    delta-chain:unit \
    delta-store:unit \
    delta-table:unit \
    delta-types:unit \
    std-gen-seed:unit \
    wai-middleware-logging:unit \
    -O0 -v0 \
    --test-options '--match="{{match}}"'

unit-tests-local-cluster-match match:
    nix shell '.#local-cluster' 'nixpkgs#just' \
        -c just unit-tests-cabal-match "{{match}}"

# run unit tests
unit-tests-cabal:
    just unit-tests-cabal-match ""

# run cardano-wallet-integration:e2e suite against the preprod network
e2e:
  nix shell '.#cardano-node' '.#cardano-wallet' '.#e2e' nixpkgs#gnutar nixpkgs#p7zip -c e2e

add_missing_json_goldens:
    CREATE_MISSING_GOLDEN=1 just unit-tests-cabal-match "JSON"

# run any integration test matching the given pattern via cabal
integration-tests-cabal-match match:
    just integration-tests-cabal-options '--match="{{match}}"'

# with seed for reproducibility
integration-tests-cabal-match-seed match seed:
    just integration-tests-cabal-options '--match "{{match}}" --seed="{{seed}}"'

# run any integration test matching the given pattern via cabal
integration-tests-cabal-options options:
  TESTS_TRACING_MIN_SEVERITY=Warning \
  LOCAL_CLUSTER_NODE_OUTPUT_FILE=/dev/null \
  LOCAL_CLUSTER_CONFIGS=../../lib/local-cluster/test/data/cluster-configs \
  CARDANO_WALLET_TEST_DATA=../../lib/integration/test/data \
  nix shell \
    '.#local-cluster' \
    -c cabal test integration -O0 -v0 \
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
integration-tests j match:
  CARDANO_WALLET_TEST_DATA=lib/integration/test/data \
  TESTS_RETRY_FAILED=1 \
  nix shell \
    '.#cardano-wallet' \
    '.#local-cluster' \
    '.#integration-exe' \
    -c integration-exe -j "{{j}}" --match="{{match}}"

node:
  nix shell \
  --accept-flake-config \
  'github:IntersectMBO/cardano-node?ref=10.5.3#cardano-node' \
  'github:IntersectMBO/cardano-node?ref=10.5.3#cardano-cli'


# run conway integration tests matching the given pattern via nix
conway-integration-tests-match j match:
  LOCAL_CLUSTER_CONFIGS=lib/local-cluster/test/data/cluster-configs \
  LOCAL_CLUSTER_ERA=conway \
  nix shell \
    '.#cardano-node' \
    '.#cardano-cli' \
    --accept-flake-config \
    -c just integration-tests "{{j}}" "{{match}}"


# run conway integration tests via nix
conway-integration-tests j:
  just conway-integration-tests-match "{{j}}" ""

latency-bench:
   BENCHMARK_CSV_FILE=ignore-me/latency-bench.csv \
   cabal run -O0 -v0 cardano-wallet-benchmarks:latency -- \
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

api-bench:
    BENCHMARK_CSV_FILE=ignore-me/api-bench.csv \
    cabal run -O0 -v0 \
            cardano-wallet-benchmarks:api \
            -- lib/benchmarks/data/api-bench

db-bench:
    BENCHMARK_CSV_FILE=ignore-me/db-bench.csv \
    cabal run -O0 -v0 \
            cardano-wallet-benchmarks:db \

read-blocks-bench:
    BENCHMARK_CSV_FILE=ignore-me/read-blocks-bench.csv \
    cabal run -O0 -v0 \
            cardano-wallet-benchmarks:read-blocks

memory-bench:
    mkdir -p ignore-me/memory-bench
    BENCHMARK_CSV_FILE=ignore-me/memory-bench.csv \
        nix shell \
            '.#cardano-node' \
            '.#cardano-wallet' \
            'nixpkgs#jq' \
            'nixpkgs#curl' \
            'nixpkgs#procps' \
            -c cabal run -O0 -v0 \
                cardano-wallet-blackbox-benchmarks:memory -- \
                    --snapshot lib/wallet-benchmarks/data/membench-snapshot.tgz \
                    --wallet cardano-wallet \
                    --node cardano-node \
                    --work-dir ignore-me/memory-bench

ruby-e2e-linux:
    # export FIXTURE_DECRYPTION_KEY=....
    CARDANO_NODE_CONFIGS=configs/cardano/ \
    AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator \
    GENESIS_VERIFICATION_KEY=5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d \
    RUBY_TEST_DIR=test/e2e \
    PLATFORM=linux \
    nix develop ./test/e2e -c./scripts/buildkite/main/ruby-e2e.sh

ruby-e2e-macos:
    # export FIXTURE_DECRYPTION_KEY=....
    CARDANO_NODE_CONFIGS=configs/cardano/ \
    AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator \
    GENESIS_VERIFICATION_KEY=5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d \
    RUBY_TEST_DIR=test/e2e \
    PLATFORM=macos-silicon \
    nix develop --system x86_64-darwin ./test/e2e -c ./scripts/buildkite/main/ruby-e2e.sh
