#!/usr/bin/env bash

# https://cardano-course.gitbook.io/cardano-course/handbook/setting-up-a-local-cluster/create-a-local-cluster

set -exuo pipefail

mkdir -p template

# wget https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/testnet-template/alonzo.json -O template/alonzo.json
# wget https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/testnet-template/byron.json -O template/byron.json
# wget https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/testnet-template/config.json -O template/config.json
# wget https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/testnet-template/shelley.json -O template/shelley.json
# wget https://raw.githubusercontent.com/input-output-hk/iohk-nix/master/cardano-lib/testnet-template/conway.json -O template/conway.json

cardano-cli genesis create-cardano \
  --genesis-dir . \
  --testnet-magic 42 \
  --gen-genesis-keys 1 \
  --gen-utxo-keys 1 \
  --supply 45000000000000000 \
  --byron-template template/byron.json \
  --shelley-template template/shelley.json \
  --alonzo-template template/alonzo.json \
  --conway-template template/conway.json \
  --node-config-template template/config.json
