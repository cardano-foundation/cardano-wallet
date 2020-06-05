#!/usr/bin/env bash

# This loosely follows the instructions at:
#   https://github.com/input-output-hk/cardano-node/blob/master/doc/shelley-genesis.md

set -euo pipefail

cardano-cli shelley genesis create \
  --genesis-dir . \
  --mainnet \
  --gen-genesis-keys 1 \
  --gen-utxo-keys 1 \
  --supply 45000000000000000

mkdir -p node1

cardano-cli shelley node key-gen-KES \
    --verification-key-file node-kes.vkey \
    --signing-key-file node-kes.skey

cardano-cli shelley node key-gen-VRF \
  --verification-key-file node-vrf.vkey \
  --signing-key-file node-vrf.skey

cardano-cli shelley node issue-op-cert \
  --hot-kes-verification-key-file node-kes.vkey \
  --cold-signing-key-file delegate-keys/delegate1.skey \
  --operational-certificate-issue-counter delegate-keys/delegate1.counter \
  --kes-period 0 \
  --out-file node.opcert

echo "To be added to the genDelegs section of genesis.yaml:"
yq --yaml-output '.genDelegs|{genDelegs: .}' < genesis.json
