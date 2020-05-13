#!/usr/bin/env bash

cardano-cli shelley genesis key-gen-genesis --verification-key-file genesis.vkey --signing-key-file genesis.skey
cardano-cli shelley genesis key-gen-delegate --verification-key-file deleg.vkey --signing-key-file deleg.skey --operational-certificate-issue-counter deleg.counter
GENESIS_HASH=$(cardano-cli shelley genesis key-hash --verification-key-file genesis.vkey)
DELEG_HASH=$(cardano-cli shelley genesis key-hash --verification-key-file deleg.vkey)

cardano-cli shelley node key-gen-VRF \
  --verification-key-file node-vrf.vkey \
  --signing-key-file node-vrf.skey

cardano-cli shelley node key-gen-KES \
  --verification-key-file node-kes.vkey \
  --signing-key-file node-kes.skey

cardano-cli shelley node issue-op-cert \
  --hot-kes-verification-key-file node-kes.vkey \
  --cold-signing-key-file deleg.skey \
  --operational-certificate-issue-counter deleg.counter \
  --kes-period 0 \
  --out-file node.opcert


echo "To be added to genesis config:"
echo "${GENESIS_HASH}": "${DELEG_HASH}"
