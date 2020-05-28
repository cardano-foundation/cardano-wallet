#!/usr/bin/env bash

startTime=$(gdate --iso-8601=s --date="5 seconds")

cat genesis.yaml | yq ".startTime=\"$startTime\"" > tmp-genesis.json

cat node.config \
  | yq -y '.GenesisFile="tmp-genesis.json"' \
  | yq -y '.minSeverity="Info"' \
  > tmp-node.config

cardano-node run \
     --config tmp-node.config \
     --topology node.topology \
     --database-path node.db \
     --socket-path node.socket \
     --port 50068 \
     --shelley-vrf-key node-vrf.skey \
     --shelley-kes-key node-kes.skey \
     --shelley-operational-certificate node.opcert
