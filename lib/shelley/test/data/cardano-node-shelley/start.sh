#!/usr/bin/env bash

set -euo pipefail

if type -p gdate > /dev/null; then
  gnu_date=gdate
else
  gnu_date=date
fi

systemStart=$($gnu_date --iso-8601=s --date="5 seconds")

yq ".systemStart=\"$systemStart\"" < genesis.yaml > tmp-genesis.json

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
