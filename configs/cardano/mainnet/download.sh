#! /bin/bash

set -euo pipefail

curl https://book.play.dev.cardano.org/environments/mainnet/config.json \
    > config.json
curl https://book.play.dev.cardano.org/environments/mainnet/db-sync-config.json \
    > db-sync-config.json
curl https://book.play.dev.cardano.org/environments/mainnet/genesis.json \
    > genesis.json
curl https://book.play.dev.cardano.org/environments/mainnet/submit-api-config.json \
    > submit-api-config.json
curl https://book.play.dev.cardano.org/environments/mainnet/topology.json \
    > topology.json
curl https://book.play.dev.cardano.org/environments/mainnet/byron-genesis.json \
    > byron-genesis.json
curl https://book.play.dev.cardano.org/environments/mainnet/shelley-genesis.json \
    > shelley-genesis.json
curl https://book.play.dev.cardano.org/environments/mainnet/alonzo-genesis.json \
    > alonzo-genesis.json
