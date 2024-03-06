#! /bin/bash

set -euo pipefail

curl https://book.play.dev.cardano.org/environments/preprod/config.json \
    > config.json
curl https://book.play.dev.cardano.org/environments/preprod/conway-genesis.json \
    > conway-genesis.json
curl https://book.play.dev.cardano.org/environments/preprod/topology.json \
    > topology.json
curl https://book.play.dev.cardano.org/environments/preprod/byron-genesis.json \
    > byron-genesis.json
curl https://book.play.dev.cardano.org/environments/preprod/shelley-genesis.json \
    > shelley-genesis.json
curl https://book.play.dev.cardano.org/environments/preprod/alonzo-genesis.json \
    > alonzo-genesis.json
