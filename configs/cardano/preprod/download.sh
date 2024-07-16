#! /bin/bash

set -euo pipefail

rm ./*.json
curl https://book.play.dev.cardano.org/environments-pre/preprod/config.json > config.json
curl https://book.play.dev.cardano.org/environments-pre/preprod/conway-genesis.json > conway-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/preprod/topology.json > topology.json
curl https://book.play.dev.cardano.org/environments-pre/preprod/byron-genesis.json > byron-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/preprod/shelley-genesis.json > shelley-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/preprod/alonzo-genesis.json > alonzo-genesis.json
