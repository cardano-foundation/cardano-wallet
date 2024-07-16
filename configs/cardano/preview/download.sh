#! /bin/bash

set -euo pipefail

rm ./*.json
curl https://book.play.dev.cardano.org/environments-pre/preview/config.json > config.json
curl https://book.play.dev.cardano.org/environments-pre/preview/conway-genesis.json  > conway-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/preview/topology.json  > topology.json
curl https://book.play.dev.cardano.org/environments-pre/preview/byron-genesis.json  > byron-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/preview/shelley-genesis.json  > shelley-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/preview/alonzo-genesis.json  > alonzo-genesis.json
