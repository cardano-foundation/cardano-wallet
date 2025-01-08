#!/usr/bin/env bash

set -euo pipefail

rm ./*.json
curl https://book.play.dev.cardano.org/environments/preview/config.json > config.json
curl https://book.play.dev.cardano.org/environments/preview/conway-genesis.json  > conway-genesis.json
curl https://book.play.dev.cardano.org/environments/preview/topology.json  > topology.json
curl https://book.play.dev.cardano.org/environments/preview/byron-genesis.json  > byron-genesis.json
curl https://book.play.dev.cardano.org/environments/preview/shelley-genesis.json  > shelley-genesis.json
curl https://book.play.dev.cardano.org/environments/preview/alonzo-genesis.json  > alonzo-genesis.json
