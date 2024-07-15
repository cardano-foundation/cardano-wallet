#! /bin/bash

set -euo pipefail

curl https://book.play.dev.cardano.org/environments-pre/mainnet/config.json > config.json
curl https://book.play.dev.cardano.org/environments-pre/mainnet/conway-genesis.json  > conway-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/mainnet/topology.json  > topology.json
curl https://book.play.dev.cardano.org/environments-pre/mainnet/byron-genesis.json  > byron-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/mainnet/shelley-genesis.json  > shelley-genesis.json
curl https://book.play.dev.cardano.org/environments-pre/mainnet/alonzo-genesis.json  > alonzo-genesis.json
