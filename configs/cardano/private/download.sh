#!/usr/bin/env bash

set -euo pipefail

rm ./*.json
wget https://book.play.dev.cardano.org/environments/private/config.json
wget https://book.play.dev.cardano.org/environments/private/topology.json
wget https://book.play.dev.cardano.org/environments/private/byron-genesis.json
wget https://book.play.dev.cardano.org/environments/private/shelley-genesis.json
wget https://book.play.dev.cardano.org/environments/private/alonzo-genesis.json
wget https://book.play.dev.cardano.org/environments/private/conway-genesis.json
