#!/usr/bin/env bash

set -euo pipefail

rm ./*.json
wget https://book.play.dev.cardano.org/environments-pre/sanchonet/config.json
wget https://book.play.dev.cardano.org/environments-pre/sanchonet/topology.json
wget https://book.play.dev.cardano.org/environments-pre/sanchonet/byron-genesis.json
wget https://book.play.dev.cardano.org/environments-pre/sanchonet/shelley-genesis.json
wget https://book.play.dev.cardano.org/environments-pre/sanchonet/alonzo-genesis.json
wget https://book.play.dev.cardano.org/environments-pre/sanchonet/conway-genesis.json
