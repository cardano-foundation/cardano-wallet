#! /bin/bash

set -euo pipefail

wget https://book.world.dev.cardano.org/environments/sanchonet/config.json
wget https://book.world.dev.cardano.org/environments/sanchonet/topology.json
wget https://book.world.dev.cardano.org/environments/sanchonet/byron-genesis.json
wget https://book.world.dev.cardano.org/environments/sanchonet/shelley-genesis.json
wget https://book.world.dev.cardano.org/environments/sanchonet/alonzo-genesis.json
wget https://book.world.dev.cardano.org/environments/sanchonet/conway-genesis.json