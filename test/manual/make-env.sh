#!/usr/bin/env bash
#
# The following script creates the environment needed to run the manual tests.

if [[ -z "${WALLET_VERSION}" ]]; then
    echo "WALLET_VERSION is not set!"
    exit 1
fi

if [[ -z "${NODE_VERSION}" ]]; then
    echo "NODE_VERSION is not set!"
    exit 1
fi

set -euxo pipefail

pushd $(mktemp -d)

# Download source repositories
curl -L -o cardano-wallet.tar.gz "https://github.com/cardano-foundation/cardano-wallet/archive/refs/tags/v${WALLET_VERSION}.tar.gz"
tar -xzf cardano-wallet.tar.gz

curl -L -o cardano-node.tar.gz "https://github.com/input-output-hk/cardano-node/archive/refs/tags/${NODE_VERSION}.tar.gz"
tar -xzf cardano-node.tar.gz

WALLET_DIR=cardano-wallet-${WALLET_VERSION}
NODE_DIR=cardano-node-${NODE_VERSION}

# Build binaries
nix-build $NODE_DIR -A cardano-node -o cardano-node
nix-build $WALLET_DIR -A cardano-wallet -o cardano-wallet

# Get latest configs
curl -L -o testnet-config.json https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/testnet-config.json
curl -L -o testnet-topology.json https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/testnet-topology.json
curl -L -o testnet-byron-genesis.json https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/testnet-byron-genesis.json
curl -L -o testnet-shelley-genesis.json https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/testnet-shelley-genesis.json
curl -L -o testnet-alonzo-genesis.json https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/testnet-alonzo-genesis.json

# Restore node database
curl -L -o db-testnet.tar.gz https://updates-cardano-testnet.s3.amazonaws.com/cardano-node-state/db-testnet.tar.gz
tar -xzf db-testnet.tar.gz

mkdir wallet-db-testnet

exec bash
