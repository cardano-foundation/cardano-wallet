# Manual Tests

The `./make-env.sh` script in this directory will make the environment necessary to run the tests listed here.

Call it with the appropriate `WALLET_VERSION` and `NODE_VERSION` environment variables set:

```
WALLET_VERSION=2022-04-27 NODE_VERSION=1.34.1 ./make-env.sh
```

Binaries:

  - cardano-node: `./cardano-node/bin/cardano-node`
  - cardano-wallet: `./cardano-wallet/bin/cardano-wallet`

Directories:

  - Use `db-testnet` as the database directory for `cardano-node`.
  - Use `wallet-db-testnet` as the database directory for `cardano-wallet`.

E.g.:

```
cardano-node/bin/cardano-node run \
    --config ./testnet-config.json \
    --topology ./testnet-topology.json \
    --database-path ./db-testnet \
    --socket-path ./node.socket

cardano-wallet/bin/cardano-wallet serve --port 8090 \
    --node-socket ./node.socket \
    --testnet ./testnet-byron-genesis.json  \
    --database ./wallet-db-testnet
```

Type `exit` to leave the test environment.
