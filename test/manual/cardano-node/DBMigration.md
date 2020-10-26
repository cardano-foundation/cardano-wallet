# DB Migration from previously released version

## OS

Windows, MacOS, Linux

**strongly recommended to test on all platforms**

## Start previous version of the wallet

1. Get **previous** release -> https://github.com/input-output-hk/cardano-wallet/releases
2. Start cardano-node and cardano-wallet on `testnet` using latest [config](https://hydra.iohk.io/build/4547830/download/1/index.html). Make sure both are fully synced.

```bash
$ cardano-node run \
		--config ./*-config.json \
		--topology ./*-topology.json \
		--database-path ./db \
		--socket-path ./node.socket

$ cardano-wallet serve --port 8090 \
		--node-socket ../relay1/node.socket \
		--testnet testnet-byron-genesis.json  \
		--database ./wallet-db
```

3. Produce some data in the cardano-wallet DB e.g.
 - add shelley wallet
 - add byron wallet
 - send tx
 - delegate to a stake-pool

## Start current version of the wallet

1. Get **current** release -> https://github.com/input-output-hk/cardano-wallet/releases
2. start wallet (on the same `--database`)

```bash
$ cardano-wallet serve --port 8090 \
		--node-socket ../relay1/node.socket \
		--testnet testnet-byron-genesis.json  \
		--database ./wallet-db
```

3. Check basic functionality:
 - add shelley wallet
 - add byron wallet
 - make sure sync progress and block height are tracked on both syncing and synced wallets
 - send tx
 - delegate to a stake-pool
 - network information

 4. Wallet starts and functions properly.
