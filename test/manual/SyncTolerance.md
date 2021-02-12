# Tests for `--sync-tolerance` on cardano-wallet

## OS

Windows, MacOS, Linux

## Small --sync-tolerance

1. Start cardano-node and cardano-wallet on `testnet` using latest [config](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html). Make sure both are fully synced.

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

2. Poll network information until the `sync_progress` is marked as `"ready"`

```bash
$ cardano-wallet network information | jq .sync_progress.status

Ok.
"ready"
```

3. Stop the cardano-wallet for at least 30 seconds, but **keep cardano-node running**.


4. Restart the server with a `--sync-tolerance` **short** in front of 30 seconds

```bash
$ cardano-wallet serve --port 8090 \
		--node-socket ../relay1/node.socket \
		--testnet testnet-byron-genesis.json  \
		--database ./wallet-db \
		--sync-tolerance 1s
```


5. Quickly query the network information and check that the `sync_progress` is `"syncing"`.

```bash
$ cardano-wallet network information | jq .sync_progress.status

Ok.
"syncing"
```

## Large --sync-tolerance


6. Stop the cardano-wallet for at least 30 seconds, but **keep cardano-node running**.


7. Restart the server with a `--sync-tolerance` **large** in front of 30 seconds

```bash
$ cardano-wallet serve --port 8090 \
		--node-socket ./node.socket \
		--testnet testnet-byron-genesis.json  \
		--database ./wallet-db \
		--sync-tolerance 90s
```


8. Quickly query the network information and check that the `sync_progress` is `"ready"`.

```bash
$ cardano-wallet network information | jq .sync_progress.status

Ok.
"ready"
```
