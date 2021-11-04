
# Test cardano-wallet recovers after losing conection with cardano-node

## OS

Windows, MacOS, Linux

## Test

1. Start cardano-node and cardano-wallet on `testnet` using latest [config](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html) in two separate terminals. Make sure both are fully synced.

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

2. Make sure there are some wallets created and that they are synced. E.g.
```bash
$ curl -X GET http://localhost:8090/v2/wallets | jq length
3

$ curl -X GET http://localhost:8090/v2/wallets | jq '.[].state'
{
  "status": "ready"
}
{
  "status": "ready"
}
{
  "status": "ready"
}
```

3. Stop the cardano-node, but **keep cardano-wallet running**.
4. Check in wallet logs that it is trying to reconnect to the node.
```bash
Couldn't connect to node (x5). Retrying in a bit...
```

5. Make sure you still can see all your wallets.

```bash
$ curl -X GET http://localhost:8090/v2/wallets | jq length
3

$ curl -X GET http://localhost:8090/v2/wallets | jq '.[].state'
{
  "status": "ready"
}
{
  "status": "ready"
}
{
  "status": "ready"
}
```


5. Start cardano-node again.
6. Make sure cardano-wallet re-connected to the node. No more `Couldn't connect to node (x5). Retrying in a bit...` entries appear in the wallet log.
7. Make sure you still can see all your wallets.

```bash
$ curl -X GET http://localhost:8090/v2/wallets | jq length
3

$ curl -X GET http://localhost:8090/v2/wallets | jq '.[].state'
{
  "status": "ready"
}
{
  "status": "ready"
}
{
  "status": "ready"
}
```
