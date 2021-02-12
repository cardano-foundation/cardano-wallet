# Tests for syncing in the Byron era

## OS

Windows, MacOS, Linux

## Restart syncing in Byron era.

1. Start cardano-node and cardano-wallet on `testnet` using latest [config](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html). **Start syncing from scratch.**

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

2. Note the `sync_progress` while wallet and node are syncing through Byron era.

```bash
$ cardano-wallet network information | jq .sync_progress

Ok.
{
  "status": "syncing",
  "progress": {
    "quantity": 49.95,
    "unit": "percent"
  }
}
```

3. Stop the cardano-wallet, but **keep cardano-node running**.


4. Start cardano-wallet again and make sure that `sync_progress` restarts from the place it was stopped at and moves forward.

```bash
$ cardano-wallet network information | jq .sync_progress

Ok.
{
  "status": "syncing",
  "progress": {
    "quantity": 49.95,
    "unit": "percent"
  }
}
```

## Listing stake pools


6. List stake pools while the wallet is syncing through Byron era and make sure appropriate error message is presented.

```bash
$ curl -X GET 'http://localhost:8090/v2/stake-pools?stake=1000000000'

{
  "code": "past_horizon",
  "message": "Tried to convert something that is past the horizon (due to uncertainty about the next hard fork). Wait for the node to finish syncing to the hard fork. Depending on the blockchain, this process can take an unknown amount of time."
}
```
