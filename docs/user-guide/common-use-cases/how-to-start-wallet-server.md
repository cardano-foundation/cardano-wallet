

---
order: 1
title: How to start wallet server
---

## Overview
The easiest and most common way of managing your funds on the Cardano blockchain is through a wallet. This guide is to show how to start `cardano-wallet` together with `cardano-node`.

## Full node mode

Here we are going to start `cardano-wallet` in full node mode, meaning that we need to have also `cardano-node` running on the same machine. We can get binaries of `cardano-wallet` and compatible version of `cardano-node` from [cardano wallet release page](https://github.com/input-output-hk/cardano-wallet/releases). `Cardano-wallet` archives published for each release, besides `cardano-wallet` itself, include all the relevant tools like `cardano-node`, `cardano-cli`, `cardano-addresses` or `bech32`.

> :information_source: Alternatively one can use handy [docker-compose](https://github.com/input-output-hk/cardano-wallet#getting-started) to start wallet and the node.
> `$ NETWORK=testnet docker-compose up`

In order to start `cardano-wallet` and `cardano-node` in full mode using downloaded binaries:
1. Get up-to-date configuration files from https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1. We are going to use `testnet` configuration in this example.
2. Start node:
```bash
$ cardano-node run \
  --config testnet-config.json \
  --topology testnet-topology.json \
  --database-path ./db \
  --socket-path /path/to/node.socket
```
3. Start wallet:
```bash
$ cardano-wallet serve --port 8090 \
  --node-socket /path/to/node.socket \
  --testnet testnet-byron-genesis.json \
  --database ./wallet-db \
  --token-metadata-server https://metadata.cardano-testnet.iohkdev.io/
```
That's it! We can basically start managing our wallets from this point. See [[how-to-create-a-wallet]] and [[how-to-manage-wallets]].

However, in order to be able to make transactions, we still need to wait until `cardano-node` syncs with the Cardano blockchain. In case of `mainnet` it may take several hours, a bit less in case of `testnet`.

We can monitor this process using `cardano-wallet's` [`GET /network/information`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getNetworkInformation) endpoint. Once the endpoint returns `sync_progress` status `ready` we'll know we are good to go:

```
$ curl -X GET http://localhost:8090/v2/network/information | jq .sync_progress
{
  "status": "ready"
}
```

## Lightwallet mode

_Soon._
