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

#### Pre-requisites
- Install cardano-wallet from [cardano wallet release page](https://github.com/input-output-hk/cardano-wallet/releases).
- Install cardano-node from [cardano wallet release page](https://github.com/input-output-hk/cardano-wallet/releases).
- Download up-to-date configuration files from [Cardano configurations](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1).

#### Start `cardano-wallet` in full node mode
We are going to use `testnet` configuration in this example.

1. Start node:
```bash
$ cardano-node run \
  --config testnet-config.json \
  --topology testnet-topology.json \
  --database-path ./db \
  --socket-path /path/to/node.socket
```
2. Start wallet:
```bash
$ cardano-wallet serve --port 8090 \
  --node-socket /path/to/node.socket \
  --testnet testnet-byron-genesis.json \
  --database ./wallet-db \
  --token-metadata-server https://metadata.cardano-testnet.iohkdev.io/
```
That's it! We can basically start managing our wallets from this point onwards. See [[how-to-create-a-wallet]] and [[how-to-manage-wallets]].

However, in order to be able to make transactions, we still need to wait until `cardano-node` is synced fully with the Cardano blockchain. In case of `mainnet` it may take several hours, in case of `testnet` a bit less.

We can monitor this process using `cardano-wallet's` [`GET /network/information`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getNetworkInformation) endpoint. Once the endpoint returns `sync_progress` status `ready` we'll know we are good to go:

```
$ curl -X GET http://localhost:8090/v2/network/information | jq .sync_progress
{
  "status": "ready"
}
```

## Light mode

> :warning: This mode is currently **under development**. Please note that some parts may be available only in `master` branch.

You can start your cardano-wallet server also in **light mode**. As opposed to full-node mode, in light your wallet is not connected to locally running instance of `cardano-node`. Instead it relays on external source of blockchain data. This significantly improves synchronization speed of the wallet and also you don't need to spend time for synchronizing the node itself. The downside is that external source of data is obviously less trusted than the local one provided by your own `cardano-node` instance.

> :information_source: Cardano-wallet currently supports only one external data provider - [Blockfrost](https://blockfrost.io/). Before using the light mode one needs to generate API key on Blockfrost page and save it into the file on the filesystem.

#### Pre-requisites
- Install cardano-wallet from [cardano wallet release page](https://github.com/input-output-hk/cardano-wallet/releases).
- Download up-to-date configuration files from [Cardano configurations](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1).

#### Start `cardano-wallet` in light mode

Let's suppose we have our API keys generated and saved in files as follows:
 - `blockfrost-testnet.key` - API key for `testnet`
 - `blockfrost-mainnet.key` - API key for `mainnet`

Now we can start cardano-wallet server for testnet or mainnet:
##### Testnet
```
$ cardano-wallet serve \
	--port 8091 \
	--light \
	--blockfrost-token-file blockfrost-testnet.key \
	--testnet testnet-byron-genesis.json \
	--database ./wallet-testnet-db-light \
```
##### Mainnet
```
$ cardano-wallet serve \
	--port 8092 \
	--light \
	--blockfrost-token-file blockfrost-mainnet.key \
	--mainnet \
	--database ./wallet-db-light \
```
