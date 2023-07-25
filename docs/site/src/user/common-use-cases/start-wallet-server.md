# Start wallet server

## Overview

The easiest and most common way of managing your funds on the Cardano blockchain is through a wallet. This guide is to show how to start `cardano-wallet` together with `cardano-node` .

## Full node mode

Here we are going to start `cardano-wallet` in full node mode, meaning that we need to have also `cardano-node` running on the same machine. We can get binaries of `cardano-wallet` and compatible version of `cardano-node` from [cardano wallet release page](https://github.com/cardano-foundation/cardano-wallet/releases). `Cardano-wallet` archives published for each release, besides `cardano-wallet` itself, include all the relevant tools like `cardano-node` , `cardano-cli` , `cardano-addresses` or `bech32` .

```admonish note
Alternatively one can use handy [docker-compose](https://github.com/cardano-foundation/cardano-wallet#getting-started) to start wallet and the node on different networks:

 `> NETWORK=mainnet docker-compose up`

 `> NETWORK=preprod docker-compose up`

 `> NETWORK=preview docker-compose up`
```

#### Pre-requisites

* Install cardano-wallet from [cardano wallet release page](https://github.com/cardano-foundation/cardano-wallet/releases).
* Install cardano-node from [cardano wallet release page](https://github.com/cardano-foundation/cardano-wallet/releases).
* Download up-to-date configuration files from [Cardano Book](https://book.world.dev.cardano.org/environments.html).

#### Start `cardano-wallet` in full node mode

```admonish info
Configuration files for all Cardano networks can be found in [Cardano Book](https://book.world.dev.cardano.org/environments.html).
```

1. Start node:

```bash
> cardano-node run \
  --config config.json \
  --topology topology.json \
  --database-path ./db \
  --socket-path /path/to/node.socket
```

2. Start wallet:

When starting a wallet instance that targets a testing environment such as `preview` or `preprod` , we need to provide a `byron-genesis.json` file to the wallet:

```bash
> cardano-wallet serve --port 8090 \
  --node-socket /path/to/node.socket \
  --testnet byron-genesis.json \
  --database ./wallet-db \
  --token-metadata-server https://metadata.cardano-testnet.iohkdev.io
```

In case of `mainnet` we simply replace `--testnet byron-genesis.json` with option `--mainnet` .

```bash
> cardano-wallet serve --port 8090 \
  --node-socket /path/to/node.socket \
  --mainnet \
  --database ./wallet-db \
  --token-metadata-server https://tokens.cardano.org
```

```admonish note
We use different URLs for mainnet and test networks with the `--token-metadata-server` option. These URLs point to [Cardano Token Registry](https://developers.cardano.org/docs/native-tokens/token-registry/cardano-token-registry) servers. See
[assets](assets.md)
for more information.
```

That's it! We can basically start managing our wallets from this point onwards. See
[how-to-create-a-wallet](../common-use-cases/create-a-wallet.md)
and [how-to-manage-wallets](../common-use-cases/how-to-manage-wallets.md)

However, in order to be able to make transactions, we still need to wait until `cardano-node` is synced fully with the Cardano blockchain. In case of `mainnet` it may take several hours, in case of `testnet` a bit less.

We can monitor this process using `cardano-wallet's` [ `GET /network/information` ](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getNetworkInformation) endpoint. Once the endpoint returns `sync_progress` status `ready` we'll know we are good to go:

```
> curl -X GET http://localhost:8090/v2/network/information | jq .sync_progress
{
  "status": "ready"
}
```
