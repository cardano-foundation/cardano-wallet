---
order: 6
title: Managing native assets
---

## Pre-requisites

 - [[how-to-start-wallet-server]]
 - [[how-to-create-a-wallet]]
 - In order to be able to send transactions we need funds on the wallet. In case of [Testnet](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Overview

The Cardano Blockchain features a unique ability to create (mint), interact (send and receive) and destroy (burn) custom tokens (or so-called 'assets') in a native way. Native, in this case, means besides sending and receiving the official currency ada, you can also interact with custom assets out of the box - without the need for smart contracts.

> :information_source: Read more: https://developers.cardano.org/docs/native-tokens/.

> :information_source: Sending and receiving native assets is supported for all wallet types in `cardano-wallet`!

## Assets on wallet balance

You can easily check what assets you have on your wallet balance. Just look for `assets` in the response of [GET /wallets/{walletId}](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getWallet).

```
$ curl -X GET http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d | jq .assets
{
  "total": [
    {
      "asset_name": "45524358",
      "quantity": 1777134804,
      "policy_id": "36e93afe19e46227069520040012411e17839f219b549b7f5ac83b68"
    },
    {
      "asset_name": "4d494c4b5348414b4530",
      "quantity": 1,
      "policy_id": "dd043a63daf194065ca8c8f041337d8e75a08d8f6c469ddc1743d2f3"
    }
  ],
  "available": [
    {
      "asset_name": "45524358",
      "quantity": 1777134804,
      "policy_id": "36e93afe19e46227069520040012411e17839f219b549b7f5ac83b68"
    },
    {
      "asset_name": "4d494c4b5348414b4530",
      "quantity": 1,
      "policy_id": "dd043a63daf194065ca8c8f041337d8e75a08d8f6c469ddc1743d2f3"
    }
  ]
}

```

## Listing assets

You can also list assets that were ever associated with the wallet.

 - [GET /wallets/{walletId}/assets](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listAssets) - list all
 - [GET /wallets/{walletId}/assets/{policyId}](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getAssetDefault) - get particular asset details by its `policy_id`
 - [GET /wallets/{walletId}/assets/{policyId}/{assetName}](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getAsset) - get particular asset details by `policy_id` and `asset_name`

## Assets off-chain metadata

Issuers of native assets may put some metadata about them in the off-chain metadata server, like **Cardano Token Registry**.

> :information_source: Read more: [Cardano Token Registry](https://developers.cardano.org/docs/native-tokens/token-registry/cardano-token-registry), [CIP26](https://cips.cardano.org/cips/cip26).

Cardano-wallet is capable of reading that metadata and serving it along while listing assets. All you have to do is to start wallet with `--token-metadata-server` parameter pointing to an off-chain metadata server.

For example on testnet that would be:

```bash
$ cardano-wallet serve --port 8090 \
  --node-socket /path/to/node.socket \
  --testnet testnet-byron-genesis.json \
  --database ./wallet-db \
  --token-metadata-server https://metadata.cardano-testnet.iohkdev.io/
```

Then, if you list assets associated with your wallet you will get their available metadata.

For instance:

```
$ curl -X GET http://localhost:8090/v2/wallets/1f82e83772b7579fc0854bd13db6a9cce21ccd95/assets/919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149/4861707079436f696e

{
  "fingerprint": "asset1v96rhc76ke22mx8sulm4v3qhdcmtym7f4m66z2",
  "asset_name": "4861707079436f696e",
  "policy_id": "919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149",
  "metadata": {
    "url": "https://happy.io",
    "name": "HappyCoin",
    "decimals": 6,
    "ticker": "HAPP",
    "description": "Coin with asset name - and everyone is happy!!!"
  }
}
```

## Minting and burning assets

Minting and burning assets is available using `cardano-cli`. See more in https://developers.cardano.org/docs/native-tokens/minting.

This feature will be available in the `cardano-wallet` soon.

## Sending assets in a transaction

Once you have some assets on your wallet balance you can send it in a transaction.

For example, I'd like to send 1.5₳ and 15 `HappyCoins` to three different addresses in a single transaction.

First I need to **construct** it as follows:
```
$ curl -X POST http://localhost:8090/v2/wallets/2269611a3c10b219b0d38d74b004c298b76d16a9/transactions-construct \  
-d '{
   "payments":[
      {
         "address":"addr_test1qqd2rhj0956q9xv8cevczvrvwg405agz7fzz0m2n87xhjvhxskq78v86w3zv9zc588rrp43sl2cusftxqkv3hzc0xs2sze9fu4",
         "amount":{
            "quantity":1500000,
            "unit":"lovelace"
         },
         "assets":[
            {
               "policy_id":"919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149",
               "asset_name":"4861707079436f696e",
               "quantity":15
            }
         ]
      },
      {
         "address":"addr_test1qqf90safefvsafmacrtu899vwg6nde3m8afeadtk8cp7qw8xskq78v86w3zv9zc588rrp43sl2cusftxqkv3hzc0xs2sekar6k",
         "amount":{
            "quantity":1500000,
            "unit":"lovelace"
         },
         "assets":[
            {
               "policy_id":"919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149",
               "asset_name":"4861707079436f696e",
               "quantity":15
            }
         ]
      },
      {
         "address":"addr_test1qpc038ku3u2js7hykte8xl7wctgl6avy20cp4k0z4ys2cy0xskq78v86w3zv9zc588rrp43sl2cusftxqkv3hzc0xs2sls284n",
         "amount":{
            "quantity":1500000,
            "unit":"lovelace"
         },
         "assets":[
            {
               "policy_id":"919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149",
               "asset_name":"4861707079436f696e",
               "quantity":15
            }
         ]
      }
   ]
}' \  
-H "Content-Type: application/json"
```
I should receive CBOR-encoded `transaction`, `fee` and `coin_selection` details in response.
I can now **sign** and **submit** such transaction just like in [[how-to-make-a-transaction]].
