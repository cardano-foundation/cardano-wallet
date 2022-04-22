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

> :information_source: Minting and burning assets is also available using `cardano-cli`. See more in https://developers.cardano.org/docs/native-tokens/minting.

Minting and burning of assets is available in the `cardano-wallet` in new transaction workflow. One can use wallet's key to mint any tokens that are guarded by native policy script. In practice it is just as simple as constructing a transaction that has a `mint_burn` field, then signing and submitting it to the network.

> :information_source: See: https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/constructTransaction

### Minting an NFT

As an example we will see how can we mint an NFT with [CIP-25](https://cips.cardano.org/cips/cip25/) metadata using `cardano-wallet`.

#### Policy key

Before we attempt any minting/burning transaction we need to make sure our wallet is set up with a policy key. We can check it using [`GET /wallets/{walletId}/policy-key`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getPolicyKey):
```
$ curl -X GET http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/policy-key
"policy_vk12d0gdel9u6px8wf3uv4z6m4h447n9qsad24gztaku8dzzdqfajzqfm3rr0"
```
Looks good.
In case we get `missing_policy_public_key` error:
```
$ curl -X GET http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/policy-key
{
  "code": "missing_policy_public_key",
  "message": "It seems the wallet lacks a policy public key. Therefore it's not possible to create a minting/burning transaction or get a policy id. Please first POST to endpoint /wallets/{walletId}/policy-key to set a policy key."
}

```
We just need to make a `POST` request to [`POST /wallets/{walletId}/policy-key`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postPolicyKey) as suggested in the error message.

```
$ curl -X POST http://localhost:8091/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/policy-key \  
-d '{"passphrase":"Secure Passphrase"}' \  
-H "Content-Type: application/json"

"policy_vk12d0gdel9u6px8wf3uv4z6m4h447n9qsad24gztaku8dzzdqfajzqfm3rr0"
```

Once we sort it out we are all set and we can proceed into minting an NFT from our wallet!


#### CIP-25 metadata

We will be attaching CIP-25 metadata to our minting transaction, so there are few things that need to be sorted out. Most basic metadata can look as follows:
```
{
      "721": {
        "<POLICY_ID>": {
          "<ASSET_NAME>": {
            "name": "My amazing NFT",
            "image": "ipfs://<IPFS_ID>"
          }
        }
      }
}
```

As we can see we need to figure out `<POLICY_ID>`, `<ASSET_NAME>` and `<IPFS_ID>`.

##### Policy ID

Policy id is basically a hash of the native script that guards minting operation. In case of Shelley wallets we can only sign with one key, a wallet spending key, but because of the fact that we can embed it into a native script we can practically have unlimited amount of policy ids from one wallet.
These are examples of native scripts templates and each of them will produce different policy id.

```
cosigner#0

{ "all": [ "cosigner#0" ] }

{ "any": [ "cosigner#0" ] }

{ "some": {"at_least": 1, "from": [ "cosigner#0" ]} }

{ "all":
     [ "cosigner#0",
       { "active_from": 120 }
     ]
}

{ "all":
     [ "cosigner#0",
       { "active_until": 1200000 }
     ]
}
```
> :information_source: `cosigner#0` stands for our wallet's spending key. In case of Shelley wallet we have only one. In the future, in the Shared wallets, we'll be able to have many spending keys shared between different users and they will be identified as `cosigner#1`, `cosigner#2`...

Let's create most basic policy id using [`POST /wallets/{walletId}/policy-id`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postPolicyId) endpoint:
```
curl -X POST http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/policy-id \
-d '{"policy_script_template":"cosigner#0"}' \
-H "Content-Type: application/json" | jq

{
  "policy_id": "6d5052088183db1ef06439a9f501b52721c2645532a50254a69d5390"
}
```

##### Asset name

The asset name acts as a sub-identifier within a given policy. Although we call it "asset name", the value needn't be text, and it could even be empty. In case of CIP-25 however we can use a plain text. Let our asset name be... `AmazingNFT`.

##### IPFS id

Let's assume that our NFT will point to a simple image, which we have already uploaded to IPFS. It is available at: https://ipfs.io/ipfs/QmRhTTbUrPYEw3mJGGhQqQST9k86v1DPBiTTWJGKDJsVFw. As we can see the IPFS id of the image is `QmRhTTbUrPYEw3mJGGhQqQST9k86v1DPBiTTWJGKDJsVFw`.

Now we can put together complete CIP-25 metadata JSON which we will use in the minting transaction:

```
{
      "721": {
        "6d5052088183db1ef06439a9f501b52721c2645532a50254a69d5390": {
          "AmazingNFT": {
            "name": "My amazing NFT",
            "image": "ipfs://QmRhTTbUrPYEw3mJGGhQqQST9k86v1DPBiTTWJGKDJsVFw"
          }
        }
      }
}
```
#### Minting transaction

We have already:
 -  verified that our wallet is equipped with policy key
 - created CIP-25 metadata JSON.

We are now ready to mint!

We will **construct**  transaction that mints 1 `AmazingNFT` with policy id derived from simple native script template = `cosigner#0` and posts related CIP-25 metadata to blockchain.

Note that the wallet expects asset name to be hex-encoded string so let's hex encode our `AmazingNFT` first:
```
$ echo -n "AmazingNFT" | xxd -p
416d617a696e674e4654
```

Let's now [`POST /wallets/{walletId}/transactions-construct`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/constructTransaction):

```
$ curl -X POST http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/transactions-construct \
-d '{
   "metadata":{
      "721":{
         "6d5052088183db1ef06439a9f501b52721c2645532a50254a69d5390":{
            "AmazingNFT":{
               "name":"My amazing NFT",
               "image":"ipfs://QmRhTTbUrPYEw3mJGGhQqQST9k86v1DPBiTTWJGKDJsVFw"
            }
         }
      }
   },
   "mint_burn":[
      {
         "operation":{
            "mint":{
               "quantity":1
            }
         },
         "policy_script_template":"cosigner#0",
         "asset_name":"416d617a696e674e4654"
      }
   ]
}' \
-H "Content-Type: application/json"
```
That's it! I should now receive CBOR-encoded `transaction`, `fee` and `coin_selection` details in response.
I can now **sign** and **submit** such transaction just like in [[how-to-make-a-transaction]].
Once submitted my freshly minted NFT should be added to my wallet balance!

### Burning an NFT


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
