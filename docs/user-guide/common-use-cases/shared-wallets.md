---
order: 10
title: Shared wallets
---

## Pre-requisites
 - [[how-to-start-wallet-server]]
  - In order to be able to send transactions, our wallet must have funds. In case of `preview` and `preprod` [testnets](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Overview
This guide is to show how to create a shared wallet and make a shared transaction providing witnesses from all required co-signers referenced in  `payment_script_template` and `delegation_script_template`. In this example we will create two shared wallets using the same template for both payment and delegation operations. The template that we use indicates that we require all signatures from co-owners in order to make a transaction. In our case the wallet will have two co-owners `cosigner#0` and `cosigner#1`:
```
"template":
    { "all":
       [ "cosigner#0", "cosigner#1"]
    }
```

## Creating wallets
First let's create two `incomplete` shared wallets using [`POST /shared-wallets`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postSharedWallet) endpoint. The wallets are called "incomplete" because they do not have public keys from all cosigners yet.

1. Cosigner#0 wallet

```
$ curl -X POST http://localhost:8090/v2/shared-wallets \
-d '{
   "mnemonic_sentence":[
      "beach",
      "want",
      "fly",
      "guess",
      "cabbage",
      "hybrid",
      "profit",
      "leaf",
      "term",
      "air",
      "join",
      "feel",
      "sting",
      "nurse",
      "anchor",
      "come",
      "entire",
      "oil",
      "kidney",
      "situate",
      "fun",
      "deal",
      "palm",
      "chimney"
   ],
   "passphrase":"Secure Passphrase",
   "name":"Cosigner#0 wallet",
   "account_index":"0H",
   "payment_script_template":{
      "cosigners":{
         "cosigner#0":"self"
      },
      "template":{
         "all":[
            "cosigner#0",
            "cosigner#1"
         ]
      }
   },
   "delegation_script_template":{
      "cosigners":{
         "cosigner#0":"self"
      },
      "template":{
         "all":[
            "cosigner#0",
            "cosigner#1"
         ]
      }
   }
}' \
-H "Content-Type: application/json"
```

2. Cosigner#1 wallet
```
$ curl -X POST http://localhost:8090/v2/shared-wallets \
-d '{
   "mnemonic_sentence":[
      "slight",
      "tool",
      "pear",
      "write",
      "body",
      "fruit",
      "crucial",
      "tomorrow",
      "hunt",
      "alley",
      "object",
      "tool",
      "voyage",
      "loud",
      "loop",
      "client",
      "access",
      "vocal",
      "unable",
      "brand",
      "patch",
      "remain",
      "object",
      "boat"
   ],
   "passphrase":"Secure Passphrase",
   "name":"Cosigner#1 wallet",
   "account_index":"0H",
   "payment_script_template":{
      "cosigners":{
         "cosigner#1":"self"
      },
      "template":{
         "all":[
            "cosigner#0",
            "cosigner#1"
         ]
      }
   },
   "delegation_script_template":{
      "cosigners":{
         "cosigner#1":"self"
      },
      "template":{
         "all":[
            "cosigner#0",
            "cosigner#1"
         ]
      }
   }
}' \
-H "Content-Type: application/json"
```

Notice that the templates `payment_script_template` and `delegation_script_template` are partially the same for both wallets:

```
  "template":{
      "all":[
        "cosigner#0",
        "cosigner#1"
      ]
  }
```
However the `"cosigners":` part differs slightly. "Cosigner#0 wallet" has `"cosigner#0":"self"` and "Cosigner#1 wallet" - `"cosigner#1":"self"`. Each wallet has partial information about cosigners. "Cosigner#0 wallet" only knows about `cosigner#0` and "Cosigner#1 wallet" only knows about `cosigner#1`.

Now we can look up just created wallets using [`GET /shared-wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getSharedWallet) endpoint. From the response we can tell that the wallet's status is "incomplete". For instance:

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8 | jq

{
  "account_index": "0H",
  "address_pool_gap": 20,
  "delegation_script_template": {
    "cosigners": {
      "cosigner#1": "acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"
    },
    "template": {
      "all": [
        "cosigner#0",
        "cosigner#1"
      ]
    }
  },
  "id": "5e46668c320bb4568dd25551e0c33b0539668aa8",
  "name": "Cosigner#1 wallet",
  "payment_script_template": {
    "cosigners": {
      "cosigner#1": "acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"
    },
    "template": {
      "all": [
        "cosigner#0",
        "cosigner#1"
      ]
    }
  },
  "state": {
    "status": "incomplete"
  }
}

```

### Patching wallets
In order to be able to spend from the wallets we need to patch their payment and delegation templates with missing cosigners.
We already know that "Cosigner#0 wallet" is missing `cosigner#1` key and "Cosigner#1 wallet" is missing `cosigner#0` key.

First let's get the `cosigner#1` key from the "Cosigner#1 wallet". We can use [`GET /shared-wallets/{walletId}/keys`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getAccountKeyShared) endpoint for that:

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/keys?format=extended

"acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"

```

Now we can patch payment and delegation templates of the "Cosigner#0 wallet" with `cosigner#1` key using [`PATCH /shared-wallets/{walletId}/payment-script-template`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/patchSharedWalletInPayment) and [`PATCH /shared-wallets/{walletId}/delegation-script-template`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/patchSharedWalletInDelegation) endpoints, respectively.

```
$ curl -X PATCH http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/payment-script-template \
-d '{"cosigner#1":"acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"}' \
-H "Content-Type: application/json"

$ curl -X PATCH http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/delegation-script-template \
-d '{"cosigner#1":"acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"}' \
-H "Content-Type: application/json"
```

We'll repeat the same for "Cosigner#1 wallet", i.e. first getting `cosigner#0` key from "Cosigner#0 wallet" and patching "Cosigner#1 wallet" payment and delegation templates with it.

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/keys?format=extended

"acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl"

$ curl -X PATCH http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/payment-script-template \
-d '{"cosigner#0":"acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl"}' \
-H "Content-Type: application/json"

$ curl -X PATCH http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/delegation-script-template \
-d '{"cosigner#0":"acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl"}' \
-H "Content-Type: application/json"
```

Now if we look up the wallet again with [`GET /shared-wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getSharedWallet) we'll notice that the wallet starts restoring and after a while it is ready to use:

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .state

{
  "status": "ready"
}
```

Also the wallets now have full information about the cosigners for delegation and payment templates:

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .payment_script_template

{
  "cosigners": {
    "cosigner#0": "acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl",
    "cosigner#1": "acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"
  },
  "template": {
    "all": [
      "cosigner#0",
      "cosigner#1"
    ]
  }
}

$ curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .delegation_script_template

{
  "cosigners": {
    "cosigner#0": "acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl",
    "cosigner#1": "acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"
  },
  "template": {
    "all": [
      "cosigner#0",
      "cosigner#1"
    ]
  }
}
```

### Spending transaction

Our shared wallets are fully operational now. In particular we have access to the addresses of the wallets via [`GET /shared-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listSharedAddresses) endpoint. We can get the address and fund it from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) so that we can spend from our wallet later.

After I have funded my "Cosigner#0 wallet" I see that the balance changed on "Cosigner#1 wallet" as well. Both wallets have the same balance:

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .balance

{
  "available": {
    "quantity": 10000000000,
    "unit": "lovelace"
  },
  "reward": {
    "quantity": 0,
    "unit": "lovelace"
  },
  "total": {
    "quantity": 10000000000,
    "unit": "lovelace"
  }
}

$ curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8 | jq .balance

{
  "available": {
    "quantity": 10000000000,
    "unit": "lovelace"
  },
  "reward": {
    "quantity": 0,
    "unit": "lovelace"
  },
  "total": {
    "quantity": 10000000000,
    "unit": "lovelace"
  }
}
```

Of course this is expected. Both co-owners have full knowledge of the balance, however they cannot spend it on their own. As required by the payment template the wallet needs both co-owners' signatures to spend from the wallet.

Let's make a simple transaction. "Cosigner#0 wallet" owner will construct and sign the transaction on their end and then provide CBOR of this transaction to "Cosigner#1 wallet" owner. Then the Cosigner#1 can sign it on his side and submit it to the network.

We will be using shared wallet's transaction enpoints:
 - [`POST /shared-wallets/{walletId}/transactions-construct`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/constructSharedTransaction)
 - [`POST /shared-wallets/{walletId}/transactions-sign`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signSharedTransaction)
 - [`POST /shared-wallets/{walletId}/transactions-submit`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/submitSharedTransaction)

#### Cosigner#0

##### Construct

"Cosigner#0" constructs transaction sending 10₳ to the external address using [`POST /shared-wallets/{walletId}/transactions-construct`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/constructSharedTransaction). In response he gets CBOR of the unsigned transaction.

```
$ curl -X POST http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/transactions-construct \
-d '{
   "payments":[
      {
         "address":"addr_test1qq86pgrf7yyzp3gysxgqwt4ahegzslygvzh77eq2qwg66pedkztnw78s6gkt3eux35sllasu0x6grejewlrzaus8kekq7cp9ck",
         "amount":{
            "quantity":10000000,
            "unit":"lovelace"
         }
      }
   ]
}' \
-H "Content-Type: application/json" | jq .transaction

"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHtNCAChAYGCAYKCAFgciTss5ZpPDifzTTlpVVbNSaZGERzre1z05XYVWYIAWBzHmpkAwWCRnUTfcTy0aZK2LLLM36Y/4gZMlJhm9fY="
```
##### Sign

"Cosigner#0" signs the transaction with [`POST /shared-wallets/{walletId}/transactions-sign`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signSharedTransaction):

```
$ curl -X POST http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/transactions-sign \
-d '{
   "passphrase":"Secure Passphrase",
   "transaction":"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCAChAYGCAYKCAFgciTss5ZpPDifzTTlpVVbNSaZGERzre1z05XYVWYIAWBzHmpkAwWCRnUTfcTy0aZK2LLLM36Y/4gZMlJhm9fY="
}' \
-H "Content-Type: application/json" | jq .transaction

"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIGCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAQGBggGCggBYHIk7LOWaTw4n8005aVVWzUmmRhEc63tc9OV2FVmCAFgcx5qZAMFgkZ1E33E8tGmStiyyzN+mP+IGTJSYZvX2"
```
and gets CBOR of partially signed transaction in response.

#### Cosigner#1

Now "Cosigner#0" can hand over the CBOR of partially signed transaction to "Cosigner#1" who can sign it on his end.

##### Sign

"Cosigner#1" signs the transaction with [`POST /shared-wallets/{walletId}/transactions-sign`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signSharedTransaction) and gets CBOR of fully signed transaction as a result.

```
$ curl -X POST http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/transactions-sign \
-d '{
   "passphrase":"Secure Passphrase",
   "transaction":"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIGCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAQGBggGCggBYHIk7LOWaTw4n8005aVVWzUmmRhEc63tc9OV2FVmCAFgcx5qZAMFgkZ1E33E8tGmStiyyzN+mP+IGTJSYZvX2"
}' \
-H "Content-Type: application/json" | jq .transaction

"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIKCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAYJYIKRn9pUgwcx7GBoIBVJd+9Gh8I/YOwFFK7KyXUXSfrWnWEAqOkD9ljkgYwqwPpuxV+4iJw0hf4PPXA7o9XVxZ4wqT6vSnu1hvsyM4ezuCP/ahTMQ7RzZHTLa1BDx6YawGHQHAYGCAYKCAFgciTss5ZpPDifzTTlpVVbNSaZGERzre1z05XYVWYIAWBzHmpkAwWCRnUTfcTy0aZK2LLLM36Y/4gZMlJhm9fY="
```
##### Submit

Transaction is fully signed by both co-owners: "Cosigner#0" and "Cosigner#1". Now either of them can submit it to the network using their respective wallet via [`POST /shared-wallets/{walletId}/transactions-submit`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/submitSharedTransaction) endpoint.

```
$ curl -X POST http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/transactions-submit \
-d '{
   "transaction":"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIKCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAYJYIKRn9pUgwcx7GBoIBVJd+9Gh8I/YOwFFK7KyXUXSfrWnWEAqOkD9ljkgYwqwPpuxV+4iJw0hf4PPXA7o9XVxZ4wqT6vSnu1hvsyM4ezuCP/ahTMQ7RzZHTLa1BDx6YawGHQHAYGCAYKCAFgciTss5ZpPDifzTTlpVVbNSaZGERzre1z05XYVWYIAWBzHmpkAwWCRnUTfcTy0aZK2LLLM36Y/4gZMlJhm9fY="
}' \
-H "Content-Type: application/json" | jq

{
  "id": "d443719bbd3e4301aa34791823b2b7821757e843509d29918006e5ca26ca368c"
}
```

Transaction has been submitted successfully. We have an id of the transaction in the response from the [`POST /shared-wallets/{walletId}/transactions-submit`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/submitSharedTransaction) endpoint.

We can look it up from both shared wallets using [`GET /shared-wallets/{walletId}/transactions`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listSharedTransactions) or [`GET /shared-wallets/{walletId}/transactions/{transactionId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getSharedTransaction) endpoints.

"Cosigner#0 wallet":

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/transactions/d443719bbd3e4301aa34791823b2b7821757e843509d29918006e5ca26ca368c | jq

{
  "amount": {
    "quantity": 10176721,
    "unit": "lovelace"
  },
  ...
  "direction": "outgoing",
  ...
  "fee": {
    "quantity": 176721,
    "unit": "lovelace"
  },
...
```

"Cosigner#1 wallet":

```
$ curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/transactions/d443719bbd3e4301aa34791823b2b7821757e843509d29918006e5ca26ca368c | jq

{
  "amount": {
    "quantity": 10176721,
    "unit": "lovelace"
  },
  ...
  "direction": "outgoing",
  ...
  "fee": {
    "quantity": 176721,
    "unit": "lovelace"
  },
...
```

### Delegation

Delegation of the shared wallet will be supported _soon_.
