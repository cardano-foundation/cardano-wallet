# Shared wallets

## Pre-requisites

- [how to start a server](start-wallet-server.md)
- In order to be able to send transactions, our wallet must have funds. In case of `preview` and `preprod` [testnets](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Overview
This guide shows you how to create a shared wallet and make a shared transaction, providing witnesses from all required co-signers referenced in the `payment_script_template` and `delegation_script_template` fields. In this example, we'll create two shared wallets using the same template for both payment and delegation operations. The template that we'll use will indicate that we require signatures from **all** co-owners in order to make a transaction. In our case, the wallet will have two co-owners `cosigner#0` and `cosigner#1`:
```
"template":
    { "all":
       [ "cosigner#0", "cosigner#1"]
    }
```

```admonish note
The script templates use Cardano [simple scripts](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md) therefore one can build more sophisticated templates to guard their shared wallet spending or delegation operations. Below are some examples of possible templates. You can also explore [`POST /shared-wallets`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postSharedWallet) swagger specification for more details.
```

### Template examples

 - required signature from any cosigner from the list
```
"template":
    { "any":
       [ "cosigner#0", "cosigner#1", "cosigner#2"]
    }
```

- required signature from at least 2 cosigners from the list
```
"template":
    { "some":
       { "at_least": 2,
         "from": [ "cosigner#0", "cosigner#1", "cosigner#2"]
       }
    }
```
- required signature from at least 1 cosigners from the list but with additional nested constraints (at least `cosigner#0` or at least `cosigner#1` or at least both `cosigner#2` and `cosigner#3` and any of `cosigner#4` and `cosigner#5`)
```
"template":{
  "some":{
      "at_least":1,
      "from":[
        "cosigner#0",
        "cosigner#1",
        {
            "all":[
              "cosigner#2",
              "cosigner#3",
              {
                  "any":[
                    "cosigner#4",
                    "cosigner#5"
                  ]
              }
            ]
        }
      ]
  }
}
```

- required signature from any cosigner but with additional time locking constraints (either `cosigner#0` or `cosigner#1` but only until slot 18447928 or `cosigner#2` but only from slot 18447928)

```
"template":{
  "any":[
      "cosigner#0",
      {
        "all":[
            "cosigner#1",
            {
              "active_until":18447928
            }
        ]
      },
      {
        "all":[
            "cosigner#2",
            {
              "active_from":18447928
            }
        ]
      }
  ]
}
```


## Creating wallets
First let's create two `incomplete` shared wallets using the [`POST /shared-wallets`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postSharedWallet) endpoint. The wallets are marked as "incomplete" because they do not yet have public keys from all cosigners.

1. `Cosigner#0` wallet

```
> curl -X POST http://localhost:8090/v2/shared-wallets \
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
   "name":"`Cosigner#0` wallet",
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

2. `Cosigner#1` wallet
```
> curl -X POST http://localhost:8090/v2/shared-wallets \
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
   "name":"`Cosigner#1` wallet",
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
However the `"cosigners":` part differs slightly. "`Cosigner#0` wallet" has `"cosigner#0":"self"` and "`Cosigner#1` wallet" - `"cosigner#1":"self"`. Each wallet has partial information about cosigners. "`Cosigner#0` wallet" only knows about `cosigner#0` and "`Cosigner#1` wallet" only knows about `cosigner#1`.

Now we can look up the wallets we've just created using the [`GET /shared-wallets/{walletId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getSharedWallet) endpoint. From the response, we can tell that the wallet's status is "incomplete". For instance:

```
> curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8 | jq

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
  "name": "`Cosigner#1` wallet",
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
In order to be able to spend from the wallets we need to _patch_ their payment and delegation templates with missing cosigners.
We already know that "`Cosigner#0` wallet" is missing the `cosigner#1` key and "`Cosigner#1` wallet" is missing the `cosigner#0` key.

First, let's get the `cosigner#1` key from the "`Cosigner#1` wallet". We can use the [`GET /shared-wallets/{walletId}/keys`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getAccountKeyShared) endpoint for this:

```
> curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/keys?format=extended

"acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"

```

Now we can _patch_ the payment and delegation templates of the "`Cosigner#0` wallet" with the `cosigner#1` key using the [`PATCH /shared-wallets/{walletId}/payment-script-template`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/patchSharedWalletInPayment) and [`PATCH /shared-wallets/{walletId}/delegation-script-template`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/patchSharedWalletInDelegation) endpoints, respectively.

```
> curl -X PATCH http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/payment-script-template \
-d '{"cosigner#1":"acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"}' \
-H "Content-Type: application/json"

> curl -X PATCH http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/delegation-script-template \
-d '{"cosigner#1":"acct_shared_xvk1mqrjad6aklkpwvhhktrzeef5yunla9pjs0jt9csp3gjcynxvumfjpk99hqxkyknn30ya6l5yjgeegs5ltmmsy70gm500sacvllvwt6qjztknp"}' \
-H "Content-Type: application/json"
```

We'll repeat the same action or "`Cosigner#1` wallet", i.e. first get `cosigner#0` key from "`Cosigner#0` wallet" and then patch the payment and delegation templates of "`Cosigner#1` wallet" with it.

```
> curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/keys?format=extended

"acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl"

> curl -X PATCH http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/payment-script-template \
-d '{"cosigner#0":"acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl"}' \
-H "Content-Type: application/json"

> curl -X PATCH http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/delegation-script-template \
-d '{"cosigner#0":"acct_shared_xvk1lk5eg5unj4fg48vamr9pc40euump5qs084a7cdgvs9u6yn82nh9lr3s62asfv45r4s0fqa239j3dc5e4f7z24heug43zpg6qdy5kawq63hhzl"}' \
-H "Content-Type: application/json"
```

Now, if we look up the wallet again with [`GET /shared-wallets/{walletId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getSharedWallet), we should notice that the wallet has started restoring, and after a while it is ready to use:

```
> curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .state

{
  "status": "ready"
}
```

Furthermore, the wallets now have complete information about cosigners for delegation and payment templates:

```
> curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .payment_script_template

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

> curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .delegation_script_template

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

### Spending transactions

Our shared wallets are now fully-operational. In particular, we can access the addresses of the wallets via the [`GET /shared-wallets/{walletId}/addresses`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/listSharedAddresses) endpoint. We can get the address and fund it from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) so that we can spend from our wallet later on.

After we have funded the "`Cosigner#0` wallet", we can see that the balance has changed on the "`Cosigner#1` wallet" as well. Both wallets have the same balance:

```
> curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f | jq .balance

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

> curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8 | jq .balance

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

Of course, this is expected. Both co-owners have full knowledge of the balance, however they cannot spend from the balance on their own. As required by the payment template, the wallet needs signatures from both co-owners before funds can be spent.

Let's make a simple transaction. The owner of "`Cosigner#0` wallet" will construct and sign a transaction on their end, and then provide a CBOR blob of this transaction to the owner of "`Cosigner#1` wallet". Then `Cosigner#1` can sign it on their own, and submit it to the network.

We will be using the following shared wallet transaction endpoints:
 - [`POST /shared-wallets/{walletId}/transactions-construct`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/constructSharedTransaction)
 - [`POST /shared-wallets/{walletId}/transactions-sign`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/signSharedTransaction)
 - [`POST /shared-wallets/{walletId}/transactions-submit`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitSharedTransaction)

```admonish note
At any point during the process of constructing and signing a transaction, both "`Cosigner#0`" and "`Cosigner#1`" may decode the transaction from its CBOR representation using the [`POST /shared-wallets/{walletId}/transactions-decode`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/decodeSharedTransaction) endpoint. Decoding the transaction makes it possible to see all details associated with the transaction, such as its `inputs`, `outputs`, `fees`, `deposits`, `metadata`, or `witness_count`.
```

#### `Cosigner#0`

##### Constructing

"`Cosigner#0`" constructs a transaction sending 10₳ to an external address using the [`POST /shared-wallets/{walletId}/transactions-construct`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/constructSharedTransaction) endpoint. In response, they receive a CBOR representation of the unsigned transaction.

```
> curl -X POST http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/transactions-construct \
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
##### Signing

"`Cosigner#0`" signs the transaction with the [`POST /shared-wallets/{walletId}/transactions-sign`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/signSharedTransaction) endpoint:

```
> curl -X POST http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/transactions-sign \
-d '{
   "passphrase":"Secure Passphrase",
   "transaction":"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCAChAYGCAYKCAFgciTss5ZpPDifzTTlpVVbNSaZGERzre1z05XYVWYIAWBzHmpkAwWCRnUTfcTy0aZK2LLLM36Y/4gZMlJhm9fY="
}' \
-H "Content-Type: application/json" | jq .transaction

"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIGCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAQGBggGCggBYHIk7LOWaTw4n8005aVVWzUmmRhEc63tc9OV2FVmCAFgcx5qZAMFgkZ1E33E8tGmStiyyzN+mP+IGTJSYZvX2"
```
and in response, receives a CBOR representation of the partially-signed transaction.

#### `Cosigner#1`

Now "`Cosigner#0`" can hand over the CBOR of the partially-signed transaction to "`Cosigner#1`", who can then sign it on their own.

##### Signing

"`Cosigner#1`" signs the transaction with [`POST /shared-wallets/{walletId}/transactions-sign`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/signSharedTransaction) and gets a CBOR representation of the fully-signed transaction in response.

```
> curl -X POST http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/transactions-sign \
-d '{
   "passphrase":"Secure Passphrase",
   "transaction":"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIGCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAQGBggGCggBYHIk7LOWaTw4n8005aVVWzUmmRhEc63tc9OV2FVmCAFgcx5qZAMFgkZ1E33E8tGmStiyyzN+mP+IGTJSYZvX2"
}' \
-H "Content-Type: application/json" | jq .transaction

"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIKCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAYJYIKRn9pUgwcx7GBoIBVJd+9Gh8I/YOwFFK7KyXUXSfrWnWEAqOkD9ljkgYwqwPpuxV+4iJw0hf4PPXA7o9XVxZ4wqT6vSnu1hvsyM4ezuCP/ahTMQ7RzZHTLa1BDx6YawGHQHAYGCAYKCAFgciTss5ZpPDifzTTlpVVbNSaZGERzre1z05XYVWYIAWBzHmpkAwWCRnUTfcTy0aZK2LLLM36Y/4gZMlJhm9fY="
```
##### Submission

The transaction is now fully-signed by both co-owners: "`Cosigner#0`" and "`Cosigner#1`". At this point, either of them can submit it to the network using their respective wallets via the [`POST /shared-wallets/{walletId}/transactions-submit`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitSharedTransaction) endpoint.

```
> curl -X POST http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/transactions-submit \
-d '{
   "transaction":"hKUAgYJYIP8Fwso5i5ovF8bJJuqZ4xOdvXC3mXJCN2O2vDIxQQOYAAGCogBYOQAPoKBp8QggxQSBkAcuvb5QKHyIYK/vZAoDka0HLbCXN3jw0iy454aNIf/2HHm0geZZd8Yu8ge2bAEaAJiWgKIAWDkwZ/CRYzhreLBgWdOZFReuuejo5RIhvNwLOk51lWQGg+0Ii6yF8VB/6jOUJdwEhwqE3Od9sHl14eQBGwAAAAJTcJsvAhoAArJRAxoBDHzCCACiAIKCWCBOK9nJ9IxJt2gddyZ2fUHC4nre84+EbPQdL60OP0m4ZlhA11gVIBWSlDZl8NQyzV4v9U8AuX8n2UqJK9+Wt1sqnM7jAeYtbuyAN5weTaVV+NDVlpKVg3piowyC1eqZBeWWAYJYIKRn9pUgwcx7GBoIBVJd+9Gh8I/YOwFFK7KyXUXSfrWnWEAqOkD9ljkgYwqwPpuxV+4iJw0hf4PPXA7o9XVxZ4wqT6vSnu1hvsyM4ezuCP/ahTMQ7RzZHTLa1BDx6YawGHQHAYGCAYKCAFgciTss5ZpPDifzTTlpVVbNSaZGERzre1z05XYVWYIAWBzHmpkAwWCRnUTfcTy0aZK2LLLM36Y/4gZMlJhm9fY="
}' \
-H "Content-Type: application/json" | jq

{
  "id": "d443719bbd3e4301aa34791823b2b7821757e843509d29918006e5ca26ca368c"
}
```

The transaction has been submitted successfully. The [`POST /shared-wallets/{walletId}/transactions-submit`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitSharedTransaction) endpoint provides a transaction identifier in its response.

We can use this transaction identifier to look up the transaction from either shared wallet using the [`GET /shared-wallets/{walletId}/transactions`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/listSharedTransactions) or [`GET /shared-wallets/{walletId}/transactions/{transactionId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getSharedTransaction) endpoints.

"`Cosigner#0` wallet":

```
> curl -X GET http://localhost:8090/v2/shared-wallets/2a0ebd0cceab2161765badf2e389b26e0961de2f/transactions/d443719bbd3e4301aa34791823b2b7821757e843509d29918006e5ca26ca368c | jq

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

"`Cosigner#1` wallet":

```
> curl -X GET http://localhost:8090/v2/shared-wallets/5e46668c320bb4568dd25551e0c33b0539668aa8/transactions/d443719bbd3e4301aa34791823b2b7821757e843509d29918006e5ca26ca368c | jq

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

Delegation of shared wallets will be supported _soon_.
