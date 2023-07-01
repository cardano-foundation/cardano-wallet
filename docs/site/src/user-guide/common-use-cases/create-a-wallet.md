# How to create a wallet

## Pre-requisites

 - [how to start a server](start-wallet-server.md)

## Overview

The easiest and most common way of managing your funds on the Cardano blockchain is through a [hierarchical deterministic wallet](../../concepts/hierarchical-deterministic-wallets.md)
One can create a wallet using one of the following endpoints of [http-api](../http-api.md):

[ `POST /wallets` ](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postWallet) - create **Shelley** wallet

[ `POST /byron-wallets` ](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postByronWallet) - create **Byron** wallet

[ `POST /shared-wallets` ](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postSharedWallet) - create **Shared** wallet

## Shelley wallets

Shelley wallets are `sequential` wallets recommended for any new applications. In particular they support delegation feature which is not supported by the Byron era wallets.

Exemplary request for creating new Shelley wallet may look as follows:

```
> curl -X POST http://localhost:8090/v2/wallets \
    -d '{"mnemonic_sentence":["slab","praise","suffer","rabbit","during","dream","arch","harvest","culture","book","owner","loud","wool","salon","table","animal","vivid","arrow","dirt","divide","humble","tornado","solution","jungle"],
        "passphrase":"Secure Passphrase",
        "name":"My Test Wallet",
        "address_pool_gap":20}' \
    -H "Content-Type: application/json"
```

Note also that you can have many wallets being operated by a single `cardano-wallet` server.

## Byron wallets

There are several Byron wallet types available:
 - random
 - icarus
 - trezor
 - ledger

The basic difference between them is that for a `random` wallet user needs to [create addresses](how-to-create-addresses.md#random-wallets-legacy-byron) manually, whereas for sequential wallets like `icarus` , `trezor` and `ledger` addresses are
[created automatically](how-to-create-addresses.md#sequential-wallets-icarus-shelley--shared) by the wallet.

> Please note that `random` wallets are considered **deprecated** and should not be used by new applications.

See more on [hierarchical deterministic wallet](../../concepts/hierarchical-deterministic-wallets.md) and [byron-address-format](../../concepts/byron-address-format.md).

## Shared wallets

Shared wallets are modern `sequential` "Shelley-type wallets". The main idea is that funds within shared wallets can be managed by more than one owner. When creating such a wallet, it's necessary to provide a `payment_script_template` that lists all future co-signers and their extended account public keys (for spending operations), as well as a template script primitive that establishes the rules for sharing custody of the wallet's spending operations between the co-signers. Similarly, one can provide a `delegation_script_template` for sharing custody of delegation operations.

Exemplary request for creating new Shared wallet may look as follows:

```
> curl -X POST http://localhost:8090/v2/shared-wallets \
    -d '{"mnemonic_sentence":["possible","lizard","zebra","hill","pluck","tourist","page","ticket","amount","fall","purpose","often","chest","fantasy","funny","sense","pig","goat","pet","minor","creek","vacant","swarm","fun"],
    "passphrase":"Secure Passphrase",
    "name":"My Test Shared Wallet",
    "account_index":"0H",
    "payment_script_template":{"cosigners":{"cosigner#0":"self"},"template":{"all":["cosigner#0",{"active_from":120}]}},
    "delegation_script_template":{"cosigners":{"cosigner#0":"self","cosigner#1":"1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db11423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db2"},"template":{"all":["cosigner#0","cosigner#1",{"active_from":120},{"active_until":300}]}}}' \
    -H "Content-Type: application/json"
```

See more elaborate example on [shared wallets](../common-use-cases/shared-wallets.md).
