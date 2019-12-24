# Tests for Cardano.Wallet syncProgress

## OS

Windows, MacOS, Linux

## state is Ready

1. Launch wallet and jormungandr in separate terminals
```bash
$ jormungandr \
     --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin \
     --rest-listen 127.0.0.1:8080 \
     --secret lib/jormungandr/test/data/jormungandr/secret.yaml

$ cardano-wallet-jormungandr serve --genesis-block-hash $(jcli genesis hash --input lib/jormungandr/test/data/jormungandr/block0.bin) --node-port 8080
```

2. Create wallets
 - Shelley
```
curl  -vX POST http://localhost:8090/v2/wallets \
  -H "Content-Type: application/json; charset=utf-8" \
  -d '{
"name": "Shelley",
"mnemonic_sentence": ["identify", "screen", "lock", "bargain", "inch", "drop", "canyon", "flock", "dry", "zone", "wash", "argue", "system", "glory", "light"],
"passphrase": "Secure Passphrase",
"address_pool_gap": 20
}' --http1.1  |  jq
```
 - Daedalus Byron (12-word mnemonic sentence)
 ```
 curl  -vX POST http://localhost:8090/v2/byron-wallets \
  -H "Content-Type: application/json; charset=utf-8" \
  -d '{
"name": "Daedalus Byron",
"mnemonic_sentence": ["rotate", "machine", "travel", "safe", "expire", "leopard", "wink", "vault", "borrow", "digital", "wisdom", "harsh"],
"passphrase": "Secure Passphrase"
}' --http1.1  |  jq
```

 - Yoroi Byron (15-word mnemonic sentence)
 ```
 curl  -vX POST http://localhost:8090/v2/byron-wallets \
  -H "Content-Type: application/json; charset=utf-8" \
  -d '{
"name": "Yoroi Byron",
"mnemonic_sentence": ["enforce", "nuclear", "script", "word", "bridge", "slim", "moon", "below", "hair", "drum", "usual", "quick", "garage", "spray", "proud"],
"passphrase": "Secure Passphrase"
}' --http1.1  |  jq
```

The state field should eventually (or quickly) reach `"state": { "status": "ready" }` on all three wallets.
 - Shelley
```bash
$ cardano-wallet-jormungandr wallet list
```
 - Byron
 ```bash
 $ curl http://localhost:8090/v2/byron-wallets | jq
 ```

 3. One can also list stake-pools and query network information successfully

 ```bash
 $ cardano-wallet-jormungandr network information
 Ok.
 {
     "network_tip": {
         "epoch_number": 1048621,
         "slot_number": 0
     },
     "node_tip": {
         "height": {
             "quantity": 4,
             "unit": "block"
         },
         "epoch_number": 1048620,
         "slot_number": 9
     },
     "sync_progress": {
         "status": "ready"
     },
     "next_epoch": {
         "epoch_start_time": "2019-12-24T08:01:37Z",
         "epoch_number": 1048622
     }
 }
 ```

 ```bash
 $ cardano-wallet-jormungandr stake-pool list
 Ok.
 [
     {
         "metrics": {
             "controlled_stake": {
                 "quantity": 1,
                 "unit": "lovelace"
             },
             "produced_blocks": {
                 "quantity": 3,
                 "unit": "block"
             }
         },
         "cost": {
             "quantity": 0,
             "unit": "lovelace"
         },
         "margin": {
             "quantity": 0,
             "unit": "percent"
         },
         "apparent_performance": 1,
         "id": "712dd028687560506e3b0b3874adbd929ab892591bfdee1221b5ee3796b79b70"
     },
     ....
 ]
 ```

## sync progress drops when no blocks are produced

1. Ctrl-C jormungandr

2. List Shelley wallets
```bash
$ cardano-wallet-jormungandr wallet list
The node backend is unreachable at the moment. Trying again in a bit might work.
```

3. List Byron wallets

```bash
$ curl http://localhost:8090/v2/byron-wallets | jq
```

The progress should eventually drop, and continue dropping. E.g:

```
"state": {
            "status": "syncing",
            "progress": {
                "quantity": 86,
                "unit": "percent"
            }
        },
```

4. Check network information and stake-pools.
```bash
$ cardano-wallet-jormungandr network information
The node backend is unreachable at the moment. Trying again in a bit might work.
```

```bash
$ cardano-wallet-jormungandr stake-pool list
The node backend is unreachable at the moment. Trying again in a bit might work.
```

## sync progress / state can reach Ready again

1. Re-launch jormungandr (cf. beginning of this document)
2. The state field should eventually reach Ready again when polling
```bash
$ cardano-wallet-jormungandr wallet list
```

```bash
$ curl http://localhost:8090/v2/byron-wallets | jq
```

3. One can also list stake-pools and query network information successfully

```bash
$ cardano-wallet-jormungandr network information
Ok.
{
    "network_tip": {
        "epoch_number": 1048621,
        "slot_number": 0
    },
    "node_tip": {
        "height": {
            "quantity": 4,
            "unit": "block"
        },
        "epoch_number": 1048620,
        "slot_number": 9
    },
    "sync_progress": {
        "status": "ready"
    },
    "next_epoch": {
        "epoch_start_time": "2019-12-24T08:01:37Z",
        "epoch_number": 1048622
    }
}
```

```bash
$ cardano-wallet-jormungandr stake-pool list
Ok.
[
    {
        "metrics": {
            "controlled_stake": {
                "quantity": 1,
                "unit": "lovelace"
            },
            "produced_blocks": {
                "quantity": 3,
                "unit": "block"
            }
        },
        "cost": {
            "quantity": 0,
            "unit": "lovelace"
        },
        "margin": {
            "quantity": 0,
            "unit": "percent"
        },
        "apparent_performance": 1,
        "id": "712dd028687560506e3b0b3874adbd929ab892591bfdee1221b5ee3796b79b70"
    },
    ....
]
```
