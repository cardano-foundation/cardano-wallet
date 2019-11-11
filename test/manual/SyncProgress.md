# Tests for Cardano.Wallet syncProgress 

## state is Ready 

1. Launch wallet and jormungandr in separate terminals
```bash
$ jormungandr \
     --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin \
     --rest-listen 127.0.0.1:8080 \
     --secret lib/jormungandr/test/data/jormungandr/secret.yaml

$ cardano-wallet-jormungandr serve --genesis-block-hash $(jcli genesis hash --input lib/jormungandr/test/data/jormungandr/block0.bin) --node-port 8080
```

2. Create a wallet
```bash
$ cardano-wallet-jormungandr wallet create wallet
```

The state field should eventually (or quickly) reach `"state": { "status": "ready" }`.
```bash
$ cardano-wallet-jormungandr wallet list
```

## sync progress drops when no blocks are produced

1. Ctrl-C jormungandr

2. Repeatedly poll
```bash
$ cardano-wallet-jormungandr wallet list
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

## sync progress / state can reach Ready again 

1. Re-launch jormungandr (cf. beginning of this document)
2. The state field should eventually reach Ready again when polling
```bash
$ cardano-wallet-jormungandr wallet list
```
