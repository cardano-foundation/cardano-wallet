# DB Migration from previously released version

## OS

Windows, MacOS, Linux

**strongly recommended to test on all platforms**

## Start previous version of the wallet

1. Get **previous** release -> https://github.com/input-output-hk/cardano-wallet/releases
2. start wallet (note: corresponding chain producer is set up according to release notes)
```bash

cardano-wallet launch --genesis-block test/data/jormungandr/block0.bin \
                      --node-port 8080 --state-dir ./data_launch \
                      -- --secret ../secret.yaml --config ../config.yaml

```
3. Produce some data in the wallet backend DB e.g.
 - add shelley wallet
 - add byron wallet
 - send tx
 - delegate to a stake-pool

## Start current version of the wallet

1. Get **current** release -> https://github.com/input-output-hk/cardano-wallet/releases
2. start wallet (on the same `--state-dir`)
```bash

cardano-wallet launch --genesis-block test/data/jormungandr/block0.bin \
                      --node-port 8080 --state-dir ./data_launch \
                      -- --secret ../secret.yaml --config ../config.yaml

```

3. Check basic functionality:
 - add shelley wallet
 - add byron wallet
 - send tx
 - delegate to a stake-pool
 - network information

 4. Wallet starts and functions properly.
