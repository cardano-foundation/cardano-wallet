# Track Block Height

## OS

Windows, MacOS, Linux

## Goal

The goal of this test suite is to make sure that correct block height is being tracked and logged by the wallet for Jörmungandr node.

## Prerequisites

Start Jörmungandr genesis-praos self-node or connect to the testnet with passive or leader node.
Start wallet backend on top of working Jörmungandr node.
e.g.
```
cardano-wallet-jormungandr launch --genesis-block test/data/jormungandr/block0.bin --secret test/data/jormungandr/secret.yaml --state-dir /tmp/state
```
or
```
ormungandr --genesis-block test/data/jormungandr/block0.bin --config test/data/jormungandr/config.yaml --secret test/data/jormungandr/secret.yaml
cardano-wallet-jormungandr serve --genesis-block-hash 1e96373...21cfd46
```

### Single wallet tracks block height

- Create a wallet via API or CLI
- Make sure that wallet logs track block height:
```
[iohk.cardano-wallet.worker.10e1a38c:Info:ThreadId 42] [2019-11-13 14:26:12.81 UTC] Piotr Wallet, created at 2019-11-13 10:53:21.820271764 UTC, not delegating
[iohk.cardano-wallet.worker.10e1a38c:Info:ThreadId 42] [2019-11-13 14:26:12.81 UTC] syncProgress: restored
[iohk.cardano-wallet.worker.10e1a38c:Info:ThreadId 42] [2019-11-13 14:26:12.81 UTC] discovered 0 new transaction(s)
[iohk.cardano-wallet.worker.10e1a38c:Info:ThreadId 42] [2019-11-13 14:26:12.81 UTC] local tip: 6a6945ef-[0.47586#3474] <-- HERE
```
- Make sure the same is shown in Wallet details
```
curl GET http://localhost:8090/v2/byron-wallets/617963656a409b8a6828dc3a09001de22af90400 |  jq .tip

{
  "height": {
    "quantity": 3474,
    "unit": "block"
  },
  "epoch_number": 0,
  "slot_number": 47586
}


```

- Make sure that it is the same value as reported from Jörmungandr node stats:
```
$ jcli rest v0 node stats get -h http://127.0.0.1:8080/api
---
state = Running
blockRecvCnt = 16
lastBlockDate = 0.47586 <-- HERE
lastBlockFees = 0
lastBlockHash = 6a6945efa1feae33a16fa63eb26f9bfcd72d25a4120ad382731bb798ff68878e <-- HERE
lastBlockHeight = 3474  <-- HERE
lastBlockSum = 0
lastBlockTime = 2019-11-13T14:26:12+00:00
lastBlockTx = 0
txRecvCnt = 0
uptime = 43

```
