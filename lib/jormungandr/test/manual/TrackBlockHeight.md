# Track Block Height

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
[iohk.cardano-wallet.serve.worker.61796365:Info:ThreadId 35] [2019-09-23 15:36:25.06 UTC] Applying blocks [0.359 ... 0.359]
[iohk.cardano-wallet.serve.worker.61796365:Info:ThreadId 35] [2019-09-23 15:36:25.09 UTC] Piotr Wallet (restored), created at 2019-09-23 15:30:32.897236882 UTC, not delegating
[iohk.cardano-wallet.serve.worker.61796365:Info:ThreadId 35] [2019-09-23 15:36:25.09 UTC] number of pending transactions: 0
[iohk.cardano-wallet.serve.worker.61796365:Info:ThreadId 35] [2019-09-23 15:36:25.09 UTC] number of new transactions: 0
[iohk.cardano-wallet.serve.worker.61796365:Info:ThreadId 35] [2019-09-23 15:36:25.09 UTC] new block height: 24 <-- HERE
```
- Make sure that it is the same value as reported from Jörmungandr node stats:
```
$ jcli rest v0 node stats get -h http://127.0.0.1:8080/api
---
blockRecvCnt: 16
lastBlockDate: "0.374"
lastBlockFees: 0
lastBlockHash: f1accc3265e3b19f9f4ab3dd687978fc2e0c934cd36db3bc08aeb41cdef3d80d
lastBlockHeight: "24" <-- HERE
lastBlockSum: 0
lastBlockTime: ~
lastBlockTx: 0
txRecvCnt: 0
uptime: 158
```

### Many wallets track block height
- Create several wallets via API or CLI
- Make sure that all log the same block height as reported in the Jörmungandr node stats:
```
$ jcli rest v0 node stats get -h http://127.0.0.1:8080/api
```

### Block height is tracked after transactions
- Create few wallets via API or CLI
- Send some funds to one of them from the faucet. `faucet-send-money.sh` may be useful from [here](https://github.com/input-output-hk/jormungandr/tree/master/scripts).
- Send some transactions between wallets via API or CLI.
- After transactions are finalized make sure that all wallets log the same block height as the one reported in the Jörmungandr node stats:
```
$ jcli rest v0 node stats get -h http://127.0.0.1:8080/api
```
