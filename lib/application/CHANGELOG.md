# Revision history for cardano-wallet-application

## Unreleased

* Added revision-1 dApp context, reviewed signing, CIP-8/CIP-95 and durable wallet submission on Conway nodes. Compatible Alonzo-, Babbage- and Conway-built transactions retain their original bytes; unsupported node eras return `dapp_unsupported_era`.
* Preserved ordinary pending transaction visibility and input reservations through durable submission, including shared wallets and migrations. Journal reconciliation and node-attempt completion now keep the actual persisted state; unsuccessful or uncertain replays do not report successful submission.
* dApp submission failure responses remain fixed and redacted on first rejection and exact replay. Restart preserves explicit single-address settings and interrupted attempts recover as outcome-unknown without rebroadcast.
* Added an optimized exact-byte Conway context benchmark with native standard and disposable enlarged-size profiles, concurrent-wallet workloads, node/SQLite timing and retained raw samples. Each profile uses one producing node without a relay, fresh baseline/final wallet processes, matched RTS settings and immediate fresh-tip admission. Context measurement avoids unused unbounded API log capture and pretty-print snapshot forcing; ordinary latency capture and integration topology remain unchanged.

## 2024.7.27

* First version. Released on an unsuspecting world.
