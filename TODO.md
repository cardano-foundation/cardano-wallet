# TODO: cardano-node 10.6.2 bump

## Promote DijkstraEra to a RecentEra

The following functions use `error` stubs for DijkstraEra and need proper
implementation once Dijkstra is promoted to a `RecentEra`:

- `lib/api/src/Cardano/Wallet/Api/Types/Era.hs`
  - `fromReadEra`: add `ApiDijkstra` constructor and mapping
  - `fromAnyCardanoEra`: add `ApiDijkstra` mapping

- `lib/balance-tx/lib/internal/Internal/Cardano/Write/Tx.hs`
  - `upgradeToOutputConway`: handle Dijkstra downgrade case

- `lib/local-cluster/test/unit/Cardano/Wallet/Launch/Cluster/Http/ServiceSpec.hs`
  - `txOutFromOutput`: implement Dijkstra case

- `lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs`
  - `_getUTxOByTxIn`: implement Dijkstra query

- `lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery/UTxO.hs`
  - `getUTxOByTxIn`: implement Dijkstra local state query

- `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Convert.hs`
  - `toWalletScript`: handle new script cases

- `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Eras.hs`
  - `fromAnyCardanoEra`: implement Dijkstra case

- `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/Outputs.hs`
  - `txOutFromOutput`: implement Dijkstra case

- `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/Sealed.hs`
  - `fromCardanoApiTx`: implement Dijkstra case

- `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/TxExtended.hs`
  - `fromCardanoTx`: implement Dijkstra case

- `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Shelley.hs`
  - `toCardanoEra`: implement Dijkstra case
  - `forAllBlocks`: implement Dijkstra case

- `lib/wallet/src/Cardano/Wallet.hs`
  - `pparamsInRecentEra`: implement Dijkstra case

- `lib/wallet/src/Cardano/Wallet/Pools.hs`
  - `withRecentEraLedgerTx`: implement Dijkstra case

- `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`
  - Various functions with wildcard `_ -> error` patterns

## Migrate from deprecated Cardano.Api.Certificate

The following files suppress deprecation warnings with
`{-# OPTIONS_GHC -Wno-deprecations #-}` and need migration from
`Cardano.Api.Certificate` to `Cardano.Api.Experimental.Certificate`:

- `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`
- `lib/wallet/src/Cardano/Wallet/Transaction/Delegation.hs`
- `lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs`
