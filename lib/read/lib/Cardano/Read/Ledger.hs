{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

The module hierarchy "Cardano.Read.Ledger" contains data types
that are used for reading from the Cardano mainnet ledger.
Specifically, these data types are represented as
era-indexed unions of types from the Haskell ledger implementations
that are used in `cardano-node`.

"Cardano.Read.Ledger" is meant to

* Provide an era-indexed interface over the Byron and Shelley-style
  ledger implementations.
* Improve the useability of type classes in the Shelley-style ledger
  implementation with explicitly notated instances
  and specialization to single eras.

In contrast, the module hierarchy "Cardano.Wallet.Read"
is meant to provide a semantic view of the ledger,
such that the implementation of this view is built on
and mostly compatible with "Cardano.Read.Ledger".

-}
module Cardano.Read.Ledger where
