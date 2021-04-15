# Transaction resubmission

Any transaction on the Cardano blockchain can be rolled back due to a
chain switch, until its block is _StabilityWindow_ slots from the
chain tip [[shelley-ledger-spec][]].

After `cardano-wallet` submits a transaction, it tracks its state as
_Pending_, until it sees the transaction in a block from the chain. At
that point, the transaction will be marked _InLedger_.

If there is a chain switch, `cardano-wallet` will roll back its
transaction history database, and the transaction will revert back to
_Pending_ state, until it has been discovered again on the new chain.

However, it's not guaranteed that the transaction will ever appear
again. If not, the transaction will remain in the wallet with
_Pending_ state, until its TTL lapses, whereupon it will change to
_Expired_ state.

The reason for this is that `cardano-node` doesn't track transactions
which left its mempool due to TxSubmission, but were never
adopted. This is fair enough. The same behaviour even applies to
transactions which entered the node's mempool via LocalTxSubmission
(i.e. those submitted to a local node by the wallet).

So `cardano-wallet` itself must retry submission of _Pending_
transactions, for as long as their slot validity period is current
(i.e. not expired).

### How it works

After posting a transaction, the wallet will store the serialized
transaction in the `local_tx_submission` table, along with the last
slot that it was submitted.

A separate thread runs per wallet, which follows the chain tip, for
the purpose of transaction resubmission.

For any pending transaction in the wallet database, it will be
resubmitted to the local node every _n_ slots (where _n_ is the number
of slots during which 10 blocks occur on average).

The LocalTxSubmission protocol will usually return an error for
retried transactions, due to already spent UTxO, etc. But this is
expected. The wallet just ignores the error, and updates the slot
field in the database table.

Once a submitted transaction has expired, or was accepted and can no
longer be rolled back (i.e. its block was more than _StabilityWindow_
ago), it is removed from the `local_tx_submission` table.

### Exceptions

Pre-signed transactions which were posted to the external transactions
endpoint are _not_ retried -- it is up to the user to retry. This
could be changed in future if the wallet can discover transactions
which don't belong to its own wallets.

[shelley-ledger-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/native.specs.shelley-ledger.x86_64-linux/latest/download/1
