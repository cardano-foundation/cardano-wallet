
# Overarching plan

Use `Submissions` table for storing transactions that are pending 

No longer use the `TxMeta` + `TxHistory`  tables for storing these transactions.

## Rewrite `readLocalTxSubmissionPending_` 

Use the `Submissions` `Store` or `DBVar`.

## Re-imagine `putLocalTxSubmission`

In `submitTx`, remove the call to `putTxHistory`, as the transactions in the
submission database are no longer part of the `TxHistory` or the `TxMeta` table.

## Re-imagine `pruneLocalTxSubmission`

Use the block height of `tip` minus `epochStability`.  (Don't worry about the
distinction between block height and `Slot`.)

## Re-imagine `removePendingOrExpiredTx_` as `forget`

## Re-imagine `updatePendingTxForExpiry_` as `rollForward`

- In `restoreBlocks`, the `Tx` need to be taken out of `blocks`.  
- In `restoreBlocks`, assume that a pattern match of `blocks` on a `List` 
  constructor always succeeds, as we are not using light-mode.

## Integrate `Submissions` into `rollbackTo_`

Change the rollback to perform two separate rollbacks:

- Rollback on the `TxMeta` table and `TxHistory` simply forgets about any
transaction that was rolled back, i.e. no longer moves it to `Pending`.  
- Rollback on the `Submissions` table.  

## Implement migration from TxMeta table to separate Submissions table

We don't convert to new encoding, we just pretend that all pending 
  transactions have expired.


- Remove all transactions whose txMetaStatus is not `InLedger` from TxMeta 
  and TxHistory
- Drop the `LocalTxSubmission` table.

## Testing: End-2-End test by Piotr

- migrations leaves only in ledger transactions in the history 
  - should this break API tests about get transactions list ?