section

parameter Slot : Type -- not sure what is

-- we choose to explicitly move around the expiring slot 
-- to remove access to Tx
inductive Status : Type
    -- a transaction has been submitted but it's not in the ledger
    | InSubmission 
      : Slot -- expiring
      -> Status
    -- a transaction is in the ledger but can be rolled out
    | InLedger 
        : Slot -- expiring 
        -> Slot -- acceptance
        -> Status
    -- a transaction has expired but could reappear in case of rollbacks
    | Expired 
      : Slot -- expiring 
      -> Status
    -- a transaction is not known to the submission db
    | unknown : Status
open Status

structure Submissions :=
  -- computations on the slots 
  (before
    : Slot -- ^ what is before or equal to
    -> Slot -- ^ reference
    -> Prop
  )

  (after
    : Slot -- ^ what is strictly after of
    -> Slot -- ^ reference
    -> Prop
  )

  -- a transaction
  (Tx : Type)

  -- transaction database for transactions that could be retransmitted to 
  -- the node
  (Submissions : Type)

  -- transaction status relative to the database
  (status : Tx -> Submissions -> Status)

  -- tip slot of the database, no transactions have, is the separation point
  -- between what is on-ledger (or expired) and what is in-submission
  (tip : Submissions -> Slot)

  -- finality slot of the database, represent the last slot that was pruned off
  (finality : Submissions -> Slot)

  (submissions_invariants
  : ∀ x xs expiring acceptance
  , let  s := status x xs
  ,      t := tip xs
  ,      f := finality xs
  ,      included w := after w f ∧ before w t
  in
      s = InSubmission expiring         → after    expiring t
    ∧ s = Expired expiring              → included expiring
    ∧ s = InLedger expiring acceptance  → included acceptance
    ∧ before f t
  )
end
