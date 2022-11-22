section

parameter Slot : Type
  -- transaction database for submitted transactions
parameter Submissions : Type

-- we choose to explicitly move around the expiring slot 
-- to remove access to Tx
inductive TxStatus : Type
    -- a transaction has been submitted but it's not in the ledger
    | InSubmission 
      : Slot -- expiring
      -> TxStatus
    -- a transaction is in the ledger but can be rolled out
    | InLedger 
        : Slot -- expiring 
        -> Slot -- acceptance
        -> TxStatus
    -- a transaction has Expired but could reappear in case of rollbacks
    | Expired 
      : Slot -- expiring 
      -> TxStatus
    -- a transaction is not known to the submission db
    | Unknown : TxStatus
open TxStatus



structure Submissions :=

  (before
    : Slot -- ^ transaction slot
    -> Slot -- ^ reference slot
    -> Prop
  )

  (after
    : Slot -- ^ transaction slot
    -> Slot -- ^ reference slot
    -> Prop
  )
  -- a transaction
  (Tx : Type)

  -- transaction status relative to the database
  (status : Tx -> Submissions -> TxStatus)

  -- tip slot of the database, no transactions have
  -- acceptance slot > tip
  (tip : Submissions -> Slot)
  -- finality slot of the database, no transactions have
  -- acceptance slot <= finality ∨ expiring slot <= finality
  (finality : Submissions -> Slot)

  -- tries to add a new transactions to the submissions store
  (addSubmission
    : Slot -- expiring slot, after which 
           -- a transaction cannot make it to the ledger
    -> Tx  -- new submitted transaction
    -> Submissions -- db
    -> Submissions -- modified db
  )
      (addSubmissions_tip_and_finality_are_not_changed 
        : ∀ (x:Tx) (xs:Submissions) (expiring:Slot)
        , let xs' := addSubmission expiring x xs 
          in tip xs' = tip xs ∧ finality xs' = finality xs
      )
      (addSubmission__changes_transaction_statuses
        : ∀ (x:Tx)  (xs:Submissions) (expiring:Slot) (y:Tx)
        , let xs' := addSubmission expiring x xs 
        ,     old := status y xs
        ,     new := status y xs'
        ,     unknown := 
                old = Unknown 
                  ∧ after expiring (tip xs)
                  ∧ x = y
        in
            unknown → new = InSubmission expiring 
            ∧  
            ¬ unknown → new = old
      )

  -- moveToLedger
  -- this operation is primitive and leaving tip untouched
  -- no property is checked that the tip is not smaller than the acceptance slot 
  (moveToLedger
    : Slot -- acceptance slot for this tx
    -> Tx  -- landed transaction
    -> Submissions -- db
    -> Submissions -- modified db
  )
      (moveToLedger_tip_and_finality_are_not_changed 
        : ∀ (x:Tx) (xs:Submissions) (acceptance:Slot)
        , let xs' := moveToLedger acceptance x xs 
          in tip xs' = tip xs ∧ finality xs' = finality xs
      )
      -- take care that transaction moved here will be with acceptance after the tip
      -- in this phase the tip is staying behind marking the minimum acceptance
      (moveToLedger_changes_transaction_statuses
        : ∀ (acceptance:Slot) (xs:Submissions) (x:Tx) (expiring:Slot) (y : Tx)
        , let xs' := moveToLedger acceptance x xs 
        ,     old := status y xs
        ,     new := status y xs'
        ,     inSubmission := 
                old = InSubmission expiring 
                  ∧ after expiring acceptance  
                  ∧ after acceptance (tip xs) 
                  ∧ x = y
        in 
           inSubmission → new = InLedger expiring acceptance 
            ∧  ¬ inSubmission → new = old
      )

  -- moveTip
  (moveTip
    : Slot  -- new tip, can be in the past or in the future
    -> Submissions  -- db
    -> Submissions  -- modified db
    )
      (moveTip_changes_the_tip
        : ∀ (newTip:Slot) (xs:Submissions)
        , let xs' := moveTip newTip xs in tip xs' = newTip
      )

      (moveTip_can_change_finality
        : ∀ (x:Tx) (xs:Submissions) (newTip:Slot)
        , let xs' := moveTip newTip xs in 
          after (finality xs) newTip → finality xs' = newTip
          ∧ 
          before (finality xs) newTip → finality xs' = finality xs 
      )      

      -- InLedger effects,  
      (moveTip_changes_transaction_statuses
        : ∀ (newTip:Slot) (xs:Submissions) (y:Tx) (acceptance:Slot) (expiring:Slot)
        , let xs' := moveTip newTip xs 
        ,     old := status y xs
        ,     new := status y xs'
        ,     inSubmission := 
                old = InSubmission expiring 
                ∧ before expiring newTip 
        ,     expired := 
                old = Expired expiring 
                ∧ after expiring newTip 
        ,     inLedger := 
                old = InLedger expiring acceptance
                ∧ after acceptance newTip 
        in
         inSubmission → new = Expired expiring 
         ∧  expired → new = InSubmission expiring
         ∧  inLedger → new = InSubmission expiring 
         ∧  (¬ (inSubmission ∨ expired ∨ inLedger)) → new = old 
      )
  -- moveFinality
  (moveFinality
    : Slot -- slot in the past for which transactions
          -- cannot roll back to submission (persisted status)
          -- or roll forward to ledger (dead status)
    -> Submissions -- db
    -> Submissions -- modified db
    )
      (moveFinality_should_not_change_tip
        : ∀ (newFinality:Slot) (xs:Submissions)
        , let xs' := moveFinality newFinality xs in tip xs' = tip xs
      )

      (moveFinality_should_have_been_updated
        : ∀ (newFinality:Slot) (xs:Submissions)
        , let xs' := moveFinality newFinality xs 
          in 
            after newFinality (finality xs) ∧ before newFinality (tip xs) 
                  → finality xs' = newFinality
            ∧ before newFinality (finality xs) 
                  → finality xs' = finality xs
            ∧ after newFinality (tip xs) 
                  → finality xs' = tip xs
      )

      (moveFinality_changes_transaction_statuses
        : ∀ (newFinality:Slot) (xs:Submissions) (y:Tx) (acceptance:Slot) (expiring:Slot)
        , let xs' := moveTip newFinality xs 
        ,     old := status y xs
        ,     new := status y xs'
        ,     expired := 
                old = Expired expiring 
                  ∧ before expiring (finality xs')
        ,     inLedger := 
                old = InLedger expiring acceptance
                  ∧ before acceptance (finality xs')
        in
         inLedger → new = Unknown
         ∧  expired → new = Unknown
         ∧  (¬ (expired ∨  inLedger)) → new = old 
      )

  (forget 
    : Tx 
    -> Submissions 
    -> Submissions 
    )
      (forget_does_not_change_tip_and_finality 
        : ∀ (x:Tx) (xs:Submissions) (expiring:Slot)
        , let xs' := forget x xs 
          in tip xs' = tip xs ∧ finality xs' = finality xs
      )
      (forget_removes_a_tx_whatever_status
      : ∀ x y xs 
      , let xs' := forget x xs 
      ,     old := status y xs
      ,     new := status y xs'
      in 
        x = y → new = Unknown ∧ x ≠ y → new = old
      )
end 
