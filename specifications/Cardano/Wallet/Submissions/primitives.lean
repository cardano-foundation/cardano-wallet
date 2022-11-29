namespace Cardano.Wallet.Submissions.Primitives
section

/-- `Slot` represents time. -/
parameter Slot : Type

/--
Database that tracks the lifecycle of transactions that have been
submitted to the distributed ledger.
-/
parameter Submissions : Type

/--
Submission `Status` of a transaction.

We choose to track the expiring `Slot` of the transaction
as part of the `Status` for clarity, even though this `Slot`
is part of the transaction data.
-/
inductive Status : Type
    -- The transaction has been submitted but is not yet in the ledger.
    | InSubmission
      : Slot -- expiring
      -> Status
    -- The transaction is in the ledger but can still be rolled back.
    | InLedger
        : Slot -- expiring
        -> Slot -- acceptance
        -> Status
    -- The transaction has expired but could reappear in case of a rollback.
    | Expired
      : Slot -- expiring
      -> Status
    -- The transaction is not known to the submission database.
    | Unknown : Status
open Status


structure Submissions :=
  -- Computations on `Slot`.
  (before
    : Slot -- slot that is before or equal to
    -> Slot -- reference slot
    -> Prop
  )

  (after
    : Slot -- slot that strictly after
    -> Slot -- reference slot
    -> Prop
  )

  -- A transaction.
  (Tx : Type)

  -- Submission status of a `Tx` as tracked by the database.
  (status : Tx -> Submissions -> Status)

  -- Tip `Slot` of the database.
  --
  -- The tip separates the transactions that are `InLedger` or `Expired`
  -- from the transactions that are still `InSubmission`.
  (tip : Submissions -> Slot)

  -- Finality `Slot` of the database.
  --
  -- The finality slot represents the last slot after which
  -- transactions are still tracked; older transactions are pruned.
  (finality : Submissions -> Slot)

  -- Tries to add a new transaction to the submissions store.
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

  -- Move a `Tx` into to the ledger.
  --
  -- This operation is primitive — it leaves the tip untouched,
  -- and will only change the transaction status if the `acceptance` slot
  -- is `after` the `tip` of the database.
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

  -- Move the `tip` of the submission database.
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

  -- Move the `finality` of the submission database.
  (moveFinality
    : Slot  -- slot in the past for which transactions
            -- cannot roll back from InLedger to InSubmission (persisted)
            -- or roll back from Expired to InSubmission (dead)
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

  -- Stop tracking a transaction in the database.
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
end Cardano.Wallet.Submissions.Primitives
