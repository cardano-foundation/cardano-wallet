import .primitives

namespace Cardano.Wallet.Submissions.Operations
section

parameter Slot : Type

def Status : Type := Cardano.Wallet.Submissions.Primitives.Status Slot
open Cardano.Wallet.Submissions.Primitives.Status

structure Submissions :=
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

  (Tx : Type)
  (Submissions : Type)

  (status : Tx -> Submissions -> Status)
  (tip : Submissions -> Slot)
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
end Cardano.Wallet.Submissions.Operations