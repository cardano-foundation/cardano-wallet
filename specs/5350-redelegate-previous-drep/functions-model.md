# Functions model

- **F1 — `joinDRepVotingAction era targetDRep delegation stakeKeyIsRegistered`**
  - Inputs: recent era, requested `DRep`, `WalletDelegation`, registration flag.
  - Result: `Either ErrCannotVote VotingAction`.
  - Constraint: reject only when `targetDRep` matches D1 `effective`; preserve
    existing era and registration behavior.
- **F2 — `voteAction context targetDRep`**
  - Inputs: IO wallet layer and requested `DRep`.
  - Result: `IO (VotingAction, VoteRequest)`.
  - Constraint: derive its duplicate verdict from D1 `effective`, matching F1.
- **F3 — Agda `effectiveDelegationStatus delegation`**
  - Inputs: D1 projected delegation (`active`, ordered `next`).
  - Result: formal delegation status.
  - Constraint: return `active` exactly when `next` is empty; otherwise return
    the status of the final `next` entry.
- **F4 — Agda `voteDecision target delegation`**
  - Inputs: requested formal DRep and D1 projected delegation.
  - Result: D2 duplicate-vote observation.
  - Constraint: derive the result only from F3; history cannot affect it.

## Formal-law to QuickCheck mapping

The implementation may choose Agda-compatible identifier spelling, but the
stable law IDs and one-to-one QuickCheck mapping are mandatory:

| Formal law | Meaning | QuickCheck mirror |
|---|---|---|
| `AGDA-5350-EMPTY` | empty `next` selects `active` | `prop_effectiveDelegationStatusEmpty` |
| `AGDA-5350-LAST` | non-empty `next` selects its final status | `prop_effectiveDelegationStatusLast` |
| `AGDA-5350-HISTORY` | changing only superseded history preserves the decision | `prop_voteDecisionIgnoresHistory` |
| `AGDA-5350-SAME` | target equal to effective DRep is rejected; a different target is accepted | `prop_joinDRepVotingActionEffective` |

`prop_joinDRepParityWithVoteRequest` remains the executable parity property
between F1 and the shared verdict consumed by F2. Each property must be
registered in the focused test tree, not merely defined.

The formal backend's `eq-refl` and `eq-sound` parameters are explicit model
assumptions, not additional `AGDA-5350-*` laws. They map together to the
registered `prop_drepEqualityMatchesStructure` property, which compares the
Haskell `Eq DRep` instance with a structural oracle and covers reflexivity.
