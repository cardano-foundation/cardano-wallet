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
