# Functions model

Numbering continues `specs/5350-redelegate-previous-drep/functions-model.md`
(F1 to F4).

- **F5 - `withdrawalPermitted bootstrap rewardAccount ledger history`**: pure,
  exported from `Cardano.Wallet`, of type
  `Bool -> RewardAccount -> Maybe VoteDelegation -> Maybe DRep -> Bool`.
  - Inputs: bootstrap flag, `RewardAccount`, D3 as `Maybe VoteDelegation`, the
    history's `Maybe DRep`.
  - Result: `Bool`, decided in this order: `True` when `bootstrap`; `True` for
    `FromScriptHash`; with `Just v`, `v /= VoteNotDelegated`; with `Nothing`,
    `isJust history`.
- **F6 - `assertIsVoting`**: IO, in `Cardano.Wallet`. Takes the network
  layer, the wallet's `DBLayer`, the era, the protocol parameters and the
  `Withdrawal`, and runs in `ExceptT ErrConstructTx IO`.
  - Acts only on a `WithdrawalSelf` in Conway. `NoWithdrawal` and a
    `WithdrawalExternal` named by a recovery phrase pass, so its callers do
    not test `containsSelfWithdrawal`.
  - Reads the bootstrap flag with
    `hardforkConwayBootstrapPhase (pp ^. ppProtocolVersionL)`. When neither
    the bootstrap phase nor a `FromScriptHash` account decides, it reads D3
    with `getCachedVoteDelegation` for the withdrawal's reward account and
    the history with `getCurrentVoting`, at the time of the call, and applies
    F5.
  - Throws `ErrConstructTxWithdrawalWithoutVoting` when F5 refuses. IO callers
    rethrow it as `ExceptionConstructTx`.
  - Called at the three existing sites and at `postTransactionFeeOld`,
    `selectCoins`, `selectCoinsForQuit`, `quitStakePool`,
    `createMigrationPlan` and `migrateWallet`; on the two quit paths, after
    `quitStakePoolDelegationAction`; on the two migration paths, through
    `assertSelfWithdrawalVoting` in `Shelley/Server.hs`, which reads the
    protocol parameters of the node tip, before the migration plan is built.
- **F7 - `ledgerVoteRequestFor target ledger delegation`**: pure, in
  `Cardano.Wallet.Delegation`, of type
  `DRep -> Maybe VoteDelegation -> WalletDelegation -> VoteRequest`.
  - With `Just (VoteDelegatedTo d)`, `VotedSameAsBefore` exactly when
    `d == target`; with `Just VoteNotDelegated`, `VotedDifferently`; with
    `Nothing`, F1's rule through `voteRequestFor target delegation`.
  - The pure
    `joinDRepVotingAction era target ledger delegation stakeKeyIsRegistered`
    takes the ledger value and uses F7; `voteAction` and
    `assertDifferentVoting` use F7.
  - The IO paths read the ledger value with `readCachedVoteDelegation`, in
    `Cardano.Wallet`: the value for the reward account of a Shelley wallet or
    of a shared wallet's delegation template, and `Nothing` for a wallet
    without a reward account. `readVoteState`, in
    `Cardano.Wallet.IO.Delegation`, reads it with the history and the
    stake-key registration. The IO `joinDRepVotingAction`, `voteAction`,
    `assertDifferentVoting` and `handleDelegationVoteRequest` gain a
    `WalletFlavor s` constraint for that read.
  - `assertDifferentVoting` lives in `Cardano.Wallet.IO.Delegation` and takes
    the `WalletLayer`: `Cardano.Wallet.Delegation`, which defines F7, imports
    `Cardano.Wallet`, so `Cardano.Wallet` cannot call F7.
- **F8 - DRep registration check**: the pure
  `guardDRepRegistered bootstrap target registered` in
  `Cardano.Wallet.Delegation`, and the IO `assertDRepRegistered` in
  `Cardano.Wallet.IO.Delegation`, which `IODeleg.joinDRepVotingAction` and
  `voteAction` call before they read the ledger value for F7.
  - Outside the bootstrap phase, a `FromDRepID` target that `isDRepRegistered`
    reports as `Just False` throws
    `ExceptionVoting (ErrDRepNotRegistered drep)`, mapped to 404 `not_found`.
    `Nothing` does not refuse.
  - The IO part queries `isDRepRegistered` only for a `FromDRepID` target
    outside the bootstrap phase.
  - `isDRepRegistered` sends `GetDRepState` with the target's credential
    only; the answer includes the DRep's set of delegators. It is not cached,
    and it reads neither the DRep layer behind `GET /v2/dreps/{drepId}`, which
    serves a DRep list cached for 15 minutes
    (`lib/wallet/src/Cardano/Wallet/DRep/Layer.hs:77-78,113-125`), nor the
    stake-pool database. It runs on the network layer's shared query queue,
    so while the node does not answer, the vote delegation request waits,
    with no timeout, as every other ledger query does. It returns `Nothing`
    only before Conway.
- **F9 - `toApiWalletDelegation`**: takes D3 and fills D5. Called from
  `mkShelleyWallet`, `mkSharedWallet` and `listStakeKeys`, each reading
  `getCachedVoteDelegation` for the wallet's reward account; absent for a
  shared wallet without a delegation template.
- **F10 - `quitWithdrawal rewardAccount derivationPath balance`**: pure,
  exported from `Cardano.Wallet`, called by `IODeleg.quitStakePool` with the
  cached balance: `NoWithdrawal` when the balance is exactly zero, otherwise
  `WithdrawalSelf` with that balance.
- **F11 - Agda `withdrawalDecision phase credKind ledgerVote history`**: D6 to
  `Bool`, the formal counterpart of F5. Its five clauses follow F5's four
  guards in order; F5's third guard, `v /= VoteNotDelegated` for a read
  value, is two clauses, one for `NoDelegation` and one for `DelegatedTo`.
- **F12 - Agda `ledgerVoteDecision target ledgerVote delegation`**: inside the
  existing `DuplicateVote` module; `DelegatedTo d` gives
  `decideAgainst target (Just d)`, `NoDelegation` gives `DifferentVote`, and
  `Unread` gives F4's `voteDecision target delegation`.
  `decideAgainst target (Just d)` compares `target == d`, while F7 compares
  `d == target`; the two agree because `Eq DRep` agrees with structural
  identity (INV-5350-DREP-EQ), which is symmetric.

## Formal-law to QuickCheck mapping

| Formal law | Meaning | QuickCheck mirror |
|---|---|---|
| `AGDA-5466-WDRL-LEDGER` | `PostBootstrap`, `KeyHashCred`, a read value: permitted exactly when the value is `DelegatedTo`, for every history | `prop_withdrawalPermittedFollowsLedger` |
| `AGDA-5466-WDRL-EXEMPT` | `Bootstrap` with any credential kind, or `ScriptHashCred` in any phase: permitted, for every value and history | `prop_withdrawalPermittedExempt` |
| `AGDA-5466-WDRL-UNREAD` | `PostBootstrap`, `KeyHashCred`, `Unread`: permitted exactly when the history holds a vote | `prop_withdrawalPermittedUnreadFollowsHistory` |
| `AGDA-5466-VOTE-LEDGER` | a read value: `SameVote` exactly when the value is `DelegatedTo` the target, for every history | `prop_ledgerVoteRequestFollowsLedger` |
| `AGDA-5466-VOTE-UNREAD` | `Unread`: the decision is F4's | `prop_ledgerVoteRequestUnreadIs5350` |

The three withdrawal laws partition the inputs: EXEMPT covers `Bootstrap` and
`ScriptHashCred`, and the other two cover `PostBootstrap` with `KeyHashCred`,
split on whether the value is read. The two `LEDGER` laws quantify over the
history, so a decision that consults the history when the value is read
breaks them.

The laws state their conditions through predicates defined independently of
F11 and F12: `IsRead v` holds for `NoDelegation` and `DelegatedTo d`,
`IsDelegated v` for `DelegatedTo d`, `IsExempt p c` for `Bootstrap` with
either credential kind and for `ScriptHashCred` in either phase, and
`HoldsVote h` for `Just d`. The model's prelude gains `isJust` and
`just-injective`. The statements, with the binders' types left out:

```agda
AGDA-5466-WDRL-LEDGER : ∀ v history → IsRead v
  → (IsDelegated v
       → withdrawalDecision PostBootstrap KeyHashCred v history ≡ True)
    ⋀ ((IsDelegated v → Empty)
       → withdrawalDecision PostBootstrap KeyHashCred v history ≡ False)

AGDA-5466-WDRL-EXEMPT : ∀ p c v history → IsExempt p c
  → withdrawalDecision p c v history ≡ True

AGDA-5466-WDRL-UNREAD : ∀ history
  → (HoldsVote history
       → withdrawalDecision PostBootstrap KeyHashCred Unread history ≡ True)
    ⋀ ((HoldsVote history → Empty)
       → withdrawalDecision PostBootstrap KeyHashCred Unread history ≡ False)

AGDA-5466-VOTE-LEDGER : ∀ t v d → IsRead v
  → (v ≡ DelegatedTo t → ledgerVoteDecision t v d ≡ SameVote)
    ⋀ ((v ≡ DelegatedTo t → Empty) → ledgerVoteDecision t v d ≡ DifferentVote)

AGDA-5466-VOTE-UNREAD : ∀ t d → ledgerVoteDecision t Unread d ≡ voteDecision t d
```

Each mirror takes the function under test as a parameter, so that the
registered mirror runs on F5 or F7 and its negative control on the mutant. It
compares that function with an oracle built from the generated values. It
carries `checkCoverage` classes over the cases that its law distinguishes
within its scope, including the inputs on which the law and its mutant
disagree:

| Mirror | Coverage classes |
|---|---|
| `prop_withdrawalPermittedFollowsLedger` | no vote delegation; a vote delegation; a vote that the history records and the ledger does not hold; a vote that the ledger holds and the history does not record |
| `prop_withdrawalPermittedExempt` | the bootstrap phase with a key-hash account; the bootstrap phase with a script-hash account; a script-hash account after the bootstrap phase; an unread value; no vote delegation; a vote delegation; a script-hash account with no vote in the ledger or the history |
| `prop_withdrawalPermittedUnreadFollowsHistory` | a history that records a vote; a history that records none |
| `prop_ledgerVoteRequestFollowsLedger` | no vote delegation; a delegation to the target; a delegation to another DRep; the history votes for the target and the ledger does not; the ledger votes for the target and the history does not |
| `prop_ledgerVoteRequestUnreadIs5350` | the effective status votes for the target; it does not; only a superseded status votes for the target |

Only the WDRL-EXEMPT mirror has classes over the phase and the credential
kind: WDRL-LEDGER and WDRL-UNREAD fix both by their scope, and F7 takes
neither. The two UNREAD mirrors fix the ledger value as unread.

## Negative controls

Each control runs the mirror's property against a named mutant under
`expectFailure`, so the control fails if the property cannot tell the mutant
from F5 or F7.

| Mirror | Mutant | Input that separates them |
|---|---|---|
| `prop_withdrawalPermittedFollowsLedger` | `historyOnlyWithdrawalPermitted`, today's rule: permitted exactly when the history holds a vote | a retired DRep: history names DRep A, value read as `NoDelegation` |
| `prop_withdrawalPermittedExempt` | `noExemptionWithdrawalPermitted`, F5 without the script-hash and bootstrap clauses | a script-hash account with no vote in the ledger or the history |
| `prop_withdrawalPermittedUnreadFollowsHistory` | `unreadIgnoresHistory`, F5 that permits every unread value | `Unread` with no recorded vote |
| `prop_ledgerVoteRequestFollowsLedger` | `historyVoteRequestFor`, F1's history rule `voteRequestFor` in the shape of F7, with the ledger value ignored | history names DRep A, value read as `NoDelegation`, target DRep A |
| `prop_ledgerVoteRequestUnreadIs5350` | `unreadAlwaysDifferent`, F7 that answers `VotedDifferently` for every unread value | `Unread`, effective history DRep A, target DRep A |

`prop_joinDRepParityWithVoteRequest` extends to generated ledger values and
remains the parity property between the pure and IO verdicts. The
`AGDA-5350-*` mirrors call `joinDRepVotingAction` with the value unread.

## Agda mutations

`nix/delegation-agda.nix` gains one mutation per law, each on one anchor line:
the `NoDelegation` clause of F11 returns permitted (WDRL-LEDGER); the
`Bootstrap` clause of F11 returns refused (WDRL-EXEMPT); the `Unread` clause of
F11 returns permitted whatever the history (WDRL-UNREAD); the `DelegatedTo`
clause of F12 returns F4's decision from the history (VOTE-LEDGER); the
`Unread` clause of F12 returns `DifferentVote` (VOTE-UNREAD). With the four
`AGDA-5350-*` mutations, the check requires Agda to reject nine mutants.
