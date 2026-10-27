# Modules model

Numbering continues `specs/5350-redelegate-previous-drep/modules-model.md`
(M1 to M6).

- **M7 - `Cardano.Wallet.Network.LocalStateQuery.RewardAccount`**: a query,
  `fetchRewardAccountStates`, that, in Conway and Dijkstra, sends
  `GetFilteredDelegationsAndRewardAccounts` and `GetFilteredVoteDelegatees`
  for the same credential set in one program and returns an empty map without
  a query for an empty set; and the pure conversion of both answers to D4,
  `toRewardAccountStates`, exported for tests.
  `fetchRewardAccounts` keeps its type for `fetchRewardAccountBalances`.
  Data: D3, D4.
- **M8 - `Cardano.Wallet.Network.LocalStateQuery.DReps`**: a
  single-credential `GetDRepState` query for F8, `isDRepRegistered`, `Nothing`
  before Conway; and, exported for tests, `credToDRepID` and its inverse
  `drepIDToCred`.
- **M9 - `Cardano.Wallet.Network` and `.Implementation`**: `NetworkLayer`
  gains `getCachedVoteDelegation :: RewardAccount -> m (Maybe VoteDelegation)`
  and `isDRepRegistered :: DRepID -> m (Maybe Bool)`;
  `newRewardBalanceFetcher` caches D4; `getCachedRewardAccountBalance`
  projects `rewardBalance`; `MsgObserverLog` carries D4.
- **M10 - `Cardano.Wallet` and `Cardano.Wallet.Transaction`**: F5, F6, F10
  and `readCachedVoteDelegation`; `ErrCannotVote` gains
  `ErrDRepNotRegistered DRepID`; `assertDifferentVoting` leaves
  `Cardano.Wallet`.
- **M11 - `Cardano.Wallet.Delegation` and `Cardano.Wallet.IO.Delegation`**:
  F7 and the pure part of F8 in `Cardano.Wallet.Delegation`; the IO part of
  F8, `assertDifferentVoting`, `readVoteState`, the call of F10 in
  `quitStakePool`, and F6's calls in `selectCoinsForQuit` and
  `quitStakePool` in `Cardano.Wallet.IO.Delegation`.
- **M12 - API layer**: D5 in `Cardano.Wallet.Api.Types`; F9, F6's calls and
  `assertSelfWithdrawalVoting` in `Shelley/Server.hs`; the reworded message
  of `ErrConstructTxWithdrawalWithoutVoting` and the mapping of
  `ErrDRepNotRegistered` in `Server/Error.hs`.
- **M13 - `specifications/api/swagger.yaml`**: the schema of D5 and the
  property `ledger_vote`; the error lists, descriptions and 404 blocks of the
  plan's "API contract".
- **M14 - `specifications/Cardano/Wallet/Delegation.agda`**: D6, F11, F12 and
  the five `AGDA-5466-*` laws. D6, F11, the predicates of the laws and the
  three withdrawal laws sit between D2 and the `DuplicateVote` module; F12
  and the two vote laws sit inside it. F12 uses the module's `decideAgainst`
  and `voteDecision` and matches on D6, and an Agda module cannot be reopened,
  so D6 comes before the module.
- **M15 - `nix/delegation-agda.nix`**: one mutation per new law, and Agda run
  with `LC_ALL=C.UTF-8`, so that each error prints in full and a mutant that
  typechecks with only a warning is not counted as rejected. The comments on
  the check in `flake.nix` and `.github/workflows/ci.yml` name #5466.
- **M16 - `Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates`**:
  exports `fromLedgerDRep`, used by M7.
- **M17 - prose specifications**:
  `specifications/Cardano/Wallet/delegation.md` and
  `specifications/api/drep-voting.md` state the ledger rule, the pre-defined
  options, the script-hash exemption, the effect of a DRep retirement and the
  remedy.
- **M18 - local cluster and integration framework**:
  `Cardano.Wallet.Launch.Cluster.DRep`; its faucet HTTP operations
  `POST /drep/register-new`, `/drep/register` and `/drep/retire`, described in
  `lib/local-cluster/data/swagger.json`; the integration `Context` fields
  `_registerNewDRep`, `_registerDRep` and `_retireDRep`, also set in
  `lib/integration/exe/e2e.hs` and `lib/benchmarks/exe/latency-bench.hs`; and
  the framework helper `shouldHaveDelegationHistory`, which compares a
  delegation object's `active` and `next`.

Dependency direction stays ledger query, network cache, wallet decision, API.
M14 and M15 specify and check the decisions of M10 and M11; the mirrors in
`DelegationSpec` check the Haskell side, and neither depends on generated
output of the other. No database or persistence dependency is added.
