# Tasks: Ledger vote delegation for withdrawal and vote checks

**Input**: [spec.md](spec.md), [plan.md](plan.md),
[data-model.md](data-model.md), [functions-model.md](functions-model.md),
[modules-model.md](modules-model.md)
**Tests**: required by the specification (FR-016 to FR-019).

Format: `[ID] [P?] [Story] Description`. `[P]` marks a task that touches
files no other open task touches. Story labels: US1 report the ledger's vote
delegation, US2 refuse a self-withdrawal the ledger would refuse, US3 decide a
repeated vote from the ledger.

## Slice S0: red evidence

- [x] **T001** [P] Add `Cardano.Wallet.Launch.Cluster.DRep` in
  `lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/DRep.hs`: generate
  DRep keys, register a DRep, register it again after retirement, and retire
  it, each with a faucet input.
- [x] **T002** Expose DRep registration and retirement through the faucet
  HTTP service
  (`lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/Http/Faucet/` and
  its OpenAPI golden file `lib/local-cluster/data/swagger.json`) and the
  integration `Context`
  (`lib/integration/framework/Test/Integration/Framework/Context.hs`,
  `Setup.hs`, and the other `Context` values in `lib/integration/exe/e2e.hs`
  and `lib/benchmarks/exe/latency-bench.hs`).
- [x] **T003** [US1] [US2] [US3] Write the retired-DRep scenarios of FR-019 in
  `lib/integration/scenarios/Test/Integration/Scenario/API/Voting.hs`, using
  the `rewardWallet` fixture voted to a registered DRep A before A retires,
  and record that they fail on the base commit.
- [x] **T004** [US2] Establish whether shared-wallet pool delegation runs on
  the Conway cluster. If it does, write the shared-wallet scenario of FR-019 in
  `lib/integration/scenarios/Test/Integration/Scenario/API/Shared/Transactions.hs`
  and record that it fails on the base commit; if it does not, record why and
  cover User Story 2 scenario 9 with unit tests in T021.

  It does not run: a shared wallet's join is refused at submission with 403
  `missing_witnesses_in_transaction`. User Story 2
  scenario 9 rests on the unit tests of T021.

**Checkpoint**: the red evidence of INV-5466-PROOF exists.

## Slice S1: ledger vote delegation (US1)

- [x] **T005** [P] [US1] Add `VoteDelegation` to
  `lib/primitive/lib/Cardano/Wallet/Primitive/Types/DRep.hs` and export
  `fromLedgerDRep` from
  `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/Certificates.hs`.
- [x] **T006** [US1] In
  `lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery/RewardAccount.hs`,
  add the Conway and Dijkstra query of M7 with its empty-set guard and the
  pure conversion to `RewardAccountState`; re-export from
  `LocalStateQuery.hs`.
- [x] **T007** [US1] In `lib/network-layer/src/Cardano/Wallet/Network.hs` and
  `Network/Implementation.hs`, cache `RewardAccountState` in
  `newRewardBalanceFetcher`, add `getCachedVoteDelegation`, and keep
  `getCachedRewardAccountBalance` and `fetchRewardAccountBalances` answering
  as today.
- [x] **T008** [P] [US1] Unit tests for the conversion of T006: absent
  account, `Abstain`, `No Confidence`, key-hash and script-hash DReps, eras
  before Conway, one entry with both values per requested account, and no
  query for an empty set.
- [x] **T009** [US1] Add `ApiWalletLedgerVote` with its JSON instances and the
  `ledgerVote` field of `ApiWalletDelegation` in
  `lib/api/src/Cardano/Wallet/Api/Types.hs`; fill it in
  `toApiWalletDelegation` and its three callers in
  `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`.
- [x] **T010** [US1] Define `ApiWalletLedgerVote` as a `oneOf` of its two
  shapes and the `ledger_vote` property in `specifications/api/swagger.yaml`.
- [x] **T011** [P] [US1] Extend
  `lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs` (Arbitrary over both
  constructors, `jsonTest`, rejection of the two invalid combinations,
  `ToSchema`) and regenerate the affected golden files under
  `lib/unit/test/data/Cardano/Wallet/Api/`.
- [x] **T012** [P] [US1] Add the new `NetworkLayer` fields to
  `dummyNetworkLayer` in
  `lib/unit/test-common/Cardano/Wallet/DummyTarget/Primitive/Types.hs`.

**Checkpoint**: `GET /v2/wallets/{walletId}` reports `delegation.ledger_vote`;
no decision has changed.

## Slice S2: withdrawal gate (US2)

- [x] **T013** [US2] Add `withdrawalPermitted` (F5) to
  `lib/wallet/src/Cardano/Wallet.hs` and make `assertIsVoting` (F6) decide
  through it.
- [x] **T014** [US2] Pass the network layer and protocol parameters to the
  existing callers of `assertIsVoting`: `postTransactionOld`,
  `constructTransaction` and `constructUnbalancedSharedTransaction`.
- [x] **T015** [US2] Call F6 in `postTransactionFeeOld` and `selectCoins`
  (`Shelley/Server.hs`) after the withdrawal is computed.
- [x] **T016** [US2] In `lib/wallet/src/Cardano/Wallet/IO/Delegation.hs`,
  build `NoWithdrawal` in `quitStakePool` when the cached balance is exactly
  zero (F10), and call F6 in `selectCoinsForQuit` and `quitStakePool` after
  `quitStakePoolDelegationAction`, rethrowing as `ExceptionConstructTx`.

  F10 is `quitWithdrawal`, a pure function exported from `Cardano.Wallet`,
  which `quitStakePool` calls with the cached balance.
- [x] **T017** [US2] Call F6 in `createMigrationPlan` and `migrateWallet`
  (`Shelley/Server.hs`) when the reward withdrawal is `WithdrawalSelf`.

  Both call F6 through `assertSelfWithdrawalVoting` in `Shelley/Server.hs`,
  which reads the protocol parameters of the node tip, before the plan is
  built; F6 passes every withdrawal that is not `WithdrawalSelf`.
- [x] **T018** [US2] Reword the message of
  `ErrConstructTxWithdrawalWithoutVoting` in
  `lib/api/src/Cardano/Wallet/Api/Http/Server/Error.hs` and the description
  of `errWithdrawalNotPossibleWithoutVote` in
  `specifications/api/swagger.yaml` to name the cause and the remedy.
- [x] **T019** [US2] Add `errWithdrawalNotPossibleWithoutVote` to the six
  response anchors of the plan's "API contract", turning the 403 of
  `responsesCreateWalletMigrationPlan` into a `oneOf`.
- [x] **T020** [US2] Update `specifications/Cardano/Wallet/delegation.md`
  (lines 12 and 93) and `specifications/api/drep-voting.md` (line 29) as M17
  states.
- [x] **T021** [P] [US2] Unit tests: `withdrawalPermitted` examples in
  `lib/unit/test/unit/Cardano/Wallet/DelegationSpec.hs`; `assertIsVoting`
  with a stubbed ledger value, for a key-hash and a script-hash reward
  account, and the zero-balance quit in
  `lib/unit/test/unit/Cardano/WalletSpec.hs`.

  The zero-balance quit is tested on `quitWithdrawal` (F10), not through
  `quitStakePool`.

**Checkpoint**: every self-withdrawal path decides from the ledger value, and
a zero-balance quit builds no withdrawal.

## Slice S3: vote decision (US3)

- [x] **T022** [US3] Add the single-credential `GetDRepState` query to
  `lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery/DReps.hs`,
  the `isDRepRegistered` field to `NetworkLayer`, and `ErrDRepNotRegistered`
  to `ErrCannotVote` in `lib/wallet/src/Cardano/Wallet/Transaction.hs`.
- [x] **T023** [US3] Add the registration check (F8) to
  `joinDRepVotingAction` and `voteAction` (`IO/Delegation.hs`), before the
  duplicate-vote check; map `ErrDRepNotRegistered` to 404 `not_found` in
  `Server/Error.hs`.

  F8 is the pure `guardDRepRegistered` in `Cardano.Wallet.Delegation` and the
  IO `assertDRepRegistered` in `IO/Delegation.hs`, which both paths call
  before they read the ledger value.
- [x] **T024** [US3] Add `ledgerVoteRequestFor` (F7) to
  `lib/wallet/src/Cardano/Wallet/Delegation.hs` and give
  `joinDRepVotingAction` the ledger value.
- [x] **T025** [US3] Read the ledger value in `joinDRepVotingAction` and
  `voteAction` (`IO/Delegation.hs`) and in `assertDifferentVoting`
  (`Wallet.hs`), all through F7.

  `assertDifferentVoting` moves to `IO/Delegation.hs` and takes the
  `WalletLayer`, because `Cardano.Wallet.Delegation`, which defines F7,
  imports `Cardano.Wallet`.
- [x] **T026** [US3] In `specifications/api/swagger.yaml`, add `errNotFound` to
  the 404 of `responsesJoinDRep`, give `responsesConstructTransaction` an
  explicit 404 block with `errNoSuchWallet`, `errWalletNotInitialized` and
  `errNotFound`, and restate the description of `errSameVote` (FR-013).
- [x] **T027** [P] [US3] Unit tests in `DelegationSpec.hs`: a delegation made
  outside the wallet, a retired DRep, a re-registered DRep, an inactive DRep,
  `Abstain` and `No Confidence`; the `AGDA-5350-*` mirrors calling
  `joinDRepVotingAction` with the value unread; and
  `prop_joinDRepParityWithVoteRequest` over generated ledger values.

**Checkpoint**: a retired target is refused with 404 before submission, and
`same_vote` follows the ledger.

## Slice S4: formal backend and mirrors

- [x] **T028** Extend `specifications/Cardano/Wallet/Delegation.agda` with D6,
  F11 and F12, and prove the five `AGDA-5466-*` laws of `functions-model.md`
  without postulates standing in for them.
- [x] **T029** Add the five mutations of `functions-model.md` to
  `nix/delegation-agda.nix` and confirm that
  `checks.x86_64-linux.delegation-agda` rejects each.

  The check also runs Agda with `LC_ALL=C.UTF-8`, so that each error prints
  in full and a mutant that typechecks with only a warning is not counted as
  rejected; the comments in `flake.nix` and `.github/workflows/ci.yml` name
  #5466.
- [x] **T030** Register the five mirrors in `DelegationSpec.hs` with
  `checkCoverage` classes over the cases that each law distinguishes within
  its scope.

  Classes over the phase and the credential kind exist only in the
  WDRL-EXEMPT mirror, the one law whose scope lets them vary; each mirror's
  classes are listed in `functions-model.md`.
- [x] **T031** Register the five negative controls of `functions-model.md`,
  each running its mirror's property against the named mutant under
  `expectFailure`.

## Slice S5: integration

- [x] **T032** Add a framework helper that compares a delegation object's
  `active` and `next` only, and use it in the 78 whole-object delegation
  assertions: 74 in the API scenarios, 3 through
  `expectCliField` and `expectCliListField` in
  `lib/integration/scenarios/Test/Integration/Scenario/CLI/Shelley/Wallets.hs`,
  and 1 at `lib/integration/framework/Test/Integration/Framework/DSL.hs:4311`.

  Done in S1, in the commit
  `test(integration): compare delegation history in assertions`, before the
  field reaches the delegation object.
- [x] **T033** Run T003 and T004 green on a local cluster.

  `VOTING_02a`, `VOTING_02b` and `VOTING_02c` pass from S3 on. T004 has no
  scenario, so the shared-wallet case rests on the unit tests of T021.
- [x] **T034** Run again on a local cluster the quit fee-boundary scenarios
  `STAKE_POOLS_QUIT_01x`
  (`lib/integration/scenarios/Test/Integration/Scenario/API/Shelley/StakePools.hs:1363-1480`)
  and the existing `VOTING_*`, `STAKE_POOLS_*` and `TRANS_NEW_JOIN_*`
  scenarios, and adjust the fee-boundary amounts if the quit without a
  zero-amount withdrawal moves a boundary.

  Covered by the S2 regression run (every `STAKE_POOLS_*` scenario,
  `STAKE_POOLS_QUIT_01x` among them, `VOTING_01` and every `TRANS_NEW_JOIN_*`
  scenario) and the S3 run of every `VOTING_*` scenario; no fee-boundary
  amount needed a change.

## Slice S6: verification

- [ ] **T035** Run `just fmt`, HLint, the unit-test target, the integration
  scenarios of T033 and T034 and `checks.x86_64-linux.delegation-agda`, and
  record the RED-to-GREEN evidence of INV-5466-PROOF in the pull request.

## Dependencies

- T001 and T002 come first; T003 and T004 need them and run against the base
  commit.
- S1 precedes S2 and S3. S4 depends on S2 and S3. T032 runs in S1, before
  the field reaches the delegation object; the scenarios of S5 depend on S2
  and S3 for their assertions.
