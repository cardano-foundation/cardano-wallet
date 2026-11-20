# Implementation Plan: Ledger vote delegation for withdrawal and vote checks

**Issue**: #5466 | **Date**: 2026-10-26 | **Spec**: [spec.md](spec.md)
**Base**: `master` at `2fc5c383c`

## Summary

Read each observed reward account's vote delegation with
`GetFilteredVoteDelegatees` in the per-tip acquisition that already reads its
reward balance, keep both in one entry of the network layer's reward-account
cache, and report the value as `delegation.ledger_vote`. Make two wallet
decisions read it: the self-withdrawal check (`assertIsVoting`), also at the
endpoints that build a self-withdrawal without calling it, and the
duplicate-vote check. Before the duplicate-vote check, query the ledger for
the vote target's registration. Make the software quit build no withdrawal at
a zero balance, as the other quit paths do. Extend the Agda delegation model
with the two ledger-aware decisions and mirror each law with a QuickCheck
property.

## Technical Context

**Language/Version**: Haskell, GHC 9.12.3 (`nix/haskell.nix:122`)
**Primary Dependencies**: ouroboros-consensus 4.2.1.0,
cardano-ledger-conway 1.23.0.0 and cardano-ledger-dijkstra 0.3.0.0, all
already pinned (`cabal.project:189,200,205`)
**Storage**: none added; the value lives in the network layer's in-memory
reward-account cache
**Testing**: hspec and QuickCheck unit tests, local-cluster integration tests
in Conway, `checks.x86_64-linux.delegation-agda`
**Target Platform**: unchanged
**Project Type**: library and REST API
**Performance Goals**: one extra query per node tip over the observed reward
accounts, inside the existing acquisition. One single-credential
`GetDRepState` query per vote delegation request to a DRep credential outside
the bootstrap phase, not cached; its answer carries the target DRep's set of
delegators (`drepDelegs` in the `DRepState` encoding, cardano-ledger-core
1.21.0.0 `DRep.hs:192-199`), so it grows with that DRep's delegator count.
Both queries run on the queue that also serves `listDReps`,
`stakeDistribution` and `getUTxOByTxIn`
(`lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs:526-538`).
**Constraints**: no database migration; API changes as listed in the spec's
"API compatibility"
**Scale/Scope**: one reward account per Shelley wallet and per shared wallet
with a delegation script template

## Constitution Check

- **I. Maintenance-first**: the change fixes transactions the node rejects
  and the input lock that follows from them on `buildSignSubmitTransaction`
  paths. The database is untouched, and every API change is listed. PASS.
- **II. Era-aware design**: the query runs only in Conway and Dijkstra through
  `onAnyEra`; the checks keep the Conway-only scope they have today, which is
  correct because Dijkstra has no withdrawal rule; the bootstrap exemption
  uses the ledger's own `hardforkConwayBootstrapPhase`. PASS.
- **III. Type safety**: the ledger value is a sum type, not a nested `Maybe`,
  in the wallet and in the API type, so the API type cannot represent a
  `delegated` value without a DRep; new public functions carry Haddock and
  explicit exports. PASS.
- **IV. Formal specification**: `specifications/api/swagger.yaml`,
  `specifications/Cardano/Wallet/Delegation.agda`,
  `specifications/Cardano/Wallet/delegation.md` and
  `specifications/api/drep-voting.md` are updated with the code that conforms
  to them. The delegation formalism is Agda, as for
  `specs/5350-redelegate-previous-drep`; no Lean project is introduced. The
  constitution does not define a breaking API change, so the spec lists every
  response that changes. PASS.
- **V. Reproducible builds**: no dependency or `cabal.project` change. PASS.
- **VI. Comprehensive testing**: unit tests, QuickCheck mirrors with negative
  controls, and local-cluster scenarios in Conway. PASS.
- **VII. Code quality gates**: fourmolu, HLint, `-Wall`, unit and integration
  tests, CI. PASS.

## Constraints

- One pull request from `master`, Conventional Commits, one concern each.
- Preserve the wallet database schema and the meaning of every existing API
  field and error code.
- Keep the established delegation formalism: extend
  `specifications/Cardano/Wallet/Delegation.agda` and
  `nix/delegation-agda.nix`; do not claim Agda-to-Haskell code generation.
- `just fmt` before every push; no reformatting beyond the change.

## Design

The data, functions and modules models are in
[data-model.md](data-model.md), [functions-model.md](functions-model.md) and
[modules-model.md](modules-model.md). Their numbering continues the model of
`specs/5350-redelegate-previous-drep` (D1 and D2, F1 to F4, M1 to M6), because
both specify decisions over the same delegation state and the same Agda
module.

### Ledger and wallet code the design rests on

- `GetFilteredVoteDelegatees :: Set (Credential Staking) -> VoteDelegatees`,
  `VoteDelegatees = Map (Credential Staking) DRep`, Conway and later
  (ouroboros-consensus 4.2.1.0,
  `ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:125,309-317`),
  answered by `SL.queryDRepDelegatees` (`:464-465`) and supported from
  `ShelleyNodeToClientVersion8` (`:570`). The wallet offers only
  `NodeToClientV_16` to `NodeToClientV_22`
  (`lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Shelley.hs:401-402`),
  and `NodeToClientV_16` already maps to `ShelleyNodeToClientVersion8` in
  every Shelley-based era (ouroboros-consensus 4.2.1.0,
  `Ouroboros/Consensus/Cardano/Node.hs:296-311,446`), so the query is always
  supported.
- `queryDRepDelegatees` returns `dRepDelegationAccountStateL` for each
  requested credential and omits credentials with no delegation; an empty set
  returns every account (cardano-ledger-api 1.14.0.0,
  `Cardano/Ledger/Api/State/Query/Account.hs:36-51`). The ledger's withdrawal
  check reads the same field (cardano-ledger-conway 1.23.0.0,
  `Rules/Ledger.hs:483-485`). Dijkstra removes the check
  (cardano-ledger-dijkstra 0.3.0.0, `Rules/Ledger.hs:500`).
- One `SomeLSQ` command runs in one acquired ledger state
  (`lib/network-layer/src/Cardano/Wallet/Network/Implementation/Ouroboros.hs:705-762`).
  A command stays on the queue until its answer is processed and is sent again
  after a dropped connection, and `send` waits for the answer
  (`Ouroboros.hs:765-780,867-872`): a read waits, it does not fail.
- The reward balance is read by `newRewardBalanceFetcher`
  (`Network/Implementation.hs:1053-1073`) over the accounts observed through
  `getCachedRewardAccountBalance` (`:697-699`), with
  `GetFilteredDelegationsAndRewardAccounts`
  (`Network/LocalStateQuery/RewardAccount.hs:62-99`). Its `fetch` always
  returns `Just` (`Implementation.hs:1065-1073`) and runs on a tip change,
  without a guarantee to see every tip (`observeForever`,
  `Implementation.hs:1333-1343`). The per-wallet worker observes the account on
  each tip (`lib/wallet/src/Cardano/Wallet.hs:1822-1835`).
- `assertIsVoting` reads only `getCurrentVoting`, the latest entry of the
  wallet's own certificate history
  (`lib/wallet/src/Cardano/Wallet.hs:3584-3592,3621-3627`;
  `lib/wallet/src/Cardano/Wallet/DB/Store/Delegations/Layer.hs:67-74`), whose
  DRep equals the DRep of the effective status that `readDelegation` and
  `effectiveDelegationStatus` derive (`Layer.hs:108-150`). It acts only in
  Conway and is called from `postTransactionOld`
  (`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs:2638-2640`),
  `constructTransaction` (`Wallet.hs:3440-3442`) and
  `constructUnbalancedSharedTransaction` (`Wallet.hs:3468-3470`).
- These paths build a self-withdrawal without calling it:
  `postTransactionFeeOld` (`Server.hs:2840-2848`), `selectCoins`
  (`Server.hs:2224-2232`), `IODeleg.selectCoinsForQuit`
  (`lib/wallet/src/Cardano/Wallet/IO/Delegation.hs:312-316`),
  `IODeleg.quitStakePool` (`IO/Delegation.hs:517-520`), and the Shelley
  migration endpoints, wired with `Just SelfWithdrawal`
  (`lib/api/src/Cardano/Wallet/Api/Http/Server.hs:420-421`;
  `Server.hs:4873-4880,4990-4997`).
- `IODeleg.quitStakePool` builds `WithdrawalSelf` with the cached balance even
  when it is zero, and `mkWithdrawalsLedger` writes a withdrawal entry whatever
  its amount
  (`lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs:474-487`),
  which the ledger checks. The other paths build no withdrawal at zero
  (`mkSelfWithdrawal`, `Wallet.hs:1655-1656`; `mkSelfWithdrawalShared`,
  `:1690-1691`; `checkRewardIsWorthTxCost`, `:1702-1703`), and `guardQuit`
  accepts no withdrawal at zero rewards
  (`lib/wallet/src/Cardano/Wallet/Delegation.hs:237-246`).
- A shared wallet's reward account is always `FromScriptHash`
  (`lib/wallet/src/Cardano/Wallet/Address/Keys/Shared.hs:194-200`); without a
  delegation template it has none. Byron and Icarus wallets cannot take a
  withdrawal (`shelleyOnlyMkWithdrawal`, `Server.hs:5375-5383`).
- The duplicate-vote decision is `voteRequestFor` over the history
  (`Delegation.hs:268-281`), used by `joinDRepVotingAction`
  (`Delegation.hs:283-312`, reached from `IO/Delegation.hs:606-633`) and by
  `voteAction` (`IO/Delegation.hs:635-661`), which serves both
  `transactions-construct` endpoints (`Server.hs:2984`, `:3602`). The Shelley
  construct handler also calls `assertDifferentVoting` whenever `vote` is
  present (`Wallet.hs:3594-3619`; `Server.hs:2993-2995`).
- The ledger refuses a delegation to an unregistered DRep only through
  membership in the DRep map (`Rules/Deleg.hs:218-226`), so a registered DRep
  past its expiry epoch is accepted. `queryDRepState` restricted to one
  credential returns that DRep's state only (cardano-ledger-api 1.14.0.0,
  `Cardano/Ledger/Api/State/Query.hs:188-194`).

### Unknown and stale values

- Unread at startup until the first read after the worker observes the
  account (`Implementation.hs:697-699,1062`; `Wallet.hs:1828-1834`). Because
  the balance and the vote delegation share one cache entry from one read
  (INV-5466-COHERENT), the balance reads zero in that window and no endpoint
  builds a self-withdrawal; a second, separate observer would reopen this
  window and is not used.
- Unread across the hard fork into Conway until the first Conway read; the
  balance from the last earlier read can be above zero, and FR-007's history
  rule decides.
- Unread for the duplicate-vote check in both windows; the #5350 rule
  decides, after the registration check.
- Stale for about one block plus the time a read waits on the shared query
  queue; a retirement in that window is not seen, as the cached reward amount
  is not seen today.

### API contract

- `ledger_vote` is a `oneOf` of `{"status": "delegated", "voting": <ApiDRep>}`
  and `{"status": "not_delegated"}`; the Haskell API type is a sum with a
  handwritten JSON instance that rejects every other combination, and its
  `Arbitrary` instance generates both constructors.
- `errWithdrawalNotPossibleWithoutVote` (`swagger.yaml:5058-5069`) and
  `errSameVote` (`:5692-5701`) describe the history rule; their descriptions
  change with the server message.
- The error is added to the anchors `responsesPostTransaction`,
  `responsesPostTransactionFee`, `responsesSelectCoins`,
  `responsesQuitStakePool`, `responsesCreateWalletMigrationPlan` and
  `responsesMigrateWallet`. Five of them are shared with Byron endpoints
  (`swagger.yaml:8342,8360,8432,8466,8499`), which cannot return it; the file
  already lists `errAlreadyWithdrawing` in a shared anchor (`:6544`), and this
  change follows that practice.
- The 403 of `responsesCreateWalletMigrationPlan` is a bare schema
  (`:6338-6342`) and becomes a `oneOf`.
- `responsesJoinDRep` has an explicit 404 block (`:6943-6973`), which gains
  `errNotFound`. The 404 of `responsesConstructTransaction`, which both
  `transactions-construct` endpoints use, comes only from merged anchors
  (`:6560-6565`; `:6111-6123`); an explicit 404 key replaces the merged ones,
  so the new block lists `errNoSuchWallet`, `errWalletNotInitialized` and
  `errNotFound`, and `responsesErr404WalletNotFound` and
  `responsesErr404WalletNotInitialized` leave the anchor's merge list.

### Prose specifications contradicted today

- `specifications/Cardano/Wallet/delegation.md:12` states the withdrawal
  coupling without the pre-defined options or the script-hash exemption.
- `specifications/Cardano/Wallet/delegation.md:93` calls voting "sticky",
  undone only by deregistration; a DRep retirement also undoes it in the
  ledger, and the document describes the wallet's history.
- `specifications/api/drep-voting.md:29` gives "re-join the stake pool" as the
  remedy; the automatic `Abstain` on pool join is decided from the history
  (`Wallet.hs:3629-3660`) and does not happen after a retirement.

### Tests

- Red evidence: the local-cluster scenarios of FR-019, written against the
  base commit once the cluster can register and retire a DRep, as three
  examples, one per predicted red point, because an hspec example stops at
  its first failure. On the base commit `VOTING_02a` fails at the first
  refusal (`payment-fees` with a self-withdrawal answers 202 instead of 403),
  `VOTING_02b` at the retired DRep (403 `same_vote` instead of 404) and
  `VOTING_02c` at the re-registered DRep (403 `same_vote` instead of 202).
  The shared-wallet withdrawal has no scenario. The unit examples need
  the new functions and cannot be red on the base commit; the negative
  controls show that each mirror can fail.
- Unit, `lib/unit/test/unit/Cardano/Wallet/DelegationSpec.hs`: examples for
  F5 and F7 covering a retired DRep, a delegation made outside the wallet, a
  re-registered DRep, `Abstain`, `No Confidence`, a script-hash account and
  the bootstrap phase; examples for the pure part of F8 covering a retired, a
  re-registered and an inactive DRep, `Abstain`, `No Confidence`, a query
  without an answer and the bootstrap phase; the five mirrors and their
  controls (`functions-model.md`). The existing #5350 controls
  (`DelegationSpec.hs:357-374`) stay as they are.
- Unit, `lib/unit/test/unit/Cardano/WalletSpec.hs`: `assertIsVoting` with a
  network layer stubbed through `dummyNetworkLayer`
  (`lib/unit/test-common/Cardano/Wallet/DummyTarget/Primitive/Types.hs:174`)
  follows the ledger value and not the history; `quitWithdrawal` (F10) builds
  no withdrawal at a zero balance; the IO part of F8, with a stubbed
  `isDRepRegistered`, refuses a DRep that the ledger does not hold as
  registered, accepts a registered DRep and a query without an answer, and
  sends no query for `Abstain`, `No Confidence` or in the bootstrap phase.
- Unit, network layer, in
  `lib/network-layer/test/Cardano/Wallet/Network/LocalStateQuery/`:
  `RewardAccountSpec` checks that the conversion of M7 maps a requested
  account absent from the delegatee map to `VoteNotDelegated`, maps
  `DRepAlwaysAbstain`, `DRepAlwaysNoConfidence`, key-hash and script-hash
  DReps, returns no vote delegation before Conway, gives every requested
  account a balance and a vote delegation from the same answer
  (INV-5466-COHERENT), and returns an empty map for an empty set without a
  query (INV-5466-NOEMPTY). `DRepsSpec` checks that M8 sends the credential
  that a vote delegation certificate delegates to, for key-hash and
  script-hash DReps; it adds `cardano-ledger-core`, already a dependency of
  the library, to the network-layer unit suite. The existing `Observer` tests
  (`lib/unit/test/unit/Cardano/Wallet/Shelley/NetworkSpec.hs:195-330`) keep
  passing with the new value type.
- Unit, `lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs`: JSON round trip,
  golden files, rejection of the two invalid combinations, and schema
  conformance for the new API type and the extended `ApiWalletDelegation`.
- Integration, local cluster, in
  `lib/integration/scenarios/Test/Integration/Scenario/API/Voting.hs`: the
  rewards come from the `rewardWallet` fixture
  (`lib/integration/framework/Test/Integration/Framework/DSL.hs:1955-1983`),
  which the setup delegates with an automatic `Abstain` vote
  (`lib/integration/framework/Test/Integration/Framework/Setup.hs:842-870`);
  the scenario votes it to a registered DRep A and retires A. Registration and
  retirement come from a new cluster module
  `Cardano.Wallet.Launch.Cluster.DRep`, which builds the certificates with
  `cardano-cli conway governance drep` and submits them with a faucet input
  through `takeFaucet` and `signAndSubmitTx`, as `preparePoolRetirement` does
  (`lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/ConfiguredPool.hs:385-412`;
  `Cluster/Tx.hs:105-115`), exposed to tests through the faucet HTTP service
  beside `SendFaucetAssetsQ`
  (`lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/Http/Faucet/Client.hs:64-71`).
- Integration, shared wallet: no scenario. Shared-wallet pool delegation does
  not run on the local cluster, so User Story 2 scenario 9 rests on the unit
  tests of F5 and F6 for a `FromScriptHash` reward account. A shared wallet's
  join, built by `transactions-construct` and signed by its cosigners, is
  refused at submission with 403 `missing_witnesses_in_transaction`. A vote
  to an unregistered DRep while the ledger value is unread, a script-hash
  DRep, an inactive DRep and a vote through the shared `transactions-construct`
  also rest on the unit tests above.
- Integration, existing: the quit fee-boundary scenarios `STAKE_POOLS_QUIT_01x`
  (`lib/integration/scenarios/Test/Integration/Scenario/API/Shelley/StakePools.hs:1363-1480`)
  run again, because a quit without a zero-amount withdrawal costs less. The
  existing `withdrawal_not_possible_without_vote` scenarios
  (`Voting.hs:337-369`; `TransactionsNew.hs:4589,4705`) keep passing: their
  wallets hold no vote delegation in the ledger either.
- Integration, existing assertions: 78 assertions compare the whole delegation
  object with `shouldBe` (77 in the scenarios, 3 of them through
  `expectCliField` and `expectCliListField` in
  `lib/integration/scenarios/Test/Integration/Scenario/CLI/Shelley/Wallets.hs`,
  and 1 in a framework helper at `DSL.hs:4311`). They compare `active` and
  `next` only, through one framework helper, `shouldHaveDelegationHistory`,
  because the new field's value at those points depends on when the per-tip
  read ran.

## Project Structure

### Documentation (this feature)

```text
specs/5466-ledger-vote-delegation/
├── spec.md
├── plan.md
├── data-model.md
├── functions-model.md
├── modules-model.md
└── tasks.md
```

### Source Code (repository root)

```text
lib/primitive/lib/Cardano/Wallet/Primitive/Types/DRep.hs
lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/Certificates.hs
lib/network-layer/src/Cardano/Wallet/Network.hs
lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs
lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery.hs
lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery/RewardAccount.hs
lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery/DReps.hs
lib/network-layer/test/Cardano/Wallet/Network/LocalStateQuery/
lib/network-layer/cardano-wallet-network-layer.cabal
lib/wallet/src/Cardano/Wallet.hs
lib/wallet/src/Cardano/Wallet/Transaction.hs
lib/wallet/src/Cardano/Wallet/Delegation.hs
lib/wallet/src/Cardano/Wallet/IO/Delegation.hs
lib/api/src/Cardano/Wallet/Api/Types.hs
lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs
lib/api/src/Cardano/Wallet/Api/Http/Server/Error.hs
specifications/api/swagger.yaml
specifications/api/drep-voting.md
specifications/Cardano/Wallet/Delegation.agda
specifications/Cardano/Wallet/delegation.md
nix/delegation-agda.nix
flake.nix
.github/workflows/ci.yml
lib/unit/test-common/Cardano/Wallet/DummyTarget/Primitive/Types.hs
lib/unit/test/unit/Cardano/Wallet/DelegationSpec.hs
lib/unit/test/unit/Cardano/WalletSpec.hs
lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs
lib/unit/test/data/Cardano/Wallet/Api/
lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/DRep.hs
lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/Http/Faucet/
lib/local-cluster/data/swagger.json
lib/local-cluster/local-cluster.cabal
lib/local-cluster/test/unit/Cardano/Wallet/Launch/Cluster/Http/Faucet/APISpec.hs
lib/integration/framework/Test/Integration/Framework/
lib/integration/exe/e2e.hs
lib/benchmarks/exe/latency-bench.hs
lib/integration/scenarios/Test/Integration/Scenario/API/Voting.hs
lib/integration/scenarios/Test/Integration/Scenario/API/Shared/Transactions.hs
lib/integration/scenarios/Test/Integration/Scenario/
```

**Structure Decision**: the change follows the existing layering: the query
in the network layer, the decisions in the wallet layer, the field and error
mapping in the API layer, and the formal model and prose under
`specifications/`.

## Strategy

1. Give the local cluster DRep registration and retirement, and write the
   integration scenarios against the base commit as the red evidence.
2. Read the ledger value per tip, make the whole-object delegation
   assertions compare only `active` and `next`, and report the value (S1).
3. Gate every self-withdrawal on it, and build no withdrawal on a zero-balance
   quit (S2).
4. Check the target's registration, then decide repeated votes from the
   ledger value (S3).
5. Make the Agda check print its errors under a UTF-8 locale, and extend the
   Agda model, the mutations, the mirrors and their controls (S4).
6. Commit the scenarios of step 1 after the code that turns them green, and
   run the integration scenarios, including the quit fee-boundary ones (S5).
7. Run the focused gate, formatting and linting before each push (S6).

## Slices

- **S0: red evidence.** T001 to T004. Commit
  `test(local-cluster): register and retire DReps`; the scenarios are
  committed in S5.
- **S1: ledger vote delegation.** T005 to T012, and T032 of S5 before the
  field reaches the delegation object. Commits
  `feat(network): read reward account vote delegation per tip`,
  `test(integration): compare delegation history in assertions` and
  `feat(api): report ledger vote delegation`.
- **S2: withdrawal gate.** T013 to T021. Commits
  `fix(wallet): build no withdrawal when quitting at a zero balance` and
  `fix(wallet): refuse self-withdrawals the ledger would refuse`.
- **S3: vote decision.** T022 to T027. Commits
  `feat(network): query whether the ledger holds a DRep as registered` and
  `fix(wallet): decide DRep registration and repeated votes from the ledger`.
- **S4: formal backend and mirrors.** T028 to T031. Commits
  `fix(nix): let Agda print its errors in the delegation check`,
  `test(wallet): check ledger vote decisions in Agda` and
  `test(wallet): mirror the ledger vote laws in QuickCheck`.
- **S5: integration.** T033 and T034; T032 is done in S1. Commit
  `test(integration): cover a retired DRep`, the scenarios of T003, last.
- **S6: verification.** T035, no commit of its own.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|---|---|---|
| 78 existing integration assertions change from whole-object to `active` and `next` comparison | The new field's value at those points depends on when the per-tip read ran | A field outside the delegation object would leave `GET /v2/wallets/{walletId}/stake-keys`, which reports delegation per reward account through the same object, without it |
