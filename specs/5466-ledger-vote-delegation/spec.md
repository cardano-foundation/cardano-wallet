# Feature Specification: Ledger vote delegation for withdrawal and vote checks

**Issue**: #5466
**Created**: 2026-10-23
**Updated**: 2026-10-26
**Status**: Draft
**Input**: issue #5466, the wallet reports and checks a vote delegation that
the ledger has removed

## Outcome

The wallet reads the vote delegation that the ledger holds for each of its
reward accounts, reports it beside the delegation history it already reports,
and decides two things from it: whether a transaction may withdraw from the
wallet's own reward account, and whether a vote delegation repeats the one in
force. A request that would put a withdrawal from the wallet's own reward
account into a transaction, and that the ledger would refuse for want of a
vote delegation, is refused before any transaction is built, recorded or
submitted. A withdrawal from an external reward account, named by a recovery
phrase, is not checked.

## Background

- From protocol version 10 the ledger refuses a transaction that withdraws
  from a key-hash reward account whose account state holds no vote delegation.
  The failure is `ConwayWdrlNotDelegatedToDRep`, raised by
  `validateWithdrawalsDelegated` in the Conway `LEDGER` rule
  (cardano-ledger-conway 1.23.0.0, `Rules/Ledger.hs:371-380,473-488`). The
  check is skipped while `hardforkConwayBootstrapPhase` holds (`Era.hs:257-258`,
  protocol version 9), reads the account state from before the transaction's
  own certificates, ignores the withdrawn amount, and does not apply to
  script-hash reward accounts. The formal ledger specification states the same
  premise in `PRE-CERT`: `filter isKeyHash wdrlCreds ⊆ dom voteDelegs`.
- CIP-1694, "Incentives for Ada holders to delegate voting stake": after the
  bootstrapping phase, reward accounts are blocked from withdrawing unless
  their stake credential is delegated to a DRep or to a pre-defined voting
  option. "Pre-defined Voting Options" defines `Abstain` and `No Confidence`.
  The CIP's wording covers every reward account; the ledger exempts
  script-hash reward accounts (`Rules/Ledger.hs:481`), and this specification
  follows the ledger.
- CIP-1694, "DRep retirement certificates": a DRep is retired as soon as the
  chain accepts its retirement certificate. The ledger then removes the vote
  delegation of every account delegated to that DRep (`ConwayUnRegDRep` in the
  Conway `GOVCERT` rule, `Rules/GovCert.hs:234-254`; `GOVCERT-deregdrep` in the
  formal specification).
- cardano-ledger-dijkstra 0.3.0.0, the version this repository pins, removes
  the rule from Dijkstra ("WdrlNotDelegatedToDRep has been removed in
  Dijkstra", `Rules/Ledger.hs:500`; CHANGELOG 0.3.0.0).
- The wallet records vote delegation only from certificates found in its own
  transactions (`Cardano.Wallet.putDelegationCertificate`, read back by
  `Dlgs.getVoting`). A retirement certificate is in the DRep's transaction, so
  after a retirement the wallet still reports the retired DRep,
  `assertIsVoting` passes, and the node rejects every transaction that
  withdraws rewards.
- The ledger answers the vote delegation of given stake credentials through
  the local-state query `GetFilteredVoteDelegatees`
  (ouroboros-consensus 4.2.1.0, `Ouroboros.Consensus.Shelley.Ledger.Query`),
  answered by `queryDRepDelegatees` from the same account field that
  `validateWithdrawalsDelegated` reads (`dRepDelegationAccountStateL`).

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Report the ledger's vote delegation (Priority: P1)

A client reads, for a wallet's reward account, the vote delegation the ledger
holds, next to the delegation history the wallet already reports. The client
can tell when the two differ, for example after the DRep the wallet delegated
to has retired, and can decide before building a transaction whether to
request a withdrawal.

**Why this priority**: the checks in User Stories 2 and 3 use the same value,
and a client cannot avoid a refused withdrawal without it.

**Independent Test**: on a local cluster, delegate a wallet's voting power to
DRep A, retire DRep A, and read `GET /v2/wallets/{walletId}`.
`delegation.ledger_vote.status` becomes `not_delegated` while the wallet's
history (the last `delegation.next` entry, else `delegation.active`) still
names DRep A.

**Acceptance Scenarios**:

1. **Given** the ledger holds the wallet's reward account delegated to DRep
   A, **When** a client calls `GET /v2/wallets/{walletId}`, **Then**
   `delegation.ledger_vote` is `{"status": "delegated", "voting": <DRep A>}`.
2. **Given** DRep A then retires, **When** the wallet has completed a read at
   a node tip that includes the retirement, **Then** `delegation.ledger_vote`
   is `{"status": "not_delegated"}`, and the retirement changes neither
   `delegation.active` nor `delegation.next`.
3. **Given** the reward account is delegated to `Abstain` or to
   `No Confidence`, **Then** `delegation.ledger_vote.voting` is `abstain` or
   `no_confidence`.
4. **Given** the wallet has not yet completed a read for the account, or the
   node is in an era before Conway, **Then** `delegation.ledger_vote` is
   absent.
5. **Given** a shared wallet with a delegation script template, or the
   wallet's own key in `GET /v2/wallets/{walletId}/stake-keys`, **Then** the
   same field reports the ledger value for that reward account.

### User Story 2 - Refuse a self-withdrawal the ledger would refuse (Priority: P1)

A client that requests `"withdrawal": "self"`, or an endpoint that adds the
wallet's own reward withdrawal by itself, receives a 403
`withdrawal_not_possible_without_vote` before anything is built for signing or
submitted, whenever the ledger holds no vote delegation for the wallet's
key-hash reward account. The wallet's funds stay available, and the response
names the remedy: a vote delegation to a DRep, to `abstain` or to
`no_confidence`, in a transaction that is in the ledger before the withdrawal.

**Why this priority**: the node rejects every such transaction. A rejected
transaction built by `buildSignSubmitTransaction` is recorded as pending
before it is submitted, so its inputs stay unavailable until it expires; the
vote delegation that would repair the account can then fail for want of
inputs.

**Independent Test**: on a local cluster, with DRep A retired and the reward
balance above zero, send each request that can be refused and observe the
403, an unchanged available balance and no new pending transaction.

**Acceptance Scenarios**:

1. **Given** the ledger holds no vote delegation for the wallet's reward
   account and the reward balance is worth withdrawing, **When** a client
   calls `POST /v2/wallets/{walletId}/transactions`,
   `POST /v2/wallets/{walletId}/payment-fees`,
   `POST /v2/wallets/{walletId}/coin-selections/random` or
   `POST /v2/wallets/{walletId}/transactions-construct` with
   `"withdrawal": "self"`, **Then** the response is 403
   `withdrawal_not_possible_without_vote` and no transaction is recorded or
   submitted.
2. **Given** the same state with a reward balance above zero, **When** a
   client calls `DELETE /v2/stake-pools/*/wallets/{walletId}`, **Then** the
   response is 403 `withdrawal_not_possible_without_vote`.
3. **Given** a cached reward balance of exactly zero and no vote delegation,
   **When** a client calls `DELETE /v2/stake-pools/*/wallets/{walletId}`,
   **Then** the transaction carries no withdrawal and is not refused on this
   ground.
4. **Given** a wallet that is not delegating, **When** it quits, **Then** the
   response is 403 `not_delegating_to` as today, whatever its vote
   delegation.
5. **Given** the ledger holds no vote delegation, **When** a client calls
   `POST /v2/wallets/{walletId}/coin-selections/random` with
   `{"delegation_action": {"action": "quit"}}` and the selection would withdraw
   the rewards, **Then** the response is 403
   `withdrawal_not_possible_without_vote`.
6. **Given** the ledger holds no vote delegation, **When** a client calls
   `POST /v2/wallets/{walletId}/migrations/plan` or
   `POST /v2/wallets/{walletId}/migrations` for a Shelley wallet whose reward
   balance is worth withdrawing, **Then** the response is 403
   `withdrawal_not_possible_without_vote` and no transaction is submitted.
7. **Given** the ledger holds a vote delegation to a DRep, to `Abstain` or to
   `No Confidence`, **When** any of the requests above is made, **Then** it is
   answered as it is today, including when the wallet's history records no
   vote.
8. **Given** the wallet has not yet completed a read for the account, **When**
   any of the requests above would put a self-withdrawal into a transaction,
   **Then** the wallet decides from its history, by the rule `assertIsVoting`
   applies today.
9. **Given** a shared wallet, whose reward account is a script hash, **When**
   `POST /v2/shared-wallets/{walletId}/transactions-construct` requests
   `"withdrawal": "self"` and the wallet's history records no vote, **Then**
   the request is accepted; the ledger accepts the withdrawal.
10. **Given** the node is at protocol version 9, **Then** no request is
    refused on this ground.

### User Story 3 - Decide a repeated vote from the ledger (Priority: P2)

A client that delegates voting power to a DRep the ledger does not hold as
registered is refused before submission. Otherwise, it is refused with
`same_vote` only when the target is the delegation the ledger holds.

**Why this priority**: the duplicate-vote check reads the same history. After
a retirement it refuses the retired DRep as `same_vote`, including after that
DRep has registered again; after a delegation made outside the wallet it
refuses a valid target and accepts a duplicate. Comparing against the ledger
alone would let a delegation to the retired DRep reach the node, which
refuses delegation to an unregistered DRep from protocol version 10
(`DelegateeDRepNotRegisteredDELEG`, `Rules/Deleg.hs:220-226`;
`DELEG-delegate` in the formal specification), so the registration check
belongs to the same change and runs first.

**Independent Test**: on a local cluster, with DRep A retired, request
`PUT /v2/dreps/{drepId}/wallets/{walletId}` for DRep A and for `abstain`, and
observe a 404 and a 202; register DRep A again and observe a 202 for DRep A.

**Acceptance Scenarios**:

1. **Given** the ledger holds the reward account delegated to DRep B while
   the wallet's history names DRep A, **When** a client delegates to DRep A,
   **Then** the request is accepted.
2. **Given** the ledger holds the reward account delegated to DRep B,
   **When** a client delegates to DRep B, **Then** the response is 403
   `same_vote`.
3. **Given** DRep A has retired, **When** a client delegates to DRep A,
   **Then** the response is 404 `not_found` and nothing is submitted, whether
   the wallet has completed a read of its vote delegation; **When** it
   delegates to a registered DRep, to `abstain` or to `no_confidence`,
   **Then** the request is accepted.
4. **Given** DRep A retired and has registered again, **When** a client
   delegates to DRep A, **Then** the request is accepted.
5. **Given** DRep A is registered but inactive (past its expiry epoch),
   **When** a client delegates to DRep A, **Then** the request is not refused
   on registration grounds; the ledger accepts delegation to an inactive DRep.
6. **Given** a registered target and an unread ledger value, **Then** the
   duplicate-vote decision follows the effective-delegation rule of
   `specs/5350-redelegate-previous-drep`.
7. **Given** any of the states above, **When** the same vote is requested
   through `POST /v2/wallets/{walletId}/transactions-construct` with `vote`,
   or through `POST /v2/shared-wallets/{walletId}/transactions-construct`
   with `vote` and at least one other payload, **Then** the verdict is the
   same as for
   `PUT /v2/dreps/{drepId}/wallets/{walletId}`, except as stated in FR-015.

### Edge Cases

- The value is the last completed read, not the node tip. A DRep retirement
  in a block after that read is not seen until the next read completes, so
  for about one block plus the time the read waits on the node query queue a
  self-withdrawal can still be built and rejected by the node. The cached
  reward balance has the same exposure today.
- A vote delegation that has been submitted but is not yet in a block does
  not count, although the node's mempool would accept a withdrawal chained
  after it.
- Reads may skip node tips: the refresh runs on a tip change and does not
  guarantee to see every intermediate tip.
- A read does not fail. Its query waits for the node and is sent again after
  a dropped connection; until it completes, the previous value stays in place.
- The registration query of FR-012 runs on the same node query queue as the
  read. While the node does not answer, a vote delegation request to a DRep
  credential waits for the answer, with no timeout, as every other request
  that queries the ledger does.
- The value is unread at startup until the first read after the wallet's
  worker starts observing the account. In that window the reward balance also
  reads zero (FR-003), so no endpoint builds a self-withdrawal. Across the hard
  fork into Conway, the balance from the last read before Conway can be above
  zero until the first read in Conway completes, and the history rule of
  FR-007 decides. The duplicate-vote check can meet an unread value in either
  window.
- A reward account whose stake key is not registered has no vote delegation
  in the ledger and reports `not_delegated`. Its reward balance is zero, so no
  withdrawal is built.
- A rollback changes the tip, and the value is read again.
- `GetFilteredVoteDelegatees` with an empty credential set returns the vote
  delegation of every account in the ledger. The wallet never sends it with
  an empty set.
- A withdrawal from an external reward account, named by a recovery phrase,
  is not a self-withdrawal and keeps its current behavior.
- Byron and Icarus wallets, and shared wallets without a delegation script
  template, have no reward account: no field and no check.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The wallet MUST read the vote delegation of every reward
  account it observes with `GetFilteredVoteDelegatees`, at most once per node
  tip, in the same local-state-query acquisition that reads the reward
  balance, in Conway and later eras. The value used by every decision is the
  last completed read.
- **FR-002**: The query function MUST NOT send `GetFilteredVoteDelegatees`
  with an empty credential set, whatever its caller does.
- **FR-003**: In Conway and later, an observed account's cached reward
  balance and cached vote delegation MUST come from the same read, so a
  cached balance implies a cached vote delegation for that account.
- **FR-004**: `GET /v2/wallets/{walletId}`, `GET /v2/wallets`, the shared
  wallet endpoints and `GET /v2/wallets/{walletId}/stake-keys` MUST report the
  value as the optional field `ledger_vote` of the delegation object:
  `{"status": "delegated", "voting": <DRep>}` or
  `{"status": "not_delegated"}`, absent while the value is unread or before
  Conway. `voting` uses the encoding of `delegation.active.voting`.
- **FR-005**: `delegation.active` and `delegation.next` MUST keep their
  meaning as the wallet's record of its own vote and stake delegation
  certificates.
- **FR-006**: `specifications/api/swagger.yaml` MUST define the field as a
  `oneOf` of the two shapes. The API type MUST NOT represent any other
  combination, and JSON decoding MUST reject one.
- **FR-007**: In Conway, a self-withdrawal from a reward account MUST be
  permitted exactly when one of these holds, in this order: the protocol
  version is in the Conway bootstrap phase; the reward account is a script
  hash; the ledger value is known and holds a vote delegation; or the ledger
  value is unknown and the wallet's history records a vote. Other eras keep
  their current behavior.
- **FR-008**: The decision of FR-007 MUST be applied, before any transaction
  is built, at `POST /v2/wallets/{walletId}/transactions`,
  `POST /v2/wallets/{walletId}/payment-fees`,
  `POST /v2/wallets/{walletId}/coin-selections/random` (a payment with
  `withdrawal`, and `delegation_action` quit),
  `POST /v2/wallets/{walletId}/transactions-construct`,
  `POST /v2/shared-wallets/{walletId}/transactions-construct`,
  `DELETE /v2/stake-pools/*/wallets/{walletId}`,
  `POST /v2/wallets/{walletId}/migrations/plan` and
  `POST /v2/wallets/{walletId}/migrations`, whenever the request would place
  a withdrawal from the wallet's own reward account in the transaction.
- **FR-009**: Every endpoint of FR-008 except
  `POST /v2/shared-wallets/{walletId}/transactions-construct` can refuse under
  FR-007; a shared wallet's reward account is a script hash and is always
  permitted.
- **FR-010**: `DELETE /v2/stake-pools/*/wallets/{walletId}` MUST build no
  withdrawal when the cached reward balance is exactly zero, as the other quit
  paths already do. On both quit paths the decision of FR-007 MUST run after
  the existing quit checks, so `not_delegating_to` and `non_null_rewards` keep
  their precedence.
- **FR-011**: A refusal under FR-007 MUST be 403
  `withdrawal_not_possible_without_vote`. Its message, and the description of
  the error in `specifications/api/swagger.yaml`, MUST name the cause (the
  ledger holds no vote delegation for the reward account) and the remedy (a
  vote delegation to a DRep, to `abstain` or to `no_confidence`, in a
  transaction that is in the ledger before the withdrawal). The response
  lists of every endpoint of FR-009 MUST include the error.
- **FR-012**: Before the duplicate-vote check, and outside the bootstrap
  phase, a vote delegation to a DRep credential that the ledger does not hold
  as registered MUST be refused with 404 `not_found` before any transaction is
  built, at `PUT /v2/dreps/{drepId}/wallets/{walletId}`, at
  `POST /v2/wallets/{walletId}/transactions-construct` with `vote`, and at
  `POST /v2/shared-wallets/{walletId}/transactions-construct` with `vote` and
  at least one other payload. A registered DRep past its
  expiry epoch is registered. `abstain` and `no_confidence` need no
  registration. The registration is queried from the node for each such
  request and is not cached. A registration query that returns no answer,
  which happens only before Conway, does not refuse.
- **FR-013**: A vote delegation request that passes FR-012 MUST be refused
  with 403 `same_vote` exactly when the ledger value is known and holds a
  delegation to the requested target, or the ledger value is unknown and the
  effective-delegation rule of `specs/5350-redelegate-previous-drep` finds the
  target. The description of `same_vote` in
  `specifications/api/swagger.yaml` MUST state this rule.
- **FR-014**: The pure decision used by `joinDRepVotingAction` and the IO
  decision used by `voteAction` MUST return the same duplicate-vote verdict for
  the same ledger value and history.
- **FR-015**: `assertDifferentVoting`, which runs on every
  `POST /v2/wallets/{walletId}/transactions-construct` request that has
  `vote`, MUST use the verdict of FR-013. A repeated vote in a request that
  also has `delegations` is refused there, as today, with 403
  `voting_in_invalid_era`; a request without `delegations` is refused earlier
  with `same_vote`.
- **FR-016**: The repository's delegation Agda specification
  (`specifications/Cardano/Wallet/Delegation.agda`) MUST be extended with the
  ledger vote observation, the withdrawal decision and the ledger-aware
  duplicate-vote decision, and prove `AGDA-5466-WDRL-LEDGER`,
  `AGDA-5466-WDRL-EXEMPT`, `AGDA-5466-WDRL-UNREAD`, `AGDA-5466-VOTE-LEDGER`
  and `AGDA-5466-VOTE-UNREAD`, as stated in `functions-model.md`, without
  postulates standing in for them. The history the withdrawal decision reads
  is the DRep of the wallet's latest delegation status (`getCurrentVoting`),
  which equals the DRep of the effective status of
  `specs/5350-redelegate-previous-drep`. The `AGDA-5350-*` laws remain and
  describe the duplicate-vote decision when the ledger value is unread.
- **FR-017**: Every `AGDA-5466-*` law MUST have a named QuickCheck mirror over
  the Haskell implementation, registered in the unit-test tree. Each mirror
  MUST carry `checkCoverage` classes over the cases that its law distinguishes
  within its scope, including the inputs on which the law and its named mutant
  disagree, as listed in `functions-model.md`. The `AGDA-5350-*` mirrors MUST
  keep running, with the ledger value unread.
- **FR-018**: Each new mirror MUST have a registered negative control: the
  same property run against a named mutant under `expectFailure`, as listed in
  `functions-model.md`. `checks.x86_64-linux.delegation-agda` MUST reject one
  law-breaking mutation per new law.
- **FR-019**: Local-cluster integration scenarios MUST cover: a wallet with
  rewards, delegated to a registered DRep, whose DRep retires (the field, every
  refusal of FR-009, the refusals of FR-012 at
  `PUT /v2/dreps/{drepId}/wallets/{walletId}` and at
  `POST /v2/wallets/{walletId}/transactions-construct`, the unchanged balance
  and pending set after each refusal, a re-delegation and a withdrawal after
  it); and the retired DRep registered again and accepted as a target. The
  shared-wallet cases rest on unit tests: a self-withdrawal without a vote
  (User Story 2 scenario 9), because shared-wallet pool delegation does not
  run on the local cluster, and a vote through
  `POST /v2/shared-wallets/{walletId}/transactions-construct`. The existing
  quit fee-boundary scenarios MUST be run again.
- **FR-020**: The change MUST NOT alter the wallet database schema. The
  ledger value lives in the network layer's reward-account cache.

### Key Entities

- **Ledger vote delegation**: for one reward account, as of the last
  completed read, either a delegation to a DRep, to `Abstain` or to
  `No Confidence`, or no delegation. Unknown until the first read for an
  observed account, and in eras before Conway.
- **Delegation history**: the existing `active` status and ordered `next`
  statuses, derived from the wallet's own certificates.
- **Withdrawal decision**: permitted or refused, from the bootstrap phase, the
  reward account's credential kind, the ledger vote delegation and, when that
  is unknown, the history.
- **Duplicate-vote decision**: `same_vote` or different, from the target, the
  ledger vote delegation and, when that is unknown, the effective delegation
  of the history.

## API compatibility

The constitution requires a version bump for breaking changes and does not
define one. This change adds one optional response field and returns
existing error codes from more requests. No field, path or error code is
removed or renamed. The responses that change:

| Request | Today | After |
|---|---|---|
| `payment-fees`, `coin-selections/random` (payment or quit), `migrations/plan`, with a self-withdrawal and no ledger vote delegation | 2xx | 403 `withdrawal_not_possible_without_vote` |
| `transactions`, `transactions-construct`, `DELETE /stake-pools/*`, `migrations`, same state, history records a vote | transaction built, then rejected by the node | 403 `withdrawal_not_possible_without_vote` |
| `DELETE /stake-pools/*` at a cached balance of zero, no vote delegation | rejected by the node | accepted, no withdrawal |
| either `transactions-construct` with `vote` to an unregistered DRep (the shared endpoint with at least one other payload) | 202 | 404 `not_found` |
| `PUT /dreps/{drepId}/wallets/{walletId}` to a retired DRep in the history | 403 `same_vote` | 404 `not_found` |
| a vote to the target the ledger holds, set outside the wallet | accepted | 403 `same_vote` |
| a vote to the history's DRep while the ledger holds another | 403 `same_vote` | accepted |
| shared-wallet or bootstrap-phase self-withdrawal, no recorded vote | 403 `withdrawal_not_possible_without_vote` | accepted |
| `withdrawal_not_possible_without_vote` | current message | message naming the ledger cause and remedy |

Each new refusal replaces an answer about a transaction the node refuses:
an estimate, a selection, a migration plan or an unsigned transaction for a
withdrawal from an account with no vote delegation (cardano-ledger-conway
1.23.0.0 `Rules/Ledger.hs:379-380`), or for a delegation to an unregistered
DRep (`Rules/Deleg.hs:224-226`). Each request that is accepted after the
change was refused by the wallet, although the ledger accepts it.

## Invariants

- **INV-5466-LEDGER** (`BLOCKING`): when the ledger value is known, the
  withdrawal decision and the duplicate-vote decision depend on it and on
  nothing in the wallet's delegation history.
- **INV-5466-EXACT** (`BLOCKING`): in Conway, outside the bootstrap phase, for
  a key-hash reward account with a known ledger value, the wallet refuses a
  self-withdrawal exactly when the last completed read holds no vote
  delegation.
- **INV-5466-NOSUBMIT** (`BLOCKING`): a request refused under FR-007 or
  FR-012 records nothing and submits nothing; the wallet's available balance
  and its pending transactions are unchanged.
- **INV-5466-FALLBACK** (`BLOCKING`): when the ledger value is unknown, a
  self-withdrawal from a key-hash reward account outside the bootstrap phase
  follows today's `assertIsVoting` rule at every endpoint of FR-008, and the
  duplicate-vote decision for a registered target follows
  `specs/5350-redelegate-previous-drep`.
- **INV-5466-COHERENT** (`BLOCKING`): in Conway and later, a cached reward
  balance implies a cached vote delegation for the same account, both from one
  read.
- **INV-5466-HISTORY** (`BLOCKING`): `delegation.active` and
  `delegation.next` report what they report today.
- **INV-5466-NOEMPTY** (`BLOCKING`): no `GetFilteredVoteDelegatees` query is
  sent with an empty credential set.
- **INV-5466-COMPAT** (`BLOCKING`): the observable API changes are exactly
  those listed under "API compatibility".
- **INV-5466-PARITY** (`BLOCKING`): the pure and IO paths return the same
  duplicate-vote verdict for the same ledger value and history.
- **INV-5466-AGDA** (`BLOCKING`): the checked Agda backend defines the ledger
  vote observation and both decisions, and proves every `AGDA-5466-*` law
  without postulates standing in for them.
- **INV-5466-MIRROR** (`BLOCKING`): each named Agda law has a named QuickCheck
  mirror that exercises the Haskell function over generated values against an
  oracle built from those values, cannot pass vacuously, and fails against its
  named mutant.
- **INV-5466-WIRING** (`BLOCKING`): CI invokes the Agda check, which rejects
  one mutation per law, and the unit-test target invokes every mirror and its
  negative control.
- **INV-5466-PROOF** (`ADVISORY`): the local-cluster scenarios are
  demonstrated RED on the base commit and GREEN with the change.

## Rejection behavior

- 403 `not_delegating_to` and `non_null_rewards` on the quit paths come
  first, as today.
- 403 `withdrawal_not_possible_without_vote`: a request under FR-008 when the
  withdrawal decision of FR-007 refuses.
- 404 `not_found`: a vote delegation to a DRep credential that the ledger
  does not hold as registered, outside the bootstrap phase, checked before the
  duplicate-vote check.
- 403 `same_vote`: a vote delegation whose target is the delegation the
  ledger holds, or, with the ledger value unknown, the effective delegation of
  the history; `voting_in_invalid_era` for the same verdict on the path of
  FR-015.
- `withdrawal_not_possible_without_vote` is not returned for a script-hash
  reward account or in the bootstrap phase, and `same_vote` is not returned
  for a target that is in the wallet's history but not in a known ledger
  value.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: In the local-cluster scenario, `delegation.ledger_vote` reports
  `not_delegated` once the wallet has completed a read at a tip that includes
  the retirement, with no request from the wallet's owner.
- **SC-002**: In the same scenario, every endpoint of FR-009 returns 403
  `withdrawal_not_possible_without_vote`, and after each refusal the wallet's
  available balance equals its value before the request and no transaction is
  pending.
- **SC-003**: After a vote delegation to `abstain` or to a registered DRep is
  in the ledger, `POST /v2/wallets/{walletId}/transactions` with
  `"withdrawal": "self"` is accepted and the transaction withdraws the reward
  balance.
- **SC-004**: CI typechecks the extended Agda model and rejects each of the
  new law mutations; each new mirror fails against its named mutant.
- **SC-005**: The existing unit and integration suites pass, with whole-object
  comparisons of the delegation object limited to `active` and `next`, and the
  quit fee-boundary scenarios pass with the zero-balance quit of FR-010.

## Assumptions

- Reading the vote delegation adds one query to the per-tip acquisition that
  already reads the reward balance, over the same set of observed reward
  accounts.
- Stake pool delegation, which carries no withdrawal, keeps deciding its
  automatic `Abstain` vote from the wallet's history.
- `checkRewardIsWorthTxCost` keeps deciding whether a payment includes the
  withdrawal.
- The checks apply in Conway, where `assertIsVoting` and
  `joinDRepVotingAction` apply today; Dijkstra has no withdrawal rule. The
  query covers Conway and Dijkstra.
