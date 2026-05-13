# Feature Specification: Drop cardano-api from Shelley/Transaction.hs and cert helpers

**Feature Branch**: `006-drop-api-shelley-tx`
**Created**: 2026-05-13
**Status**: Draft
**Input**: GitHub issue [cardano-foundation/cardano-wallet#5285](https://github.com/cardano-foundation/cardano-wallet/issues/5285); parent epic [#5237](https://github.com/cardano-foundation/cardano-wallet/issues/5237); active driver [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243).

The wallet codebase is mid-migration off the `cardano-api` library. After PR #5236 (additive types) and PR #5270 (ledger-native cert builders, `Transaction.Ledger`), the largest remaining `cardano-api` carrier under `lib/wallet/` is `Shelley/Transaction.hs` (~1500 lines, 9 `Cardano.Api*` import lines), plus the two single-function cert helper modules `Transaction/Voting.hs` and `Transaction/Delegation.hs` whose ledger-native replacements already exist. This feature removes those usages in vertical, independently shippable slices.

The top-level `Transaction.hs` interface module (which only carries one dead `Cardano.Api.Extra ()` import) is **out of scope** — issue #5243 names it loosely; it is not actually a `cardano-api` carrier.

## User Scenarios & Testing *(mandatory)*

Audience: codebase maintainers driving the `cardano-api` removal epic. Each story is an independently shippable refactor slice.

### User Story 1 - Delete the cert helper modules (Priority: P1)

The two helpers `Transaction/Voting.hs` and `Transaction/Delegation.hs` each export exactly one function (`certificateFromVotingAction` and `certificateFromDelegationAction`) built on `cardano-api` types. Ledger-native equivalents `certificateFromVotingActionLedger` and `certificateFromDelegationActionLedger` already live in `Shelley/Transaction/Ledger.hs` since PR #5270. The single caller of the original helpers is `Shelley/Transaction.hs`. This slice deletes both helper files, switches the callsites in `Shelley/Transaction.hs` to the `*Ledger` variants, and prunes the deleted modules from the cabal `exposed-modules`.

**Why this priority**: Unblocked, mechanical, small surface area. Lands fastest, takes two `Cardano.Api*` imports out of `Shelley/Transaction.hs` and removes two whole modules without depending on any other open acceptance criterion in #5243.

**Independent Test**: After this slice, `grep "Cardano.Api" lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs` and `.../Delegation.hs` return "No such file or directory"; the existing unit tests covering delegation- and voting-cert construction (`cardano-wallet-unit` Shelley specs) pass unchanged; `lib/wallet` builds.

**Acceptance Scenarios**:

1. **Given** the current `master` plus this slice, **When** a developer searches `lib/wallet/src/Cardano/Wallet/Transaction/` for cert-helper files, **Then** only `Transaction.hs` (the interface module) remains and the directory contains no `cardano-api` imports.
2. **Given** an existing wallet client that constructs a delegation or voting certificate via the public `TransactionLayer` interface, **When** that client is exercised by the integration test suite, **Then** the resulting on-chain certificate is byte-identical to the pre-migration result.

---

### User Story 2 - Migrate body construction in `Shelley/Transaction.hs` (Priority: P2)

The `mkUnsignedTx` and `mkWithdrawalTx` paths in `Shelley/Transaction.hs` build transaction bodies via `cardano-api`'s `createTransactionBody` over a populated `TxBodyContent` record. This slice replaces those calls with the ledger-native body builder exposed by `Transaction.Ledger`, removing the remaining `Cardano.Api`, `Cardano.Api.Shelley`, and related imports that feed `TxBodyContent` fields (witness sets, mint sets, certificate sets, validity intervals, withdrawal sets).

**Why this priority**: The largest single reduction in `cardano-api` surface inside `lib/wallet`, but blocked on minting + script-witness support being added to `Transaction.Ledger` (open acceptance criterion in #5243). Cannot land until that prerequisite is satisfied; it may be folded into this slice or coordinated as a parallel ticket.

**Independent Test**: After this slice, `grep -E "Cardano\.Api" lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` returns nothing for the body-construction code paths; constructed bodies match pre-migration `TxBody` bytes across the existing property and golden suites for `mkUnsignedTx` and `mkWithdrawalTx`.

**Acceptance Scenarios**:

1. **Given** the prerequisite Ledger-side minting/script-witness support is in place, **When** the migrated `mkUnsignedTx` is invoked across the existing property-test vectors, **Then** every produced body is byte-equal to the body produced by the pre-migration code.
2. **Given** a transaction with a non-empty mint set or script witness, **When** the migrated `mkUnsignedTx` is invoked, **Then** it succeeds without falling back to any `cardano-api` reconstruction helper.

---

### User Story 3 - Migrate `signTransaction` off cardano-api (Priority: P3)

`signTransaction` in `Shelley/Transaction.hs` currently constructs signed transactions via `cardano-api` witness types. This slice rewrites it directly on ledger types, removing the last `cardano-api` callsites in the file.

**Why this priority**: Blocked on the `signTransaction` ledger-rewrite acceptance criterion in #5243. Smaller surface than Story 2, but ordered last because its prerequisite is a separate piece of foundation work.

**Independent Test**: After this slice, `grep -E "Cardano\.Api" lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` returns nothing; signed transaction bytes are byte-equal across the existing signing property tests.

**Acceptance Scenarios**:

1. **Given** the ledger-side `signTransaction` rewrite is available, **When** the migrated wallet `signTransaction` runs over the existing signing property vectors, **Then** every produced signed transaction is byte-equal to the pre-migration result and verifies under the existing on-chain validation tests.

---

### Edge Cases

- **Multi-asset / script-witness transactions**: Story 2 must not regress mint-set or script-witness handling — body bytes must match exactly. The ledger-side prerequisite may not cover every edge case the current `cardano-api` path covers; surfacing that gap is part of Story 2's acceptance.
- **Era handling**: `Shelley/Transaction.hs` currently dispatches on `AnyCardanoEra`. The replacement must preserve the existing Babbage / Conway / Dijkstra era coverage and produce the same body shape per era.
- **Dead-code residue after helper deletion**: cabal `exposed-modules` and `other-modules` entries that reference the deleted helpers must be pruned in the same commit that removes the file, or `lib/wallet` will not build.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The two helper modules `lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs` and `.../Delegation.hs` MUST be deleted, and their cabal entries pruned, in the same reviewed commit that switches their caller to the `*Ledger` variants.
- **FR-002**: After all three stories land, `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` MUST contain zero `Cardano.Api*` import lines.
- **FR-003**: Each behaviour-preserving migration step (Stories 1, 2, 3) MUST be byte-equivalent to the pre-migration output for the inputs exercised by the existing unit, property, and golden suites covering the affected functions.
- **FR-004**: The top-level interface module `lib/wallet/src/Cardano/Wallet/Transaction.hs` MUST remain untouched by this feature (out of scope; the type-relocation needed to remove its dead import is a separate ticket).
- **FR-005**: Each slice MUST land as a single vertical commit (RED proof + GREEN implementation in one bisect-safe commit), per the project PR policy.
- **FR-006**: When the `cardano-api` build-depends entry in `lib/wallet/cardano-wallet.cabal` no longer has any consumer inside `lib/wallet`, the build-depends entry MUST be removed in the same commit that removes its last consumer.

### Key Entities

- **`Shelley/Transaction.hs`**: The wallet's Shelley-era transaction construction module; current largest `cardano-api` consumer in `lib/wallet`.
- **`Transaction.Ledger`** (`Shelley/Transaction/Ledger.hs`): The ledger-native replacement surface introduced by PR #5270; provides `certificateFromVotingActionLedger`, `certificateFromDelegationActionLedger`, and (once prerequisites land) ledger-native body construction and signing.
- **Cert helper modules**: `Transaction/Voting.hs` and `Transaction/Delegation.hs`; each exports one function backed by `cardano-api` types and has exactly one in-tree caller.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: The number of `Cardano.Api*` import lines in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` goes from 9 (master at `0821bddb2c`) to 0 across the three stories.
- **SC-002**: The number of `.hs` files under `lib/wallet/src/Cardano/Wallet/Transaction/` directly importing `Cardano.Api*` goes from 2 (`Voting.hs`, `Delegation.hs`) to 0 after Story 1.
- **SC-003**: Across the three stories, no behavioural regression: every existing unit, property, golden, and integration test that exercises affected code paths passes unchanged on each merged slice.
- **SC-004**: The full `cardano-wallet` package's `cardano-api` build-depends entry is removed at or before the moment when `Shelley/Transaction.hs` contains zero `Cardano.Api*` imports and no other module under `lib/wallet/` consumes `cardano-api`. (May land in a follow-up commit if other consumers outside this feature still exist.)
- **SC-005**: Each merged slice keeps `git bisect` safe: every commit in the resulting history builds and tests green.

## Assumptions

- The ledger-native cert builders `certificateFromVotingActionLedger` and `certificateFromDelegationActionLedger` introduced by [PR #5270](https://github.com/cardano-foundation/cardano-wallet/pull/5270) are behaviour-equivalent to the `cardano-api`-based originals over the test vectors in `cardano-wallet-unit`. (Already used as the call path in `Cardano.Wallet.hs`.)
- The Story 2 prerequisite — minting and script-witness support in `Transaction.Ledger` — is tracked as a separate acceptance criterion in [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243). This feature does NOT include adding it; if it has not landed when Story 2 is picked up, Story 2 is blocked and only Stories 1 and 3 (the latter once *its* prerequisite lands) are workable.
- The Story 3 prerequisite — a ledger-native `signTransaction` rewrite — is tracked under the same parent #5243.
- Existing test coverage (unit, property, golden, integration) is sufficient to detect any byte-level regression in the migrated transaction-body and signing paths. No new tests are required *unless* a coverage gap surfaces during implementation.
- The wallet's public `TransactionLayer` interface (exported from the out-of-scope top-level `Transaction.hs`) does not change shape as part of this feature. Callers see no API change.
- The branch sequencing of this feature does not assume any particular order between Stories 2 and 3 once their respective prerequisites are met — they may be implemented in either order or in parallel.
