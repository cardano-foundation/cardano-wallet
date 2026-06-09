# Feature Specification: Unsigned Shelley ledger builder migration

**Feature Branch**: `5285-unsigned-ledger`  
**Created**: 2026-05-19  
**Status**: Draft  
**Input**: GitHub issue cardano-foundation/cardano-wallet#5285, child of #5243 under parent #5237.

## Context

#5288 is merged, so `Cardano.Wallet.Shelley.Transaction.Ledger` now has script-witness parity for native-script inputs, staking scripts, mint/burn `ScriptSource` values, and output reference scripts. This ticket uses that foundation to retire the remaining unsigned-body construction path in `Cardano.Wallet.Shelley.Transaction`.

The migration must not touch integration tests. The parent epic's no-integration-test rule applies to this PR and is enforced by `gate.sh`.

## User Scenarios & Testing

### User Story 1 - Unsigned bodies use the ledger builder (Priority: P1)

As a wallet maintainer, I can construct unsigned Shelley transaction bodies through the ledger-native builder instead of `Cardano.createTransactionBody`, while preserving the transaction body bytes for supported scenarios.

**Why this priority**: This removes the body-construction dependency that blocks the rest of the `cardano-api` removal sequence.

**Independent Test**: The Shelley transaction unit specs compare the migrated `mkUnsignedTx` path against the already-proven ledger builder for non-script, certificate, mint/burn, native-script, staking-script, reference-input mint, and output-reference-script scenarios. Existing binary/golden assertions in `TransactionSpec` remain green.

**Acceptance Scenarios**:

1. **Given** a normal selection with no scripts, **When** `mkUnsignedTransaction` builds a body, **Then** the body bytes match the current ledger builder output.
2. **Given** native-script inputs, staking-script withdrawals/certs, mint/burn script sources, or output reference scripts, **When** `mkUnsignedTx` builds a body, **Then** the body matches the `buildLedgerTxRaw` result that #5288 proved equivalent to the legacy builder.
3. **Given** a `PreSelection`, **When** the migrated path builds an unsigned body, **Then** it preserves the existing no-real-input behavior without reintroducing `Cardano.TxBodyContent`.

### User Story 2 - Obsolete certificate helpers are removed (Priority: P1)

As a wallet maintainer, I can delete the `Cardano.Wallet.Transaction.Delegation` and `Cardano.Wallet.Transaction.Voting` helper modules because all remaining call sites now use ledger-native certificates.

**Why this priority**: The helper deletion is only safe after User Story 1 changes the certificate type flowing through `mkUnsignedTransaction` from `Cardano.Certificate` to `TxCert era`.

**Independent Test**: `rg` finds no imports of the deleted helper modules; `lib/wallet/cardano-wallet.cabal` no longer exposes them; the wallet library builds.

**Acceptance Scenarios**:

1. **Given** a delegation action in `mkUnsignedTransaction`, **When** certificates are assembled, **Then** `certificateFromDelegationActionLedger` is used and the resulting body contains ledger `TxCert` values.
2. **Given** a voting action in `mkUnsignedTransaction`, **When** certificates are assembled, **Then** `certificateFromVotingActionLedger` is used and the old voting helper module is unused.
3. **Given** the cabal exposed-module list, **When** the helper files are deleted, **Then** the library still configures and builds.

### User Story 3 - Remaining-work policy is machine checked (Priority: P2)

As the epic owner, I can verify that this PR does not edit integration tests and only advances the intended transaction-layer slice.

**Why this priority**: Earlier migration PRs did touch `lib/integration/**`; this stricter rule applies from #5288 onward and must be enforced, not implied.

**Independent Test**: `./gate.sh` fails if the branch diff contains any `lib/integration/**` path.

**Acceptance Scenarios**:

1. **Given** the branch diff against `origin/master`, **When** `./gate.sh` runs, **Then** it exits non-zero if any `lib/integration/**` path appears.
2. **Given** the final PR diff, **When** `gh pr diff --name-only` is filtered for `^lib/integration/`, **Then** the result is empty.

### Edge Cases

- The current `Transaction.Ledger` module imports `Shelley.Transaction`; `Shelley.Transaction` must not import it directly and create a cycle. Shared unsigned-builder code must move behind an acyclic module boundary.
- Existing `mkUnsignedTx` callers in `TransactionSpec` and `TransactionLedgerSpec` must be updated to the new certificate type without weakening their assertions.
- Dijkstra remains unsupported in this slice, matching the current `pendingWith`/`error "Dijkstra not yet supported"` pattern.
- The migrated path may still return `Cardano.TxBody` to preserve exported interfaces, but it must not assemble that body through `Cardano.TxBodyContent` or `Cardano.createTransactionBody`.
- If a production change appears to require an integration-test edit, this PR stops and a separate ticket is opened.

## Requirements

### Functional Requirements

- **FR-001**: `mkUnsignedTransaction` and `mkUnsignedTx` MUST NOT call `Cardano.createTransactionBody`.
- **FR-002**: `mkUnsignedTransaction` and `mkUnsignedTx` MUST NOT assemble a `Cardano.TxBodyContent` record.
- **FR-003**: The migrated unsigned path MUST use the ledger-native body builder for inputs, outputs, fees, validity, metadata, withdrawals, certificates, mint/burn values, and script-witness fields covered by #5288.
- **FR-004**: The remaining certificate call sites in `Shelley.Transaction` MUST use `certificateFromDelegationActionLedger` and `certificateFromVotingActionLedger`.
- **FR-005**: `Cardano.Wallet.Transaction.Delegation` and `Cardano.Wallet.Transaction.Voting` MUST be deleted and pruned from `lib/wallet/cardano-wallet.cabal`.
- **FR-006**: Existing public return types that still expose `Cardano.TxBody` MAY remain until the signing-removal child #5289, but conversion from ledger tx to API tx body must happen after ledger construction.
- **FR-007**: The PR MUST modify no files under `lib/integration/**`.
- **FR-008**: The focused Shelley transaction unit specs, wallet library build, format check, and HLint MUST pass locally before push.

### Key Entities

- **Unsigned builder facade**: The `mkUnsignedTransaction` / `mkUnsignedTx` interface in `Shelley.Transaction`, still consumed by wallet construction helpers and unit specs.
- **Ledger body builder**: The acyclic ledger construction code that both `Shelley.Transaction` and `Transaction.Ledger` can call.
- **Ledger certificate helpers**: `certificateFromDelegationActionLedger` and `certificateFromVotingActionLedger`, returning `[TxCert era]`.
- **ScriptWitnesses**: The #5288 carrier for native input scripts, staking scripts, minting sources, and output reference scripts.
- **Integration-test guard**: The `gate.sh` branch-diff check that fails on `lib/integration/**`.

## Success Criteria

### Measurable Outcomes

- **SC-001**: `rg -n "createTransactionBody|TxBodyContent" lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` returns no matches inside `mkUnsignedTransaction` / `mkUnsignedTx`.
- **SC-002**: `rg -n "Cardano.Wallet.Transaction.(Delegation|Voting)" lib/wallet lib/unit` returns no matches.
- **SC-003**: `git diff --name-only origin/master...HEAD | rg '^lib/integration/'` returns no matches.
- **SC-004**: `./gate.sh` exits 0 at HEAD before the PR is marked ready.
- **SC-005**: The final PR contains exactly one behavior-changing implementation slice for this ticket, plus orchestration commits.

## Assumptions

- Conway is the supported recent era for this migration; Dijkstra remains tracked by #5209.
- #5288's parity proof is the foundation for script-witness correctness. #5285 tests verify the migrated wrapper calls that same ledger builder correctly rather than duplicating the whole #5288 proof.
- Removing the package-level `cardano-api` dependency is out of scope and remains #5290.
- Rewriting signing without cardano-api is out of scope and remains #5289.
