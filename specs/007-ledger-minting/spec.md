# Feature Specification: Ledger Body Builder Minting Support

**Feature Branch**: `007-ledger-minting`
**Created**: 2026-05-13
**Status**: Draft
**Input**: User description: "Minting support in Cardano.Wallet.Shelley.Transaction.Ledger body builder. Issue #5243. Follows PR #5286 (branch 006-drop-api-shelley-tx), which documented that the migration of `mkUnsignedTransaction` off cardano-api cannot ship while the ledger-based body builder silently drops minting. Smallest first slice: plumb the mint set through the ledger body builder and translate the wallet's mint representation to the ledger's. Script-witness support is a separate later feature."

## Context

Issue [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243) tracks the wallet's migration off `cardano-api` in the Shelley transaction layer. Story 1 of issue [#5285](https://github.com/cardano-foundation/cardano-wallet/issues/5285) — switching `mkUnsignedTransaction` to the ledger-based body builder — was documented in PR [#5286](https://github.com/cardano-foundation/cardano-wallet/pull/5286) as blocked: the ledger body builder hardcodes the transaction's mint field to empty, so the migration would silently drop every mint request that reaches it. This feature removes that structural blocker. It does not perform the migration itself, and it does not add support for the script witnesses that an actual on-chain mint also requires.

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Wallet's ledger body builder honours the mint requested by its caller (Priority: P1)

When upstream wallet code asks the ledger-based transaction body builder to construct a transaction body that includes a mint (one or more native-token policies, each with assets and quantities to mint or burn), the resulting transaction body's mint field reflects exactly what the caller asked for, asset-for-asset and quantity-for-quantity.

**Why this priority**: Without this, the migration of `mkUnsignedTransaction` from the `cardano-api` body builder to the ledger body builder cannot proceed — any mint request reaching the new path would be silently dropped, producing a wrong transaction body without warning. This is the single blocker called out by PR [#5286](https://github.com/cardano-foundation/cardano-wallet/pull/5286).

**Independent Test**: Drive the ledger body builder with a representative mint set (one policy with several assets, mixed positive and negative quantities) and inspect the produced transaction body's mint field. The mint field must equal the caller's mint set, translated into the ledger's mint representation, with no assets added, dropped, renamed, or rescaled.

**Acceptance Scenarios**:

1. **Given** a caller that passes an empty mint set, **When** the ledger body builder constructs the transaction body, **Then** the body's mint field is empty (identical to the previous hardcoded behaviour).
2. **Given** a caller that passes a single-policy, single-asset mint with a positive quantity, **When** the ledger body builder constructs the transaction body, **Then** the body's mint field contains that policy, that asset, with that exact positive quantity, and nothing else.
3. **Given** a caller that passes a multi-policy mint mixing positive and negative quantities, **When** the ledger body builder constructs the transaction body, **Then** every policy, asset, and signed quantity appears unchanged in the body's mint field.
4. **Given** the two surface forms of the ledger body builder (the raw form and the higher-level form), **When** both are invoked with the same mint set on the same inputs, **Then** both produce transaction bodies whose mint fields are equal.

---

### User Story 2 — Migration of `mkUnsignedTransaction` to the ledger path is unblocked on minting (Priority: P1)

Once this feature lands, the only remaining minting-related blocker for switching `mkUnsignedTransaction` off `cardano-api` is the separate question of script witnesses. A reviewer of the follow-up migration PR can verify that minting is no longer silently dropped without reading any code outside the migration diff.

**Why this priority**: This is the why-it-matters for User Story 1. Independently testable in the sense that the follow-up migration PR can be opened and have its minting-related risk reviewed without needing further changes to the ledger body builder.

**Independent Test**: Switch `mkUnsignedTransaction` to the ledger body builder on a branch and exercise it with the same mint scenarios as User Story 1. The resulting transaction body's mint field must agree with the input mint set on every scenario from User Story 1.

**Acceptance Scenarios**:

1. **Given** a follow-up branch that wires `mkUnsignedTransaction` to the ledger body builder, **When** it is invoked with a non-empty mint set, **Then** the produced transaction body's mint field reflects that set (rather than being empty as it would be today).

---

### Edge Cases

- **Empty mint set**: Must produce the same transaction body as today's hardcoded-empty behaviour, so that all non-minting flows are unaffected.
- **Burn-only mint** (all-negative quantities): Must appear in the body's mint field with the negative quantities preserved; not coerced, not filtered.
- **Zero-quantity entries** in the input mint set: Handled the same way the wallet handles them today in the rest of its pipeline (no new policy on quantity normalisation is introduced by this feature).
- **Multiple policies in a single mint set**: All policies must appear in the output mint field, none dropped, none merged.
- **Round-tripping**: The translation from the wallet's mint representation to the ledger's mint representation must not lose information for any input the wallet can produce.
- **Existing non-minting callers**: The change must not alter callers that pass no mint; their behaviour is preserved exactly.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The ledger body builder MUST accept a caller-provided mint specification as a parameter rather than hardcoding the mint field of the transaction body.
- **FR-002**: The ledger body builder MUST translate the wallet's native mint representation into the ledger's mint representation without losing or altering any policy, asset name, or signed quantity.
- **FR-003**: When the caller provides an empty mint specification, the resulting transaction body's mint field MUST be empty, matching today's behaviour exactly.
- **FR-004**: When the caller provides a non-empty mint specification, the resulting transaction body's mint field MUST be equal — under the chosen translation — to the caller's mint specification, with no extra assets and no missing assets.
- **FR-005**: Both the raw and the higher-level surface of the ledger body builder MUST honour the mint specification consistently; a fixed mint specification must yield equal mint fields on both surfaces.
- **FR-006**: Existing callers that today rely on the hardcoded empty mint MUST continue to compile and produce unchanged transaction bodies after the change.
- **FR-007**: The hardcoded "TODO: minting support" placeholder in the ledger body builder MUST be removed; no equivalent silent-drop site may remain.
- **FR-008**: This feature MUST NOT add script-witness handling for mint scripts; that capability is explicitly deferred to a separate later feature.
- **FR-009**: The translation from the wallet mint representation to the ledger mint representation MUST be exercised by automated tests covering at minimum: empty input, single-policy single-asset, single-policy multi-asset, multi-policy, and mixed positive/negative quantities.

### Key Entities

- **Wallet mint specification**: The wallet's existing in-process representation of a set of mints — for each policy, the assets and signed quantities to mint or burn. Already produced by upstream code paths today, but currently discarded before reaching the ledger body builder.
- **Ledger mint field**: The transaction body's native-asset mint field as the ledger sees it. The on-chain contract of the transaction. Currently always empty in bodies produced by the ledger body builder.
- **Mint translation**: The mapping from the wallet mint specification to the ledger mint field. New artefact introduced by this feature; must be total and information-preserving for all inputs the wallet can produce.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For every input mint specification covered by the test scenarios, the ledger body builder's output mint field matches the input under the documented translation. No silent drops.
- **SC-002**: No call site in the ledger body builder still hardcodes the transaction body's mint field to empty. A repo-wide search for the previous placeholder returns zero hits.
- **SC-003**: A follow-up PR that switches `mkUnsignedTransaction` to the ledger body builder can be opened with no further changes required in the ledger body builder for minting; the remaining blocker is only script-witness support.
- **SC-004**: All pre-existing wallet tests that build transactions without minting continue to pass without modification.
- **SC-005**: A reviewer can read the diff for this feature in under fifteen minutes and verify by inspection that (a) the mint parameter is plumbed end-to-end, (b) the translation is total, and (c) no other behaviour is altered.

## Out of Scope

- **Script-witness support for mints**: A mint that has any effect on-chain requires a Plutus or native-script witness. Producing such witnesses is deferred to a separate later feature and is explicitly not delivered here. Transaction bodies built by this feature with a non-empty mint and no script witnesses will not validate on-chain on their own; that is acceptable, because this feature's only contract is structural plumbing.
- **Migration of `mkUnsignedTransaction` itself**: Switching `mkUnsignedTransaction` from the `cardano-api` body builder to the ledger body builder is the consumer of this feature; it lives in the follow-up Story 1 work of [#5285](https://github.com/cardano-foundation/cardano-wallet/issues/5285), not here.
- **New user-facing minting flows or APIs**: This feature does not change any HTTP API, CLI, or wallet behaviour that an external user can observe today.

## Assumptions

- The wallet already produces a mint specification in its existing internal representation at the call sites that reach the ledger body builder; this feature only stops discarding it. (PR [#5286](https://github.com/cardano-foundation/cardano-wallet/pull/5286)'s description confirms the upstream of the ledger body builder is already mint-aware.)
- The ledger's mint type is a strict superset of what the wallet's mint representation can express, so the translation is total without loss. If a counterexample is found during planning, it becomes a planning-phase decision (e.g., either widen the wallet representation or document the asymmetry).
- It is acceptable to ship a body builder that produces mints without script witnesses, given that the only consumer today is the still-blocked migration; no production code path will exercise the new mint output until script-witness support also lands.
- "Script-witness support" is a single, separable follow-up feature and not entangled with this one — i.e., adding witnesses later will not require revisiting the mint-translation introduced here.
