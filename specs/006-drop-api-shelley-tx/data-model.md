# Data Model — cardano-api → ledger symbol map

**Branch**: `006-drop-api-shelley-tx`

This document captures the type-level mapping between the `cardano-api` symbols imported by `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` (and the two cert helpers) and their ledger-native replacements. It is the input contract for the implementation tasks.

## Story 1 — Cert helpers

| cardano-api producer | ledger replacement | Replacement location |
|---|---|---|
| `Cardano.Wallet.Transaction.Voting.certificateFromVotingAction` | `Cardano.Wallet.Shelley.Transaction.Ledger.certificateFromVotingActionLedger` | `Shelley/Transaction/Ledger.hs:702-745` |
| `Cardano.Wallet.Transaction.Delegation.certificateFromDelegationAction` | `Cardano.Wallet.Shelley.Transaction.Ledger.certificateFromDelegationActionLedger` | `Shelley/Transaction/Ledger.hs:643-698` |

Return-type change at the callsite:

- Before: `[Cardano.Certificate (CardanoApiEra era)]`
- After:  `[Cardano.Ledger.Api.Tx.Cert.TxCert era]`

Callers consume the result via `TxPayload._certificates`, whose field type is era-parameterised. The change is downstream-compatible because the post-Story-1 callers already construct a ledger-native `TxPayload` in the new code path — see `TxPayload` in `Shelley/Transaction/Ledger.hs:18-50` exports.

## Story 2 — Body construction (mapping; not implemented in this PR)

This table is informational. It is the contract for the prerequisite work in `Transaction.Ledger` (open AC in #5243) and for the eventual Story 2 implementation.

| cardano-api type/value | Ledger-native concept | Notes |
|---|---|---|
| `Cardano.TxBodyContent` | `Cardano.Ledger.Api.Tx.Body.TxBody era` (built via `Build.mkLedgerTx` and friends) | The whole record disappears; field accessors become record-update on the era-specific `TxBody`. |
| `Cardano.createTransactionBody` | `Build.mkLedgerTx` (existing in `Shelley/Transaction/Build`, used today inside `mkTransactionLedger`) | The Ledger path already exists for the simple case; Story 2 extends it to cover all `TxBodyContent` fields. |
| `Cardano.BuildTxWith` | (eliminated; ledger uses concrete values) | The `BuildTxWith` wrapper is a cardano-api-only abstraction; the ledger builder takes the inner value directly. |
| `Cardano.TxFeeExplicit` | `Cardano.Ledger.Coin.Coin` in the era's body fee field | One field on the era's `TxBody`. |
| `Cardano.TxValidityNoLowerBound`, `TxValidityLowerBound`, `TxValidityUpperBound` | `Cardano.Ledger.BaseTypes.StrictMaybe SlotNo` in the era's validity-interval body field | Direct field. |
| `Cardano.TxMetadataNone`, `TxMetadataInEra`, `makeTransactionMetadata`, `fromShelleyMetadata` | `Cardano.Ledger.Shelley.TxAuxData.Metadata era` via the era's `auxDataHash` body field | Aux data is computed off-band; body stores the hash. |
| `Cardano.PolicyAssets`, `Cardano.TxMintValue` | `Cardano.Ledger.Mary.Value.MultiAsset (EraCrypto era)` in the era's mint body field | **Blocked**: `Transaction.Ledger.hs:433,496` currently stub mint as `mempty`. |
| `Cardano.SimpleScript`, `Cardano.SimpleScriptWitness`, `Cardano.ScriptWitness*` | `Cardano.Ledger.Alonzo.Scripts.AlonzoScript era` (or `TimelockScript era` for Mary-era simple scripts) plus the era's witness-set scripts field | **Blocked**: ledger-side script-witness wiring is the other half of the Story 2 prerequisite AC. |
| `Cardano.TxInsReferenceNone`, `TxInsReference`, `SReferenceScript` | The era's `referenceInputs` body field plus `referenceScripts` witness-set field | Direct. |
| `Cardano.TxInsCollateralNone`, `TxTotalCollateralNone`, `TxReturnCollateralNone` | Era's `collateralInputs`, `totalCollateral`, `collateralReturn` body fields | Direct. |
| `Cardano.TxProtocolParams` | (eliminated; passed as argument to the builder, not embedded) | `mkLedgerTx` takes protocol params as a function argument. |
| `Cardano.TxScriptValidityNone`, `Cardano.TxExtraKeyWitnessesNone` | Era's `scriptValidity` body field and `requiredSigners` body field | Direct. |
| `Cardano.TxCertificates`, `Cardano.Certificate` | Sequence of `Cardano.Ledger.Api.Tx.Cert.TxCert era` in the era's `certificates` body field | Same shape as Story 1 cert builders. |
| `Cardano.StakeAddress`, `Cardano.makeStakeAddress` | `Cardano.Ledger.Address.RewardAccount (EraCrypto era)` | Already used elsewhere in the codebase. |
| `Cardano.shelleyToBabbageEraConstraints`, `Cardano.conwayEraOnwardsConstraints` | Class-based dispatch on `RecentEra era` | Already in use in `Transaction.Ledger`. |
| `Cardano.Api.Certificate.ShelleyRelatedCertificate`, `ConwayCertificate`, `Cardano.Api.Experimental.Certificate.Certificate` | `TxCert era` | Subsumed by Story 1's cert mapping. |

## Story 3 — Signing / witness (mapping; not implemented in this PR)

This table is informational; documents the contract for the prerequisite ledger `signTransaction` rewrite.

| cardano-api type/value | Ledger-native concept | Notes |
|---|---|---|
| `Cardano.Tx`, `Cardano.TxBody`, `Cardano.getTxBodyContent` | `Cardano.Ledger.Api.Tx.Tx era`, `Cardano.Ledger.Api.Tx.Body.TxBody era`, direct field access | No-op once the body builder uses the ledger type. |
| `Cardano.KeyWitness`, `Cardano.makeShelleyKeyWitness` | `Cardano.Ledger.Keys.WitVKey 'Witness (EraCrypto era)` plus `mkShelleyWitnessLedger` | `mkShelleyWitnessLedger` already exists at `Shelley/Transaction/Ledger.hs:776-812`. |
| `Cardano.makeSignedTransaction` | Direct construction of `Cardano.Ledger.Api.Tx.Tx era` with populated witness set | Trivial once witness types are unified. |
| `Cardano.WitnessPaymentExtendedKey`, `Cardano.PaymentExtendedSigningKey` | `Cardano.Crypto.Wallet.XPrv` (already in use elsewhere) | Extended-key witnessing uses the existing `mkShelleyWitnessLedger` interface. |
| `Cardano.ShelleyBootstrapWitness` | `Cardano.Ledger.Shelley.TxBody.BootstrapWitness (EraCrypto era)` plus `mkByronWitnessLedger` | `mkByronWitnessLedger` already exists at `Shelley/Transaction/Ledger.hs:819-844`. |
| `Cardano.makeShelleyAddress`, `Cardano.PaymentCredentialByKey`, `Cardano.NoStakeAddress` | `Cardano.Ledger.Address.Addr (EraCrypto era)` constructors | Direct. |
| `Cardano.Api.Byron.mkAttributes`, `AddrAttributes` | `Cardano.Chain.Common.Attributes`, `AddrAttributes` from cardano-ledger-byron | Already a transitive dep. |
| `txIns`, `txInsCollateral`, `txWithdrawals`, `txExtraKeyWits`, `txCertificates` field accessors | Era-specific `TxBody` lenses / pattern-matching | Direct. |
| `Cardano.NetworkId` | `Cardano.Ledger.BaseTypes.Network` | Already used elsewhere; the rename is mechanical. |

## Invariants

- The exports of `Cardano.Wallet.Shelley.Transaction` MUST NOT change in this feature. Its public callers (`Cardano.Wallet`, the API layer, integration tests) see no signature changes. Internal call paths only.
- The exports of `Cardano.Wallet.Transaction` (the interface module) are explicitly out of scope and untouched.
- `TxPayload` (defined in `Shelley/Transaction/Ledger.hs:18`) is parameterised over `era` — its `_certificates` field type stays compatible because `*Ledger` cert builders already return `[TxCert era]`.
