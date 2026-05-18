# Research — Drop cardano-api from Shelley/Transaction.hs and cert helpers

**Branch**: `006-drop-api-shelley-tx` • **Issue**: [#5285](https://github.com/cardano-foundation/cardano-wallet/issues/5285) • **Parent**: [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243)

Inventory gathered by an Explore sub-agent over the master tip (`0821bddb2c`). All line numbers and exact contents below are from the working tree at that commit.

## A. `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` — cardano-api surface

Four `Cardano.Api*` imports drive ~40 distinct symbols. Grouped by Story scope.

### Story 1 — cert helpers (TWO symbols, removable now)

The file imports the two helpers from `Cardano.Wallet.Transaction.Voting` and `Cardano.Wallet.Transaction.Delegation`; switching to the `*Ledger` variants removes both helper imports.

- `Cardano.Wallet.Transaction.Voting.certificateFromVotingAction` — callsites at lines 778, 784.
- `Cardano.Wallet.Transaction.Delegation.certificateFromDelegationAction` — callsites at lines 400, 849, 855.

(These are not `Cardano.Api*` imports themselves, but their internal cardano-api dependencies disappear when the helper modules are deleted.)

### Story 2 — body construction (~25 symbols)

`mkUnsignedTransaction` (line 749) and `mkUnsignedTx` (line ~930) drive the body construction:

| Symbol (qualified `Cardano.`) | Line(s) | Purpose |
|---|---|---|
| `createTransactionBody` | 931 | the body builder itself |
| `TxBodyContent` | 933 | record passed to the builder |
| `BuildTxWith` | 949, 965, 975, 995, 1052, 1170 | field wrapper |
| `TxFeeExplicit` | 1019 | fee field |
| `TxValidityNoLowerBound`, `TxValidityLowerBound`, `TxValidityUpperBound` | 1023-1027 | validity interval |
| `TxMetadataNone`, `TxMetadataInEra`, `makeTransactionMetadata`, `fromShelleyMetadata` | 1030-1034 | metadata |
| `TxAuxScriptsNone`, `TxUpdateProposalNone` | 1037, 1039 | unused fields |
| `SReferenceScript` | 1045 | reference script |
| `PolicyAssets`, `TxMintValue` | 1057, 1066 | minting set |
| `KeyWitnessForSpending`, `KeyWitnessForStakeAddr` | 965, 1171 | witness context |
| `ScriptWitness`, `ScriptWitnessForSpending`, `ScriptWitnessForStakeAddr` | 972, 1009, 1154 | script witnesses |
| `SimpleScript`, `SimpleScriptWitness` | 1006, 1138 | simple scripts |
| `TxInsReferenceNone`, `TxInsReference` | 944-948 | reference inputs |
| `TxInsCollateralNone`, `TxTotalCollateralNone`, `TxReturnCollateralNone` | 986-988 | collateral |
| `TxProtocolParams` | 989 | params |
| `TxScriptValidityNone`, `TxExtraKeyWitnessesNone` | 990, 991 | optional flags |
| `TxCertificates` | 997 | certs field |
| `Certificate` | 317, 330, 906 | `TxPayload._certificates` element type |
| `StakeAddress`, `makeStakeAddress` | 905, 1220, 1228 | withdrawal helper |
| `shelleyToBabbageEraConstraints`, `conwayEraOnwardsConstraints` | 1206, 1251 | era dispatch |
| `Cardano.Api.Certificate.ShelleyRelatedCertificate`, `ConwayCertificate` | 1249-1252 | cert conversion |
| `Cardano.Api.Experimental.Certificate.Certificate` | 1250 | cert conversion |

### Story 3 — signing / witness (~13 symbols)

`signTransaction` (line 462) and the witness builders `mkShelleyWitness` (line 1237), `mkByronWitness` (line 1274), `mkExtraWitness` (line 612):

| Symbol (qualified `Cardano.`) | Line(s) | Purpose |
|---|---|---|
| `Tx`, `TxBody`, `getTxBodyContent` | 319, 348, 492, 506, 575, 1202 | tx/body decomposition |
| `KeyWitness`, `makeSignedTransaction`, `makeShelleyKeyWitness` | 319, 490, 575, 586, 604, 1237 | witness assembly |
| `WitnessPaymentExtendedKey`, `PaymentExtendedSigningKey` | 1242-1243 | extended-key witness |
| `ShelleyBootstrapWitness` | 1274 | bootstrap witness |
| `makeShelleyAddress`, `PaymentCredentialByKey`, `NoStakeAddress` | 612-615 | extra-witness address |
| `Cardano.Api.Byron.mkAttributes`, `AddrAttributes` | 1288-1290 | Byron address attrs |
| `txIns`, `txInsCollateral`, `txWithdrawals`, `txExtraKeyWits`, `txCertificates` | 509-530 | body-content accessors used during signing |
| `NetworkId` (also used by body construction) | 462, 749, 757, … | shared param |

`NetworkId` is shared between Stories 2 and 3 — its last importer is whichever slice removes its final reference.

## B. Cert helpers under `lib/wallet/src/Cardano/Wallet/Transaction/`

### `Voting.hs`

```haskell
module Cardano.Wallet.Transaction.Voting
    ( certificateFromVotingAction
    )
```

```haskell
certificateFromVotingAction
    :: RecentEra era
    -> Either XPub (Script KeyHash)
    -> Maybe Coin
    -> VotingAction
    -> [Cardano.Certificate (CardanoApiEra era)]
```

Internal cardano-api usage: `makeStakeAddressDelegationCertificate`, `StakeDelegationRequirementsConwayOnwards`, `makeStakeAddressRegistrationCertificate`, `StakeAddrRegistrationConway`, `ConwayEraOnwardsConway`, `StakeCredentialByKey`, `StakeCredentialByScript`, `hashScript`, `SimpleScript`.

Callers: `Shelley/Transaction.hs:778,784`. (`Cardano/Wallet.hs` already uses `certificateFromVotingActionLedger` — see lines 639, 2738, 3542 in the current tree.)

### `Delegation.hs`

```haskell
module Cardano.Wallet.Transaction.Delegation
    ( certificateFromDelegationAction
    )
```

```haskell
certificateFromDelegationAction
    :: RecentEra era
    -> Either XPub (Script KeyHash)
    -> Maybe Coin
    -> DelegationAction
    -> [Cardano.Certificate (CardanoApiEra era)]
```

Internal cardano-api usage: same set as `Voting.hs` plus `makeStakeAddressUnregistrationCertificate`.

Callers: `Shelley/Transaction.hs:400,849,855`. (`Cardano/Wallet.hs` already uses `certificateFromDelegationActionLedger` — see lines 638, 2729, 3533 in the current tree.)

## C. Replacements in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs`

Module exports (lines 18-50):

```text
mkTransaction
constructUnsignedTxLedger
sealWriteTx
signTransaction                 -- re-exported from Shelley.Transaction (still cardano-api)
mkShelleyWitnessLedger
mkByronWitnessLedger
TxPayload (..)
txConstraints
TxWitnessTag (..)
txWitnessTagForKey
_txRewardWithdrawalCost
_txRewardWithdrawalSize
certificateFromDelegationActionLedger
certificateFromVotingActionLedger
```

Replacement signatures (Story 1 targets):

- `certificateFromDelegationActionLedger :: RecentEra era → Either XPub (Script KeyHash) → Maybe Coin → DelegationAction → [TxCert era]` (lines 643-698).
- `certificateFromVotingActionLedger :: RecentEra era → Either XPub (Script KeyHash) → Maybe Coin → VotingAction → [TxCert era]` (lines 702-745).

Witness builders (Story 3 partial prerequisite, already present):

- `mkShelleyWitnessLedger :: RecentEra era → Write.TxBody era → (XPrv, Passphrase "encryption") → Keys.WitVKey Keys.Witness` (lines 776-812).
- `mkByronWitnessLedger :: RecentEra era → Write.TxBody era → Cardano.NetworkId → Address → (XPrv, Passphrase "encryption") → BootstrapWitness` (lines 819-844).

**Not yet present**: ledger-native body builder for `mkUnsignedTx` / `mkWithdrawalTx`; ledger-native `signTransaction`. These are the Story 2 and Story 3 prerequisites.

## D. Cabal wiring

`lib/wallet/cardano-wallet.cabal`:

- Library `exposed-modules` includes `Cardano.Wallet.Transaction`, `Cardano.Wallet.Transaction.Built`, `Cardano.Wallet.Transaction.Delegation`, `Cardano.Wallet.Transaction.Voting` (lines 254-257) and `Cardano.Wallet.Shelley.Transaction`, `Cardano.Wallet.Shelley.Transaction.Build`, `Cardano.Wallet.Shelley.Transaction.Ledger` (lines 243-245).
- `build-depends: cardano-api` is in the main library stanza (line 52). No separate ledger-only stanza.

Story 1 prunes `Cardano.Wallet.Transaction.Delegation` and `Cardano.Wallet.Transaction.Voting` from `exposed-modules`. The `cardano-api` build-depends entry is **not** removed in Story 1 — `Shelley/Transaction.hs` still uses it.

## E. Test coverage

Concentrated in `lib/unit/test/unit/Cardano/Wallet/Shelley/`:

- `TransactionSpec.hs` — property tests on `signTransaction` for Byron and Shelley witness tags (`describe "Sign transaction"`, lines 377-412), `SealedTx` serialisation (line 1057), fee-estimation regression (line 1079), Byron/Shelley witness-size calculations (lines 1133-1295), `txConstraints` and withdrawal-cost coverage (lines 1296-1455).
- `TransactionLedgerSpec.hs` — mirrors `TransactionSpec.hs` for the ledger-native path (`mkTransaction`, `sealWriteTx`, `mkShelleyWitnessLedger`, `mkByronWitnessLedger`).

No dedicated test files for `Voting.hs` / `Delegation.hs`; the cert helpers are exercised indirectly via the signing-property tests.

**Implication for Story 1**: the existing property and golden suites are the RED proof. If they pass before and after the switch to `*Ledger` variants, byte-equivalence is established. No new test required unless a coverage gap surfaces (none observed).

## F. Prerequisites for Stories 2 and 3

Foundation signalling in code:

- `Shelley/Transaction/Ledger.hs:47-49` — TODO comment: `mkUnsignedTransaction uses Cardano.NetworkId, Cardano.Certificate, and other cardano-api types pervasively. Needs a ledger-native rewrite.`
- `Shelley/Transaction/Ledger.hs` — #5287 removed the former `mempty -- TODO: minting support` placeholders in `buildLedgerTx` and `buildLedgerTxRaw`.
- No `signTransactionLedger` stub found. `signTransaction` is re-exported from `Shelley.Transaction` (line 26 of `Ledger.hs`) and still uses cardano-api internally.

Story 2 cannot land until script-witness support in `Transaction.Ledger` is complete. Story 3 cannot land until a ledger-native `signTransaction` exists.

## G. Cert flow in `Shelley/Transaction.hs` — what blocks Story 1

Discovered during the implementation pre-flight on commit `dce6abbbf1`: an attempted swap of the cert-builder calls in `Shelley/Transaction.hs` to the `*Ledger` variants fails to compile.

### What goes wrong

`mkUnsignedTransaction` (line 749) builds `votingCerts` and `delegCerts` (the same 5 callsites listed in §A "Story 1") and passes them down to `constructUnsignedTx` at line ~834 in the form `(md, votingCerts)` and equivalent for delegation. `constructUnsignedTx` is the **cardano-api** body builder; its certificate parameter is typed `[ApiCert.Certificate (CardanoApiEra era)]`. The `*Ledger` cert builders return `[Ledger.TxCert era]`. The two are not interchangeable:

```
src/Cardano/Wallet/Shelley/Transaction.hs:834:30: error: [GHC-05617]
    • Could not deduce ‘Ledger.TxCert era
                        ~ ApiCert.Certificate (CardanoApiEra era)’
      …
      Expected: [ApiCert.Certificate (CardanoApiEra era)]
        Actual: [Ledger.TxCert era]
    • In the expression: votingCerts
      In the second argument of ‘constructUnsignedTx’, namely
        ‘(md, votingCerts)’
```

The same shape of error appears at line 410 (delegation path).

### Why `Cardano/Wallet.hs` works without this problem

`Cardano/Wallet.hs` only constructs certs as inputs to `constructUnsignedTxLedger` (the **ledger** body builder) — see lines 2729/2738/3533/3542 feeding into the ledger code path. There the certificate parameter is already `[Ledger.TxCert era]`, so the `*Ledger` cert builders are a type-compatible match. That is why Wallet.hs has been on the `*Ledger` variants since earlier work and why the wrong inference (Story 1 unblocked at the `Shelley/Transaction.hs` callsites too) survived plan + spec review.

### Implication for the story ordering

Earlier sections of this file claimed Story 1 was independent of Story 2. They were wrong. The actual dependency is:

- Story 1 (delete `Voting.hs` + `Delegation.hs`, switch the 5 callsites in `Shelley/Transaction.hs` to `*Ledger` variants) requires `mkUnsignedTransaction` to call the ledger body builder, **which is Story 2**.
- Once Story 2 lands, the helper deletion is the trivial coda — and may even fold into Story 2's commit because the cardano-api cert builders become orphaned by the body-builder swap.
- Story 3 (signing) is unchanged: independent of Stories 1 and 2 once its own prerequisite (ledger-native `signTransaction` rewrite) lands.

Net: the open-blocker list under [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243) blocks **all three** stories under this feature, not just Stories 2 and 3. There is no shippable Story 1 slice today.

## Decisions, Rationale, Alternatives

| Decision | Rationale | Alternative rejected because |
|---|---|---|
| Story 1 is one commit (not split per file). | The two helper deletions, all 7 callsite updates, and cabal prune are interdependent — splitting them would produce a non-buildable intermediate commit and break `git bisect`. | Splitting "delete helper 1" / "delete helper 2" was considered; both helpers and both call clusters are completely independent at the type level but coupled at the cabal level (a single `exposed-modules` block). Single commit is simpler and bisect-safe. |
| Stories 2 and 3 stay in this feature spec but are not implemented in this PR. | They are part of the same logical migration and share `NetworkId`-style cross-symbol dependencies; keeping them in one spec preserves the narrative. They are gated by foundation work tracked separately. | Splitting them into independent specs was considered; rejected because it would duplicate the surface inventory and complicate the dependency story. |
| Existing test suite is the RED proof for Story 1. | Property tests over signing already exercise the cert helpers indirectly via `Sign transaction` cases; byte-equivalence falls out of the suite passing pre/post. | Adding bespoke "cert helper" tests was considered; rejected because they would duplicate the existing property coverage and add maintenance burden for a refactor with zero behavioural change. |
| `cardano-api` build-depends entry is not removed in Story 1. | Other modules in `lib/wallet/` still import `cardano-api`. The entry is removed in whichever slice closes the last importer. | Removing it speculatively was rejected because it would not compile. |
| Top-level `Transaction.hs` is out of scope. | Carries one dead `Cardano.Api.Extra ()` import only; removing it is a type-relocation refactor, not a cardano-api migration. | Including it in the spec was rejected because it is orthogonal to the cardano-api removal effort and would dilute SC-001 / SC-002. |
