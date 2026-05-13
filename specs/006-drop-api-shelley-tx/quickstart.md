# Quickstart — Story 1 implementation walkthrough

**Branch**: `006-drop-api-shelley-tx` • **Story**: 1 (cert helpers, unblocked)

This is the on-ramp for the unblocked first slice. Stories 2 and 3 stay parked until their prerequisite acceptance criteria in [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243) land.

## Pre-flight

```bash
cd /code/cardano-wallet-5285
git status                              # clean working tree expected
git log --oneline -1                    # docs(5285): spec, plan, ...
```

Confirm `Transaction.Ledger` already exports the replacements:

```bash
grep -nE "certificateFrom(Voting|Delegation)ActionLedger" \
  lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs
```

Expected: two hits in the export list (around lines 18-50) plus their definitions (lines 643-745).

## Edits (single commit)

### 1. Switch the 5 callsites in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`

| Line | Old call | New call |
|---|---|---|
| 400 | `certificateFromDelegationAction era cred deposit action` | `certificateFromDelegationActionLedger era cred deposit action` |
| 778 | `certificateFromVotingAction era cred deposit action` | `certificateFromVotingActionLedger era cred deposit action` |
| 784 | (second voting cite — verify exact form in source) | `*Ledger` variant |
| 849 | (delegation site) | `*Ledger` variant |
| 855 | (delegation site) | `*Ledger` variant |

Update the imports near the top of the file: remove `Cardano.Wallet.Transaction.Voting` and `Cardano.Wallet.Transaction.Delegation`; add the two `*Ledger` names to the existing `Cardano.Wallet.Shelley.Transaction.Ledger` import group.

The return-type change at callsites is `[Cardano.Certificate (CardanoApiEra era)] → [TxCert era]`. Inspect each caller to confirm it stores into `TxPayload._certificates` (which is already era-parameterised on the ledger type after #5270).

### 2. `Cardano/Wallet.hs` — no edit needed

Confirmed during pre-flight (commit `73b1088113`): `lib/wallet/src/Cardano/Wallet.hs` already imports the `*Ledger` names (lines 637-642) and its 4 callsites at 2729 (delegation), 2738 (voting), 3533 (delegation), 3542 (voting) already use the `*Ledger` variants. An earlier draft of this file claimed Wallet.hs needed switching; that was a Phase-0 inventory error and has been corrected. Skip this section.

### 3. Delete the helper files

```bash
git rm lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs
git rm lib/wallet/src/Cardano/Wallet/Transaction/Delegation.hs
```

### 4. Prune cabal `exposed-modules`

In `lib/wallet/cardano-wallet.cabal`, remove these two lines (around 256-257):

```text
Cardano.Wallet.Transaction.Delegation
Cardano.Wallet.Transaction.Voting
```

Do NOT remove `cardano-api` from `build-depends` — `Shelley/Transaction.hs` is still a consumer.

## Verification (the slice's gate)

Run from the worktree root:

```bash
nix develop --quiet -c \
  cabal build cardano-wallet 2>&1 | tail -30
```

Expected: clean build, no warnings about missing modules.

```bash
nix develop --quiet -c \
  cabal test cardano-wallet-unit:unit \
    --test-options="--match \"Sign transaction\""
```

Expected: all signing-property cases pass. This is the RED proof — the same suite passes before and after the switch because the `*Ledger` builders are byte-equivalent on the cases it exercises.

```bash
git grep -nE "^import.*Cardano\.Wallet\.Transaction\.(Voting|Delegation)" lib/
```

Expected: empty (no remaining importers of the deleted modules).

```bash
git grep -nE "^import.*Cardano\.Api" lib/wallet/src/Cardano/Wallet/Transaction/
```

Expected: empty (the `Transaction/` subdirectory no longer contains any cardano-api importer).

## Commit message

Conventional Commits per the constitution. Single commit, vertical, bisect-safe:

```text
refactor(wallet)!: drop cardano-api cert helpers (#5285)

Switch the 5 cert-helper callsites in Shelley/Transaction.hs to the
ledger-native certificateFromDelegationActionLedger /
certificateFromVotingActionLedger introduced in #5270. Delete the two
helper modules and prune their cabal exposed-modules entries.

(Cardano/Wallet.hs already used the *Ledger variants from earlier work;
no edit there.)

Behaviour-preserving; existing signing property suite is the regression
proof.

Refs: #5285, #5243
```

`!:` only if any export is removed that downstream packages outside `lib/wallet/` would notice — verify by running `cabal build all` over the monorepo. If only internal imports change, drop the `!`.

## Done criteria

- [ ] `cabal build cardano-wallet` clean.
- [ ] `cabal test cardano-wallet-unit:unit` green.
- [ ] No grep hits for the two deleted module names anywhere under `lib/`.
- [ ] No grep hits for `Cardano.Api*` imports under `lib/wallet/src/Cardano/Wallet/Transaction/`.
- [ ] `cardano-wallet.cabal` `exposed-modules` block is shorter by exactly two entries.
- [ ] Single commit on the branch, conventional message, no fixups.

## Next

Push the slice; the PR description on [#5286](https://github.com/cardano-foundation/cardano-wallet/pull/5286) gets updated to reflect Story 1 landing. Stories 2 and 3 wait for their prerequisites to land in `Transaction.Ledger`.
