# tasks — #5411

One slice, `S-1`. The whole boundary lands together because A-001 constraint 3
requires the return-type change and both consumers in the same commit, and
because removing `Cardano/Wallet.hs`'s pragma is only correct once **both**
deprecated causes in that module are gone.

The OWNER campaign's local RED/GREEN commits are unpushed provenance; the
accepted candidate is squashed into one behaviour commit, which satisfies
constraint 3 by construction.

## S-1 — retire the transaction-body round-trips and their suppressions

### Proof

- [x] **T-1** RED: a differential check over the four fields — inputs, outputs,
      collateral, withdrawals — comparing the values read from the ledger
      transaction against the values the `cardano-api` path produces, over the
      same generated inputs. Must fail before the migration and pass after
      (INV-3).
- [x] **T-2** RED: the check is shown able to fail for the right reason —
      perturb one field's source and observe that exact field's comparison go
      red, not merely "something failed".

### Production

- [x] **T-3** Delete the four-field `cardano-api` round-trip in
      `buildCoinSelectionForTransaction`; read the four values from the
      `Write.Tx era` argument it already takes. No new type, no new helper
      grouping the four (INV-2).
- [x] **T-4** Retire `Cardano.TxBody` from the producer chain:
      `mkUnsignedTx`, `constructUnsignedTx`, `mkUnsignedTransaction`
      (`Shelley/Transaction.hs`), and the two `Cardano.Wallet` signatures
      `constructTransaction` / `constructUnbalancedSharedTransaction`.
      Result types only; argument names and types unchanged.
- [x] **T-5** Drop the converter application at the two consumers,
      `lib/api/.../Shelley/Server.hs:3177` and `:3650`, and the now-unused
      `fromCardanoApiTx` import at `:169`. **Nothing else under `lib/api/**`.**
- [x] **T-6** All four of T-4's `Wallet.hs` signatures and T-5's call sites, or
      none. Half the boundary leaves the pragma in place and the diff
      misleading.

### Suppressions

- [x] **T-7** Remove `{-# OPTIONS_GHC -Wno-deprecations #-}` from
      `lib/wallet/src/Cardano/Wallet.hs` and build under a **named** sanctioned
      path. Under `-Werror` a live suppression fails the build, so a green build
      with the pragma absent is the evidence.
- [x] **T-8** Same for
      `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`.
- [x] **T-9** Same for
      `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs`.
- [x] **T-10** Report what the build-based measurement adds beyond T-7..T-9, if
      anything. **Not a number inherited from the issue or the brief.** If a
      module's suppression turns out to have a non-`cardano-api` cause as well,
      it is **not** cleared by deleting the pragma — narrow the cause and report
      it; deleting it would break the build under `-Werror`.

### Evidence

- [x] **T-11** Name which sanctioned path each receipt used —
      `--flags=release` (`nix/haskell.nix:277-278`) or `just build`
      (`justfile:50-51`). A bare `cabal build` is not a gate.
- [x] **T-12** Report both suppression populations separately, each with its
      instrument stated. A single total lets a partial fix read as complete.
- [x] **T-13** Re-run the converter-orphaning check **after** the change:
      neither `toCardanoApiTx` nor `fromCardanoApiTx` may be left without
      callers. If either is, **stop and report** — `Cardano/Api/Extra.hs` is
      #5290's to delete, not this ticket's.
- [x] **T-14** Unit tests covering the migrated surfaces pass.

## Out of scope, restated as checkable absences

- [x] **T-15** No `Cardano.Api.Experimental` import anywhere in the diff.
- [x] **T-16** No file under `lib/integration/**` in the diff.
- [x] **T-17** `lib/wallet/src/Cardano/Api/Extra.hs` unmodified.

## S-0 — legibility prerequisite (landed before S-1)

Authored by the ticket owner under an operator-directed exception to the
ticket-owner fence on production code, and recorded as such.

- [x] **T-18** Replace the wildcard `import qualified Cardano.Api as Cardano` in
      `Cardano/Wallet.hs`, `Cardano/Wallet/Shelley/Transaction.hs` and
      `Api/Http/Shelley/Server.hs` with explicit lists naming exactly the symbols
      each module uses, so the S-1 migration shows up as deleted names rather
      than an unchanged import line. Behaviour-preserving; verified under
      `-Werror`, which is what proves the lists are not over-broad. The repo-wide
      sweep of the remaining wildcards is #5414.
