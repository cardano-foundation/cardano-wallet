# Quickstart — Story 1 implementation walkthrough — DEFERRED

**Branch**: `006-drop-api-shelley-tx` • **Status**: implementation deferred — see `research.md` §G

## Why this file is now a stub

Earlier drafts of this quickstart described how to switch the cert-builder calls in `Shelley/Transaction.hs` to the `*Ledger` variants, delete `Voting.hs` + `Delegation.hs`, and prune the cabal `exposed-modules`. An implementation pre-flight on commit `dce6abbbf1` proved the switch does not compile: `mkUnsignedTransaction` passes the cert lists into `constructUnsignedTx` (the cardano-api body builder), which requires `[ApiCert.Certificate (CardanoApiEra era)]`, while the `*Ledger` builders return `[Ledger.TxCert era]`.

Story 1 is therefore blocked transitively on Story 2 (the body-construction migration to `constructUnsignedTxLedger`). After #5287, mint plumbing is available; the remaining blocker is script-witness parity. When Story 2 lands, the helper deletion + cabal prune fold into Story 2's commit; there is no separate Story 1 to hand off.

Full analysis: `research.md` §G ("Cert flow in `Shelley/Transaction.hs` — what blocks Story 1").

## What to do when Story 2 unblocks

1. Re-read `research.md` §A (cardano-api inventory in `Shelley/Transaction.hs`) and §G (cert-flow analysis) — line numbers will have drifted.
2. Implement the Story 2 body-construction migration. The cert-builder switch is mechanical once the body builder takes `[Ledger.TxCert era]`.
3. In the same commit, delete `lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs` and `.../Delegation.hs`, and prune the two `exposed-modules` entries from `lib/wallet/cardano-wallet.cabal`.
4. Run `cabal build cardano-wallet` and `cabal test cardano-wallet-unit:unit --test-options="--match \"Sign transaction\""` from the worktree root; both must be green and the test result byte-equivalent to the pre-Story-2 baseline.

There is no standalone Story 1 commit to plan for in advance.
