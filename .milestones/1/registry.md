# Contract registry — cardano-wallet M1 desk

contract:   c1 tx-layer seam between cardano-api removal (M1) and
            ledger/node version bumps (M2/M6)
parties:    M1 arc (rewrites Transaction.Ledger, SealedTx, signing),
            M2/M6 arc (bumps cardano-ledger/node pins the same modules build on)
invariant:  one arc rebases on the other by explicit desk decision; both
            lanes never mutate the tx layer concurrently
enforced:   NONE — sequencing is a desk arbitration (parked decision)

contract:   c2 wallet tracks the mainnet protocol version
parties:    cardano-wallet (pins node/ledger deps), Cardano mainnet
            (advances by governance HF)
invariant:  wallet's supported protocol version >= mainnet's enacted version
enforced:   NONE — breach found by hand 2026-07-29 (mainnet on v11 since
            2026-07-20, wallet pins node 10.x). Candidate check: CI job that
            queries a public tip and fails when protocolVersion exceeds the
            supported ceiling. Commission under M2 or M3.

contract:   c3 wallet source-repository-package pins onto upstream mains
parties:    cardano-wallet (pins), cardano-foundation/cardano-balance-transaction,
            cardano-foundation/cardano-ledger-read,
            cardano-foundation/cardano-coin-selection (publish main)
invariant:  every pin targets a commit reachable from the upstream repo main
            (never a branch), and for M2 those mains build against
            cardano-ledger-conway >= 1.22.1.0 / node 11.0.1 CHaP index
            BEFORE any wallet ledger-line re-pin (11.0.1: DONE, verified 2026-07-29; NEXT trigger: Dijkstra / node 12). Historical blocker:
            balance-transaction (10.7 bump). cardano-addresses (#5293) is the
            fourth upstream seam (published version, not a git pin).
enforced:   partial — pins-main-only is desk law but has no CI check; the
            build order is epic-owner sequencing. Candidate check: CI verifying
            each pin's tag is an ancestor of upstream main.

recurring:  gate.sh pre-commit lint vs `just check-fmt` CI wrapper — the
            wrapper ends with `git diff --exit-code` (clean-worktree
            assertion) so it cannot gate an uncommitted slice; every wallet
            lane must use direct check-mode tools (fourmolu/cabal-fmt/nixfmt
            --check + hlint) inside the nix dev shell. First hit: t5088
            Q-001 (2026-07-29). Candidate consolidation: a `just
            check-fmt-tree` recipe upstreamed in cardano-wallet so gates and
            CI share one path.
