# Plan: cardano-node 11.2 pre-release

Use the existing `sl/node-11.2` worktree. Number 70005 follows the inspected
branches, worktrees and specs (including the abandoned 70002 variants).

## Technical context and design

Nix exposes the runtime through `cardano-node-runtime` independently of the
wallet's Cabal libraries. Upstream tag `11.2.0` resolves to
`dac0d7ab2dfdee1f9eda901589e3c6ac8eaa1899` and is marked as a pre-release.
Update that input and let Nix regenerate its transitive lock entries. Inspect
the diff for changes outside that input's dependency graph.

Review the existing network client and flat local-cluster configuration for
the upstream V23 minimum and configuration compatibility. Build the runtime
and CLI, then exercise the project's local-cluster validation with the wallet.
If compatibility requires library changes, document the dependency changes
and perform the relevant build and test gates before claiming completion.

Source review found the wallet caps its handshake at V22. The pinned
`ouroboros-consensus-4.2.1.0` already maps V23 to
`CardanoNodeToClientVersion19`, so extend the wallet's advertised range to
V23 without changing its library pins. Existing `withNetwork` cluster tests
exercise negotiation, queries and block streaming with the new runtime.

The first runtime tests failed before negotiation because ledger 0.4 requires
additional Dijkstra genesis fields. The user approved updating the cluster
genesis after that failure. Use the complete testnet Dijkstra template from
the verified node 11.2.0 tag, including its Plutus V4 cost model; its four
previously present reference-script values are unchanged. The cluster copies
this JSON as a value, so its older ledger libraries do not discard new keys.

Verify both runtime generations: the advertised V16–V23 range must still
negotiate with node 11.1.3, while node 11.2.0 requires V23.

## Constitution check

This is maintenance work, with no API, key-handling or storage changes.
Nix remains the reproducible source of runtime dependencies; Cabal remains
the source of wallet library constraints. Run Nix commands with `--quiet`.
Do not claim unexecuted integration or cross-platform checks passed.

## Sources

- https://github.com/IntersectMBO/cardano-node/releases/tag/11.2.0
- Upstream tag's `cabal.project` and `flake.lock`.
- https://github.com/IntersectMBO/cardano-node/blob/11.2.0/configuration/cardano/testnet-template-dijkstra.json
