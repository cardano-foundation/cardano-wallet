# Upgrade cardano-node to the 11.2 pre-release

**Branch**: `sl/node-11.2` (existing user worktree)
**Created**: 2026-10-28
**Status**: Implemented and validated on Linux
**Input**: Update cardano-node to the 11.2 pre-release.

## User story (P1)

Developers and packaged wallet users receive the upstream 11.2.0 pre-release
node through the repository's existing Nix runtime outputs.

### Acceptance scenarios

1. Given a checkout with its lock file, building `.#cardano-node` selects
   the verified upstream 11.2.0 tag and prints version 11.2.0.
2. Given the existing wallet and local cluster, the new runtime accepts
   their configuration and negotiates a supported node-to-client version.

## Requirements

- Pin the upstream pre-release reproducibly in `flake.nix` and `flake.lock`.
- Preserve existing output wiring for native, musl and Windows packages.
- Check runtime/client compatibility before changing wallet library pins;
  update those dependencies only when required for this runtime upgrade.
- Record build and integration validation, including any blockers.

## Edge cases and success criteria

11.2.0 accepts legacy flat configs but removes node-to-client versions below
V23. Confirm the wallet's protocol support and exercise a local cluster.
Success requires the selected node version and successful local interaction;
cross-platform builds remain separate checks unless actually executed.
