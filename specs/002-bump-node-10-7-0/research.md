# Research: Bump dependencies to cardano-node 10.7.0

## Freeze File Analysis

**Source**: `/tmp/cardano-node-10.7.0/cabal.project.freeze`
**Method**: `nix develop -c cabal freeze` in cloned cardano-node 10.7.0

## Key Breaking Change: Ouroboros Package Consolidation

**Decision**: Replace all references to absorbed packages with their new homes.

**Details**: ouroboros-consensus 1.0.0.0 absorbs:
- ouroboros-consensus-diffusion
- ouroboros-consensus-protocol
- ouroboros-consensus-cardano
- lsm, lmdb sublibraries

ouroboros-network 1.1.0.0 absorbs:
- ouroboros-network-api
- ouroboros-network-framework
- ouroboros-network-protocols

**Impact on wallet**: All `.cabal` files referencing these packages need deps updated. Import paths may change but that's a code change, not a bounds change.

**Rationale**: Upstream consolidation, no alternative.

## CHaP Alignment

**Decision**: Use CHaP rev `887d73ce434831e3a67df48e070f4f979b3ac5a6` and index-state `2026-03-23T18:21:55Z`.

**Rationale**: Must match cardano-node 10.7.0 exactly to avoid Windows cross-compilation failures (wine/iserv socket errors from transitive dependency mismatches).

## E2E Probe Results

E2E probe (PR #5248) with only flake.nix bump (no cabal changes):
- Node 10.7.0 built and synced preprod successfully
- Wallet API responded 200 OK
- Test failure was missing `HAL_E2E_PREPROD_MNEMONICS` env var, not a compatibility issue
- Runtime compatibility confirmed

## Source Repository Packages

Need to verify compatibility of pinned source-repository-packages with new ledger versions:
- `cardano-ledger-read` (pin: `0ce0e7a`)
- `cardano-balance-tx` (pin: `98e7f41`)
- `cardano-coin-selection` (pin: `1766110`)

These may need tag bumps if they depend on ledger packages that changed.
