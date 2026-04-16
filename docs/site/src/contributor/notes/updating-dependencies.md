# Updating Dependencies

If you use [Nix](nix.md)
to manage build and runtime dependencies you can be
confident that you will always have the correct versions for the
branch you are on, and that these will be exactly the same as those
versions used in CI.

It is possible to specify any git revision for the dependency and Nix
will automatically build it -- unless it has already been built
in a nix cache -- in which case the build result will be downloaded instead.

## nix develop

The default `nix develop` contains build tools, utilities and GHC
configured with a global package-db which matches `cabal.project`. This
is defined in the `devShells.default` attribute of `flake.nix`.

## nix flake lock

[`nix flake`](https://nixos.wiki/wiki/Flakes) manages the file [`flake.lock`](https://github.com/cardano-foundation/cardano-wallet/blob/master/flake.lock).

## Bumping cardano-node

This is the most involved dependency update. The cardano-wallet ecosystem depends on many Cardano Haskell libraries that must all be updated in lockstep.

### Overview

1. Choose the target cardano-node version (e.g. 10.7.0)
2. Clone the target cardano-node tag and freeze its dependencies (`cabal freeze`)
3. Align `flake.lock` CHaP with cardano-node's CHaP
4. Update `cabal.project` index-state, constraints, and source-repository-package pins
5. Bump upstream source-repository-packages (cardano-ledger-read, cardano-balance-tx) if they depend on changed packages
6. Handle package consolidation (replace absorbed packages with sublibrary syntax in `.cabal` files)
7. Determine sublibrary order (`cabal-plan topo`)
8. Create the commit layout before making any code changes. Use `cabal-plan topo` to get the sublibrary order, then create one empty stgit patch per sublibrary:
   ```bash
   stg new -m "fix: adapt cardano-numeric to 10.7.0"
   stg new -m "fix: adapt text-class to 10.7.0"
   stg new -m "fix: adapt cardano-wallet-launcher to 10.7.0"
   # ... one per sublibrary in topological order
   ```
   This defines where each library's changes will go. Work proceeds by going to each patch (`stg goto`), making changes, and refreshing (`stg refresh`).
9. Build each sublibrary in topological order (one commit per sublibrary). Each commit must pass the full quality gate before moving to the next:
   - `fourmolu --mode check` on changed `.hs` files — formatting
   - `hlint` on changed `.hs` files — linting
   - `cabal-fmt -i` on changed `.cabal` files — cabal formatting
   - `nix build --quiet .#<target>` for each affected flake output — **this is the authoritative build check**. Do NOT rely on `cabal build` alone; cabal's incremental build caches compiled modules and misses errors in unchanged-but-transitively-affected files. The nix build compiles from scratch every time and matches CI exactly.

   The nix build targets grow as you fix more libraries. At each patch, include only the targets that are fixed so far:
   ```bash
   # After fixing cardano-wallet-primitive:
   nix build --quiet .#unit-cardano-wallet-primitive
   # After fixing all libraries:
   nix build --quiet .#cardano-wallet .#local-cluster .#integration-exe \
     .#test-local-cluster-exe .#unit-cardano-wallet-unit \
     .#unit-cardano-numeric .#unit-cardano-wallet-primitive \
     .#unit-cardano-wallet-secrets .#unit-cardano-wallet-test-utils \
     .#unit-cardano-wallet-launcher .#unit-cardano-wallet-network-layer \
     .#unit-cardano-wallet-application-tls \
     .#unit-cardano-wallet-blackbox-benchmarks .#unit-delta-chain \
     .#unit-delta-store .#unit-delta-table .#unit-delta-types \
     .#unit-std-gen-seed .#unit-wai-middleware-logging \
     .#unit-benchmark-history .#wallet-key-export .#wallet-key-export-test
   ```
10. Update the cardano-node-runtime input in `flake.nix`
11. Update local cluster configs (genesis files, node config) for the new node version
12. Run integration tests and E2E tests

### Cardano Haskell Packages (CHaP)

CHaP is the custom Hackage repository for Cardano packages, hosted at `https://chap.intersectmbo.org/`. The `index-state` in `cabal.project` controls which package versions are visible:

```cabal
index-state:
  , hackage.haskell.org 2025-01-01T23:24:19Z
  , cardano-haskell-packages 2025-03-01T00:00:00Z
```

**Critical**: use the same CHaP revision as cardano-node to avoid Windows cross-compilation failures (wine/iserv socket errors caused by transitive dependency mismatches).

```bash
# Get cardano-node's CHaP rev
cd /code/cardano-node && git checkout <target-version>
cat flake.lock | jq '.nodes.CHaP.locked.rev'

# Update cardano-wallet to use same CHaP
nix flake update CHaP --override-input CHaP \
  github:intersectmbo/cardano-haskell-packages/<rev>
```

### Dependency bounds

Standard format for version bounds in `.cabal` files:

```cabal
build-depends:
    aeson >= 2.1.2.1 && < 2.2
  , base >= 4.18 && < 5
  , cardano-api >= 10.0 && < 10.1
```

Lower bound = exact version from freeze; upper bound = next minor version.

### Source repository packages

For packages not yet on CHaP or when you need a specific commit:

```cabal
source-repository-package
    type: git
    location: https://github.com/IntersectMBO/cardano-addresses
    tag: 2bca06deaa60e54a5322ac757387d744bf043367
    --sha256: 1y1mzfly7jac40b9g4xc078rcm5zqhc3xxv77kwxi10yph1jwq7z
    subdir: command-line
            core
```

Get the SHA256 with `nix flake prefetch github:owner/repo/commit-sha`.
Then convert to nix32: `nix hash convert --to nix32 'sha256-xxx='`.

### Package consolidation

When upstream packages are absorbed into parent packages (e.g. ouroboros-consensus 1.0.0.0 absorbing ouroboros-consensus-cardano), downstream `.cabal` files must replace the old package names with sublibrary syntax using curly braces:

```cabal
-- Before (standalone packages):
build-depends:
    ouroboros-consensus
  , ouroboros-consensus-cardano
  , ouroboros-consensus-protocol
  , ouroboros-network
  , ouroboros-network-api
  , ouroboros-network-framework

-- After (sublibraries):
build-depends:
    ouroboros-consensus:{ouroboros-consensus, cardano, protocol}
  , ouroboros-network:{ouroboros-network, api, framework}
```

The syntax is `package:{sublib1, sublib2}` with curly braces. Multiple sublibraries from the same package can be grouped. The main library must be listed explicitly (e.g. `ouroboros-consensus` inside the braces).

**Import paths do not change** — the sublibraries re-export from the same module paths as before.

haskell.nix handles this syntax natively via Cabal 3.0+.

When packages are consolidated, their transitive dependencies may also need explicit constraints in `cabal.project` (e.g. `typed-protocols`, `network-mux`, `fs-api`).

### Local cluster configs

When bumping to a new node version, check if the local cluster configs need updating:

- New era genesis files (e.g. `dijkstra-genesis.json`) must be added to `lib/local-cluster/test/data/cluster-configs/` and wired through `GenesisFiles.hs` and `GenNodeConfig.hs`
- The shelley genesis `ppProtocolVersionL` must be high enough for the node's `cardanoProtocolVersion` — otherwise forged blocks are rejected with `HeaderProtVerTooHigh`
- `ExperimentalHardForksEnabled` may need to be toggled depending on the node version

## CHaP-only dependency bump

When a Cardano library updates on CHaP without a full cardano-node version bump:

1. Bump the CHaP `index-state` in `cabal.project` to a timestamp that includes the new package version
2. Update version bounds in affected `.cabal` files
3. Build and test: `cabal build all -O0`

## GHC bump

1. Update the `haskell.nix` pin in `flake.lock`
2. Set the compiler version in `flake.nix`
3. Fix build errors (imports, extensions, warnings-as-errors)
4. Update the Windows cross-compilation overlay if needed
5. Re-enable or disable tools that depend on GHC version (e.g. HLS, weeder)
