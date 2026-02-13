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

1. Choose the target cardano-node version (e.g. 10.2.0)
2. Clone/checkout all Cardano dependency repos at matching tags
3. Extract changelogs between current and target versions
4. Align `flake.lock` CHaP with cardano-node's CHaP
5. Freeze dependencies
6. Determine sublibrary order (`cabal-plan topo`)
7. Update `.cabal` files in topological order (one commit per sublibrary)
8. Update `cabal.project` (index-state, source-repository-package)
9. Update the cardano-node input in `flake.nix`
10. Final summary commit

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
