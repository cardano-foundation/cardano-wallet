---
name: cardano-deps
description: Cardano dependencies management, CHaP, source-repository-package, version bumps
---

# Cardano Dependencies Management

Managing Cardano Haskell dependencies is complex due to the interconnected ecosystem. This skill provides the workflow for updating dependencies reliably.

## Dependency Update Workflow

### 1. Choose Target cardano-node Version

User decides which cardano-node version to align with (e.g., 10.1.0, 10.2.0).

### 2. Clone and Checkout Cardano Dependencies

All Cardano dependencies must be cloned locally and checked out to the correct tag.

**Required repos** (in `/code/`):
- `cardano-node`
- `cardano-api`
- `cardano-ledger`
- `cardano-base`
- `ouroboros-network`
- `ouroboros-consensus`

For each repo:
```bash
cd /code/<repo>
git fetch --tags
git checkout <tag>  # matching the target cardano-node version
```

Check cardano-node's `cabal.project` for the exact versions/tags of each dependency.

### 3. Extract Changelogs

For each Cardano repo, extract the changelog between current and target version:

```bash
cd /code/<repo>
git log --oneline <current-tag>..<target-tag>
```

Create a combined changelog document (e.g., `bump-changelog.md`) that:
1. Lists changes from all repos
2. Orders entries chronologically
3. Groups by repo for reference

Commit this document. It provides a unified view of all ecosystem changes.

### 4. Freeze Dependencies

```bash
cd /code/cardano-node
cabal update
cabal freeze
```

This generates `cabal.project.freeze` with exact versions of all dependencies.

### 5. Determine Sublibrary Order

Use `cabal-plan` to get topological order:

```bash
cabal-plan topo
```

Or for a visual graph:

```bash
cabal-plan dot | dot -Tpng -o deps.png
```

### 6. Update .cabal Files in Order

For each sublibrary, from least dependent to most dependent:

1. Open the `.cabal` file
2. Update `build-depends` bounds using freeze as reference:
   - **Lower bound:** version from freeze
   - **Upper bound:** next minor version

   Example: if freeze says `aeson-2.1.2.1`, use `aeson >= 2.1.2.1 && < 2.2`

3. Build the sublibrary
4. Format and lint:
   - `fourmolu -i` on all source files
   - `hlint` on all source files
5. If sublibrary has tests:
   - Build tests
   - Format and lint test files
   - (tests don't need to pass - fixing tests is done with the user)
6. **Commit** this sublibrary (one commit per sublibrary)
7. **Never touch this sublibrary again** during the bump

This ensures each sublibrary is fully complete before moving to the next.

### 7. Update cabal.project

After all .cabal files are updated:
- Update `index-state` for hackage and CHaP
- Update any `source-repository-package` entries

### 8. Update Node Runtime Version

Update the cardano-node input in `flake.nix` to the target version.

This is required to run integration tests against the new node version.

### 9. Final Summary Commit

Create a final commit that:
1. Summarizes all operations performed during the bump
2. References the changelog document created in step 3
3. Lists all dependency version changes
4. Documents any breaking changes or notable updates from dependencies

## cardano-wallet Sublibrary Order

33 sublibraries in dependency order (update in this sequence):

**Level 0 - No internal dependencies:**
- `delta-types`
- `text-class`
- `cardano-numeric`
- `std-gen-seed`
- `crypto-primitives`
- `cardano-wallet-buildkite`
- `faucet`
- `cardano-wallet-application-tls`

**Level 1:**
- `iohk-monitoring-extra`
- `cardano-wallet-launcher`
- `cardano-wallet-test-utils`
- `delta-store`
- `wai-middleware-logging`
- `cardano-wallet-secrets`
- `cardano-wallet-application-extras`

**Level 2:**
- `temporary-extra`
- `delta-chain`
- `delta-table`
- `cardano-api-extra`
- `cardano-wallet-primitive`

**Level 3:**
- `cardano-coin-selection`
- `address-derivation-discovery`

**Level 4:**
- `cardano-balance-tx`

**Level 5:**
- `cardano-wallet-network-layer`

**Level 6:**
- `cardano-wallet`
- `local-cluster`

**Level 7:**
- `cardano-wallet-api`

**Level 8:**
- `cardano-wallet-ui`

**Level 9:**
- `cardano-wallet-application`

**Level 10:**
- `cardano-wallet-integration`
- `cardano-wallet-unit`

**Level 11:**
- `cardano-wallet-benchmarks`

**Level 12:**
- `cardano-wallet-blackbox-benchmarks`

## Key Concepts

### Cardano Haskell Packages (CHaP)

CHaP is the custom Hackage repository for Cardano packages:

```cabal
repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
```

**index-state** controls which package versions are visible:

```cabal
index-state:
  , hackage.haskell.org 2025-01-01T23:24:19Z
  , cardano-haskell-packages 2025-03-01T00:00:00Z
```

### Source Repository Packages

For packages not on CHaP or when you need a specific commit:

```cabal
source-repository-package
    type: git
    location: https://github.com/IntersectMBO/cardano-addresses
    tag: 2bca06deaa60e54a5322ac757387d744bf043367
    --sha256: 1y1mzfly7jac40b9g4xc078rcm5zqhc3xxv77kwxi10yph1jwq7z
    subdir: command-line
            core
```

**Getting SHA256:**

```bash
nix flake prefetch github:owner/repo/commit-sha
```

## Bound Format

Standard format for dependency bounds:

```cabal
build-depends:
    aeson >= 2.1.2.1 && < 2.2
  , base >= 4.18 && < 5
  , cardano-api >= 10.0 && < 10.1
```

- Lower bound: exact version from freeze
- Upper bound: next minor version (allows patch updates)

## Troubleshooting

### "Could not resolve dependencies"

1. `cabal update`
2. Check if package exists in CHaP at your index-state
3. Try newer index-state
4. Add as source-repository-package

### Build failures after update

1. `rm -rf dist-newstyle`
2. `cabal update`
3. `cabal build all -O0`

## Automation Opportunities

**Potential tools to create:**

1. **deps-order** - Script to output sublibrary update order from cabal.project
2. **bounds-update** - Parse freeze file, update .cabal bounds automatically
3. **validate-bounds** - Check all .cabal files have consistent bounds

These could be added to the justfile or as standalone tools.
