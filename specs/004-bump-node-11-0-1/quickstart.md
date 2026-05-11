# Quickstart: Upgrade dependencies to cardano-node 11.0.1

Run commands from `/code/cardano-wallet` inside the repository's Nix development environment unless stated otherwise.

## 1. Confirm target metadata

```bash
gh issue view 5275 --repo cardano-foundation/cardano-wallet
git status --short --branch
```

Confirm the target remains cardano-node 11.0.1 with cardano-api/cardano-cli 11.x, cardano-ledger-conway >= 1.22.1.0, and CHaP index-state 2026-05-02T16:21:41Z.

## 2. Generate the authoritative topology

First generate the node freeze:

```bash
git -C /code/cardano-node checkout 11.0.1
cd /code/cardano-node
cabal update
cabal freeze
mkdir -p /tmp/cardano-node-11.0.1
cp cabal.project.freeze /tmp/cardano-node-11.0.1/cabal.project.freeze
cd /code/cardano-wallet
```

Then update wallet metadata enough for a solver plan and generate the topology:

```bash
nix develop --quiet -c cabal build all --dry-run
nix develop --quiet -c cabal-plan topo
```

Compare the generated order with `plan.md` and `tasks.md`. If it differs, update both documents before any component edits.

## 3. Adapt upstream wallet dependencies

Validate upstream dependency repos against the node freeze before local wallet consumers close:

```bash
cd /code/cardano-ledger-read
# align constraints with /tmp/cardano-node-11.0.1/cabal.project.freeze
# run that repo's build and test gate

cd /code/cardano-balance-transaction
# align constraints with /tmp/cardano-node-11.0.1/cabal.project.freeze
# run that repo's build and test gate

cd /code/cardano-wallet
nix flake prefetch github:cardano-foundation/cardano-ledger-read/<commit>
nix flake prefetch github:cardano-foundation/cardano-balance-transaction/<commit>
```

Update the wallet `cabal.project` source-repository-package tags and sha256 values only after each upstream repo has a validated commit.

## 4. Create the patch stack before code edits

Create one patch for target metadata, one for external source-repository-package pins if needed, and one patch per component in the confirmed topology order.

```bash
stg new -m "build: align node 11.0.1 dependency metadata"
stg new -m "build: align external Cardano source pins"
stg new -m "fix: adapt cardano-wallet-read to node 11.0.1"
stg new -m "fix: adapt delta-types to node 11.0.1"
```

Continue until every component has a pre-created patch. Work by `stg goto`, edit only that component's ownership boundary, validate, then `stg refresh`.

## 5. Component vertical gate

For each component:

```bash
cabal-fmt -i path/to/component.cabal
fourmolu -i <changed-haskell-files>
hlint <changed-haskell-files>
nix build --quiet .#<component-or-test-output>
```

If the component declares any Cabal `test-suite`, run that component's unit tests and keep the unit-test fixes in the same patch/commit as the component adaptation. The component is not closed until those unit tests are green. Use the component-specific Nix output where available. If no narrow output exists, document the smallest reliable build/test command in `tasks.md` before closing the component.

## 6. No-return rule

After a component is validated and its patch is refreshed, later patches must not edit files under that component directory. If a later component exposes a lower-level defect, stop and either reorder the stack or document a reopened-component correction before editing.

## 7. Final validation

```bash
nix build --quiet .#cardano-wallet .#local-cluster .#integration-exe .#test-local-cluster-exe
just e2e
```

Run the full project gate agreed for the PR before marking the feature complete.
