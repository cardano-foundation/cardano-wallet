# Research: Upgrade dependencies to cardano-node 11.0.1

## Decision: Use dependency-first component topology as the implementation order

**Decision**: Generate or confirm the Cabal component topology before code edits, then execute from dependency leaves to dependents. If component A depends on component B, B closes first.

**Rationale**: Dependency upgrades fail when lower-level components are left half-fixed and later consumers accumulate opportunistic fixes. Closing one component vertically gives each later patch a stable dependency surface.

**Alternatives considered**:

- Fix compiler errors wherever they appear: rejected because it causes scattered edits across packages and makes review/debugging hard.
- Organize by user story: rejected for implementation sequencing because several user stories touch the same Cabal components.
- Organize alphabetically: rejected because it ignores dependency direction.

## Decision: Generate node 11.0.1 freeze before wallet edits

**Decision**: Checkout cardano-node 11.0.1, run `cabal freeze`, and use that freeze as the authoritative package version source for wallet constraints and component bounds.

**Rationale**: The issue target is not just a node binary version; it is the dependency set that node 11.0.1 resolves with. Freezing the node release prevents hand-picked package versions from drifting away from the ecosystem release.

**Alternatives considered**:

- Copy package versions from the release notes or memory: rejected because the freeze captures transitive versions and solver constraints.
- Update wallet constraints opportunistically until the solver succeeds: rejected because it can silently diverge from the node release.

## Decision: Treat cardano-ledger-read and cardano-balance-transaction as upstream gates

**Decision**: Adapt and validate `/code/cardano-ledger-read` and `/code/cardano-balance-transaction` before local wallet consumer components are closed. Update wallet source-repository-package pins only after the upstream repos have their own validated commits and sha256 values.

**Rationale**: Several wallet components consume these packages. If they are adapted late, lower-level wallet components may need to be reopened, violating the no-return rule.

**Alternatives considered**:

- Patch around upstream incompatibility inside wallet components: rejected because it hides the real dependency boundary.
- Update the source-repository-package pins after local component work: rejected because consumers would validate against the wrong dependency surface.

## Decision: One component patch means one closed ownership boundary

**Decision**: Each component patch owns its `.cabal` file, source files, tests, test data, and validation evidence. Later patches cannot edit that component after it is closed.

**Rationale**: The no-return rule prevents "just fix it there" drift. If a later component exposes a real lower-level bug, the stack must explicitly reopen that component or reorder before proceeding.

**Alternatives considered**:

- Allow small drive-by fixes to closed components: rejected because it weakens patch review and hides dependency mistakes.
- Keep one large compatibility patch: rejected because failures become hard to attribute.

## Decision: Nix build gates are authoritative

**Decision**: Cabal builds are useful for fast iteration, but a component closes only with the relevant Nix build/test gate plus formatting and linting.

**Rationale**: Cabal incremental builds can miss errors in unchanged but transitively affected files. Nix builds match CI and rebuild from a cleaner dependency boundary.

**Alternatives considered**:

- Rely on `cabal build all -O0`: rejected as the only gate; acceptable as a fast pre-check.
- Rely on CI only: rejected by the project constitution; local verification is required.

## Decision: Treat local-cluster compatibility as a prerequisite or explicit linked dependency

**Decision**: If the separate minimal local-cluster PR for cardano-node 11.0.1 has landed, consume it. If it has not landed, keep the local-cluster work as a clearly linked prerequisite before runtime validation.

**Rationale**: Node 11.0 changes experimental hard-fork defaults and protocol-version expectations. Mixing a broad dependency bump with unrelated local-cluster debugging would obscure root cause and review scope.

**Alternatives considered**:

- Fold all local-cluster changes into the dependency bump unconditionally: rejected unless the prerequisite PR is unavailable and this feature must carry the minimum required compatibility change.

## Decision: Seed order is documented but must be verified

**Decision**: `plan.md` and `tasks.md` include a seed component order from the repository workflow and current package layout. The first executable tasks require regenerating or confirming the topology after the 11.0.1 solver plan exists.

**Rationale**: A static order is useful for planning, but dependency metadata can change during the target bump. The generated Cabal topology is the final authority.

**Alternatives considered**:

- Trust the static order without verification: rejected because the package graph has changed across recent specs.

## Implementation Log: T001 issue targets and checkout state

Recorded on 2026-05-11 before implementation edits.

Issue #5275 remains open at https://github.com/cardano-foundation/cardano-wallet/issues/5275. Target scope:

- Align wallet dependencies with cardano-node 11.0.1.
- Move `cardano-api` and `cardano-cli` from the 10.x line to `^>=11.0`.
- Move `cardano-ledger-conway` to `>= 1.22.1.0`.
- Use CHaP index-state `2026-05-02T16:21:41Z`.
- Keep `ouroboros-consensus ^>= 3.0.1` and `ouroboros-network ^>= 1.1` unless the node freeze proves otherwise.
- Consume the cardano-node-runtime 11.0.1/local-cluster prerequisite work already present on `origin/master`.

Initial checkout state:

- `/code/cardano-wallet`: branch `004-bump-node-11-0-1`, clean, tracking `origin/004-bump-node-11-0-1`.
- `/code/cardano-node`: detached checkout at tag `11.0.1`, clean.
- `/code/cardano-ledger-read`: branch `chore/update-gha-node24`, clean, ahead of `origin/chore/update-gha-node24` by 7 commits, HEAD `80d01c6a4cf113b548167de99e0cb06cb6fec5e1`.
- `/code/cardano-balance-transaction`: branch `main`, HEAD `5d69cc9bd47062b363929c877b83f0ab96369583`, with untracked `.claude/` and `specs/002-remove-cardano-api/spec.md.local-bak`.

## Implementation Log: T002-T003 node freeze

The first plain-shell attempt failed because `cabal` was not on PATH. A second attempt inside `nix develop` failed while the stale ignored `/code/cardano-node/cabal.project.freeze` still constrained `base ==4.21.0.0` under the default GHC environment. The stale freeze also pinned `cardano-api 10.23.0.0` and `cardano-ledger-conway 1.20.0.0`, which did not match issue #5275 or the node 11.0.1 Cabal files.

Final command sequence:

```bash
cd /code/cardano-node
backup=$(mktemp /tmp/cardano-node-11.0.1-freeze-old.XXXXXX)
cp cabal.project.freeze "$backup"
rm cabal.project.freeze
nix develop --quiet -c bash -lc 'cabal freeze'
mkdir -p /tmp/cardano-node-11.0.1
cp cabal.project.freeze /tmp/cardano-node-11.0.1/cabal.project.freeze
```

Result:

- Node checkout: tag `11.0.1`, commit `97036a66bcf8c89f687ae57a048eecc0389977ef`.
- Freeze artifact: `/tmp/cardano-node-11.0.1/cabal.project.freeze`.
- Freeze SHA256: `3c147cc1c960dfb3163e6970e9f7a1c1879fc763922ff0120cf5b102495ba0cc`.
- Freeze size: 585 lines.
- CHaP flake revision: `e8a483522ee73c8c9493ea6055553e5c2532e66b`.
- CHaP flake nar hash: `sha256-ZzXz2vOhqethlqPgBExPXEnKWvaTbidsIxh5MGv+pwE=`.
- CHaP index-state: `2026-05-02T16:21:41Z`.
- Hackage index-state: `2026-03-26T20:21:33Z`.

Wallet-relevant target versions from the regenerated freeze:

| Package | Version |
|---------|---------|
| `base` | `4.18.3.0` |
| `cardano-api` | `11.0.0.0` |
| `cardano-cli` | `11.0.0.0` |
| `cardano-ledger-conway` | `1.22.1.0` |
| `ouroboros-consensus` | `3.0.1.0` |
| `ouroboros-network` | `1.1.0.0` |

## Implementation Log: T004 topology attempt

Initial command from `/code/cardano-wallet`:

```bash
nix develop --quiet -c bash -lc 'cabal build all --dry-run && cabal-plan topo'
```

Result: stopped after more than six minutes in Nix evaluation without reaching the Cabal dry run. Output before interruption:

- `warning: Git tree '/code/cardano-wallet' is dirty`
- several missing object messages from `https://paolino.cachix.org`
- `error: interrupted by the user`

T004 remains open. The topology gate must be rerun after wallet metadata has been aligned to the 11.0.1 freeze and before any wallet component slice is edited.

## Upstream issue tracking

The upstream dependency gates are tracked in their own repositories before wallet pins move:

- `cardano-ledger-read`: https://github.com/cardano-foundation/cardano-ledger-read/issues/16
- `cardano-balance-transaction`: https://github.com/cardano-foundation/cardano-balance-transaction/issues/41
- `cardano-ledger-read` draft PR: https://github.com/cardano-foundation/cardano-ledger-read/pull/17
- `cardano-balance-transaction` draft PR: https://github.com/cardano-foundation/cardano-balance-transaction/pull/42

Both tickets are on the Planning board in Backlog with Work ownership. Implementation branches:

- `/code/cardano-ledger-read`: `chore/issue-16-node-11-0-1`
- `/code/cardano-balance-transaction`: `chore/issue-41-node-11-0-1`

Upstream implementation note:

- Both upstream repositories are being harmonized with wallet's `ghc9123` compiler setting before validation.
- Both upstream dev shells set `withHoogle = true` under `ghc9123`.
- Wallet pins must not be updated until each upstream repo has a validated issue-scoped commit/PR.
- The draft PRs are pushed to warm remote builders before the wallet `source-repository-package` pins move.
