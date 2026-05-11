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
