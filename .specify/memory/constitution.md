# Cardano Wallet Constitution

## Core Principles

### I. Maintenance-First Stability
Cardano Wallet is production software managing real ADA funds. The project is in maintenance mode: stability, node upgrades, and bug fixes take priority over new features. Every change must justify its risk against the existing stability baseline. External contributions adding new features are welcome but held to the same quality bar.

### II. Era-Aware Design
All domain types and logic must account for Cardano's multi-era protocol evolution (Byron → Shelley → Allegra → Mary → Alonzo → Babbage → Conway → Dijkstra and beyond). Era-indexed type families ensure compile-time correctness across protocol boundaries. When a new era is introduced, the ERA-CHANGES.md checklist governs the migration path.

### III. Type Safety as Security
Handling cryptographic keys and financial transactions demands defense in depth through Haskell's type system. Prefer type-level guarantees over runtime checks. NoImplicitPrelude is the default. Explicit module export lists are required. Every public function must have Haddock documentation.

### IV. Formal Specification
Critical invariants are formalized in Lean 4 proofs under `specifications/`. The API surface is defined by the OpenAPI spec in `specifications/api/swagger.yaml`. Code must conform to specifications, not the other way around. Specification changes require the same review rigor as code changes.

### V. Reproducible Builds
Nix flakes are the single source of truth for the build environment. All `nix` commands use `--quiet`. Development happens inside `nix develop`; `just` recipes assume this shell. Dependencies are pinned via `cabal.project` (centralized constraints, not per-package). CHaP provides Cardano ecosystem packages. Source-repository-packages carry SHA256 pins.

### VI. Comprehensive Testing
- **Unit tests** cover all core libraries independently
- **Integration tests** run against local cardano-node clusters across multiple eras
- **E2E tests** validate against preprod network
- **Benchmarks** track database, restoration, API latency, and memory usage
- **Cross-platform** validation on Linux (musl static), Windows (cross-compiled), macOS (Intel + Apple Silicon)

Tests are run locally before pushing. CI is not a substitute for local verification.

### VII. Code Quality Gates
All code must pass before merge:
1. **Fourmolu** formatting (70-character line limit, leading commas, 4-space indent)
2. **HLint** static analysis with project-specific qualified import aliases
3. **Compilation** with `-Wall` (and `-Werror` in release builds)
4. **Unit tests** green
5. **Integration tests** green on target eras
6. CI must be green — never merge with failing checks

## Architecture

### Layered Design
The monorepo follows a strict layering:
- **Read Layer** (`cardano-wallet-read`): Era-indexed ledger access
- **Primitive Layer** (`primitive`): Core domain types
- **Wallet Layer** (`wallet`): Business logic
- **API Layer** (`api`): Servant-based REST endpoints
- **Application Layer** (`application`): Executable assembly

Dependencies flow downward only. Each library in `lib/` is a standalone Cabal package with its own test suite.

### Delta-Based Storage
The `delta-*` family of libraries implements immutable, append-only storage. Changes are captured as deltas rather than mutations. This pattern ensures auditability and simplifies reasoning about state.

### REST API Contract
The Servant API is the primary interface. The OpenAPI spec is the contract. Breaking API changes require version bumps and migration documentation. Auto-generated documentation stays in sync with the spec.

## Development Workflow

### Commit Discipline
Conventional Commits format. Each commit addresses a single concern. `feat:` for minor bumps, `fix:` for patches, `feat!:` for breaking changes. Release-please automates versioning from commit history.

### Code Review Culture
Reviews follow the project's code review guidelines: assume competence, ask before criticizing, provide specific actionable feedback, mark nitpicks clearly. Security-sensitive changes (key handling, transaction signing) require extra scrutiny.

### Dependency Management
All version constraints live in `cabal.project`, never in `.cabal` files. Dependency updates are deliberate — each bump is a conscious decision with changelog review. Node version upgrades follow a structured process with integration test validation.

## Governance

This constitution defines the quality and design principles for all spec-driven work on Cardano Wallet. Amendments require documentation and review. All specs, plans, and implementations must demonstrate compliance with these principles.

**Version**: 1.0.0 | **Ratified**: 2026-04-05
