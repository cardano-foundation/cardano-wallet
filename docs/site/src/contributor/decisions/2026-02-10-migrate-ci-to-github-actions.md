# **Migrate CI from Buildkite to GitHub Actions**

|              |                 |
|--------------|-----------------|
| Decided      | 2026-02-10      |
| Decided by   | Paolo Veronelli |
| Executed by  | Paolo Veronelli |

## **Why**

The Cardano Wallet project is entering maintenance-only mode and the Cardano Foundation is preparing for potential handover of maintenance to the community. The CI setup should be as accessible and self-contained as possible for any future maintainer.

Buildkite requires self-hosted infrastructure, agent token management, and familiarity with a separate CI system. GitHub Actions is natively integrated with the repository, visible to all contributors, and does not require additional accounts or permissions beyond GitHub itself.

## **Decision**

We migrate all CI workflows from [Buildkite](https://buildkite.com) to [GitHub Actions](https://github.com/cardano-foundation/cardano-wallet/actions).

This supersedes the [2023-01-27 Continuous Integration](2023-01-27-continuous-integration.md) decision which chose Buildkite as the primary CI system.

### Goals

1. **Visibility** — CI status, logs, and configuration are visible directly in the repository. No separate Buildkite account or dashboard is needed.
2. **Standardisation** — All workflows use the same system (GitHub Actions), eliminating the split between Buildkite and GitHub Actions that existed before.
3. **Handover readiness** — A future maintainer only needs GitHub access to operate, debug, and modify CI. No Buildkite organisation membership, agent tokens, or pipeline configuration is required.
4. **Reduced operational burden** — Agent token rotation, Buildkite webhook configuration, and Buildkite-specific hook scripts are no longer needed.

### What changed

| Before (Buildkite) | After (GitHub Actions) |
|---------------------|------------------------|
| Main build pipeline (`pipeline.yml`) | [`ci.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/ci.yml) |
| Release pipeline (`release.yml`) | [`release.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/release.yml) |
| Restoration benchmarks (`restoration-benchmarks.yml`) | [`linux-benchmarks.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/linux-benchmarks.yml) |
| Windows unit tests (already GHA) | [`windows.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/windows.yml) |
| — | [`linux-e2e.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/linux-e2e.yml), [`windows-e2e.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/windows-e2e.yml) |
| — | [`macos-unit-tests.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/macos-unit-tests.yml), [`macos-integration.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/macos-integration.yml) |
| — | [`linux-mithril-sync.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/linux-mithril-sync.yml) |

Self-hosted runners (Windows, macOS) remain necessary for platform-specific tests. Runner setup documentation has moved from `.buildkite/README.md` to the [Continuous Integration](../how/continuous-integration.md) docs page.

### What stays the same

- Self-hosted machines for Windows and macOS builds
- Nix-based build environment
- Attic binary cache for macOS
- Granularity model (post-commit, pre-merge, post-merge, dispatch)

## **Rationale**

The [2023 decision](2023-01-27-continuous-integration.md) chose Buildkite primarily because of its good nix cache support and artifact handling. Since then:

- GitHub Actions runners gained better nix support via community actions
- The Attic cache replaced the Buildkite-specific nix cache
- The split between Buildkite (Linux/macOS) and GitHub Actions (Windows) created maintenance overhead with two different CI configurations to maintain
- Buildkite required separate account access and agent token management, creating a bus-factor risk for maintenance handover

GitHub Actions eliminates these issues while retaining the self-hosted runner capability needed for platform-specific builds.

## **References**

- [#5105](https://github.com/cardano-foundation/cardano-wallet/pull/5105) — initial CI migration
- [#5124](https://github.com/cardano-foundation/cardano-wallet/pull/5124) — build gates
- [#5149](https://github.com/cardano-foundation/cardano-wallet/pull/5149) — restoration benchmarks
- [#5153](https://github.com/cardano-foundation/cardano-wallet/pull/5153) — release automation
- [#5168](https://github.com/cardano-foundation/cardano-wallet/pull/5168) — Attic cache
