# **Continuous Integration**

|              |            |
|--------------|------------|
| Started      | 2022-12-06 |
| Decided      | 2023-01-27 |
| Last amended | 2023-04-04 |

## **Why**

The sudden decommission of Hydra forces us to revisit our Continuous Integration (CI) setup.

## **Decision**

We predominantly rely on [Buildkite](https://buildkite.com) as CI system.

We build **artifacts** and run **checks** on them. Artifacts include compiled executables, but also the source code itself. Checks include unit and integration tests, but also source code linters. We specify our artifacts in \`flake.nix\`, and most of our checks, too.

We perform these builds and checks with different **granularity** — in order to keep our computing resources within reasonable limits, we don’t automatically check everything on every commit. Instead, the granularities are:

* `post-commit`	\= (at most once) after each commit; for a quick sanity check after a push
* `pre-merge`	\= before merging each pull request; for a reasonably complete, automated check that master will satisfy functional requirements
* `post-merge`	\= after merging each pull request to master; for an exhaustive check that master satisfies functional requirements on all platforms
* `nightly`		\= every night; for an exhaustive, automated check of functional and non-functional requirements

The following **table** lists our artifacts, the checks performed on them (\`.\` for build), the granularity at which the check is performed, and the CI system used for doing that:

| Artifact | Check | Granularity | CI System | (Status) |
| :---- | :---- | :---- | :---- | ----- |
| Source code | Code formatting style | post-commit | Buildkite | 🔵 |
| Documentation | . | post-commit | Github Action | 🔵 |
| Pull request (PR) | Mergeable to master concurrently with other PRs | pre-merge | Bors | 🔵 |
| Compiled modules | . | post-commit | Buildkite | 🔵 |
|  | Unit tests (linux) | post-commit | Buildkite | 🔵 |
|  | Unit tests (macos) | post-merge | Buildkite | 🔵 |
|  | Unit tests (windows) | nightly | Github Action | 🔵 |
| Executables / Release archive | . (linux) | pre-merge | Buildkite | 🟡[ADP-2502](https://cardanofoundation.atlassian.net/browse/ADP-2502)  |
|  | . (macos) | post-merge | Buildkite | 🔵  |
|  | . (windows, cross-compiled) | pre-merge | Buildkite | 🔵 |
| Executables | Integration tests (linux) | pre-merge | Buildkite | 🔵 |
|  | ~~Integration tests (macos)~~ |  | Buildkite | 🔴[ADP-2522](https://cardanofoundation.atlassian.net/browse/ADP-2522) |
|  | ~~Integration tests (windows)~~ |  | Github Action | 🔴[ADP-2517](https://cardanofoundation.atlassian.net/browse/ADP-2517) |
|  | Benchmarks | nightly | Buildkite | 🔵 |
| Release archive | E2E tests | nightly | Github Action | 🔵  |
| Docker image | . | post-commit | Buildkite | 🔵 |

Legend: Status 🔵\= working; 🟡= needs work; 🔴\= not working
