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

## Details

### **Granularity**

* Granularity refers to **automatic** actions taken by the CI system. It should be possible to trigger a build or check manually at any time.
* The purpose of granularity is to **conserve** computing **resources** — in a world with infinite resources, the system would perform every build and check on every commit.
* The name of the granularity “**post-commit**” was chosen for brevity — the action is performed automatically on the **latest** commit after a \`git push\`, not on the git commits in between. In other words, the action is performed at most once per commit.
* We use the “**post-merge**” granularity for actions that
  * consume scarce resources and have a high chance of failing, e.g. builds and checks on macOS
* We use the “**nightly**” granularity for actions that
  * consume many resources, e.g. benchmarks

### **CI System**

As a general rule, we choose

* Github Actions for actions that
  * are very simple and do not require a nix store / environment
  * run on Windows
* Buildkite otherwise
  * especially for actions that require a nix store

We have a **tension** where we have to set up some checks (e.g. unit tests) in two different environments due to different availability of operating systems:

* Linux, macOS — in Buildkite
* Windows — in Github Actions

We hope to address this tension by requesting a **Windows machine** for use with **Buildkite**.

### **Platform macOS**

At the time of writing, we have two mac-mini machines that act as Buildkite agents. Unfortunately, they are frequently overloaded and fail the builds or checks. Hence, we only use granularity “post-merge” or “nightly” for them.

### **Company Processes**

For developing and maintaining our CI, we may use DevX/SRE expertise from IOG.

* Our **tribe** is responsible for choosing our CI tooling
* Our **tribe** should have a process for getting DevX/SRE support
* Our tribes’ DevX/SRE resources can help teach us how to debug problems that arise
* Link to the [SRE Chapter of IOG](https://input-output.atlassian.net/wiki/spaces/CI/pages/3528785931/SRE+Chapter)

## **Rationale**

### **Artifacts and checks**

The two main concerns of a CI pipeline are: building **artifacts** and running **checks**.

The purpose of building an artifact is to produce, say, an executable or HTML. The purpose of running a check is to check that the artifact satisfies certain properties, e.g. all unit tests pass.

Different CI systems, like Hydra, Cicero, Buildkite or Github Actions, have a different focus regarding these concerns.

* The world view of Hydra is that everything is about building artifacts. Hydra was surprisingly successful as a CI tool, because this world view can be used for running checks, too — they can be expressed as trivial artifacts, where success of the check is equivalent to success of building \`()\`, and failure of the check is equivalent to failure of the artifact build.
* The world view of Github Actions, Buildkite or Cicero is that everything is about running checks. The drawback is that building artifacts is more difficult and we have problems managing the build cache.

For us, the main takeaway is that we should try to separate these concerns clearly.

Our **artifacts** include: **source code** and **compiled** **executables**. We have different **checks** on these: Linters and style checkers on the source code, unit and integration tests on the executables.

As we are coming from Hydra, compiling executables is easiest to do through a **cached nix store**. At the moment, it looks like only Buildkite has good support for that; hence we choose Buildkite.

### **Our options for CI system**

Buildkite

* Pro — Good at artifacts, working nix cache
* Pro — Good documentation, easy to write
* Con — Dependency on machine (currently provided by SRE / [Samuel Leathers](mailto:samuel.leathers@iohk.io))
* Con — no Windows machine
* Con — Dependency on permissions (currently only SRE / [Samuel Leathers](mailto:samuel.leathers@iohk.io) has write permission)

In a pinch, the dependencies can be solved by forking the repository and providing our own machines.

Github Action

* Neutral — Good at small actions, but problems at scale
* Neutral — Good documentation, but a bit cumbersome to write
* Pro — No dependency on machine
* Pro — Windows machine
* Pro — No dependency on permissions

Cicero

* Con — Poor at artifacts, nix cache currently not working properly
* Con — Poor documentation
* Neutral — Dependency on machine (provided by SRE, but they have long-time commitment)
* Con — no Windows machine
* Pro — No dependency on permission

## **References**

\[1\] G Kim, K Behr, G Spafford; [The Phoenix Project](https://www.goodreads.com/book/show/17255186-the-phoenix-project); IT Revolution Press (2013). A business novel about the DevOps movement: make the flow of work visible and automate it, to an extreme of, say, 30 releases per day.

\[2\] [Cicero on Github](https://github.com/input-output-hk/cicero#readme)

# **Scratchbook**

## **Random Findings**

Installing Nix with the \`cachix/install-nix-action\` Github Action: [https://github.com/input-output-hk/cardano-node/blob/db396b163af615aa89286aa985583ef8843cfcde/.github/workflows/check-mainnet-config.yml\#L16-L23](https://github.com/input-output-hk/cardano-node/blob/db396b163af615aa89286aa985583ef8843cfcde/.github/workflows/check-mainnet-config.yml#L16-L23)

## **Documentation Findings**

### **Cicero**

[Cicero](https://github.com/input-output-hk/cicero#readme) \= An *engine* for executing actions. An “action” is an arbitrary program (Bash, Python, Nix, …) that is run in the Nomad execution environment.

[Tullia](https://github.com/input-output-hk/tullia#readme) \= A domain specific language, embedded in the Nix language, for expressing actions to be run with Cicero. This is useful when writing Cicero actions that mainly build stuff with Nix.
