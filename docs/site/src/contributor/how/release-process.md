# Release Process

## **Trunk-based development**

We use [Trunk-based Development](https://martinfowler.com/articles/branching-patterns.html#Trunk-basedDevelopment): We merge features and improvements into the `master` branch very frequently. If a large feature takes more than one pull request to develop, and is therefore incomplete, we hide it behind a feature flag. The `master` branch should be kept in a state where all tests pass, so that we can **release** it **at any time**.

Even though the act of making a release frequently reveals **problems** in our **testing** and **dependency** infrastructure, solving these problems is **not in scope** for the present decision — they need their own decisions.

## **Release candidate branches**

Unfortunately, releasing directly from the `master` branch is not quite practical for two reasons: a) secondary CI and b) administrative commits.

Our [Continuous Integration](../decisions/2023-01-27-continuous-integration.md) can be grouped into two categories:

* Primary CI — gatekeeper for accepting pull requests to the `master` branch.
* Secondary CI — gatekeeper for making releases.

**Secondary CI** denotes tests that are impractical to run before every merge into the `master` branch, such as manual tests or automated tests that take a long time to run, e.g. benchmarks. The core teaching of Continuous Integration is to minimize secondary CI as much as possible, but it is a fact of life that we still have it.

**Administrative commits** are commits that change metadata in the repository, such as version numbers, release dates, or artifact URLs. These commits need to be checked by the secondary CI as well, but they are made just before a release, so if we were to include them in `master`, we would incur a time delay between our intention to release and the act of releasing where we wait for secondary CI to finish.

In order to accommodate these two facts of life, we use **automated release candidate branches**. The branches relevant for the release process are:

* `master` branch — development can progress even while preparing a release.
* `release-candidate/vYYYY-MM-DD` branch — at the granularity of the secondary CI, one release candidate branch is created automatically, i.e. daily. This branch forks from a commit on  `master`, and adds a single administrative commit, i.e. the version number bump. Secondary CI is run on that branch. Then, we can release at any time from the last release candidate branch where the secondary CI succeeded. (As a technical synchronization aid, the primary CI creates a Git tag `rc-latest` on the release candidate branch when it runs successfully; this tag signals to the secondary CI that it may proceed on this branch.)

Since the release candidate branches are created frequently, we need to **garbage collect** them. For the sake of being specific, we keep the last **31** release candidate branches where secondary CI succeeded and remove the rest.

## **Release Artifacts and their Publication**

A **release** is a collection of artifacts that have been tested together and will be published. We use automation to create and test these artifacts automatically from the source code in the repository.

All artifacts in a release are created from the same **Git commit**, also called the **release commit**. This Git commit is marked by a **Git tag** of the form `vVersionNumber`, e.g. `v2024-03-01`. This release commit is the head of one of the **release candidate branches** described above.

We keep a **Changelog** which records all user-facing changes between the previous release and the current release. We use **pull requests** (PR) as the smallest unit for user-facing changes. In order to ensure that PRs are indeed the smallest unit, we require that the Git commit of `master` from which release candidate branch is forked is the merge commit of a pull request.

The **release artifacts** and their places of **publication** are:

| Artifact | Artifact Details | Place of Publication |
| :---- | :---- | :---- |
| Release notes (metadata) | Version of compatible cardano-node Changes to previous release [Changelog](http://keepachangelog.com) HTTP API changes Known issues | [Github Releases (link)](https://github.com/cardano-foundation/cardano-wallet/releases/latest) |
| Self-contained executables on all platforms | Compressed archive containing  cardano-wallet cardano-node shared library dependencies (static linking preferred) Platforms Linux, macOS, Windows Testing includes E2E tests with all components | [Github Releases (link)](https://github.com/cardano-foundation/cardano-wallet/releases/latest)  |
| Documentation | Command-line interface help OpenAPI specification of HTTP API | [Github Pages (link)](https://cardano-foundation.github.io/cardano-wallet/) |
| Docker image |  | [Docker Hub (link)](https://hub.docker.com/r/cardanofoundation/cardano-wallet) |
| Haskell packages |  | [CHaP](https://github.com/input-output-hk/cardano-haskell-packages)[Hackage](https://hackage.haskell.org/) |

We use [Github Releases](https://github.com/cardano-foundation/cardano-wallet/releases/latest) as the **source of truth** for publication: A release is considered to be published once it has been published there.

## **Changelog**

We publish a **changelog** as part of the release notes.

We adhere to the format and rationale described at [https://keepachangelog.com/en/1.1.0/](https://keepachangelog.com/en/1.1.0/) . Importantly, a changelog focuses on being **human-readable**.

The changelog is distilled from a sequence of pull requests as follows:

* Keep only those pull requests which represent **user-facing** changes.
  * A user-facing change is one where the user may have to adapt. This is typically the case for changes that
    * change the API
    * or accommodate a changed API of a run-time dependency
* **Group** the pull requests numbers by their user-facing changes.
  * Sometimes, the list of PRs associated with a user-facing change can be large. In such cases, mention only the PRs that we would need to revert in order to hide the change.. For example, when an experimental feature is built behind a feature flag, the PR that merges the flag into the core functionality is to be mentioned here; whereas pull requests that do the implementation work need not be mentioned.
* Amend the **description** of the change to give more context.

## **Release checklist**

We use a **release checklist** in order to ensure that we do not miss creating, testing, or publishing any artifact. The focus of the checklist is for a human to **verify** that nothing has been missed — creating and testing the artifacts should be automated as much as possible by [Continuous Integration](../decisions/2023-01-27-continuous-integration.md).

(The work required to perform the release checklist, resulting in the publication of the release artifacts, is tracked in a ticketing system decided in our [HAL Workflow Review](../decisions/2023-07-28-workflow-review.md).)

The release checklist is subject to updates as the environment changes. Here, we only specify the **general outline** of the release checklist:

1. Choose a Git commit from which we want to release.
   1. This commit must be the head of a release candidate branch (Trunk-based development).
   2. All packages in this commit must have the version numbers to be released.
   3. Secondary CI must have succeeded on this commit, including artifact creation.
2. Mark the commit with a Git Tag specifying the version number, i.e. `vYYYY-MM-DD`.
3. Write the release notes, specifically the human-readable Changelog section.
4. When the commit is ready to be released, press the “Publish release” button in [Github Releases](https://docs.github.com/en/repositories/releasing-projects-on-github) .
   1. The commit is ready to be released if all of the following are satisfied:
      1. All automated tests pass (both primary and secondary CI).
      2. A human has inspected the test logs and feels confident that the automated tests have run correctly, i.e. the “pass” mark is not the result of a bug in the test pipeline.
      3. Release notes have been written.
   2. We use automation to publish the release artifacts on other platforms, such as [Docker Hub](https://hub.docker.com) or [Read the Docs](https://readthedocs.org) as desired.

The release checklist is kept in the [project's wiki](https://github.com/cardano-foundation/cardano-wallet/wiki). To create checklist for a new version:
1. Copy the latest version, eg. [v2025-01-09](https://github.com/cardano-foundation/cardano-wallet/wiki/Release-v2025-01-09) as a new page named `Release-vYYYY-MM-DD.md`
2. Uncheck all the boxes and remove all the links there
3. Go through all the steps, checking box and updating links as needed

## **Automation**

We use the following **tools** to help automate the release process:

| Type        | Name                                        | Task                                                                                                                                                                                                                     |
|:------------|:--------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Script      | updateVersionNumbers `release-candidate.sh` | Updates the version numbers of all packages in the repository. Useful for preparing the administrative commit.                                                                                                           |
| Script      | draftReleaseNotes `make_changelog.sh`       | Prepares draft release notes by collecting a list of merged pull requests and known issues. To support automation, Github labels can be utilized. To support human-readability, PRs should be connected to JIRA tickets. |
| CI pipeline |                                             | Pipeline that automatically creates release artifacts and runs tests on them, used for the release candidate branches. It’s important that the created executables are subject to test, not a surrogate.                 |

# **Rationale**

## **Trunk-based development**

Experience, both by us ([Continuous Integration](../decisions/2023-01-27-continuous-integration.md)) and by others _Phoenix2013_[^2], _Google2020_[^3], indicates that trunk-based development is the way to go — the ability to release at any time ensures the ability to release in the first place. Even though problems with testing and dependency management frequently resurface, they do not grow so bad that they become intractable.

## **Bundling dependent executables**

We bundle both the `cardano-wallet` executable, but also dependent executables such as `cardano-node` or `carano-address` in our compressed archive that is released. I believe that this is worth doing for several reasons:

* Bundling specifies precisely which executables have been tested together — not just by the version number, but down to the **precise executable file**. Even though a version number might suffice, this prevents obscure, platform specific bugs or missing shared libraries. Through bundling, the user can rely on the fact that at least this specific combination has been tested to work. That said, bundling a **hash** of the executable file as opposed to the file would satisfy the same requirement.
* Bundling makes it convenient to test the integration of the different executables, by running them from the archive. We have to test their integration on **each platform**, as the communication between executables can be highly platform-specific (IPC, named sockets, file paths on Windows…) and subject to obscure failures, such as delayed file deletion on Windows. Again, a hash would suffice.
* Bundling is convenient for users, including Daedalus, as they get ready-to-use executables with a single download. Users do not have to collect dependencies separately (download from other repositories), or build them themselves (takes time, may fail).

 - Create new page on cardano-wallet's [wiki](https://github.com/cardano-foundation/cardano-wallet/wiki/_new) called `Release vYYYY-MM-DD` by copying contents of the previous releas.
 - Follow the `Release checklist`. Update progress or report obstacles on the thread.

# Releasing packages to CHaP

[CHaP](https://github.com/IntersectMBO/cardano-haskell-packages) or the _Cardano Haskell Packages_ repository is useful to publish _Haskell_ packages relevant to the Cardano ecosystem as this allows standard build tools like `Cabal` to work out-of-the-box with only some configuration to point to the repository in addition to https://hackage.haskell.org

To publish packages on CHaP:

1. Clone the repository locally from https://github.com/cardano-foundation/cardano-haskell-packages/
2. Run script to create relevant commits, for example to release `cardano-coin-selection` package and its (local) dependencies:

   ```
   $ scripts/add-from-github.sh https://github.com/cardano-foundation/cardano-wallet 9eb5f59c328163ca061a20f47519686b6f118d74 lib/coin-selection lib/primitive lib/test-utils lib/delta-types lib/crypto-primitives lib/launcher lib/numeric lib/text-class
   ```
3. Open a Pull Request on upstream repository (ie. https://github.com/IntersectMBO/cardano-haskell-packages/)


[^3]: T. Winters, T. Manshreck, and H. Wright, “[Software Engineering at Google: Lessons Learned from Programming over Time](https://abseil.io/resources/swe-book)”, O'Reilly (2020).

[^2]: G Kim, K Behr, G Spafford; [The Phoenix Project](https://www.goodreads.com/book/show/17255186-the-phoenix-project); IT Revolution Press (2013).
