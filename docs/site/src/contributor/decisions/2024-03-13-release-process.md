# Update Release Process

|         |            |
|---------|------------|
| Started | 2023-07-19 |
| Decided | 2024-03-23 |

## Why

The release v2023-07-18 of cardano-wallet highlighted the need to further automate the release process, and to clarify consistency between artifacts and tests. The release v2023-12-18 highlighted further gaps in test and dependency maintenance.

## Decision

* We continue to use **trunk-based development**, where all code is merged into the master branch frequently, such that this branch is (almost) ready to be released at any point in time.
* A **release** is a collection of artifacts that have been tested together and will be published.
  * We **create** a release from a specific Git commit, marked by a **Git tag**.
  * We create and **test artifacts** without human involvement using **automation**.
  * We publish human-readable release notes, specifically a **changelog** and a list of **known issues**.
  * We **publish** a release by clicking a button on [Github Releases](https://docs.github.com/en/repositories/releasing-projects-on-github). Artifacts are automatically pushed to other platforms.
* The release is made under **human supervision** using a **release checklist**.

## Details

see [Release process](../how/release-process.md)
