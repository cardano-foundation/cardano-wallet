# Hydra

The [Nix build server](https://github.com/NixOS/hydra) - not to be confused with [Ouroboros Hydra](https://eprint.iacr.org/2020/299).

## Jobset

The Hydra jobset is defined by [`release.nix`](https://github.com/input-output-hk/cardano-wallet/blob/master/release.nix). There are some explanatory comments at the top of this file.

Here are links to the jobsets for different branches:

* Master branch - https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs
* bors/staging - https://hydra.iohk.io/jobset/Cardano/cardano-wallet-bors-staging
* GitHub PR builds - https://hydra.iohk.io/jobset/Cardano/cardano-wallet-pr-NNNN - replace _NNNN_ with the PR number.


## Required job

Within the jobset, one job is special. This determines whether the build is reporting to GitHub as a success or failure.

It is an aggregate job containing the following constituents:

https://hydra.iohk.io/job/Cardano/cardano-wallet/required/latest-finished#tabs-constituents

This job is set up with the `mkRequiredJob` function in `release.nix`.


## Evaluation

Hydra instantiates the `release.nix` file on the Hydra master host (hydra.iohk.io). This is evaluation. It then sends the jobs out to the build farm to be built. Evaluation fails if there are syntax errors in the Nix files, etc.

**Note**: If evaluation fails, no build status will ever be reported to GitHub. If there is a Bors job waiting for this status, it will time out. 

As a workaround for this issue, devops added a `hydra-eval-errors` [Buildkite job](https://buildkite.com/input-output-hk/cardano-wallet) which polls the Hydra web interface for evaluation status. If it detects evaluation failure on Hydra then the Buildkite pipeline will consequently fail.


## Binary cache

See [iohk-nix/docs/nix.md](https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix.md) for information on configuring the Hydra binary cache on your system.
