The [Nix build server](https://github.com/NixOS/hydra) - not to be confused with [Ouroboros Hydra](https://eprint.iacr.org/2020/299).

For general information about nix, see [Nix](./Nix).

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


## Build Products

Some build jobs have build products which can be downloaded from the
Hydra web interface. Other build systems sometimes call these
"artefacts".

When you click the "latest successful build" link for a job, it
redirects (HTTP 302 Moved Temporarily) to the location of the _current
latest successful build_. To make a download link which is always the
latest, click the `Details` button next a build product, and copy the
links.

A nix derivation builder script can register build products by adding
lines to the file `$out/nix-support/hydra-build-products`.


## Evaluation

Hydra instantiates the `release.nix` file on the Hydra master host
(hydra.iohk.io). This is evaluation. It then sends the jobs out to the
build farm to be built.

Evaluation fails if there are syntax errors in the Nix files, type
errors, etc. Sometimes evaluation can fail due to memory exhaustion on
Hydra master.

**Note**: If evaluation fails, the Hydra eval runner will retry until
it succeeds. The `ci/hydra-eval` status will change from pending to
failed, until the evaluation succeeds.


## Restarting builds

Sign in with the [adrestia](https://hydra.iohk.io/dashboard/adrestia) user to restart build jobs.
