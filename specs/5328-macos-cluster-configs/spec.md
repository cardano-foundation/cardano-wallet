# Spec — #5328 macOS CI: `LOCAL_CLUSTER_CONFIGS` resolves under `lib/unit`

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5328
PR: https://github.com/cardano-foundation/cardano-wallet/pull/5331

## P1 user story

As a cardano-wallet maintainer watching the macOS CI pipeline, I want the
`wallet-unit / Shelley` shard to find its local-cluster configuration files,
so that the regression test for issue #1708 actually runs and macOS
`push:master` goes green again instead of failing on every commit.

## Background

`localClusterConfigsFromEnv` (`lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/Env.hs:55`)
resolves the cluster-config directory two different ways:

| `LOCAL_CLUSTER_CONFIGS` | Resolution |
| --- | --- |
| set | value taken verbatim, absolutized against the process CWD |
| unset | fallback `../local-cluster/test/data/cluster-configs`, absolutized against the process CWD |

The fallback is written to be correct when CWD is `lib/unit`.

`.github/workflows/macos-unit-tests.yml` sets, at the `unit-tests` job level
(lines 48–49), a **repo-root-relative** value
`lib/local-cluster/test/data/cluster-configs`. Since 05f20bbb555 every
`wallet-unit /*` matrix shard runs with `working-directory: lib/unit`. Set
value + `lib/unit` CWD ⇒ `lib/unit/lib/local-cluster/...`, which does not
exist ⇒ `openDirStream: does not exist` on `faucet-addrs`.

`ci.yml`'s equivalent `unit-tests` job (lines 121–294) sets no such job-level
env, uses the fallback, and passes. That is the known-green baseline.

## User stories

- **US1** — As a maintainer, the macOS `wallet-unit / Shelley` shard passes,
  including regression test #1708.
- **US2** — As a maintainer, every other shard in the macOS `unit-tests` job
  keeps passing; the fix must not trade one broken shard for another.
- **US3** — As a reviewer, I can see mechanically that the fix holds and
  that its precondition (wallet-unit shards run from `lib/unit`) is still
  true, without running macOS CI myself.

## Functional requirements

- **FR1** — The macOS `unit-tests` job MUST NOT set `LOCAL_CLUSTER_CONFIGS`
  at job level.
- **FR2** — The macOS `unit-tests` job MUST NOT set `LOCAL_CLUSTER_CONFIGS`
  at step level either (equivalent leak, same failure).
- **FR3** — Every `wallet-unit /*` matrix shard MUST keep
  `working-directory: lib/unit`. The fallback path is only correct from
  there; without it, FR1 turns the fix into a new bug.
- **FR4** — No other workflow file is modified. `windows.yml`,
  `windows-e2e.yml`, `release.yml`, `macos-integration.yml`,
  `local-cluster-stress.yml` and `ci.yml` all set the variable for
  jobs that run from a different CWD and are out of scope. `windows.yml`
  in particular belongs to sibling ticket #5329.
- **FR5** — No product code changes. `Env.hs` keeps its current two-branch
  behaviour; this is a CI-configuration fix only.

## Evidence that dropping the variable job-wide is safe

The only test source in the repository that calls
`localClusterConfigsFromEnv` is
`lib/unit/test/unit/Cardano/Wallet/Shelley/NetworkSpec.hs:396`, part of the
`cardano-wallet-unit` suite. Every shard of that suite in this job already
runs from `lib/unit`. The remaining shards in the matrix
(`cardano-numeric`, `delta-*`, `std-gen-seed`, `wai-middleware-logging`,
`cardano-wallet-launcher`, `cardano-wallet-network-layer`,
`cardano-wallet-secrets`, `cardano-wallet-test-utils`,
`cardano-wallet-primitive`, `cardano-wallet-application-tls`,
`cardano-wallet-blackbox-benchmarks`) never read the variable. Therefore no
per-shard scoping is required and the job-level `env:` block can be deleted
outright.

## Success criteria

- **SC1** — `./gate.sh` passes at HEAD.
- **SC2** — A `workflow_dispatch` run of `macos-unit-tests.yml` against this
  branch shows `wallet-unit / Shelley` green (the workflow declares
  `workflow_dispatch`, so a branch-ref dispatch is possible; if it turns out
  not to be, the residual risk is stated explicitly in the PR body).
- **SC3** — Post-merge: three consecutive green `push:master` runs. Outside
  this PR's control; tracked on the issue.

## Out of scope

- Windows `cardano-node.exe` PATH failure (#5329, parallel ticket).
- macOS integration tests still being dispatch-only (#5126).
- Any change to `Env.hs`'s resolution semantics.
