# Continuous Integration

All CI for `cardano-wallet` runs on [GitHub Actions](https://github.com/cardano-foundation/cardano-wallet/actions). Workflows are defined in [`.github/workflows/`](https://github.com/cardano-foundation/cardano-wallet/tree/master/.github/workflows).

## Workflows

### Core CI

| Workflow | Trigger | Description |
|----------|---------|-------------|
| [`ci.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/ci.yml) | push, PR | Main build & test pipeline — Linux unit/integration tests, builds all artifacts |

### Platform-specific

| Workflow | Trigger | Description |
|----------|---------|-------------|
| [`windows.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/windows.yml) | push, dispatch | Windows build & unit tests (self-hosted) |
| [`macos-unit-tests.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/macos-unit-tests.yml) | push, dispatch | macOS unit tests (self-hosted Apple Silicon) |
| [`macos-integration.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/macos-integration.yml) | dispatch | macOS integration tests (self-hosted) |

### End-to-end

| Workflow | Trigger | Description |
|----------|---------|-------------|
| [`linux-e2e.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/linux-e2e.yml) | push, dispatch | Linux E2E tests against preprod |
| [`windows-e2e.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/windows-e2e.yml) | dispatch | Windows E2E tests (self-hosted) |

### Benchmarks & sync

| Workflow | Trigger | Description |
|----------|---------|-------------|
| [`linux-benchmarks.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/linux-benchmarks.yml) | push, dispatch | Restoration benchmarks on mainnet (long-running) |
| [`linux-mithril-sync.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/linux-mithril-sync.yml) | push, dispatch | Mithril snapshot sync test |

### Release

| Workflow | Trigger | Description |
|----------|---------|-------------|
| [`release.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/release.yml) | push, tags | Creates release candidate branches and release artifacts |
| [`publish.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/publish.yml) | push, tags, PR | Publishes documentation to GitHub Pages |

### Housekeeping

| Workflow | Trigger | Description |
|----------|---------|-------------|
| [`cleanup.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/cleanup.yml) | dispatch | Deletes old workflow runs |
| [`approve-docs.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/approve-docs.yml) | PR target | Auto-approves docs-only PRs |
| [`lean.yml`](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/lean.yml) | push, PR | Lean specification checks (path-filtered to `specifications/`) |

## Self-hosted runners

Several workflows run on self-hosted machines. The GHA runner service replaces the former Buildkite agent on these machines.

### Windows machine

#### System configuration

We assume the machine is configured with a recent Windows version (2022 Server) and has winget installed.

* Install the [GitHub Actions runner](https://docs.github.com/en/actions/hosting-your-own-runners/managing-self-hosted-runners/adding-self-hosted-runners) as a Windows service
* Install the **Ruby** environment in version 2.7 using winget (needed for E2E tests):
  ```
  winget install RubyInstallerTeam.Ruby.2.7 --force --disable-interactivity  --accept-source-agreements --accept-package-agreements
  ```
* Install Ruby installer toolkit to be able to compile native extensions:
  ```
  ridk install
  ```
* Install some more packages:
  * `winget install zstandard` — decompressing hosted archives
  * `winget install nssm` — running cardano-node as a service

#### Runner configuration

* When launched as a service, the GHA runner runs as the [`Local System Account`](https://learn.microsoft.com/en-us/windows/win32/services/localsystem-account) which does not inherit the environment from the `hal` user. Ensure software installed through `winget` is on the runner's PATH.
* Configure environment secrets (`FIXTURE_DECRYPTION_KEY`, etc.) via the runner's `.env` file or repository/org-level GitHub Actions secrets.
* Ensure node DB files can be removed (they are created readonly which breaks `git clean -xfd`):
  ```
  icacls . /grant hal:F /T /Q
  ```
* Ensure `cardano-node` and `cardano-wallet` services are cleaned up after each run to avoid leaks from interrupted workflows.

#### Troubleshooting

Windows permissions are complex. Stick to the default `Local System Account` as the user for the runner service.

1. Ensure the user `SYSTEM` has full control to the runner work directory and this right is _inherited_:

   ```
   PS C:\actions-runner> icacls.exe .
   . NT AUTHORITY\SYSTEM:(OI)(CI)(F)
     BUILTIN\Administrators:(F)
     ZUR1-S-D-027\hal:(OI)(CI)(F)
   ```

2. To operate under the right identity, use [pstools](https://learn.microsoft.com/en-us/sysinternals/downloads/pstools):
   ```
   psexec -s -i cmd
   ```

3. If steps fail to delete the checkout directory, ensure no other process is locking it.

### macOS machine (hal-mac)

The macOS builder runs on a Mac Mini (Apple Silicon) managed via nix-darwin. Access via SSH through the jumpbox:

```bash
ssh mac-builder  # requires SSH config with ProxyJump through jumpbox-dev
```

#### Runner configuration

The GHA runner runs as a launchd service. Key paths:

- **Service plist**: `/Library/LaunchDaemons/org.nixos.github-runner-hal-mac.plist`
- **Runner directory**: `/var/lib/github-runner-hal-mac/`
- **Log file**: check via `journalctl` or the runner's `_diag/` directory

#### Updating the runner token

If the runner token expires or becomes invalid:

1. **Create a new runner token** at the repository's Settings > Actions > Runners page
2. **Update the token on the machine**:
   ```bash
   ssh mac-builder
   # Re-configure the runner with the new token
   ```
3. **Restart the runner**:
   ```bash
   sudo launchctl kickstart -k system/org.nixos.github-runner-hal-mac
   ```
4. **Verify the runner is connected** in the repository's Settings > Actions > Runners page

#### Environment variables

Secrets are configured via:

- `ATTIC_TOKEN` — from `/var/lib/gha-runner-hal-mac/env-attic-token`
- `FIXTURE_DECRYPTION_KEY` — from `/var/lib/gha-runner-hal-mac/env-fixture-decryption-key`
- `HAL_E2E_PREPROD_MNEMONICS` — from `/var/lib/gha-runner-hal-mac/env-hal-e2e-preprod-mnemonics`

#### Troubleshooting

- **Check runner status**: repository Settings > Actions > Runners
- **View logs**: check the runner's `_diag/` directory
- **Restart service**: `sudo launchctl kickstart -k system/org.nixos.github-runner-hal-mac`
- **Stop service**: `sudo launchctl stop system/org.nixos.github-runner-hal-mac`

##### Attic cache failures

The Attic cache job pushes build artifacts to the Attic cache server. If it fails:

1. **Check Attic token** — The JWT token in `/var/lib/gha-runner-hal-mac/env-attic-token` may have expired. Decode it:
   ```bash
   cat /var/lib/gha-runner-hal-mac/env-attic-token | cut -d. -f2 | base64 -d
   ```
   Look for the `exp` field (Unix timestamp).

2. **Test Attic login**:
   ```bash
   ATTIC_TOKEN=$(cat /var/lib/gha-runner-hal-mac/env-attic-token)
   nix-shell -p attic-client --run "attic login adrestia https://attic.cf-app.org/ $ATTIC_TOKEN"
   ```

3. **Verify Attic server** is reachable: `curl -I https://attic.cf-app.org/`

4. **SSL Certificate Error** — If you see `invalid peer certificate: UnknownIssuer`, the machine may be resolving `attic.cf-app.org` to an internal IP with an untrusted certificate.

   Check which IP is being used:
   ```bash
   ping -c1 attic.cf-app.org
   ```

   If it resolves to an internal IP (e.g., `10.1.21.x`), override with the external IP in `/etc/hosts`:
   ```bash
   sudo sed -i '' 's/ attic.cf-app.org//g; s/ attic//g' /etc/hosts
   echo "195.48.82.220 attic.cf-app.org" | sudo tee -a /etc/hosts
   ```

##### Stale processes

Test cluster processes (cardano-node) may accumulate if builds are interrupted:
```bash
ps aux | grep cardano-node
pkill -f "cardano-node.*test-cluster"
```
