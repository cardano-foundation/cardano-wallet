# Buildkite Configuration

> The Cardano Wallet uses [buildkite](https://buildkite.com/cardano-foundation/cardano-wallet) as its main CI, with some actions still being run on [GitHub](https://github.com/cardano-foundation/cardano-wallet/actions). The pipeline(s) are run on self-hosted Linux/MacOS/Windows machines and we list here some requirements for those machines.

## Workflows

* [pipeline](pipeline.yml): The main build pipeline for Cardano wallet, builds all artifacts and run all the tests on all supported environements (linux, MacOS, Windows)
* [release](release.yml): Release pipeline, runs nightly to create _release candidate_ branch and can be triggered manually for releasing (see [release process](../docs/site/src/contributor/how/release-process.md)
* [restoration-benchmark](restoration-benchmarks.yml): A special pipeline loooong running pipeline (7 hours+) benchmarking restoration time on mainnet. This needs to be triggered manually before releasing to prevent regressions, but it's run weekly on Sundays.

## Windows Machine

### System configuration

We assume the machine is configured with a recent windows version (2022 Server?) and has winget installed.

* Install [Windows Buildkite](https://buildkite.com/docs/agent/v3/windows) agent
  * Note the "automated" Powershell install did not work so the agent was installed following the manual process
* Install the **Ruby** environment in version 2.7 using winget (the E2E tests are defined using Ruby, in a version which noone ever bothered upgrading:
  ```
  winget install RubyInstallerTeam.Ruby.2.7 --force --disable-interactivity  --accept-source-agreements --accept-package-agreements
  ```
  This was done once through the pipeline step and then removed because we don't want to pay the 3 minutes price of forcing Ruby installation whereas it could just know it's already installed and skip it
* Install Ruby installer toolkit to be able to compile native extensions
  ```
  ridk install
  ```
* Install some more packages
  * `winget install zstandard` is probably needed for decompressing hosted archive (but probably not as it's natively suppported by tar)
  * `winget install nssm` which is needed to run cardano-node as a service

### Buildkite Agent configuration

* Use powershell for scripts and have a wider definition of redacted variables

  ```
  # user powershell (because, why not?)
  shell="C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe"

  redacted-vars="*PASSWORD*,*SECRET*,*TOKEN*,*KEY*,*_CONNECTION_STRING"
  ```
* Define `hooks\environment.bat` to store secrets needed to run the tests:

  ```
  set BUILDKITE_API_TOKEN=<needed to retrieve artifacts stored by buildkite steps>
  set FIXTURE_DECRYPTION_KEY=<needed to decrypt gpg store containing wallet keys for E2E tests>
  ```
* Configure buildkite agent to [run as a service](https://buildkite.com/docs/agent/v3/windows#running-as-a-service)
  * when launched as a service, `buildkite-agent.exe` runs as the [`Local System Account`](https://learn.microsoft.com/en-us/windows/win32/services/localsystem-account) user which is a special user dedicated to such tasks. As such, it does not inherit the environment from the `hal` user which is the standard user we use. To ensure software installed through `winget` are found, we need to explicitly add it to the agent's executors path.
* Define `hooks\pre-checkout.bat` to ensure node DB files can be removed (they are created readonly which breaks `git clean -xfd`)

  ```
  set PATH=%PATH%;C:\Users\hal\AppData\Local\Microsoft\WinGet\Links
  icacls . /grant hal:F /T /Q
  ```
* Define `hooks\pre-exit.bat` that (tries to) ensures `cardano-node` and `cardano-wallet` services are removed. It could be the case manually interrupting the CI or some cosmic ray glitch will leave the services running after the workflow stops.

### Troubleshooting

Windows permissions are quite complex and beyond the cognitive abilities of most human beings. Although it's not great security wise, it's a good idea to stick to the default `Local System Account` as user for the buildkite service and avoid switching between local user (eg. `hal`) and this predefined user.

1. Ensure the user `SYSTEM` has full control to `C:\buildkite-agent` and this right is _inherited_. It should look something like:

   ```
   PS C:\buildkite-agent> icacls.exe .
   . NT AUTHORITY\SYSTEM:(OI)(CI)(F)
     BUILTIN\Administrators:(F)
     ZUR1-S-D-027\hal:(OI)(CI)(F)

   Successfully processed 1 files; Failed processing 0 files
   ```

2. To ensure one operates under the right identities, download [pstools](https://learn.microsoft.com/en-us/sysinternals/downloads/pstools) and open a terminal as a `SYSTEM` user:

   ```
   psexec -s -i cmd
   ```
3. If some steps fail to delete checkout directory, ensure there's no other process on the machine (in particular, shells...) locking the directory

## macOS Machine (hal-mac)

The macOS builder runs on a Mac Mini (Apple Silicon) managed via nix-darwin. The machine is accessible via SSH through the jumpbox:

```bash
ssh mac-builder  # requires SSH config with ProxyJump through jumpbox-dev
```

### Buildkite Agent Configuration

The agent runs as a launchd service under the `buildkite-agent-hal-mac` user:

- **Service plist**: `/Library/LaunchDaemons/org.nixos.buildkite-agent-hal-mac.plist`
- **Config file**: `/var/lib/buildkite-agent-hal-mac/buildkite-agent.cfg`
- **Token file**: `/var/lib/buildkite-agent-hal-mac/buildkite-token`
- **Log file**: `/var/lib/buildkite-agent-hal-mac/buildkite-agent.log`
- **Agent tags**: `queue=cardano-wallet,system=aarch64-darwin`

### Updating the Agent Token

If the Buildkite agent token expires or becomes invalid, the agent will fail to connect with `401 Unauthorized: Invalid access token` errors in the log.

To update the token:

1. **Create a new agent token** in Buildkite:
   - Go to https://buildkite.com/organizations/cardano-foundation/agents#tokens
   - Click "New Token" and give it a description (e.g., "hal-mac")
   - Copy the token value (it's only shown once)

2. **Update the token on the machine**:
   ```bash
   ssh mac-builder
   sudo tee /var/lib/buildkite-agent-hal-mac/buildkite-token <<< 'YOUR_NEW_TOKEN'
   ```

3. **Restart the agent**:
   ```bash
   sudo launchctl kickstart -k system/org.nixos.buildkite-agent-hal-mac
   ```

4. **Verify the agent is running**:
   ```bash
   pgrep -fl buildkite-agent
   tail -20 /var/lib/buildkite-agent-hal-mac/buildkite-agent.log
   ```

### Environment Variables

The buildkite hooks (`/nix/store/.../buildkite-agent-hooks/environment`) set up:

- `ATTIC_TOKEN` - from `/var/lib/buildkite-agent-hal-mac/env-attic-token`
- `FIXTURE_DECRYPTION_KEY` - from `/var/lib/buildkite-agent-hal-mac/env-fixture-decryption-key`
- `HAL_E2E_PREPROD_MNEMONICS` - from `/var/lib/buildkite-agent-hal-mac/env-hal-e2e-preprod-mnemonics`

### Troubleshooting

- **Check agent status**: `pgrep -fl buildkite-agent`
- **View logs**: `tail -f /var/lib/buildkite-agent-hal-mac/buildkite-agent.log`
- **Restart service**: `sudo launchctl kickstart -k system/org.nixos.buildkite-agent-hal-mac`
- **Stop service**: `sudo launchctl stop system/org.nixos.buildkite-agent-hal-mac`

#### Attic Cache Failures

The "Dev Shell Attic Cache (macos)" job pushes build artifacts to the Attic cache server. If it fails:

1. **Check Attic token** - The JWT token in `/var/lib/buildkite-agent-hal-mac/env-attic-token` may have expired. Decode the token to check:
   ```bash
   cat /var/lib/buildkite-agent-hal-mac/env-attic-token | cut -d. -f2 | base64 -d
   ```
   Look for the `exp` field (Unix timestamp).

2. **Test Attic login**:
   ```bash
   ATTIC_TOKEN=$(cat /var/lib/buildkite-agent-hal-mac/env-attic-token)
   nix-shell -p attic-client --run "attic login adrestia https://attic.cf-app.org/ $ATTIC_TOKEN"
   ```

3. **Verify Attic server** is reachable: `curl -I https://attic.cf-app.org/`

4. **SSL Certificate Error** - If you see:
   ```
   invalid peer certificate: UnknownIssuer
   ```

   The mac-builder may be resolving `attic.cf-app.org` to an internal IP (via internal DNS) that serves a certificate signed by the internal CA, which is not in the system trust store.

   Check which IP is being used:
   ```bash
   ping -c1 attic.cf-app.org
   ```

   If it resolves to an internal IP (e.g., `10.1.21.x`), override with the external IP in `/etc/hosts`:
   ```bash
   # First, remove any existing internal DNS entry for attic from /etc/hosts
   sudo sed -i '' 's/ attic.cf-app.org//g; s/ attic//g' /etc/hosts
   # Then add the external IP
   echo "195.48.82.220 attic.cf-app.org" | sudo tee -a /etc/hosts
   ```

   The external server uses a Let's Encrypt certificate which is trusted by default.

#### Stale Processes

Test cluster processes (cardano-node) may accumulate if builds are interrupted:
```bash
ps aux | grep cardano-node
# Kill orphaned processes if needed
pkill -f "cardano-node.*test-cluster"
```
