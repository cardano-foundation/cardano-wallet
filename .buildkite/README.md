# Buildkite Configuration

The Cardano Wallet uses [buildkite](https://buildkite.com/cardano-foundation/cardano-wallet) as its main CI, with some actions still being run on [GitHub](https://github.com/cardano-foundation/cardano-wallet/actions). The pipeline(s) are run on self-hosted Linux/MacOS/Windows machines and we list here some requirements for those machines.

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
  set AWS_DEFAULT_REGION=<needed to retrieve snapshot>
  set AWS_ACCESS_KEY_ID=<needed to retrieve snapshot>
  set AWS_SECRET_ACCESS_KEY=<needed to retrieve snapshot>
  ```
* Configure buildkite agent to [run as a service](https://buildkite.com/docs/agent/v3/windows#running-as-a-service)
  * when launched as a service, `buildkite-agent.exe` runs as the [`Local System Account`](https://learn.microsoft.com/en-us/windows/win32/services/localsystem-account) user which is a special user dedicated to such tasks. As such, it does not inherit the environment from the `hal` user which is the standard user we use. To ensure software installed through `winget` are found, we need to explicitly add it to the agent's executors path.
* Define `hooks\pre-checkout.bat` to ensure node DB files can be removed (they are created readonly which breaks `git clean -xfd`)

  ```
  set PATH=%PATH%;C:\Users\hal\AppData\Local\Microsoft\WinGet\Links
  icacls . /grant hal:F /T /Q
  ```
