# GitHub Actions Self-Hosted Runners

Self-hosted runners for Linux unit tests on `builder-new` (10.1.21.17).

## Setup

### 1. Create user

```bash
sudo useradd -m -s /bin/bash gha-runner
sudo usermod -aG nix-trusted-users gha-runner  # if using trusted-users group
```

Ensure `/etc/nix/nix.conf` includes `gha-runner` in `trusted-users` (or its group).

### 2. Install runner instances

Run as `gha-runner`:

```bash
REPO_URL="https://github.com/cardano-foundation/cardano-wallet"
RUNNER_VERSION="2.322.0"  # check https://github.com/actions/runner/releases

for i in $(seq 1 12); do
  mkdir -p ~/actions-runner-$i && cd ~/actions-runner-$i
  curl -sL "https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-linux-x64-${RUNNER_VERSION}.tar.gz" | tar xz
  ./config.sh --url "$REPO_URL" \
    --token "$(gh api repos/cardano-foundation/cardano-wallet/actions/runners/registration-token -q .token)" \
    --name "builder-new-$i" \
    --labels "self-hosted,linux,x86_64,cardano-wallet" \
    --unattended
done
```

### 3. systemd service

Install the service template as root:

```bash
sudo cp gha-runner@.service /etc/systemd/system/
sudo systemctl daemon-reload
for i in $(seq 1 12); do
  sudo systemctl enable --now gha-runner@$i
done
```

### 4. Verify

```bash
systemctl status gha-runner@{1..12}
```

Runners should appear at:
https://github.com/cardano-foundation/cardano-wallet/settings/actions/runners

## Maintenance

### Work directory cleanup

Runner work directories grow over time. Set up a weekly cron:

```cron
0 3 * * 0 find /home/gha-runner/actions-runner-*/\_work -maxdepth 1 -mindepth 1 -mtime +7 -exec rm -rf {} +
```

### Scaling

With 12 runners and ~37 jobs across 3 workflows, most jobs run in parallel.
Each runner uses its own work directory but shares the host Nix store
via the daemon socket.

### Removing runners

```bash
sudo systemctl stop gha-runner@$i
cd /home/gha-runner/actions-runner-$i
./config.sh remove --token "$(gh api repos/cardano-foundation/cardano-wallet/actions/runners/registration-token -q .token)"
```
