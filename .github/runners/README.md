# GitHub Actions Self-Hosted Runners

## Machines

| Host | IP | Cores | RAM | Runners | Labels |
|---|---|---|---|---|---|
| builder-new | 10.1.21.17 | 64 | 125GB | 12 (`builder-new-{1..12}`) | `cardano-wallet` |
| benchmark-new | 10.1.21.18 | 64 | 125GB | 4 (`benchmark-new-{1..4}`) | `cardano-wallet-bench` |

All runners share the host Nix store via the daemon socket.

## Setup (per machine)

### 1. Create user and grant Nix access

```bash
sudo useradd -m -s /bin/bash gha-runner
# Add gha-runner to trusted-users in /etc/nix/nix.conf
sudo sed -i 's/^trusted-users = .*/& gha-runner/' /etc/nix/nix.conf
sudo systemctl restart nix-daemon
```

### 2. Install dependencies

```bash
sudo apt-get install -y libicu76 liblttng-ust1t64  # Debian 13
```

### 3. Install runner instances

Run as `gha-runner`:

```bash
REPO_URL="https://github.com/cardano-foundation/cardano-wallet"
RUNNER_VERSION="2.322.0"  # check https://github.com/actions/runner/releases
COUNT=12       # builder-new: 12, benchmark-new: 4
PREFIX="builder-new"  # or "benchmark-new"
LABELS="self-hosted,linux,x86_64,cardano-wallet"  # or "...,cardano-wallet-bench"

cd ~
curl -sL "https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-linux-x64-${RUNNER_VERSION}.tar.gz" -o runner.tar.gz

for i in $(seq 1 $COUNT); do
  mkdir -p actions-runner-$i
  tar xzf runner.tar.gz -C actions-runner-$i
  cd actions-runner-$i
  ./config.sh --url "$REPO_URL" \
    --token "$(gh api repos/cardano-foundation/cardano-wallet/actions/runners/registration-token -q .token)" \
    --name "${PREFIX}-$i" \
    --labels "$LABELS" \
    --work _work \
    --unattended
  cd ~
done
rm runner.tar.gz
```

### 4. systemd service

Install the service template as root:

```bash
sudo cp gha-runner@.service /etc/systemd/system/
sudo systemctl daemon-reload
for i in $(seq 1 $COUNT); do
  sudo systemctl enable --now gha-runner@$i
done
```

### 5. Verify

```bash
systemctl status gha-runner@{1..N}
```

Runners should appear at:
https://github.com/cardano-foundation/cardano-wallet/settings/actions/runners

## Maintenance

### Work directory cleanup

Runner work directories grow over time. Set up a weekly cron:

```cron
0 3 * * 0 find /home/gha-runner/actions-runner-*/_work -maxdepth 1 -mindepth 1 -mtime +7 -exec rm -rf {} +
```

### Removing runners

```bash
sudo systemctl stop gha-runner@$i
cd /home/gha-runner/actions-runner-$i
./config.sh remove --token "$(gh api repos/cardano-foundation/cardano-wallet/actions/runners/registration-token -q .token)"
```
