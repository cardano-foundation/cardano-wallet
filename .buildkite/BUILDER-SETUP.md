# Buildkite Builder Setup

This document describes how to set up a new Buildkite builder machine for the cardano-wallet CI infrastructure.

## Overview

The Buildkite builders are Debian-based VMs that run CI jobs for cardano-wallet. They use:
- **Nix** for reproducible builds
- **Buildkite Agent** for job execution
- **Attic** for Nix binary cache

## Machine Naming Convention

Builders are named based on their IP address's last octet:
- `builder` (10.1.21.12) → `machine:zurich.12-agent`
- `builder-new` (10.1.21.17) → `machine:zurich.17-agent`

## Prerequisites

- Debian 13 (Trixie) or compatible
- SSH access via jumpbox (`jumpbox-dev` at `dev.colo-primary.cf-systems.org`)
- Sufficient resources (recommended: 64+ cores, 125GB+ RAM, 2TB+ disk)

## Setup Steps

### 1. Install Nix

If Nix is already installed but not in PATH, add to `/etc/profile.d/`:

```bash
sudo ln -sf /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh /etc/profile.d/nix.sh
```

For user-specific setup (e.g., in `~/.bashrc`):

```bash
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
```

### 2. Create nixbld Group and Users

Nix multi-user mode requires build users:

```bash
sudo groupadd -g 30000 nixbld

for i in $(seq 1 32); do
  sudo useradd -c "Nix build user $i" \
    -d /var/empty \
    -g nixbld \
    -G nixbld \
    -M -N -r \
    -s /sbin/nologin \
    -u $((30000 + i)) \
    nixbld$i
done
```

### 3. Configure Nix

Create `/etc/nix/nix.conf`:

```ini
experimental-features = nix-command flakes
trusted-users = buildkite-agent paolino
build-users-group = nixbld
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
max-jobs = 48
```

### 4. Enable Nix Daemon

```bash
sudo ln -sf /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.service /etc/systemd/system/
sudo ln -sf /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.socket /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now nix-daemon.socket
```

### 5. Install Buildkite Agent

```bash
# Add Buildkite GPG key and repository
curl -fsSL https://keys.openpgp.org/vks/v1/by-fingerprint/32A37959C2FA5C3C99EFBC32A79206696452D198 \
  | gpg --dearmor -o /usr/share/keyrings/buildkite-agent-archive-keyring.gpg

echo "deb [signed-by=/usr/share/keyrings/buildkite-agent-archive-keyring.gpg] https://apt.buildkite.com/buildkite-agent stable main" \
  | sudo tee /etc/apt/sources.list.d/buildkite-agent.list

sudo apt-get update
sudo apt-get install -y buildkite-agent
```

### 6. Configure Buildkite Agent

Edit `/etc/buildkite-agent/buildkite-agent.cfg`:

```ini
# Get token from Buildkite UI: Organization Settings → Agents → Agent Token
token="YOUR_AGENT_TOKEN"

# Name format: machine:zurich.XX-agent:%spawn (XX = last IP octet)
name="machine:zurich.17-agent:%spawn"

# Number of parallel agents
spawn=30

# Queue tags
tags="queue=adrestia,queue=cardano-wallet,system=x86_64-linux"

# Paths
build-path="/var/lib/buildkite-agent/builds"
hooks-path="/etc/buildkite-agent/hooks"
plugins-path="/etc/buildkite-agent/plugins"

# Logging
no-color=true
```

### 7. Create Environment Hook

Create `/etc/buildkite-agent/hooks/environment`:

```bash
#!/bin/bash

set -e

# SSH agent for git operations
eval `ssh-agent`
ssh-add ~/.ssh/ed25519
export $SSH_AGENT_SOCK

# Add nix to PATH explicitly (required for buildkite-agent user)
export PATH="/nix/var/nix/profiles/default/bin:$PATH"
if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix.sh ]; then
    . /nix/var/nix/profiles/default/etc/profile.d/nix.sh
    nix --version
fi

# Environment variables (get these from existing builder or secrets manager)
export GITHUB_TOKEN="..."
export GH_TOKEN="..."
export BUILDKITE_API_TOKEN="..."
export FIXTURE_DECRYPTION_KEY="..."
export ATTIC_TOKEN="..."
export BUMP_DAILY_TOKEN="..."
export BUMP_NIGHTLY_TOKEN="..."
export BUMP_RELEASE_TOKEN="..."
export BUMP_TEST_TOKEN="..."
export PUSH_ARTIFACTS_TOKEN="..."
export DOCKER_HUB_TOKEN="..."
export GIT_SSH_COMMAND="ssh -vvv"
export HAL_E2E_PREPROD_MNEMONICS="..."
```

Make it executable:

```bash
sudo chmod +x /etc/buildkite-agent/hooks/environment
sudo chown buildkite-agent:buildkite-agent /etc/buildkite-agent/hooks/environment
```

### 8. Copy SSH Keys

Copy SSH keys from existing builder to `/var/lib/buildkite-agent/.ssh/`:

```bash
# From existing builder
ssh builder "sudo tar -C /var/lib/buildkite-agent -cf - .ssh" \
  | ssh builder-new "sudo tar -C /var/lib/buildkite-agent -xf -"

sudo chown -R buildkite-agent:buildkite-agent /var/lib/buildkite-agent/.ssh
sudo chmod 700 /var/lib/buildkite-agent/.ssh
sudo chmod 600 /var/lib/buildkite-agent/.ssh/*
```

### 9. Configure Attic Cache

Create `/var/lib/buildkite-agent/.config/attic/config.toml`:

```toml
default-server = "adrestia"

[servers.adrestia]
endpoint = "https://attic.cf-app.org/"
token = "YOUR_ATTIC_TOKEN"
```

```bash
sudo mkdir -p /var/lib/buildkite-agent/.config/attic
sudo chown -R buildkite-agent:buildkite-agent /var/lib/buildkite-agent/.config
```

### 10. Create /cache Symlink

Some builds expect a `/cache` directory:

```bash
sudo ln -s /tmp /cache
```

### 11. Start Buildkite Agent

```bash
sudo systemctl enable buildkite-agent
sudo systemctl start buildkite-agent
```

Verify agents are running:

```bash
sudo journalctl -u buildkite-agent -f
```

You should see 30 agents registering and "Waiting for instructions..."

## Troubleshooting

### "nix: command not found"

Ensure `/nix/var/nix/profiles/default/bin` is in PATH in the environment hook.

### "experimental Nix feature 'nix-command' is disabled"

Add to `/etc/nix/nix.conf`:
```ini
experimental-features = nix-command flakes
```

Then restart nix-daemon:
```bash
sudo systemctl restart nix-daemon.socket
```

### "the group 'nixbld' does not exist"

Create the nixbld group and users (see step 2).

### "permission denied" for /cache

Create the symlink:
```bash
sudo ln -s /tmp /cache
```

### Token rejected (401 Unauthorized)

Get fresh agent token from Buildkite UI: Organization Settings → Agents → Agent Token

## Migration from Old Builder

When migrating from an old builder:

1. Copy environment hook: `/etc/buildkite-agent/hooks/environment`
2. Copy SSH keys: `/var/lib/buildkite-agent/.ssh/`
3. Copy attic config: `/var/lib/buildkite-agent/.config/attic/`
4. Update agent token if rotated
5. Stop old builder agents before starting new ones (to avoid duplicate job execution)

## References

- [Buildkite Agent Documentation](https://buildkite.com/docs/agent/v3)
- [Nix Manual](https://nixos.org/manual/nix/stable/)
- [Attic Documentation](https://docs.attic.rs/)
