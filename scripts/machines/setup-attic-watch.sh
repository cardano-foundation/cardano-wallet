#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ -z "${ATTIC_TOKEN:-}" ]; then
  echo "ERROR: ATTIC_TOKEN environment variable is required" >&2
  exit 1
fi

# Install attic-client if not already on PATH
if ! command -v attic &>/dev/null; then
  echo "Installing attic-client..."
  nix profile install github:zhaofengli/attic
fi

# Configure attic login
echo "Configuring attic login for adrestia cache..."
attic login adrestia https://attic.cf-app.org/ "$ATTIC_TOKEN"

# Detect OS and install the appropriate service
case "$(uname -s)" in
  Linux)
    echo "Installing systemd service..."
    cp "$SCRIPT_DIR/attic-watch-store.service" /etc/systemd/system/
    systemctl daemon-reload
    systemctl enable --now attic-watch-store.service
    echo "Verifying..."
    if systemctl is-active --quiet attic-watch-store.service; then
      echo "attic-watch-store is running"
    else
      echo "ERROR: service failed to start" >&2; exit 1
    fi
    ;;
  Darwin)
    echo "Installing launchd plist..."
    cp "$SCRIPT_DIR/com.attic.watch-store.plist" /Library/LaunchDaemons/
    launchctl bootstrap system /Library/LaunchDaemons/com.attic.watch-store.plist
    echo "Verifying..."
    if launchctl print system/com.attic.watch-store &>/dev/null; then
      echo "attic-watch-store is running"
    else
      echo "ERROR: service failed to start" >&2; exit 1
    fi
    ;;
  *)
    echo "ERROR: unsupported OS: $(uname -s)" >&2
    exit 1
    ;;
esac
