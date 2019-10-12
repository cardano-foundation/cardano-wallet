#!/usr/bin/env bash

set -euo pipefail

exec $(nix-build `dirname $0`/../lib.nix -A nix-tools.regeneratePackages --no-out-link)
