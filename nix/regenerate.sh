#!/usr/bin/env bash

set -euo pipefail

exec $(nix-build `dirname $0`/. -A regenerateStackPackages --no-out-link)
