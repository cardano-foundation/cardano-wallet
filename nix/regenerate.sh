#!/usr/bin/env bash

set -euo pipefail

exec $(nix-build `dirname $0`/. -A stackNixRegenerate --no-out-link) "$@"
