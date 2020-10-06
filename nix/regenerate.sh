#!/usr/bin/env bash

set -euo pipefail

# Provide default value for LOCALE_ARCHIVE environment variable
: "${LOCALE_ARCHIVE:=/usr/lib/locale/locale-archive}"
export LOCALE_ARCHIVE

exec $(nix-build `dirname $0`/. -A stackNixRegenerate --no-out-link)
