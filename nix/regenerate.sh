#!/usr/bin/env bash

set -euo pipefail

nix="$(dirname "$0")/default.nix"

# Regenerate stack-to-nix files in ./.stack-nix
$(nix-build "$nix" -A stackNixRegenerate --no-out-link) "$@"

# Regenerate materialized haskell-build-tools in ./materialized
$(nix-build "$nix" -A haskell-build-tools.regenerateMaterialized --no-out-link)/bin/regenerate-materialized-nix

# Regenerate materialized iohk-nix-utils in ./materialized
$(nix-build "$nix" -A iohk-nix-utils.regenerateMaterialized --no-out-link)/bin/regenerate-materialized-nix
