#!/usr/bin/env -S nix shell nixpkgs#bash nixpkgs#jq --inputs-from . --command bash

set -euo pipefail

. "$(dirname "$0")/../cabal-lib.sh"

jq -r '.["install-plan"][]|select(."style"=="local")|select(."component-name"|test("^test:"))|"\(."pkg-name"):\(."component-name")"' "$plan_json"
