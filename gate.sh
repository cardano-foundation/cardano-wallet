#!/usr/bin/env bash
# Mechanical gate for PR #5329 (Windows CI: cardano-node.exe for wallet-unit).
#
# Deliberately eval-only. A real `nix build .#ci.artifacts.win64.tests.wallet-unit`
# needs ~1007 derivations built locally (whole Haskell world cross-compiled to
# mingw32) and is not usable as a per-slice gate. `nix derivation show` answers
# the same question — "does the bundle pull in the Windows cardano-node?" — in
# seconds, because nix/windows-test-exe.nix unconditionally copies
# ${pkg}/bin/*.exe for every extraPkgs entry.
#
# Slice A checks are active from slice A onward; slice B checks tolerate the
# pre-slice-B state so the gate is runnable during slice A too.
set -euo pipefail

cd "$(dirname "$0")"

fail() { printf 'GATE FAIL: %s\n' "$*" >&2; exit 1; }
ok()   { printf 'GATE  ok : %s\n' "$*"; }

NODE_DRV_RE='/nix/store/[a-z0-9]+-cardano-node-exe-[^"]*\.drv'

# ---------------------------------------------------------------- slice A ----
printf 'gate: evaluating win64 derivations (eval only, no build)...\n'

integration_node=$(nix derivation show .#ci.artifacts.win64.integration \
  | grep -oE "$NODE_DRV_RE" | sort -u)
[ -n "$integration_node" ] \
  || fail "reference job broke: .#ci.artifacts.win64.integration no longer references a Windows cardano-node derivation"
[ "$(printf '%s\n' "$integration_node" | wc -l)" -eq 1 ] \
  || fail "expected exactly one cardano-node drv in integration, got:\n$integration_node"
ok "integration references $integration_node"

unit_node=$(nix derivation show .#ci.artifacts.win64.tests.wallet-unit \
  | grep -oE "$NODE_DRV_RE" | sort -u || true)
[ -n "$unit_node" ] \
  || fail "tests.wallet-unit references no Windows cardano-node derivation (FR1 unmet)"
[ "$unit_node" = "$integration_node" ] \
  || fail "tests.wallet-unit uses a different cardano-node than integration:\n  unit:        $unit_node\n  integration: $integration_node"
ok "tests.wallet-unit references the same cardano-node as integration"

# FR4: the shared builder must not have been re-parameterised.
git diff --quiet "$(git merge-base HEAD origin/master)" -- nix/windows-test-exe.nix \
  || fail "nix/windows-test-exe.nix was modified; the extraPkgs mechanism was supposed to suffice (FR4)"
ok "nix/windows-test-exe.nix untouched"

# ---------------------------------------------------------------- slice B ----
WF=.github/workflows/windows.yml

# No python on this host's default PATH; borrow one with pyyaml from the
# nixpkgs registry entry (~1s, fully cached).
py() {
  nix shell --impure --quiet --expr \
    'with builtins.getFlake "nixpkgs"; legacyPackages.${builtins.currentSystem}.python3.withPackages(ps: [ ps.pyyaml ])' \
    -c python3 "$@"
}

py -c "import sys,yaml; yaml.safe_load(open('$WF'))" \
  || fail "$WF is not valid YAML"
ok "$WF parses as YAML"

# The unit-tests "Run tests" step must carry both the explicit pwsh shell and
# the PATH prepend, mirroring integration-smoke. Scoped to the unit-tests job.
py - "$WF" <<'PY' || exit 1
import os, sys, yaml
wf = yaml.safe_load(open(sys.argv[1]))
job = wf["jobs"]["unit-tests"]
steps = {s.get("name"): s for s in job["steps"]}
run = steps.get("Run tests")
if run is None:
    print("GATE FAIL: unit-tests has no 'Run tests' step", file=sys.stderr); sys.exit(1)
missing = []
if run.get("shell") != "pwsh":
    missing.append("shell: pwsh")
if '$env:PATH' not in run.get("run", ""):
    missing.append('$env:PATH prepend')
if missing:
    msg = "unit-tests/Run tests still missing: " + ", ".join(missing)
    if os.environ.get("GATE_STRICT_B"):
        print("GATE FAIL: " + msg, file=sys.stderr); sys.exit(1)
    print("GATE PENDING (slice B): " + msg)
else:
    print("GATE  ok : unit-tests/Run tests sets pwsh + prepends the bundle dir to PATH")
# integration-smoke must keep its own PATH line untouched (FR3).
smoke = {s.get("name"): s for s in wf["jobs"]["integration-smoke"]["steps"]}
step = next(s for n, s in smoke.items() if n and n.startswith("Run TRANS_CREATE"))
assert '$env:PATH = "$(Get-Location);$env:PATH"' in step["run"], \
    "integration-smoke PATH line was altered (FR3)"
print("GATE  ok : integration-smoke PATH line intact")
PY

printf 'GATE PASS\n'
