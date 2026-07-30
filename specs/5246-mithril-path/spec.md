# Spec: Use mithril-client from PATH when available, download as fallback

## P1 User Story

As a developer running E2E tests inside `nix develop`, I want the test suite to use the mithril-client binary already available on PATH (from the flake input) instead of downloading it from GitHub releases, so that:
- Tests start faster (no network download)
- There is no version drift between the flake pin (2617.0) and the hardcoded download URL (2603.1)
- The Nix flake pin is the single source of truth for the mithril-client version

## User Stories

1. **Nix shell developer**: When I run E2E tests inside `nix develop`, the test suite finds `mithril-client` on PATH and uses it directly without making any HTTP request to GitHub releases.

2. **Windows/non-Nix user**: When I run E2E tests outside a Nix environment (e.g., on Windows or a system without Nix), the test suite falls back to downloading mithril-client from GitHub releases, preserving the existing behavior.

3. **CI/CD pipeline**: When the E2E tests run in an environment where mithril-client is pre-installed on PATH, the tests use it; otherwise, they download it.

## Functional Requirements

### FR1: PATH lookup before download
`Cardano.Launcher.Mithril.downloadMithril` MUST check for `mithril-client` on PATH using `System.Directory.findExecutable` BEFORE attempting any download.

### FR2: No HTTP request when PATH hit
When `mithril-client` is found on PATH, the function MUST return immediately without making any HTTP request (no `httpBS`, no `curl.exe` invocation).

### FR3: Fallback download preserved
When `mithril-client` is NOT found on PATH, the function MUST fall back to the existing download logic (GitHub releases via `httpBS` on Unix, `curl.exe` on Windows).

### FR4: Observable behavior
The function MUST log/trace which path was taken (PATH hit vs. download fallback) so that the "no HTTP request" requirement can be verified.

### FR5: Version source of truth
When PATH is used, the Nix flake pin (currently 2617.0) is the source of truth. When download fallback is used, the hardcoded version (currently 2603.1) applies. This is acceptable because the fallback is only for non-Nix environments.

## Success Criteria

1. **AC1**: `Cardano.Launcher.Mithril` checks PATH (`findExecutable "mithril-client"`) before downloading (unit: exact lookup string; production wiring by inspection + live probe).
2. **AC2**: When `mithril-client` is on PATH (e.g., inside `nix develop`), production `downloadMithril` returns the PATH binary and does not run the download fallback (verified via PATH log line and a live probe with an impossible working directory that would fail if fallback ran). Static E2E wiring remains `downloadLatestSnapshot dir =<< downloadMithril dir` in `lib/integration/exe/e2e.hs`. Full preprod snapshot download is **not** required for this AC.
3. **AC3**: Fallback download path still works on Windows and non-Nix environments (fallback body preserved; unit proves fallthrough + working-directory forwarding).
4. **AC4**: No hardcoded version drift when PATH is used: the Nix flake pin is the source of truth for the PATH binary.

## Task IDs

Canonical unique numeric IDs: **T001**–**T007** (see `tasks.md` / `plan.md`).

## Non-Goals

- Changing the hardcoded download version (2603.1) to match the flake pin (2617.0) — the fallback is for non-Nix environments where the flake pin doesn't apply.
- Modifying the flake.nix or Nix configuration.
- Changing the E2E test logic beyond the mithril-client acquisition path.
- Using `e2e -- --help` as PATH proof (does not enter `downloadMithril`).
