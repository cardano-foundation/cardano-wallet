# Tasks: Use mithril-client from PATH when available

## Planning

- [X] T001: Add spec, plan, and tasks for mithril-path (bootstrap)

## Slice 1 — PATH lookup with fallback

- [X] T002: Strengthen unit proof: PATH-hit asserts exact lookup string `"mithril-client"` and download is not called
- [X] T003: Strengthen unit proof: PATH-miss asserts download receives the exact working directory and returns its result
- [X] T004: Implement PATH lookup in `downloadMithril` with observable branch logs (stdout)
- [X] T005: Focused launcher unit gate + format check green

## Slice 2 — Production-wiring / boundary verification

- [X] T006: Production live PATH probe via `downloadMithril` with impossible working dir (no fallback/HTTP); record static E2E call site
- [X] T007: Finalization (commit-gate trailers, signed commits, desk push authorization)

## Notes

- Do **not** use `e2e -- --help` as PATH proof: `main` runs `getConfig` then `hspec`; `--help` does not enter `aroundAll` / `configureContext` / `downloadMithril`.
- Static E2E call site (unchanged): `downloadLatestSnapshot dir =<< downloadMithril dir` in `lib/integration/exe/e2e.hs`.
