# Implementation Plan: Fix Linux Memory Benchmark Temp Directory

**Branch**: `5283-linux-memory-benchmark-tmpdir` | **Date**: 2026-05-12 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/code/cardano-wallet/specs/005-fix-linux-memory-benchmark-tmpdir/spec.md`

## Summary

Fix the Linux Benchmarks workflow so only the Memory Benchmark matrix entry switches from the long GitHub runner temp path to a short, unique directory under `/tmp`. The workflow will create that directory with `mktemp`, export it as `TMPDIR` before running the existing memory benchmark command, and clean it with a shell trap. Other benchmark jobs keep `${{ runner.temp }}`.

## Technical Context

**Language/Version**: GitHub Actions YAML and Bash
**Primary Dependencies**: Existing self-hosted Linux benchmark runners, `scripts/ci/bench-memory.sh`, `mktemp`
**Storage**: Temporary files under `/tmp/cwbench.XXXXXX` for the memory benchmark only
**Testing**: `actionlint` plus a local path-length smoke check
**Target Platform**: Linux self-hosted benchmark runners
**Project Type**: CI workflow maintenance
**Performance Goals**: No benchmark behavior change; restore missing memory benchmark artifacts
**Constraints**: Avoid a fixed shared `/tmp` directory while keeping the final `node.socket` path below 108 bytes
**Scale/Scope**: One workflow step

## Design Decisions

- Use `mktemp -d /tmp/cwbench.XXXXXX` inside the Memory Benchmark step. This gives a short path and a unique per-step directory without relying on long runner work paths.
- Keep `${{ runner.temp }}` as the default step `TMPDIR`. This preserves the previous isolation decision for benchmark jobs that do not hit the socket path limit.
- Clean the short root with `trap 'rm -rf "$TMPDIR"' EXIT`. The existing `bench-memory.sh` still removes its own `bench/memory` subdirectory, and the outer trap removes the temporary root.

## Proof Strategy

- **Regression proof**: The old runner temp root from issue #5283 produces a representative memory benchmark socket path of 120 bytes, exceeding the 108-byte Unix socket limit.
- **Green proof**: The new `/tmp/cwbench.XXXXXX` root produces a representative memory benchmark socket path below 108 bytes.
- **Workflow proof**: Run `actionlint` on `.github/workflows/linux-benchmarks.yml`, ignoring only the repository's known custom self-hosted runner label warnings.
- **Boundary follow-up**: Confirm the next GitHub Actions workflow dispatch or `master` Linux Benchmarks run produces the memory heap profile, SVG, and CSV artifacts.

## Vertical Slice

One bisect-safe CI slice changes `.github/workflows/linux-benchmarks.yml` and carries its specification, tasks, local review notes, and gate script.
