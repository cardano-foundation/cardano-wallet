# Plan

## Strategy

Extend the existing post-publication workflow with a separately testable
merge-back operation. Keep GitHub mutation at the workflow boundary and put
deterministic tag/branch/duplicate decisions behind a local test surface.

## Live boundary

The GitHub release event, Git refs, and pull-request API are the live boundary.
Tests substitute frozen command responses; the workflow remains responsible
for authenticated mutation.

## Slice S1

Add stable-release selection, tag/head validation, idempotent PR creation,
focused regression tests, and the minimum workflow permissions.
