# Data Model: Upgrade dependencies to cardano-node 11.0.1

## Dependency Target

- **Fields**: target node version, package name, accepted version/range, source of truth, validation status.
- **Examples**: cardano-node 11.0.1, cardano-api/cardano-cli 11.x, cardano-ledger-conway >= 1.22.1.0, CHaP index-state 2026-05-02T16:21:41Z.
- **Validation**: Every target from issue #5275 maps to a repository change or documented follow-up.

## Node Freeze

- **Fields**: node tag, checkout path, freeze file path, generated date, extracted package version table, CHaP revision/index-state.
- **Example**: cardano-node tag `11.0.1` in `/code/cardano-node`, freeze file `/tmp/cardano-node-11.0.1/cabal.project.freeze`.
- **Validation**: Wallet constraints and upstream dependency updates cite this freeze as their source.

## External Source Repository Package

- **Fields**: package name, repository, tag, sha256, consuming local components, compatibility status.
- **Examples**: cardano-ledger-read, cardano-balance-tx, cardano-coin-selection.
- **Validation**: Pins are updated before local consumers close, and their consuming components validate against the final pins.

## Upstream Dependency Work Item

- **Fields**: upstream repo path, starting wallet pin, target node freeze, required code changes, local validation commands, resulting commit/tag, resulting sha256.
- **Examples**: `/code/cardano-ledger-read` from `cc9ca75f4b14967c714af502b50f7eee44d7a53c`; `/code/cardano-balance-transaction` from `5d69cc9bd47062b363929c877b83f0ab96369583`.
- **Validation**: Each upstream dependency is validated in its own repo before the wallet pin update is considered complete.

## Component Slice

- **Fields**: component name, Cabal file, ownership root, topology index, predecessor components, validation commands, closed status.
- **Ownership rule**: A slice owns its `.cabal` file and files under its component directory unless another component's Cabal file explicitly owns the path.
- **State transitions**: `pending` -> `in_progress` -> `validated` -> `closed`; `closed` -> `reopened` only with documented topology correction.

## Validation Evidence

- **Fields**: component, command, result, date, notes, blocker link if failed.
- **Required per component**: formatting, linting, relevant Nix build/test gate, and any component-specific unit/test executable.
- **Required final**: full clean build, local-cluster/integration, e2e, and review traceability.

## Closed Component Lock

- **Fields**: component, closing patch, closing validation evidence, later patches allowed to edit it.
- **Rule**: Later patches allowed to edit it must be empty unless an emergency correction is documented before the edit.
