# Tasks: Reconnect when the Windows named pipe read is aborted

**Input**: [spec.md](spec.md), [plan.md](plan.md)
**Tests**: required by the specification (SC-001).

Format: `[ID] [P?] [Story] Description`.

## Slice 1: classification, logging and tests

- [x] **T001** [US1] Add `isNamedPipeError` (location and `InvalidArgument` type) and the `ConsultPolicy` guard to `handleIOException` in `lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs`.
- [x] **T002** [US2] Return Warning for the retried error in the `ConnectionStatusLog` severity.
- [x] **T003** [US1] Export `RetryHandlers`, `recoveringNodeConnection`, `retryOnConnectionLost` and `handleIOException`, with Haddock.
- [x] **T004** [US1] Add `Cardano.Wallet.Network.ImplementationSpec` with the classification cases of FR-001 and FR-002 and the retry of an action that throws the error once, and list it in `cardano-wallet-network-layer.cabal`.
- [x] **T005** Run fourmolu and HLint (clean) and `cabal test cardano-wallet-network-layer:unit` (13 examples, 0 failures, also with the `release` flag on).
- [x] **T006** Commit as `fix(network-layer): retry aborted Windows named pipe I/O`. The spec folder lands as its own `docs(5471): ...` commit.
