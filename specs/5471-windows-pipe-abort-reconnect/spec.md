# Spec: Reconnect when the Windows named pipe read is aborted

**Feature Branch**: `fix/windows-pipe-abort-reconnect`
**Issue**: #5471
**Status**: Draft

## User Scenarios & Testing

### User Story 1 - The wallet survives an aborted pipe read (Priority: P1)

As a Windows user, I want the wallet to reconnect to cardano-node when a named pipe operation is aborted, so that the wallet keeps running instead of shutting down.

**Why this priority**: the failure terminates the whole wallet process.

**Independent Test**: classify an `IOError` located at `withIODataPtr (readHandle)` and run an action that throws it once under the node connection retry wrapper.

**Acceptance Scenarios**:

1. **Given** a connection to the node over a named pipe, **When** a read fails with an `InvalidArgument` `IOError` located at `withIODataPtr (readHandle)` or `withIODataPtr (writeHandle)`, **Then** the connection is retried with the existing backoff policy.
2. **Given** an action under the retry wrapper that throws that error once, wrapped as a mux `IOException`, **When** it is run, **Then** it is run again and completes.
3. **Given** an `InvalidArgument` `IOError` at any other location, or a permission error at a `withIODataPtr` location, **When** it is classified, **Then** it is not retried. Other error types at that location keep their existing classification.

### User Story 2 - The reconnect is visible (Priority: P2)

As an operator, I want a reconnect caused by this error to be logged, so that the cause of a reconnect is visible in the log.

**Independent Test**: inspect the log of a wallet that reconnects after such an error.

**Acceptance Scenarios**:

1. **Given** the retried error, **When** it is traced, **Then** it is logged at Warning, as a vanished resource is.

### Edge Cases

- The description of Windows error 995 is localized; the location string and the error type are used to recognize the error, not the description.

## Requirements

- **FR-001**: `handleIOException` MUST return `ConsultPolicy` for an `IOError` of type `InvalidArgument` whose location starts with `withIODataPtr`.
- **FR-002**: `handleIOException` MUST NOT change its result for any other `IOError`, including an `InvalidArgument` error at another location and another error type at a `withIODataPtr` location.
- **FR-003**: `MsgConnectionLost (ReasonException e)` MUST have severity Warning when `e` is such an error, using the same predicate as FR-001.
- **FR-004**: `RetryHandlers`, `recoveringNodeConnection`, `retryOnConnectionLost` and `handleIOException` MUST be exported, under a Testing heading, from `Cardano.Wallet.Network.Implementation`.

## Success Criteria

- **SC-001**: the network-layer unit suite contains specs for FR-001, FR-002 and the retry of an aborted action, and they pass.
- **SC-002**: on Windows, an aborted pipe read is logged as "Node connection lost" and the wallet keeps running.

## Assumptions

- The location `withIODataPtr (...)` is set by Win32-network for failed pipe reads and writes and by nothing else the wallet uses.
