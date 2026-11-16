# Plan: Reconnect when the Windows named pipe read is aborted

## Summary

Classify an `InvalidArgument` `IOError` located at `withIODataPtr` (raised by the Windows named pipe bearer) as a lost connection in `handleIOException`, log it at Warning, and cover the classification and the retry with a unit spec.

## Technical Context

- Language: Haskell, package `cardano-wallet-network-layer`.
- Test: `cabal test cardano-wallet-network-layer:unit` (hspec).
- Platform: the fix targets Windows; the tests run on Linux because they construct the `IOError` directly.

## Constitution Check

- Explicit export lists: four names are exported to the existing export list.
- Haddock on public functions: `handleIOException` and `recoveringNodeConnection` gain Haddock comments.
- Fourmolu and HLint: run through the dev shell.
- Conventional Commits, one concern per commit: a single `fix(network-layer)` commit.
- Tests run locally before pushing.

## Where the pieces live

| Thing | File | Lines (at `2fc5c383c`) |
|---|---|---|
| IOException classification | `lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs` | 1278-1295 |
| Mux error classification | same | 1297-1307 |
| Retry wrapper and handlers | same | 1247-1273 |
| Linked node clients | same | 585, 599 |
| Log severity | same | 1418-1432 |
| Unit suite | `lib/network-layer/test/` | new `Cardano/Wallet/Network/ImplementationSpec.hs` |

## Design

`handleIOException` gains a guard, placed before the final `DontRetry`, that returns `ConsultPolicy` when `ioeGetLocation e` starts with `withIODataPtr` and `ioeGetErrorType e` is `InvalidArgument`. The check is a named helper used by both the guard and the severity function, so the two cannot disagree. The mux `IOException` case already delegates to `handleIOException`, so `handleMuxError` needs no change.

Error type alone is not the discriminator: error 995 maps to `InvalidArgument`, which is also what unrelated failures produce.

## Project Structure

Documentation: `specs/5471-windows-pipe-abort-reconnect/{spec,plan,tasks}.md`, committed separately as `docs(5471): ...`.

Source: the module above and the new spec module, registered in `cardano-wallet-network-layer.cabal` (`hspec-discover` finds the spec).
