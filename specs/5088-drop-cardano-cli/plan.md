# Plan: Stop building the cardano-cli library for local-cluster

One ordered, bisect-safe implementation slice changes exactly two files.

## Slice A — replace the CLI key reader and drop the dependency

Owned files:

- `lib/local-cluster/lib/Cardano/Wallet/Launch/Cluster/ConfiguredPool.hs`
- `lib/local-cluster/local-cluster.cabal`

RED first: remove `cardano-cli` from the library dependencies without changing
the import, then run `nix build --quiet --no-link .#local-cluster`. The build
must fail because `Cardano.CLI.Type.Key` is no longer available. Freeze that
diff and evidence for navigator review.

GREEN: use `Cardano.Api.readFileTextEnvelope` with the existing typed `File`
path to read each verification key. Preserve the existing failure behavior by
turning the returned `FileError` into the same fatal cluster setup failure.
Remove only imports and constraints made redundant by this replacement.

Proof:

1. The focused Nix build passes.
2. The local-cluster closure no longer contains a `cardano-cli` library
   derivation.
3. The full formatting and HLint checks pass.
4. The 39-example local-cluster suite, including live local-node cases, passes.

Commit subject:
`build(local-cluster): drop cardano-cli library dependency`

Commit trailer:
`Tasks: T508801, T508802, T508803, T508804`
