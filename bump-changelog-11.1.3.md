# Ecosystem changelog — `cardano-node` 11.1.1 → 11.1.3

Companion to `bump-changelog-11.1.1.md` and `bump-changelog.md`. `master` pins
11.1.1; 11.1.2 was never merged, so this covers both steps.

## What the releases are for

11.1.2, from the upstream release notes:

> Cardano-node `11.1.2` optimizes memory usage in time lock scripts.

11.1.3, from the upstream release notes:

> Cardano-node `11.1.3` issues a fix Ledger's IPv4 decoding: releases before
> `11.1.x` decoded IPv4s using network order, which `11.1.2` inadvertently
> changed; this has been reverted to using network order for decoding IPv4s.

Breaking change listed for 11.1.3: `plc optimise` no longer accepts `--certify`
and `--certifier-*` (`plutus-ledger-api`).

## Pin set delta

From `cabal freeze` of each node tag in a fresh clone (537 packages each):

| package | 11.1.1 | 11.1.3 | source revision |
|---|---|---|---|
| `cardano-ledger-allegra` | 1.10.0.0 | 1.10.1.0 | `cardano-ledger@0372ead6` |
| `cardano-ledger-binary` | 1.9.0.0 | 1.9.1.0 | `cardano-ledger@048b78d2` |
| `cardano-slotting` | 0.2.1.0 | 0.2.2.0 | `cardano-base@b6559d4a` |
| `plutus-core`, `plutus-ledger-api`, `plutus-tx`, `plutus-tx-plugin`, `plutus-metatheory` | 1.65.0.0 | 1.70.0.0 | `plutus` tags |
| `deriving-aeson` | 0.2.10.0.0.0.0.1 | — | dropped by `plutus-core` 1.67 |

Unchanged: `cardano-api` 11.6.0.0, `ouroboros-consensus` 4.2.1.0,
`ouroboros-network` 1.2.0.0, every other `cardano-ledger-*` package,
`cardano-crypto-*`, `cardano-base` 0.1.6.0.

CHaP index-state 2026-09-03T10:20:53Z → 2026-09-23T18:57:23Z, flake rev
`95889113` → `6eb65fb4`. Hackage index-state unchanged at 2026-08-14T13:38:07Z.

The two `cardano-ledger` source revisions are on no branch of the upstream
repository; they are reachable only through GitHub's commit API and CHaP.

## Chronological

```
2026-07-09 plutus-core 1.66.0.0
2026-08-06 plutus 1.67.0.0
2026-08-21 plutus 1.68.0.0
2026-08-27 cardano-ledger 9f5433f1 shelley: force the initial-funds injection result
2026-09-08 cardano-base   a793148  Add SlotInterval type
2026-09-11 plutus 1.69.0.0
2026-09-16 cardano-ledger 6dc6a4e9 Remove unnecessary memory overhead in Timelocks
2026-09-16 cardano-ledger 9a8533f3 Simplify Eq and Ord instances for Timelock
2026-09-16 cardano-ledger 0372ead6 Bump up the version              (allegra 1.10.1.0)
2026-09-16 cardano-node   fef83fed ledger: allegra bump             (11.1.2)
2026-09-22 cardano-ledger 048b78d2 Fix IPv4 encoding order to use network order instead of swapped
2026-09-22 plutus 1.70.0.0
2026-09-23 cardano-node   e45a881b Bump version of cardano-ledger-binary to 1.9.0.1
2026-09-23 cardano-node   a1ae0ce7 Bump plutus for 11.1.x
2026-09-24 cardano-node   938cba99 Bump to 11.1.3
```

## `cardano-node` 11.1.1 → 11.1.3

```
938cba990 Bump to 11.1.3
2b0368ab0 Merge pull request #6705 from IntersectMBO/zliu41/bump-plutus
f56161b08 update
a1ae0ce7b Bump plutus for 11.1.x
06bf7c694 Merge pull request #6704 from IntersectMBO/koslambrou/prepare-11.1.x-fixup
e45a881b7 Bump version of cardano-ledger-binary to 1.9.0.1
fef83fed0 ledger: allegra bump
```

The only source change is the ledger protocol version advertised by the node:
`ProtVer 11 0` → `11 1` (11.1.2) → `11 2` (11.1.3). `pvMajor` is unchanged, so
`maxMajorProtVer` and the obsolete-node envelope check are untouched. Everything
else is `cabal.project`, `.cabal` bounds, `flake.lock` (CHaP, `iohk-nix`) and
configuration (mainnet peer snapshot, dijkstra testnet template).

## `cardano-ledger-binary` 1.9.0.0 → 1.9.1.0

```
048b78d2 Fix IPv4 encoding order to use network order instead of swapped
```

```diff
-  toIPv4w <$> binaryGetDecoder "decodeIPv4" getWord32le
+  toIPv4w <$> binaryGetDecoder "decodeIPv4" getWord32be
-ipv4ToBytes = BSL.toStrict . runPut . putWord32le . fromIPv4w
+ipv4ToBytes = BSL.toStrict . runPut . putWord32be . fromIPv4w
```

1.9.0.0, which `master` pins, encodes and decodes IPv4 byte-swapped. Upstream
regenerated every golden file containing a pool relay (`tx.cbor` in all eras,
`queryPoolParameters`, `queryPoolState`, `queryStakePoolRelays`). Any wallet
golden carrying an IPv4 relay is expected to change with this bump.

## `cardano-ledger-allegra` 1.10.0.0 → 1.10.1.0

```
6dc6a4e9 Remove unnecessary memory overhead in Timelocks
9a8533f3 Simplify Eq and Ord instances for Timelock
0372ead6 Bump up the version
```

`Timelock` lost its derived `Eq` in favour of comparing `mbHash` alone, and
gained an `Ord` doing the same. In the same release, nested sub-scripts are
decoded by `decodeNoBytesTimelock` as `MemoBytes t mempty def`. After a CBOR
round trip, distinct children of `RequireAllOf`/`RequireAnyOf`/`RequireMOf`
compare equal, hash to the all-zero default and re-encode to zero bytes;
top-level scripts are unaffected. 1.10.2.0 (CHaP, 2026-10-01, for node 11.2)
carries the same code. See #5444 for the reproducer against 1.10.0.0.

## `cardano-slotting` 0.2.1.0 → 0.2.2.0

```
a793148 Add SlotInterval type
```

Additive: `SlotInterval`, `addSlotInterval`.

## `plutus` 1.65.0.0 → 1.70.0.0

From the package changelogs:

- 1.66: `RecInline` PIR pass; textual UPLC `value` literals must be strictly
  ordered and non-zero; `FloatDelay` soundness fix.
- 1.67: `multiIndexArray` builtin (CIP-0156, `futurePV` only); `policies`
  builtin (CIP-0168, PV12); `CollapseCase` PIR pass; `deriving-aeson` dropped,
  `LowerInitialCharacter` no longer exported; `Value` API re-exported from
  `PlutusLedgerApi.V2`/`V3`/`Data.*`.
- 1.68: Plutus V4 and `dijkstraPV`; `assetCount` builtin (PV12);
  `multiIndexArray` costed and capped at 1024 indices.
- 1.69: V4 product types encoded as `List`; `with-crypto` cabal flag; cost
  models for `policies` and `assetCount` (new cost-model parameters);
  `keepPolicies`/`dropPolicies` builtins (PV12); `Data` casing.
- 1.70: `plc optimise` drops `--certify`; V4 script-context helpers.

New builtins and cost-model parameters are gated at PV12; mainnet is at PV11.
