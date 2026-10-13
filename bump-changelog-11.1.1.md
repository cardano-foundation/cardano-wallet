# Ecosystem changelog — `cardano-node` 11.1.0 → 11.1.1

Companion to `bump-changelog.md` (11.0.1 → 11.1.0).

## Pin set delta

Taken from the `cardano-node` 11.1.1 `cabal freeze` (537 packages). 36 packages are
pinned in `cabal.project`; 32 are unchanged.

| package | 11.1.0 | 11.1.1 |
|---|---|---|
| `cardano-api` | 11.5.0.0 | 11.6.0.0 |
| `cardano-cli` | 11.2.1.0 | 11.2.3.0 |
| `cardano-ledger-shelley` | 1.19.0.0 | 1.19.0.1 |
| `ouroboros-consensus` | 4.1.0.0 | 4.2.1.0 |

`cardano-base` and `ouroboros-network` packages are unchanged across the two pin sets.

CHaP index-state: `2026-08-17T11:39:16Z` → `2026-09-03T10:20:53Z`.
hackage index-state unchanged at `2026-08-14T13:38:07Z`.

## Grouped by repo

### `cardano-node`

`11.1.0` → `11.1.1`

40 commits.

```
c2ebdc87d Merge pull request #6674 from IntersectMBO/f-f/prepare-11.1.1
b15554ba1 Prepare 11.1.1 release
8119a6752 Merge pull request #6675 from IntersectMBO/jutaro/fix6667
c310c0d76 nixosTests/cardanoNodeDbtools: 45s global timeout
d29499df3 Moved namespaceInventoryDiff to test file
8f75a3883 Fix BlockFetch.Decision and BlockFetch.Client.ClientMetrics trace namespace drift (#6667)
a38eac60b Merge pull request #6663 from IntersectMBO/jl/sre-11.1.1
5b748b4d7 nixos: do not evaluate expression twice
25b6059a9 nixos: do not mask eval errors
1967e8218 bump: iohkNix for mainnet bootstrap peers and match ci cfg
68342d87e Merge pull request #6662 from IntersectMBO/mgalazyn/feature/update-cardano-api-11.6
6c027a6df Bump cardano-api to 11.6, cardano-rpc to 11.2, cardano-cli to 11.2.
39d586694 Merge pull request #6661 from IntersectMBO/f-f/thread-protocol-info
8365c5b41 Merge pull request #6653 from IntersectMBO/f-f/fix-cardano-tracer-bound
1ad9374e7 Add cardano-timeseries-io bound to cardano-tracer
e7c244efd Call protocolInfo only once at startup to avoid rebuilding the ledger state
8374ca36c Merge pull request #6655 from IntersectMBO/mgalazyn/test/rpc-readgenesis-followtip-fetchblock
c493c1503 cardano-testnet: disable RPC ReadGenesis initialFunds assertion until cardano-rpc resolves sgExtraConfig
b2c7e1bdd cardano-testnet: add changelog fragment for RPC tests
c8f9ca43d cardano-testnet: use shared RPC predicate/TxIn helpers in SearchUtxos and Transaction tests
c63838a49 cardano-testnet: add UTxO RPC ReadGenesis test
0df175f86 cardano-testnet: add UTxO RPC FollowTip test
8c5f27aae cardano-testnet: add UTxO RPC FetchBlock test
cf1e78d9e Merge pull request #6440 from IntersectMBO/fmaste/plutusv3
e87286425 wb | silence ChainDB.PerasCertDbEvent
b9949a9e8 wb | reproducible SnapshotInterval behavior (nomadcloud runs fix)
ba9184650 cardano-profile: regenerate files
d5c59934e locli: Plutus workload calibration CSV as part of report
2a04d0be2 bench: add ExpModInteger benchmark script and cardano-profile expmod support
bdb1c4a5a cardano-profile: new V3 and PV11 profiles
acd0562d8 wb: remove obselete files from fetch-analysis
41d6bb4d9 cardano-profile: new voltv11 and plutusv3 profiles
9197f4f1b tx-generator: v11-preview for calibrate-script
f96decb6e wb | sync van Rossem cost models from mainnet (PV11 preview)
6bc0685c8 tx-generator: integrate new scripts
60fe6b537 bench: plutus-scripts-bench: decouple mkPlutusBenchScript from PlutusTx accepting serialised UPLC
cf7a067d6 bench: plutus-scripts-bench: add CustomCallV3
cd8d0a3d9 bench: plutus-scripts-bench: add EcdsaSecp256k1LoopV3 SchnorrSecp256k1LoopV3
0ec640489 bench: plutus-scripts-bench: rigorous imports and formatting
dabd5b825 bench: plutus-scripts-bench: Unused Haskell extensions removal
```

### `cardano-api`

`cardano-api-11.5.0.0` → `cardano-api-11.6.0.0`

37 commits.

```
8d395158e Release cardano-api-11.6.0.0
1f00422a9 Merge pull request #1305 from IntersectMBO/mgalazyn/fix/rpc-initial-funds-extraconfig
9c71b655b cardano-rpc: resolve genesis initial funds from sgExtraConfig. Add timed cache of resolved Shelley Genesis.
d65d8a077 Merge pull request #1296 from IntersectMBO/mgalazyn/chore/bump-herald
227848ea8 Update herald tool to 0.2.0
019c9a1b9 Merge pull request #1317 from IntersectMBO/expose-dijkstra-era-enumerations
159f7c766 Expose the Dijkstra era in AnyCardanoEra, AnyShelleyBasedEra and SomeEra
94202d164 Merge pull request #1313 from IntersectMBO/serialise-dijkstra-transactions
81db72aba Merge pull request #1312 from IntersectMBO/enable-dijkstra-tx-construction
f370f9a2c Merge pull request #1310 from IntersectMBO/dispatch-dijkstra-queries
8a8bd1b35 Merge pull request #1309 from IntersectMBO/support-dijkstra-pparams
cf197e7bd Serialise and witness Dijkstra-era transactions
1d08fada1 Enable Dijkstra-era transaction construction and balancing
3a7e24442 Fix the era wording in the Conway-onwards query error messages
befbc5585 Dispatch ledger queries in the Dijkstra era
810540ee0 Support Dijkstra protocol parameters
602302dbc Merge pull request #1298 from IntersectMBO/complete-dijkstra-eons
71fce3a3c Type-gate the deprecated reqSignerHashesTxBodyL
9caeda9a0 Make the Eon Era instance exhaustive and sort the ledger re-exports
3fd6e0fc5 Merge pull request #1290 from IntersectMBO/wasm-demo-staking
a0e5686c0 Merge pull request #1304 from IntersectMBO/wasm-ci-gitlab-fallback
47d7d148d Fix the build on GHC 9.6 and 9.10
973b2ec6f Merge pull request #1300 from IntersectMBO/improve-readme
d6b6e520b Deprecate the LedgerTxBody wrapper lenses that have ledger equivalents
de8aafe6c Address review feedback
f4a421f53 Rewrite README.md with onboarding sections
51b091c11 haskell-wasm CI: add archive.org fallback for the ghc-wasm-meta input
d8ba5e933 Complete the era eons for Dijkstra
857a8b5dc Merge pull request #1297 from IntersectMBO/skip-drep-query-when-not-needed
4b2b6a71e Skip the DRep state query when balancing needs no DRep deposits
59f25da3a Merge pull request #1294 from IntersectMBO/release/cardano-rpc-11.1.0.0
60bba45e2 Add release changelog fragment for cardano-rpc 11.1.0.0
97204cd82 Release cardano-rpc-11.1.0.0
62fd1ae3e Merge pull request #1293 from IntersectMBO/release/cardano-api-11.5.0.0
e7f3d59a5 Add release changelog fragment for cardano-api 11.5.0.0
2a269509f cardano-wasm demo: address review feedback
f6744837c cardano-wasm demo: staking (certificates, pool picker, stake witnesses)
```

### `ouroboros-consensus`

`c85b71d7e98c32fa95b4011a892ab910ffb4bb93` → `82ecba329d7d054340bf707d44fe6e9ac27cec40`

16 commits.

```
82ecba329 Serve GetGenesisConfig without sgExtraConfig and release consensus 4.2.1.0 (#2257)
500772db1 Release ouroboros-consensus 4.2.1.0
6f011ada6 Decode ShelleyGenesis from 15 fields only
24abaf6d8 Add a changelog entry for the GetGenesisConfig fix
d76873b92 Ignore the artefacts written by the CDDL tests
ccc5938db Serve GetGenesisConfig without sgExtraConfig
13381200d Add a ShelleyGenesis codec without sgExtraConfig
27396eb4f Disable IOWait accounting on the LSM backend and release patch consensus (#2247)
76b91ca9d Release ouroboros-consensus 4.2.0.1
4496b8f3d Disable IOWait accounting on the LSM backend
418baa12d Release ouroboros-consensus 4.2.0.0 (#2242)
6016e1546 Release ouroboros-consensus 4.2.0.0
91e652115 Decouple LedgerDB garbage collection from snapshotting (#2227)
d85a76fc6 Serve LedgerDB snapshot requests in a ChainDB background thread
4ff69698d Ensure a single snapshot is queued
0d12014ef Decouple LedgerDB garbage collection from snapshotting
```

### `cardano-ledger`

`f649f9751074d2ab3de033fc3912f29c9862c1f5` → `4c81e909df7555b6a58a000db8f8f4f89c7acdc4`

2 commits.

```
4c81e909d Merge pull request #6028 from IntersectMBO/f-f/cardano-ledger-shelley-1.19.0.1-2
9f5433f1f shelley: force the initial-funds injection result
```

### `cardano-cli`

`7cb4f2b2` (11.2.1.0) → `eac27b8b` (11.2.3.0)

37 commits.

```
118f0da Check the CLI network id against the network id of the node
316ad1b `query stake-address-info`: check the address against the network id
d20bf2c Add changelog fragment
af04b29 Address review comments: flatten queryNodeNetworkId, reformat changelog
d1e3ee9 Run the network id check once before each command
3d2986a Remove x86_64-darwin from release-upload workflow
ec59cba Add release changelog fragment for cardano-cli 11.2.1.0
6c8fc05 Merge pull request #1419 from IntersectMBO/release/cardano-cli-11.2.1.0
0f8b821 Merge pull request #1411 from IntersectMBO/jm/release-upload-drop-x86_64-darwin
9ad21a6 Increase test watchdog on windows, ignore warp exceptions in tests
bba9de9 Merge pull request #1421 from IntersectMBO/mgalazyn/test/improve-windows-test-stability
dd0fc0f Merge pull request #1405 from IntersectMBO/1402-guard-against-networkid-mismatches
83f48da Update cardano-api-11.6
3ce617e Merge pull request #1424 from IntersectMBO/mgalazyn/feature/bump-cardano-api
5deecc8 Release cardano-cli-11.2.2.0
88b35f8 Add release changelog fragment for cardano-cli 11.2.2.0
eefdd5c Merge pull request #1425 from IntersectMBO/release/cardano-cli-11.2.2.0
6605fa1 Derive the accepted era list when reading transaction witnesses
1bae352 Support Dijkstra simple scripts read from JSON
cb3cc3f Support Dijkstra protocol-parameter updates
f8281e5 Deduplicate the era case in readFileSimpleScript
9f54f16 Describe the accepted range when rejecting a cost multiplier
94a0866 Render Dijkstra certificates in the friendly JSON view
a1c0428 Remove a trailing space from the anchor key in the Dijkstra view
ae17805 Merge pull request #1426 from IntersectMBO/derive-accepted-witness-eras
43ea07d Merge pull request #1427 from IntersectMBO/support-dijkstra-simple-scripts
386c470 Merge pull request #1429 from IntersectMBO/support-dijkstra-pparam-updates
b38438f Merge pull request #1430 from IntersectMBO/render-dijkstra-friendly-certs
045fe41 Enable the full Dijkstra command set
d302ca6 Merge pull request #1431 from IntersectMBO/enable-dijkstra-command-set
42db86c Add an end-to-end test for Dijkstra transaction witnessing
4e99a78 Compare the witness and the assembled transaction against golden files
62e6b19 Pin a transaction that was accepted by a Dijkstra testnet
af6862f Merge pull request #1432 from IntersectMBO/add-dijkstra-witnessing-test
b02965c Update CHaP
42a9a75 Merge pull request #1436 from IntersectMBO/update-chap-in-flake
eac27b8 Release cardano-cli-11.2.3.0
```

