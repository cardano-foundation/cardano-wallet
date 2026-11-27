# Ecosystem changelog — `cardano-node` 11.1.3 → 11.2.1

Companion to `bump-changelog-11.1.3.md`. `master` pins 11.1.3; this covers the
step to 11.2.1 (tag `11.2.1`, commit `90bc1cd93`). It goes through 11.2.0 (tag
`11.2.0`, commit `dac0d7ab2`, 2026-10-27, published 2026-10-28), whose breaking
changes 11.2.1 keeps.

## What the releases are for

11.2.1 was published as a pre-release on 2026-11-10, about two weeks after
11.2.0. Its upstream changelog entry (dated 2026-11-06) reads, in full: "Update
bounds: cardano-api-11.12, cardano-cli-11.7, ouroboros-consensus-5.4". It is
not a patch of the node alone: the freeze moves `cardano-ledger-core` to 1.23,
`cardano-ledger-alonzo` to 1.18 and `cardano-ledger-dijkstra` to 0.5 (see
"Moved again in 11.2.1").

### 11.2.0

11.2.0 is a **pre-release**: "not yet recommended for use on mainnet". From the
upstream release notes:

> Its headline change is the move to the new **cardano-config** library for
> reading node configuration: the node now understands a versioned configuration
> "envelope" alongside the existing flat format, and the configuration files
> published for each network are now in the envelope format.

Breaking changes listed upstream that a wallet build or deployment meets:

- **Node-to-client:** `NodeToClientV_16` – `NodeToClientV_22` are removed, with
  the queries only they could reach (`GetProposedPParamsUpdates`,
  `GetStakeDistribution`, `GetPoolDistr`). "Clients (wallets, indexers, tools)
  must be built against a version of the network libraries that speaks
  `NodeToClientV_23`."
- **Configuration:** the shipped network configs are enveloped and cannot be
  read by nodes older than 11.2. Flat configurations still work. Merge
  documents written for the flat shape silently do nothing against an enveloped
  config (stated for the Docker `CARDANO_CONFIG_JSON_MERGE`; the same applies
  to any tool that edits keys in place).
- **Ledger snapshots:** taken by default every `40 * k` slots, no offset.
- **cardano-cli:** `node issue-pop-BLS` and `node key-hash-BLS` removed;
  `--script-valid`/`--script-invalid` removed from Dijkstra `transaction build*`.

## Pin set delta

From `cabal freeze` of each node tag in a fresh clone, run inside the tag's own
`nix develop` (537 packages at 11.1.3, 550 at 11.2.0 and at 11.2.1). Between
11.1.3 and 11.2.0, 108 packages differ: 23 added, 10 removed, 75 changed.
Between 11.2.0 and 11.2.1, 12 packages change version and none is added or
removed. **None moved backwards.** The table below shows the 11.2.0 values;
"Moved again in 11.2.1" gives the final ones for the packages that changed
once more.

### Cardano packages

| package | 11.1.3 | 11.2.0 | published | source revision |
|---|---|---|---|---|
| `ouroboros-network` | 1.2.0.0 | **2.0.0.0** | 2026-10-22 | `ouroboros-network@4646039f` |
| `network-mux` | 0.11.0.0 | **1.0.0.0** | 2026-10-22 | `ouroboros-network@4646039f` |
| `cardano-diffusion` | 1.1.1.0 | 1.3.0.1 | 2026-10-22 | `ouroboros-network@4646039f` |
| `typed-protocols` | 1.2.1.0 | 1.2.2.0 | | |
| `ouroboros-consensus` | 4.2.1.0 | **5.2.0.0** | 2026-10-26 | `ouroboros-consensus@952b7020` |
| `cardano-ledger-core` | 1.21.0.0 | 1.22.0.0 | 2026-10-01 | `cardano-ledger@54e24ba4` |
| `cardano-ledger-shelley` | 1.19.0.1 | 1.20.0.0 | | `cardano-ledger@54e24ba4` |
| `cardano-ledger-alonzo`, `-babbage` | 1.16.0.0, 1.14.0.0 | 1.17.0.0, 1.15.0.0 | | `cardano-ledger@54e24ba4` |
| `cardano-ledger-conway` | 1.23.0.0 | 1.24.0.0 | | `cardano-ledger@54e24ba4` |
| `cardano-ledger-api` | 1.14.0.0 | 1.15.0.0 | | `cardano-ledger@54e24ba4` |
| `cardano-ledger-dijkstra` | 0.3.0.0 | 0.4.0.0 | | `cardano-ledger@54e24ba4` |
| `cardano-ledger-allegra`, `-mary` | 1.10.1.0, 1.11.0.0 | 1.10.2.0, 1.11.1.0 | | `cardano-ledger@54e24ba4` |
| `cardano-ledger-binary` | 1.9.1.0 | 1.10.1.0 | 2026-10-15 | `cardano-ledger@36e72ca4` |
| `cardano-data`, `cardano-crypto-wrapper` | 1.3.1.0, 1.7.0.0 | 1.4.0.0, 1.8.0.0 | | `cardano-ledger@54e24ba4` |
| `cardano-protocol` | 0.1.0.0 | 0.2.0.0 | 2026-10-01 | `cardano-ledger@54e24ba4` |
| `cardano-protocol-tpraos` | 1.6.0.0 | 1.6.0.1 | | `cardano-ledger@54e24ba4` |
| `small-steps` | 1.1.4.0 | 1.2.0.0 | | `cardano-ledger@54e24ba4` |
| `cardano-crypto-class` | 2.5.1.0 | 2.6.0.0 | 2026-07-27 | `cardano-base@12168e4b` |
| `cardano-base` | 0.1.6.0 | 0.1.7.1 | | `cardano-base@5813d211` |
| `cardano-crypto` | 1.3.0 | 1.4.0 | 2026-07-27 | `cardano-crypto@ac2e12a4` |
| `cardano-api` | 11.6.0.0 | **11.11.0.0** | 2026-10-26 | `cardano-api@76fa5f2d` |
| `cardano-cli` | 11.2.3.0 | 11.6.0.0 | 2026-10-26 | `cardano-cli@90cc2b9a` |
| `plutus-core`, `plutus-ledger-api`, `plutus-tx` | 1.70.0.0 | 1.71.0.0 | 2026-09-29 | `plutus@261766ad` |
| `cardano-config` | — | 2.2.3.0 | 2026-10-26 | new |
| `cardano-keys` | — | 11.0.0.0 | 2026-09-15 | new |
| `kes-agent-crypto` | 1.2.0.0 | — | | dropped |

Unchanged: `cardano-binary`, `cardano-crypto-praos`, `cardano-prelude`,
`cardano-slotting`, `cardano-strict-containers`, `cardano-ledger-byron`.

### Moved again in 11.2.1

| package | 11.2.0 | 11.2.1 |
|---|---|---|
| `ouroboros-consensus` | 5.2.0.0 | **5.4.0.0** |
| `cardano-ledger-core` | 1.22.0.0 | **1.23.0.0** |
| `cardano-ledger-alonzo` | 1.17.0.0 | **1.18.0.0** |
| `cardano-ledger-dijkstra` | 0.4.0.0 | **0.5.0.0** |
| `cardano-ledger-babbage` | 1.15.0.0 | 1.15.0.1 |
| `cardano-ledger-conway` | 1.24.0.0 | 1.24.0.1 |
| `cardano-ledger-mary` | 1.11.1.0 | 1.11.1.1 |
| `cardano-ledger-shelley` | 1.20.0.0 | 1.20.0.1 |
| `cardano-protocol` | 0.2.0.0 | 0.2.0.1 |
| `cardano-api` | 11.11.0.0 | **11.12.0.0** |
| `cardano-cli` | 11.6.0.0 | 11.7.0.0 |
| `trace-dispatcher` | 2.13.2 | 2.13.3 |

`plutus-core`, `plutus-ledger-api` and `plutus-tx` stay at 1.71.0.0 in the
freeze. The unconstrained resolver picks 1.72.0.0 at the 11.2.1 CHaP index-state,
so `cabal.project` pins all three to 1.71.0.0.

### Other packages that moved

`tls` 2.1.14 → 2.4.6, `crypton-x509` 1.7.7 → 1.9.1 (with `-store`,
`-system`, `-validation` 1.9.x), `crypton` 1.0.6 → 1.1.5, `http2` 5.3.9 →
5.4.6, `warp` 3.4.9 → 3.4.16, `network` 3.2.8.0 → 3.2.9.0, `network-run`
0.4.4 → 0.6.0, `time-manager` 0.2.4 → 0.3.2, `QuickCheck` 2.15.0.1 → 2.18.0.0,
`grapesy` 1.1.1 → 1.2.1, `string-interpolate` 0.3.4.0 → 1.0.0.0,
`resource-registry` 0.3.0.0 → 0.4.0.0.

Added: `crypton-asn1-{types,encoding,parse}` 0.4.1/0.10.0 and `crypton-pem` 0.3.0
(replacing `asn1-*` and `pem`), `autodocodec` 0.6.0.0, `mlkem` 0.2.3.0,
`quickcheck-classes` 0.6.5.0, `time-hourglass` 0.3.0, `validity-*`.

Removed: `asn1-types`, `asn1-encoding`, `asn1-parse`, `pem`, `happy`,
`happy-lib`, `haskell-src-exts`, `haskell-src-meta`, `th-env`.

The `tls`/`crypton-x509` move matters to `cardano-wallet-application-tls`.
That package already depends on `crypton-asn1-encoding` and `crypton-asn1-types`.
`cabal.project` pins `tls >= 2.2` with a comment recording that the chain was
capped below 2.3 (`crypton-connection`, `tls-session-manager`) and resolved to
tls-2.2.2; the node's 11.2.0 freeze resolves tls-2.4.6, so that comment becomes
stale. `crypton-x509-system` 1.9.0 satisfies the existing `< 1.6.8 || >= 1.7`
constraint.

CHaP index-state 2026-09-23T18:57:23Z → 2026-10-27T09:47:50Z (11.2.0) →
2026-11-05T22:55:22Z (11.2.1), flake rev `6eb65fb4` → `f150513f` → `5bdd1e3a`.
Hackage index-state 2026-08-14T13:38:07Z → 2026-09-29T11:48:37Z (the node's own,
unchanged between 11.2.0 and 11.2.1).

Revision counts between the old and new source revisions: `cardano-ledger` 406,
`ouroboros-consensus` 361, `cardano-api` 192, `cardano-base` 104 (+16 for
`cardano-crypto-class`), `ouroboros-network` 66, `cardano-node` 151.

## Breaking changes by package

From each package's `CHANGELOG.md` over the revision range above. Only items a
wallet-shaped consumer can meet.

### `ouroboros-network` 2.0.0.0, `cardano-diffusion` 1.3.0.0, `network-mux` 1.0.0.0

- `cardano-diffusion` 1.3.0.0: removed `NodeToClientV_16` – `NodeToClientV_22`.
- Removed `LocalStateQuery_V1`, `LedgerPeerSnapshotV2`.
- `connectToNode`, `connectToNode'`, `connectToNodeWithMux`,
  `connectToNodeWithMux'`, `connectToNodeSocket` and `Server.Simple.with` take a
  negotiated data flow callback; `mkMiniProtocolInfo`/`toMiniProtocolInfos` take
  `Provenance` and `DataFlow` parameters.
- `TxSubmissionCounters` is parameterised over its duration type; instances
  need `FlexibleInstances`.
- `network-mux`: `TraceChannelSendStart` no longer carries the message length.

### `ouroboros-consensus` 5.2.0.0

- Depends on `ouroboros-network ^>= 2.0`, `network-mux ^>= 1.0`.
- Shelley and Cardano serialisation golden files regenerated: IPv4 relay
  addresses are now in network byte order (`cardano-ledger-binary` 1.10.1.0).

### `cardano-ledger-core` 1.22.0.0 and the eras

- `StakePoolParams` and `PoolCert` gain an `era` parameter; `sppBlsKey` added to
  `StakePoolParams`.
- `EraDecoder` removed, `EraCodec` added; `ppEraDecoder` removed from `PParam`,
  `ppuEraCodec` added to `PParamUpdate`.
- `BbodySignal` removed; `TPraosBbodySignal`, `PraosBbodySignal`,
  `LeiosBbodySignal` added. `EraBlockHeader` split into `TPraosEraBlockHeader`,
  `PraosEraBlockHeader`, `LeiosEraBlockHeader`; `protVerBlockHeaderL` moved.
- `allInputsTxBodyF` moved from `EraTxBody` to `EraTx`.
- `poolDistrDistrL`, `poolDistrTotalL`, `numSegComponents`, `consumed`
  (`EraUTxO`) removed; generic `EncCBOR`/`DecCBOR` for `Block` removed in
  favour of per-era instances.
- `Val`, `Compactible`, `EraScript`, `EraTxOut`, `EraStake` gain `Ord`
  superclass constraints; `EraTxOut` gains `FromJSON (TxOut era)`, `EraTx`
  gains `ToJSON`/`FromJSON (Tx TopTx era)`, `EraTxBody` the `TxBody` pair,
  `EraTxWits` gains `ToJSON`, `FromJSON`, `NFData`.
- `directDepositsMissingAccounts` and `withdrawalsMissingAccounts` take a
  `Network`.
- Stake snapshots split by phase (`MarkSnapShot`, `SetSnapShot`, `GoSnapShot`);
  **the on-disk ledger state format changes and requires replay.**
- `cardano-ledger-conway` 1.24.0.0: `reStakePoolDistrL` → `reVotingStakePoolDistrL`,
  `computeDRepDistr` takes `VotingStakePoolDistr`; `certsTx` removed from
  `CertsEnv`; `WithdrawalsNotInRewardsCERTS` removed from
  `ConwayCertsPredFailure`.
- `cardano-ledger-api` 1.15.0.0: `querySetSnapshotStakePoolDistr` returns
  `QueryResultPoolDistr`.
- `cardano-ledger-binary` 1.10.x: `decodeIPv4`/`decodeIPv6` move into
  `Cardano.Ledger.Binary.Decoding.DecCBOR`; indefinite-length `Text`, tuples and
  `CertifiedVRF` accepted from protocol version 12.
- `cardano-ledger-shelley` 1.20.0.0: `freshBootstapAddress` →
  `freshBootstrapAddress`.
- `cardano-protocol` 0.2.0.0: `EbAnnouncement` moved to `cardano-ledger-core`
  as `EbReferencesAnnouncement`.
- New in Dijkstra (`cardano-ledger-dijkstra` 0.4.0.0): BLS pool keys, CIP-0050
  pledge leverage, CIP-0023 minimum pool margin, Leios committee selection
  (CIP-0164), Plutus V4 script context as a list.

### 11.2.1: `cardano-ledger-core` 1.23.0.0, `-alonzo` 1.18.0.0, `-dijkstra` 0.5.0.0

- `mkAlonzoStAnnTx`, `mkPlutusWithContext`, `collectPlutusScriptsWithContext`,
  `scriptsWithContextFromLedgerTxInfo`, `scriptsWithContextFromLedgerTxInfoWithResult`,
  `evalTxExUnits`, `evalTxExUnitsWithLogs` (alonzo) and `mkDijkstraStAnnTopTx`
  (dijkstra) require `Inject (AlonzoContextError era) (ContextError era)`.
- `PlutusWithContext` carries `pwcLanguageCostModel` instead of `pwcCostModel`;
  `LanguageCostModel` with its encoder, decoder and evaluation-context helpers
  is added; `getEvaluationContext` and `pdoLanguage` are removed.
- `cardano-api` 11.12.0.0 requires `ouroboros-consensus` 5.4,
  `cardano-ledger-core` 1.23 and `cardano-ledger-alonzo` 1.18, and adds the same
  `Inject` constraint to `AlonzoEraOnwardsConstraints` and `EraCommonConstraints`.
  `IntroducedInDijkstraPParams` gains ten fields (minimum pool margin, maximum
  pledge leverage, six Peras parameters, two reference-input costs).
- `cardano-ledger-conway` 1.24.0.1, `-babbage` 1.15.0.1, `-shelley` 1.20.0.1,
  `-mary` 1.11.1.1, `cardano-protocol` 0.2.0.1: patch releases.

### `plutus` 1.71.0.0

Package version bump only for the wallet's purposes; no source change is
expected in wallet code.

## Where the wallet is affected

Found by building each package in dependency order on the 11.2.1 pin set.

### Ledger and consensus

| place | what | why |
|---|---|---|
| `cardano-wallet-primitive`, `Ledger/Shelley.hs` | `nodeToClientVersions = [NodeToClientV_16 .. NodeToClientV_22]` becomes `[NodeToClientV_23]` | the older constructors are removed; the wallet would otherwise offer only versions the new node refuses |
| `cardano-wallet-read`, `PParams/Mock.hs` | `OrdExUnits` imported from `Cardano.Ledger.Plutus` | moved out of `Cardano.Ledger.Alonzo.PParams` |
| `cardano-wallet-primitive`, `Ledger/Read/Tx/Features/Certificates.hs` | `StakePoolParams era` | the type gained an `era` parameter |
| `cardano-api-extra` `Cardano.Api.Gen`, `local-cluster` `ConfiguredPool.hs` | optional BLS key left unset | `StakePoolParameters` and `StakePoolParams` gained `sppBlsKey` |
| `cardano-wallet`, `Cardano/Wallet.hs` | `allInputsTxBodyF` imported from `EraTx` | moved from `EraTxBody` |
| `cardano-wallet-primitive`, `SlottingSpec`; `cardano-wallet-unit`, `ServerSpec` | `Bound` ends in `boundNextPerasRound`; `defaultEraParams` takes `HF.NoPerasEnabled` | `ouroboros-consensus` 5.2 |
| `local-cluster`, `test/data/cluster-configs/dijkstra-genesis.json` | replaced by the node's `testnet-template-dijkstra.json` | node 11.2 refuses a Dijkstra genesis without `minPoolMargin` and the other new keys; the old file had four keys, the template has twenty-four |

### `cardano-addresses` 4.0.8 and the crypto stack

`cardano-addresses` 4.0.8 vendors the extended-key code of `cardano-crypto` as
`Cardano.Address.Crypto.Wallet`, with its own `XPrv`, `XPub` and `XSignature`.
It requires `crypton` 1.1, which the 11.2.x pin set provides, and uses `ram`
instead of `memory`. `cardano-crypto` 1.4 uses the same two.

| place | what | why |
|---|---|---|
| every package that held keys (`address-derivation-discovery`, `cardano-wallet`, `cardano-wallet-api`, `cardano-wallet-secrets`, `cardano-wallet-primitive` `Unsafe`, tests) | `XPrv`, `XPub` and their functions come from `Cardano.Address.Crypto.Wallet` | the key classes in `Cardano.Wallet.Address.Derivation` already take their types from `Cardano.Address.Derivation`, which now re-exports the vendored ones |
| `cardano-wallet`, `Shelley/Transaction.hs` (`toLedgerXPrv`) | the Byron and Shelley witnesses convert the key to `cardano-crypto`'s through its 128-byte form | the ledger and `cardano-api` signing keys still take `cardano-crypto`'s type |
| `crypto-primitives` | `cryptonite` and `memory` become `crypton` and `ram`; the `support_rdrand` flag in `cabal.project` moves from `cryptonite` to `crypton` | `Cryptography.Core` re-exports `MonadRandom`, whose `ByteArray` class has to be the one `cardano-crypto` and `cardano-addresses` use. Same module names, no source change; the OpenSSL golden tests pass unchanged |
| all packages | `memory` becomes `ram` | `Digest`, `Auth` and `ScrubbedBytes` use the classes of `ram` |
| `cardano-wallet-secrets` | `Passphrase` also instances `ByteArrayAccess` of `memory`, through a cabal mixin that renames `memory`'s `Data.ByteArray` | `cardano-crypto-wallet` 0.2.0.0, the source of `Cardano.Crypto.WalletHD.Encrypted`, still depends on `memory`; the passphrase bytes are not copied |
| `cardano-wallet`, `Cardano/Wallet.hs` | the metadata hash is converted to bytes before signing with `cardano-crypto-wallet` | same reason, for a non-secret digest |
| `cardano-wallet`, `Checkpoints/Store.hs`; `cardano-wallet-api`, `Server/Error.hs`; `cardano-wallet-primitive`, `Ledger/Convert.hs` | cosigner indices and the `RequireSomeOf` count are `Word` in `cardano-addresses` 4.0.8 (were `Word8`); converted with a checked cast | the database column and the API schema stay one byte (0 to 255); the API already rejects larger indices |
| `cardano-wallet-unit` | `Malformed` messages and the `key child --help` golden follow the new text of `cardano-addresses` 4.0.8 | a bare string is read as a cosigner script; the help describes the single-quote hardened notation |

### Not affected

- `nix/overlays/cardano-deployments.nix` edits flat keys of
  `cardanoLib.environments`, which come from the wallet's own `iohk-nix` input.
  That input does not move in this change, so the configs stay flat. It needs a
  second look when `iohk-nix` ships enveloped configs.
- `local-cluster` writes a flat node configuration. Node 11.2.1 starts with it
  and logs `failed to parse the node configuration (ignored)` (the new
  `cardano-config` parser misses `ByronGenesisHash`, then the node falls back to
  its legacy parser). A cluster of one relay and four pools starts and answers
  tip queries in the `NetworkSpec` regression test.
- `tls` resolves to 2.4.6 and `application-tls` builds unchanged against
  `crypton-x509` 1.9.
