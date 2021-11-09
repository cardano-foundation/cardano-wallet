Wallet
------

- [Wallet Cryptography and Encoding](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Cryptography-and-Encoding)
- [About Address Derivation](https://github.com/input-output-hk/cardano-wallet/wiki/About-Address-Derivation)
- [About Coin Selection](https://github.com/input-output-hk/cardano-wallet/wiki/About-Coin-Selection)
- [About UTxO](https://github.com/input-output-hk/cardano-wallet/wiki/About-UTxO)
- [Formal Specification for a Cardano Wallet](https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/wallet/formal-specification-for-a-cardano-wallet.pdf)
- [[Byron] About Address Format](https://github.com/input-output-hk/cardano-wallet/wiki/About-Address-Format---Byron)
- [HD Chimeric wallets](https://github.com/input-output-hk/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md)
- [About Transactions Lifecyle](https://github.com/input-output-hk/cardano-wallet/wiki/About-Transactions-Lifecycle)
- [Notes about BIP-44](https://github.com/input-output-hk/cardano-wallet/wiki/Notes-about-BIP-44)

Cardano Node
------

- [User Guide](https://docs.cardano.org/en/latest/)
- [Engineering Design Specification for Delegation and Incentives](https://hydra.iohk.io/build/902246/download/1/delegation_design_spec.pdf) 
- [A Formal Specification of the Cardano Ledger](https://hydra.iohk.io/build/1224753/download/1/ledger-spec.pdf)
- Networking Protocol(s)
  - Repository: [input-output-hk/ouroboros-network](https://github.com/input-output-hk/ouroboros-network), includes specifications
  - Specification: [The Shelley Networking Protocol](https://hydra.iohk.io/build/6955704/download/2/network-spec.pdf) (Version 1.3.0, 16th July 2021)
- Low-Level Specifications
  - [[Byron] On-Chain Binary Formats](https://github.com/input-output-hk/cardano-sl/blob/master/docs/on-the-wire/current-spec.cddl)
  - [[Shelley] Network Binary Formats - Draft](https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-network/test/messages.cddl)
  - [[Shelley] On chain Binary formats - Draft](https://github.com/input-output-hk/cardano-ledger-specs/blob/master/shelley/chain-and-ledger/cddl-spec/shelley.cddl#L32)

Plutus
------

- [Plutus Book](plutus-book.surge.sh)
- [Multi-assets Ledger Specs extension](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-mc/latest/download/1/multi-asset.pdf)


Jörmungandr
------

- [User Guide](https://input-output-hk.github.io/jormungandr/)
- [REST API](https://redocly.github.io/redoc/?url=https://raw.githubusercontent.com/input-output-hk/jormungandr/master/doc/openapi.yaml)
- [Protocol Buffer Specifications](https://github.com/input-output-hk/chain-libs/blob/master/network-grpc/proto/node.proto)
- [Low-Level Specifications](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc)
  - [On-Chain Binary Formats](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/format.abnf)
  - [Incentives](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/incentives.md)
  - [Address](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/address.md)
  - [Multisig](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/multisig.md)
  - [Update Semantics](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/update.md)

Emurgo
------

- [Seiza: explorer](https://www.seiza.com/)
- [Tangata Manu: Yoroi data importer](https://github.com/Emurgo/tangata-manu)
- [Yoroi](https://github.com/Emurgo/yoroi-frontend)
- [Emurgo Improvements Proposals](https://github.com/Emurgo/EmIPs/tree/master/specs)
  - [Account Number Plates (Checksum IDs)](https://github.com/Emurgo/EmIPs/blob/master/specs/emip-001.md)
  - [URI Scheme for ADA transfer](https://github.com/Emurgo/EmIPs/blob/master/specs/emip-002.md)
  - [Encrypting Data With a Passphrase](https://github.com/Emurgo/EmIPs/blob/master/specs/emip-003.md)

Bitcoin
------

- [BIP 0032 - Hierarchical Deterministic Wallets](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
- [BIP 0039 - Mnemonic Code for Generating Deterministic Keys](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
- [BIP 0044 - Multi-Account Hierarchy for Deterministic Wallets](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki)
- [BIP 0173 - Base32 Address Format](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)