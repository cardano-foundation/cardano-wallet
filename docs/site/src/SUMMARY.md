- [Introduction](index.md)
- [User Manual](user.md)
    - [When to use](user/when.md)
    - [How to](user/common-use-cases.md)
        - [Start a server](user/common-use-cases/start-wallet-server.md)
        - [Create a wallet](user/common-use-cases/create-a-wallet.md)
        - [Manage wallets](user/common-use-cases/how-to-manage-wallets.md)
        - [Create addresses](user/common-use-cases/how-to-create-addresses.md)
        - [Create a transaction](user/common-use-cases/how-to-make-a-transaction.md)
        - [Handle assets](user/common-use-cases/assets.md)
        - [Handle delegation](user/common-use-cases/delegation.md)
        - [Handle metadata](user/common-use-cases/handle-metadata.md)
        - [Create shared-wallets](user/common-use-cases/shared-wallets.md)
    - [Installation](user/installation.md)
        - [Use docker](user/installation/use-docker.md)
        - [Use NixOS](user/installation/use-nixos.md)
    - [CLI](user/cli.md)
    - [HTTP-API](user/http-api.md)
    - [Hardware Recommendations](user/hardware-recommendations.md)
    - [Security](user/security.md)
    - [Known integrations](user/integrations.md)
    - [EKG and prometheus](user/ekg-and-prometheus.md)
    - [Plutus application backend](user/common-use-cases/plutus-application-backend.md)
    - [FAQ](user/faq.md)
- [Design](design.md)
    - [Architecture](design/architecture.md)
    - [Adrestia Architecture](design/adrestia-architecture.md)
    - [Links](design/links.md)
    - [Concepts](design/concepts.md)
        - [Eras](design/concepts/eras.md)
        - [Recovery Phrases](design/concepts/recovery-phrases.md)
        - [Master Key Generation](design/concepts/master-key-generation.md)
        - [Notes about BIP 44](design/concepts/Notes-about-BIP-44.md)
        - [Address Derivation](design/concepts/address-derivation.md)
        - [Byron Address Format](design/concepts/byron-address-format.md)
        - [Coin Selection](design/concepts/coin-selection.md)
        - [Hierarchical Deterministic Wallets](design/concepts/hierarchical-deterministic-wallets.md)
        - [Transaction Lifecycle](design/concepts/transaction-lifecycle.md)
        - [UTxO](design/concepts/utxo.md)
    - [Specifications](design/specs.md)
      - [Wallet ID](design/specs/wallet-id.md)
    - [Prototypes](design/prototypes.md)
      - [Light Mode](design/prototypes/light-mode.md)
- [Contributor Manual](contributor.md)
    - [What – Code and Languages](contributor/what.md)
      - [Building](contributor/what/building.md)
      - [Coding Standards](contributor/what/coding-standards.md)
      - [Logging Guidelines](contributor/what/logging-guidelines.md)
      - [Swagger Development](contributor/what/swagger-development.md)
      - [Specifying exceptions with Servant and Swagger](contributor/what/specifying-exceptions-with-servant-and-swagger.md)
      - [Nix build language](contributor/what/nix.md)
      - [Nix flake](contributor/what/nix-flake.md)
    - [How – Processes](contributor/how.md)
      - [Testing](contributor/how/testing.md)
      - [Continuous Integration](contributor/how/continuous-integration.md)
      - [Release Process](contributor/how/release-process.md)
        - [Release checklist](contributor/how/release-checklist.md)
      - [Code Review Guidelines](contributor/how/code-review-guidelines.md)
    - [Notes](contributor/notes.md)
      - [Updating Dependencies](contributor/notes/updating-dependencies.md)
      - [Notes from upgrading GHC version](contributor/notes/notes-from-upgrading-ghc-version.md)