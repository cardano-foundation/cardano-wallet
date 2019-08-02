{
  extras = hackage:
    {
      packages = {
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "generic-monoid" = (((hackage.generic-monoid)."0.1.0.0").revisions).default;
        } // {
        cardano-wallet = ./cardano-wallet.nix;
        bech32 = ./bech32.nix;
        cardano-wallet-core = ./cardano-wallet-core.nix;
        cardano-wallet-core-integration = ./cardano-wallet-core-integration.nix;
        cardano-wallet-cli = ./cardano-wallet-cli.nix;
        cardano-wallet-launcher = ./cardano-wallet-launcher.nix;
        text-class = ./text-class.nix;
        cardano-wallet-test-utils = ./cardano-wallet-test-utils.nix;
        cardano-wallet-http-bridge = ./cardano-wallet-http-bridge.nix;
        cardano-wallet-jormungandr = ./cardano-wallet-jormungandr.nix;
        cardano-wallet-shelley = ./cardano-wallet-shelley.nix;
        cardano-crypto = ./cardano-crypto.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-shell = ./cardano-shell.nix;
        cardano-sl-x509 = ./cardano-sl-x509.nix;
        io-sim-classes = ./io-sim-classes.nix;
        network-mux = ./network-mux.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        typed-protocols = ./typed-protocols.nix;
        typed-protocols-cbor = ./typed-protocols-cbor.nix;
        bimap = ./bimap.nix;
        canonical-json = ./canonical-json.nix;
        cborg = ./cborg.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.24";
  compiler = "ghc-8.6.5";
  }