{
  extras = hackage:
    {
      packages = {
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "OddWord" = (((hackage.OddWord)."1.0.2.0").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "clock" = (((hackage.clock)."0.8").revisions).default;
        "zip" = (((hackage.zip)."1.3.0").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.2").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "katip" = (((hackage.katip)."0.8.3.0").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "generic-monoid" = (((hackage.generic-monoid)."0.1.0.0").revisions).default;
        "bimap" = (((hackage.bimap)."0.4.0").revisions)."c59d587b56b575c299ba0c2fff44e630991a120a167de5a19cd7a81320f63c84";
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions)."9021f435ccb884a3b4c55bcc6b50eb19d5fc3cc3f29d5fcbdef016f5bbae23a2";
        "cborg" = (((hackage.cborg)."0.2.2.0").revisions)."eaee50d09d766af95ba18348e4fc230243033b98633ed46ccb5ae85efef7dc6c";
        "statistics-linreg" = (((hackage.statistics-linreg)."0.3").revisions)."95c6efe6c7f6b26bc6e9ada90ab2d18216371cf59a6ef2b517b4a6fd35d9a76f";
        bech32 = ./bech32.nix;
        cardano-wallet-core = ./cardano-wallet-core.nix;
        cardano-wallet-core-integration = ./cardano-wallet-core-integration.nix;
        cardano-wallet-cli = ./cardano-wallet-cli.nix;
        cardano-wallet-launcher = ./cardano-wallet-launcher.nix;
        text-class = ./text-class.nix;
        cardano-wallet-test-utils = ./cardano-wallet-test-utils.nix;
        cardano-wallet-jormungandr = ./cardano-wallet-jormungandr.nix;
        cardano-wallet-byron = ./cardano-wallet-byron.nix;
        persistent = ./persistent.nix;
        persistent-sqlite = ./persistent-sqlite.nix;
        persistent-template = ./persistent-template.nix;
        cardano-crypto = ./cardano-crypto.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        lobemo-backend-aggregation = ./lobemo-backend-aggregation.nix;
        lobemo-backend-monitoring = ./lobemo-backend-monitoring.nix;
        ekg-prometheus-adapter = ./ekg-prometheus-adapter.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-slotting = ./cardano-slotting.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        io-sim-classes = ./io-sim-classes.nix;
        network-mux = ./network-mux.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        typed-protocols = ./typed-protocols.nix;
        typed-protocols-cbor = ./typed-protocols-cbor.nix;
        };
      };
  resolver = "lts-13.24";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "zip" = { flags = { "disable-bzip2" = lib.mkOverride 900 true; }; };
          };
        })
    {
      packages = {
        "$locals" = { package = { ghcOptions = "-ddump-to-file -ddump-hi"; }; };
        };
      }
    ];
  }