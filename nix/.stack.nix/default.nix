{
  extras = hackage:
    {
      packages = {
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "persistent-template" = (((hackage.persistent-template)."2.7.2").revisions).default;
        "persistent" = (((hackage.persistent)."2.10.1").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        bech32 = ./bech32.nix;
        cardano-wallet-core = ./cardano-wallet-core.nix;
        cardano-wallet-core-integration = ./cardano-wallet-core-integration.nix;
        cardano-wallet-cli = ./cardano-wallet-cli.nix;
        cardano-wallet-launcher = ./cardano-wallet-launcher.nix;
        text-class = ./text-class.nix;
        cardano-wallet-test-utils = ./cardano-wallet-test-utils.nix;
        cardano-wallet-http-bridge = ./cardano-wallet-http-bridge.nix;
        cardano-wallet-jormungandr = ./cardano-wallet-jormungandr.nix;
        persistent-sqlite = ./persistent-sqlite.nix;
        cardano-crypto = ./cardano-crypto.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.24";
  modules = [
    {
      packages = {
        "iohk-monitoring" = {
          flags = {
            "disable-examples" = true;
            "disable-ekg" = true;
            "disable-systemd" = true;
            "disable-prometheus" = true;
            "disable-gui" = true;
            "disable-graylog" = true;
            };
          };
        };
      }
    ];
  compiler = "ghc-8.6.5";
  }