{
  extras = hackage:
    {
      packages = {
        "bech32" = (((hackage.bech32)."1.0.2").revisions).default;
        "bech32-th" = (((hackage.bech32-th)."1.0.2").revisions).default;
        "OddWord" = (((hackage.OddWord)."1.0.2.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "wai-extra" = (((hackage.wai-extra)."3.0.29.1").revisions).default;
        "servant" = (((hackage.servant)."0.17").revisions).default;
        "servant-server" = (((hackage.servant-server)."0.17").revisions).default;
        "servant-client-core" = (((hackage.servant-client-core)."0.17").revisions).default;
        "servant-client" = (((hackage.servant-client)."0.17").revisions).default;
        "servant-swagger" = (((hackage.servant-swagger)."1.1.8").revisions).default;
        "zip" = (((hackage.zip)."1.3.0").revisions).default;
        cardano-wallet-core = ./cardano-wallet-core.nix;
        cardano-wallet-core-integration = ./cardano-wallet-core-integration.nix;
        cardano-wallet-cli = ./cardano-wallet-cli.nix;
        cardano-wallet-launcher = ./cardano-wallet-launcher.nix;
        text-class = ./text-class.nix;
        cardano-wallet-test-utils = ./cardano-wallet-test-utils.nix;
        cardano-wallet-jormungandr = ./cardano-wallet-jormungandr.nix;
        cardano-wallet-byron = ./cardano-wallet-byron.nix;
        cardano-wallet-shelley = ./cardano-wallet-shelley.nix;
        persistent = ./persistent.nix;
        persistent-sqlite = ./persistent-sqlite.nix;
        persistent-template = ./persistent-template.nix;
        cardano-addresses = ./cardano-addresses.nix;
        cardano-api = ./cardano-api.nix;
        cardano-cli = ./cardano-cli.nix;
        cardano-config = ./cardano-config.nix;
        cardano-node = ./cardano-node.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        byron-spec-chain = ./byron-spec-chain.nix;
        byron-spec-ledger = ./byron-spec-ledger.nix;
        small-steps = ./small-steps.nix;
        shelley-spec-non-integral = ./shelley-spec-non-integral.nix;
        shelley-spec-ledger = ./shelley-spec-ledger.nix;
        shelley-spec-ledger-test = ./shelley-spec-ledger-test.nix;
        };
      };
  resolver = "https://raw.githubusercontent.com/input-output-hk/cardano-haskell/633f6141c3e7211dbb0c6ae6ad50d4976313e190/snapshots/cardano-1.14.2.yaml";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "cardano-crypto-praos" = {
            flags = { "external-libsodium-vrf" = lib.mkOverride 900 false; };
            };
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