{
  extras = hackage:
    {
      packages = {
        "OddWord" = (((hackage.OddWord)."1.0.2.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "wai-extra" = (((hackage.wai-extra)."3.0.29.1").revisions).default;
        "servant" = (((hackage.servant)."0.17").revisions).default;
        "servant-server" = (((hackage.servant-server)."0.17").revisions).default;
        "servant-client-core" = (((hackage.servant-client-core)."0.17").revisions).default;
        "servant-client" = (((hackage.servant-client)."0.17").revisions).default;
        "servant-swagger" = (((hackage.servant-swagger)."1.1.8").revisions).default;
        "zip" = (((hackage.zip)."1.3.0").revisions).default;
        "base16" = (((hackage.base16)."0.1.2.1").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "base64" = (((hackage.base64)."0.4.1").revisions).default;
        "bech32" = (((hackage.bech32)."1.1.0").revisions).default;
        "bech32-th" = (((hackage.bech32-th)."1.0.2").revisions).default;
        "bimap" = (((hackage.bimap)."0.4.0").revisions).default;
        "binary" = (((hackage.binary)."0.8.7.0").revisions).default;
        "brick" = (((hackage.brick)."0.47").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
        "clock" = (((hackage.clock)."0.8").revisions).default;
        "config-ini" = (((hackage.config-ini)."0.2.4.0").revisions).default;
        "connection" = (((hackage.connection)."0.3.1").revisions).default;
        "containers" = (((hackage.containers)."0.5.11.0").revisions).default;
        "data-clist" = (((hackage.data-clist)."0.1.2.2").revisions).default;
        "dns" = (((hackage.dns)."3.0.4").revisions).default;
        "generic-monoid" = (((hackage.generic-monoid)."0.1.0.0").revisions).default;
        "generics-sop" = (((hackage.generics-sop)."0.5.1.0").revisions).default;
        "gray-code" = (((hackage.gray-code)."0.3.1").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0.2").revisions).default;
        "hspec" = (((hackage.hspec)."2.7.0").revisions).default;
        "hspec-core" = (((hackage.hspec-core)."2.7.0").revisions).default;
        "hspec-discover" = (((hackage.hspec-discover)."2.7.0").revisions).default;
        "io-streams" = (((hackage.io-streams)."1.5.1.0").revisions).default;
        "io-streams-haproxy" = (((hackage.io-streams-haproxy)."1.0.1.0").revisions).default;
        "katip" = (((hackage.katip)."0.8.4.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "moo" = (((hackage.moo)."1.2").revisions).default;
        "network" = (((hackage.network)."3.1.1.1").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.2").revisions).default;
        "quickcheck-instances" = (((hackage.quickcheck-instances)."0.3.19").revisions).default;
        "QuickCheck" = (((hackage.QuickCheck)."2.12.6.1").revisions).default;
        "quiet" = (((hackage.quiet)."0.2").revisions).default;
        "snap-core" = (((hackage.snap-core)."1.0.4.1").revisions).default;
        "snap-server" = (((hackage.snap-server)."1.1.1.1").revisions).default;
        "sop-core" = (((hackage.sop-core)."0.5.0.1").revisions).default;
        "statistics-linreg" = (((hackage.statistics-linreg)."0.3").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "systemd" = (((hackage.systemd)."2.3.0").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.2").revisions).default;
        "text" = (((hackage.text)."1.2.4.0").revisions).default;
        "text-zipper" = (((hackage.text-zipper)."0.10.1").revisions).default;
        "th-lift-instances" = (((hackage.th-lift-instances)."0.1.14").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "transformers-except" = (((hackage.transformers-except)."0.1.1").revisions).default;
        "Unique" = (((hackage.Unique)."0.4.7.6").revisions).default;
        "websockets" = (((hackage.websockets)."0.12.6.1").revisions).default;
        "Win32" = (((hackage.Win32)."2.6.2.0").revisions).default;
        "word-wrap" = (((hackage.word-wrap)."0.4.1").revisions).default;
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
        cardano-transactions = ./cardano-transactions.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-slotting = ./cardano-slotting.nix;
        cardano-crypto-praos = ./cardano-crypto-praos.nix;
        cardano-crypto = ./cardano-crypto.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        byron-spec-chain = ./byron-spec-chain.nix;
        byron-spec-ledger = ./byron-spec-ledger.nix;
        small-steps = ./small-steps.nix;
        small-steps-test = ./small-steps-test.nix;
        shelley-spec-non-integral = ./shelley-spec-non-integral.nix;
        shelley-spec-ledger = ./shelley-spec-ledger.nix;
        shelley-spec-ledger-test = ./shelley-spec-ledger-test.nix;
        cardano-api = ./cardano-api.nix;
        cardano-cli = ./cardano-cli.nix;
        cardano-config = ./cardano-config.nix;
        cardano-node = ./cardano-node.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-sl-x509 = ./cardano-sl-x509.nix;
        goblins = ./goblins.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        lobemo-backend-aggregation = ./lobemo-backend-aggregation.nix;
        lobemo-backend-ekg = ./lobemo-backend-ekg.nix;
        lobemo-backend-monitoring = ./lobemo-backend-monitoring.nix;
        lobemo-scribe-systemd = ./lobemo-scribe-systemd.nix;
        tracer-transformers = ./tracer-transformers.nix;
        lobemo-backend-trace-forwarder = ./lobemo-backend-trace-forwarder.nix;
        io-sim = ./io-sim.nix;
        io-sim-classes = ./io-sim-classes.nix;
        network-mux = ./network-mux.nix;
        ntp-client = ./ntp-client.nix;
        Win32-network = ./Win32-network.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        ouroboros-consensus-byron = ./ouroboros-consensus-byron.nix;
        ouroboros-consensus-byronspec = ./ouroboros-consensus-byronspec.nix;
        ouroboros-consensus-cardano = ./ouroboros-consensus-cardano.nix;
        ouroboros-consensus-shelley = ./ouroboros-consensus-shelley.nix;
        ouroboros-consensus-mock = ./ouroboros-consensus-mock.nix;
        ouroboros-consensus-test-infra = ./ouroboros-consensus-test-infra.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-network-framework = ./ouroboros-network-framework.nix;
        ouroboros-network-testing = ./ouroboros-network-testing.nix;
        typed-protocols = ./typed-protocols.nix;
        typed-protocols-examples = ./typed-protocols-examples.nix;
        cborg = ./cborg.nix;
        http-client = ./http-client.nix;
        };
      };
  resolver = "lts-14.25";
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