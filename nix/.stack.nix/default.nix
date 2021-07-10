{
  extras = hackage:
    {
      packages = {
        "OddWord" = (((hackage.OddWord)."1.0.2.0").revisions).default;
        "command" = (((hackage.command)."0.1.1").revisions).default;
        "markov-chain-usage-model" = (((hackage.markov-chain-usage-model)."0.0.0").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.7.0").revisions).default;
        "hspec" = (((hackage.hspec)."2.8.2").revisions).default;
        "hspec-core" = (((hackage.hspec-core)."2.8.2").revisions).default;
        "base16-bytestring" = (((hackage.base16-bytestring)."1.0.1.0").revisions).default;
        "bech32" = (((hackage.bech32)."1.1.0").revisions).default;
        "bech32-th" = (((hackage.bech32-th)."1.0.2").revisions).default;
        "Cabal" = (((hackage.Cabal)."3.4.0.0").revisions).default;
        "parsec" = (((hackage.parsec)."3.1.14.0").revisions).default;
        "async-timer" = (((hackage.async-timer)."0.2.0.0").revisions).default;
        "Unique" = (((hackage.Unique)."0.4.7.6").revisions).default;
        "Win32" = (((hackage.Win32)."2.6.2.0").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
        "gray-code" = (((hackage.gray-code)."0.3.1").revisions).default;
        "ip" = (((hackage.ip)."1.5.1").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "moo" = (((hackage.moo)."1.2").revisions).default;
        "nothunks" = (((hackage.nothunks)."0.1.2").revisions).default;
        "partial-order" = (((hackage.partial-order)."0.2.0.0").revisions).default;
        "regex-posix-clib" = (((hackage.regex-posix-clib)."2.7").revisions).default;
        "statistics-linreg" = (((hackage.statistics-linreg)."0.3").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.2.2.0").revisions).default;
        "transformers-except" = (((hackage.transformers-except)."0.1.1").revisions).default;
        cardano-wallet-core = ./cardano-wallet-core.nix;
        cardano-wallet-core-integration = ./cardano-wallet-core-integration.nix;
        cardano-wallet-cli = ./cardano-wallet-cli.nix;
        cardano-wallet-launcher = ./cardano-wallet-launcher.nix;
        cardano-numeric = ./cardano-numeric.nix;
        text-class = ./text-class.nix;
        cardano-wallet-test-utils = ./cardano-wallet-test-utils.nix;
        cardano-wallet = ./cardano-wallet.nix;
        strict-non-empty-containers = ./strict-non-empty-containers.nix;
        cardano-addresses-cli = ./cardano-addresses-cli.nix;
        cardano-addresses = ./cardano-addresses.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-crypto-praos = ./cardano-crypto-praos.nix;
        cardano-crypto-tests = ./cardano-crypto-tests.nix;
        cardano-slotting = ./cardano-slotting.nix;
        strict-containers = ./strict-containers.nix;
        cardano-crypto = ./cardano-crypto.nix;
        byron-spec-chain = ./byron-spec-chain.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        byron-spec-ledger = ./byron-spec-ledger.nix;
        cardano-ledger-byron = ./cardano-ledger-byron.nix;
        cardano-ledger-byron-test = ./cardano-ledger-byron-test.nix;
        cardano-ledger-core = ./cardano-ledger-core.nix;
        small-steps = ./small-steps.nix;
        small-steps-test = ./small-steps-test.nix;
        shelley-spec-non-integral = ./shelley-spec-non-integral.nix;
        shelley-spec-ledger = ./shelley-spec-ledger.nix;
        shelley-spec-ledger-test = ./shelley-spec-ledger-test.nix;
        cardano-ledger-shelley-ma = ./cardano-ledger-shelley-ma.nix;
        cardano-ledger-shelley-ma-test = ./cardano-ledger-shelley-ma-test.nix;
        cardano-api = ./cardano-api.nix;
        cardano-api-test = ./cardano-api-test.nix;
        cardano-cli = ./cardano-cli.nix;
        cardano-config = ./cardano-config.nix;
        cardano-node = ./cardano-node.nix;
        cardano-node-chairman = ./cardano-node-chairman.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-sl-x509 = ./cardano-sl-x509.nix;
        goblins = ./goblins.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        lobemo-backend-aggregation = ./lobemo-backend-aggregation.nix;
        lobemo-backend-ekg = ./lobemo-backend-ekg.nix;
        lobemo-backend-monitoring = ./lobemo-backend-monitoring.nix;
        lobemo-backend-trace-forwarder = ./lobemo-backend-trace-forwarder.nix;
        lobemo-scribe-systemd = ./lobemo-scribe-systemd.nix;
        tracer-transformers = ./tracer-transformers.nix;
        io-sim = ./io-sim.nix;
        io-sim-classes = ./io-sim-classes.nix;
        network-mux = ./network-mux.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        ouroboros-consensus-byron = ./ouroboros-consensus-byron.nix;
        ouroboros-consensus-cardano = ./ouroboros-consensus-cardano.nix;
        ouroboros-consensus-shelley = ./ouroboros-consensus-shelley.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-network-framework = ./ouroboros-network-framework.nix;
        ouroboros-network-testing = ./ouroboros-network-testing.nix;
        typed-protocols = ./typed-protocols.nix;
        typed-protocols-examples = ./typed-protocols-examples.nix;
        cardano-client = ./cardano-client.nix;
        ntp-client = ./ntp-client.nix;
        ouroboros-consensus-mock = ./ouroboros-consensus-mock.nix;
        Win32-network = ./Win32-network.nix;
        http-client = ./http-client.nix;
        };
      compiler.version = "8.10.5";
      compiler.nix-name = "ghc8105";
      };
  resolver = "lts-17.9";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "cardano-crypto-praos" = {
            flags = { "external-libsodium-vrf" = lib.mkOverride 900 false; };
            };
          "cryptonite" = {
            flags = { "support_rdrand" = lib.mkOverride 900 false; };
            };
          "zip" = { flags = { "disable-bzip2" = lib.mkOverride 900 true; }; };
          };
        })
    { packages = {}; }
    ];
  compiler = "ghc-8.10.5";
  }