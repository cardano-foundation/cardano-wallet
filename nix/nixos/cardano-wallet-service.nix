{ config, lib, pkgs, ... }:
let
  cfg = config.services.cardano-wallet;
  inherit (lib) mkIf mkEnableOption mkOption types;
  logLevels = [ "DEBUG" "INFO" "NOTICE" "WARNING" "ERROR" "CRITICAL" "ALERT" "EMERGENCY" ];
in
{
  options.services.cardano-wallet = {

    enable = mkEnableOption "Cardano Wallet service";

    serverArgs = mkOption {
      description = "Command-line to launch cardano-wallet server.";
      type = types.separatedString " ";
      default = lib.concatStringsSep " " (
        # Only specify arguments if they have different value than the default:
        lib.optionals (cfg.listenAddress != "127.0.0.1") [
          "--listen-address"
          (lib.escapeShellArg cfg.listenAddress)
        ] ++ lib.optionals (cfg.port != 8090) [
          "--port"
          (toString cfg.port)
        ] ++ lib.optionals (cfg.logLevel != "DEBUG") [
          "--log-level"
          cfg.logLevel
        ] ++ lib.optionals (cfg.syncTolerance != 300) [
          "--sync-tolerance"
          "${toString cfg.syncTolerance}s"
        ] ++ [
          "--node-socket"
          "\"$CARDANO_NODE_SOCKET_PATH\""
          "--pool-metadata-fetching"
          (if (cfg.poolMetadataFetching.enable)
          then
            (if cfg.poolMetadataFetching.smashUrl != null
            then cfg.poolMetadataFetching.smashUrl else "direct")
          else "none")
          "--${cfg.walletMode}"
        ] ++ lib.optional (cfg.walletMode != "mainnet")
          (lib.escapeShellArg cfg.genesisFile)
        ++ lib.optionals (cfg.tokenMetadataServer != null)
          [ "--token-metadata-server" cfg.tokenMetadataServer ]
        ++ lib.optionals (cfg.database != null)
          [ "--database" "\"$STATE_DIRECTORY\"" ]
        ++ lib.mapAttrsToList
          (name: level: "--trace-${name}=${level}")
          cfg.trace
      );
    };

    command = mkOption {
      type = types.str;
      internal = true;
      default = lib.concatStringsSep " " ([
        "${cfg.package}/bin/${cfg.package.exeName}"
        "serve"
        cfg.serverArgs
      ] ++ lib.optionals (cfg.rtsOpts != "") [ "+RTS" cfg.rtsOpts "-RTS" ]);
    };

    package = mkOption {
      type = types.package;
      default = ((import ../.. { }).legacyPackages.${pkgs.system}).hsPkgs.cardano-wallet-api.components.exes.cardano-wallet;
      description = "Package for the cardano wallet executable.";
    };

    genesisFile = mkOption {
      type = types.nullOr (types.either types.str types.path);
      default = null;
      description = "Path to genesis file, if not running on mainnet.";
    };

    listenAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Which host to bind the API server to.";
    };

    port = mkOption {
      type = types.port;
      default = 8090;
      description = "The port on which the cardano-wallet HTTP API server will listen.";
    };

    nodeSocket = mkOption {
      type = types.str;
      default = "/run/cardano-node/node.socket";
      description = ''Cardano-Node local communication socket path.'';
    };

    walletMode = mkOption {
      type = types.enum [ "mainnet" "testnet" ];
      default = "mainnet";
      description = "Which mode to start wallet in: --mainnet or --testnet";
    };

    database = mkOption {
      type = types.nullOr types.str;
      default = "cardano-wallet";
      description = ''Directory (under /var/lib/) for storing wallets.
        Run in-memory if null.
        Default to 'cardano-wallet'.
      '';
    };

    syncTolerance = mkOption {
      type = types.ints.positive;
      default = 300;
      description = "Time duration within which we consider being synced with the network. Expressed in seconds.";
    };

    poolMetadataFetching = mkOption {
      type = types.submodule {
        options = {
          enable = mkEnableOption "Stake pool metadata fetching.";
          smashUrl = mkOption {
            description = ''
              URL of SMASH metadata proxy to use.
              If null, metadata will be fetched directly from the
              stake pool's URL.
            '';
            type = types.nullOr types.str;
            default = null;
          };
        };
      };
      default = { enable = false; };
      example = {
        enable = true;
        smashUrl = "https://smash.cardano-mainnet.iohk.io";
      };
    };

    tokenMetadataServer = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Sets the URL of the token metadata server,
        default to 'metadataUrl' of the 'environnement' attribute, if exists.
        If unset, metadata will not be fetched. By using this
        option, you are fully trusting the operator of the
        metadata server to provide authentic token metadata.
      '';
    };

    logLevel = mkOption {
      type = types.enum logLevels;
      default = "DEBUG";
      description = "Global minimum severity for a message to be logged.";
    };

    trace = mkOption {
      type = types.attrsOf (types.enum (logLevels ++ [ "off" ]));
      default = { };
      description = ''
        For each tracer, minimum severity for a message to be logged, or
        "off" to disable the tracer".
      '';
    };

    rtsOpts = mkOption {
      type = types.separatedString " ";
      default = "-N2";
      example = "-M2G -N4";
      description = ''
        GHC runtime-system options for the cardano-wallet process.
        See https://downloads.haskell.org/ghc/8.10.7/docs/html/users_guide/runtime_control.html#setting-rts-options-on-the-command-line for documentation.
      '';
    };

  };

  config = mkIf cfg.enable {

    assertions = [
      {
        assertion = (cfg.walletMode == "mainnet") == (cfg.genesisFile == null);
        message = ''The option services.cardano-wallet.genesisFile must be set
        if, and only if, services.cardano-wallet.walletMode is not \"mainnet\".
      '';
      }
      {
        assertion = !(lib.hasPrefix "/" cfg.database || lib.hasPrefix ".." cfg.database);
        message = "The option services.cardano-node.database should be a relative path (of /var/lib/).";
      }
    ];

    systemd.services.cardano-wallet = {
      description = "cardano-wallet daemon";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        DynamicUser = true;
        ExecStart = cfg.command;
        StateDirectory = cfg.database;
      };

      environment = {
        CARDANO_NODE_SOCKET_PATH = cfg.nodeSocket;
      };
    };
  };
}
