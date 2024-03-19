################################################################################
# Wallet service wrapper script
#
# This script runs the cardano-wallet server preconfigured with
# a specific network.
#
# It is used by devops to run the wallet in Nomad clusters.
#
################################################################################

{ evalService
, project
, customConfigs
}:
with project.pkgs;
let
  mkScript = envConfig:
    let
      service = evalService {
        inherit pkgs customConfigs;
        serviceName = "cardano-wallet";
        modules = [
          ./nixos/cardano-wallet-service.nix
          ({ config, ... }: {
            services.cardano-wallet = let cfg = config.services.cardano-wallet; in
              {
                package = lib.mkDefault project.hsPkgs.cardano-wallet-api.components.exes.cardano-wallet;
                walletMode = lib.mkDefault ({ mainnet = "mainnet"; }.${envConfig.name} or "testnet");
                genesisFile = lib.mkIf (cfg.walletMode != "mainnet")
                  (lib.mkDefault envConfig.nodeConfig.ByronGenesisFile);
                database = lib.mkDefault null;
                nodeSocket = lib.mkDefault "";
                poolMetadataFetching = lib.mkDefault {
                  enable = lib.mkDefault true;
                  smashUrl = lib.mkIf (envConfig ? smashUrl)
                    (lib.mkDefault envConfig.smashUrl);
                };
                tokenMetadataServer = lib.mkIf (envConfig ? metadataUrl) (lib.mkDefault envConfig.metadataUrl);
              };
          })
        ];
      };

    in
    writeScriptBin "cardano-wallet-${envConfig.name}" ''
      #!${pkgs.runtimeShell}
      set -euo pipefail
      ${lib.optionalString (service.database != null) ''
      STATE_DIRECTORY="${service.database}"
      ''}
      ${lib.optionalString (service.nodeSocket != "") ''
      CARDANO_NODE_SOCKET_PATH="${service.nodeSocket}"
      ''}
      exec ${service.command} $@
    '';

  debugDeps = with pkgs; [
    coreutils
    findutils
    gnugrep
    gnused
    sqlite
    strace
    lsof
    dnsutils
    bashInteractive
    iproute
    curl
    netcat
    bat
    tree
  ];

in
cardanoLib.forEnvironments (environment: lib.recurseIntoAttrs (
  let wallet = mkScript environment;
  in
  {
    inherit wallet;
  } // lib.optionalAttrs stdenv.buildPlatform.isLinux {
    wallet-debug = pkgs.symlinkJoin {
      inherit (wallet) name;
      paths = [ wallet ] ++ debugDeps;
    };
  }
))
