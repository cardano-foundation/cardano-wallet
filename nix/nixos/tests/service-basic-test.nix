{ pkgs, project, lib, ... }: with pkgs;
let
  inherit (project.hsPkgs.cardano-wallet.components.exes) cardano-wallet;
  inherit (pkgs) cardanoLib;
in
{
  name = "wallet-nixos-test";
  nodes = {
    machine = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
        (project.pkg-set.config.packages.cardano-node.src + "/nix/nixos")
      ];
      services.cardano-wallet = {
        enable = true;
        package = cardano-wallet;
        walletMode = "mainnet";
        nodeSocket = config.services.cardano-node.socketPath;
        poolMetadataFetching = {
          enable = true;
          smashUrl = cardanoLib.environments.mainnet.smashUrl;
        };
        tokenMetadataServer = cardanoLib.environments.mainnet.metadataUrl;
      };
      services.cardano-node = {
        enable = true;
        environment = "mainnet";
        environments = { mainnet = { }; };
        package = project.hsPkgs.cardano-node.components.exes.cardano-node;
        inherit (cardanoLib.environments.mainnet) nodeConfig;
        topology = cardanoLib.mkEdgeTopology {
          port = 3001;
          edgeNodes = [ "127.0.0.1" ];
        };
        systemdSocketActivation = true;
      };
      systemd.services.cardano-node.serviceConfig.Restart = lib.mkForce "no";
      systemd.services.cardano-wallet = {
        after = [ "cardano-node.service" ];
        serviceConfig = {
          Restart = "no";
          SupplementaryGroups = "cardano-node";
        };
      };
    };
  };
  testScript = ''
    start_all()
    machine.wait_for_unit("cardano-node.service")
    machine.wait_for_open_port(3001)
    machine.wait_for_unit("cardano-wallet.service")
    machine.wait_for_open_port(8090)
    machine.succeed(
        "${cardano-wallet}/bin/cardano-wallet network information"
    )
  '';

}
