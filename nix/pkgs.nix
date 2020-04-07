# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
  inherit (import ./default.nix {}) commonLib;
in pkgs: super: with pkgs; {
  jmPkgs = import ./jormungandr.nix { inherit (pkgs) commonLib; inherit pkgs; };
  cardanoNodePkgs = import sources.cardano-node { inherit system crossSystem config; };
  cardano-node = cardanoNodePkgs.cardano-node
    // commonLib.cardanoLib.forEnvironments (env:
      env // {
        configFile =
          let
            rawCfg = env.nodeConfig // {
              GenesisFile = env.genesisFile;
              minSeverity = "Error";
            };
          in
          builtins.toFile "cardano-node-config" (builtins.toJSON rawCfg);
        })
        // { topologyFiles = {
            # Note: It was easier to define as topologyFiles.testnet than testnet.topologyFile.
            # TODO: Retrieve these from iohk-nix or cardano-node directly.
            mainnet = builtins.toFile "topology" "{\"Producers\":[{\"addr\":\"relays-new.cardano-mainnet.iohk.io\",\"port\":3001,\"valency\":1}]}";
            testnet = builtins.toFile "topology" "{\"Producers\":[{\"addr\":\"relays-new.cardano-testnet.iohkdev.io\",\"port\":3001,\"valency\":1}]}";
          };};

  inherit (pkgs1903) stack;
}
