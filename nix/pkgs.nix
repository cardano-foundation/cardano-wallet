# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
  inherit (import ./default.nix {}) commonLib;
in pkgs: super: with pkgs; {
  jmPkgs = import ./jormungandr.nix { inherit (pkgs) commonLib; inherit pkgs; };
  cardanoNodePkgs = import sources.cardano-node { inherit system crossSystem config; };
  cardano-node = cardanoNodePkgs.cardano-node // {
    # Provide real deployment configurations for use in dev/tests/benchmarks.
    # https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html
    deployments = let
      environments = {
        inherit (pkgs.commonLib.cardanoLib.environments) ff mainnet testnet;
      };
      updateConfig = cfg: cfg // {
        GenesisFile = "genesis.json";
        minSeverity = "Notice";
      };
      mkTopology = env: pkgs.commonLib.cardanoLib.mkEdgeTopology {
        edgeNodes = [ env.relaysNew ];
        valency = 2;
      };
      mapAttrsToString = f: attrs:
        pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList f attrs);
    in pkgs.runCommand "cardano-node-deployments" {
      nativeBuildInputs = [ pkgs.buildPackages.jq ];
    } (mapAttrsToString (name: env: ''
      cfg=$out/${name}
      mkdir -p $cfg
      jq . < ${__toFile "${name}-config.json" (__toJSON (updateConfig env.nodeConfig))} > $cfg/configuration.json
      jq . < ${env.genesisFile} > $cfg/genesis.json
      jq . < ${mkTopology env} > $cfg/topology.json
    '') environments);

    # Provide configuration directory as a convenience
    configs = pkgs.runCommand "cardano-node-configs" {} ''
      cp -R ${sources.cardano-node}/configuration $out;
    '';
  };
  inherit (cardanoNodePkgs.haskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (pkgs1903) stack;
}
