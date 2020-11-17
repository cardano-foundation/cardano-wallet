# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
  inherit (import ./default.nix {}) commonLib;
in pkgs: super: with pkgs; {
  cardanoNodePkgs = import sources.cardano-node {
    inherit system crossSystem config;
    gitrev = sources.cardano-node.rev;
  };
  cardano-node = cardanoNodePkgs.cardano-node // {
    # Provide real deployment configurations for use in dev/tests/benchmarks.
    # https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html
    deployments = let
      environments = {
        inherit (pkgs.commonLib.cardanoLib.environments)
          mainnet
          staging
          testnet
          shelley_qa
          ;
      };
      updateConfig = env: env.nodeConfig // {
        minSeverity = "Notice";
      } // (if (env.consensusProtocol == "Cardano") then {
        ByronGenesisFile = "genesis-byron.json";
        ShelleyGenesisFile = "genesis-shelley.json";
      } else {
        GenesisFile = "genesis.json";
      });
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
      jq . < ${mkTopology env} > $cfg/topology.json
      jq . < ${__toFile "${name}-config.json" (__toJSON (updateConfig env))} > $cfg/configuration.json
    '' + (if env.consensusProtocol == "Cardano" then ''
      jq . < ${env.nodeConfig.ByronGenesisFile} > $cfg/genesis-byron.json
      cp ${env.nodeConfig.ShelleyGenesisFile} $cfg/genesis-shelley.json
    '' else ''
      jq . < ${env.genesisFile} > $cfg/genesis.json
    '')) environments);

    # Provide configuration directory as a convenience
    configs = pkgs.runCommand "cardano-node-configs" {} ''
      cp -R ${sources.cardano-node}/configuration $out;
    '';
  };
  inherit (cardanoNodePkgs.haskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (pkgs1903) stack;
}
