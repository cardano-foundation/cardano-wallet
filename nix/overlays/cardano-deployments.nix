pkgs: _: {
  # Provide real deployment configurations for use in dev/tests/benchmarks.
  # https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html
  cardano-node-deployments = let
    environments = {
      inherit (pkgs.cardanoLib.environments)
        mainnet
        staging
        testnet
        shelley_qa
        alonzo-purple
        ;
    };
    updateConfig = env: env.nodeConfig // {
      minSeverity = "Notice";
    } // (if (env.consensusProtocol == "Cardano") then {
      ByronGenesisFile = "genesis-byron.json";
      ShelleyGenesisFile = "genesis-shelley.json";
      AlonzoGenesisFile = "genesis-alonzo.json";
    } else {
      GenesisFile = "genesis.json";
    });
    mkTopology = env: pkgs.cardanoLib.mkEdgeTopology {
      edgeNodes = [ env.relaysNew ];
      valency = 2;
    };
    mapAttrsToString = f: attrs:
      pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList f attrs);
  in
    pkgs.runCommand "cardano-node-deployments" {
      nativeBuildInputs = [ pkgs.buildPackages.jq ];
    } (mapAttrsToString (name: env: ''
      cfg=$out/${name}
      mkdir -p $cfg
      jq . < ${mkTopology env} > $cfg/topology.json
      jq . < ${__toFile "${name}-config.json" (__toJSON (updateConfig env))} > $cfg/configuration.json
    '' + (if env.consensusProtocol == "Cardano" then ''
      jq . < ${env.nodeConfig.ByronGenesisFile} > $cfg/genesis-byron.json
      cp ${env.nodeConfig.ShelleyGenesisFile} $cfg/genesis-shelley.json
      cp ${env.nodeConfig.AlonzoGenesisFile} $cfg/genesis-alonzo.json
    '' else ''
      jq . < ${env.genesisFile} > $cfg/genesis.json
    '')) environments);
}
