# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
  commonLib' = import ./default.nix {};
  commonLib = commonLib'.commonLib;
in pkgs: super: with pkgs; {
  jmPkgs = import ./jormungandr.nix { inherit (pkgs) commonLib; inherit pkgs; };
  cardanoNodePkgs = import sources.cardano-node { inherit system crossSystem config; };
  cardano-node = cardanoNodePkgs.cardano-node // {
    mainnet = {
      configFile =
        let
          rawCfg = commonLib.cardanoLib.environments.mainnet.nodeConfig // {
						GenesisFile = commonLib.cardanoLib.environments.mainnet.genesisFile;
					};
				in
          builtins.toFile "cardano-node-mainnet-config" (builtins.toJSON rawCfg);
    };

    # provide configuration directory as a convenience
    configs = pkgs.runCommand "cardano-node-configs" {} ''
      cp -R ${sources.cardano-node}/configuration $out;
    '';
  };
  inherit (pkgs1903) stack;
}
