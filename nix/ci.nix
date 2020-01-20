# Hercules CI attribute set
# https://hercules-ci.com/github/input-output-hk/cardano-wallet
# https://docs.hercules-ci.com/hercules-ci/getting-started/minimal-repository/

let
  inherit (import ./nixpkgs-haskell.nix {}) lib;

  walletJobs = walletPkgs:
    walletPkgs.pkgs.recurseIntoAttrs {
      inherit (walletPkgs)
        cardano-wallet-jormungandr
        checks
        benchmarks;
    };

  nativeBuilds = builtins.mapAttrs (system: _:
    walletJobs (import ../default.nix { inherit system; })
  ) {
    x86_64-linux = {};
    x86_64-darwin = {};
  };

  crossSystems = with lib.systems.examples;
    [ mingwW64 ];

  crossBuilds = lib.listToAttrs (map (crossSystem: {
    name = crossSystem.config;
    value = walletJobs (import ../default.nix {
      system = "x86_64-linux";
      inherit crossSystem;
    });
  }) crossSystems);

in
  nativeBuilds // crossBuilds
