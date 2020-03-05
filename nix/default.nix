
{ system ? builtins.currentSystem
, crossSystem ? null
# Lets you customise ghc and profiling (see ./haskell.nix):
, config ? {}
# Lets you override niv dependencies of the project without
# modifications to the source.
# eg. to test build against a local checkout of cardano-node:
#   nix-build default.nix -A cardano-wallet-byron --arg sourcesOverride '{ cardano-node = ../cardano-node; }'
, sourcesOverride ? {}
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  iohkNix = import sources.iohk-nix {};
  haskellNix = import sources."haskell.nix";
  # use our own nixpkgs if it exists in our sources,
  # otherwise use iohkNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then (builtins.trace "Not using IOHK default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else (builtins.trace "Using IOHK default nixpkgs"
      iohkNix.nixpkgs);

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra
    # iohkNix: nix utilities and niv:
    ++ iohkNix.overlays.iohkNix
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {

        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // iohkNix
          // import ./util.nix { inherit lib haskell-nix; }
          # also expose our sources and overlays
          // { inherit overlays sources; };
      })
      # And, of course, our haskell-nix-ified stack project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
