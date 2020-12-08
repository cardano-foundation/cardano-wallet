
{ system ? builtins.currentSystem
, crossSystem ? null
# Lets you customise ghc and profiling (see ./haskell.nix):
, config ? {}
# Lets you override niv dependencies of the project without
# modifications to the source.
# eg. to test build against a local checkout of cardano-node:
#   nix-build default.nix -A cardano-wallet --arg sourcesOverride '{ cardano-node = ../cardano-node; }'
, sourcesOverride ? {}
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  iohkNixMain = import sources.iohk-nix {};
  haskellNix = (import sources."haskell.nix" { inherit system sourcesOverride; }).nixpkgsArgs;
  # use our own nixpkgs if it exists in our sources,
  # otherwise use iohkNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then sources.nixpkgs
      # TODO: after updating to ghc-8.10, go back to default nixpkgs-20.09
      # (builtins.trace "Not using IOHK default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      # sources.nixpkgs)
    else iohkNixMain.nixpkgs;

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNixMain.overlays.haskell-nix-extra
    ++ iohkNixMain.overlays.crypto
    # iohkNix: nix utilities and niv:
    ++ iohkNixMain.overlays.iohkNix
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {
        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // iohkNix
          // import ./util.nix { inherit lib haskell-nix; }
          # also expose our sources and overlays
          // { inherit overlays sources; };
      })
      # cardano-node packages
      (import ./cardano-node-overlay.nix)
      # Other package overlays
      (import ./pkgs.nix { inherit system crossSystem config; })
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
