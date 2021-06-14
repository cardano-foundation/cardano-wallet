{ system ? builtins.currentSystem
, crossSystem ? null
# Lets you customise ghc and profiling (see ./haskell.nix):
, config ? {}
# Lets you override niv dependencies of the project without
# modifications to the source.
# eg. to test build against a local checkout of cardano-node:
#   nix-build default.nix -A cardano-wallet --arg sourcesOverride '{ nixpkgs = ../nixpkgs; }'
, sourcesOverride ? {}
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  iohkNixMain = import sources.iohk-nix {};
  haskellNix = (import sources."haskell.nix" {
    inherit system;
    # if niv sources hackage or stackage are present, pass them
    # through to Haskell.nix.
    sourcesOverride =
      (if builtins.hasAttr "hackage" sources then { inherit (sources) hackage; } else {}) //
      (if builtins.hasAttr "stackage" sources then { inherit (sources) stackage; } else {})
      // sourcesOverride;
  }).nixpkgsArgs;
  # use our own nixpkgs if it exists in our sources,
  # otherwise use iohkNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then
      (builtins.trace "Not using IOHK default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
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
      (pkgs: _: {
        # commonLib: iohk-nix utils and our own:
        commonLib = pkgs.iohkNix
          // import ./util.nix { inherit (pkgs) lib; }
          # also expose our sources and overlays
          // { inherit overlays sources; };
      })
      # Haskell build tools
      (import ./overlays/build-tools.nix)
      # Cardano deployments
      (import ./overlays/cardano-deployments.nix)
      # Other packages overlay
      (import ./overlays/pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
