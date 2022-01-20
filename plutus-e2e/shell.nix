{ plutusE2EPackages ? import ./default.nix { inherit system crossSystem config sourcesOverride; }
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? plutusE2EPackages.pkgs
, profiling ? false  # enable profiling in haskell dependencies
, sourcesOverride ? {}  # see sourcesOverride in nix/default.nix
}:

let
  inherit (pkgs) lib;
  inherit (pkgs.haskell-nix.haskellLib) selectProjectPackages;

  mkShell = name: project: project.shellFor rec {
    inherit name;
    packages = ps: lib.attrValues (selectProjectPackages ps);

    # fixme: this is needed to prevent Haskell.nix double-evaluating hoogle
    tools.hoogle = {
      inherit (pkgs.haskell-build-tools.hoogle) version;
      inherit (pkgs.haskell-build-tools.hoogle.project) index-state;
    };

    meta.platforms = lib.platforms.unix;
  };
in
  mkShell "plutus-E2E-shell" plutusE2EPackages.project
