######################################################################
# Overlay containing extra Haskell packages which we build with
# Haskell.nix, but which aren't part of our project's package set
# themselves.
#
# These are e.g. build tools for developer usage.
#
# NOTE: Updating versions of Haskell tools
#
# Modify the tool version number to a different Hackage version.
# If you have chosen a recent version, you may also need to
# advance the "index-state" variable to include the upload date
# of your package.
#
# When increasing the "index-state" variable, it's likely that
# you will also need to update Hackage.nix to get a recent
# Hackage package index.
#
#   niv update hackage
#
# After changing tool versions, you can update the generated
# files which are cached in ./nix/materialized. Run this
# command, follow the instructions shown, then commit the
# updated files.
#
#   nix-shell --run regenerate-materialized-nix
#
######################################################################

let
  index-state = "2021-10-05T00:00:00Z";
  tools = {
    cabal-cache.version             = "1.0.2.1";
    cabal-install.exe               = "cabal";
    cabal-install.version           = "3.4.0.0";
    haskell-language-server.version = "1.4.0.0";
    hoogle.version                  = "5.0.18.1";
    hlint.version                   = "3.3.1";
    lentil.version                  = "1.5.2.0";
    stylish-haskell.version         = "0.11.0.3";
    weeder.version                  = "2.1.3";
  };

  compiler-nix-name = "ghc8107";  # TODO: get it from the project
in

pkgs: super: let
  hsPkgs = pkgs.lib.mapAttrs mkTool tools;

  mkTool = name: args: pkgs.haskell-nix.hackage-package ({
    inherit name index-state compiler-nix-name;
  } // pkgs.lib.optionalAttrs enableMaterialization {
    checkMaterialization = false;
    materialized = ../materialized + "/${name}";
  } // builtins.removeAttrs args ["exe"]);

  # A script for updating materialized files
  regenerateMaterialized = pkgs.writeShellScriptBin "regenerate-materialized-nix"
    (pkgs.lib.optionalString enableMaterialization
      (pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (name: hsPkg: ''
        echo 'Updating materialized nix for ${name}'
        ${mkMaterialize hsPkg}
      '') hsPkgs)));
  mkMaterialize = hsPkg: hsPkg.project.plan-nix.passthru.updateMaterialized;
  # https://github.com/input-output-hk/nix-tools/issues/97
  enableMaterialization = pkgs.stdenv.isLinux;

  # Get the actual tool executables from the haskell packages.
  mapExes = pkgs.lib.mapAttrs (name: hsPkg: hsPkg.components.exes.${tools.${name}.exe or name});

in {
  haskell-build-tools = pkgs.recurseIntoAttrs
    ((super.haskell-build-tools or {})
      // { inherit regenerateMaterialized; }
      // mapExes hsPkgs);

  # These overrides are picked up by cabalWrapped in iohk-nix
  cabal = pkgs.haskell-build-tools.cabal-install;
  cabal-install = pkgs.haskell-build-tools.cabal-install;

  # Override this package set from iohk-nix with local materializations.
  # This is a little bit messy, unfortunately.
  iohk-nix-utils = let
    name = "iohk-nix-utils";
    project = super.haskellBuildUtils.mkProject ({
      inherit compiler-nix-name index-state;
    } // pkgs.lib.optionalAttrs enableMaterialization {
      checkMaterialization = false;
      materialized = ../materialized + "/${name}";
    });
  in pkgs.symlinkJoin {
    inherit name;
    paths = pkgs.lib.attrValues project.iohk-nix-utils.components.exes;
    passthru = {
      inherit project;
      inherit (project) roots;
      regenerateMaterialized = pkgs.writeShellScriptBin "regenerate-materialized-nix"
        (pkgs.lib.optionalString enableMaterialization ''
          echo 'Updating materialized nix for ${name}'
          ${mkMaterialize project.iohk-nix-utils}
        '');
    };
  };
}
