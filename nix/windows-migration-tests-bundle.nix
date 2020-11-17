############################################################################
# Windows migration tests bundle
#
# This zips up the windows build of the migration tests and adds
# metadata for the Hydra build artifact.
#
# To build:
#
#   nix-build \
#        --arg crossSystem '{ config = "x86_64-w64-mingw32"; libc = "msvcrt"; platform = { }; }' \
#        nix/windows-migration-tests-bundle.nix
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, project ? import ../default.nix { inherit system crossSystem config; }
, pkgs ? project.pkgs
}:

let
  name = "cardano-wallet-${project.version}-migration-tests-win64";
  migration-tests = import ./migration-tests.nix { inherit system crossSystem config pkgs; };

in pkgs.buildPackages.runCommand name {
  nativeBuildInputs = [ pkgs.buildPackages.zip ];
} ''
  mkdir -p $out/nix-support
  cd ${migration-tests}
  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
