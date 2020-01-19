############################################################################
# Windows migration tests bundle
#
# This zips up the windows build of the migration tests and adds
# metadata for the Hydra build artifact.
#
############################################################################

{ pkgs
, project
, migration-tests
}:

let
  name = "cardano-wallet-jormungandr-${project.version}-migration-tests-win64";

in pkgs.runCommand name {
  nativeBuildInputs = [ pkgs.zip ];
} ''
  mkdir -p $out/nix-support
  cd ${migration-tests}
  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
