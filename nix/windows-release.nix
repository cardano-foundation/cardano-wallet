############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, exe
}:

let
  name = "${exe.name}-win64";

in pkgs.runCommand name { buildInputs = [ pkgs.buildPackages.zip ]; } ''
  mkdir -p $out/nix-support release
  cd release

  cp ${exe}/bin/* .
  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products

  # make a separate configuration package if needed
  if [ -d ${exe}/configuration ]; then
    cp -R ${exe}/configuration ..
    cd ../configuration
    chmod -R +w .

    zip -r $out/${exe.name}-configuration.zip .
    echo "file binary-dist $out/${exe.name}-configuration.zip" >> $out/nix-support/hydra-build-products
  fi
''
