############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, exes ? []
}:

let
  # Take the filename from the first exe passed in
  exe = pkgs.lib.head exes;
  name = "${exe.meta.name}-win64";

in pkgs.runCommand name { buildInputs = [ pkgs.buildPackages.zip ]; } ''
  mkdir -p $out/nix-support release
  cd release

  cp -fR ${pkgs.lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} .
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

  # make a separate deployments configuration package if needed
  if [ -d ${exe}/deployments ]; then
    cp -R ${exe}/deployments ..
    cd ../deployments
    chmod -R +w .

    zip -r $out/${exe.name}-deployments.zip .
    echo "file binary-dist $out/${exe.name}-deployments.zip" >> $out/nix-support/hydra-build-products
  fi
''
