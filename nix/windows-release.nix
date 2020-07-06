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

with pkgs.lib;

assert (assertMsg (builtins.length exes > 0) "empty list of exes");

let
  # Take the filename from the first exe passed in
  exe = head exes;
  name = "${exe.meta.name}-win64";

in pkgs.stdenv.mkDerivation {
  inherit name;
  buildInputs = [ pkgs.buildPackages.zip ];
  checkInputs = with pkgs.buildPackages; [ unzip ruby wineMinimal ];
  doCheck = true;
  phases = [ "buildPhase" "checkPhase" ];
  zipname = "${name}.zip";
  buildPhase = ''
    mkdir -p $out/nix-support release
    cd release

    cp -nR ${concatMapStringsSep " " (exe: "${exe}/bin/*") exes} .
    chmod -R +w .

    zip -r $out/$zipname .
    echo "file binary-dist $out/$zipname" > $out/nix-support/hydra-build-products

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
  '';

  # test that executables work under wine
  checkPhase = ''
    cd `mktemp -d`
    echo " - extracting $zipname"
    unzip $out/$zipname
    export PATH=`pwd`/$name:$PATH

    # setup wine
    export WINEPREFIX=$TMP
    export HOME=$TMP
    export WINEDLLOVERRIDES="winemac.drv=d"
    export WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag
    export LC_ALL=en_US.UTF-8

    echo " - running checks"
    ruby ${../scripts/check-bundle.rb} ${getName exe.name} wine64
  '';
}
