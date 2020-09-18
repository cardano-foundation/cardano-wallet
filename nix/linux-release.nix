############################################################################
# Linux release
#
# This bundles up the fully static Linux build of the given exes and
# sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, exes ? []
}:

with pkgs.lib;

assert (assertMsg (builtins.length exes > 0) "empty list of exes");

let
  exe = head exes;
  name = exe.meta.name;

in pkgs.stdenv.mkDerivation {
  inherit name;
  buildInputs = with pkgs.buildPackages; [ gnutar gzip binutils ];
  checkInputs = with pkgs.buildPackages; [ ruby gnugrep glibc.bin ];
  doCheck = true;
  phases = [ "buildPhase" "checkPhase" ];
  tarname = "${name}-linux64.tar.gz";
  buildPhase = ''
    mkdir $name
    cp -nR ${concatMapStringsSep " " (exe: "${exe}/bin/*") exes} $name
    chmod -R 755 $name
    strip $name/*

    mkdir -p $out/nix-support
    tar -czf $out/$tarname $name
    echo "file binary-dist $out/$tarname" > $out/nix-support/hydra-build-products
  '';

  # test that executables work
  checkPhase = ''
    cd `mktemp -d`
    echo " - extracting $tarname"
    tar -xzvf $out/$tarname
    export PATH=`pwd`/$name:$PATH

    echo " - running checks"
    ruby ${../scripts/check-bundle.rb} ${getName exe.name}
  '';
}
