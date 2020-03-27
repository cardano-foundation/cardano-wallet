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
  name = (head exes).meta.name;
  tarname = "${name}-linux64.tar.gz";

in pkgs.runCommand name {
  buildInputs = with pkgs.buildPackages; [ gnutar gzip binutils ];
} ''
  mkdir ${name}
  cp -R ${concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ${name}
  chmod -R 755 ${name}
  strip ${name}/*

  mkdir -p $out/nix-support
  tar -czf $out/${tarname} ${name}
  echo "file binary-dist $out/${tarname}" > $out/nix-support/hydra-build-products
''
