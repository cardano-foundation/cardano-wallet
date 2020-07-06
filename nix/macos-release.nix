############################################################################
# macOS release
#
# This bundles up the fully darwin build of the given exes, with their
# dependencies, and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, exes ? []
}:

with pkgs.lib;

assert (assertMsg (builtins.length exes > 0) "empty list of exes");

let
  name = (head exes).meta.name;
  tarname = "${name}-macos64.tar.gz";

in pkgs.runCommand name {
  buildInputs = with pkgs.buildPackages; [ gnutar gzip binutils ];
} ''
  mkdir ${name}
  cp -fR ${concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ${name}
  chmod -R 755 ${name}

  mkdir -p $out/nix-support
  tar -czf $out/${tarname} ${name}
  echo "file binary-dist $out/${tarname}" > $out/nix-support/hydra-build-products
''
