############################################################################
# Release build for cardano-wallet
#
############################################################################

{ pkgs
, exe  # executable component for the program, from Haskell.nix
, backend ? null  # node backend
}:

with pkgs.lib;

pkgs.stdenv.mkDerivation rec {
  name = "${exe.identifier.name}-${version}";
  version = exe.identifier.version;
  phases = [ "installPhase" ];
  installPhase = ''
    cp -R ${exe} $out
  '' + (optionalString (pkgs.stdenv.hostPlatform.isWindows && backend != null) ''
    # fixme: remove this
    cp -Rv ${backend.deployments} $out/deployments
  '');

  meta.platforms = platforms.all;
  passthru = optionalAttrs (backend != null) { inherit backend; };
}
