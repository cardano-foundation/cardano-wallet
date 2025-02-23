############################################################################
# Release build for cardano-wallet
#
############################################################################

{ pkgs
, exe  # executable component for the program, from Haskell.nix
, backend ? null  # node backend
}:

with pkgs.lib;

let drv = pkgs.stdenv.mkDerivation rec {
  name = "${exe.exeName}-${version}";
  version = exe.identifier.version;
  buildInputs = [ pkgs.tree ];
  phases = [ "installPhase" ];
  installPhase = ''

    echo "Building ${exe.exeName} ${version}"
    tree ${exe}
    exit 1
    cp -R ${exe} $out
  '' + (optionalString (pkgs.stdenv.hostPlatform.isWindows && backend != null) ''
    # fixme: remove this
    cp -Rv ${backend.deployments} $out/deployments
  '');
  meta.platforms = platforms.all;
#   passthru = {
#     exePath = drv + "/bin/cardano-wallet";
#   } // (optionalAttrs (backend != null) { inherit backend; });
};
in drv
