############################################################################
# Release build for cardano-wallet
#
# Copies the Haskell.nix-built executable and stamps it with the git
# revision.  The stamp is applied here (not inside haskell.nix) so that
# the expensive Haskell compilation is cached across commits — only this
# lightweight copy+stamp derivation changes when the revision changes.
############################################################################
{
  pkgs,
  exe, # executable component for the program, from Haskell.nix
  backend ? null, # node backend
  set-git-rev ? null, # set-git-rev tool derivation
  gitrev ? null, # git revision string to stamp into binaries
}:
with pkgs.lib; let
  drv = pkgs.stdenv.mkDerivation rec {
    name = "${exe.exeName}-${version}";
    version = exe.identifier.version;
    phases = ["installPhase"];
    installPhase =
      ''
        cp -R ${exe} $out
      ''
      + (optionalString (set-git-rev != null && gitrev != null) ''
        chmod +w $out/bin/*
        ${set-git-rev}/bin/set-git-rev "${gitrev}" $out/bin/*
      '')
      + (optionalString (pkgs.stdenv.hostPlatform.isWindows && backend != null) ''
        # fixme: remove this
        cp -Rv ${backend.deployments} $out/deployments
      '');
    meta.platforms = platforms.all;
    passthru =
      {
        exePath = drv + "/bin/cardano-wallet";
      }
      // (optionalAttrs (backend != null) {inherit backend;});
  };
in
  drv
