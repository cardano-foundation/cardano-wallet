############################################################################
# Release package for cardano-wallet-itn
#
# This makes release builds of cardano-wallet-itn and bundles
# dependencies as required for different systems.
#
############################################################################

{ pkgs
, gitrev
, cardano-wallet-itn
, jmPkgs
, haskellBuildUtils
}:

with pkgs.lib;

let
  name = "cardano-wallet-itn-${version}";
  version = cardano-wallet-itn.identifier.version;

  buildCommand =
    nativeBuildInputs: installPhase:
      pkgs.stdenv.mkDerivation {
        inherit name version installPhase;
        phases = [ "installPhase" ];
        nativeBuildInputs = nativeBuildInputs ++ [ haskellBuildUtils pkgs.buildPackages.nix ];
        meta.platforms = platforms.all;
      };

  jormungandr = jmPkgs.jormungandr;
  jormungandr-win64 = jmPkgs.jormungandr-win64;

  drvs = {
    nix = buildCommand [ pkgs.buildPackages.makeWrapper pkgs.buildPackages.binutils ] ''
      cp -R ${cardano-wallet-itn} $out
      chmod -R +w $out
      ${setGitRev}
      $STRIP $out/bin/cardano-wallet-itn
      wrapProgram $out/bin/cardano-wallet-itn \
        --prefix PATH : ${jormungandr}/bin
    '';

    static = buildCommand [ pkgs.buildPackages.binutils ] ''
      cp -R ${cardano-wallet-itn} $out
      chmod -R +w $out
      ${setGitRev}
      $STRIP $out/bin/cardano-wallet-itn
    '';

    darwin = buildCommand [] ''
      cp -R ${cardano-wallet-itn} $out
      chmod -R +w $out
      rewrite-libs $out/bin $out/bin/cardano-wallet-itn
      ${setGitRev}
      cp ${jormungandr}/bin/* $out/bin
    '';

    windows = buildCommand [] ''
      cp -R ${cardano-wallet-itn} $out
      chmod -R +w $out
      ${setGitRev}
      rm $out/bin/libffi-6.dll
      cp -v ${pkgs.libffi}/bin/libffi-6.dll $out/bin
      cp ${jormungandr-win64}/bin/* $out/bin
    '';
  };

  setGitRev = ''
    set-git-rev "${gitrev}" $out/bin/cardano-wallet-itn* || true
  '';

in
  with pkgs.stdenv.hostPlatform;
  with drvs;
  if isWindows then windows else (if isDarwin then darwin else (if isMusl then static else nix))
