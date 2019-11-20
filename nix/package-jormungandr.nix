############################################################################
# Release package for cardano-wallet-jormungandr
#
# This makes release builds of cardano-wallet-jormungandr and bundles
# dependencies as required for different systems.
#
############################################################################

{ pkgs
, gitrev
, cardano-wallet-jormungandr
, jmPkgs
, haskellBuildUtils
}:

with pkgs.lib;

let
  name = "cardano-wallet-jormungandr-${version}";
  version = cardano-wallet-jormungandr.identifier.version;

  buildCommand =
    nativeBuildInputs: installPhase:
      pkgs.stdenv.mkDerivation {
        inherit name version installPhase;
        phases = [ "installPhase" ];
        nativeBuildInputs = nativeBuildInputs ++ [ haskellBuildUtils pkgs.buildPackages.nix ];
      };

  jormungandr = jmPkgs.jormungandr;
  jormungandr-win64 = jmPkgs.jormungandr-win64;

  drvs = {
    nix = buildCommand [ pkgs.buildPackages.makeWrapper pkgs.buildPackages.binutils ] ''
      cp -R ${cardano-wallet-jormungandr} $out
      chmod -R +w $out
      ${setGitRev}
      strip $out/bin/cardano-wallet-jormungandr
      wrapProgram $out/bin/cardano-wallet-jormungandr \
        --prefix PATH : ${jormungandr}/bin
    '';

    static = buildCommand [ pkgs.buildPackages.binutils ] ''
      cp -R ${cardano-wallet-jormungandr} $out
      chmod -R +w $out
      ${setGitRev}
      strip $out/bin/cardano-wallet-jormungandr
    '';

    darwin = buildCommand [] ''
      cp -R ${cardano-wallet-jormungandr} $out
      chmod -R +w $out
      rewrite-libs $out/bin $out/bin/cardano-wallet-jormungandr
      ${setGitRev}
      cp ${jormungandr}/bin/* $out/bin
    '';

    windows = buildCommand [] ''
      cp -R ${cardano-wallet-jormungandr} $out
      chmod -R +w $out
      ${setGitRev}
      cp -v ${pkgs.libffi}/bin/libffi-6.dll $out/bin
      cp ${jormungandr-win64}/bin/* $out/bin
    '';
  };

  setGitRev = ''
    set-git-rev "${gitrev}" $out/bin/cardano-wallet-jormungandr* || true
  '';

in
  with pkgs.stdenv.hostPlatform;
  with drvs;
  if isWindows then windows else (if isDarwin then darwin else (if isMusl then static else nix))
