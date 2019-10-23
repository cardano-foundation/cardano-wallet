############################################################################
# Release package for cardano-wallet-jormungandr
#
# This makes release builds of cardano-wallet-jormungandr and bundles
# dependencies as required for different systems.
#
############################################################################

{ pkgs
, version
, cardano-wallet-jormungandr
, jmPkgs
}:

with pkgs.lib;

let
  name = "cardano-wallet-jormungandr-${version}";

  jormungandr = jmPkgs.jormungandr;
  jormungandr-win64 = jmPkgs.jormungandr-win64;

  deps = {
    nix = ''
      strip $out/bin/cardano-wallet-jormungandr
      wrapProgram $out/bin/cardano-wallet-jormungandr \
        --prefix PATH : ${jormungandr}/bin
    '';
    darwin = ''
      cp ${jormungandr}/bin/* $out/bin
    '';
    windows = ''
      cp -v ${pkgs.libffi}/bin/libffi-6.dll $out/bin
      cp ${jormungandr-win64}/* $out/bin
    '';
  };
  provideDeps = { nix, darwin ? "", windows ? "" }:
    with pkgs.stdenv.hostPlatform;
    if isWindows then windows else (if isDarwin then darwin else nix);

in pkgs.runCommand name {
  inherit version;
  nativeBuildInputs = [ pkgs.makeWrapper pkgs.binutils ];
} ''
  cp -R ${cardano-wallet-jormungandr} $out
  chmod -R +w $out

  ${provideDeps deps}
''
