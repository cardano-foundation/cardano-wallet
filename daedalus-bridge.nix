{ target, pkgs, haskellPackages, system, crossSystem, jormungandr }:

let
  commonLib = import ./nix/iohk-common.nix {};
  pkgsCross = commonLib.getPkgs { crossSystem = pkgs.lib.systems.examples.mingwW64; };
  cardano-shell-src = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-shell";
    rev = "efba204c14a83b9e1af9bb138c2d551fe3d7a43e";
    sha256 = "0cr260b1drj4yal91fxcai9007jgbi2i2mh3mjnkq2rzjz6rvh8s";
  };
  cardano-shell = import cardano-shell-src { inherit system crossSystem; }; # todo, shell should accept target
in pkgs.runCommandCC "daedalus-bridge" {} ''
  mkdir -pv $out/bin $out/config
  touch $out/config/todo
  cd $out/bin
  cp ${haskellPackages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr}/bin/cardano-wallet-jormungandr* .
  cp ${cardano-shell.nix-tools.cexes.cardano-launcher.cardano-launcher}/bin/cardano-launcher* .
  #cp {nix-tools.cexes.cardano-node.cardano-node}/bin/cardano-node* .
  ${pkgs.lib.optionalString (target == "x86_64-windows") ''
    echo ${jormungandr}
    cp ${pkgsCross.libffi}/bin/libffi-6.dll .
    cp ${pkgsCross.openssl.out}/lib/libeay32.dll .
  ''}
  ${pkgs.lib.optionalString (target == "x86_64-linux") ''
    chmod +w -R .
    for bin in cardano-launcher; do
      strip $bin
      patchelf --shrink-rpath $bin
    done
  ''}
''
