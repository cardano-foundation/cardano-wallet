{ target, pkgs, haskellPackages, system, crossSystem, jormungandr }:

let
  commonLib = import ../lib.nix {};
  pkgsCross = commonLib.getPkgs { crossSystem = pkgs.lib.systems.examples.mingwW64; };
  cardano-shell = import commonLib.sources.cardano-shell { inherit system crossSystem; }; # todo, shell should accept target
  jormungandrConfig = builtins.toFile "config.yaml" (builtins.toJSON commonLib.jormungandrLib.defaultJormungandrConfig);
in pkgs.runCommandCC "daedalus-bridge" {} ''
  mkdir -pv $out/bin $out/config
  cp ${jormungandrConfig} $out/config/jormungandr-config.yaml
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
