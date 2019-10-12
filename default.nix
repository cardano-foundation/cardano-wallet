{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./lib.nix { inherit system crossSystem config; }
# Use nixpkgs pin from iohkLib
, pkgs ? iohkLib.pkgs
}:

with import ./nix/util.nix { inherit pkgs; };

let
  haskell = iohkLib.nix-tools.haskell { inherit pkgs; };
  src = iohkLib.cleanSourceHaskell ./.;

  inherit (import ./nix/jormungandr.nix { inherit iohkLib pkgs; })
    jormungandr jormungandr-cli;

  cardano-http-bridge = iohkLib.rust-packages.pkgs.callPackage
    ./nix/cardano-http-bridge.nix { inherit pkgs; };
  cardano-sl-node = import ./nix/cardano-sl-node.nix { inherit pkgs; };

  haskellPackages = import ./nix/default.nix {
    inherit pkgs haskell src;
    inherit cardano-http-bridge cardano-sl-node jormungandr;
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

  daedalusBridge = pkgs.callPackage ./nix/daedalus-bridge.nix { };

in {
  inherit pkgs iohkLib src haskellPackages;
  inherit cardano-http-bridge cardano-sl-node jormungandr jormungandr-cli;
  inherit (haskellPackages.cardano-wallet-core.identifier) version;

  inherit (haskellPackages.cardano-wallet-http-bridge.components.exes)
    cardano-wallet-http-bridge;
  inherit (haskellPackages.cardano-wallet-jormungandr.components.exes)
    cardano-wallet-jormungandr;

  tests = collectComponents "tests" isCardanoWallet haskellPackages;
  benchmarks = collectComponents "benchmarks" isCardanoWallet haskellPackages;

  shell = haskellPackages.shellFor {
    name = "cardano-wallet-shell";
    packages = ps: with ps; [
      cardano-wallet-cli
      cardano-wallet-launcher
      cardano-wallet-core
      cardano-wallet-core-integration
      cardano-wallet-http-bridge
      cardano-wallet-jormungandr
      cardano-wallet-test-utils
      bech32
      text-class
    ];
    buildInputs =
      with pkgs.haskellPackages; [ stylish-haskell weeder ghcid ]
      ++ [ cardano-sl-node cardano-http-bridge jormungandr jormungandr-cli
           pkgs.pkgconfig pkgs.sqlite-interactive
           iohkLib.hlint iohkLib.openapi-spec-validator ];
  };
  devopsShell = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [ iohkLib.niv ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';

  };
}
