{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? import ./default.nix {}
}:

with pkgs.lib;

let
  src = pkgs.applyPatches {
    src = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "cardano-sl";
      rev = "c1815b7dce5fb71d997bd3a94c4c5ccf2c9a9a94";
      sha256 = "1dzfhv7qhv1498sd8jrac39qd8vgpdahv7dskg1hnfs5rw2gipar";
    };
    patches = [ ./cardano-sl-bitrot.patch ];
  };
  # src = pkgs.haskell-nix.haskellLib.cleanGit {
  #   name = "cardano-sl";
  #   src = ../../cardano-sl;
  # };

  proj = pkgs.haskell-nix.project {
    projectFileName = "stack.yaml";
    inherit src;
    compiler-nix-name = "ghc865"; # Not used for `stack.yaml` based projects.

    modules = [
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        nonReinstallablePkgs =
            [ "rts" "ghc" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "array" "deepseq" "pretty" "ghc-boot-th" "template-haskell" "ghc-heap" ];
        doHaddock = false;
        doExactConfig = true;
      }
      {
        packages.cardano-sl.patches = [ src /patches/cardano-sl.patch ];
        packages.ekg-wai.components.library.enableSeparateDataOutput = true;
      }
    ];

  };

in proj.cardano-wallet.components.exes.export-wallets
