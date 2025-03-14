{ indexState, src, haskell-nix, ... }:
let
  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      hoogle = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.gitAndTools.git
      pkgs.haskellPackages.fourmolu
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.stylish-haskell
    ];
    shellHook = ''
      echo "Entering shell for rewrite-libs development"
    '';
  };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "cardano-deposit-wallet";
    compiler-nix-name = "ghc966";
    inherit src;
    shell = shell { inherit pkgs; };
    modules = [ ];
  };
  project = haskell-nix.cabalProject' mkProject;
  packages = {
    inherit project;
    default = packages.project.hsPkgs.rewrite-libs.components.exes.rewrite-libs;
  };
in {
  inherit packages;
  devShell = project.shell;
}
