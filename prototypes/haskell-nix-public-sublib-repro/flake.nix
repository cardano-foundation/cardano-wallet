{
  description = "Minimal haskell.nix repro for public sublibraries from Hackage and CHaP";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/ef52c36b9835c77a255befe2a20075ba71e3bfab";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/5aed5285a952e0b949eb3ba02c12fa4fcfef535f";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs =
    {
      self,
      CHaP,
      flake-utils,
      haskellNix,
      nixpkgs,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };

        mkProject =
          {
            name,
            src,
            extraArgs ? { },
          }:
          pkgs.haskell-nix.cabalProject' (
            {
              inherit name src;
              compiler-nix-name = "ghc966";
              shell = {
                exactDeps = true;
                nativeBuildInputs = [ pkgs.cabal-install ];
                withHoogle = false;
              };
            }
            // extraArgs
          );

        hackageProject = mkProject {
          name = "hackage-public-sublib-repro";
          src = ./hackage;
        };

        chapProject = mkProject {
          name = "chap-public-sublib-repro";
          src = ./chap;
          extraArgs = {
            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
            };
          };
        };
      in
      {
        packages = {
          hackage = hackageProject.hsPkgs."hackage-sublib-repro".components.exes."hackage-sublib-repro";
          chap = chapProject.hsPkgs."chap-sublib-repro".components.exes."chap-sublib-repro";
          default = hackageProject.hsPkgs."hackage-sublib-repro".components.exes."hackage-sublib-repro";
        };

        devShells = {
          hackage = hackageProject.shell;
          chap = chapProject.shell;
          default = hackageProject.shell;
        };
      }
    );
}
