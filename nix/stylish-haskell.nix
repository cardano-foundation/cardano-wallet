{ lib, runCommand, fetchFromGitHub, haskell-nix }:

let
  src = fetchFromGitHub {
    owner = "jaspervdj";
    repo = "stylish-haskell";
    rev = "9958a5253a9498c29508895450c4ac47542d5f2a";
    sha256 = "1lc2q15qdhv7xnawdqbrxcdhmy4m7h9v6z1sg4qpyvhf93b43bix";
  };

  pkgSet = haskell-nix.mkStackPkgSet {
    stack-pkgs = (haskell-nix.importAndFilterProject (haskell-nix.callStackToNix {
      inherit src;
    })).pkgs;
    pkg-def-extras = [];
    modules = [];
  };

  packages = pkgSet.config.hsPkgs;
in
  packages.stylish-haskell.components.exes.stylish-haskell
