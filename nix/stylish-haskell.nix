{ lib, runCommand, fetchFromGitHub, haskell-nix }:

let
  src = fetchFromGitHub {
    owner = "jaspervdj";
    repo = "stylish-haskell";
    # When updating this use `checkMaterialization` flag below to
    # update the `stack-sha256` and `materialized` arguments below.
    rev = "9958a5253a9498c29508895450c4ac47542d5f2a";
    sha256 = "1lc2q15qdhv7xnawdqbrxcdhmy4m7h9v6z1sg4qpyvhf93b43bix";
  };

  project = haskell-nix.stackProject {
    inherit src;
    stack-sha256 = "1j2j97qzgfgdrrvc1xg1bj87n4qkg21dnjdy536cr6cpidxs7gmm";
    # Using a /nix/store ref here will not work on hydra, this is mostly about
    # improving nix-shell startup times
    materialized = /nix/store/1fpjp8jbg1r48fwwsscq1r9r85kjd875-stack-to-nix-pkgs;
    # Set this flag to on temporarily when `src` changes to find out new
    # values for `stack-sha256` and `materialized`
    # checkMaterialization = true;
    pkg-def-extras = [];
    modules = [];
  };
in
  project.stylish-haskell.components.exes.stylish-haskell
