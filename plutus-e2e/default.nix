{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import pinned Nixpkgs with iohk-nix and Haskell.nix overlays
, pkgs ? import ../nix/default.nix { inherit system crossSystem config sourcesOverride; }
# Use this git revision for stamping executables
, gitrev ? pkgs.commonLib.commitIdFromGitRepoOrZero ../.git
# Use this to reference local sources rather than the niv pinned versions (see nix/default.nix)
, sourcesOverride ? {}
# GitHub PR number (as a string), set when building a Hydra PR jobset.
, pr ? null
# Bors job type (as a string), set when building a Hydra bors jobset.
, borsBuild ? null
}:

let

  # The project sources. Sources are filtered by filename, and then
  # further filtered by package subdirectory.
  src = pkgs.lib.cleanSourceWith {
    src = pkgs.haskell-nix.cleanSourceHaskell { src = ./.; };
    name = "plutus-e2e-src";
    filter = pkgs.commonLib.removeSocketFilesFilter;
  };

  buildHaskellProject = args: import ./nix/haskell.nix ({
    inherit config pkgs;
    inherit (pkgs) buildPackages lib stdenv haskell-nix;
    inherit src gitrev pr borsBuild;
  } // args);
  project = buildHaskellProject {};

  self = {
    inherit pkgs project;
  };
in
  self
