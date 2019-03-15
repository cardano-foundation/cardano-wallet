{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}

# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix
# Pin nixpkgs to a revision on the nixos-19.03 channel
, nixpkgs ? iohkLib.fetchNixpkgs ./nix/nixpkgs-src.json
, pkgs ? import nixpkgs { inherit system crossSystem config; }

# Keep this argument even if unused.
# It will prevent Hydra from caching the evaluation.
, gitrev ? iohkLib.commitIdFromGitRepo ./.
}:

{
  inherit pkgs;

  cardano-http-bridge = import ./nix/cardano-http-bridge.nix { inherit pkgs; };
}
