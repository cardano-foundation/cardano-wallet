# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ walletPackages ? import ../default.nix {}
, pkgs ? walletPackages.private.pkgs
, ghcVersion ? null
, extraArgs ? []
}:

let
  cabalShell = import ./cabal-shell.nix {
    inherit walletPackages pkgs ghcVersion;
  };
in
  cabalShell.overrideAttrs (old: {
    name = "cardano-wallet-stack-env";

    buildInputs = old.buildInputs ++ [ pkgs.stack ];

    # Build environment setup copied from
    # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
    STACK_PLATFORM_VARIANT = "nix";
    STACK_IN_NIX_SHELL = 1;
    STACK_IN_NIX_EXTRA_ARGS = extraArgs;
  })
