{ pkgs ? import ../nix/default.nix {} }:

with pkgs;

mkShell {
  name = "cardano-wallet-docs-env";
  nativeBuildInputs = [ emanote ];
}
