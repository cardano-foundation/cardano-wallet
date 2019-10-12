{ ... }@args:

let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix (args // { nixpkgsOverride = import sources.nixpkgs args; });
in
iohkNix.pkgs.lib // iohkNix // {
  inherit sources;
}
