{ ... }@args:

let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix (args // { nixpkgsOverride = import sources.nixpkgs args; });
  niv = (import sources.niv {}).niv;
  jormungandrLib = import (sources.jormungandr-nix + "/lib.nix");
in
iohkNix.pkgs.lib // iohkNix // {
  inherit sources niv jormungandrLib;
}
