let
  self = import ./default.nix {};
  # a shell that only has basic CI tools, and no dependencies
  ci-shell = self.pkgs.stdenv.mkDerivation {
    name = "ci-shell";
    buildInputs = [ self.pkgs.haskellPackages.stylish-haskell self.iohkLib.hlint ];
  };
in self.shell // { inherit ci-shell; }
