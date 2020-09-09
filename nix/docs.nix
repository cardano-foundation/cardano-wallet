{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? import ./default.nix {}
, haskellPackages
, ghc
}:

let
  inherit (pkgs) lib;

  haddock-combine = pkgs.callPackage ./haddock-combine.nix {
    inherit ghc;
  };

  toHaddock = pkgs.haskell-nix.haskellLib.collectComponents' "library" haskellPackages;

in pkgs.recurseIntoAttrs {
  combined-haddock = haddock-combine {
      hspkgs = lib.attrValues toHaddock;
      prologue = pkgs.writeTextFile {
        name = "prologue";
        text = "Combined documentation for all the public cardano-wallet libraries.";
    };
  };

}
