final: prev:
    let
      old = prev.haskell-nix;
      fix = { pkgs, buildModules, config, lib, ... }: prev.haskell-nix.haskellLib.addPackageKeys {
            packages.basement.components.library.configureFlags = [ "--hsc2hs-option=--cflag=-Wno-int-conversion" ];
      };
    in {
      haskell-nix = old // { defaultModules = old.defaultModules ++ [ fix ]; };
    }

