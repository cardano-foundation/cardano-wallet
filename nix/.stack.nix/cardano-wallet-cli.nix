{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-wallet-cli"; version = "2019.6.24"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Utilities for a building Command-Line Interfaces";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.ansi-terminal)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-wallet-core)
          (hsPkgs.docopt)
          (hsPkgs.fmt)
          (hsPkgs.heredoc)
          (hsPkgs.http-client)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.text)
          (hsPkgs.text-class)
          ];
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.hspec)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.QuickCheck)
            (hsPkgs.text)
            (hsPkgs.text-class)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/cli; }