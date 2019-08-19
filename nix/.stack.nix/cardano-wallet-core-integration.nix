{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-wallet-core-integration";
        version = "2019.6.24";
        };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Core integration test library.";
      description = "Shared core functionality for our integration test suites.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.aeson-qq)
          (hsPkgs.async)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-wallet-cli)
          (hsPkgs.cardano-wallet-core)
          (hsPkgs.command)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.extra)
          (hsPkgs.generic-lens)
          (hsPkgs.hspec)
          (hsPkgs.hspec-expectations-lifted)
          (hsPkgs.http-api-data)
          (hsPkgs.http-client)
          (hsPkgs.http-types)
          (hsPkgs.process)
          (hsPkgs.retry)
          (hsPkgs.time)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.text-class)
          (hsPkgs.time)
          (hsPkgs.warp)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/core-integration; }