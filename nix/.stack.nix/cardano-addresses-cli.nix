{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { release = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "cardano-addresses-cli"; version = "3.4.0"; };
      license = "Apache-2.0";
      copyright = "2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/cardano-addresses#readme";
      url = "";
      synopsis = "Utils for constructing a command-line on top of cardano-addresses.";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/cardano-addresses>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      exes = {
        "cardano-address" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."cardano-addresses-cli" or (errorHandler.buildDepError "cardano-addresses-cli"))
            ];
          buildable = true;
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."cardano-addresses-cli" or (errorHandler.buildDepError "cardano-addresses-cli"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."string-interpolate" or (errorHandler.buildDepError "string-interpolate"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."hjsonschema" or (errorHandler.buildDepError "hjsonschema"));
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            (hsPkgs.buildPackages.cardano-address.components.exes.cardano-address or (pkgs.buildPackages.cardano-address or (errorHandler.buildToolDepError "cardano-address:cardano-address")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-addresses";
      rev = "f87bdef504d3a6e4d908af51aedaa3ae6197a30c";
      sha256 = "0q8pw1rd6j2nx9yjc7mpdk4q5hh7q3x34p3vav2xsa6nvv6h6227";
      });
    postUnpack = "sourceRoot+=/command-line; echo source root reset to \$sourceRoot";
    }) // { cabal-generator = "hpack"; }