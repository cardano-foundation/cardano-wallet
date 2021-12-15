{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "ekg-forward"; version = "0.1.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "Denis Shevchenko <denis.shevchenko@iohk.io>";
      author = "Denis Shevchenko";
      homepage = "https://github.com/input-output-hk/ekg-forward";
      url = "";
      synopsis = "See README for more info";
      description = "See README for more info";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."typed-protocols-cborg" or (errorHandler.buildDepError "typed-protocols-cborg"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      exes = {
        "demo-forwarder" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "demo-acceptor" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      tests = {
        "ekg-forward-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ekg-forward";
      rev = "2adc8b698443bb10154304b24f6c1d6913bb65b9";
      sha256 = "0cyixq3jmq43zs1yzrycqw1klyjy0zxf1vifknnr1k9d6sc3zf6b";
      }) // {
      url = "https://github.com/input-output-hk/ekg-forward";
      rev = "2adc8b698443bb10154304b24f6c1d6913bb65b9";
      sha256 = "0cyixq3jmq43zs1yzrycqw1klyjy0zxf1vifknnr1k9d6sc3zf6b";
      };
    }