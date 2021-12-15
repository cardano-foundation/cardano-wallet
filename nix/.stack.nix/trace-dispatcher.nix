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
      identifier = { name = "trace-dispatcher"; version = "1.29.0"; };
      license = "NONE";
      copyright = "2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "Juergen Nicklisch-Franken";
      homepage = "";
      url = "";
      synopsis = "Package for development of simple and efficient tracers\nbased on the arrow based contra-tracer package";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
          (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
          (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      exes = {
        "trace-dispatcher-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      tests = {
        "trace-dispatcher-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "4f65fb9a27aa7e3a1873ab4211e412af780a3648";
      sha256 = "00k9fqrm0gphjji23x0nc9z6bqh8bqrncgivn3mi3csacjzicrrx";
      }) // {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "4f65fb9a27aa7e3a1873ab4211e412af780a3648";
      sha256 = "00k9fqrm0gphjji23x0nc9z6bqh8bqrncgivn3mi3csacjzicrrx";
      };
    postUnpack = "sourceRoot+=/trace-dispatcher; echo source root reset to \$sourceRoot";
    }