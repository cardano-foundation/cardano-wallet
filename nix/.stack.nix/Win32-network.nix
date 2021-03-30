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
    flags = { demo = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "Win32-network"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "Win32 network API";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."network" or (errorHandler.buildDepError "network"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."ws2_32" or (errorHandler.sysDepError "ws2_32"));
        buildable = true;
        };
      exes = {
        "named-pipe-demo" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."binary" or (errorHandler.buildDepError "binary"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
          buildable = true;
          };
        };
      tests = {
        "test" = {
          depends = (((((((((([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
            ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."async" or (errorHandler.buildDepError "async"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."binary" or (errorHandler.buildDepError "binary"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."network" or (errorHandler.buildDepError "network"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."stm" or (errorHandler.buildDepError "stm"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/Win32-network";
      rev = "94153b676617f8f33abe8d8182c37377d2784bd1";
      sha256 = "0pb7bg0936fldaa5r08nqbxvi2g8pcy4w3c7kdcg7pdgmimr30ss";
      });
    }