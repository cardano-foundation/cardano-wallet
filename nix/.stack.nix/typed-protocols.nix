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
      specVersion = "1.10";
      identifier = { name = "typed-protocols"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "alex@well-typed.com, duncan@well-typed.com, marcin.szamotulski@iohk.io";
      author = "Alexander Vieth, Duncan Coutts, Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "A framework for strongly typed protocols";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "d070bad7ce389a4b2ff7fb4fcb7937fdeca80f3a";
      sha256 = "0jzdwjgqcj06b0rvwyh61cgf23dlh62lcn8z7dbm7wxwjjgpkjb1";
      }) // {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "d070bad7ce389a4b2ff7fb4fcb7937fdeca80f3a";
      sha256 = "0jzdwjgqcj06b0rvwyh61cgf23dlh62lcn8z7dbm7wxwjjgpkjb1";
      };
    postUnpack = "sourceRoot+=/typed-protocols; echo source root reset to \$sourceRoot";
    }